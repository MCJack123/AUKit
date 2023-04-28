local aukit = require "aukit"
local args = {...}

local help = [=[
auconvert - Modify and convert audio files

Usage: auconvert <options...>

Options:
  -i|--input <path>                 Input file (can specify multiple)
  -o|--output <path>                Output file (can specify multiple)
  -f|--input-format <format>        Format of the last input file
  -F|--output-format <format>       Format of the last output file
    For available formats, use `-[f|F] list`
  -b|--input-bit-depth <bits>       Bit depth of the last input file
  -B|--output-bit-depth <bits>      Bit depth of the last output file
  -t|--input-data-type <type>       Data type of the last input file
  -T|--output-data-type <type>      Data type of the last output file
  -c|--input-channels <number>      Channel count of the last input file
  -C|--output-channels <number>     Channel count of the last output file
  -r|--input-sample-rate <rate>     Sample rate of the last input file
  -R|--output-sample-rate <rate>    Sample rate of the last output file

  -e|--effect <name>[,<args...>]    Apply an effect to the last output file before writing
    For available effects, use `-e list`
  -m|--map <map command>            Map one or more input channels to an output channel
     --interpolation <type>         Set the interpolation type for audio scaling (none, linear, cubic, sinc)
  -h|--help                         Show this help

Map command format:
  General form: <input>[<operator><param>]...=<output>
  Multiple files can be specified through <file index>:<channel> (defaults to file 1)
  Only one map can be specified per output channel
  Operators:
    <a>+<b>: Concatenate channels
    <a>&<b>: Mix channels
    <a>*<n>: Repeat channel `n` times
    <a>[[start],[end]]: Split channel (start/end refer to time in seconds, negative = from end, if end = 0 then end is end of file)
    Order of operations: () [] * + &
    Parentheses may be used to override operation order
  Examples:
    1=1             -> map input channel 1 to output channel 1
    1:2=2:1         -> map input file 1 channel 2 to output file 2 channel 1
    1:1+2:1+3:1=1   -> map the concatenation of the first channels of input files 1-3 to output file 1 channel 1
    1&2=1           -> mix input channels 1 & 2 to output channel 1 (can work as stereo-to-mono mixdown)
    1[,10]=1        -> map first 10 seconds of input channel 1 to output channel 1
]=]

local argmap = {
    i = "--input",
    o = "--output",
    f = "--input-format",
    F = "--output-format",
    b = "--input-bit-depth",
    B = "--output-bit-depth",
    t = "--input-data-type",
    T = "--output-data-type",
    c = "--input-channels",
    C = "--output-channels",
    r = "--input-sample-rate",
    R = "--output-sample-rate",
    e = "--effect",
    m = "--map",
    h = "--help"
}
local dataTypes = {signed = "signed", unsigned = "unsigned", float = "float"}
local interp = {none = "none", linear = "linear", cubic = "cubic", sinc = "sinc"}
local ops = {['['] = 1, ['*'] = 1, ['+'] = 2, ['&'] = 3}
local function num(n, ...) if not n then return end return tonumber(n), num(...) end
local function seq(i, j) if i > j then return else return i, seq(i+1, j) end end

local inputs = {}
local outputs = {}
local maps = {} -- first dimension is each file, and contains either a number (input file to copy from directly), or a table of channels with lists of instructions in RPN order
                -- entries in the instruction list may be a table with `file`? and `channel` (audio operand), a table with `start` and `last` (subscript operator), a string (binary operator), or a number (repeat operand)
                -- the number argument to * is stored in the channel field of an operand

for i = 1, #args, 2 do
    local arg, param = args[i], args[i+1]
    if arg:match "^%-%a$" then arg = argmap[arg:match "^%-(%a)$"] or arg end
    if not arg:match "^%-%-" then error("Invalid argument at position " .. i) end
    if arg ~= "--help" and param == nil then error("Missing parameter to " .. arg) end
    if arg == "--input" then inputs[#inputs+1] = {path = shell.resolve(param)}
    elseif arg == "--output" then outputs[#outputs+1] = {path = shell.resolve(param)}
    elseif arg == "--input-format" then
        if param == "list" then print("List of input formats: adpcm, aiff, au, dfpwm, flac, pcm, wav") return end
        inputs[#inputs].format = assert(aukit[param] and param, "Invalid input format")
    elseif arg == "--output-format" then
        if param == "list" then print("List of output formats: dfpwm, pcm, wav") return end
        outputs[#outputs].format = param
    elseif arg == "--input-bit-depth" then inputs[#inputs].bitDepth = assert(tonumber(param), "Invalid number")
    elseif arg == "--output-bit-depth" then outputs[#outputs].bitDepth = assert(tonumber(param), "Invalid number")
    elseif arg == "--input-data-type" then inputs[#inputs].dataType = assert(dataTypes[param], "Invalid type")
    elseif arg == "--output-data-type" then outputs[#outputs].dataType = assert(dataTypes[param], "Invalid type")
    elseif arg == "--input-channels" then inputs[#inputs].channels = assert(tonumber(param), "Invalid number")
    elseif arg == "--output-channels" then outputs[#outputs].channels = assert(tonumber(param), "Invalid number")
    elseif arg == "--input-sample-rate" then inputs[#inputs].sampleRate = assert(tonumber(param), "Invalid number")
    elseif arg == "--output-sample-rate" then outputs[#outputs].sampleRate = assert(tonumber(param), "Invalid number")
    elseif arg == "--effect" then
        if param == "list" then
            local e = {"mono", "reverse"}
            for k in pairs(aukit.effects) do e[#e+1] = k end
            table.sort(e)
            print("List of effects: " .. table.concat(e, ", "))
            return
        end
        local e = {}
        for str in param:gmatch("[^,]+") do e[#e+1] = tonumber(str) or str end
        outputs[#outputs].effects = outputs[#outputs].effects or {}
        outputs[#outputs].effects[#outputs[#outputs].effects+1] = e
    elseif arg == "--map" then
        local inp, out = param:match "^([%d:+*&,%[%]%(%)]+)=(%d+:?%d*)"
        if not inp then error("Invalid map command") end
        local current = nil
        local outstack, opstack = {}, {}
        local state = 0
        for n, c in inp:gmatch "()(.)" do
            if state == 0 or state == 1 then
                if c:match "%d" then
                    if state == 1 then current = current * 10 + tonumber(c)
                    elseif not current then current = {channel = tonumber(c)}
                    else current.channel = current.channel * 10 + tonumber(c) end
                elseif c == ':' then
                    if state == 1 then error("Syntax error in map command: unexpected `:` at " .. n) end
                    if not current then error("Syntax error in map command: expected number, got `:` at " .. n) end
                    if current.file then error("Syntax error in map command: unexpected `:` at " .. n) end
                    current.file, current.channel = current.channel, 0
                elseif c == '*' then
                    if current then outstack[#outstack+1], current = current, nil end
                    while #opstack > 0 and opstack[#opstack] ~= '(' and (ops[opstack[#opstack]] >= ops[c]) do
                        outstack[#outstack+1], opstack[#opstack] = opstack[#opstack], nil
                    end
                    opstack[#opstack+1] = c
                    state = 1
                    current = 0
                elseif c == '[' then
                    if current then outstack[#outstack+1], current = current, nil end
                    while #opstack > 0 and opstack[#opstack] ~= '(' and (ops[opstack[#opstack]] >= ops[c]) do
                        outstack[#outstack+1], opstack[#opstack] = opstack[#opstack], nil
                    end
                    opstack[#opstack+1] = c
                    current = {start = 0, last = 0}
                    state = 2
                elseif c == '+' or c == '&' then
                    if current then outstack[#outstack+1], current = current, nil end
                    while #opstack > 0 and opstack[#opstack] ~= '(' and (ops[opstack[#opstack]] >= ops[c]) do
                        outstack[#outstack+1], opstack[#opstack] = opstack[#opstack], nil
                    end
                    opstack[#opstack+1] = c
                    state = 0
                elseif c == '(' then
                    if current then error("Syntax error in map command: unexpected `(` at " .. n) end
                    opstack[#opstack+1] = c
                elseif c == ')' then
                    while #opstack > 0 and opstack[#opstack] ~= '(' do
                        outstack[#outstack+1], opstack[#opstack] = opstack[#opstack], nil
                    end
                    if #opstack == 0 then error("Syntax error in map command: unexpected `)` at " .. n) end
                    opstack[#opstack] = nil
                    state = 0
                else error("Syntax error in map command: unexpected token `" .. c .. "` at " .. n) end
            elseif state == 2 or state == 3 then
                if c:match "%d" then
                    if state == 2 then current.start = current.start * 10 + tonumber(c)
                    else current.last = current.last * 10 + tonumber(c) end
                elseif c == ',' then
                    if state == 3 then error("Syntax error in map command: unexpected token `,` at " .. n) end
                    state = 3
                elseif c == ']' then
                    if state == 2 then error("Syntax error in map command: expected `,` at " .. n) end
                    outstack[#outstack+1], current = current, nil
                    state = 0
                end
            end
        end
        if current then outstack[#outstack+1], current = current, nil end
        while #opstack > 0 do
            if opstack[#opstack] == '(' then error("Syntax error in map command: expected `)` at <eof>") end
            outstack[#outstack+1], opstack[#opstack] = opstack[#opstack], nil
        end
        local file, channel = 1, nil
        if out:find ":" then file, channel = num(out:match "(%d+):(%d+)")
        else channel = tonumber(out) end
        maps[file] = maps[file] or {}
        if maps[file][channel] then error("Mapping already exists for file " .. file .. ", channel " .. channel) end
        maps[file][channel] = outstack
    elseif arg == "--interpolation" then aukit.defaultInterpolation = assert(interp[param], "Invalid interpolation type")
    elseif arg == "--help" then textutils.pagedPrint(help) return
    else print("Unknown argument " .. arg .. ", use --help for help") end
end

print("auconvert, using AUKit version " .. aukit._VERSION)
if #inputs == 0 then error("No inputs specified.") end
for i, v in ipairs(inputs) do
    print("Input " .. i .. ":")
    local file, err = fs.open(v.path, "rb")
    if not file then error("Could not open file '" .. v.path .. "': " .. err) end
    local data = file.readAll()
    file.close()
    print("  File: " .. v.path)
    if v.format then
        if v.format == "pcm" then
            v.audio = aukit[v.format](data, v.bitDepth, v.dataType, v.channels, v.sampleRate)
            print("  Type: PCM, " .. (v.bitDepth or 8) .. " bit " .. (v.dataType or "signed"))
        elseif v.format == "adpcm" or v.format == "dfpwm" then
            v.audio = aukit[v.format](data, v.channels, v.sampleRate)
            print("  Type: " .. v.format:upper())
        else
            v.audio = aukit[v.format](data)
            print("  Type: " .. v.format:upper())
        end
    else
        local format, dataType, bitDepth
        if v.path:lower():match("%.dfpwm$") then format = "dfpwm"
        elseif v.path:lower():match("%.wav$") then format = "wav"
        elseif v.path:lower():match("%.aiff?$") then format = "aiff"
        elseif v.path:lower():match("%.au$") then format = "au"
        elseif v.path:lower():match("%.flac$") then format = "flac"
        elseif v.path:lower():match("%.pcm$") or v.path:lower():match("%.raw$") then format, dataType, bitDepth = "pcm", "signed", 8
        else format, dataType, bitDepth = aukit.detect(data) end
        if not format then error("Could not detect file type.") end
        if format == "pcm" then
            v.audio = aukit[format](data, v.bitDepth or bitDepth, v.dataType or dataType, v.channels, v.sampleRate)
            print("  Type: PCM, " .. (v.bitDepth or (bitDepth .. " (guessed)")) .. " bit " .. (v.dataType or (dataType .. " (guessed)")))
        elseif format == "adpcm" or format == "dfpwm" then
            v.audio = aukit[format](data, v.channels, v.sampleRate)
            print("  Type: " .. format:upper())
        else
            v.audio = aukit[format](data)
            print("  Type: " .. format:upper())
        end
    end
    if v.audio.info.dataType then print("  Sample format: " .. v.audio.info.dataType) end
    if v.audio.info.bitDepth then print("  Sample depth: " .. v.audio.info.bitDepth) end
    if v.channels and v.channels ~= #v.audio.data then
        while v.channels > #v.audio.data do v.audio = v.audio:combine(v.audio, v.audio) end
        if v.channels < #v.audio.data then v.audio = v.audio:split({seq(1, v.channels)}) end
    end
    print("  Channels: " .. #v.audio.data)
    print("  Sample rate: " .. v.audio.sampleRate)
    print("  Length: " .. textutils.formatTime(v.audio:len() / 60, true))
    if next(v.audio.metadata) then
        print("  Metadata:")
        for k, m in pairs(v.audio.metadata) do print("    " .. k:sub(1, 1):upper() .. k:sub(2):gsub("(%u)", " %1") .. ": " .. m) end
    end
    print()
    sleep(0)
end

if #outputs == 0 then error("No outputs specified.") end
local usedInputs = {}
if not next(maps) then
    -- Set up basic mappings for n:* => n:*
    for i = 1, #inputs do
        if not outputs[i] then error("Not enough outputs for the specified inputs. Add more outputs to match the number of inputs, or use --map to map multiple inputs to one output.") end
        maps[i] = i
    end
end
-- Optimize maps that contain simple n:* => m:* instructions
for k, v in pairs(maps) do
    local n
    if type(v) == "table" then
        for i, w in pairs(v) do
            if #w == 1 and type(w[1]) == "table" and w[1].channel == i and (n == nil or w[1].file == n) then n = w[1].file
            else n = nil break end
        end
    end
    if n then maps[k] = n end
end
for i, v in ipairs(outputs) do
    print("Output " .. i .. ":")
    -- Calculate missing target parameters from mapped inputs (use highest params for multiple files)
    if not maps[i] then error("Missing mappings for file. Please specify --map parameters for this file.")
    elseif type(maps[i]) == "number" then
        local inp = inputs[maps[i]]
        if not inp then error("Mapped input " .. maps[i] .. " does not exist.") end
        usedInputs[maps[i]] = true
        v.bitDepth = v.bitDepth or inp.bitDepth or inp.audio.info.bitDepth or 8
        v.dataType = v.dataType or inp.dataType or inp.audio.info.dataType or "signed"
        v.channels = v.channels or inp.channels or #inp.audio.data
        v.sampleRate = v.sampleRate or inp.audio.sampleRate
    else
        local bitDepth, dataType, channels, sampleRate = 8, "signed", 1, 1
        for c, l in pairs(maps[i]) do
            channels = math.max(channels, c)
            for _, n in ipairs(l) do
                if type(n) == "table" and n.channel then
                    local inp = inputs[n.file or i]
                    if not inp then error("Mapped input " .. maps[i] .. " does not exist.") end
                    usedInputs[n.file or i] = true
                    bitDepth = math.max(bitDepth, inp.bitDepth or inp.audio.info.bitDepth or 8)
                    sampleRate = math.max(sampleRate, inp.audio.sampleRate)
                    if inp.dataType == "float" or inp.audio.info.dataType == "float" then dataType = "float" end
                end
            end
        end
        v.bitDepth = v.bitDepth or bitDepth
        v.dataType = v.dataType or dataType
        v.channels = v.channels or channels
        v.sampleRate = v.sampleRate or sampleRate
        for c = 1, v.channels do if not maps[i][c] then error("Missing mapping for channel " .. c .. ". Please specify a --map parameter for this channel.") end end
    end
    if v.format == "wav" then
        if v.bitDepth == 1 then v.dataType = "dfpwm"
        elseif v.bitDepth == 8 then v.dataType = "unsigned"
        else v.dataType = "signed" end
    end
    print("  File: " .. v.path)
    if v.format then
        if v.format == "pcm" then
            print("  Type: PCM, " .. v.bitDepth .. " bit " .. v.dataType)
        else
            print("  Type: " .. v.format:upper())
        end
    else
        if v.path:lower():match("%.dfpwm$") then v.format = "dfpwm"
        elseif v.path:lower():match("%.wav$") then v.format = "wav"
        elseif v.path:lower():match("%.pcm$") or v.path:lower():match("%.raw$") then v.format = "pcm"
        else error("Could not detect file type.") end
        if v.format == "pcm" then print("  Type: PCM, " .. v.bitDepth .. " bit " .. v.dataType)
        else print("  Type: " .. v.format:upper()) end
    end
    print("  Sample format: " .. v.dataType)
    print("  Sample depth: " .. v.bitDepth)
    print("  Channels: " .. v.channels)
    print("  Sample rate: " .. v.sampleRate)
    print()
end

for i in pairs(maps) do if not outputs[i] then error("Mappings were specified for output " .. i .. ", but no path was specified.") end end
for i in ipairs(inputs) do if not usedInputs[i] then term.setTextColor(colors.yellow) print("Warning: Input " .. i .. " was never used. Consider mapping it to an output.") term.setTextColor(colors.white) end end

for i, v in ipairs(outputs) do
    print("Processing output " .. i .. "...")
    local x, y = term.getCursorPos()
    if type(maps[i]) == "table" then
        -- Process map instructions
        local channels = {}
        for c, l in pairs(maps[i]) do
            term.setCursorPos(x, y)
            term.clearLine()
            write("> Processing map for channel " .. c)
            local audioStack = {}
            for _, n in ipairs(l) do
                if type(n) == "table" then
                    if n.start or n.last then
                        -- Subscript operator
                        write("[" .. (n.start or 0) .. "," .. (n.last or 0) .. "] ")
                        if #audioStack == 0 then error("Syntax error in map command: Not enough arguments") end
                        audioStack[#audioStack] = audioStack[#audioStack]:sub(n.start, n.last)
                        sleep(0)
                    else
                        -- Audio channel operand
                        write((n.file or i) .. ":" .. n.channel .. " ")
                        audioStack[#audioStack+1] = inputs[n.file or i].audio:split({n.channel})
                        sleep(0)
                    end
                elseif type(n) == "number" then
                    -- Repeat operand
                    write(n .. " ")
                    audioStack[#audioStack+1] = n
                elseif n == "+" then
                    -- Concatenate operator
                    write("+ ")
                    if #audioStack < 2 then error("Syntax error in map command: Not enough arguments") end
                    local a, b = audioStack[#audioStack-1], audioStack[#audioStack]
                    audioStack[#audioStack] = nil
                    audioStack[#audioStack] = a:concat(b)
                    sleep(0)
                elseif n == "&" then
                    -- Mix operator
                    write("& ")
                    if #audioStack < 2 then error("Syntax error in map command: Not enough arguments") end
                    local a, b = audioStack[#audioStack-1], audioStack[#audioStack]
                    audioStack[#audioStack] = nil
                    audioStack[#audioStack] = a:mix(b)
                    sleep(0)
                elseif n == "*" then
                    -- Repeat operator
                    write("* ")
                    if #audioStack < 2 then error("Syntax error in map command: Not enough arguments") end
                    local a, b = audioStack[#audioStack-1], audioStack[#audioStack]
                    if type(a) == "number" then a, b = b, a end
                    audioStack[#audioStack] = nil
                    audioStack[#audioStack] = a:rep(b)
                    sleep(0)
                end
            end
            if #audioStack ~= 1 then error("Syntax error in map command: Wrong number of arguments") end
            channels[c] = audioStack[1]
        end
        v.audio = channels[1]:combine(table.unpack(channels, 2))
    else v.audio = inputs[maps[i]].audio end
    for n, e in ipairs(v.effects or {}) do
        term.setCursorPos(x, y)
        term.clearLine()
        write("> Processing effect #" .. n .. " (" .. e[1] .. ")")
        if e[1] == "mono" or e[1] == "reverse" then v.audio = v.audio[e[1]](v.audio)
        else aukit.effects[e[1]](v.audio, table.unpack(e, 2)) end
        sleep(0)
    end
    if #v.audio.data > v.channels then v.audio = v.audio:split({seq(1, v.channels)}) end
    if v.audio.sampleRate ~= v.sampleRate then
        term.setCursorPos(x, y)
        term.clearLine()
        write("> Resampling")
        v.audio = v.audio:resample(v.sampleRate)
        sleep(0)
    end
    term.setCursorPos(x, y)
    term.clearLine()
    write("> Writing file")
    local data
    if v.format == "pcm" then data = v.audio:pcm(v.bitDepth, v.dataType)
    elseif v.format == "dfpwm" then data = v.audio:dfpwm()
    elseif v.format == "wav" then data = v.audio:wav(v.bitDepth) end
    local file = fs.open(v.path, "wb")
    file.write(data)
    file.close()
    term.setCursorPos(x, y)
    term.clearLine()
    print("Completed processing file " .. i .. ".")
    sleep(0)
end
