local aukit = require "aukit"

local speakers = {peripheral.find("speaker")}
if #speakers == 0 then error("No speaker attached") end
local mono = #speakers == 1
if #speakers == 2 and peripheral.getName(speakers[1]) == "right" and peripheral.getName(speakers[2]) == "left" then speakers = {speakers[2], speakers[1]} end

local path = ...
local data
if path:match("^https?://") then
    local handle, err = http.get(path, nil, true)
    if not handle then error("Could not connect to " .. path .. ": " .. err) end
    local code = handle.getResponseCode()
    if code ~= 200 then handle.close() error("Could not connect to " .. path .. ": HTTP " .. code) end
    data = handle.readAll()
    handle.close()
elseif path:match("^wss?://") then
    local handle, err = http.websocket(path)
    if not handle then error("Could not connect to " .. path .. ": " .. err) end
    local closed = false
    function data()
        local _ = handle -- keep the handle alive
        if closed then return nil end
        while true do
            local ev, url, msg, binary = os.pullEvent()
            if ev == "websocket_message" and url == path then
                if not binary then print("Warning: Text message detected! This audio may be corrupt.") end
                return msg
            elseif ev == "websocket_closed" and url == path then
                closed = true
                return nil
            end
        end
    end
elseif path:match("^rednet://%d+") or path:match("^rednet%+%l+://%d+") then
    peripheral.find("modem", rednet.open)
    local proto, id, name = path:match("^rednet%+?(%l*)://(%d+)(/?.*)$")
    id = tonumber(id)
    if proto == "" then proto = nil end
    if path ~= "" and id >= 0 then rednet.send(id, name, proto) end
    function data()
        local i, msg
        repeat i, msg = rednet.receive(proto) until id == -1 or i == id
        return msg
    end
else
    path = shell.resolve(...)
    local file, err = fs.open(path, "rb")
    if not file then error("Could not open " .. path .. ": " .. err) end
    data = file.readAll()
    file.close()
end

local iter, length
if path:match("%.dfpwm$") then
    local params = select(2, ...)
    local v = {}
    if params then v = textutils.unserialize("{" .. params:gsub("[^%w,=\"]+", "") .. "}") end
    iter, length = aukit.stream.dfpwm(data, v.sampleRate, v.channels, mono)
elseif path:match("%.wav$") then iter, length = aukit.stream.wav(data, mono)
elseif path:match("%.aiff?$") then iter, length = aukit.stream.aiff(data, mono)
elseif path:match("%.au$") then iter, length = aukit.stream.au(data, mono)
elseif path:match("%.flac$") then iter, length = aukit.stream.flac(data, mono)
elseif path:match("%.pcm$") or path:match("%.raw$") or path:match("^rednet%+?%l*://") then
    local params = select(2, ...)
    local v = {}
    if params then v = textutils.unserialize("{" .. params:gsub("[^%w,=\"]+", "") .. "}") end
    iter, length = aukit.stream.pcm(data, v.bitDepth, v.dataType, v.channels, v.sampleRate, v.bigEndian, mono)
else error("Unknown file type. Make sure to add the right file extension to the path/URL.") end

print("Streaming...")
local w = term.getSize()
local y = select(2, term.getCursorPos())
local fg, bg = colors.toBlit(term.getTextColor()), colors.toBlit(term.getBackgroundColor())
term.write(("00:00 %s %02d:%02d"):format(("\127"):rep(w - 12), math.floor(length / 60), length % 60))
aukit.play(iter, function(pos)
    local p = pos / length
    term.setCursorPos(1, y)
    if p > 1 then
        term.blit(("%02d:%02d %s --:--"):format(math.floor(pos / 60), pos % 60, (" "):rep(w - 12)), fg:rep(w), bg:rep(6) .. fg:rep(w - 12) .. bg:rep(6))
    else
        term.blit(("%02d:%02d %s%s %02d:%02d"):format(math.floor(pos / 60), pos % 60, (" "):rep(math.floor((w - 12) * p)), ("\127"):rep((w - 12) - math.floor((w - 12) * p)), math.floor(length / 60), length % 60),
            fg:rep(w), bg:rep(6) .. fg:rep(math.floor((w - 12) * p)) .. bg:rep((w - 12) - math.floor((w - 12) * p) + 6))
    end
end, table.unpack(speakers))
print()