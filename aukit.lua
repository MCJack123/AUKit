local expect = require "cc.expect"
local dfpwm = require "cc.audio.dfpwm"

local aukit = {effects = {}}

--- @type Audio
local Audio = {}
local Audio_mt

local function intunpack(str, pos, sz, signed, be)
    local n = 0
    if be then for i = 0, sz - 1 do n = n * 256 + str:byte(pos+i) end
    else for i = 0, sz - 1 do n = n + str:byte(pos+i) * 2^(8*i) end end
    if signed and n >= 2^(sz*8-1) then n = n - 2^(sz*8) end
    return n, pos + sz
end

local interpolate = {
    none = function(data, x)
        return data[math.floor(x)]
    end,
    linear = function(data, x)
        return data[math.floor(x)] + ((data[math.ceil(x)] or data[math.floor(x)]) - data[math.floor(x)]) * (x - math.floor(x))
    end,
    cubic = function(data, x)
        local p0, p1, p2, p3, fx = data[math.floor(x)-1] or data[math.floor(x)], data[math.floor(x)], data[math.ceil(x)] or data[math.floor(x)], data[math.ceil(x)+1] or data[math.ceil(x)] or data[math.floor(x)], x - math.floor(x)
        return (-0.5*p0 + 1.5*p1 - 1.5*p2 + 0.5*p3)*fx^3 + (p0 - 2.5*p1 + 2*p2 - 0.5*p3)*fx^2 + (-0.5*p0 + 0.5*p2)*fx + p1
    end
}

local decodeFLAC do
     
    -- Simple FLAC decoder (Java)
    -- 
    -- Copyright (c) 2017 Project Nayuki. (MIT License)
    -- https://www.nayuki.io/page/simple-flac-implementation
    -- 
    -- Permission is hereby granted, free of charge, to any person obtaining a copy of
    -- this software and associated documentation files (the "Software"), to deal in
    -- the Software without restriction, including without limitation the rights to
    -- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
    -- the Software, and to permit persons to whom the Software is furnished to do so,
    -- subject to the following conditions:
    -- - The above copyright notice and this permission notice shall be included in
    --   all copies or substantial portions of the Software.
    -- - The Software is provided "as is", without warranty of any kind, express or
    --   implied, including but not limited to the warranties of merchantability,
    --   fitness for a particular purpose and noninfringement. In no event shall the
    --   authors or copyright holders be liable for any claim, damages or other
    --   liability, whether in an action of contract, tort or otherwise, arising from,
    --   out of or in connection with the Software or the use or other dealings in the
    --   Software.

    local FIXED_PREDICTION_COEFFICIENTS = {
        {},
        {1},
        {2, -1},
        {3, -3, 1},
        {4, -6, 4, -1},
    };

    local function BitInputStream(data, pos)
        local obj = {}
        local bitBuffer, bitBufferLen = 0, 0
        function obj.alignToByte()
            bitBufferLen = bitBufferLen - bitBufferLen % 8
        end
        function obj.readByte()
            return obj.readUint(8)
        end
        function obj.readUint(n)
            if n == 0 then error("0") end
            while bitBufferLen < n do
                local temp = data:byte(pos)
                pos = pos + 1
                if temp == nil then return nil end
                bitBuffer = (bitBuffer * 256 + temp) % 0x100000000000
                bitBufferLen = bitBufferLen + 8
            end
            bitBufferLen = bitBufferLen - n
            local result = math.floor(bitBuffer / 2^bitBufferLen)
            if n < 32 then result = result % 2^n end
            return result
        end
        function obj.readSignedInt(n)
            local v = obj.readUint(n)
            if v >= 2^(n-1) then v = v - 2^n end
            return v
        end
        function obj.readRiceSignedInt(param)
            local val = 0
            while (obj.readUint(1) == 0) do val = val + 1 end
            val = val * 2^param + obj.readUint(param)
            if bit32.btest(val, 1) then return -math.floor(val / 2) - 1
            else return math.floor(val / 2) end
        end
        return obj
    end

    local function decodeResiduals(inp, warmup, blockSize, result)
        local method = inp.readUint(2);
        if (method >= 2) then error("Reserved residual coding method " .. method) end
        local paramBits = method == 0 and 4 or 5;
        local escapeParam = method == 0 and 0xF or 0x1F;

        local partitionOrder = inp.readUint(4);
        local numPartitions = 2^partitionOrder;
        if (blockSize % numPartitions ~= 0) then
            error("Block size not divisible by number of Rice partitions")
        end
        local partitionSize = math.floor(blockSize / numPartitions);

        for i = 0, numPartitions-1 do
            local start = i * partitionSize + (i == 0 and warmup or 0);
            local endd = (i + 1) * partitionSize;

            local param = inp.readUint(paramBits);
            if (param < escapeParam) then
                for j = start, endd - 1 do
                    result[j+1] = inp.readRiceSignedInt(param)
                end
            else
                local numBits = inp.readUint(5);
                for j = start, endd - 1 do
                    result[j+1] = inp.readSignedInt(numBits)
                end
            end
        end
    end

    local function restoreLinearPrediction(result, coefs, shift, blockSize)
        for i = #coefs, blockSize - 1 do
            local sum = 0
            for j = 0, #coefs - 1 do
                sum = sum + result[i - j] * coefs[j + 1]
            end
            result[i + 1] = result[i + 1] + math.floor(sum / 2^shift)
        end
    end

    local function decodeFixedPredictionSubframe(inp, predOrder, sampleDepth, blockSize, result)
        for i = 1, predOrder do
            result[i] = inp.readSignedInt(sampleDepth);
        end
        decodeResiduals(inp, predOrder, blockSize, result);
        restoreLinearPrediction(result, FIXED_PREDICTION_COEFFICIENTS[predOrder+1], 0, blockSize);
    end

    local function decodeLinearPredictiveCodingSubframe(inp, lpcOrder, sampleDepth, blockSize, result)
        for i = 1, lpcOrder do
            result[i] = inp.readSignedInt(sampleDepth);
        end
        local precision = inp.readUint(4) + 1;
        local shift = inp.readSignedInt(5);
        local coefs = {};
        for i = 1, lpcOrder do
            coefs[i] = inp.readSignedInt(precision);
        end
        decodeResiduals(inp, lpcOrder, blockSize, result);
        restoreLinearPrediction(result, coefs, shift, blockSize);
    end

    local function decodeSubframe(inp, sampleDepth, blockSize, result)
        inp.readUint(1);
        local type = inp.readUint(6);
        local shift = inp.readUint(1);
        if (shift == 1) then
            while (inp.readUint(1) == 0) do shift = shift + 1 end
        end
        sampleDepth = sampleDepth - shift

        if (type == 0) then  -- Constant coding
            local c = inp.readSignedInt(sampleDepth)
            for i = 1, blockSize do result[i] = c end
        elseif (type == 1) then  -- Verbatim coding
            for i = 1, blockSize do
                result[i] = inp.readSignedInt(sampleDepth);
            end
        elseif (8 <= type and type <= 12) then
            decodeFixedPredictionSubframe(inp, type - 8, sampleDepth, blockSize, result)
        elseif (32 <= type and type <= 63) then
            decodeLinearPredictiveCodingSubframe(inp, type - 31, sampleDepth, blockSize, result)
        else
            error("Reserved subframe type")
        end

        for i = 1, blockSize do
            result[i] = result[i] * 2^shift
        end
    end

    local function decodeSubframes(inp, sampleDepth, chanAsgn, blockSize, result)
        local subframes = {}
        for i = 1, #result do subframes[i] = {} end
        if (0 <= chanAsgn and chanAsgn <= 7) then
            for ch = 1, #result do
                decodeSubframe(inp, sampleDepth, blockSize, subframes[ch])
            end
        elseif (8 <= chanAsgn and chanAsgn <= 10) then
            decodeSubframe(inp, sampleDepth + (chanAsgn == 9 and 1 or 0), blockSize, subframes[1])
            decodeSubframe(inp, sampleDepth + (chanAsgn == 9 and 0 or 1), blockSize, subframes[2])
            if (chanAsgn == 8) then
                for i = 1, blockSize do
                    subframes[2][i] = subframes[1][i] - subframes[2][i]
                end
            elseif (chanAsgn == 9) then
                for i = 1, blockSize do
                    subframes[1][i] = subframes[1][i] + subframes[2][i]
                end
            elseif (chanAsgn == 10) then
                for i = 1, blockSize do
                    local side = subframes[2][i]
                    local right = subframes[1][i] - math.floor(side / 2)
                    subframes[2][i] = right
                    subframes[1][i] = right + side
                end
            end
        else
            error("Reserved channel assignment");
        end
        for ch = 1, #result do
            for i = 1, blockSize do
                local s = subframes[ch][i]
                if s >= 2^(sampleDepth-1) then s = s - 2^sampleDepth end
                result[ch][i] = s / 2^sampleDepth
            end
        end
    end

    local function decodeFrame(inp, numChannels, sampleDepth, out2)
        local out = {}
        for i = 1, numChannels do out[i] = {} end
        -- Read a ton of header fields, and ignore most of them
        local temp = inp.readByte()
        if temp == nil then
            return false
        end
        local sync = temp * 64 + inp.readUint(6);
        if sync ~= 0x3FFE then error("Sync code expected") end

        inp.readUint(1);
        inp.readUint(1);
        local blockSizeCode = inp.readUint(4);
        local sampleRateCode = inp.readUint(4);
        local chanAsgn = inp.readUint(4);
        inp.readUint(3);
        inp.readUint(1);

        temp = inp.readUint(8);
        local t2 = -1
        for i = 7, 0, -1 do if not bit32.btest(temp, 2^i) then break end t2 = t2 + 1 end
        for i = 1, t2 do inp.readUint(8) end

        local blockSize
        if (blockSizeCode == 1) then
            blockSize = 192
        elseif (2 <= blockSizeCode and blockSizeCode <= 5) then
            blockSize = 576 * 2^(blockSizeCode - 2)
        elseif (blockSizeCode == 6) then
            blockSize = inp.readUint(8) + 1
        elseif (blockSizeCode == 7) then
            blockSize = inp.readUint(16) + 1
        elseif (8 <= blockSizeCode and blockSizeCode <= 15) then
            blockSize = 256 * 2^(blockSizeCode - 8)
        else
            error("Reserved block size")
        end

        if (sampleRateCode == 12) then
            inp.readUint(8)
        elseif (sampleRateCode == 13 or sampleRateCode == 14) then
            inp.readUint(16)
        end

        inp.readUint(8)

        decodeSubframes(inp, sampleDepth, chanAsgn, blockSize, out)
        inp.alignToByte()
        inp.readUint(16)

        for c = 1, numChannels do
            local n = #out2[c]
            for i = 1, blockSize do out2[c][n+i] = out[c][i] end
        end

        return true
    end

    function decodeFLAC(inp)
        local out = {}
        local pos = 1
        -- Handle FLAC header and metadata blocks
        local temp temp, pos = intunpack(inp, pos, 4, false, true)
        if temp ~= 0x664C6143 then error("Invalid magic string") end
        local sampleRate, numChannels, sampleDepth, numSamples
        local last = false
        while not last do
            temp, pos = inp:byte(pos), pos + 1
            last = bit32.btest(temp, 0x80)
            local type = bit32.band(temp, 0x7F);
            local length length, pos = intunpack(inp, pos, 3, false, true)
            if type == 0 then  -- Stream info block
                pos = pos + 10
                sampleRate, pos = intunpack(inp, pos, 2, false, true)
                sampleRate = sampleRate * 16 + bit32.rshift(inp:byte(pos), 4)
                numChannels = bit32.band(bit32.rshift(inp:byte(pos), 1), 7) + 1;
                sampleDepth = bit32.band(inp:byte(pos), 1) * 16 + bit32.rshift(inp:byte(pos+1), 4) + 1;
                numSamples, pos = intunpack(inp, pos + 2, 4, false, true)
                numSamples = numSamples + bit32.band(inp:byte(pos-5), 15) * 2^32
                pos = pos + 16
            else
                pos = pos + length
            end
        end
        if not sampleRate then error("Stream info metadata block absent") end
        if sampleDepth % 8 ~= 0 then error("Sample depth not supported") end

        for i = 1, numChannels do out[i] = {} end

        -- Decode FLAC audio frames and write raw samples
        inp = BitInputStream(inp, pos)
        repeat until not decodeFrame(inp, numChannels, sampleDepth, out)
        return {sampleRate = sampleRate, data = out}
    end

end

--- Returns the length of the audio object in seconds.
-- @treturn The audio length
function Audio:len()
    return #self.data[1] / self.sampleRate
end

--- Creates a new audio object with the data resampled to a different sample rate.
-- If the target rate is the same, the object is copied without modification.
-- @tparam number sampleRate The new sample rate in Hertz
-- @tparam[opt="cubic"] "none"|"linear"|"cubic" interpolation The interpolation mode to use
-- @treturn Audio A new audio object with the resampled data
function Audio:resample(sampleRate, interpolation)
    expect(1, sampleRate, "number")
    interpolation = expect(2, interpolation, "string", "nil") or "cubic"
    if interpolation ~= "none" and interpolation ~= "linear" and interpolation ~= "cubic" then error("bad argument #2 (invalid interpolation type)", 2) end
    local new = setmetatable({sampleRate = sampleRate, data = {}}, Audio_mt)
    local ratio = sampleRate / self.sampleRate
    local newlen = #self.data[1] * ratio
    local interp = interpolate[interpolation]
    for y, c in ipairs(self.data) do
        local line = {}
        for i = 1, newlen do
            local x = (i - 1) / ratio + 1
            if x % 1 == 0 then line[i] = c[x]
            else line[i] = math.max(math.min(interp(c, x), 1), -1) end
        end
        new.data[y] = line
    end
    return new
end

--- Mixes down all channels to a new mono-channel audio object.
-- @treturn Audio A new audio object with the audio mixed to mono
function Audio:mono()
    local new = setmetatable({sampleRate = self.ampleRate, data = {{}}}, Audio_mt)
    local cn = #self.data
    for i = 1, #self.data[1] do
        local s = 0
        for c = 1, cn do s = s + self.data[c][i] end
        new.data[1][i] = s / cn
    end
    return new
end

--- Concatenates this audio object with another, adding the contents of each
-- new channel to the end of each old channel, resampling the new channels to match
-- this one (if necessary), and inserting silence in any missing channels.
-- @tparam Audio audio The audio object to concatenate
-- @treturn Audio The new concatenated audio object
function Audio:concat(audio)
    
end

--- Takes a subregion of the audio and returns a new audio object with its contents.
-- This takes the same arguments as @{string.sub}.
-- @tparam[opt=1] number start The start position of the audio
-- @tparam[opt=-1] number last The end position of the audio
-- @treturn Audio The new split audio object
function Audio:sub(start, last)

end

--- Combines the channels of this audio object with another, adding the new
-- channels on the end of the new object, resampling the new channels to match
-- this one (if necessary), and extending any channels that are shorter than the
-- longest channel with zeroes.
-- @tparam Audio audio The audio object to combine with
-- @treturn Audio The new combined audio object
function Audio:combine(audio)

end

--- Splits this audio object into one or more objects with the specified channels.
-- @tparam { [number]... } ... The lists of channels in each new object
-- @treturn Audio... The new audio objects created from the channels in each list
function Audio:split(...)

end

--- Returns a new audio object that repeats this audio a number of times.
-- @tparam number count The number of times to play the audio
-- @treturn Audio The repeated audio
function Audio:rep(count)

end

--- Returns a reversed version of this audio.
-- @treturn Audio The reversed audio
function Audio:reverse()

end

local function encodePCM(info, pos)
    local maxValue = 2^(info.bitDepth-1)
    local add = info.dataType == "unsigned" and maxValue or 0
    local source = info.audio.data
    local function encode(d)
        if info.dataType == "float" then return d
        else return d * (d < 0 and maxValue or maxValue-1) + add end
    end
    local data = {}
    local nc = #source
    local len = #source[1]
    if pos > len then return nil end
    if info.interleaved then for n = pos, pos + info.len - 1 do for c = 1, nc do data[(n-1)*nc+c] = encode(source[c][n]) end end
    elseif info.multiple then
        for c = 1, nc do
            data[c] = {}
            for n = pos, pos + info.len - 1 do
                local s = source[c][n]
                if not s then break end
                data[c][n-pos+1] = encode(s)
            end
        end
        return pos + info.len, table.unpack(data)
    else for c = 1, nc do for n = pos, pos + info.len - 1 do data[(c-1)*len+n] = encode(source[c][n]) end end end
    return data
end

--- Converts the audio data to raw PCM samples.
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32)
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @tparam[opt=true] boolean interleaved Whether to interleave each channel
-- @treturn { [number]... } The resulting audio data
function Audio:pcm(bitDepth, dataType, interleaved)
    bitDepth = expect(1, bitDepth, "number", "nil") or 8
    dataType = expect(2, dataType, "string", "nil") or "signed"
    expect(3, interleaved, "boolean", "nil")
    if interleaved == nil then interleaved = true end
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    if dataType ~= "signed" and dataType ~= "unsigned" and dataType ~= "float" then error("bad argument #3 (invalid data type)", 2) end
    if dataType == "float" and bitDepth ~= 32 then error("bad argument #2 (float audio must have 32-bit depth)", 2) end
    return encodePCM({audio = self, bitDepth = bitDepth, dataType = dataType, interleaved = interleaved, len = #self.data[1]}, 1)
end

--- Returns a function that can be called to encode PCM samples in chunks.
-- This is useful as a for iterator.
-- @tparam[opt=131072] number chunkSize The size of each chunk
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32)
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @treturn function(state: table, pos: number):number,table... The iterator function
-- @treturn table The state for the iterator
-- @treturn number The initial position of the audio
function Audio:stream(chunkSize, bitDepth, dataType)
    chunkSize = expect(1, chunkSize, "number", "nil") or 131072
    bitDepth = expect(2, bitDepth, "number", "nil") or 8
    dataType = expect(3, dataType, "string", "nil") or "signed"
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    if dataType ~= "signed" and dataType ~= "unsigned" and dataType ~= "float" then error("bad argument #3 (invalid data type)", 2) end
    if dataType == "float" and bitDepth ~= 32 then error("bad argument #2 (float audio must have 32-bit depth)", 2) end
    return encodePCM, {audio = self, bitDepth = bitDepth, dataType = dataType, interleaved = false, multiple = true, len = chunkSize}, 1
end

--- Coverts the audio data to a WAV file.
-- @tparam[opt=16] number bitDepth The bit depth of the audio (8, 16, 24, 32)
-- @treturn string The resulting WAV file data
function Audio:wav(bitDepth)
    -- TODO: Support float data
    bitDepth = expect(1, bitDepth, "number", "nil") or 16
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    local data = self:pcm(bitDepth, bitDepth == 8 and "unsigned" or "signed", true)
    return ("<c4Ic4c4IHHIIHHc4I"):pack("RIFF", #data + 36, "WAVE", "fmt ", 16, 1, #self.data, self.sampleRate, self.sampleRate * #self.data, #self.data * bitDepth / 8, bitDepth, "data", #data) .. data
end

--- Converts the audio data to DFPWM. All channels share the same encoder, and
-- channels are stored sequentially uninterleaved.
-- @treturn string... The resulting DFPWM data for each channel
function Audio:dfpwm()
    local channels = {self:pcm(8, "signed", false)}
    local encode = dfpwm.make_encoder()
    for i = 1, #channels do channels[i] = encode(channels[i]) end
    return table.unpack(channels)
end

Audio_mt = {__index = Audio, __add = Audio.combine, __mul = Audio.rep, __concat = Audio.concat, __len = Audio.len}

--- Creates a new audio object from the specified raw PCM data.
-- @tparam string|table data The audio data, either as a raw string, or a table
-- of values (in the format specified by `bitDepth` and `dataType`)
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32); if `dataType` is "float" then this must be 32
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @tparam[opt=true] boolean interleaved Whether each channel is interleaved or separate
-- @tparam[opt=false] boolean bigEndian Whether the audio is big-endian or little-endian; ignored if data is a table
-- @treturn Audio A new audio object containing the specified data
function aukit.pcm(data, bitDepth, dataType, channels, sampleRate, interleaved, bigEndian)
    expect(1, data, "string", "table")
    bitDepth = expect(2, bitDepth, "number", "nil") or 8
    dataType = expect(3, dataType, "string", "nil") or "signed"
    channels = expect(4, channels, "number", "nil") or 1
    sampleRate = expect(5, sampleRate, "number", "nil") or 48000
    expect(6, interleaved, "boolean", "nil")
    if interleaved == nil then interleaved = true end
    expect(7, bigEndian, "boolean", "nil")
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    if dataType ~= "signed" and dataType ~= "unsigned" and dataType ~= "float" then error("bad argument #3 (invalid data type)", 2) end
    if dataType == "float" and bitDepth ~= 32 then error("bad argument #2 (float audio must have 32-bit depth)", 2) end
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    local byteDepth = bitDepth / 8
    if (#data / (type(data) == "table" and 1 or byteDepth)) % channels ~= 0 then error("bad argument #1 (uneven amount of data per channel)", 2) end
    local len = (#data / (type(data) == "table" and 1 or byteDepth)) / channels
    local format = (bigEndian and ">" or "<") .. (dataType == "float" and "f" or ((dataType == "signed" and "i" or "I") .. byteDepth))
    local maxValue = 2^(bitDepth-1)
    local obj = setmetatable({
        sampleRate = sampleRate,
        data = {}
    }, Audio_mt)
    for i = 1, channels do obj.data[i] = {} end
    local pos = 1
    local function read()
        local s
        if type(data) == "table" then s, pos = data[pos], pos + 1
        elseif dataType == "float" then s, pos = format:unpack(data, pos)
        else s, pos = intunpack(data, pos, byteDepth, dataType == "signed", bigEndian) end
        if dataType == "signed" then s = s / (s < 0 and maxValue or maxValue-1)
        elseif dataType == "unsigned" then s = (s - 128) / (s < 128 and -maxValue or maxValue-1) end
        expect.range(s, -1, 1)
        return s
    end
    if interleaved then
        local d = obj.data
        for i = 1, len do --[[if i % 10000 == 0 then write(".") end]] for j = 1, channels do d[j][i] = read() end end
    else for j = 1, channels do
        local line = {}
        for i = 1, len do line[i] = read() end
        obj.data[j] = line
    end end
    return obj
end

--- Creates a new audio object from IMA ADPCM data.
-- @tparam string|table data The audio data, either as a raw string, or a table of nibbles
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @tparam[opt=true] boolean topFirst Whether the top nibble is the first nibble (true) or last (false)
-- (Ignored if `data` is a table)
-- @tparam[opt=true] boolean interleaved Whether each channel is interleaved or separate
-- @treturn Audio A new audio object containing the decoded data
function aukit.adpcm(data, channels, sampleRate, topFirst, interleaved)

end

--- Creates a new audio object from DFPWM1a data. All channels are expected to
-- share the same decoder, and are stored uninterleaved sequentially.
-- @tparam string data The audio data as a raw string
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @treturn Audio A new audio object containing the decoded data
function aukit.dfpwm(data, channels, sampleRate)
    expect(1, data, "string")
    channels = expect(2, channels, "number", "nil") or 1
    sampleRate = expect(3, sampleRate, "number", "nil") or 48000
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    if #data % channels ~= 0 then error("bad argument #1 (uneven amount of data per channel)", 2) end
    return aukit.pcm(dfpwm.decode(data), 8, "signed", channels, sampleRate, false, false)
end

--- Creates a new audio object from a WAV file.
-- @tparam string data The WAV data to load
-- @treturn Audio A new audio object with the contents of the WAV file
function aukit.wav(data)
    -- TODO: add float support, add metadata support, use a proper decoder
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length
    local temp, pos = ("c4"):unpack(data)
    if temp ~= "RIFF" then error("bad argument #1 (not a WAV file)", 2) end
    pos = pos + 4
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "WAVE" then error("bad argument #1 (not a WAV file)", 2) end
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "fmt " then error("invalid WAV file", 2) end
    temp, pos = ("<I"):unpack(data, pos)
    if temp ~= 16 then error("unsupported WAV file", 2) end
    temp, pos = ("<H"):unpack(data, pos)
    if temp ~= 1 then error("unsupported WAV file", 2) end
    channels, sampleRate, pos = ("<HI"):unpack(data, pos)
    pos = pos + 6
    bitDepth, pos = ("<H"):unpack(data, pos)
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "data" then error("invalid WAV file", 2) end
    length, pos = ("<I"):unpack(data, pos)
    local data = data:sub(pos, pos + length - 1)
    if #data < length then error("invalid WAV file", 2) end
    return aukit.pcm(data, bitDepth, bitDepth == 8 and "unsigned" or "signed", channels, sampleRate, true, false)
end

--- Creates a new audio object from an AIFF file.
-- @tparam string data The AIFF data to load
-- @treturn Audio A new audio object with the contents of the AIFF file
function aukit.aiff(data)

end

--- Creates a new audio object from an AU file.
-- @tparam string data The AU data to load
-- @treturn Audio A new audio object with the contents of the AU file
function aukit.au(data)

end

--- Creates a new audio object from a FLAC file.
-- @tparam string data The FLAC data to load
-- @treturn Audio A new audio object with the contents of the FLAC file
function aukit.flac(data)
    expect(1, data, "string")
    return setmetatable(decodeFLAC(data), Audio_mt)
end

--- Creates a new empty audio object with the specified duration.
-- @tparam number duration The length of the audio in seconds
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @treturn Audio The new empty audio object
function aukit.new(duration, channels, sampleRate)
    expect(1, duration, "number")
    channels = expect(2, channels, "number", "nil") or 1
    sampleRate = expect(3, sampleRate, "number", "nil") or 48000
    expect.range(duration, 0)
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    local obj = setmetatable({sampleRate = sampleRate, data = {}}, Audio_mt)
    for c = 1, channels do
        local l = {}
        for i = 1, duration * sampleRate do l[i] = 0 end
        obj.data[c] = l
    end
    return obj
end

--- Creates a new audio object with a tone of the specified frequency and duration.
-- @tparam number frequency The frequency of the tone in Hertz
-- @tparam number duration The length of the audio in seconds
-- @tparam[opt="sine"] "sine"|"triangle"|"sawtooth"|"square" waveType The type of wave to generate
-- @tparam[opt=0.5] number duty The duty cycle of the square wave if selected; ignored otherwise
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @treturn Audio A new audio object with the tone
function aukit.tone(frequency, duration, waveType, duty, channels, sampleRate)

end

--- Creates a new audio object with white noise for the specified duration.
-- @tparam number duration The length of the audio in seconds
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @treturn Audio A new audio object with noise
function aukit.noise(duration, channels, sampleRate)

end

--- Packs a table with PCM data into a string using the specified data type.
-- @tparam { [number]... } data The PCM data to pack
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32); if `dataType` is "float" then this must be 32
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @tparam[opt=false] boolean bigEndian Whether the data should be big-endian or little-endian
-- @treturn string The packed PCM data
function aukit.pack(data, bitDepth, dataType, bigEndian)
    expect(1, data, "string", "table")
    bitDepth = expect(2, bitDepth, "number", "nil") or 8
    dataType = expect(3, dataType, "string", "nil") or "signed"
    expect(4, bigEndian, "boolean", "nil")
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    if dataType ~= "signed" and dataType ~= "unsigned" and dataType ~= "float" then error("bad argument #3 (invalid data type)", 2) end
    if dataType == "float" and bitDepth ~= 32 then error("bad argument #2 (float audio must have 32-bit depth)", 2) end
    local byteDepth = bitDepth / 8
    local format = (bigEndian and ">" or "<") .. (dataType == "float" and "f" or ((dataType == "signed" and "i" or "I") .. byteDepth))
    local formatChunk = format:sub(1, 1) .. format:sub(2):rep(512)
    local retval = ""
    for i = 1, #data, 512 do
        if #data < i + 512 then retval = retval .. format:rep(#data % 512):pack(table.unpack(data, i, #data))
        else retval = retval .. formatChunk:pack(table.unpack(data, i, i+511)) end
    end
    return retval
end

--- Amplifies the audio by the multiplier specified.
-- @tparam Audio audio The audio to modify
-- @tparam number multiplier The multiplier to apply
-- @treturn Audio The audio modified
function aukit.effects.amplify(audio, multiplier)

end

--- Changes the speed and pitch of audio by a multiplier, resampling to keep the
-- same sample rate.
-- @tparam Audio audio The audio to modify
-- @tparam number multiplier The multiplier to apply
-- @treturn Audio The audio modified
function aukit.effects.speed(audio, multiplier)

end

--- Fades a period of music from one amplitude to another.
-- @tparam Audio audio The audio to modify
-- @tparam number startTime The start time of the fade, in seconds
-- @tparam number startAmplitude The amplitude of the beginning of the fade, from 0.0 to 1.0
-- @tparam number endTime The end time of the fade, in seconds
-- @tparam number endAmplitude The amplitude of the end of the fade, from 0.0 to 1.0
-- @treturn Audio The audio modified
function aukit.effects.fade(audio, startTime, startAmplitude, endTime, endAmplitude)

end

--- Inverts all channels in the specified audio.
-- @tparam Audio audio The audio to modify
-- @treturn Audio The audio modified
function aukit.effects.invert(audio)

end

--- Normalizes audio to the specified peak amplitude.
-- @tparam Audio audio The audio to modify
-- @tparam number peakAmplitude The maximum amplitude, from 0.0 to 1.0
-- @tparam[opt=false] boolean independent Whether to normalize each channel independently
-- @treturn Audio The audio modified
function aukit.effects.normalize(audio, peakAmplitude, independent)

end

--- Centers the DC offset of each channel.
-- @tparam Audio audio The audio to modify
-- @treturn Audio The audio modified
function aukit.effects.center(audio)

end

--- Trims any extra silence on either end of the specified audio.
-- @tparam Audio audio The audio to modify
-- @treturn Audio The audio modified
function aukit.effects.trim(audio)

end

return aukit