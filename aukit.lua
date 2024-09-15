--- AUKit: aukit.Audio decoding and processing framework for ComputerCraft
--- @file
---
--- AUKit is a framework designed to simplify the process of loading, modifying,
--- and playing audio files in various formats. It includes support for loading
--- audio from many sources, including PCM, DFPWM, G.711, and ADPCM codecs, as
--- well as WAV, AIFF, AU, and FLAC files. It can also generate audio on-the-fly
--- as tones, noise, or silence.
---
--- AUKit uses a structure called aukit.Audio to store information about each audio
--- chunk. An audio object holds the sample rate of the audio, as well as the
--- data for each channel stored as floating-point numbers. aukit.Audio objects can
--- hold any number of channels at any sample rate with any duration.
---
--- To obtain an audio object, you can use any of the main functions in the aukit
--- module. These allow loading from various raw codecs or file formats, with
--- data sources as strings, or tables if using a raw codec loader.
---
--- Once the audio is loaded, various basic operations are available. A subset of
--- the string library is available to simplify operations on the audio, and a
--- number of operators (+, *, .., #) are overridden as well. There's also built-
--- in functions for resampling the audio, with nearest-neighbor, linear, cubic,
--- and sinc interpolation available; as well as mixing channels (including down to
--- mono) and combining/splitting channels. Finally, audio objects can be exported
--- back to PCM, DFPWM, or WAV data, allowing changes to be easily stored on disk.
--- The stream function also automatically chunks data for use with a speaker.
--- All of these functions return a new audio object, leaving the original intact.
---
--- There are also a number of effects available for audio. These are contained
--- in the aukit.effects table, and modify the audio passed to them (as well as
--- returning the audio for streamlining). The effects are intended to speed up
--- common operations on audio. More effects may be added in future versions.
---
--- For simple audio playback tasks, the aukit.stream table provides a number of
--- functions that can quickly decode audio for real-time playback. Each function
--- returns an iterator function that can be called multiple times to obtain fully
--- decoded chunks of audio in 8-bit PCM, ready for playback to one or more
--- speakers. The functions decode the data, resample it to 48 kHz (using the
--- default resampling method), apply a low-pass filter to decrease interpolation
--- error, mix to mono if desired, and then return a list of tables with samples
--- in the range [-128, 127], plus the current position of the audio. The
--- iterators can be passed directly to the aukit.play function, which complements
--- the aukit.stream suite by playing the decoded audio on speakers while decoding
--- it in real-time, handling synchronization of speakers as best as possible.
---
--- If you're really lazy, you can also call `aukit` as a function, which takes
--- the path to a file, and plays this on all available speakers.
---
--- Be aware that processing large amounts of audio (especially loading FLAC or
--- resampling with higher quality) is *very* slow. It's recommended to use audio
--- files with lower data size (8-bit mono PCM/WAV/AIFF is ideal), and potentially
--- a lower sample rate, to reduce the load on the system - especially as all
--- data gets converted to 8-bit DFPWM data on playback anyway. The code yields
--- internally when things take a long time to avoid abort timeouts.
---
--- For an example of how to use AUKit, see the accompanying auplay.lua file.
---
--- @author JackMacWindows
---
--- @copyright
---
--- MIT License
---
--- Copyright (c) 2021-2024 JackMacWindows
---
--- Permission is hereby granted, free of charge, to any person obtaining a copy
--- of this software and associated documentation files (the "Software"), to deal
--- in the Software without restriction, including without limitation the rights
--- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--- copies of the Software, and to permit persons to whom the Software is
--- furnished to do so, subject to the following conditions:
---
--- The above copyright notice and this permission notice shall be included in all
--- copies or substantial portions of the Software.
---
--- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--- SOFTWARE.

local expect = require "cc.expect"
local dfpwm = require "cc.audio.dfpwm"

local bit32_band, bit32_bxor, bit32_lshift, bit32_rshift, bit32_arshift, bit32_btest, bit32_extract = bit32.band, bit32.bxor, bit32.lshift, bit32.rshift, bit32.arshift, bit32.btest, bit32.extract
local math_floor, math_ceil, math_sin, math_abs, math_fmod, math_min, math_max, math_pi = math.floor, math.ceil, math.sin, math.abs, math.fmod, math.min, math.max, math.pi
local os_epoch, os_queueEvent, os_pullEvent = os.epoch, os.queueEvent, os.pullEvent
local str_pack, str_unpack, str_sub, str_byte, str_rep = string.pack, string.unpack, string.sub, string.byte, string.rep
local table_pack, table_unpack, table_insert, table_remove = table.pack, table.unpack, table.insert, table.remove

--- The main AUKit module.
---@class aukit
---@field _VERSION string The version of AUKit that is loaded. This follows [SemVer](https://semver.org) format.
---@field defaultInterpolation "none"|"linear"|"cubic"|"sinc" Default interpolation mode for `Audio.resample` and other functions that need to resample.
local aukit = setmetatable({
    _VERSION = "1.10.0",
    defaultInterpolation = "linear"
}, {__call = function(aukit, path)
    expect(1, path, "string")
    local file = assert(fs.open(path, "rb"))
    local type = aukit.detect(file.read(64)) or "dfpwm"
    file.seek("set", 0)
    aukit.play(aukit.stream[type](function() return file.read(48000) end), peripheral.find("speaker"))
    file.close()
end})
--- Effects that can modify audio chunks.
---@class aukit.effects
aukit.effects = {}
--- Loader functions for streaming audio from a remote resource. These are usually used with `aukit.play`.
---@class aukit.stream
aukit.stream = {}


--- The aukit.Audio class represents a chunk of audio with variable channels and sample rate.
---@class aukit.Audio
---@field data number[][] The samples in each channel.
---@field sampleRate number The sample rate of the audio.
---@field metadata table Stores any metadata read from the file if present.
---@field info Metadata Stores any decoder-specific information, including `bitDepth` and `dataType`.
local Audio = {}
local Audio_mt

local dfpwmUUID = "3ac1fa38-811d-4361-a40d-ce53ca607cd1" -- UUID for DFPWM in WAV files

local function uuidBytes(uuid) return uuid:gsub("-", ""):gsub("%x%x", function(c) return string.char(tonumber(c, 16)) end) end

local sincWindowSize = jit and 30 or 10

local wavExtensible = {
    dfpwm = uuidBytes(dfpwmUUID),
    pcm = uuidBytes "01000000-0000-1000-8000-00aa00389b71",
    msadpcm = uuidBytes "02000000-0000-1000-8000-00aa00389b71",
    alaw = uuidBytes "06000000-0000-1000-8000-00aa00389b71",
    ulaw = uuidBytes "07000000-0000-1000-8000-00aa00389b71",
    adpcm = uuidBytes "11000000-0000-1000-8000-00aa00389b71",
    pcm_float = uuidBytes "03000000-0000-1000-8000-00aa00389b71"
}

local wavExtensibleChannels = {
    0x04,
    0x03,
    0x07,
    0x33,
    0x37,
    0x3F,
    0x637,
    0x63F,
    0x50F7,
    0x50FF,
    0x56F7,
    0x56FF
}

local ima_index_table = {
    [0] = -1, -1, -1, -1, 2, 4, 6, 8,
    -1, -1, -1, -1, 2, 4, 6, 8
}

local ima_step_table = {
    [0] = 7, 8, 9, 10, 11, 12, 13, 14, 16, 17,
    19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
    50, 55, 60, 66, 73, 80, 88, 97, 107, 118,
    130, 143, 157, 173, 190, 209, 230, 253, 279, 307,
    337, 371, 408, 449, 494, 544, 598, 658, 724, 796,
    876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066,
    2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358,
    5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
    15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767
}

local msadpcm_adaption_table = {
    [0] = 230, 230, 230, 230, 307, 409, 512, 614,
    [-8] = 768, [-7] = 614, [-6] = 512, [-5] = 409, [-4] = 307, [-3] = 230, [-2] = 230, [-1] = 230
}

local flacMetadata = {
    tracknumber = "trackNumber",
    ["encoded-by"] = "encodedBy",
    sourcemedia = "sourceMedia",
    labelno = "labelNumber",
    discnumber = "discNumber",
    partnumber = "partNumber",
    productnumber = "productNumber",
    catalognumber = "catalogNumber",
    ["release date"] = "releaseDate",
    ["source medium"] = "sourceMedium",
    ["source artist"] = "sourceArtist",
    ["guest artist"] = "guestArtist",
    ["source work"] = "sourceWork",
    disctotal = "discCount",
    tracktotal = "trackCount",
    parttotal = "partCount",
    tcm = "composer"
}

local wavMetadata = {
    IPRD = "album",
    INAM = "title",
    IART = "artist",
    IWRI = "author",
    IMUS = "composer",
    IPRO = "producer",
    IPRT = "trackNumber",
    ITRK = "trackNumber",
    IFRM = "trackCount",
    PRT1 = "partNumber",
    PRT2 = "partCount",
    TLEN = "length",
    IRTD = "rating",
    ICRD = "date",
    ITCH = "encodedBy",
    ISFT = "encoder",
    ISRF = "media",
    IGNR = "genre",
    ICMT = "comment",
    ICOP = "copyright",
    ILNG = "language"
}

local function utf8decode(str, pos)
    local codes = {utf8.codepoint(str, 1, -1)}
    for i, v in ipairs(codes) do if v > 0xFF then codes[i] = 0x3F end end
    return string.char(table_unpack(codes)), pos
end

local function clamp(n, min, max)
    if n < min then return min
    elseif n > max then return max
    else return n end
end

local function expectAudio(n, var)
    if type(var) == "table" and getmetatable(var) == Audio_mt then return var end
    expect(n, var, "Audio") -- always fails
end

local function copy(tab)
    local t = {}
    for k, v in pairs(tab) do t[k] = v end
    return t
end

local function intunpack(str, pos, sz, signed, be)
    local n = 0
    if be then for i = 0, sz - 1 do n = n * 256 + str_byte(str, pos+i) end
    else for i = 0, sz - 1 do n = n + str_byte(str, pos+i) * 2^(8*i) end end
    if signed and n >= 2^(sz*8-1) then n = n - 2^(sz*8) end
    return n, pos + sz
end

local interpolate = {
    none = function(data, x)
        return data[math_floor(x)]
    end,
    linear = function(data, x)
        local ffx = math_floor(x)
        return data[ffx] + ((data[ffx+1] or data[ffx]) - data[ffx]) * (x - ffx)
    end,
    cubic = function(data, x)
        local ffx = math_floor(x)
        local p0, p1, p2, p3, fx = data[ffx-1], data[ffx], data[ffx+1], data[ffx+2], x - ffx
        p0, p2, p3 = p0 or p1, p2 or p1, p3 or p2 or p1
        return (-0.5*p0 + 1.5*p1 - 1.5*p2 + 0.5*p3)*fx^3 + (p0 - 2.5*p1 + 2*p2 - 0.5*p3)*fx^2 + (-0.5*p0 + 0.5*p2)*fx + p1
    end,
    sinc = function(data, x)
        local ffx = math_floor(x)
        local fx = x - ffx
        local sum = 0
        for n = -sincWindowSize, sincWindowSize do
            local idx = ffx+n
            local d = data[idx]
            if d then
                local px = math_pi * (fx - n)
                if px == 0 then sum = sum + d
                else sum = sum + d * math_sin(px) / px end
            end
        end
        return sum
    end
}
local interpolation_start = {none = 1, linear = 1, cubic = 0, sinc = 0}
local interpolation_end = {none = 1, linear = 2, cubic = 3, sinc = 0}

local wavegen = {
    sine = function(x, freq, amplitude)
        return math_sin(2 * x * math_pi * freq) * amplitude
    end,
    triangle = function(x, freq, amplitude)
        return 2.0 * math_abs(amplitude * math_fmod(2.0 * x * freq + 1.5, 2.0) - amplitude) - amplitude
    end,
    square = function(x, freq, amplitude, duty)
        if (x * freq) % 1 >= duty then return -amplitude else return amplitude end
    end,
    sawtooth = function(x, freq, amplitude)
        return amplitude * math_fmod(2.0 * x * freq + 1.0, 2.0) - amplitude
    end
}

--[[
.########.##..........###.....######.
.##.......##.........##.##...##....##
.##.......##........##...##..##......
.######...##.......##.....##.##......
.##.......##.......#########.##......
.##.......##.......##.....##.##....##
.##.......########.##.....##..######.
]]

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
            if n == 0 then return 0 end
            while bitBufferLen < n do
                local temp = str_byte(data, pos)
                pos = pos + 1
                if temp == nil then return nil end
                bitBuffer = (bitBuffer * 256 + temp) % 0x100000000000
                bitBufferLen = bitBufferLen + 8
            end
            bitBufferLen = bitBufferLen - n
            local result = math_floor(bitBuffer / 2^bitBufferLen)
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
            if bit32_btest(val, 1) then return -math_floor(val / 2) - 1
            else return math_floor(val / 2) end
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
        local partitionSize = math_floor(blockSize / numPartitions);

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
            result[i + 1] = result[i + 1] + math_floor(sum / 2^shift)
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
                    local right = subframes[1][i] - math_floor(side / 2)
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

    local function decodeFrame(inp, numChannels, sampleDepth, out2, callback)
        local out = {}
        for i = 1, numChannels do out[i] = {} end
        -- Read a ton of header fields, and ignore most of them
        local temp = inp.readByte()
        if temp == nil then
            return false
        end
        local sync = temp * 64 + inp.readUint(6);
        if sync ~= 0x3FFE then error("Sync code expected") end

        inp.readUint(2);
        local blockSizeCode = inp.readUint(4);
        local sampleRateCode = inp.readUint(4);
        local chanAsgn = inp.readUint(4);
        inp.readUint(4);

        temp = inp.readUint(8);
        local t2 = -1
        for i = 7, 0, -1 do if not bit32_btest(temp, 2^i) then break end t2 = t2 + 1 end
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

        if callback then callback(out) else
            for c = 1, numChannels do
                local n = #out2[c]
                for i = 1, blockSize do out2[c][n+i] = out[c][i] end
            end
        end

        return true
    end

    function decodeFLAC(inp, callback, head)
        local out = {}
        local pos = 1
        -- Handle FLAC header and metadata blocks
        local temp temp, pos = intunpack(inp, pos, 4, false, true)
        if temp ~= 0x664C6143 then error("Invalid magic string") end
        local sampleRate, numChannels, sampleDepth, numSamples
        local last = false
        local meta = {}
        while not last do
            temp, pos = str_byte(inp, pos), pos + 1
            last = bit32_btest(temp, 0x80)
            local type = bit32_band(temp, 0x7F);
            local length length, pos = intunpack(inp, pos, 3, false, true)
            if type == 0 then  -- Stream info block
                pos = pos + 10
                sampleRate, pos = intunpack(inp, pos, 2, false, true)
                sampleRate = sampleRate * 16 + bit32_rshift(str_byte(inp, pos), 4)
                numChannels = bit32_band(bit32_rshift(str_byte(inp, pos), 1), 7) + 1;
                sampleDepth = bit32_band(str_byte(inp, pos), 1) * 16 + bit32_rshift(str_byte(inp, pos+1), 4) + 1;
                numSamples, pos = intunpack(inp, pos + 2, 4, false, true)
                numSamples = numSamples + bit32_band(str_byte(inp, pos-5), 15) * 2^32
                pos = pos + 16
            elseif type == 4 then
                local ncomments
                meta.vendor, ncomments, pos = str_unpack("<s4I4", inp, pos)
                for i = 1, ncomments do
                    local str
                    str, pos = utf8decode(str_unpack("<s4", inp, pos))
                    local k, v = str:match "^([^=]+)=(.*)$"
                    if k then meta[flacMetadata[k:lower()] or k:lower()] = v end
                end
            else
                pos = pos + length
            end
        end
        if not sampleRate then error("Stream info metadata block absent") end
        if sampleDepth % 8 ~= 0 then error("Sample depth not supported") end

        for i = 1, numChannels do out[i] = {} end

        if callback then callback(sampleRate, numSamples) end

        -- Decode FLAC audio frames and write raw samples
        if head then return {sampleRate = sampleRate, data = out, metadata = meta, info = {bitDepth = sampleDepth, dataType = "signed"}} end
        inp = BitInputStream(inp, pos)
        repeat until not decodeFrame(inp, numChannels, sampleDepth, out, callback)
        if not callback then return {sampleRate = sampleRate, data = out, metadata = meta, info = {bitDepth = sampleDepth, dataType = "signed"}} end
    end

end

--[[
....###....##.....##.########..####..#######.
...##.##...##.....##.##.....##..##..##.....##
..##...##..##.....##.##.....##..##..##.....##
.##.....##.##.....##.##.....##..##..##.....##
.#########.##.....##.##.....##..##..##.....##
.##.....##.##.....##.##.....##..##..##.....##
.##.....##..#######..########..####..#######.
]]

--- Audio
---@section Audio

---@alias Metadata {bitDepth: number|nil, dataType: string|nil}

--- Returns the length of the audio object in seconds.
---@return number _ The audio length
function Audio:len()
    return #self.data[1] / self.sampleRate
end

--- Returns the number of channels in the audio object.
---@return number _ The number of channels
function Audio:channels()
    return #self.data
end

--- Creates a new audio object with the data resampled to a different sample rate.
--- If the target rate is the same, the object is copied without modification.
---@param sampleRate number The new sample rate in Hertz
---@param interpolation? "none"|"linear"|"cubic" The interpolation mode to use
---@return aukit.Audio _ A new audio object with the resampled data
function Audio:resample(sampleRate, interpolation)
    expect(1, sampleRate, "number")
    interpolation = expect(2, interpolation, "string", "nil") or aukit.defaultInterpolation
    if not interpolate[interpolation] then error("bad argument #2 (invalid interpolation type)", 2) end
    local new = setmetatable({sampleRate = sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    local ratio = sampleRate / self.sampleRate
    local newlen = #self.data[1] * ratio
    local interp = interpolate[interpolation]
    local start = os_epoch "utc"
    for y, c in ipairs(self.data) do
        local line = {}
        for i = 1, newlen do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            local x = (i - 1) / ratio + 1
            if x % 1 == 0 then line[i] = c[x]
            else line[i] = clamp(interp(c, x), -1, 1) end
        end
        new.data[y] = line
    end
    return new
end

--- Mixes down all channels to a new mono-channel audio object.
---@return aukit.Audio _ A new audio object with the audio mixed to mono
function Audio:mono()
    local new = setmetatable({sampleRate = self.sampleRate, data = {{}}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    local ndata = new.data[1]
    local cn = #self.data
    local start = os_epoch "utc"
    for i = 1, #self.data[1] do
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
        local s = 0
        for c = 1, cn do s = s + self.data[c][i] end
        ndata[i] = s / cn
    end
    return new
end

--- Concatenates this audio object with another, adding the contents of each
--- new channel to the end of each old channel, resampling the new channels to match
--- this one (if necessary), and inserting silence in any missing channels.
---@param ... aukit.Audio The audio objects to concatenate
---@return aukit.Audio _ The new concatenated audio object
function Audio:concat(...)
    local audios = {self, ...}
    local l = {#self.data[1]}
    local cn = #self.data
    for i = 2, #audios do
        expectAudio(i-1, audios[i])
        if audios[i].sampleRate ~= self.sampleRate then audios[i] = audios[i]:resample(self.sampleRate) end
        l[i] = #audios[i].data[1]
        cn = math_max(cn, #audios[i].data)
    end
    local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    for c = 1, cn do
        local ch = {}
        local pos = 0
        for a = 1, #audios do
            local sch = audios[a].data[c]
            if sch then for i = 1, l[a] do ch[pos+i] = sch[i] end
            else for i = 1, l[a] do ch[pos+i] = 0 end end
            pos = pos + l[a]
        end
        obj.data[c] = ch
    end
    return obj
end

--- Takes a subregion of the audio and returns a new audio object with its contents.
--- This takes the same arguments as `string.sub`, but positions start at 0.
---@param start? number The start position of the audio in seconds
---@param last? number The end position of the audio in seconds (0 means end of file)
---@return aukit.Audio _ The new split audio object
function Audio:sub(start, last)
    start = math_floor(expect(1, start, "number", "nil") or 0)
    last = math_floor(expect(2, last, "number", "nil") or 0)
    local len = #self.data[1] / self.sampleRate
    if start < 0 then start = len + start end
    if last <= 0 then last = len + last end
    expect.range(start, 0, len)
    expect.range(last, 0, len)
    start, last = start * self.sampleRate + 1, last * self.sampleRate + 1
    local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    for c = 1, #self.data do
        local ch = {}
        local sch = self.data[c]
        for i = start, last do ch[i-start+1] = sch[i] end
        obj.data[c] = ch
    end
    return obj
end

--- Combines the channels of this audio object with another, adding the new
--- channels on the end of the new object, resampling the new channels to match
--- this one (if necessary), and extending any channels that are shorter than the
--- longest channel with zeroes.
---@param ... aukit.Audio The audio objects to combine with
---@return aukit.Audio _ The new combined audio object
function Audio:combine(...)
    local audios = {self, ...}
    local len = #self.data[1]
    for i = 2, #audios do
        expectAudio(i-1, audios[i])
        if audios[i].sampleRate ~= self.sampleRate then audios[i] = audios[i]:resample(self.sampleRate) end
        len = math_max(len, #audios[i].data[1])
    end
    local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    local pos = 0
    for a = 1, #audios do
        for c = 1, #audios[a].data do
            local sch, ch = audios[a].data[c], {}
            for i = 1, len do ch[i] = sch[i] or 0 end
            obj.data[pos+c] = ch
        end
        pos = pos + #audios[a].data
    end
    return obj
end

--- Splits this audio object into one or more objects with the specified channels.
--- Passing a channel that doesn't exist will throw an error.
---@param ... number[] The lists of channels in each new object
---@return aukit.Audio ... The new audio objects created from the channels in each list
---@usage Split a stereo track into independent mono objects
--
---     local left, right = stereo:split({1}, {2})
function Audio:split(...)
    local retval = {}
    for n, cl in ipairs{...} do
        expect(n, cl, "table")
        if #cl == 0 then error("bad argument #" .. n .. " (cannot use empty table)") end
        local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
        for cd, cs in ipairs(cl) do
            local sch, ch = self.data[expect(cd, cs, "number")], {}
            if not sch then error("channel " .. cs .. " (in argument " .. n .. ") out of range", 2) end
            for i = 1, #sch do ch[i] = sch[i] end
            obj.data[cd] = ch
        end
        retval[#retval+1] = obj
    end
    return table_unpack(retval)
end

--- Mixes two or more audio objects into a single object, amplifying each sample
--- with a multiplier (before clipping) if desired, and clipping any values
--- outside the audio range ([-1, 1]). Channels that are shorter are padded with
--- zeroes at the end, and non-existent channels are replaced with all zeroes.
--- Any audio objects with a different sample rate are resampled to match this one.
---@param amplifier number|Audio The multiplier to apply, or the first audio object
---@param ... aukit.Audio The objects to mix with this one
---@return aukit.Audio _ The new mixed audio object
function Audio:mix(amplifier, ...)
    local audios = {self, ...}
    local len = #self.data[1]
    local cn = #self.data
    for i = 2, #audios do
        expectAudio(i, audios[i])
        if audios[i].sampleRate ~= self.sampleRate then audios[i] = audios[i]:resample(self.sampleRate) end
        len = math_max(len, #audios[i].data[1])
        cn = math_max(cn, #audios[i].data)
    end
    if type(amplifier) ~= "number" then
        expectAudio(1, amplifier)
        if amplifier.sampleRate ~= self.sampleRate then amplifier = amplifier:resample(self.sampleRate) end
        len = math_max(len, #amplifier.data[1])
        cn = math_max(cn, #amplifier.data)
        table_insert(audios, 2, amplifier)
        amplifier = 1
    end
    local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    for c = 1, cn do
        local ch = {}
        local sch = {}
        for a = 1, #audios do sch[a] = audios[a].data[c] end
        for i = 1, len do
            local s = 0
            for a = 1, #audios do if sch[a] then s = s + (sch[a][i] or 0) end end
            ch[i] = clamp(s * amplifier, -1, 1)
        end
        obj.data[c] = ch
    end
    return obj
end

--- Returns a new audio object that repeats this audio a number of times.
---@param count number The number of times to play the audio
---@return aukit.Audio _ The repeated audio
function Audio:rep(count)
    if type(self) ~= "table" and type(count) == "table" then self, count = count, self end
    expect(1, count, "number")
    local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    for c = 1, #self.data do
        local sch, ch = self.data[c], {}
        for n = 0, count - 1 do
            local pos = n * #sch
            for i = 1, #sch do ch[pos+i] = sch[i] end
        end
        obj.data[c] = ch
    end
    return obj
end

--- Returns a reversed version of this audio.
---@return aukit.Audio _ The reversed audio
function Audio:reverse()
    local obj = setmetatable({sampleRate = self.sampleRate, data = {}, metadata = copy(self.metadata), info = copy(self.info)}, Audio_mt)
    for c = 1, #self.data do
        local sch, ch = self.data[c], {}
        local len = #sch
        for i = 1, len do ch[len-i+1] = sch[i] end
        obj.data[c] = ch
    end
    return obj
end

local function encodePCM(info, pos)
    local maxValue = 2^(info.bitDepth-1)
    local add = info.dataType == "unsigned" and maxValue or 0
    local source = info.audio.data
    local encode
    if info.dataType == "float" then encode = function(d) return d end
    else encode = function(d) return d * (d < 0 and maxValue or maxValue-1) + add end end
    local data = {}
    local nc = #source
    local len = #source[1]
    if pos > len then return nil end
    local start = os_epoch "utc"
    if info.interleaved then for n = pos, pos + info.len - 1 do if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end for c = 1, nc do data[(n-1)*nc+c] = encode(source[c][n]) end end
    elseif info.multiple then
        for c = 1, nc do
            data[c] = {}
            for n = pos, pos + info.len - 1 do
                if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
                local s = source[c][n]
                if not s then break end
                data[c][n-pos+1] = encode(s)
            end
        end
        return pos + info.len, table_unpack(data)
    else for c = 1, nc do for n = pos, pos + info.len - 1 do if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end data[(c-1)*len+n] = encode(source[c][n]) end end end
    return data
end

--- Converts the audio data to raw PCM samples.
---@param bitDepth? number The bit depth of the audio (8, 16, 24, 32)
---@param dataType? "signed"|"unsigned"|"float" The type of each sample
---@param interleaved? boolean Whether to interleave each channel
---@return number[]|nil ... The resulting audio data
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
--- This is useful as a for iterator, and can be used with `aukit.play`.
---@param chunkSize? number The size of each chunk
---@param bitDepth? number The bit depth of the audio (8, 16, 24, 32)
---@param dataType? "signed"|"unsigned"|"float" The type of each sample
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number _ The total length of the audio in seconds
function Audio:stream(chunkSize, bitDepth, dataType)
    chunkSize = expect(1, chunkSize, "number", "nil") or 131072
    bitDepth = expect(2, bitDepth, "number", "nil") or 8
    dataType = expect(3, dataType, "string", "nil") or "signed"
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    if dataType ~= "signed" and dataType ~= "unsigned" and dataType ~= "float" then error("bad argument #3 (invalid data type)", 2) end
    if dataType == "float" and bitDepth ~= 32 then error("bad argument #2 (float audio must have 32-bit depth)", 2) end
    local info, pos = {audio = self, bitDepth = bitDepth, dataType = dataType, interleaved = false, multiple = true, len = chunkSize}, 1
    return function()
        if info == nil then return nil end
        local p = pos / self.sampleRate
        local v = {encodePCM(info, pos)}
        if v[1] == nil then info = nil return nil end
        pos = table_remove(v, 1)
        return v, p
    end, #self.data[1] / self.sampleRate
end

--- Coverts the audio data to a WAV file.
---@param bitDepth? number The bit depth of the audio (1 = DFPWM, 8, 16, 24, 32)
---@return string _ The resulting WAV file data
function Audio:wav(bitDepth)
    -- TODO: Support float data
    bitDepth = expect(1, bitDepth, "number", "nil") or 16
    if bitDepth == 1 then
        local str = self:dfpwm(true)
        if self.metadata and next(self.metadata) then
            local info = {}
            for k, v in pairs(self.metadata) do
                for l, w in pairs(wavMetadata) do
                    if w == k then
                        info[#info+1] = l
                        info[#info+1] = tostring(v)
                        break
                    end
                end
            end
            local list = str_pack("!2<c4" .. ("c4s4Xh"):rep(#info / 2), "INFO", table.unpack(info))
            return str_pack("<c4Ic4c4IHHIIHHHHIc16c4IIc4s4c4I",
                "RIFF", #str + 72, "WAVE",
                "fmt ", 40, 0xFFFE, #self.data, self.sampleRate, self.sampleRate * #self.data / 8, math_ceil(#self.data / 8), 1,
                    22, 1, wavExtensibleChannels[#self.data] or 0, wavExtensible.dfpwm,
                "fact", 4, #self.data[1],
                "LIST", list,
                "data", #str) .. str
        else
            return str_pack("<c4Ic4c4IHHIIHHHHIc16c4IIc4I",
                "RIFF", #str + 72, "WAVE",
                "fmt ", 40, 0xFFFE, #self.data, self.sampleRate, self.sampleRate * #self.data / 8, math_ceil(#self.data / 8), 1,
                    22, 1, wavExtensibleChannels[#self.data] or 0, wavExtensible.dfpwm,
                "fact", 4, #self.data[1],
                "data", #str) .. str
        end
    elseif bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    local data = self:pcm(bitDepth, bitDepth == 8 and "unsigned" or "signed", true)
    local str = ""
    local csize = jit and 7680 or 32768
    local format = ((bitDepth == 8 and "I" or "i") .. (bitDepth / 8)):rep(csize)
    for i = 1, #data - csize, csize do str = str .. format:pack(table_unpack(data, i, i + csize - 1)) end
    str = str .. ((bitDepth == 8 and "I" or "i") .. (bitDepth / 8)):rep(#data % csize):pack(table_unpack(data, math_floor(#data / csize) * csize))
    if self.metadata and next(self.metadata) then
        local info = {}
        for k, v in pairs(self.metadata) do
            for l, w in pairs(wavMetadata) do
                if w == k then
                    info[#info+1] = l
                    info[#info+1] = tostring(v)
                    break
                end
            end
        end
        local list = str_pack("!2<c4" .. ("c4s4Xh"):rep(#info / 2), "INFO", table.unpack(info))
        return str_pack("<c4Ic4c4IHHIIHHc4s4c4I", "RIFF", #str + 36, "WAVE", "fmt ", 16, 1, #self.data, self.sampleRate, self.sampleRate * #self.data * bitDepth / 8, #self.data * bitDepth / 8, bitDepth, "LIST", list, "data", #str) .. str
    else
        return str_pack("<c4Ic4c4IHHIIHHc4I", "RIFF", #str + 36, "WAVE", "fmt ", 16, 1, #self.data, self.sampleRate, self.sampleRate * #self.data * bitDepth / 8, #self.data * bitDepth / 8, bitDepth, "data", #str) .. str
    end
end

--- Converts the audio data to DFPWM. All channels share the same encoder, and
--- channels are stored sequentially uninterleaved if `interleaved` is false, or
--- in one interleaved string if `interleaved` is true.
---@param interleaved? boolean Whether to interleave the channels
---@return string ... The resulting DFPWM data for each channel (only one string
--- if `interleaved` is true)
function Audio:dfpwm(interleaved)
    expect(1, interleaved, "boolean", "nil")
    if interleaved == nil then interleaved = true end
    if interleaved then
        return dfpwm.encode(self:pcm(8, "signed", true))
    else
        local channels = {self:pcm(8, "signed", false)}
        ---@type fun(samples:number[]):string
        local encode = dfpwm.make_encoder()
        for i = 1, #channels do channels[i] = encode(channels[i]) end
        ---@diagnostic disable-next-line return-type-mismatch
        return table_unpack(channels)
    end
end

Audio_mt = {__index = Audio, __add = Audio.combine, __mul = Audio.rep, __concat = Audio.concat, __len = Audio.len, __name = "Audio"}

function Audio_mt:__tostring()
    return "Audio: " .. self.sampleRate .. " Hz, " .. #self.data .. " channels, " .. (#self.data[1] / self.sampleRate) .. " seconds"
end

--[[
....###....##.....##.##....##.####.########
...##.##...##.....##.##...##...##.....##...
..##...##..##.....##.##..##....##.....##...
.##.....##.##.....##.#####.....##.....##...
.#########.##.....##.##..##....##.....##...
.##.....##.##.....##.##...##...##.....##...
.##.....##..#######..##....##.####....##...
]]

--- aukit
--- @section aukit

--- Creates a new audio object from the specified raw PCM data.
---@param data string|table The audio data, either as a raw string, or a table
--- of values (in the format specified by `bitDepth` and `dataType`)
---@param bitDepth? number The bit depth of the audio (8, 16, 24, 32); if `dataType` is "float" then this must be 32
---@param dataType? "signed"|"unsigned"|"float" The type of each sample
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param interleaved? boolean Whether each channel is interleaved or separate
---@param bigEndian? boolean Whether the audio is big-endian or little-endian; ignored if data is a table
---@return aukit.Audio _ A new audio object containing the specified data
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
    local csize = jit and 7680 or 32768
    local csizeb = csize * byteDepth
    local bitDir = bigEndian and ">" or "<"
    local sformat = dataType == "float" and "f" or ((dataType == "signed" and "i" or "I") .. byteDepth)
    local format = bitDir .. str_rep(sformat, csize)
    local maxValue = 2^(bitDepth-1)
    local obj = setmetatable({sampleRate = sampleRate, data = {}, metadata = {}, info = {bitDepth = bitDepth, dataType = dataType}}, Audio_mt)
    for i = 1, channels do obj.data[i] = {} end
    local pos, spos = 1, 1
    local tmp = {}
    local read
    if type(data) == "table" then
        if dataType == "signed" then
            function read()
                local s = data[pos]
                pos = pos + 1
                return s / (s < 0 and maxValue or maxValue-1)
            end
        elseif dataType == "unsigned" then
            function read()
                local s = data[pos]
                pos = pos + 1
                return (s - 128) / (s < 128 and maxValue or maxValue-1)
            end
        else
            function read()
                local s = data[pos]
                pos = pos + 1
                return s
            end
        end
    elseif dataType == "float" then
        function read()
            if pos > #tmp then
                if spos + csizeb > #data then
                    local f = bitDir .. str_rep(sformat, (#data - spos + 1) / byteDepth)
                    tmp = {str_unpack(f, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                else
                    tmp = {str_unpack(format, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                end
                pos = 1
            end
            local s = tmp[pos]
            pos = pos + 1
            return s
        end
    elseif dataType == "signed" then
        function read()
            if pos > #tmp then
                if spos + csizeb > #data then
                    local f = bitDir .. str_rep(sformat, (#data - spos + 1) / byteDepth)
                    tmp = {str_unpack(f, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                else
                    tmp = {str_unpack(format, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                end
                pos = 1
            end
            local s = tmp[pos]
            pos = pos + 1
            return s / (s < 0 and maxValue or maxValue-1)
        end
    else -- unsigned
        function read()
            if pos > #tmp then
                if spos + csizeb > #data then
                    local f = bitDir .. str_rep(sformat, (#data - spos + 1) / byteDepth)
                    tmp = {str_unpack(f, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                else
                    tmp = {str_unpack(format, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                end
                pos = 1
            end
            local s = tmp[pos]
            pos = pos + 1
            return (s - 128) / (s < 128 and maxValue or maxValue-1)
        end
    end
    local start = os_epoch "utc"
    if interleaved and channels > 1 then
        local d = obj.data
        for i = 1, len do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            for j = 1, channels do d[j][i] = read() end
        end
    else for j = 1, channels do
        local line = {}
        obj.data[j] = line
        for i = 1, len do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            line[i] = read()
        end
    end end
    return obj
end

--- Creates a new audio object from IMA ADPCM data.
---@param data string|table The audio data, either as a raw string, or a table of nibbles
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param topFirst? boolean Whether the top nibble is the first nibble
--- (true) or last (false); ignored if `data` is a table
---@param interleaved? boolean Whether each channel is interleaved or separate
---@param predictor? number|table The initial predictor value(s)
---@param step_index? number|table The initial step index(es)
---@return aukit.Audio _ A new audio object containing the decoded data
function aukit.adpcm(data, channels, sampleRate, topFirst, interleaved, predictor, step_index)
    expect(1, data, "string", "table")
    channels = expect(2, channels, "number", "nil") or 1
    sampleRate = expect(3, sampleRate, "number", "nil") or 48000
    expect(4, topFirst, "boolean", "nil")
    if topFirst == nil then topFirst = true end
    expect(5, interleaved, "boolean", "nil")
    if interleaved == nil then interleaved = true end
    predictor = expect(6, predictor, "number", "table", "nil")
    step_index = expect(7, step_index, "number", "table", "nil")
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    if predictor == nil then
        predictor = {}
        for i = 1, channels do predictor[i] = 0 end
    elseif type(predictor) == "number" then
        if channels ~= 1 then error("bad argument #6 (table too short)", 2) end
        predictor = {expect.range(predictor, -32768, 32767)}
    else
        if channels > #predictor then error("bad argument #6 (table too short)", 2) end
        for i = 1, channels do expect.range(predictor[i], -32768, 32767) end
    end
    if step_index == nil then
        step_index = {}
        for i = 1, channels do step_index[i] = 0 end
    elseif type(step_index) == "number" then
        if channels ~= 1 then error("bad argument #7 (table too short)", 2) end
        step_index = {expect.range(step_index, 0, 88)}
    else
        if channels > #step_index then error("bad argument #7 (table too short)", 2) end
        for i = 1, channels do expect.range(step_index[i], 0, 88) end
    end
    local pos = 1
    local read, tmp, len
    if type(data) == "string" then
        function read()
            if tmp then
                local v = tmp
                tmp = nil
                return v
            else
                local b = str_byte(data, pos)
                pos = pos + 1
                if topFirst then tmp, b = bit32_band(b, 0x0F), bit32_rshift(b, 4)
                else tmp, b = bit32_rshift(b, 4), bit32_band(b, 0x0F) end
                return b
            end
        end
        len = math_floor(#data * 2 / channels)
    else
        function read()
            local v = data[pos]
            pos = pos + 1
            return v
        end
        len = #data / channels
    end
    local obj = setmetatable({sampleRate = sampleRate, data = {}, metadata = {}, info = {bitDepth = 16, dataType = "signed"}}, Audio_mt)
    local step = {}
    local start = os_epoch "utc"
    if interleaved then
        local d = obj.data
        for j = 1, channels do d[j] = {} end
        for i = 1, len do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            for j = 1, channels do
                local nibble = read()
                step[j] = ima_step_table[step_index[j]]
                step_index[j] = clamp(step_index[j] + ima_index_table[nibble], 0, 88)
                local diff = bit32_rshift((nibble % 8) * step[j], 2) + bit32_rshift(step[j], 3)
                if nibble >= 8 then predictor[j] = clamp(predictor[j] - diff, -32768, 32767)
                else predictor[j] = clamp(predictor[j] + diff, -32768, 32767) end
                d[j][i] = predictor[j] / (predictor[j] < 0 and 32768 or 32767)
            end
        end
    else for j = 1, channels do
        local line = {}
        local predictor, step_index, step = predictor[j], step_index[j], nil
        for i = 1, len do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            local nibble = read()
            step = ima_step_table[step_index]
            step_index = clamp(step_index + ima_index_table[nibble], 0, 88)
            local diff = bit32_rshift((nibble % 8) * step, 2) + bit32_rshift(step, 3)
            if nibble >= 8 then predictor = clamp(predictor - diff, -32768, 32767)
            else predictor = clamp(predictor + diff, -32768, 32767) end
            line[i] = predictor / (predictor < 0 and 32768 or 32767)
        end
        obj.data[j] = line
    end end
    return obj
end

--- Creates a new audio object from Microsoft ADPCM data.
---@param data string The audio data as a raw string
---@param blockAlign number The number of bytes in each block
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param coefficients? table Two lists of coefficients to use
---@return aukit.Audio _ A new audio object containing the decoded data
function aukit.msadpcm(data, blockAlign, channels, sampleRate, coefficients)
    expect(1, data, "string")
    expect(2, blockAlign, "number")
    channels = expect(3, channels, "number", "nil") or 1
    sampleRate = expect(4, sampleRate, "number", "nil") or 48000
    expect(5, coefficients, "table", "nil")
    expect.range(sampleRate, 1)
    local coeff1, coeff2
    if coefficients then
        if type(coefficients[1]) ~= "table" then error("bad argument #5 (first entry is not a table)", 2) end
        if type(coefficients[2]) ~= "table" then error("bad argument #5 (second entry is not a table)", 2) end
        if #coefficients[1] ~= #coefficients[2] then error("bad argument #5 (lists are not the same length)", 2) end
        coeff1, coeff2 = {}, {}
        for i, v in ipairs(coefficients[1]) do
            if type(v) ~= "number" then error("bad entry #" .. i .. " in coefficient list 1 (expected number, got " .. type(v) .. ")", 2) end
            coeff1[i-1] = v
        end
        for i, v in ipairs(coefficients[2]) do
            if type(v) ~= "number" then error("bad entry #" .. i .. " in coefficient list 2 (expected number, got " .. type(v) .. ")", 2) end
            coeff2[i-1] = v
        end
    else coeff1, coeff2 = {[0] = 256, 512, 0, 192, 240, 460, 392}, {[0] = 0, -256, 0, 64, 0, -208, -232} end
    local obj = setmetatable({sampleRate = sampleRate, data = {{}, channels == 2 and {} or nil}, metadata = {}, info = {bitDepth = 16, dataType = "signed"}}, Audio_mt)
    local left, right = obj.data[1], obj.data[2]
    local start = os_epoch "utc"
    for n = 1, #data, blockAlign do
        if channels == 2 then
            local predictorIndexL, predictorIndexR, deltaL, deltaR, sample1L, sample1R, sample2L, sample2R = str_unpack("<BBhhhhhh", data, n)
            local c1L, c2L, c1R, c2R = coeff1[predictorIndexL], coeff2[predictorIndexL], coeff1[predictorIndexR], coeff2[predictorIndexR]
            left[#left+1] = sample2L / (sample2L < 0 and 32768 or 32767)
            left[#left+1] = sample1L / (sample1L < 0 and 32768 or 32767)
            right[#right+1] = sample2R / (sample2R < 0 and 32768 or 32767)
            right[#right+1] = sample1R / (sample1R < 0 and 32768 or 32767)
            for i = 14, blockAlign - 1 do
                local b = str_byte(data, n+i)
                local hi, lo = bit32_rshift(b, 4), bit32_band(b, 0x0F)
                if hi >= 8 then hi = hi - 16 end
                if lo >= 8 then lo = lo - 16 end
                local predictor = clamp(math_floor((sample1L * c1L + sample2L * c2L) / 256) + hi * deltaL, -32768, 32767)
                left[#left+1] = predictor / (predictor < 0 and 32768 or 32767)
                sample2L, sample1L = sample1L, predictor
                deltaL = math_max(math_floor(msadpcm_adaption_table[hi] * deltaL / 256), 16)
                predictor = clamp(math_floor((sample1R * c1R + sample2R * c2R) / 256) + lo * deltaR, -32768, 32767)
                right[#right+1] = predictor / (predictor < 0 and 32768 or 32767)
                sample2R, sample1R = sample1R, predictor
                deltaR = math_max(math_floor(msadpcm_adaption_table[lo] * deltaR / 256), 16)
            end
        elseif channels == 1 then
            local predictorIndex, delta, sample1, sample2 = str_unpack("<!1Bhhh", data)
            local c1, c2 = coeff1[predictorIndex], coeff2[predictorIndex]
            left[#left+1] = sample2 / (sample2 < 0 and 32768 or 32767)
            left[#left+1] = sample1 / (sample1 < 0 and 32768 or 32767)
            for i = 7, blockAlign - 1 do
                local b = str_byte(data, n+i)
                local hi, lo = bit32_rshift(b, 4), bit32_band(b, 0x0F)
                if hi >= 8 then hi = hi - 16 end
                if lo >= 8 then lo = lo - 16 end
                local predictor = clamp(math_floor((sample1 * c1 + sample2 * c2) / 256) + hi * delta, -32768, 32767)
                left[#left+1] = predictor / (predictor < 0 and 32768 or 32767)
                sample2, sample1 = sample1, predictor
                delta = math_max(math_floor(msadpcm_adaption_table[hi] * delta / 256), 16)
                predictor = clamp(math_floor((sample1 * c1 + sample2 * c2) / 256) + lo * delta, -32768, 32767)
                left[#left+1] = predictor / (predictor < 0 and 32768 or 32767)
                sample2, sample1 = sample1, predictor
                delta = math_max(math_floor(msadpcm_adaption_table[lo] * delta / 256), 16)
            end
        else error("Unsupported number of channels: " .. channels) end
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
    end
    return obj
end

--- Creates a new audio object from G.711 u-law/A-law data.
---@param data string The audio data as a raw string
---@param ulaw boolean Whether the audio uses u-law (true) or A-law (false).
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@return aukit.Audio _ A new audio object containing the decoded data
function aukit.g711(data, ulaw, channels, sampleRate)
    expect(1, data, "string")
    expect(2, ulaw, "boolean")
    channels = expect(3, channels, "number", "nil") or 1
    sampleRate = expect(4, sampleRate, "number", "nil") or 8000
    local retval = {}
    local csize = jit and 7680 or 32768
    local xor = ulaw and 0xFF or 0x55
    for i = 1, channels do retval[i] = {} end
    local start = os_epoch "utc"
    for i = 1, #data, csize do
        local bytes = {str_byte(data, i, i + csize - 1)}
        for j = 1, #bytes do
            local b = bit32_bxor(bytes[j], xor)
            local m, e = bit32_band(b, 0x0F), bit32_extract(b, 4, 3)
            if not ulaw and e == 0 then m = m * 4 + 2
            else m = bit32_lshift(m * 2 + 33, e) end
            if ulaw then m = m - 33 end
            retval[(i+j-2) % channels + 1][math_floor((i+j-2) / channels + 1)] = m / (bit32_btest(b, 0x80) == ulaw and -0x2000 or 0x2000)
        end
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
    end
    return setmetatable({sampleRate = sampleRate, data = retval, metadata = {bitDepth = ulaw and 14 or 13, dataType = "signed"}, info = {}}, Audio_mt)
end

--- Creates a new audio object from DFPWM1a data. All channels are expected to
--- share the same decoder, and are stored interleaved in a single stream.
---@param data string The audio data as a raw string
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@return aukit.Audio _ A new audio object containing the decoded data
function aukit.dfpwm(data, channels, sampleRate)
    expect(1, data, "string")
    channels = expect(2, channels, "number", "nil") or 1
    sampleRate = expect(3, sampleRate, "number", "nil") or 48000
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    local audio = {}
    local decoder = dfpwm.make_decoder()
    local pos = 1
    local last = 0
    local start = os_epoch "utc"
    while pos <= #data do
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
        local temp = decoder(str_sub(data, pos, pos + 6000))
        if temp == nil or #temp == 0 then break end
        for i=1,#temp do
            audio[last+i] = temp[i]
        end
        last = last + #temp
        pos = pos + 6000
    end
    return aukit.pcm(audio, 8, "signed", channels, sampleRate, true, false)
end

--- Creates a new audio object from MDFPWMv3 data.
---@param data string The audio data as a raw string
---@param head boolean Whether to only load metadata (header data) - this will not decode the audio
---@return aukit.Audio _ A new audio object containing the decoded data
function aukit.mdfpwm(data, head)
    expect(1, data, "string")
    if data:sub(1, 7) ~= "MDFPWM\3" then error("bad argument #1 (not a MDFPWM file)", 2) end
    local length, artist, title, album, pos = ("<Is1s1s1"):unpack(data, 8)
    if head then
        local obj = aukit.new(0, 2, 48000)
        obj.metadata = {artist = artist, title = title, album = album}
        return obj
    end
    local audio = {}
    local decoderL, decoderR = dfpwm.make_decoder(), dfpwm.make_decoder()
    local last = 0
    local start = os_epoch "utc"
    while pos <= #data do
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
        local tempL = decoderL(str_sub(data, pos, pos + 5999))
        if tempL == nil or #tempL == 0 then break end
        for i = 1, #tempL do audio[last+i*2-1] = tempL[i] end
        local tempR = decoderR(str_sub(data, pos + 6000, pos + 11999            ))
        if tempR == nil or #tempR == 0 then break end
        for i = 1, #tempR do audio[last+i*2] = tempR[i] end
        last = last + #tempL + #tempR
        pos = pos + 12000
    end
    for i = length * 8 + 1, #audio do audio[i] = nil end
    local obj = aukit.pcm(audio, 8, "signed", 2, 48000, true, false)
    obj.metadata = {artist = artist, title = title, album = album}
    return obj
end

--- Creates a new audio object from a WAV file. This accepts PCM files up to 32
--- bits, including float data, as well as DFPWM files [as specified here](https://gist.github.com/MCJack123/90c24b64c8e626c7f130b57e9800962c),
--- plus IMA and Microsoft ADPCM formats and G.711 u-law/A-law.
---@param data string The WAV data to load
---@param head boolean Whether to only load metadata (header data) - this will not decode the audio
---@return aukit.Audio _ A new audio object with the contents of the WAV file
function aukit.wav(data, head)
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length, dataType, blockAlign, coefficients
    local temp, pos = str_unpack("c4", data)
    if temp ~= "RIFF" then error("bad argument #1 (not a WAV file)", 2) end
    pos = pos + 4
    temp, pos = str_unpack("c4", data, pos)
    if temp ~= "WAVE" then error("bad argument #1 (not a WAV file)", 2) end
    local meta = {}
    local obj
    while pos <= #data do
        local size
        temp, size, pos = str_unpack("<c4I", data, pos)
        if temp == "fmt " then
            local chunk = str_sub(data, pos, pos + size - 1)
            pos = pos + size
            local format
            format, channels, sampleRate, blockAlign, bitDepth = str_unpack("<HHIxxxxHH", chunk)
            if format == 1 then
                dataType = bitDepth == 8 and "unsigned" or "signed"
            elseif format == 2 then
                dataType = "msadpcm"
                local numcoeff = str_unpack("<H", chunk, 21)
                if numcoeff > 0 then
                    coefficients = {{}, {}}
                    for i = 1, numcoeff do
                        coefficients[1][i], coefficients[2][i] = str_unpack("<hh", chunk, i * 4 + 19)
                    end
                end
            elseif format == 3 then
                dataType = "float"
            elseif format == 6 then
                dataType = "alaw"
            elseif format == 7 then
                dataType = "ulaw"
            elseif format == 0x11 then
                dataType = "adpcm"
            elseif format == 0xFFFE then
                bitDepth = str_unpack("<H", chunk, 19)
                local uuid = str_sub(chunk, 25, 40)
                if uuid == wavExtensible.pcm then dataType = bitDepth == 8 and "unsigned" or "signed"
                elseif uuid == wavExtensible.dfpwm then dataType = "dfpwm"
                elseif uuid == wavExtensible.msadpcm then dataType = "msadpcm"
                elseif uuid == wavExtensible.pcm_float then dataType = "float"
                elseif uuid == wavExtensible.alaw then dataType = "alaw"
                elseif uuid == wavExtensible.ulaw then dataType = "ulaw"
                elseif uuid == wavExtensible.adpcm then dataType = "adpcm"
                else error("unsupported WAV file", 2) end
            else error("unsupported WAV file", 2) end
        elseif temp == "data" then
            local data = str_sub(data, pos, pos + size - 1)
            if #data < size then error("invalid WAV file", 2) end
            if head then obj = aukit.new(0, channels, sampleRate)
            elseif dataType == "adpcm" then
                local blocks = {}
                for n = 1, #data, blockAlign do
                    if channels == 2 then
                        local predictorL, indexL, predictorR, indexR = str_unpack("<hBxhB", data, n)
                        local nibbles = {}
                        for i = 8, blockAlign - 1, 8 do
                            local b = str_byte(data, n+i)
                            nibbles[(i-7)*2-1] = bit32_band(b, 0x0F)
                            nibbles[(i-6)*2-1] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+1)
                            nibbles[(i-5)*2-1] = bit32_band(b, 0x0F)
                            nibbles[(i-4)*2-1] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+2)
                            nibbles[(i-3)*2-1] = bit32_band(b, 0x0F)
                            nibbles[(i-2)*2-1] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+3)
                            nibbles[(i-1)*2-1] = bit32_band(b, 0x0F)
                            nibbles[i*2-1] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+4)
                            nibbles[(i-7)*2] = bit32_band(b, 0x0F)
                            nibbles[(i-6)*2] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+5)
                            nibbles[(i-5)*2] = bit32_band(b, 0x0F)
                            nibbles[(i-4)*2] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+6)
                            nibbles[(i-3)*2] = bit32_band(b, 0x0F)
                            nibbles[(i-2)*2] = bit32_rshift(b, 4)
                            b = str_byte(data, n+i+7)
                            nibbles[(i-1)*2] = bit32_band(b, 0x0F)
                            nibbles[i*2] = bit32_rshift(b, 4)
                        end
                        blocks[#blocks+1] = aukit.adpcm(nibbles, channels, sampleRate, false, true, {predictorL, predictorR}, {indexL, indexR})
                    else
                        local predictor, index = str_unpack("<hB", data, n)
                        index = bit32_band(index, 0x0F)
                        blocks[#blocks+1] = aukit.adpcm(str_sub(data, n + 4, n + blockAlign - 1), channels, sampleRate, false, false, predictor, index)
                    end
                end
                obj = blocks[1]:concat(table_unpack(blocks, 2))
            elseif dataType == "msadpcm" then obj = aukit.msadpcm(data, blockAlign, channels, sampleRate, coefficients)
            elseif dataType == "alaw" or dataType == "ulaw" then obj = aukit.g711(data, dataType == "ulaw", channels, sampleRate)
            elseif dataType == "dfpwm" then obj = aukit.dfpwm(data, channels, sampleRate)
            else obj = aukit.pcm(data, bitDepth, dataType, channels, sampleRate, true, false) end
            obj.metadata = meta
            obj.info = {dataType = dataType, bitDepth = bitDepth}
            pos = pos + size
        elseif temp == "fact" then
            -- TODO
            pos = pos + size
        elseif temp == "LIST" then
            local type = str_unpack("c4", data, pos)
            if type == "INFO" then
                local e = pos + size
                pos = pos + 4
                while pos < e do
                    local str
                    type, str, pos = str_unpack("!2<c4s4Xh", data, pos)
                    if wavMetadata[type] then meta[wavMetadata[type]] = tonumber(str) or str end
                end
            else pos = pos + size end
        else pos = pos + size end
    end
    if obj then return obj end
    error("invalid WAV file", 2)
end

--- Creates a new audio object from an AIFF or AIFC file.
---@param data string The AIFF data to load
---@param head boolean Whether to only load metadata (header data) - this will not decode the audio
---@return aukit.Audio _ A new audio object with the contents of the AIFF file
function aukit.aiff(data, head)
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length, offset, compression, blockAlign
    local isAIFC = false
    local temp, pos = str_unpack("c4", data)
    if temp ~= "FORM" then error("bad argument #1 (not an AIFF file)", 2) end
    pos = pos + 4
    temp, pos = str_unpack("c4", data, pos)
    if temp == "AIFC" then isAIFC = true
    elseif temp ~= "AIFF" then error("bad argument #1 (not an AIFF file)", 2) end
    local meta = {}
    while pos <= #data do
        local size
        temp, size, pos = str_unpack(">c4I", data, pos)
        if temp == "COMM" then
            local e, m
            channels, length, bitDepth, e, m, pos = str_unpack(">hIhHI7x", data, pos)
            if isAIFC then
                local s
                compression, s, pos = str_unpack(">c4s1", data, pos)
                if #s % 2 == 0 then pos = pos + 1 end
            end
            length = length * channels * math_floor(bitDepth / 8)
            local s = bit32_btest(e, 0x8000)
            e = ((bit32_band(e, 0x7FFF) - 0x3FFE) % 0x800)
            sampleRate = math.ldexp(m * (s and -1 or 1) / 0x100000000000000, e)
        elseif temp == "SSND" then
            offset, blockAlign, pos = str_unpack(">II", data, pos)
            local data = str_sub(data, pos + offset, pos + offset + length - 1)
            --if #data < length then error("invalid AIFF file", 2) end
            local obj
            if head then obj = aukit.new(0, channels, sampleRate)
            elseif compression == nil or compression == "NONE" then obj = aukit.pcm(data, bitDepth, "signed", channels, sampleRate, true, true)
            elseif compression == "sowt" then obj = aukit.pcm(data, bitDepth, "signed", channels, sampleRate, true, false)
            elseif compression == "fl32" or compression == "FL32" then obj = aukit.pcm(data, 32, "float", channels, sampleRate, true, true)
            elseif compression == "alaw" or compression == "ulaw" or compression == "ALAW" or compression == "ULAW" then obj = aukit.g711(data, compression == "ulaw" or compression == "ULAW", channels, sampleRate)
            else error("Unsupported compression scheme " .. compression, 2) end
            obj.metadata = meta
            return obj
        elseif temp == "NAME" then
            meta.title = str_sub(data, pos, pos + size - 1)
            pos = pos + size
        elseif temp == "AUTH" then
            meta.artist = str_sub(data, pos, pos + size - 1)
            pos = pos + size
        elseif temp == "(c) " then
            meta.copyright = str_sub(data, pos, pos + size - 1)
            pos = pos + size
        elseif temp == "ANNO" then
            meta.comment = str_sub(data, pos, pos + size - 1)
            pos = pos + size
        else pos = pos + size end
    end
    error("invalid AIFF file", 2)
end

--- Creates a new audio object from an AU file.
---@param data string The AU data to load
---@return aukit.Audio _ A new audio object with the contents of the AU file
function aukit.au(data)
    expect(1, data, "string")
    local magic, offset, size, encoding, sampleRate, channels = str_unpack(">c4IIIII", data)
    if magic ~= ".snd" then error("invalid AU file", 2) end
    if encoding == 1 then return aukit.g711(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), true, channels, sampleRate)
    elseif encoding == 2 then return aukit.pcm(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 8, "signed", channels, sampleRate, true, true)
    elseif encoding == 3 then return aukit.pcm(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 16, "signed", channels, sampleRate, true, true)
    elseif encoding == 4 then return aukit.pcm(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 24, "signed", channels, sampleRate, true, true)
    elseif encoding == 5 then return aukit.pcm(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 32, "signed", channels, sampleRate, true, true)
    elseif encoding == 6 then return aukit.pcm(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 32, "float", channels, sampleRate, true, true)
    elseif encoding == 27 then return aukit.g711(str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), false, channels, sampleRate)
    else error("unsupported encoding type " .. encoding, 2) end
end

--- Creates a new audio object from a FLAC file.
---@param data string The FLAC data to load
---@param head boolean Whether to only load metadata (header data) - this will not decode the audio
---@return aukit.Audio _ A new audio object with the contents of the FLAC file
function aukit.flac(data, head)
    expect(1, data, "string")
    return setmetatable(decodeFLAC(data, nil, head), Audio_mt)
end

local qoa_dequant_tab = {
	[0] = {[0] =    1,    -1,    3,    -3,    5,    -5,     7,     -7},
	{[0] =    5,    -5,   18,   -18,   32,   -32,    49,    -49},
	{[0] =   16,   -16,   53,   -53,   95,   -95,   147,   -147},
	{[0] =   34,   -34,  113,  -113,  203,  -203,   315,   -315},
	{[0] =   63,   -63,  210,  -210,  378,  -378,   588,   -588},
	{[0] =  104,  -104,  345,  -345,  621,  -621,   966,   -966},
	{[0] =  158,  -158,  528,  -528,  950,  -950,  1477,  -1477},
	{[0] =  228,  -228,  760,  -760, 1368, -1368,  2128,  -2128},
	{[0] =  316,  -316, 1053, -1053, 1895, -1895,  2947,  -2947},
	{[0] =  422,  -422, 1405, -1405, 2529, -2529,  3934,  -3934},
	{[0] =  548,  -548, 1828, -1828, 3290, -3290,  5117,  -5117},
	{[0] =  696,  -696, 2320, -2320, 4176, -4176,  6496,  -6496},
	{[0] =  868,  -868, 2893, -2893, 5207, -5207,  8099,  -8099},
	{[0] = 1064, -1064, 3548, -3548, 6386, -6386,  9933,  -9933},
	{[0] = 1286, -1286, 4288, -4288, 7718, -7718, 12005, -12005},
	{[0] = 1536, -1536, 5120, -5120, 9216, -9216, 14336, -14336},
}

local function signed_rshift(a, b)
    local n = bit32_arshift(a, b)
    if n >= 0x80000000 then return n - 0x100000000 else return n end
end

local function qoa_lms_predict(lms)
    local w, h = lms.weights, lms.history
	return signed_rshift(w[1] * h[1] + w[2] * h[2] + w[3] * h[3] + w[4] * h[4], 13)
end

local function qoa_lms_update(lms, sample, residual)
	local delta = signed_rshift(residual, 4)
    local w, h = lms.weights, lms.history
    lms.weights = {
        w[1] + (h[1] < 0 and -delta or delta),
        w[2] + (h[2] < 0 and -delta or delta),
        w[3] + (h[3] < 0 and -delta or delta),
        w[4] + (h[4] < 0 and -delta or delta)
    }
    lms.history = {h[2], h[3], h[4], sample}
end

--- Creates a new audio object from a QOA file.
---@param data string The QOA data to load
---@return aukit.Audio _ A new audio object with the contents of the QOA file
function aukit.qoa(data)
    expect(1, data, "string")
    local magic, file_samples, pos = (">c4I4"):unpack(data)
    if magic ~= "qoaf" then error("Not a QOA file", 2) end
    local file_channels, file_sampleRate = (">BI3"):unpack(data, pos)
    local retval = setmetatable({sampleRate = file_sampleRate, data = {}, metadata = {}, info = {bitDepth = 16, dataType = "signed"}}, Audio_mt) ---@type aukit.Audio
    local lms = {}
    for i = 1, file_channels do
        retval.data[i] = {}
        lms[i] = {history = {}, weights = {}}
    end

    local start = os_epoch "utc"
    local sample_pos = 0
    while pos + 16 * file_channels + 8 <= #data and sample_pos < file_samples do
        -- from https://github.com/phoboslab/qoa/blob/master/qoa.h
        -- MIT license - Copyright (c) 2023 Dominic Szablewski

        -- Read and verify the frame header
        local channels, samplerate, samples, frame_size
        channels, samplerate, samples, frame_size, pos = (">BI3I2I2"):unpack(data, pos)

        local data_size = frame_size - 8 - 4 * 4 * channels
        local num_slices = math_floor(data_size / 8)
        local max_total_samples = num_slices * 20

        if
            channels ~= file_channels or
            samplerate ~= file_sampleRate or
            frame_size > #data - pos + 1 or
            samples * channels > max_total_samples
        then
            --error("Bad frame data", 2)
            break
        end

        -- Read the LMS state: 4 x 2 bytes history, 4 x 2 bytes weights per channel
        for c = 1, channels do
            lms[c].history = {(">i2i2i2i2"):unpack(data, pos)}
            pos = table.remove(lms[c].history)
            lms[c].weights = {(">i2i2i2i2"):unpack(data, pos)}
            pos = table.remove(lms[c].weights)
        end

        -- Decode all slices for all channels in this frame
        for sample_index = 1, samples, 20 do
            for c = 1, channels do
                local sliceH, sliceL
                sliceH, sliceL, pos = (">I4I4"):unpack(data, pos)

                local scalefactor = bit32_extract(sliceH, 28, 4)

                for si = sample_index, sample_index + 19 do
                    local predicted = qoa_lms_predict(lms[c])
                    local quantized = bit32_extract(sliceH, 25, 3)
                    local dequantized = qoa_dequant_tab[scalefactor][quantized]
                    --print(predicted, dequantized)
                    local reconstructed = math_min(math_max(predicted + dequantized, -32768), 32767)

                    retval.data[c][sample_pos + si] = reconstructed / (reconstructed < 0 and 32768 or 32767)
                    sliceH = bit32_lshift(sliceH, 3) + bit32_extract(sliceL, 29, 3)
                    sliceL = bit32_lshift(sliceL, 3)

                    qoa_lms_update(lms[c], reconstructed, dequantized)
                end
            end
        end
        sample_pos = sample_pos + samples
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
    end
    return retval
end

--- Creates a new empty audio object with the specified duration.
---@param duration number The length of the audio in seconds
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@return aukit.Audio _ The new empty audio object
function aukit.new(duration, channels, sampleRate)
    expect(1, duration, "number")
    channels = expect(2, channels, "number", "nil") or 1
    sampleRate = expect(3, sampleRate, "number", "nil") or 48000
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    local obj = setmetatable({sampleRate = sampleRate, data = {}, metadata = {}, info = {}}, Audio_mt)
    for c = 1, channels do
        local l = {}
        for i = 1, duration * sampleRate do l[i] = 0 end
        obj.data[c] = l
    end
    return obj
end

--- Creates a new audio object with a tone of the specified frequency and duration.
---@param frequency number The frequency of the tone in Hertz
---@param duration number The length of the audio in seconds
---@param amplitude? number The amplitude of the audio from 0.0 to 1.0
---@param waveType? "sine"|"triangle"|"sawtooth"|"square" The type of wave to generate
---@param duty? number The duty cycle of the square wave if selected; ignored otherwise
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@return aukit.Audio _ A new audio object with the tone
function aukit.tone(frequency, duration, amplitude, waveType, duty, channels, sampleRate)
    expect(1, frequency, "number")
    expect(2, duration, "number")
    amplitude = expect(3, amplitude, "number", "nil") or 1
    waveType = expect(4, waveType, "string", "nil") or "sine"
    duty = expect(5, duty, "number", "nil") or 0.5
    channels = expect(6, channels, "number", "nil") or 1
    sampleRate = expect(7, sampleRate, "number", "nil") or 48000
    expect.range(amplitude, 0, 1)
    local f = wavegen[waveType]
    if not f then error("bad argument #4 (invalid wave type)", 2) end
    expect.range(duty, 0, 1)
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    local obj = setmetatable({sampleRate = sampleRate, data = {}, metadata = {}, info = {}}, Audio_mt)
    for c = 1, channels do
        local l = {}
        for i = 1, duration * sampleRate do l[i] = f(i / sampleRate, frequency, amplitude, duty) end
        obj.data[c] = l
    end
    return obj
end

--- Creates a new audio object with white noise for the specified duration.
---@param duration number The length of the audio in seconds
---@param amplitude? number The amplitude of the audio from 0.0 to 1.0
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@return aukit.Audio _ A new audio object with noise
function aukit.noise(duration, amplitude, channels, sampleRate)
    expect(1, duration, "number")
    amplitude = expect(2, amplitude, "number", "nil") or 1
    channels = expect(3, channels, "number", "nil") or 1
    sampleRate = expect(4, sampleRate, "number", "nil") or 48000
    expect.range(amplitude, 0, 1)
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    local obj = setmetatable({sampleRate = sampleRate, data = {}, metadata = {}, info = {}}, Audio_mt)
    local random = math.random
    for c = 1, channels do
        local l = {}
        for i = 1, duration * sampleRate do l[i] = (random() * 2 - 1) * amplitude end
        obj.data[c] = l
    end
    return obj
end

--- Packs a table with PCM data into a string using the specified data type.
---@param data number[] The PCM data to pack
---@param bitDepth? number The bit depth of the audio (8, 16, 24, 32); if `dataType` is "float" then this must be 32
---@param dataType? "signed"|"unsigned"|"float" The type of each sample
---@param bigEndian? boolean Whether the data should be big-endian or little-endian
---@return string _ The packed PCM data
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
    local formatChunk = str_sub(format, 1, 1) .. str_sub(format, 2):rep(512)
    local retval = ""
    for i = 1, #data, 512 do
        if #data < i + 512 then retval = retval .. str_pack(str_rep(format, #data % 512), table_unpack(data, i, #data))
        else retval = retval .. str_pack(formatChunk, table_unpack(data, i, i+511)) end
    end
    return retval
end

---@alias speaker {playAudio: fun(samples: number[], volume?: number)}

--- Plays back stream functions created by one of the `aukit.stream` functions
--- or `Audio.stream`.
---@param callback fun():number[][]|nil The iterator function that returns each chunk
---@param progress? fun(pos:number) A callback to report progress to
--- the caller; if omitted then this argument is the first speaker
---@param volume? number The volume to play the audio at; if omitted then
--- this argument is the second speaker (if provided)
---@param ... speaker The speakers to play on
function aukit.play(callback, progress, volume, ...)
    expect(1, callback, "function")
    expect(2, progress, "function", "table")
    expect(3, volume, "number", "table", "nil")
    local speakers = {...}
    if type(volume) == "table" then
        table_insert(speakers, 1, volume)
        volume = nil
    end
    if type(progress) == "table" then
        table_insert(speakers, 1, progress)
        progress = nil
    end
    if #speakers == 0 then error("bad argument #2 (expected speakers, got nil)", 2) end
    local chunks = {}
    local complete = false
    local a, b = coroutine.create(function()
        for chunk, pos in callback do chunks[#chunks+1] = {chunk, pos} coroutine.yield(speakers) end
        complete = true
    end), coroutine.create(function()
        while not complete or #chunks > 0 do
            while not chunks[1] do if complete then return end coroutine.yield(speakers) end
            local pchunk = table_remove(chunks, 1)
            local fn = {}
            if progress then progress(pchunk[2]) end
            pchunk = pchunk[1]
            local chunklist = {}
            if #pchunk[1] < 96000 then chunklist = {pchunk}
            else
                for i = 0, #pchunk[1] - 1, 48000 do
                    local chunk = {}
                    chunklist[#chunklist+1] = chunk
                    for j = 1, #pchunk do
                        local s, c = pchunk[j], {}
                        chunk[j] = c
                        for k = 1, 48000 do c[k] = s[k+i] end
                    end
                end
            end
            for _, chunk in ipairs(chunklist) do
                for i, v in ipairs(speakers) do fn[i] = function()
                    local name = peripheral.getName(v)
                    if _HOST:find("CraftOS-PC v2.6.4") and config and not config.get("standardsMode") then
                        v.playAudio(chunk[i] or chunk[1], volume)
                        repeat until select(2, os_pullEvent("speaker_audio_empty")) == name
                    else while not v.playAudio(chunk[i] or chunk[1], volume) do
                        repeat until select(2, os_pullEvent("speaker_audio_empty")) == name
                    end end
                end end
                parallel.waitForAll(table_unpack(fn))
            end
        end
    end)
    local ok, af, bf
    local aq, bq = {{}}, {{}}
    repeat
        if #aq > 0 then
            local event = table_remove(aq, 1)
            if af == speakers then
                af = nil
                table_insert(aq, 1, event)
            end
            if af == nil or event[1] == af then
                ok, af = coroutine.resume(a, table_unpack(event, 1, event.n))
                if not ok then error(af, 2) end
            end
        end
        if #bq > 0 then
            local event = table_remove(bq, 1)
            if bf == speakers then
                bf = nil
                table_insert(bq, 1, event)
            end
            if bf == nil or event[1] == bf then
                ok, bf = coroutine.resume(b, table_unpack(event, 1, event.n))
                if not ok then error(bf, 2) end
            end
        end
        if coroutine.status(b) == "suspended" and (#aq == 0 or #bq == 0) then
            if af ~= nil and bf ~= nil then
                local event = table_pack(os_pullEvent())
                aq[#aq+1] = event
                bq[#bq+1] = event
            else
                os_queueEvent("__queue_end")
                while true do
                    local event = table_pack(os_pullEvent())
                    if event[1] == "__queue_end" then break end
                    aq[#aq+1] = event
                    bq[#bq+1] = event
                end
            end
        end
    until coroutine.status(b) == "dead" or complete
    while coroutine.status(b) == "suspended" and #bq > 0 do
        local event = table_remove(bq, 1)
        if bf == nil or event[1] == bf then
            ok, bf = coroutine.resume(b, table_unpack(event, 1, event.n))
            if not ok then error(bf, 2) end
        end
    end
    while coroutine.status(b) == "suspended" do
        ok, bf = coroutine.resume(b, os_pullEvent())
        if not ok then error(bf, 2) end
    end
end

--- An asynchronous player created by `aukit.player`.
---@class aukit.Player
---@field isPaused boolean Whether the player is paused.
---@field position number The current position of the audio, in seconds
---@field volume number The volume of the audio
---@field loaderTask Task The task that is loading the audio
---@field playerTask Task The task that is playing the audio
---@field speakers speaker[] The speakers the audio is playing on
---@field lastPlayback {pos:number,time:number} Internal playback positions for pausing
---@field invalidate boolean Internal flag to invalidate current chunk
local Player = {}
local Player_mt = {__index = Player, __name = "aukit.player"}

--- Pauses the player if it's playing.
function Player:pause()
    if self.isPaused then return end
    if not self.playerTask then error("Player is stopped", 2) end
    local time = os_epoch "utc"
    self.isPaused = true
    self.invalidate = true
    for _, v in ipairs(self.speakers) do v.stop() end
    self.position = (self.lastPlayback.pos + ((time - self.lastPlayback.time) * 48) - 1) / 48000
end

--- Plays the audio if it's paused.
function Player:play()
    if not self.isPaused then return end
    if not self.playerTask then error("Player is stopped", 2) end
    self.isPaused = false
end

--- Returns the estimated current position of audio playback.
---@return number pos The position of the audio, in seconds
function Player:livePosition()
    if not self.playerTask then error("Player is stopped", 2) end
    if not self.lastPlayback then return 0 end
    if self.isPaused then return self.position end
    return (self.lastPlayback.pos + ((os_epoch "utc" - self.lastPlayback.time) * 48) - 1) / 48000
end

--- Seeks the current audio playback position. If the requested position isn't
--- loaded yet, the player will hang until the audio is loaded. (If the position
--- is out of bounds, the player will not continue.)
---@param pos number The position to seek to, in seconds
function Player:seek(pos)
    expect.range(pos, 0)
    if not self.playerTask then error("Player is stopped", 2) end
    self.position = pos
    self.invalidate = true
    for _, v in ipairs(self.speakers) do v.stop() end
end

--- Stops the audio playback. This kills all tasks and invalidates the player.
function Player:stop()
    if not self.playerTask then error("Player is stopped", 2) end
    self.playerTask:remove()
    if self.loaderTask then self.loaderTask:remove() end
    self.playerTask = nil
    self.loaderTask = nil
end

--- Creates a player object that runs asynchronously. This requires the
--- [Taskmaster](https://gist.github.com/MCJack123/1678fb2c240052f1480b07e9053d4537)
--- library to function.
---@param loop Taskmaster The Taskmaster loop to start the player on
---@param callback fun():number[][]|nil The iterator function that returns each chunk
---@param volume? number The volume to play the audio at; if omitted then
--- this argument is the second speaker (if provided)
---@param ... speaker The speakers to play on
---@return aukit.Player player The player object to control playback with
function aukit.player(loop, callback, volume, ...)
    expect(1, loop, "table")
    expect(2, callback, "function")
    expect(3, volume, "number", "table")
    local speakers = {...}
    if type(volume) == "table" then
        table_insert(speakers, 1, volume)
        volume = nil
    end
    if #speakers == 0 then error("bad argument #3 (expected speakers, got nil)", 2) end
    local player = setmetatable({
        isPaused = false,
        position = 0,
        volume = volume,
        speakers = speakers
    }, Player_mt)
    local decoded = {}
    local id = tostring(player)
    player.loaderTask = loop:addTask(function()
        local n = 0
        for chunk, pos in callback do
            for j = 1, #chunk do
                local c, d = chunk[j], decoded[j]
                if not d then d = {} decoded[j] = d end
                for i = 1, #c do
                    d[n+i] = c[i]
                end
            end
            n = n + #chunk[1]
            os_queueEvent("aukit.loader_next", id)
            repeat local _, param = os_pullEvent("aukit.loader_next")
            until param == id
        end
        player.loaderTask = nil
    end)
    player.playerTask = loop:addTask(function()
        while true do
            local spos = math_floor(player.position * 48000) + 1
            if not player.isPaused and decoded[1] and decoded[1][spos] then
                local chunk = {}
                for j = 1, #decoded do
                    chunk[j] = {table_unpack(decoded[j], spos, math_min(spos + 47999, #decoded[j]))}
                end
                player.position = player.position + #chunk[1] / 48000
                local fn = {}
                for i, v in ipairs(speakers) do fn[i] = function()
                    local name = peripheral.getName(v)
                    if _HOST:find("CraftOS-PC v2.6.4") and config and not config.get("standardsMode") then
                        v.playAudio(chunk[i] or chunk[1], volume)
                        repeat until select(2, os_pullEvent("speaker_audio_empty")) == name
                    else while not v.playAudio(chunk[i] or chunk[1], volume) do
                        repeat until select(2, os_pullEvent("speaker_audio_empty")) == name
                        if player.invalidate then chunk = nil return end
                    end end
                    player.lastPlayback = {time = os.epoch "utc", pos = spos}
                end end
                parallel.waitForAll(table_unpack(fn))
                player.invalidate = false
            elseif not player.isPaused and player.loaderTask == nil and spos == #decoded[1] + 1 then
                player.playerTask = nil
                return
            else
                os_pullEvent()
            end
        end
    end)
    return player
end

local datafmts = {
    {"bbbbbbbb", 8, "signed"},
    {"BBBBBBBB", 8, "unsigned"},
    {"hhhhhhhh", 16, "signed"},
    {"iiiiiiii", 32, "signed"},
    {"ffffffff", 32, "float"},
    {"i3i3i3i3i3i3i3i3", 24, "signed"},
    {"IIIIIIII", 32, "unsigned"},
    {"I3I3I3I3I3I3I3I3", 24, "unsigned"},
    {"HHHHHHHH", 16, "unsigned"},
}

--- Detect the type of audio file from the specified data. This uses heuristic
--- detection methods to attempt to find the correct data type for files without
--- headers. It is not recommended to rely on the data type/bit depth reported
--- for PCM files - they are merely a suggestion.
---@param data string The audio file to check
---@return string|nil type The type of audio file detected, or `nil` if none could be found ("pcm"|"dfpwm"|"mdfpwm"|"wav"|"aiff"|"au"|"flac")
---@return number|nil bitDepth The bit depth for PCM data, if the type is "pcm" and the bit depth can be detected
---@return string|nil dataType The data type for PCM data, if the type is "pcm" and the type can be detected ("signed"|"unsigned"|"float")
function aukit.detect(data)
    expect(1, data, "string")
    if data:match "^RIFF....WAVE" then return "wav"
    elseif data:match "^FORM....AIF[FC]" then return "aiff"
    elseif data:match "^%.snd" then return "au"
    elseif data:match "^fLaC" then return "flac"
    elseif data:match "^MDFPWM\3" then return "mdfpwm"
    elseif data:match "^qoaf" then return "qoa"
    else
        -- Detect data type otherwise
        -- This expects the start or end of the audio to be (near) silence
        for _, bits in pairs(datafmts) do
            local mid, gap = bits[3] == "unsigned" and 2^(bits[2]-1) or 0, bits[3] == "float" and 0.001 or 8 * 2^(bits[2]-8)
            local nums = {pcall(str_unpack, bits[1], data)}
            nums[#nums] = nil
            if table_remove(nums, 1) then
                local allzero, ok = true, true
                for _, v in ipairs(nums) do
                    if v ~= mid then allzero = false end
                    if v < mid - gap or v > mid + gap then ok = false break end
                end
                ---@diagnostic disable-next-line return-type-mismatch
                if ok and not allzero then return "pcm", table_unpack(bits, 2) end
            end
            nums = {pcall(str_unpack, bits[1], data, #data - bits[2])}
            nums[#nums] = nil
            if table_remove(nums, 1) then
                local allzero, ok = true, true
                for _, v in ipairs(nums) do
                    if v ~= mid then allzero = false end
                    if v < mid - gap or v > mid + gap then ok = false break end
                end
                ---@diagnostic disable-next-line return-type-mismatch
                if ok and not allzero then return "pcm", table_unpack(bits, 2) end
            end
        end
        if data:match(("\x55"):rep(12)) or data:match(("\xAA"):rep(12)) then return "dfpwm" end
    end
    return nil
end

--[[
..######..########.########..########....###....##.....##
.##....##....##....##.....##.##.........##.##...###...###
.##..........##....##.....##.##........##...##..####.####
..######.....##....########..######...##.....##.##.###.##
.......##....##....##...##...##.......#########.##.....##
.##....##....##....##....##..##.......##.....##.##.....##
..######.....##....##.....##.########.##.....##.##.....##
]]

--- aukit.stream
---@section aukit.stream

--- Returns an iterator to stream raw PCM audio in CC format. aukit.Audio will
--- automatically be resampled to 48 kHz, and optionally mixed down to mono. Data
--- *must* be interleaved - this will not work with planar audio.
---@param data string|table|function The audio data, either as a raw string, a
--- table of values (in the format specified by `bitDepth` and `dataType`), or a
--- function that returns either of those types. Functions will be called at
--- least once before returning to get the type of data to use.
---@param bitDepth? number The bit depth of the audio (8, 16, 24, 32); if `dataType` is "float" then this must be 32
---@param dataType? "signed"|"unsigned"|"float" The type of each sample
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param bigEndian? boolean Whether the audio is big-endian or little-endian; ignored if data is a table
---@param mono? boolean Whether to mix the audio down to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number _ The total length of the audio in seconds, or the length of
--- the first chunk if using a function
function aukit.stream.pcm(data, bitDepth, dataType, channels, sampleRate, bigEndian, mono)
    local fn, complete
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string", "table")
    bitDepth = expect(2, bitDepth, "number", "nil") or 8
    dataType = expect(3, dataType, "string", "nil") or "signed"
    channels = expect(4, channels, "number", "nil") or 1
    sampleRate = expect(5, sampleRate, "number", "nil") or 48000
    expect(6, bigEndian, "boolean", "nil")
    expect(7, mono, "boolean", "nil")
    if bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    if dataType ~= "signed" and dataType ~= "unsigned" and dataType ~= "float" then error("bad argument #3 (invalid data type)", 2) end
    if dataType == "float" and bitDepth ~= 32 then error("bad argument #2 (float audio must have 32-bit depth)", 2) end
    expect.range(channels, 1)
    expect.range(sampleRate, 1)
    if channels == 1 then mono = false end
    local byteDepth = bitDepth / 8
    local len = (#data / (type(data) == "table" and 1 or byteDepth)) / channels
    local csize = jit and 7680 or 32768
    local csizeb = csize * byteDepth
    local bitDir = bigEndian and ">" or "<"
    local sformat = dataType == "float" and "f" or ((dataType == "signed" and "i" or "I") .. byteDepth)
    local format = bitDir .. str_rep(sformat, csize)
    local maxValue = 2^(bitDepth-1)
    local pos, spos = 1, 1
    local tmp = {}
    local read
    if type(data) == "table" then
        if dataType == "signed" then
            function read()
                if complete then return nil end
                if fn and pos > #data then
                    data, pos = fn(), 1
                    if not data then complete = true return nil end
                end
                local s = data[pos]
                pos = pos + 1
                return s / (s < 0 and maxValue or maxValue-1)
            end
        elseif dataType == "unsigned" then
            function read()
                if complete then return nil end
                if fn and pos > #data then
                    data, pos = fn(), 1
                    if not data then complete = true return nil end
                end
                local s = data[pos]
                pos = pos + 1
                return (s - 128) / (s < 128 and maxValue or maxValue-1)
            end
        else
            function read()
                if complete then return nil end
                if fn and pos > #data then
                    data, pos = fn(), 1
                    if not data then complete = true return nil end
                end
                local s = data[pos]
                pos = pos + 1
                return s
            end
        end
    elseif dataType == "float" then
        function read()
            if complete then return nil end
            if pos > #tmp then
                if fn and spos > #data then
                    data, spos = fn(), 1
                    if not data then complete = true return nil end
                end
                if spos + csizeb > #data then
                    local f = bitDir .. str_rep(sformat, (#data - spos + 1) / byteDepth)
                    tmp = {str_unpack(f, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                else
                    tmp = {str_unpack(format, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                end
                pos = 1
            end
            local s = tmp[pos]
            pos = pos + 1
            return s
        end
    elseif dataType == "signed" then
        function read()
            if complete then return nil end
            if pos > #tmp then
                if fn and spos > #data then
                    data, spos = fn(), 1
                    if not data then complete = true return nil end
                end
                if spos + csizeb > #data then
                    local f = bitDir .. str_rep(sformat, (#data - spos + 1) / byteDepth)
                    tmp = {str_unpack(f, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                else
                    tmp = {str_unpack(format, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                end
                pos = 1
            end
            local s = tmp[pos]
            pos = pos + 1
            return s / (s < 0 and maxValue or maxValue-1)
        end
    else -- unsigned
        function read()
            if complete then return nil end
            if pos > #tmp then
                if fn and spos > #data then
                    data, spos = fn(), 1
                    if not data then complete = true return nil end
                end
                if spos + csizeb > #data then
                    local f = bitDir .. str_rep(sformat, (#data - spos + 1) / byteDepth)
                    tmp = {str_unpack(f, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                else
                    tmp = {str_unpack(format, data, spos)}
                    spos = tmp[#tmp]
                    tmp[#tmp] = nil
                end
                pos = 1
            end
            local s = tmp[pos]
            pos = pos + 1
            return (s - 128) / (s < 128 and maxValue or maxValue-1)
        end
    end
    local d = {}
    local ratio = 48000 / sampleRate
    local lp_alpha = 1 - math.exp(-(sampleRate / 96000) * 2 * math_pi)
    local interp = interpolate[aukit.defaultInterpolation]
    for j = 1, (mono and 1 or channels) do d[j] = setmetatable({}, {__index = function(self, i)
        if mono then for _ = 1, channels do self[i] = (rawget(self, i) or 0) + read() end self[i] = self[i] / channels
        else self[i] = read() end
        return rawget(self, i)
    end}) end
    local n = 0
    local ok = true
    return function()
        if not ok or complete then return nil end
        for i = (n == 0 and interpolation_start[aukit.defaultInterpolation] or 1), interpolation_end[aukit.defaultInterpolation] do
            if mono then
                local s = 0
                for j = 1, channels do
                    local c = read()
                    if not c then return nil end
                    s = s + c
                end
                d[1][i] = s / channels
            else for j = 1, channels do d[j][i] = read() if not d[j][i] then return nil end end end
        end
        local chunk = {}
        for j = 1, #d do chunk[j] = {} end
        ok = pcall(function()
            local ls = {}
            for y = 1, #d do
                local s = chunk[y][0] or 0
                ls[y] = s / (s < 0 and 128 or 127)
            end
            for i = 1, 48000 do
                for y = 1, #d do
                    local x = ((i - 1) / ratio) + 1
                    local s
                    if x % 1 == 0 then s = d[y][x]
                    else s = interp(d[y], x) end
                    local ns = ls[y] + lp_alpha * (s - ls[y])
                    chunk[y][i] = clamp(ns * (ns < 0 and 128 or 127), -128, 127)
                    ls[y] = s
                end
            end
        end)
        if #chunk[1] == 0 then return nil end
        n = n + #chunk[1]
        for y = 1, #d do
            if aukit.defaultInterpolation == "sinc" then
                local t, l = {}, #d[y]
                for i = -sincWindowSize, 0 do
                    t[i] = d[y][l + i]
                end
                d[y] = setmetatable(t, getmetatable(d[y]))
            else
                local l2, l1 = d[y][#d[y]-1], d[y][#d[y]]
                d[y] = setmetatable({}, getmetatable(d[y]))
                d[y][-1], d[y][0] = l2, l1
            end
        end
        return chunk, (n - #chunk[1]) / 48000
    end, len / sampleRate
end

--- Returns an iterator to stream audio from DFPWM data. aukit.Audio will automatically
--- be resampled to 48 kHz. Multiple channels are expected to be interleaved in
--- the encoded DFPWM data.
---@param data string|fun():string The DFPWM data to decode, or a function
--- returning chunks to decode
---@param sampleRate? number The sample rate of the audio in Hertz
---@param channels? number The number of channels present in the audio
---@param mono? boolean Whether to mix the audio down to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number|nil _ The total length of the audio in seconds, or nil if data
--- is a function
function aukit.stream.dfpwm(data, sampleRate, channels, mono)
    expect(1, data, "string", "function")
    sampleRate = expect(2, sampleRate, "number", "nil") or 48000
    channels = expect(3, channels, "number", "nil") or 1
    expect.range(sampleRate, 1)
    expect.range(channels, 1)
    if channels == 1 then mono = false end
    local decoder = dfpwm.make_decoder()
    local pos = 1
    local last = 0
    local isstr = type(data) == "string"
    local buf = ""
    return function()
        local d
        if isstr then
            if pos > #data then return nil end
            d = str_sub(data, pos, pos + 6000 * channels)
        else
            while #buf < sampleRate / 8 * channels do
                local chunk = data()
                if not chunk then
                    if #buf == 0 then return nil
                    else break end
                end
                buf = buf .. chunk
            end
            d = str_sub(buf, 1, 6000 * channels)
            buf = str_sub(buf, 6000 * channels + 1)
        end
        local audio = decoder(d)
        if audio == nil or #audio == 0 then return nil end
        audio[0], last = last, audio[#audio]
        os_queueEvent("nosleep")
        repeat until "nosleep" == os_pullEvent()
        local ratio = 48000 / sampleRate
        local newlen = #audio * ratio
        local interp = interpolate[aukit.defaultInterpolation]
        local lines = {{}}
        if not mono then for j = 1, channels do lines[j] = {} end end
        for i = 1, newlen, channels do
            local n = 0
            for j = 1, channels do
                local x = (i - 1) / ratio + 1
                local s
                if x % 1 == 0 then s = audio[x]
                else s = clamp(interp(audio, x), -128, 127) end
                if mono then n = n + s
                else lines[j][math_ceil(i / channels)] = s end
            end
            if mono then lines[1][math_ceil(i / channels)] = n / channels end
        end
        os_queueEvent("nosleep")
        repeat until "nosleep" == os_pullEvent()
        local p = pos
        pos = pos + 6000 * channels
        return lines, p * 8 / sampleRate / channels
    end, isstr and #data * 8 / sampleRate / channels or nil
end

--- Returns an iterator to stream audio from MDFPWMv3 data.
---@param data string|fun():string The MDFPWM data to decode, or a function
--- returning chunks to decode
---@param mono? boolean Whether to mix the audio down to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number|nil _ The total length of the audio in seconds, or nil if data
--- is a function
function aukit.stream.mdfpwm(data, mono)
    expect(1, data, "string", "function")
    local decoderL, decoderR = dfpwm.make_decoder(), dfpwm.make_decoder()
    local isstr = type(data) == "string"
    local pos = 1
    local headerSize = 0
    local length
    local buf = ""
    local _
    if isstr then
        if data:sub(1, 7) ~= "MDFPWM\3" then error("bad argument #1 (invalid MDFPWM data)", 2) end
        length, _, _, _, pos = str_unpack("<Is1s1s1", data, 8)
        headerSize = pos - 1
    else
        repeat buf = buf .. data()
        until pcall(string.unpack, "<c7Is1s1s1", buf)
        if str_sub(buf, 1, 7) ~= "MDFPWM\3" then error("bad argument #1 (invalid MDFPWM data)", 2) end
        length, _, _, _, pos = str_unpack("<Is1s1s1", buf, 8)
        buf = str_sub(buf, pos)
        pos = 1
    end
    return function()
        local dL, dR
        if isstr then
            if pos > #data then return nil end
            dL = str_sub(data, pos, pos + 5999)
            dR = str_sub(data, pos + 6000, pos + 11999)
        else
            while #buf < 12000 do
                local chunk = data()
                if not chunk then
                    if #buf == 0 then return nil
                    else break end
                end
                buf = buf .. chunk
            end
            dL = str_sub(buf, 1, 6000)
            dR = str_sub(buf, 6001, 12000)
            buf = str_sub(buf, 12001)
        end
        local audioL = decoderL(dL)
        if audioL == nil or #audioL == 0 then return nil end
        local audioR = decoderR(dR)
        if audioR == nil or #audioR == 0 then return nil end
        os_queueEvent("nosleep")
        repeat until "nosleep" == os_pullEvent()
        if pos - headerSize + 12000 > length then
            for i = (length / 2) % 6000 + 1, 6000 do
                audioL[i], audioR[i] = nil
            end
        end
        local lines
        if mono then
            local l = {}
            lines = {l}
            for i = 1, 48000 do
                l[i] = clamp(math_floor(audioL[i] + audioR[i] / 2), -128, 127)
            end
        else lines = {audioL, audioR} end
        os_queueEvent("nosleep")
        repeat until "nosleep" == os_pullEvent()
        local p = pos - headerSize
        pos = pos + #dL + #dR
        return lines, p / 12000
    end, length / 12000
end

--- Returns an iterator to stream audio from Microsoft ADPCM data. aukit.Audio will
--- automatically be resampled to 48 kHz.
---@param input string|fun():string The audio data as a raw string or
--- reader function
---@param blockAlign number The number of bytes in each block
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param mono? boolean Whether to mix the audio down to mono
---@param coefficients? table Two lists of coefficients to use
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number|nil _ The total length of the audio in seconds, or nil if data
--- is a function
function aukit.stream.msadpcm(input, blockAlign, channels, sampleRate, mono, coefficients)
    expect(1, input, "string", "function")
    expect(2, blockAlign, "number")
    channels = expect(3, channels, "number", "nil") or 1
    sampleRate = expect(4, sampleRate, "number", "nil") or 48000
    expect(5, mono, "boolean", "nil")
    expect(6, coefficients, "table", "nil")
    expect.range(sampleRate, 1)
    local isfunc = type(input) == "function"
    local coeff1, coeff2
    if coefficients then
        if type(coefficients[1]) ~= "table" then error("bad argument #5 (first entry is not a table)", 2) end
        if type(coefficients[2]) ~= "table" then error("bad argument #5 (second entry is not a table)", 2) end
        if #coefficients[1] ~= #coefficients[2] then error("bad argument #5 (lists are not the same length)", 2) end
        coeff1, coeff2 = {}, {}
        for i, v in ipairs(coefficients[1]) do
            if type(v) ~= "number" then error("bad entry #" .. i .. " in coefficient list 1 (expected number, got " .. type(v) .. ")", 2) end
            coeff1[i-1] = v
        end
        for i, v in ipairs(coefficients[2]) do
            if type(v) ~= "number" then error("bad entry #" .. i .. " in coefficient list 2 (expected number, got " .. type(v) .. ")", 2) end
            coeff2[i-1] = v
        end
    else coeff1, coeff2 = {[0] = 256, 512, 0, 192, 240, 460, 392}, {[0] = 0, -256, 0, 64, 0, -208, -232} end
    local ratio = 48000 / sampleRate
    local interp = interpolate[aukit.defaultInterpolation]
    local n, pos = 1, 0
    local data = isfunc and input() or input
    if channels == 2 then
        local samplesPerBlock = blockAlign - 14
        local iterPerSecond = math_ceil(sampleRate / samplesPerBlock)
        local bytesPerSecond = blockAlign * iterPerSecond
        local newlen = math_floor(samplesPerBlock * ratio)
        local lastL, lastR
        return function()
            if data == nil then return nil end
            local target = n + bytesPerSecond
            local retval = {{}, not mono and {} or nil}
            local rp = 0
            local start = os_epoch "utc"
            while n < target do
                if os_epoch "utc" - start > 3000 then
                    os_queueEvent("nosleep")
                    repeat until "nosleep" == os_pullEvent()
                    start = os_epoch "utc"
                end
                if isfunc and n > #data then
                    pos = pos + #data
                    n = n - #data
                    data = input()
                    if data == nil then return nil end
                end
                if n > #data then break end
                local left, right = {}, {}
                if lastL then for i = 1, #lastL do
                    left[i-#lastL-1] = lastL[i]
                    right[i-#lastR-1] = lastR[i]
                end end
                local predictorIndexL, predictorIndexR, deltaL, deltaR, sample1L, sample1R, sample2L, sample2R = str_unpack("<BBhhhhhh", data, n)
                local c1L, c2L, c1R, c2R = coeff1[predictorIndexL], coeff2[predictorIndexL], coeff1[predictorIndexR], coeff2[predictorIndexR]
                left[1] = math_floor(sample2L / (sample2L < 0 and 128 or 127))
                left[2] = math_floor(sample1L / (sample1L < 0 and 128 or 127))
                right[1] = math_floor(sample2R / (sample2R < 0 and 128 or 127))
                right[2] = math_floor(sample1R / (sample1R < 0 and 128 or 127))
                for i = 14, blockAlign - 1 do
                    local b = str_byte(data, n+i)
                    local hi, lo = bit32_rshift(b, 4), bit32_band(b, 0x0F)
                    if hi >= 8 then hi = hi - 16 end
                    if lo >= 8 then lo = lo - 16 end
                    local predictor = clamp(math_floor((sample1L * c1L + sample2L * c2L) / 256) + hi * deltaL, -32768, 32767)
                    left[#left+1] = math_floor(predictor / (predictor < 0 and 128 or 127))
                    sample2L, sample1L = sample1L, predictor
                    deltaL = math_max(math_floor(msadpcm_adaption_table[hi] * deltaL / 256), 16)
                    predictor = clamp(math_floor((sample1R * c1R + sample2R * c2R) / 256) + lo * deltaR, -32768, 32767)
                    right[#right+1] = math_floor(predictor / (predictor < 0 and 128 or 127))
                    sample2R, sample1R = sample1R, predictor
                    deltaR = math_max(math_floor(msadpcm_adaption_table[lo] * deltaR / 256), 16)
                end
                lastL, lastR = left, right
                for i = 1, newlen do
                    local x = (i - 1) / ratio + 1
                    local l, r
                    if x % 1 == 0 then l, r = left[x], right[x]
                    else l, r = interp(left, x), interp(right, x) end
                    if mono then retval[1][rp+i] = clamp(math_floor(l + r / 2), -128, 127)
                    else retval[1][rp+i], retval[2][rp+i] = clamp(math_floor(l), -128, 127), clamp(math_floor(r), -128, 127) end
                end
                rp = rp + newlen
                n = n + blockAlign
            end
            if #retval[1] == 0 then return nil end
            return retval, (n + pos) / bytesPerSecond
        end, not isfunc and #data / blockAlign * samplesPerBlock / sampleRate or nil
    elseif channels == 1 then
        local samplesPerBlock = (blockAlign - 7) * 2
        local iterPerSecond = math_ceil(sampleRate / samplesPerBlock)
        local bytesPerSecond = blockAlign * iterPerSecond
        local newlen = math_floor(samplesPerBlock * ratio)
        return function()
            if data == nil then return nil end
            local target = n + bytesPerSecond
            local retval = {{}}
            local rp = 0
            local start = os_epoch "utc"
            while n < target do
                if os_epoch "utc" - start > 3000 then
                    os_queueEvent("nosleep")
                    repeat until "nosleep" == os_pullEvent()
                    start = os_epoch "utc"
                end
                if isfunc and n > #data then
                    pos = pos + #data
                    n = n - #data
                    data = input()
                    if data == nil then return nil end
                end
                if n > #data then break end
                local left = {}
                local predictorIndex, delta, sample1, sample2 = str_unpack("<!1Bhhh", data)
                local c1, c2 = coeff1[predictorIndex], coeff2[predictorIndex]
                left[1] = sample2 / (sample2 < 0 and 128 or 127)
                left[2] = sample1 / (sample1 < 0 and 128 or 127)
                for i = 7, blockAlign - 1 do
                    local b = str_byte(data, n+i)
                    local hi, lo = bit32_rshift(b, 4), bit32_band(b, 0x0F)
                    if hi >= 8 then hi = hi - 16 end
                    if lo >= 8 then lo = lo - 16 end
                    local predictor = clamp(math_floor((sample1 * c1 + sample2 * c2) / 256) + hi * delta, -32768, 32767)
                    left[#left+1] = predictor / (predictor < 0 and 128 or 127)
                    sample2, sample1 = sample1, predictor
                    delta = math_max(math_floor(msadpcm_adaption_table[hi] * delta / 256), 16)
                    predictor = clamp(math_floor((sample1 * c1 + sample2 * c2) / 256) + lo * delta, -32768, 32767)
                    left[#left+1] = predictor / (predictor < 0 and 128 or 127)
                    sample2, sample1 = sample1, predictor
                    delta = math_max(math_floor(msadpcm_adaption_table[lo] * delta / 256), 16)
                end
                for i = 1, newlen do
                    local x = (i - 1) / ratio + 1
                    if x % 1 == 0 then retval[1][rp+i] = clamp(math_floor(left[x]), -128, 127)
                    else retval[1][rp+i] = clamp(math_floor(interp(left, x)), -128, 127) end
                end
                rp = rp + newlen
                n = n + blockAlign
            end
            if #retval[1] == 0 then return nil end
            return retval, (n + pos) / bytesPerSecond
        end, not isfunc and #data / blockAlign * samplesPerBlock / sampleRate or nil
    else error("Unsupported number of channels: " .. channels) end
end

--- Returns an iterator to stream data from IMA ADPCM data. aukit.Audio will
--- automatically be resampled to 48 kHz, and mixed to mono if desired. Data
--- *must* be in the interleaving format used in WAV files (i.e. periodic blocks
--- with 4/8-byte headers, channels alternating every 4 bytes, lower nibble first).
---@param input string|fun():string The audio data as a raw string or
--- reader function
---@param blockAlign number The number of bytes in each block
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param mono? boolean Whether to mix the audio down to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number|nil _ The total length of the audio in seconds, or nil if data
--- is a function
function aukit.stream.adpcm(input, blockAlign, channels, sampleRate, mono)
    expect(1, input, "string", "function")
    expect(2, blockAlign, "number")
    channels = expect(3, channels, "number", "nil") or 1
    sampleRate = expect(4, sampleRate, "number", "nil") or 48000
    expect(5, mono, "boolean", "nil")
    expect.range(sampleRate, 1)
    local isfunc = type(input) == "function"
    local ratio = 48000 / sampleRate
    local interp = interpolate[aukit.defaultInterpolation]
    local n, pos = 1, 0
    local data = isfunc and input() or input
    local samplesPerBlock = (blockAlign - 4 * channels) * 2 / channels
    local iterPerSecond = math_ceil(sampleRate / samplesPerBlock)
    local bytesPerSecond = blockAlign * iterPerSecond
    local newlen = math_floor(samplesPerBlock * ratio)
    local last
    return function()
        if data == nil then return nil end
        local target = n + bytesPerSecond
        local retval = {{}}
        if not mono then for i = 2, channels do retval[i] = {} end end
        local rp = 0
        if isfunc and target > #data then
            pos = pos + n - 1
            target = target - n + 1
            data = str_sub(data, n)
            n = 1
            while #data < target do
                local d = input()
                if not d then break end
                data = data .. d
            end
        end
        local start = os_epoch "utc"
        while n < target do
            if os_epoch "utc" - start > 3000 then
                os_queueEvent("nosleep")
                repeat until "nosleep" == os_pullEvent()
                start = os_epoch "utc"
            end
            if n + channels * 4 > #data then break end
            local d = {}
            for i = 1, channels do d[i] = {} end
            if last then for i = 1, channels do for j = 1, #last[i] do d[j-#last[i]-1] = last[i][j] end end end
            local predictor, step_index, step = {}, {}, {}
            for i = 1, channels do predictor[i], step_index[i] = str_unpack("<hB", data, n + (i - 1) * 4) end
            for i = channels * 4, blockAlign, channels * 4 do
                local p = (i - channels * 4) / channels * 2 + 1
                if #data < n + i + channels*4 then break end
                for j = 1, channels do
                    local num = str_unpack("<I", data, n + i + (j-1)*4)
                    for k = 0, 7 do
                        local nibble = bit32_extract(num, k*4, 4)
                        step[j] = ima_step_table[step_index[j]]
                        step_index[j] = clamp(step_index[j] + ima_index_table[nibble], 0, 88)
                        local diff = bit32_rshift((nibble % 8) * step[j], 2) + bit32_rshift(step[j], 3)
                        if nibble >= 8 then predictor[j] = clamp(predictor[j] - diff, -32768, 32767)
                        else predictor[j] = clamp(predictor[j] + diff, -32768, 32767) end
                        d[j][p+k] = predictor[j] / (predictor[j] < 0 and 128 or 127)
                    end
                end
            end
            last = d
            if #d[1] < samplesPerBlock then newlen = math_floor(#d[1] * ratio) end
            for i = 1, newlen do
                local x = (i - 1) / ratio + 1
                local c = {}
                if x % 1 == 0 then for j = 1, channels do c[j] = d[j][x] end
                else for j = 1, channels do c[j] = interp(d[j], x) end end
                if mono then
                    local n = 0
                    for j = 1, channels do n = n + c[j] end
                    retval[1][rp+i] = clamp(math_floor(n / channels), -128, 127)
                else for j = 1, channels do retval[j][rp+i] = clamp(math_floor(c[j]), -128, 127) end end
            end
            rp = rp + newlen
            n = n + blockAlign
        end
        if #retval[1] == 0 then return nil end
        return retval, (n + pos) / bytesPerSecond
    end, not isfunc and #data / blockAlign * samplesPerBlock / sampleRate or nil
end

--- Returns an iterator to stream data from u-law/A-law G.711 data. aukit.Audio will
--- automatically be resampled to 48 kHz, and mixed to mono if desired.
---@param input string|fun():string The audio data as a raw string or
--- reader function
---@param ulaw boolean Whether the audio uses u-law (true) or A-law (false).
---@param channels? number The number of channels present in the audio
---@param sampleRate? number The sample rate of the audio in Hertz
---@param mono? boolean Whether to mix the audio down to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number|nil _ The total length of the audio in seconds, or nil if data
--- is a function
function aukit.stream.g711(input, ulaw, channels, sampleRate, mono)
    expect(1, input, "string", "function")
    expect(2, ulaw, "boolean")
    channels = expect(3, channels, "number", "nil") or 1
    sampleRate = expect(4, sampleRate, "number", "nil") or 8000
    expect(5, mono, "boolean", "nil")
    local csize = jit and 7680 or 32768
    local xor = ulaw and 0xFF or 0x55
    local isfunc = type(input) == "function"
    local buf, pos = "", 1
    local ratio = 48000 / sampleRate
    local interp = interpolate[aukit.defaultInterpolation]
    local last
    return function()
        local lp = pos
        local retval = {}
        for i = 1, channels do retval[i] = {} end
        if last then for i = 1, channels do for j = 1, #last[i] do retval[j-#last[i]-1] = last[i][j] end end end
        local data
        if isfunc then
            while #buf < sampleRate * channels do
                local d = input()
                if not input then
                    if #buf == 0 then return nil
                    else break end
                end
                buf = buf .. d
            end
            data = str_sub(buf, 1, sampleRate * channels)
            buf = str_sub(buf, sampleRate * channels + 1)
        else data = str_sub(input, pos, pos + sampleRate * channels - 1) end
        pos = pos + sampleRate * channels
        local start = os_epoch "utc"
        for i = 1, #data, csize do
            local bytes = {str_byte(data, i, i + csize - 1)}
            for j = 1, #bytes do
                local b = bit32_bxor(bytes[j], xor)
                local m, e = bit32_band(b, 0x0F), bit32_extract(b, 4, 3)
                if not ulaw and e == 0 then m = m * 4 + 2
                else m = bit32_lshift(m * 2 + 33, e) end
                if ulaw then m = m - 33 end
                retval[(i+j-2) % channels + 1][math_floor((i+j-2) / channels + 1)] = m / (bit32_btest(b, 0x80) == ulaw and -0x40 or 0x40)
            end
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
        end
        last = {}
        for j = 1, channels do last[j] = {} for i = 1, sincWindowSize do last[j][i] = retval[j][#retval[j]-30+i] end end
        local newlen = math_floor(#retval[1] * ratio)
        local resamp = {}
        for j = 1, channels do resamp[j] = {} end
        for i = 1, newlen do
            local x = (i - 1) / ratio + 1
            local c = {}
            if x % 1 == 0 then for j = 1, channels do c[j] = retval[j][x] end
            else for j = 1, channels do c[j] = interp(retval[j], x) end end
            if mono then
                local n = 0
                for j = 1, channels do n = n + c[j] end
                resamp[1][i] = clamp(math_floor(n / channels), -128, 127)
            else for j = 1, channels do resamp[j][i] = clamp(math_floor(c[j]), -128, 127) end end
        end
        return resamp, (lp - 1) / sampleRate / channels
    end, not isfunc and #input / sampleRate / channels or nil
end

--- Returns an iterator to stream audio from a WAV file. aukit.Audio will automatically
--- be resampled to 48 kHz, and optionally mixed down to mono. This accepts PCM
--- files up to 32 bits, including float data, as well as DFPWM files [as specified here](https://gist.github.com/MCJack123/90c24b64c8e626c7f130b57e9800962c).
---@param data string|fun():string The WAV file to decode, or a function
--- returning chunks to decode (the first chunk MUST contain the ENTIRE header)
---@param mono? boolean Whether to mix the audio to mono
---@param ignoreHeader? boolean Whether to ignore additional headers
--- if they appear later in the audio stream
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number|nil _ The total length of the audio in seconds
function aukit.stream.wav(data, mono, ignoreHeader)
    local fn
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length, dataType, blockAlign, coefficients
    local temp, pos = str_unpack("c4", data)
    if temp ~= "RIFF" then error("bad argument #1 (not a WAV file)", 2) end
    pos = pos + 4
    temp, pos = str_unpack("c4", data, pos)
    if temp ~= "WAVE" then error("bad argument #1 (not a WAV file)", 2) end
    while pos <= #data do
        local size
        temp, pos = str_unpack("c4", data, pos)
        size, pos = str_unpack("<I", data, pos)
        if temp == "fmt " then
            local chunk = str_sub(data, pos, pos + size - 1)
            pos = pos + size
            local format
            format, channels, sampleRate, blockAlign, bitDepth = str_unpack("<HHIxxxxHH", chunk)
            if format == 1 then
                dataType = bitDepth == 8 and "unsigned" or "signed"
            elseif format == 2 then
                dataType = "msadpcm"
                local numcoeff = str_unpack("<H", chunk, 21)
                if numcoeff > 0 then
                    coefficients = {{}, {}}
                    for i = 1, numcoeff do
                        coefficients[1][i], coefficients[2][i] = str_unpack("<hh", chunk, i * 4 + 19)
                    end
                end
            elseif format == 3 then
                dataType = "float"
            elseif format == 6 then
                dataType = "alaw"
            elseif format == 7 then
                dataType = "ulaw"
            elseif format == 0x11 then
                dataType = "adpcm"
            elseif format == 0xFFFE then
                bitDepth = str_unpack("<H", chunk, 19)
                local uuid = str_sub(chunk, 25, 40)
                if uuid == wavExtensible.pcm then dataType = bitDepth == 8 and "unsigned" or "signed"
                elseif uuid == wavExtensible.dfpwm then dataType = "dfpwm"
                elseif uuid == wavExtensible.msadpcm then dataType = "msadpcm"
                elseif uuid == wavExtensible.pcm_float then dataType = "float"
                elseif uuid == wavExtensible.alaw then dataType = "alaw"
                elseif uuid == wavExtensible.ulaw then dataType = "ulaw"
                elseif uuid == wavExtensible.adpcm then dataType = "adpcm"
                else error("unsupported WAV file", 2) end
            else error("unsupported WAV file", 2) end
        elseif temp == "data" then
            local data = str_sub(data, pos, pos + size - 1)
            if not fn and #data < size then error("invalid WAV file", 2) end
            if fn then
                local first, f = data
                data = function()
                    if first then f, first = first return f
                    elseif ignoreHeader then
                        local d = fn()
                        if not d then return nil end
                        if d:match "^RIFF....WAVE" then return str_sub(d, d:match("^RIFF....WAVE.?data....()"))
                        else return d end
                    else return fn() end
                end
            end
            if dataType == "adpcm" then return aukit.stream.adpcm(data, blockAlign, channels, sampleRate, mono)
            elseif dataType == "msadpcm" then return aukit.stream.msadpcm(data, blockAlign, channels, sampleRate, mono, coefficients)
            elseif dataType == "dfpwm" then return aukit.stream.dfpwm(data, sampleRate, channels, mono), size / channels / (bitDepth / 8) / sampleRate
            elseif dataType == "alaw" or dataType == "ulaw" then return aukit.stream.g711(data, dataType == "ulaw", channels, sampleRate, mono)
            else return aukit.stream.pcm(data, bitDepth, dataType, channels, sampleRate, false, mono), size / channels / (bitDepth / 8) / sampleRate end
        elseif temp == "fact" then
            -- TODO
            pos = pos + size
        else pos = pos + size end
    end
    error("invalid WAV file", 2)
end

--- Returns an iterator to stream audio from an AIFF or AIFC file. aukit.Audio will
--- automatically be resampled to 48 kHz, and optionally mixed down to mono.
---@param data string|fun():string The AIFF file to decode, or a function
--- returning chunks to decode (the first chunk MUST contain the ENTIRE header)
---@param mono? boolean Whether to mix the audio to mono
---@param ignoreHeader? boolean Whether to ignore additional headers
--- if they appear later in the audio stream
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number _ The total length of the audio in seconds
function aukit.stream.aiff(data, mono, ignoreHeader)
    local fn
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string")
    expect(2, mono, "boolean", "nil")
    local channels, sampleRate, bitDepth, length, offset, compression, blockAlign
    local isAIFC = false
    local temp, pos = str_unpack("c4", data)
    if temp ~= "FORM" then error("bad argument #1 (not an AIFF file)", 2) end
    pos = pos + 4
    temp, pos = str_unpack("c4", data, pos)
    if temp == "AIFC" then isAIFC = true
    elseif temp ~= "AIFF" then error("bad argument #1 (not an AIFF file)", 2) end
    while pos <= #data do
        local size
        temp, pos = str_unpack("c4", data, pos)
        size, pos = str_unpack(">I", data, pos)
        if temp == "COMM" then
            local e, m
            channels, length, bitDepth, e, m, pos = str_unpack(">hIhHI7x", data, pos)
            if isAIFC then
                local s
                compression, s, pos = str_unpack(">c4s1", data, pos)
                if #s % 2 == 0 then pos = pos + 1 end
            end
            length = length * channels * math_floor(bitDepth / 8)
            local s = bit32_btest(e, 0x8000)
            e = ((bit32_band(e, 0x7FFF) - 0x3FFE) % 0x800)
            sampleRate = math.ldexp(m * (s and -1 or 1) / 0x100000000000000, e)
        elseif temp == "SSND" then
            offset, blockAlign, pos = str_unpack(">II", data, pos)
            local data = str_sub(data, pos + offset, pos + offset + length - 1)
            if not fn and #data < length then error("invalid AIFF file", 2) end
            if fn then
                local first, f = data
                data = function()
                    if first then f, first = first return f
                    elseif ignoreHeader then
                        local d = fn()
                        if not d then return nil end
                        if d:match "^FORM....AIF[FC]" then
                            local n, p = d:match("^FORM....AIF[FC].-SSND(....)....()")
                            offset = str_unpack(">I", n)
                            return str_sub(d, p + offset)
                        else return d end
                    else return fn() end
                end
            end
            if compression == nil or compression == "NONE" then return aukit.stream.pcm(data, bitDepth, "signed", channels, sampleRate, true, mono), length / channels / (bitDepth / 8) / sampleRate
            elseif compression == "sowt" then return aukit.stream.pcm(data, bitDepth, "signed", channels, sampleRate, true, mono), length / channels / (bitDepth / 8) / sampleRate
            elseif compression == "fl32" or compression == "FL32" then return aukit.stream.pcm(data, 32, "float", channels, sampleRate, true, mono), length / channels / 4 / sampleRate
            elseif compression == "alaw" or compression == "ulaw" or compression == "ALAW" or compression == "ULAW" then return aukit.stream.g711(data, compression == "ulaw" or compression == "ULAW", channels, sampleRate, mono), length / channels / sampleRate
            else error("Unsupported compression scheme " .. compression, 2) end
            return aukit.stream.pcm(data, bitDepth, "signed", channels, sampleRate, true, mono), length / channels / (bitDepth / 8) / sampleRate
        else pos = pos + size end
    end
    error("invalid AIFF file", 2)
end

--- Returns an iterator to stream data from an AU file. aukit.Audio will automatically
--- be resampled to 48 kHz, and optionally mixed down to mono.
---@param data string|fun():string The AU file to decode, or a function
--- returning chunks to decode (the first chunk MUST contain the ENTIRE header)
---@param mono? boolean Whether to mix the audio to mono
---@param ignoreHeader? boolean Whether to ignore additional headers
--- if they appear later in the audio stream
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number _ The total length of the audio in seconds
function aukit.stream.au(data, mono, ignoreHeader)
    local fn
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string")
    expect(2, mono, "boolean", "nil")
    local magic, offset, size, encoding, sampleRate, channels = str_unpack(">c4IIIII", data)
    if magic ~= ".snd" then error("invalid AU file", 2) end
    if fn then
        local first, f = str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), nil
        data = function()
            if first then f, first = first return f
            elseif ignoreHeader then
                local d = fn()
                if not d then return nil end
                if d:match "^.snd" then return str_sub(d, str_unpack(">I", str_sub(d, 5, 8)), nil)
                else return d end
            else return fn() end
        end
    else data = str_sub(data, offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil) end
    if encoding == 1 then return aukit.stream.g711(data, true, channels, sampleRate, mono), size / channels / sampleRate
    elseif encoding == 2 then return aukit.stream.pcm(data, 8, "signed", channels, sampleRate, true, mono), size / channels / sampleRate
    elseif encoding == 3 then return aukit.stream.pcm(data, 16, "signed", channels, sampleRate, true, mono), size / channels / 2 / sampleRate
    elseif encoding == 4 then return aukit.stream.pcm(data, 24, "signed", channels, sampleRate, true, mono), size / channels / 3 / sampleRate
    elseif encoding == 5 then return aukit.stream.pcm(data, 32, "signed", channels, sampleRate, true, mono), size / channels / 4 / sampleRate
    elseif encoding == 6 then return aukit.stream.pcm(data, 32, "float", channels, sampleRate, true, mono), size / channels / 4 / sampleRate
    elseif encoding == 27 then return aukit.stream.g711(data, false, channels, sampleRate, mono), size / channels / sampleRate
    else error("unsupported encoding type " .. encoding, 2) end
end

--- Returns an iterator to stream data from a FLAC file. aukit.Audio will automatically
--- be resampled to 48 kHz, and optionally mixed down to mono.
---@param data string|fun():string The FLAC file to decode, or a function
--- returning chunks to decode
---@param mono? boolean Whether to mix the audio to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number _ The total length of the audio in seconds
function aukit.stream.flac(data, mono)
    expect(1, data, "string", "function")
    expect(2, mono, "boolean", "nil")
    local infn = false
    if type(data) == "function" then data = setmetatable({str = "", fn = data, final = false, byte = function(self, start, e)
        while start > #self.str do
            infn = true
            local d = self.fn()
            infn = false
            if not d then self.final = true return nil end
            self.str = self.str .. d
        end
        while e and e > #self.str do
            infn = true
            local d = self.fn()
            infn = false
            if not d then self.final = true return nil end
            self.str = self.str .. d
        end
        return str_byte(self.str, start, e)
    end}, {__len = function(self) return self.final and #self.str or math.huge end}) end
    local function saferesume(coro, ...)
        local res = table_pack(coroutine.resume(coro, ...))
        while res[1] and infn do res = table_pack(coroutine.resume(coro, coroutine.yield(table_unpack(res, 2, res.n)))) end
        return table_unpack(res, 1, res.n)
    end
    local coro = coroutine.create(decodeFLAC)
    local ok, sampleRate, len = saferesume(coro, data, coroutine.yield)
    if not ok then error(sampleRate, 2) end
    local pos = 0
    local ratio = 48000 / sampleRate
    local lp_alpha = 1 - math.exp(-(sampleRate / 96000) * 2 * math_pi)
    local interp = interpolate[aukit.defaultInterpolation]
    local last = {0, 0}
    return function()
        if coroutine.status(coro) == "dead" then return nil end
        local chunk = {{}}
        while #chunk[1] < sampleRate do
            local ok, res = saferesume(coro)
            if not ok or res == nil or res.sampleRate then break end
            os_queueEvent("nosleep")
            repeat until "nosleep" == os_pullEvent()
            for c = 1, #res do
                chunk[c] = chunk[c] or {}
                local src, dest = res[c], chunk[c]
                local start = #dest
                src[0] = last[2]
                src[-1] = last[1]
                local ls = last[2] / (last[2] < 0 and 128 or 127)
                for i = 1, math_floor(#src * ratio) do
                    local d = start+i
                    local x = ((i - 1) / ratio) + 1
                    local s
                    if x % 1 == 0 then s = src[x]
                    else s = interp(src, x) end
                    s = ls + lp_alpha * (s - ls)
                    ls = s
                    dest[d] = clamp(s * (s < 0 and 128 or 127), -128, 127)
                end
                last = {src[#src-1], src[#src]}
            end
            os_queueEvent("nosleep")
            repeat until "nosleep" == os_pullEvent()
        end
        pos = pos + #chunk[1] / 48000
        return chunk, pos
    end, len / sampleRate
end

--- Returns an iterator to stream data from a QOA file. aukit.Audio will automatically
--- be resampled to 48 kHz, and optionally mixed down to mono.
---@param data string|fun():string The QOA file to decode, or a function
--- returning chunks to decode
---@param mono? boolean Whether to mix the audio to mono
---@return fun():number[][]|nil,number|nil _ An iterator function that returns
--- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
--- the current position of the audio in seconds
---@return number _ The total length of the audio in seconds
function aukit.stream.qoa(data, mono)
    expect(1, data, "string", "function")
    expect(2, mono, "boolean", "nil")
    local read, peek
    if type(data) == "string" then
        local pos = 1
        function read(n)
            if pos > #data then return nil end
            local d = data:sub(pos, pos + n - 1)
            pos = pos + n
            return d
        end
        function peek(n)
            if pos > #data then return nil end
            return data:sub(pos, pos + n - 1)
        end
    else
        local buf = ""
        function read(n)
            while #buf < n do
                local d = data()
                if not d then return nil end
                buf = buf .. d
            end
            local d = buf:sub(1, n)
            buf = buf:sub(n + 1)
            return d
        end
        function peek(n)
            while #buf < n do
                local d = data()
                if not d then return nil end
                buf = buf .. d
            end
            return buf:sub(1, n)
        end
    end
    local magic, file_samples = (">c4I4"):unpack(assert(read(8), "Not a QOA file"), nil)
    if magic ~= "qoaf" then error("Not a QOA file", 2) end
    local file_channels, file_sampleRate = (">BI3"):unpack(assert(peek(4), "Not a QOA file"), nil)
    local lms = {}
    local last = {}
    for i = 1, file_channels do
        lms[i] = {history = {}, weights = {}}
        last[i] = {0, 0}
    end

    local file_pos = 0
    local ratio = 48000 / file_sampleRate
    local lp_alpha = 1 - math.exp(-(file_sampleRate / 96000) * 2 * math_pi)
    local interp = interpolate[aukit.defaultInterpolation]
    return function()
        local chunk = {}
        for i = 1, file_channels do chunk[i] = {[-1] = last[i][1], [0] = last[i][2]} end
        local sample_pos = 0
        while sample_pos < file_sampleRate do
            -- from https://github.com/phoboslab/qoa/blob/master/qoa.h
            -- MIT license - Copyright (c) 2023 Dominic Szablewski

            -- Read and verify the frame header
            local d = read(8)
            if not d then break end
            local channels, samplerate, samples, frame_size = (">BI3I2I2"):unpack(d)

            local data_size = frame_size - 8 - 4 * 4 * channels
            local num_slices = math_floor(data_size / 8)
            local max_total_samples = num_slices * 20

            if
                channels ~= file_channels or
                samplerate ~= file_sampleRate or
                samples * channels > max_total_samples
            then
                --error("Bad frame data", 2)
                break
            end

            -- Read the LMS state: 4 x 2 bytes history, 4 x 2 bytes weights per channel
            for c = 1, channels do
                lms[c].history = {(">i2i2i2i2"):unpack(assert(read(8), "Invalid QOA data"), nil)}
                lms[c].weights = {(">i2i2i2i2"):unpack(assert(read(8), "Invalid QOA data"), nil)}
            end

            -- Decode all slices for all channels in this frame
            for sample_index = 1, samples, 20 do
                for c = 1, channels do
                    local sliceH, sliceL = (">I4I4"):unpack(assert(read(8), "Invalid QOA data"), nil)

                    local scalefactor = bit32_extract(sliceH, 28, 4)

                    for si = sample_index, sample_index + 19 do
                        local predicted = qoa_lms_predict(lms[c])
                        local quantized = bit32_extract(sliceH, 25, 3)
                        local dequantized = qoa_dequant_tab[scalefactor][quantized]
                        --print(predicted, dequantized)
                        local reconstructed = math_min(math_max(predicted + dequantized, -32768), 32767)

                        chunk[c][sample_pos + si] = math_floor(reconstructed / 256)
                        sliceH = bit32_lshift(sliceH, 3) + bit32_extract(sliceL, 29, 3)
                        sliceL = bit32_lshift(sliceL, 3)

                        qoa_lms_update(lms[c], reconstructed, dequantized)
                    end
                end
            end
            sample_pos = sample_pos + samples
        end

        if #chunk[1] == 0 then return nil end

        local newlen = #chunk[1] * ratio
        local lines = {{}}
        if not mono then for j = 2, file_channels do lines[j] = {} end end
        local ls = {}
        for j = 1, file_channels do ls[j] = last[j][2] end
        for i = 1, newlen do
            local n = 0
            for j = 1, file_channels do
                local x = (i - 1) / ratio + 1
                local s
                if x % 1 == 0 then s = chunk[j][x]
                else s = clamp(interp(chunk[j], x), -128, 127) end
                s = ls[j] + lp_alpha * (s - ls[j])
                ls[j] = s
                if mono then n = n + s
                else lines[j][i] = s end
            end
            if mono then lines[1][i] = n / file_channels end
        end

        local pos = file_pos / file_sampleRate
        file_pos = file_pos + sample_pos
        for i = 1, file_channels do last[i] = {chunk[i][#chunk[i]-1], chunk[i][#chunk[i]]} end
        return lines, pos
    end, file_samples / file_sampleRate
end

--[[
.########.########.########.########..######..########..######.
.##.......##.......##.......##.......##....##....##....##....##
.##.......##.......##.......##.......##..........##....##......
.######...######...######...######...##..........##.....######.
.##.......##.......##.......##.......##..........##..........##
.##.......##.......##.......##.......##....##....##....##....##
.########.##.......##.......########..######.....##.....######.
]]

--- aukit.effects
---@section aukit.effects

--- Amplifies the audio by the multiplier specified.
---@param audio aukit.Audio The audio to modify
---@param multiplier number The multiplier to apply
---@return aukit.Audio _ The audio modified
function aukit.effects.amplify(audio, multiplier)
    expectAudio(1, audio)
    expect(2, multiplier, "number")
    if multiplier == 1 then return audio end
    local start = os_epoch "utc"
    for c = 1, #audio.data do
        local ch = audio.data[c]
        for i = 1, #ch do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            ch[i] = clamp(ch[i] * multiplier, -1, 1)
        end
    end
    return audio
end

--- Changes the speed and pitch of audio by a multiplier, resampling to keep the
--- same sample rate.
---@param audio aukit.Audio The audio to modify
---@param multiplier number The multiplier to apply
---@return aukit.Audio _ The audio modified
function aukit.effects.speed(audio, multiplier)
    expectAudio(1, audio)
    expect(2, multiplier, "number")
    if multiplier == 1 then return audio end
    local rate = audio.sampleRate
    audio.sampleRate = audio.sampleRate * multiplier
    local new = audio:resample(rate)
    audio.sampleRate, audio.data = rate, new.data
    return audio
end

--- Fades a period of music from one amplitude to another.
---@param audio aukit.Audio The audio to modify
---@param startTime number The start time of the fade, in seconds
---@param startAmplitude number The amplitude of the beginning of the fade
---@param endTime number The end time of the fade, in seconds
---@param endAmplitude number The amplitude of the end of the fade
---@return aukit.Audio _ The audio modified
function aukit.effects.fade(audio, startTime, startAmplitude, endTime, endAmplitude)
    expectAudio(1, audio)
    expect(2, startTime, "number")
    expect(3, startAmplitude, "number")
    expect(4, endTime, "number")
    expect(5, endAmplitude, "number")
    if startAmplitude == 1 and endAmplitude == 1 then return audio end
    local startt = os_epoch "utc"
    for c = 1, #audio.data do
        local ch = audio.data[c]
        local start = startTime * audio.sampleRate
        local m = (endAmplitude - startAmplitude) / ((endTime - startTime) * audio.sampleRate)
        for i = start, endTime * audio.sampleRate do
            if os_epoch "utc" - startt > 5000 then startt = os_epoch "utc" sleep(0) end
            ch[i] = clamp(ch[i] * (m * (i - start) + startAmplitude), -1, 1)
        end
    end
    return audio
end

--- Inverts all channels in the specified audio.
---@param audio aukit.Audio The audio to modify
---@return aukit.Audio _ The audio modified
function aukit.effects.invert(audio)
    expectAudio(1, audio)
    for c = 1, #audio.data do
        local ch = audio.data[c]
        for i = 1, #ch do ch[i] = -ch[i] end
    end
    return audio
end

--- Normalizes audio to the specified peak amplitude.
---@param audio aukit.Audio The audio to modify
---@param peakAmplitude? number The maximum amplitude
---@param independent? boolean Whether to normalize each channel independently
---@return aukit.Audio _ The audio modified
function aukit.effects.normalize(audio, peakAmplitude, independent)
    expectAudio(1, audio)
    peakAmplitude = expect(2, peakAmplitude, "number", "nil") or 1
    expect(3, independent, "boolean", "nil")
    local mult
    local start, sampleRate = os_epoch "utc", audio.sampleRate
    if not independent then
        local max = 0
        for c = 1, #audio.data do
            local ch = audio.data[c]
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            for i = 1, #ch do max = math_max(max, math_abs(ch[i])) end
        end
        mult = peakAmplitude / max
    end
    for c = 1, #audio.data do
        local ch = audio.data[c]
        if independent then
            local max = 0
            for i = 1, #ch do max = math_max(max, math_abs(ch[i])) end
            mult = peakAmplitude / max
        end
        if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
        for i = 1, #ch do
            ch[i] = clamp(ch[i] * mult, -1, 1)
        end
    end
    return audio
end

--- Centers the DC offset of each channel.
---@param audio aukit.Audio The audio to modify
---@return aukit.Audio _ The audio modified
function aukit.effects.center(audio)
    expectAudio(1, audio)
    for c = 1, #audio.data do
        local ch = audio.data[c]
        for i = 0, #ch - 1, audio.sampleRate do
            local avg = 0
            local l = math_min(#ch - i, audio.sampleRate)
            for j = 1, l do avg = avg + ch[i+j] end
            avg = avg / l
            for j = 1, l do ch[i+j] = clamp(ch[i+j] - avg, -1, 1) end
        end
    end
    return audio
end

--- Trims any extra silence on either end of the specified audio.
---@param audio aukit.Audio The audio to modify
---@param threshold? number The maximum value to register as silence
---@return aukit.Audio _ The audio modified
function aukit.effects.trim(audio, threshold)
    expectAudio(1, audio)
    threshold = expect(2, threshold, "number", "nil") or (1/65536)
    local s, e
    for i = 1, #audio.data[1] do
        for c = 1, #audio.data do if math_abs(audio.data[c][i]) > threshold then s = i break end end
        if s then break end
    end
    for i = #audio.data[1], 1, -1 do
        for c = 1, #audio.data do if math_abs(audio.data[c][i]) > threshold then e = i break end end
        if e then break end
    end
    local new = str_sub(audio, s / audio.sampleRate, e / audio.sampleRate)
    audio.data = new.data
    return audio
end

--- Adds a delay to the specified audio.
---@param audio aukit.Audio The audio to modify
---@param delay number The amount of time to delay for, in seconds
---@param multiplier? number The multiplier to apply to the delayed audio
---@return aukit.Audio _ The audio modified
function aukit.effects.delay(audio, delay, multiplier)
    expectAudio(1, audio)
    expect(2, delay, "number")
    multiplier = expect(3, multiplier, "number", "nil") or 0.5
    local samples = math_floor(delay * audio.sampleRate)
    for c = 1, #audio.data do
        local original = {}
        local o = audio.data[c]
        for i = 1, #o do original[i] = o[i] end
        for i = samples + 1, #o do o[i] = clamp(o[i] + original[i - samples] * multiplier, -1, 1) end
    end
    return audio
end

--- Adds an echo to the specified audio.
---@param audio aukit.Audio The audio to modify
---@param delay? number The amount of time to echo after, in seconds
---@param multiplier? number The decay multiplier to apply to the echoed audio
---@return aukit.Audio _ The audio modified
function aukit.effects.echo(audio, delay, multiplier)
    expectAudio(1, audio)
    delay = expect(2, delay, "number", "nil") or 1
    multiplier = expect(3, multiplier, "number", "nil") or 0.5
    local samples = math_floor(delay * audio.sampleRate)
    for c = 1, #audio.data do
        local o = audio.data[c]
        for i = samples + 1, #o do o[i] = clamp(o[i] + o[i - samples] * multiplier, -1, 1) end
    end
    return audio
end

local combDelayShift = {0, -11.73, 19.31, -7.97}
local combDecayShift = {0, 0.1313, 0.2743, 0.31}

--- Adds reverb to the specified audio.
---@param audio aukit.Audio The audio to modify
---@param delay? number The amount of time to reverb after, in **milliseconds**
---@param decay? number The decay factor to use
---@param wetMultiplier? number The wet (reverbed) mix amount
---@param dryMultiplier? number The dry (original) mix amount
---@return aukit.Audio _ The audio modified
function aukit.effects.reverb(audio, delay, decay, wetMultiplier, dryMultiplier)
    expectAudio(1, audio)
    delay = expect(2, delay, "number", "nil") or 100
    decay = expect(3, decay, "number", "nil") or 0.3
    wetMultiplier = expect(4, wetMultiplier, "number", "nil") or 1
    dryMultiplier = expect(5, dryMultiplier, "number", "nil") or 0
    for c = 1, #audio.data do
        -- four comb filters
        local sum = {}
        local o = audio.data[c]
        for n = 1, 4 do
            local comb = {}
            local samples = math_floor((delay + combDelayShift[n]) / 1000 * audio.sampleRate)
            local multiplier = decay - combDecayShift[n]
            for i = 1, math_min(samples, #o) do
                comb[i] = o[i]
                sum[i] = (sum[i] or 0) + o[i]
            end
            for i = samples + 1, #o do
                local s = o[i] + comb[i - samples] * multiplier
                comb[i] = s
                sum[i] = (sum[i] or 0) + s
            end
        end
        -- mix wet/dry
        for i = 1, #sum do sum[i] = sum[i] * wetMultiplier + o[i] * dryMultiplier end
        -- two all pass filters
        local samples = math_floor(0.08927 * audio.sampleRate)
        sum[samples+1] = sum[samples+1] - 0.131 * sum[1]
        for i = samples + 2, #sum do sum[i] = sum[i] - 0.131 * sum[i - samples] + 0.131 * sum[i + 20 - samples] end
        o[samples+1] = clamp(sum[samples+1] - 0.131 * sum[1], -1, 1)
        for i = samples + 2, #sum do o[i] = clamp(sum[i] - 0.131 * sum[i - samples] + 0.131 * sum[i + 20 - samples], -1, 1) end
    end
    return audio
end

--- Applies a low-pass filter to the specified audio.
---@param audio aukit.Audio The audio to modify
---@param frequency number The cutoff frequency for the filter
---@return aukit.Audio _ The audio modified
function aukit.effects.lowpass(audio, frequency)
    expectAudio(1, audio)
    expect(2, frequency, "number")
    local a = 1 - math.exp(-(frequency / audio.sampleRate) * 2 * math_pi)
    for c = 1, #audio.data do
        local d = audio.data[c]
        for i = 2, #d do
            local l = d[i-1]
            d[i] = l + a * (d[i] - l)
        end
    end
    return audio
end

--- Applies a high-pass filter to the specified audio.
---@param audio aukit.Audio The audio to modify
---@param frequency number The cutoff frequency for the filter
---@return aukit.Audio _ The audio modified
function aukit.effects.highpass(audio, frequency)
    expectAudio(1, audio)
    expect(2, frequency, "number")
    local a = 1 / (2 * math_pi * (frequency / audio.sampleRate) + 1)
    for c = 1, #audio.data do
        local d = audio.data[c]
        local lx = d[1]
        for i = 2, #d do
            local llx = d[i]
            d[i] = a * (d[i-1] + llx - lx)
            lx = llx
        end
    end
    return audio
end

return aukit
