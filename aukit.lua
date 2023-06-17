--- AUKit: Audio decoding and processing framework for ComputerCraft
--
-- AUKit is a framework designed to simplify the process of loading, modifying,
-- and playing audio files in various formats. It includes support for loading
-- audio from many sources, including PCM, DFPWM, and IMA ADPCM codecs, as well
-- as WAV, AIFF, AU, and FLAC files. It can also generate audio on-the-fly as
-- tones, noise, or silence.
--
-- AUKit uses a structure called Audio to store information about each audio
-- chunk. An audio object holds the sample rate of the audio, as well as the
-- data for each channel stored as floating-point numbers. Audio objects can
-- hold any number of channels at any sample rate with any duration.
--
-- To obtain an audio object, you can use any of the main functions in the aukit
-- module. These allow loading from various raw codecs or file formats, with
-- data sources as strings, or tables if using a raw codec loader.
--
-- Once the audio is loaded, various basic operations are available. A subset of
-- the string library is available to simplify operations on the audio, and a
-- number of operators (+, *, .., #) are overridden as well. There's also built-
-- in functions for resampling the audio, with nearest-neighbor, linear, cubic,
-- and sinc interpolation available; as well as mixing channels (including down to
-- mono) and combining/splitting channels. Finally, audio objects can be exported
-- back to PCM, DFPWM, or WAV data, allowing changes to be easily stored on disk.
-- The stream function also automatically chunks data for use with a speaker.
-- All of these functions return a new audio object, leaving the original intact.
--
-- There are also a number of effects available for audio. These are contained
-- in the aukit.effects table, and modify the audio passed to them (as well as
-- returning the audio for streamlining). The effects are intended to speed up
-- common operations on audio. More effects may be added in future versions.
--
-- For simple audio playback tasks, the aukit.stream table provides a number of
-- functions that can quickly decode audio for real-time playback. Each function
-- returns an iterator function that can be called multiple times to obtain fully
-- decoded chunks of audio in 8-bit PCM, ready for playback to one or more
-- speakers. The functions decode the data, resample it to 48 kHz (using the
-- default resampling method), apply a low-pass filter to decrease interpolation
-- error, mix to mono if desired, and then return a list of tables with samples
-- in the range [-128, 127], plus the current position of the audio. The
-- iterators can be passed directly to the aukit.play function, which complements
-- the aukit.stream suite by playing the decoded audio on speakers while decoding
-- it in real-time, handling synchronization of speakers as best as possible.
--
-- If you're really lazy, you can also call `aukit` as a function, which takes
-- the path to a file, and plays this on all available speakers.
--
-- Be aware that processing large amounts of audio (especially loading FLAC or
-- resampling with higher quality) is *very* slow. It's recommended to use audio
-- files with lower data size (8-bit mono PCM/WAV/AIFF is ideal), and potentially
-- a lower sample rate, to reduce the load on the system - especially as all
-- data gets converted to 8-bit DFPWM data on playback anyway. The code yields
-- internally when things take a long time to avoid abort timeouts.
--
-- For an example of how to use AUKit, see the accompanying auplay.lua file.
--
-- @author JackMacWindows
-- @license MIT
--
-- <style>#content {width: unset !important;}</style>
--
-- @module aukit
-- @set project=AUKit

-- MIT License
--
-- Copyright (c) 2021-2023 JackMacWindows
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

local expect = require "cc.expect"
local dfpwm = require "cc.audio.dfpwm"

local bit32_band, bit32_rshift, bit32_btest = bit32.band, bit32.rshift, bit32.btest
local math_floor, math_ceil, math_sin, math_abs, math_fmod, math_min, math_pi = math.floor, math.ceil, math.sin, math.abs, math.fmod, math.min, math.pi
local os_epoch = os.epoch
local str_pack, str_unpack, str_sub, str_byte, str_rep = string.pack, string.unpack, string.sub, string.byte, string.rep
local table_unpack = table.unpack

local aukit = setmetatable({}, {__call = function(aukit, path)
    expect(1, path, "string")
    local file = assert(fs.open(path, "rb"))
    local type = aukit.detect(file.read(64)) or "dfpwm"
    file.seek("set", 0)
    aukit.play(aukit.stream[type](function() return file.read(48000) end), peripheral.find("speaker"))
    file.close()
end})
aukit.effects, aukit.stream = {}, {}

--- @tfield string _VERSION The version of AUKit that is loaded. This follows [SemVer](https://semver.org) format.
aukit._VERSION = "1.5.1"

--- @tfield "none"|"linear"|"cubic"|"sinc" defaultInterpolation Default interpolation mode for @{Audio:resample} and other functions that need to resample.
aukit.defaultInterpolation = "linear"

--- @type Audio
local Audio = {}
local Audio_mt

--- @tfield number sampleRate The sample rate of the audio.
Audio.sampleRate = nil

--- @tfield table metadata Stores any metadata read from the file if present.
Audio.metadata = nil

--- @tfield table info Stores any decoder-specific information, including `bitDepth` and `dataType`.
Audio.info = nil

local dfpwmUUID = "3ac1fa38-811d-4361-a40d-ce53ca607cd1" -- UUID for DFPWM in WAV files

local function uuidBytes(uuid) return uuid:gsub("-", ""):gsub("%x%x", function(c) return string.char(tonumber(c, 16)) end) end

local sincWindowSize = jit and 30 or 10

local wavExtensible = {
    dfpwm = uuidBytes(dfpwmUUID),
    pcm = uuidBytes "01000000-0000-1000-8000-00aa00389b71",
    adpcm = uuidBytes "02000000-0000-1000-8000-00aa00389b71",
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

local function clamp(n, min, max) return math.max(math.min(n, max), min) end

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
    if be then for i = 0, sz - 1 do n = n * 256 + str:byte(pos+i) end
    else for i = 0, sz - 1 do n = n + str:byte(pos+i) * 2^(8*i) end end
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

    function decodeFLAC(inp, callback)
        local out = {}
        local pos = 1
        -- Handle FLAC header and metadata blocks
        local temp temp, pos = intunpack(inp, pos, 4, false, true)
        if temp ~= 0x664C6143 then error("Invalid magic string") end
        local sampleRate, numChannels, sampleDepth, numSamples
        local last = false
        local meta = {}
        while not last do
            temp, pos = inp:byte(pos), pos + 1
            last = bit32_btest(temp, 0x80)
            local type = bit32_band(temp, 0x7F);
            local length length, pos = intunpack(inp, pos, 3, false, true)
            if type == 0 then  -- Stream info block
                pos = pos + 10
                sampleRate, pos = intunpack(inp, pos, 2, false, true)
                sampleRate = sampleRate * 16 + bit32_rshift(inp:byte(pos), 4)
                numChannels = bit32_band(bit32_rshift(inp:byte(pos), 1), 7) + 1;
                sampleDepth = bit32_band(inp:byte(pos), 1) * 16 + bit32_rshift(inp:byte(pos+1), 4) + 1;
                numSamples, pos = intunpack(inp, pos + 2, 4, false, true)
                numSamples = numSamples + bit32_band(inp:byte(pos-5), 15) * 2^32
                pos = pos + 16
            elseif type == 4 then
                local ncomments
                meta.vendor, ncomments, pos = ("<s4I4"):unpack(inp, pos)
                for i = 1, ncomments do
                    local str
                    str, pos = utf8decode(("<s4"):unpack(inp, pos))
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

--- Returns the length of the audio object in seconds.
-- @treturn number The audio length
function Audio:len()
    return #self.data[1] / self.sampleRate
end

--- Returns the number of channels in the audio object.
-- @treturn number The number of channels
function Audio:channels()
    return #self.data
end

--- Creates a new audio object with the data resampled to a different sample rate.
-- If the target rate is the same, the object is copied without modification.
-- @tparam number sampleRate The new sample rate in Hertz
-- @tparam[opt=aukit.defaultInterpolation] "none"|"linear"|"cubic" interpolation The interpolation mode to use
-- @treturn Audio A new audio object with the resampled data
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
-- @treturn Audio A new audio object with the audio mixed to mono
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
-- new channel to the end of each old channel, resampling the new channels to match
-- this one (if necessary), and inserting silence in any missing channels.
-- @tparam Audio ... The audio objects to concatenate
-- @treturn Audio The new concatenated audio object
function Audio:concat(...)
    local audios = {self, ...}
    local l = {#self.data[1]}
    local cn = #self.data
    for i = 2, #audios do
        expectAudio(i-1, audios[i])
        if audios[i].sampleRate ~= self.sampleRate then audios[i] = audios[i]:resample(self.sampleRate) end
        l[i] = #audios[i].data[1]
        cn = math.max(cn, #audios[i].data)
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
-- This takes the same arguments as @{string.sub}, but positions start at 0.
-- @tparam[opt=0] number start The start position of the audio in seconds
-- @tparam[opt=0] number last The end position of the audio in seconds (0 means end of file)
-- @treturn Audio The new split audio object
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
-- channels on the end of the new object, resampling the new channels to match
-- this one (if necessary), and extending any channels that are shorter than the
-- longest channel with zeroes.
-- @tparam Audio ... The audio objects to combine with
-- @treturn Audio The new combined audio object
function Audio:combine(...)
    local audios = {self, ...}
    local len = #self.data[1]
    for i = 2, #audios do
        expectAudio(i-1, audios[i])
        if audios[i].sampleRate ~= self.sampleRate then audios[i] = audios[i]:resample(self.sampleRate) end
        len = math.max(len, #audios[i].data[1])
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
-- Passing a channel that doesn't exist will throw an error.
-- @tparam {[number]...} ... The lists of channels in each new object
-- @treturn Audio... The new audio objects created from the channels in each list
-- @usage Split a stereo track into independent mono objects
--
--     local left, right = stereo:split({1}, {2})
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
-- with a multiplier (before clipping) if desired, and clipping any values
-- outside the audio range ([-1, 1]). Channels that are shorter are padded with
-- zeroes at the end, and non-existent channels are replaced with all zeroes.
-- Any audio objects with a different sample rate are resampled to match this one.
-- @tparam number|Audio amplifier The multiplier to apply, or the first audio object
-- @tparam[opt] Audio ... The objects to mix with this one
-- @treturn Audio The new mixed audio object
function Audio:mix(amplifier, ...)
    local audios = {self, ...}
    local len = #self.data[1]
    local cn = #self.data
    for i = 2, #audios do
        expectAudio(i, audios[i])
        if audios[i].sampleRate ~= self.sampleRate then audios[i] = audios[i]:resample(self.sampleRate) end
        len = math.max(len, #audios[i].data[1])
        cn = math.max(cn, #audios[i].data)
    end
    if type(amplifier) ~= "number" then
        expectAudio(1, amplifier)
        if amplifier.sampleRate ~= self.sampleRate then amplifier = amplifier:resample(self.sampleRate) end
        len = math.max(len, #amplifier.data[1])
        cn = math.max(cn, #amplifier.data)
        table.insert(audios, 2, amplifier)
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
-- @tparam number count The number of times to play the audio
-- @treturn Audio The repeated audio
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
-- @treturn Audio The reversed audio
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
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32)
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @tparam[opt=true] boolean interleaved Whether to interleave each channel
-- @treturn {[number]...} The resulting audio data
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
-- This is useful as a for iterator, and can be used with @{aukit.play}.
-- @tparam[opt=131072] number chunkSize The size of each chunk
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32)
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds
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
        pos = table.remove(v, 1)
        return v, p
    end, #self.data[1] / self.sampleRate
end

--- Coverts the audio data to a WAV file.
-- @tparam[opt=16] number bitDepth The bit depth of the audio (1 = DFPWM, 8, 16, 24, 32)
-- @treturn string The resulting WAV file data
function Audio:wav(bitDepth)
    -- TODO: Support float data
    bitDepth = expect(1, bitDepth, "number", "nil") or 16
    if bitDepth == 1 then
        local str = self:dfpwm(true)
        return ("<c4Ic4c4IHHIIHHHHIc16c4IIc4I"):pack(
            "RIFF", #str + 72, "WAVE",
            "fmt ", 40, 0xFFFE, #self.data, self.sampleRate, self.sampleRate * #self.data / 8, math_ceil(#self.data / 8), 1,
                22, 1, wavExtensibleChannels[#self.data] or 0, wavExtensible.dfpwm,
            "fact", 4, #self.data[1],
            "data", #str) .. str
    elseif bitDepth ~= 8 and bitDepth ~= 16 and bitDepth ~= 24 and bitDepth ~= 32 then error("bad argument #2 (invalid bit depth)", 2) end
    local data = self:pcm(bitDepth, bitDepth == 8 and "unsigned" or "signed", true)
    local str = ""
    local csize = jit and 7680 or 32768
    local format = ((bitDepth == 8 and "I" or "i") .. (bitDepth / 8)):rep(csize)
    for i = 1, #data - csize, csize do str = str .. format:pack(table_unpack(data, i, i + csize - 1)) end
    str = str .. ((bitDepth == 8 and "I" or "i") .. (bitDepth / 8)):rep(#data % csize):pack(table_unpack(data, math_floor(#data / csize) * csize))
    return ("<c4Ic4c4IHHIIHHc4I"):pack("RIFF", #str + 36, "WAVE", "fmt ", 16, 1, #self.data, self.sampleRate, self.sampleRate * #self.data * bitDepth / 8, #self.data * bitDepth / 8, bitDepth, "data", #str) .. str
end

--- Converts the audio data to DFPWM. All channels share the same encoder, and
-- channels are stored sequentially uninterleaved if `interleaved` is false, or
-- in one interleaved string if `interleaved` is true.
-- @tparam[opt=true] boolean interleaved Whether to interleave the channels
-- @treturn string... The resulting DFPWM data for each channel (only one string
-- if `interleaved` is true)
function Audio:dfpwm(interleaved)
    expect(1, interleaved, "boolean", "nil")
    if interleaved == nil then interleaved = true end
    if interleaved then
        return dfpwm.encode(self:pcm(8, "signed", true))
    else
        local channels = {self:pcm(8, "signed", false)}
        local encode = dfpwm.make_encoder()
        for i = 1, #channels do channels[i] = encode(channels[i]) end
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
-- @section aukit

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
-- @tparam string|table data The audio data, either as a raw string, or a table of nibbles
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @tparam[opt=true] boolean topFirst Whether the top nibble is the first nibble
-- (true) or last (false); ignored if `data` is a table
-- @tparam[opt=true] boolean interleaved Whether each channel is interleaved or separate
-- @tparam[opt=0] number|table predictor The initial predictor value(s)
-- @tparam[opt=0] number|table step_index The initial step index(es)
-- @treturn Audio A new audio object containing the decoded data
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
        step_index = {expect.range(step_index, 0, 15)}
    else
        if channels > #step_index then error("bad argument #7 (table too short)", 2) end
        for i = 1, channels do expect.range(step_index[i], 0, 15) end
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
        for i = 1, len do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            for j = 1, channels do
                local nibble = read()
                step_index[j] = clamp(step_index[j] + ima_index_table[nibble], 0, 88)
                local diff = ((nibble >= 8 and nibble - 16 or nibble) + 0.5) * step[j] / 4
                predictor[j] = clamp(predictor[j] + diff, -32768, 32767)
                step[j] = ima_step_table[step_index]
                d[j][i] = predictor[j] / (predictor[j] < 0 and 32768 or 32767)
            end
        end
    else for j = 1, channels do
        local line = {}
        local predictor, step_index, step = predictor[j], step_index[j], nil
        for i = 1, len do
            if os_epoch "utc" - start > 3000 then start = os_epoch "utc" sleep(0) end
            local nibble = read()
            step_index = clamp(step_index + ima_index_table[nibble], 0, 88)
            local diff = ((nibble >= 8 and nibble - 16 or nibble) + 0.5) * step / 4
            predictor = clamp(predictor + diff, -32768, 32767)
            step = ima_step_table[step_index]
            line[i] = predictor / (predictor < 0 and 32768 or 32767)
        end
        obj.data[j] = line
    end end
    return obj
end

--- Creates a new audio object from DFPWM1a data. All channels are expected to
-- share the same decoder, and are stored interleaved in a single stream.
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

--- Creates a new audio object from a WAV file. This accepts PCM files up to 32
-- bits, including float data, as well as DFPWM files [as specified here](https://gist.github.com/MCJack123/90c24b64c8e626c7f130b57e9800962c).
-- @tparam string data The WAV data to load
-- @treturn Audio A new audio object with the contents of the WAV file
function aukit.wav(data)
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length, dataType, predictor, step_index
    local temp, pos = ("c4"):unpack(data)
    if temp ~= "RIFF" then error("bad argument #1 (not a WAV file)", 2) end
    pos = pos + 4
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "WAVE" then error("bad argument #1 (not a WAV file)", 2) end
    local meta = {}
    while pos <= #data do
        local size
        temp, size, pos = ("<c4I"):unpack(data, pos)
        if temp == "fmt " then
            local chunk = data:sub(pos, pos + size - 1)
            pos = pos + size
            local format
            format, channels, sampleRate, bitDepth = ("<HHIxxxxxxH"):unpack(chunk)
            if format ~= 1 and format ~= 2 and format ~= 3 and format ~= 0xFFFE then error("unsupported WAV file", 2) end
            if format == 1 then
                dataType = bitDepth == 8 and "unsigned" or "signed"
            elseif format == 0x11 then
                dataType = "adpcm"
                -- TODO: read in the correct length values
            elseif format == 3 then
                dataType = "float"
            elseif format == 0xFFFE then
                bitDepth = ("<H"):unpack(chunk, 19)
                local uuid = chunk:sub(25, 40)
                if uuid == wavExtensible.pcm then dataType = bitDepth == 8 and "unsigned" or "signed"
                elseif uuid == wavExtensible.adpcm then dataType = "adpcm"
                elseif uuid == wavExtensible.pcm_float then dataType = "float"
                elseif uuid == wavExtensible.dfpwm then dataType = "dfpwm"
                else error("unsupported WAV file", 2) end
            end
        elseif temp == "data" then
            local data = str_sub(data, pos, pos + size - 1)
            if #data < size then error("invalid WAV file", 2) end
            local obj
            if dataType == "adpcm" then error("unsupported WAV file", 2) -- TODO
            elseif dataType == "dfpwm" then obj = aukit.dfpwm(data, channels, sampleRate)
            else obj = aukit.pcm(data, bitDepth, dataType, channels, sampleRate, true, false) end
            obj.metadata = meta
            obj.info = {dataType = dataType, bitDepth = bitDepth}
            return obj
        elseif temp == "fact" then
            -- TODO
            pos = pos + size
        elseif temp == "LIST" then
            local type = ("c4"):unpack(data, pos)
            if type == "INFO" then
                local e = pos + size
                pos = pos + 4
                while pos < e do
                    local str
                    type, str, pos = ("!2<c4s4Xh"):unpack(data, pos)
                    if wavMetadata[type] then meta[wavMetadata[type]] = tonumber(str) or str end
                end
            else pos = pos + size end
        else pos = pos + size end
    end
    error("invalid WAV file", 2)
end

--- Creates a new audio object from an AIFF file.
-- @tparam string data The AIFF data to load
-- @treturn Audio A new audio object with the contents of the AIFF file
function aukit.aiff(data)
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length, offset
    local temp, pos = ("c4"):unpack(data)
    if temp ~= "FORM" then error("bad argument #1 (not an AIFF file)", 2) end
    pos = pos + 4
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "AIFF" then error("bad argument #1 (not an AIFF file)", 2) end
    local meta = {}
    while pos <= #data do
        local size
        temp, size, pos = (">c4I"):unpack(data, pos)
        if temp == "COMM" then
            local e, m
            channels, length, bitDepth, e, m, pos = (">hIhHI7x"):unpack(data, pos)
            length = length * channels * math_floor(bitDepth / 8)
            local s = bit32_btest(e, 0x8000)
            e = ((bit32_band(e, 0x7FFF) - 0x3FFE) % 0x800)
            sampleRate = math.ldexp(m * (s and -1 or 1) / 0x100000000000000, e)
        elseif temp == "SSND" then
            offset, _, pos = (">II"):unpack(data, pos)
            local data = data:sub(pos + offset, pos + offset + length - 1)
            if #data < length then error("invalid AIFF file", 2) end
            local obj = aukit.pcm(data, bitDepth, "signed", channels, sampleRate, true, true)
            obj.metadata = meta
            return obj
        elseif temp == "NAME" then
            meta.title = data:sub(pos, pos + size - 1)
            pos = pos + size
        elseif temp == "AUTH" then
            meta.artist = data:sub(pos, pos + size - 1)
            pos = pos + size
        elseif temp == "(c) " then
            meta.copyright = data:sub(pos, pos + size - 1)
            pos = pos + size
        elseif temp == "ANNO" then
            meta.comment = data:sub(pos, pos + size - 1)
            pos = pos + size
        else pos = pos + size end
    end
    error("invalid AIFF file", 2)
end

--- Creates a new audio object from an AU file.
-- @tparam string data The AU data to load
-- @treturn Audio A new audio object with the contents of the AU file
function aukit.au(data)
    expect(1, data, "string")
    local magic, offset, size, encoding, sampleRate, channels = (">c4IIIII"):unpack(data)
    if magic ~= ".snd" then error("invalid AU file", 2) end
    if encoding == 2 then return aukit.pcm(data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 8, "signed", channels, sampleRate, true, true)
    elseif encoding == 3 then return aukit.pcm(data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 16, "signed", channels, sampleRate, true, true)
    elseif encoding == 4 then return aukit.pcm(data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 24, "signed", channels, sampleRate, true, true)
    elseif encoding == 5 then return aukit.pcm(data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 32, "signed", channels, sampleRate, true, true)
    elseif encoding == 6 then return aukit.pcm(data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), 32, "float", channels, sampleRate, true, true)
    else error("unsupported encoding type " .. encoding, 2) end
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
-- @tparam number frequency The frequency of the tone in Hertz
-- @tparam number duration The length of the audio in seconds
-- @tparam[opt=1] number amplitude The amplitude of the audio from 0.0 to 1.0
-- @tparam[opt="sine"] "sine"|"triangle"|"sawtooth"|"square" waveType The type of wave to generate
-- @tparam[opt=0.5] number duty The duty cycle of the square wave if selected; ignored otherwise
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @treturn Audio A new audio object with the tone
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
-- @tparam number duration The length of the audio in seconds
-- @tparam[opt=1] number amplitude The amplitude of the audio from 0.0 to 1.0
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @treturn Audio A new audio object with noise
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
-- @tparam {[number]...} data The PCM data to pack
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
        if #data < i + 512 then retval = retval .. str_pack(str_rep(format, #data % 512), table_unpack(data, i, #data))
        else retval = retval .. str_pack(formatChunk, table_unpack(data, i, i+511)) end
    end
    return retval
end

--- Plays back stream functions created by one of the @{aukit.stream} functions
-- or @{Audio:stream}.
-- @tparam function():{{[number]...}...} callback The iterator function that returns each chunk
-- @tparam[opt] function(pos:number) progress A callback to report progress to
-- the caller; if omitted then this argument is the first speaker
-- @tparam[opt] number volume The volume to play the audio at; if omitted then
-- this argument is the second speaker (if provided)
-- @tparam speaker ... The speakers to play on
function aukit.play(callback, progress, volume, ...)
    expect(1, callback, "function")
    expect(2, progress, "function", "table")
    expect(3, volume, "number", "table", "nil")
    local speakers = {...}
    if type(volume) == "table" then
        table.insert(speakers, 1, volume)
        volume = nil
    end
    if type(progress) == "table" then
        table.insert(speakers, 1, progress)
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
            local chunk = table.remove(chunks, 1)
            local fn = {}
            if progress then progress(chunk[2]) end
            chunk = chunk[1]
            for i, v in ipairs(speakers) do fn[i] = function()
                local name = peripheral.getName(v)
                if _HOST:find("CraftOS-PC v2.6.4") and config and not config.get("standardsMode") then
                    v.playAudio(chunk[i] or chunk[1], volume)
                    repeat until select(2, os.pullEvent("speaker_audio_empty")) == name
                else while not v.playAudio(chunk[i] or chunk[1], volume) do
                    repeat until select(2, os.pullEvent("speaker_audio_empty")) == name
                end end
            end end
            parallel.waitForAll(table_unpack(fn))
        end
    end)
    local ok, af, bf
    local aq, bq = {{}}, {{}}
    repeat
        if #aq > 0 then
            local event = table.remove(aq, 1)
            if af == speakers then
                af = nil
                table.insert(aq, 1, event)
            end
            if af == nil or event[1] == af then
                ok, af = coroutine.resume(a, table_unpack(event, 1, event.n))
                if not ok then error(af, 2) end
            end
        end
        if #bq > 0 then
            local event = table.remove(bq, 1)
            if bf == speakers then
                bf = nil
                table.insert(bq, 1, event)
            end
            if bf == nil or event[1] == bf then
                ok, bf = coroutine.resume(b, table_unpack(event, 1, event.n))
                if not ok then error(bf, 2) end
            end
        end
        if coroutine.status(b) == "suspended" and (#aq == 0 or #bq == 0) then
            if af ~= nil and bf ~= nil then
                local event = table.pack(os.pullEvent())
                aq[#aq+1] = event
                bq[#bq+1] = event
            else
                os.queueEvent("__queue_end")
                while true do
                    local event = table.pack(os.pullEvent())
                    if event[1] == "__queue_end" then break end
                    aq[#aq+1] = event
                    bq[#bq+1] = event
                end
            end
        end
    until coroutine.status(b) == "dead" or complete
    while coroutine.status(b) == "suspended" and #bq > 0 do
        local event = table.remove(bq, 1)
        if bf == nil or event[1] == bf then
            ok, bf = coroutine.resume(b, table_unpack(event, 1, event.n))
            if not ok then error(bf, 2) end
        end
    end
    while coroutine.status(b) == "suspended" do
        ok, bf = coroutine.resume(b, os.pullEvent())
        if not ok then error(bf, 2) end
    end
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
-- detection methods to attempt to find the correct data type for files without
-- headers. It is not recommended to rely on the data type/bit depth reported
-- for PCM files - they are merely a suggestion.
-- @tparam string data The audio file to check
-- @treturn "pcm"|"dfpwm"|"wav"|"aiff"|"au"|"flac"|nil The type of audio file detected, or `nil` if none could be found
-- @treturn number|nil The bit depth for PCM data, if the type is "pcm" and the bit depth can be detected
-- @treturn "signed"|"unsigned"|"float"|nil The data type for PCM data, if the type is "pcm" and the type can be detected
function aukit.detect(data)
    expect(1, data, "string")
    if data:match "^RIFF....WAVE" then return "wav"
    elseif data:match "^FORM....AIFF" then return "aiff"
    elseif data:match "^%.snd" then return "au"
    elseif data:match "^fLaC" then return "flac"
    else
        -- Detect data type otherwise
        -- This expects the start or end of the audio to be (near) silence
        for _, bits in pairs(datafmts) do
            local mid, gap = bits[3] == "unsigned" and 2^(bits[2]-1) or 0, bits[3] == "float" and 0.001 or 8 * 2^(bits[2]-8)
            local nums = {pcall(string.unpack, bits[1], data)}
            nums[#nums] = nil
            if table.remove(nums, 1) then
                local allzero, ok = true, true
                for _, v in ipairs(nums) do
                    if v ~= mid then allzero = false end
                    if v < mid - gap or v > mid + gap then ok = false break end
                end
                if ok and not allzero then return "pcm", table_unpack(bits, 2) end
            end
            nums = {pcall(string.unpack, bits[1], data, #data - bits[2])}
            nums[#nums] = nil
            if table.remove(nums, 1) then
                local allzero, ok = true, true
                for _, v in ipairs(nums) do
                    if v ~= mid then allzero = false end
                    if v < mid - gap or v > mid + gap then ok = false break end
                end
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
-- @section aukit.stream

--- Returns an iterator to stream raw PCM data in CC format. Audio will automatically
-- be resampled to 48 kHz, and optionally mixed down to mono. Data *must* be
-- interleaved - this will not work with planar audio.
-- @tparam string|table|function data The audio data, either as a raw string, a
-- table of values (in the format specified by `bitDepth` and `dataType`), or a
-- function that returns either of those types. Functions will be called at
-- least once before returning to get the type of data to use.
-- @tparam[opt=8] number bitDepth The bit depth of the audio (8, 16, 24, 32); if `dataType` is "float" then this must be 32
-- @tparam[opt="signed"] "signed"|"unsigned"|"float" dataType The type of each sample
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @tparam[opt=false] boolean bigEndian Whether the audio is big-endian or little-endian; ignored if data is a table
-- @tparam[opt=false] boolean mono Whether to mix the audio down to mono
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds, or the length of
-- the first chunk if using a function
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

--- Returns an iterator to stream data from DFPWM data. Audio will automatically
-- be resampled to 48 kHz. Multiple channels are expected to be interleaved in
-- the encoded DFPWM data.
-- @tparam string|function():string data The DFPWM data to decode, or a function
-- returning chunks to decode
-- @tparam[opt=48000] number sampleRate The sample rate of the audio in Hertz
-- @tparam[opt=1] number channels The number of channels present in the audio
-- @tparam[opt=false] boolean mono Whether to mix the audio down to mono
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds, or the length of
-- the first chunk if using a function
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
    return function()
        if pos > #data then return nil end
        local d
        if isstr then d = data:sub(pos, pos + 6000 * channels)
        else d = data() if not d then return nil end end
        local audio = decoder(d)
        if audio == nil or #audio == 0 then return nil end
        audio[0], last = last, audio[#audio]
        os.queueEvent("nosleep")
        os.pullEvent()
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
        os.queueEvent("nosleep")
        os.pullEvent()
        local p = pos
        pos = pos + 6000 * channels
        return lines, p * 8 / sampleRate / channels
    end, #data * 8 / sampleRate / channels
end

--- Returns an iterator to stream data from a WAV file. Audio will automatically
-- be resampled to 48 kHz, and optionally mixed down to mono. This accepts PCM
-- files up to 32 bits, including float data, as well as DFPWM files [as specified here](https://gist.github.com/MCJack123/90c24b64c8e626c7f130b57e9800962c).
-- @tparam string|function():string data The WAV file to decode, or a function
-- returning chunks to decode (the first chunk MUST contain the ENTIRE header)
-- @tparam[opt=false] boolean mono Whether to mix the audio to mono
-- @tparam[opt=false] boolean ignoreHeader Whether to ignore additional headers
-- if they appear later in the audio stream
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds
function aukit.stream.wav(data, mono, ignoreHeader)
    local fn
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string")
    local channels, sampleRate, bitDepth, length, dataType
    local temp, pos = ("c4"):unpack(data)
    if temp ~= "RIFF" then error("bad argument #1 (not a WAV file)", 2) end
    pos = pos + 4
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "WAVE" then error("bad argument #1 (not a WAV file)", 2) end
    while pos <= #data do
        local size
        temp, pos = ("c4"):unpack(data, pos)
        size, pos = ("<I"):unpack(data, pos)
        if temp == "fmt " then
            local chunk = data:sub(pos, pos + size - 1)
            pos = pos + size
            local format
            format, channels, sampleRate, bitDepth = ("<HHIxxxxxxH"):unpack(chunk)
            if format ~= 1 and format ~= 2 and format ~= 3 and format ~= 0xFFFE then error("unsupported WAV file", 2) end
            if format == 1 then
                dataType = bitDepth == 8 and "unsigned" or "signed"
            elseif format == 0x11 then
                dataType = "adpcm"
                -- TODO: read in the correct length values
            elseif format == 3 then
                dataType = "float"
            elseif format == 0xFFFE then
                bitDepth = ("<H"):unpack(chunk, 19)
                local uuid = chunk:sub(25, 40)
                if uuid == wavExtensible.pcm then dataType = bitDepth == 8 and "unsigned" or "signed"
                elseif uuid == wavExtensible.adpcm then dataType = "adpcm"
                elseif uuid == wavExtensible.pcm_float then dataType = "float"
                elseif uuid == wavExtensible.dfpwm then dataType = "dfpwm"
                else error("unsupported WAV file", 2) end
            end
        elseif temp == "data" then
            local data = data:sub(pos, pos + size - 1)
            if not fn and #data < size then error("invalid WAV file", 2) end
            if fn then
                local first, f = data
                data = function()
                    if first then f, first = first return f
                    elseif ignoreHeader then
                        local d = fn()
                        if not d then return nil end
                        if d:match "^RIFF....WAVE" then return d:sub(d:match("^RIFF....WAVE.?data....()"))
                        else return d end
                    else return fn() end
                end
            end
            if dataType == "adpcm" then error("unsupported WAV file", 2) -- TODO
            elseif dataType == "dfpwm" then return aukit.stream.dfpwm(data, sampleRate, channels, mono), size / channels / (bitDepth / 8) / sampleRate
            else return aukit.stream.pcm(data, bitDepth, dataType, channels, sampleRate, false, mono), size / channels / (bitDepth / 8) / sampleRate end
        elseif temp == "fact" then
            -- TODO
            pos = pos + size
        else pos = pos + size end
    end
    error("invalid WAV file", 2)
end

--- Returns an iterator to stream data from an AIFF file. Audio will automatically
-- be resampled to 48 kHz, and optionally mixed down to mono.
-- @tparam string|function():string data The AIFF file to decode, or a function
-- returning chunks to decode (the first chunk MUST contain the ENTIRE header)
-- @tparam[opt=false] boolean mono Whether to mix the audio to mono
-- @tparam[opt=false] boolean ignoreHeader Whether to ignore additional headers
-- if they appear later in the audio stream
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds
function aukit.stream.aiff(data, mono, ignoreHeader)
    local fn
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string")
    expect(2, mono, "boolean", "nil")
    local channels, sampleRate, bitDepth, length, offset
    local temp, pos = ("c4"):unpack(data)
    if temp ~= "FORM" then error("bad argument #1 (not an AIFF file)", 2) end
    pos = pos + 4
    temp, pos = ("c4"):unpack(data, pos)
    if temp ~= "AIFF" then error("bad argument #1 (not an AIFF file)", 2) end
    while pos <= #data do
        local size
        temp, pos = ("c4"):unpack(data, pos)
        size, pos = (">I"):unpack(data, pos)
        if temp == "COMM" then
            local e, m
            channels, length, bitDepth, e, m, pos = (">hIhHI7x"):unpack(data, pos)
            length = length * channels * math_floor(bitDepth / 8)
            local s = bit32_btest(e, 0x8000)
            e = ((bit32_band(e, 0x7FFF) - 0x3FFE) % 0x800)
            sampleRate = math.ldexp(m * (s and -1 or 1) / 0x100000000000000, e)
        elseif temp == "SSND" then
            offset, _, pos = (">II"):unpack(data, pos)
            local data = data:sub(pos + offset, pos + offset + length - 1)
            if not fn and #data < length then error("invalid AIFF file", 2) end
            if fn then
                local first, f = data
                data = function()
                    if first then f, first = first return f
                    elseif ignoreHeader then
                        local d = fn()
                        if not d then return nil end
                        if d:match "^FORM....AIFF" then
                            local n, p = d:match("^FORM....AIFF.?SSND(....)....()")
                            offset = (">I"):unpack(n)
                            return d:sub(p + offset)
                        else return d end
                    else return fn() end
                end
            end
            return aukit.stream.pcm(data, bitDepth, "signed", channels, sampleRate, true, mono), length / channels / (bitDepth / 8) / sampleRate
        else pos = pos + size end
    end
    error("invalid AIFF file", 2)
end

--- Returns an iterator to stream data from an AU file. Audio will automatically
-- be resampled to 48 kHz, and optionally mixed down to mono.
-- @tparam string|function():string data The AU file to decode, or a function
-- returning chunks to decode (the first chunk MUST contain the ENTIRE header)
-- @tparam[opt=false] boolean mono Whether to mix the audio to mono
-- @tparam[opt=false] boolean ignoreHeader Whether to ignore additional headers
-- if they appear later in the audio stream
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds
function aukit.stream.au(data, mono, ignoreHeader)
    local fn
    if type(data) == "function" then fn, data = data, data() end
    expect(1, data, "string")
    expect(2, mono, "boolean", "nil")
    local magic, offset, size, encoding, sampleRate, channels = (">c4IIIII"):unpack(data)
    if magic ~= ".snd" then error("invalid AU file", 2) end
    if fn then
        local first, f = data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil), nil
        data = function()
            if first then f, first = first return f
            elseif ignoreHeader then
                local d = fn()
                if not d then return nil end
                if d:match "^.snd" then return d:sub((">I"):unpack(d:sub(5, 8)), nil)
                else return d end
            else return fn() end
        end
    else data = data:sub(offset, size ~= 0xFFFFFFFF and offset + size - 1 or nil) end
    if encoding == 2 then return aukit.stream.pcm(data, 8, "signed", channels, sampleRate, true, mono), size / channels / sampleRate
    elseif encoding == 3 then return aukit.stream.pcm(data, 16, "signed", channels, sampleRate, true, mono), size / channels / 2 / sampleRate
    elseif encoding == 4 then return aukit.stream.pcm(data, 24, "signed", channels, sampleRate, true, mono), size / channels / 3 / sampleRate
    elseif encoding == 5 then return aukit.stream.pcm(data, 32, "signed", channels, sampleRate, true, mono), size / channels / 4 / sampleRate
    elseif encoding == 6 then return aukit.stream.pcm(data, 32, "float", channels, sampleRate, true, mono), size / channels / 4 / sampleRate
    else error("unsupported encoding type " .. encoding, 2) end
end

--- Returns an iterator to stream data from a FLAC file. Audio will automatically
-- be resampled to 48 kHz, and optionally mixed down to mono.
-- @tparam string|function():string data The FLAC file to decode, or a function
-- returning chunks to decode
-- @tparam[opt=false] boolean mono Whether to mix the audio to mono
-- @treturn function():{{[number]...}...},number An iterator function that returns
-- chunks of each channel's data as arrays of signed 8-bit 48kHz PCM, as well as
-- the current position of the audio in seconds
-- @treturn number The total length of the audio in seconds
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
        return self.str:byte(start, e)
    end}, {__len = function(self) return self.final and #self.str or math.huge end}) end
    local function saferesume(coro, ...)
        local res = table.pack(coroutine.resume(coro, ...))
        while res[1] and infn do res = table.pack(coroutine.resume(coro, coroutine.yield(table_unpack(res, 2, res.n)))) end
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
            os.queueEvent("nosleep")
            os.pullEvent()
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
            os.queueEvent("nosleep")
            os.pullEvent()
        end
        pos = pos + #chunk[1] / 48000
        return chunk, pos
    end, len / sampleRate
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
-- @section aukit.effects

--- Amplifies the audio by the multiplier specified.
-- @tparam Audio audio The audio to modify
-- @tparam number multiplier The multiplier to apply
-- @treturn Audio The audio modified
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
-- same sample rate.
-- @tparam Audio audio The audio to modify
-- @tparam number multiplier The multiplier to apply
-- @treturn Audio The audio modified
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
-- @tparam Audio audio The audio to modify
-- @tparam number startTime The start time of the fade, in seconds
-- @tparam number startAmplitude The amplitude of the beginning of the fade
-- @tparam number endTime The end time of the fade, in seconds
-- @tparam number endAmplitude The amplitude of the end of the fade
-- @treturn Audio The audio modified
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
-- @tparam Audio audio The audio to modify
-- @treturn Audio The audio modified
function aukit.effects.invert(audio)
    expectAudio(1, audio)
    for c = 1, #audio.data do
        local ch = audio.data[c]
        for i = 1, #ch do ch[i] = -ch[i] end
    end
    return audio
end

--- Normalizes audio to the specified peak amplitude.
-- @tparam Audio audio The audio to modify
-- @tparam[opt=1] number peakAmplitude The maximum amplitude
-- @tparam[opt=false] boolean independent Whether to normalize each channel independently
-- @treturn Audio The audio modified
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
            for i = 1, #ch do max = math.max(max, math_abs(ch[i])) end
        end
        mult = peakAmplitude / max
    end
    for c = 1, #audio.data do
        local ch = audio.data[c]
        if independent then
            local max = 0
            for i = 1, #ch do max = math.max(max, math_abs(ch[i])) end
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
-- @tparam Audio audio The audio to modify
-- @treturn Audio The audio modified
function aukit.effects.center(audio)
    expectAudio(1, audio)
    for c = 1, #audio.data do
        local ch = audio.data[c]
        for i = 0, #ch - 1, audio.sampleRate do
            local avg = 0
            local l = math.min(#ch - i, audio.sampleRate)
            for j = 1, l do avg = avg + ch[i+j] end
            avg = avg / l
            for j = 1, l do ch[i+j] = clamp(ch[i+j] - avg, -1, 1) end
        end
    end
    return audio
end

--- Trims any extra silence on either end of the specified audio.
-- @tparam Audio audio The audio to modify
-- @tparam[opt=1/65536] number threshold The maximum value to register as silence
-- @treturn Audio The audio modified
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
    local new = audio:sub(s / audio.sampleRate, e / audio.sampleRate)
    audio.data = new.data
    return audio
end

--- Adds a delay to the specified audio.
-- @tparam Audio audio The audio to modify
-- @tparam number delay The amount of time to delay for, in seconds
-- @tparam[opt=0.5] number multiplier The multiplier to apply to the delayed audio
-- @treturn Audio The audio modified
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
-- @tparam Audio audio The audio to modify
-- @tparam[opt=1] number delay The amount of time to echo after, in seconds
-- @tparam[opt=0.5] number multiplier The decay multiplier to apply to the echoed audio
-- @treturn Audio The audio modified
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
-- @tparam Audio audio The audio to modify
-- @tparam[opt=100] number delay The amount of time to reverb after, in **milliseconds**
-- @tparam[opt=0.3] number decay The decay factor to use
-- @tparam[opt=1] number wetMultiplier The wet (reverbed) mix amount
-- @tparam[opt=0] number dryMultiplier The dry (original) mix amount
-- @treturn Audio The audio modified
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
-- @tparam Audio audio The audio to modify
-- @tparam number frequency The cutoff frequency for the filter
-- @treturn Audio The audio modified
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
-- @tparam Audio audio The audio to modify
-- @tparam number frequency The cutoff frequency for the filter
-- @treturn Audio The audio modified
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
