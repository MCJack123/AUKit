local aukit = require "aukit"
local speaker = peripheral.find "speaker"

local path = shell.resolve(...)
local file = fs.open(path, "rb")
local data = file.readAll()
file.close()

print("Loading file...")
local audio
if path:match("%.wav$") then audio = aukit.wav(data)
elseif path:match("%.aiff?$") then audio = aukit.aiff(data)
elseif path:match("%.au$") then audio = aukit.au(data)
elseif path:match("%.flac$") then audio = aukit.flac(data)
else error("Unknown file type!") end
sleep(0)

print("Resampling...")
local resamp = audio:resample(48000)
sleep(0)
print("Converting to mono...")
local mono = resamp:mono()
sleep(0)
print("Normalizing...")
local normal = aukit.effects.normalize(mono, 0.8)
sleep(0)

print("Playing.")
for _, chunk in normal:stream() do while not speaker.playAudio(chunk) do os.pullEvent("speaker_audio_empty") end end