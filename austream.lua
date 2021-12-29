local aukit = require "aukit"

local path = shell.resolve(...)
local file = fs.open(path, "rb")
local data = file.readAll()
file.close()

print("Streaming...")
if path:match("%.wav$") then aukit.stream.wav(data, peripheral.find "speaker")
elseif path:match("%.aiff?$") then aukit.stream.aiff(data, peripheral.find "speaker")
elseif path:match("%.au$") then aukit.stream.au(data, peripheral.find "speaker")
elseif path:match("%.flac$") then aukit.stream.flac(data, peripheral.find "speaker")
else error("Unknown file type.") end