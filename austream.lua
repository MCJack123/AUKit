local aukit = require "aukit"

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
    data = ""
    repeat
        local ev, url, msg, bin = os.pullEvent()
        if ev == "websocket_message" and url == path then
            data = data .. msg
            if not bin then print("Warning: A text message was sent. This data may have been corrupted.") end
        end
    until ev == "websocket_closed" and url == path
else
    path = shell.resolve(...)
    local file, err = fs.open(path, "rb")
    if not file then error("Could not open " .. path .. ": " .. err) end
    data = file.readAll()
    file.close()
end

print("Streaming...")
if path:match("%.dfpwm$") then aukit.play(aukit.stream.dfpwm(data, 48000), peripheral.find "speaker")
elseif path:match("%.wav$") then aukit.play(aukit.stream.wav(data), peripheral.find "speaker")
elseif path:match("%.aiff?$") then aukit.play(aukit.stream.aiff(data), peripheral.find "speaker")
elseif path:match("%.au$") then aukit.play(aukit.stream.au(data), peripheral.find "speaker")
elseif path:match("%.flac$") then aukit.play(aukit.stream.flac(data), peripheral.find "speaker")
else error("Unknown file type.") end
