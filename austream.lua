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

print("Streaming...")
if path:match("%.dfpwm$") then aukit.play(aukit.stream.dfpwm(data, 48000), peripheral.find "speaker")
elseif path:match("%.wav$") then aukit.play(aukit.stream.wav(data), peripheral.find "speaker")
elseif path:match("%.aiff?$") then aukit.play(aukit.stream.aiff(data), peripheral.find "speaker")
elseif path:match("%.au$") then aukit.play(aukit.stream.au(data), peripheral.find "speaker")
elseif path:match("%.flac$") then aukit.play(aukit.stream.flac(data), peripheral.find "speaker")
else error("Unknown file type.") end
