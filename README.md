# AUKit
AUKit: Audio decoding and processing framework for ComputerCraft

## Usage
Download `aukit.lua` to your computer. You can also grab `austream` and `auplay` if you'd like. Then simply use `local aukit = require "aukit"` to load the library in a program.

Both `austream` and `auplay` take a path as an argument. `austream` additionally supports HTTP(S), WS(S), and Rednet URLs. The difference between the two programs is that `auplay` preloads files, always plays in mono, and normalizes the audio. `austream` does all processing while playing, and supports multiple speakers, but does no processing outside of resampling (and mixing to mono if there's multiple audio channels but only one speaker).

Both programs require the argument to end with the correct file extension - this is used to detect the file type. If using WebSocket or Rednet URLs, add a path with a (fake) file name which has the required file extension. Rednet does not require a path, but if none is given then 8-bit signed PCM at 48kHz is assumed. Files with the extension `.pcm` or `.raw` are taken as raw PCM data, and parameters for the file can be specified in a second argument using a table-like format: `austream file.pcm sampleRate=44100,bitDepth=16,dataType="signed",channels=2,bigEndian=false`. Unspecified arguments use the defaults [listed by `aukit.stream.pcm`](https://mcjack123.github.io/AUKit/#aukit.stream.pcm).

`austream` accepts Rednet URIs in the format `rednet[+<protocol>]://<id>[/<path>]`. If a protocol is specified, only messages with the specified protocol will be accepted. If a path is specified, it will be sent to the other computer before reading any data. If the ID is `-1`, all senders will be accepted, but no paths will be sent. The sender must send `nil` to report no more data is available. Example: `austream rednet+wav://6/file.wav`

To avoid overflowing the CC event queue, it is recommended that a delay is added to senders.

For information about the API, see [the Lua docs](https://mcjack123.github.io/AUKit/).

## License
AUKit is licensed under the MIT license.
