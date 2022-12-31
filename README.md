# AUKit
AUKit: Audio decoding and processing framework for ComputerCraft

## Usage
Download `aukit.lua` to your computer. You can also grab `austream` and `auplay` if you'd like. Then simply use `local aukit = require "aukit"` to load the library in a program.

Basic audio playback can be achieved in only two lines:

```lua
local aukit = require "aukit"
aukit.play(aukit.stream.wav(io.lines("file.wav", 48000)), peripheral.find("speaker"))
```

For information about the API, see [the Lua docs](https://mcjack123.github.io/AUKit/).

### `austream`/`auplay`
Both `austream` and `auplay` take a path as an argument. `austream` additionally supports HTTP(S), WS(S), and Rednet URLs. The difference between the two programs is that `auplay` preloads files, always plays in mono, and normalizes the audio. `austream` does all processing while playing, and supports multiple speakers, but does no processing outside of resampling (and mixing to mono if there's multiple audio channels but only one speaker).

Both programs require the argument to end with the correct file extension - this is used to detect the file type. The file type can also be forced by adding `type=<extension>` to the parameter list. If using WebSocket or Rednet URLs, add a path with a (fake) file name which has the required file extension. Rednet does not require a path, but if none is given then 8-bit signed PCM at 48kHz is assumed. Files with the extension `.pcm` or `.raw` are taken as raw PCM data. Unspecified arguments to PCM data use the defaults [listed by `aukit.stream.pcm`](https://mcjack123.github.io/AUKit/#aukit.stream.pcm).

`austream` accepts Rednet URIs in the format `rednet[+<protocol>]://<id>[/<path>]`. If a protocol is specified, only messages with the specified protocol will be accepted. If a path is specified, it will be sent to the other computer before reading any data. If the ID is `-1`, all senders will be accepted, but no paths will be sent. The sender must send `nil` to report no more data is available. Example: `austream rednet+wav://6/file.wav`

To avoid overflowing the CC event queue, it is recommended that a delay is added to senders.

#### Parameter list
`austream` also accepts parameters for the file in a second argument using a table-like format (quotes are optional for strings): `austream file.bin type=pcm,sampleRate=44100,bitDepth=16,dataType=signed,channels=2,bigEndian=false`

The following options are supported in the parameter list:
- `type` [all]: Type of file to load (`pcm`, `dfpwm`, `wav`, `aiff`, `au`, `flac`)
- `volume` [all]: Playback volume from 0.0 to 3.0 (default 1.0)
- `mono` [all]: Whether to mix audio down to mono (`true`, `false`) (default false)
- `interpolation` [all]: Interpolation mode to use when resampling (`none`, `linear`, `cubic`)
- `sampleRate` [pcm, dfpwm]: Sample rate of the audio (default 48000 Hz)
- `channels` [pcm, dfpwm]: Number of channels in the file (default 1)
- `bitDepth` [pcm]: Bit depth of each sample (8, 16, 24, 32) (default 8)
- `dataType` [pcm]: Type of data of each sample (`signed`, `unsigned`, `float`) (default signed)
- `bigEndian` [pcm]: Whether integers are in big endian form (`true`, `false`) (default false)

### `auconvert`
`auconvert` is a program similar to FFmpeg that allows you to convert and modify audio files using AUKit. It is currently in beta, and has not been thoroughly tested; however, it appears to work.

```
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
     --interpolation <type>         Set the interpolation type for audio scaling (none, linear, cubic)
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
```

## License
AUKit is licensed under the MIT license.
