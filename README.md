# AUKit
AUKit: Audio decoding and processing framework for ComputerCraft

## Usage
Download `aukit.lua` to your computer. You can also grab `austream` and `auplay` if you'd like. Then simply use `local aukit = require "aukit"` to load the library in a program.

Both `austream` and `auplay` take a path as an argument. `austream` additionally supports HTTP(S) and WS(S) URLs. The difference between the two programs is that `auplay` preloads files, always plays in mono, and normalizes the audio. `austream` does all processing while playing, and supports multiple speakers, but does no processing outside of resampling (and mixing to mono if there's multiple audio channels but only one speaker).

For information about the API, see [the Lua docs](https://mcjack123.github.io/AUKit/).

## License
AUKit is licensed under the MIT license.
