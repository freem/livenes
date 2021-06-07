# livenes
NES homebrew to interactively write values to the 2A03 sound registers

Tool previously released as binary only in 2014 here:
http://ploguechipsounds.blogspot.com/2014/09/plogue-livenes.html

And shown in these vids:
https://www.youtube.com/watch?v=q7uoukR4o3M
https://www.instagram.com/wwwploguecom/p/BGzuUG4CnTq/ (FDS version)

This includes a VRC6 and FDS variant of the tool, which allows you to switch between sound regions by hitting select on the D-PAD.

The batch files are there as examples. 
If you add nintendulator.exe in the folder where you compile, it will auto start the emulator with the rom you freshly build.

## VT02 version
Some hardware produced by V.R. Technology allows for the use of a second APU.

There are two binaries included:
* live_VT02.nes - for use with NintendulatorNRS
* live_VT02.bin - for use with real hardware, NintendulatorNRS, EmuVT, or MAME

This has not yet been tested on real hardware. NintendulatorNRS seems to work.
