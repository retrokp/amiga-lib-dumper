# amiga-lib-dumper

A tool to view the contents of `amiga.lib` linker libraries.
This tool has been tested to work with various Amiga NDKs for Motorola 68k.

## Usage

    cargo run -- [--sort] amiga.lib

The sort option sorts the result by the unit name so that different NDK amiga.lib libraries
can be compared using commands like diff:

    cargo run -- --sort NDK1.3/amiga.lib > dump13.txt
    cargo run -- --sort NDK3.2/amiga.lib > dump32.txt
    diff --side-by-side dump13.txt dump32.txt

## Short example output

~~~
; unit
;  name: romhunks
;  symbols: [("ps_common", 48), ("ps_size", 38), ("__doprnt", 0), ("ps_empty", 66), ("stuffChar", 76), ("sc_put", 76)]
;  ext_symbols: [("__doprnt", 0)]
__doprnt:
;    [72, 231, 0, 58, 40, 111, 0, 20, 32, 111, 0, 24, 34, 111, 0, 28, 69, 250, 0, 58, 79, 239, 255, 116, 38, 79, 44, 121, 0, 0, 0, 0, 78, 174, 0, 0, 112, 255, 74, 27, 87, 200, 255, 252, 70, 128, 103, 18, 47, 0, 72, 111, 0, 4, 72, 84, 78, 185, 0, 0, 0, 0, 79, 239, 0, 12, 79, 239, 0, 140, 76, 223, 92, 0, 78, 117, 22, 192, 78, 117]
00000000 : 48e7 003a                  MOVEM.L  D1,D3-D5,-(A7)
00000004 : 286f 0014                  MOVEA.L  +20(A7),A4
00000008 : 206f 0018                  MOVEA.L  +24(A7),A0

...

_bltddat      EQU    #$dff000
_custom       EQU    #$dff000
; unit
_romend       EQU    #$ffffff
_romstart     EQU    #$fc0000
_bootrom      EQU    #$f80000
_cartridge    EQU    #$f00000
_AbsExecBase  EQU    #4
~~~

## License

Licensed under <a href="LICENSE-0BSD">0BSD license</a>.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be licensed as above, without any
additional terms or conditions.
