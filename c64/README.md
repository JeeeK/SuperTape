# C64 SuperTape

This is the implementation of the SuperTape format for Commodore C64 as published as listing in c't Magazin 1984/10 (Heise Verlag). It has been already published several months before in a variation which needed a hardware modification on the tape interface. In this newer version this limitation has been overcome by more sophisticated read-logic just by taking the timing of falling edges into account (the tape input is only capable of recognizing falling edges).

A more advanced version named SuperTape D2 has been published in the tape/disk magazine INPUT64 04/1985. It does not only supporting LOAD, VERIFY and SAVE but also the OPEN/PRINT#/INPUT#/GET#/CLOSE interface from BASIC. Currently this project doesn't cover this whole functional extent.

Beside the original implemention which can be found here, the branch here has been widely reworked with improvements in source code style, code arrangement and factoring, making it more compact and and uses faster code parts. Especially the filename handling in conjunction with pattern matching is now solid (toward the better implementations on other platforms).


## Version history

### File types

| Filepattern | Description                                                                       |
| ----------- | --------------------------------------------------------------------------------- |
| s-*.prg     | HypraAss sourcecode (tokenized)                                                   |
| s-*.has     | HypraAss sourcecode converted with petcat (plain)                                 |
| c-*.prg     | CBM program file (binary)                                                         |
| l-*.txt     | HypraAss list output after assembly converted to ASCII (for printing)             |
| y-*.txt     | HypraAss symbol table output after assembly converted to ASCII (for printing)     |


### Versions

**Save #25**: Comments added, fixed filename handling (loading with empty filename).

**Save #24**: Bugfix in read byte with checksum routine! Comments added. 
Bad checksum in header message and FOUND message with longer pause time to recognize these during screen blanking.

**Save #23**: Checksum routine changed to table-based version. Byte read routine with integrated checksum calculation.
Erroneous: source line 3512 wrong destination

**Save #22**: Further comments and labels made self-explaining. Filename handling (syntax check) fixed/improved.

**Save #21**: Save name handling reworked (labels, optimized, less zero-page usage).

**Save #20**: Improvements: byte write routine merged into one, filename handling (extension) fixed, verify flag into bit 7 (easier to handle), checksum calculation optimized.

**Save #19**: Fixed interrupt handling on exit on writing (bugfix).

**Save #18**: Bug in byte write routine and name parsing fixed (bugfix).

**Save #17**: Setup, exit, filename processing, several optimizations for speed and common style.
Improved "JK" version: optimized code, source code structure, error message text not taken from KERNAL in case some alternative ROM is in place (like SpeedDOS+ which doesn't have the tape messages).

**Save #13**: The first archived version, equal to the original listing from c't magazine 1984/10.
Just converted to HypraAss syntax with consistent label naming scheme and reworked comments (german).



### Test data


st-13-test.bas.tap

Tape image, saved with c-st-13 (original version) with 3600 baud, serves as reference.
The saved image is a BASIC program (a simple two-liner with FOR-NEXT).


st2-24-ha.aut.tap

tape image, saved with c-st-14 with 7200 baud, for cross-checking versions.
The saved image contains the assembler HypraAss as a autostart programm (started by RUN after loading).



## Usage


### Starting and setup

Original version:

```
load "c-st 13",8,1
new
sys 53000
```


Improved version:

These ST2 versions are compatible with alternative kernals which do not contain any tape routines, hence missing the error message texts. 

with SpeedDOS+ kernal:
```
load "c-st2-25"
new
sys $cfca
```

with standard kernal:
```
load "c-st2-25",8,1
new
sys 53194
```



### Start addresses

| program/version | entry point (hex) | entry point (dec) |
| --------------- |:-----------------:|:-----------------:|
| st-13           | $CF08             | 53000             |
| st2-23          | $CFCE             | 53198             |
| st2-24          | $CFC9             | 53193             |
| st2-25          | $CFCA             | 53194             |


### BASIC interface


Load the file matching the filename exact ...

`LOAD"FILE",7`

Load the next file starting with "S-" and with extension .BAS ...

`LOAD"S-*.BAS",7`

Load the next file ...

`LOAD"*",7`

or

`LOAD"",7`

Save with 3600 baud ...

`SAVE"FILE",7`

Save with 7200 baud ...

`SAVE"FILE",7,1`

Verify the current programm in memory ...

`VERIFY"FILE",7,1`


## Bugs

### ST2 version bugs:

Not found in the original version ...

* Filename pattern matching: fixed since Save #20

* Checksum calculation for reading: fixed since Save #20
  Details: in case of rollover of the 16 bit checksum value some bits (up to 7) are not counted.



## Programming


### Timer programming

CIA 1, /FLAG pin is the reading line, connected to the datasette port.
Only the falling edge can be detected!


```
  ;*****************************
  ;* timer setzen (baudrate)
  ;*****************************
  ;
  cc74 1008   :5508 -settimer  bpl set1
  cc76 a29c   :5510 -          ldx #hispdl    ;7200
  cc78 a080   :5514 -          ldy #hispdh    ;baud
  cc7a a9bb   :5518 -          lda #hicmp
  cc7c d006   :5522 -          bne set2
  cc7e a246   :5524 -set1      ldx #lospdl    ;3600
  cc80 a081   :5526 -          ldy #lospdh    ;baud
  cc82 a973   :5528 -          lda #locmp
  cc84 8e04dc :5540 -set2      stx cntal      ;timer
  cc87 8c05dc :5542 -          sty cntah      ;value
  cc8a 8557   :5544 -          sta tlim       ;timelimit for t3

; start of initializing the setup of the /FLAG pin for reading
; bit 7 = 0: corresponds to clear bits 0-4, bit 7=1: corresponds to set bits 0-4
; clear all bits ($7F)
  cc8c a97f   :5546 -          lda #$7f
  cc8e 8d0ddc :5548 -          sta icr
; activate /FLAG interrupt bit (raise an interrupt on the falling edge)
; $80 means "set", $10 = bit 4 corresponds to /FLAG
  cc91 a990   :5550 -          lda #$90
  cc93 8d0ddc :5552 -          sta icr        ;timer interrupt
  cc96 60     :5554 -          rts
```


### Bit counting


#### table driven

```
  ;*****************************
  ;* byte lesen mit pruefsumme
  ;*****************************
  ;
  ; y-reg bit7 muss immer 0 sein!
  ;
  cb84 245b   :3510 -rdbytecs  bit wtflag
  cb86 10fc   :3512 -          bpl rdbytecs   ;wait
  cb88 845b   :3514 -          sty wtflag     ;clear flag
  cb8a a55a   :3516 -          lda buff
  cb8c 4a     :3520 -calccs    lsr            ;checksum
  cb8d 4a     :3521 -          lsr            ;high nibble
  cb8e 4a     :3522 -          lsr
  cb8f 4a     :3523 -          lsr            ;bit3 into carry
  cb90 8594   :3525 -          sta tmpx
  cb92 a55a   :3530 -          lda buff
  cb94 2907   :3531 -          and #07        ;just bit0-2
  cb96 aa     :3535 -          tax
  cb97 bdaacb :3540 -          lda bctab,x    ;bitcount bit0-2
  cb9a a694   :3541 -          ldx tmpx       ;bitcount bit4-7
  cb9c 7daacb :3542 -          adc bctab,x    ;carry holds bit3
  cb9f 655d   :3545 -          adc chksml     ;add to checksum
  cba1 855d   :3550 -          sta chksml
  cba3 9002   :3560 -          bcc ccsexit
  cba5 e65e   :3565 -          inc chksmh
  cba7 a55a   :3570 -ccsexit   lda buff
  cba9 60     :3575 -          rts
  ; table values -> bit-count
               3580 -bctab     .by 0,1,1,2,1,2,2,3; nibblewerte 0-7
               3585 -          .by 1,2,2,3,2,3,3,4; nibblewerte 8-15
```


#### iterative, optimized (compared to the original)

```
  cd99 a65d   :6626 -          ldx chksml
  cd9b 4a     :6628 -stpploop  lsr            ;checksum
  cd9c 9007   :6630 -          bcc stppnull   ;calculation
  cd9e e8     :6632 -          inx
  cd9f d0fa   :6634 -          bne stpploop
  cda1 e65e   :6636 -          inc chksmh
  cda3 f0f6   :6637 -          beq stpploop   ;on overrun, continue!
  cda5 d0f4   :6638 -stppnull  bne stpploop   ;all one bits counted?
  cda7 865d   :6639 -          stx chksml
```


