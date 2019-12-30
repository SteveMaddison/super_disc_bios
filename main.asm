;
; ============================= Super Disc BIOS =============================
;
; This main file contains no code and simply serves to tie everything together.
;
.INCLUDE "config.inc"

.INCLUDE "inc/snes.inc"
.INCLUDE "inc/scd.inc"
.INCLUDE "inc/bios.inc"

; Internationisation/translations.
.IFDEF ORIGINAL
  .INCLUDE "i18n/default.inc"
.ELSE
  .IFEQ LANG "en"
    .INCLUDE "i18n/en.inc"
  .ENDIF
.ENDIF

;
; ROM banks $00-03:8000-FFFF are used. These would usually be mirrored to
; $80-$83:8000-FFFF but that space resides in the expansion WRAM in the BIOS
; cartridge in this case.
;
; Main program (title screen, RAM menu)
.BANK 0 SLOT 0
.ORGA $8000
.INCLUDE "bank_00.asm"

; Diagnostics/self tests
.BANK 1 SLOT 0
.ORGA $8000
.INCLUDE "bank_01.asm"

; Data referenced from Electronic Book code.
.BANK 2 SLOT 0
.ORGA $8000
.INCLUDE "bank_02.asm"

; Tile data for various character sets for Electronic Books.
.BANK 3 SLOT 0
.ORGA $8000
.INCLUDE "bank_03.asm"


; Interrupt/reset vectors.
.SNESNATIVEVECTOR
  NMI   $1FF8
  IRQ   $1FFC
.ENDNATIVEVECTOR

.SNESEMUVECTOR
  RESET $8000
.ENDEMUVECTOR

;
; The following is a more valid header than the original, which is completely
; filled with $FF bytes. This can be enabled/disabled via `config.inc`.
;
.IFDEF SNES_HEADER
.SNESHEADER
  ID   "SDBR"
  NAME "SUPER DISC BOOT ROM  "

  SLOWROM
  LOROM

  CARTRIDGETYPE $02
  ROMSIZE $08
  SRAMSIZE $03
  COUNTRY $00
  LICENSEECODE $00
  VERSION $95
.ENDSNES
.ENDIF
