;
; Build configuration
;

; Define this to generate the original prototype ROM image with an MD5 checksum
; of 865d9f3208dd32d8ccf8f5d85b3e18bc. Enabling this will cause a warning when
; assembled, due to a conflict between the literal header and that created by
; the assembler.
.DEFINE ORIGINAL

; All other configuration is dependent on ORIGINAL not being set.
.IFNDEF ORIGINAL
  .DEFINE LANG "en"
  ;.DEFINE SNES_HEADER

  .DEFINE REGION "NTSC"
  ;.DEFINE REGION "PAL"
.ELSE
  .DEFINE REGION "NTSC"
.ENDIF

; Frames per second influences some delays/timers. Others are not easily
; expressed in terms of fractions for both 50 and 60 Hz but these do not seem
; to be critical (e.g. joypad auto repeat delays) so are left as-is.
.IFEQ REGION "PAL"
  .DEFINE FPS 50
.ELSE
  .DEFINE FPS 60
.ENDIF

.IFDEF ORIGINAL
  .DEFINE BIOS_VERSION "0.95"
.ELSE
  .DEFINE BIOS_VERSION "0.96"
.ENDIF
