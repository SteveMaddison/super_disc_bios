

; Ensure the above are all the same length, so that overwriting one with the
; other will blank out the previous text with spaces.
.DEFINE I18N_LENGTH_STATUS 16
.MACRO I18N_STATUS_ACCESS
	.ASC "     ACCESS     "
.ENDM
.MACRO I18N_STATUS_PUSH_START
	.ASC "   PUSH START   "
.ENDM
.MACRO I18N_STATUS_NOW_LOADING
	.ASC " NOW LOADING... "
.ENDM
.MACRO I18N_STATUS_NO_DISC
    .ASC "    NO DISC     "
.ENDM
.MACRO I18N_STATUS_NO_CD_ROM_SYSTEM
	.ASC "NO CD-ROM SYSTEM"
.ENDM
.MACRO I18N_STATUS_DISC_ERROR
	.ASC "   DISC ERROR   "
.ENDM
.MACRO I18N_STATUS_NOT_SUPER_DISC
	.ASC " NOT SUPER DISC "
.ENDM
.MACRO I18N_STATUS_MUSIC_DISC
	.ASC "   MUSIC DISC   "
.ENDM
.MACRO I18N_STATUS_IPL_NOT_FOUND
	.ASC " IPL NOT FOUND  "
.ENDM
.MACRO I18N_STATUS_M_COM_ERROR
	.ASC "  M-COM ERROR   "
.ENDM
.MACRO I18N_STATUS_NOT_DEFINED
	.ASC "  NOT DEFINED   "
.ENDM
.MACRO I18N_STATUS_TRAY_OPEN
	.ASC "   TRAY OPEN    "
.ENDM
.MACRO I18N_STATUS_NOT_EB
	.ASC "     NOT EB     "
.ENDM
.MACRO I18N_STATUS_ELECTRONIC_BOOK
	.ASC "ELECTRONIC BOOK "
.ENDM

;
; Backup RAM screen
;
.DEFINE I18N_LENGTH_TITLE_BACKUP_RAM 10
.MACRO I18N_TITLE_BACKUP_RAM
	.ASC "BACKUP RAM"
.ENDM

.DEFINE I18N_LENGTH_RAM_MENU_LEGEND 22
.MACRO I18N_RAM_MENU_LEGEND
	.DB  $DA,$DB 	; kanji meaning "select/choose"
	.DB  $AA,$AB 	; hiragana "bu"
	.ASC "   B "
	.DB  $D6,$D7 	; kanji for "erase"
	.DB  $40,$41 	; hiragana "su"
	.ASC "   Y "
	.DB  $D8,$D9 	; kanji for "back"
	.DB  $78,$79 	; hiragana "ru"
.ENDM
.MACRO I18N_RAM_MENU_LEGEND_ALTERNATE
	.DB  $DA,$DB 	; kanji meaning "select/choose"
	.DB  $AA,$AB 	; hiragana "bu"
	.ASC "   B "
	.DB  $D6,$D7 	; kanji for "erase"
	.DB  $40,$41 	; hiragana "su"
	.ASC "   A " 	; This is "Y" in the other copy.
	.DB  $D8,$D9 	; kanji for "back"
	.DB  $78,$79 	; hiragana "ru"
.ENDM

.DEFINE I18N_LENGTH_RAM_ERROR 20
.MACRO I18N_RAM_ERROR
	; Japanese text for "There is a RAM error"
	.ASC "RAM "
	.DB $52,$53,$D2,$D3,$D4,$D5,$88,$89
	.DB $28,$29,$76,$77,$64,$65,$40,$41
.ENDM

.DEFINE I18N_LENGTH_NO_FILES 20
.MACRO I18N_NO_FILES
	; Japanese text for "There are no files"
	.DB $5E,$5F,$BC,$BD,$2A,$2B,$78,$79,$88,$89
	.DB $28,$29,$76,$77,$64,$65,$42,$43,$82,$83
.ENDM

;
; Monitor/diagnostic main menu
;
.DEFINE I18N_LENGTH_MONITOR_MENU (32 * 8)
.MACRO I18N_MONITOR_MENU
	.ASC "                                "
	.ASC "      SFX-100 MONITOR MENU      "
	.ASC "                                "
	.ASC "          E X I T               "
	.ASC "        1.SELF CHECK            "
	.ASC "        2.ADPCM                 "
	.ASC "        3.COMMUNICATION         "
	.ASC "        4.CXD-1800              "
.ENDM

.MACRO I18N_SELF_TEST_SCREEN_1
	.ASC "                                "
	.ASC "          "
	  .DB $34,$35,$62,$63,$82,$83,$34,$35,$58,$59,$2C,$2D; "basic functions"
	  .ASC "          "
	.ASC "                                "
	.ASC "  V-RAM                     --  "
	.ASC "  CG-RAM                    --  "
	.ASC "  OAM                       --  "
	.ASC "  WORK RAM                  --  "
	.ASC "  DMA                       --  "
	.ASC "  TIMER                     --  "
	.ASC "  "
	  .DB  $30,$31,$82,$83,$8E,$8F,$82,$83 ; "sound"
	  .ASC "CHECK             --  "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.DSB (30 - BIOS_VERSION.length), $24
	  .ASC BIOS_VERSION
	  .ASC "  "
	.ASC "                                "
.ENDM

.MACRO I18N_SELF_TEST_SCREEN_2
	.ASC "                                "
	.ASC "        "
	  .DB $32,$33,$36,$37,$48,$49,$CA,$CB,$2C,$2D,$34,$35,$58,$59,$2C,$2D; "expansion functions"
	  .ASC "        "
	.ASC "                                "
	.ASC "  "
	  .DB $32,$33,$36,$37,$48,$49 $CA,$CB,$2C,$2D; "expansion"
	  .ASC "RAM             --  "
	.ASC "  BACKUP RAM                --  "
	.ASC "  CD-ROM DECODER            --  "
	.ASC "  CD-PLAYER I/F             --  "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.DSB (30 - BIOS_VERSION.length), $24
	  .ASC BIOS_VERSION
	  .ASC "  "
	.ASC "                                "
.ENDM

.DEFINE I18N_LENGTH_TEST_COMPLETE 20
.MACRO I18N_TEST_COMPLETE
	; Japanese message for "test complete".
	.DB $3E,$3F,$C8,$C9,$2C,$2D,$76,$77,$CA,$CB,$2C,$2D,$3E,$3F,$64,$65,$3E,$3F,$46,$47
.ENDM
