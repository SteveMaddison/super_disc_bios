.DEFINE BIOS_VERSION_SUFFIX "-EN"

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
	.ASC "SELECT B ERASE  Y BACK"
.ENDM
.MACRO I18N_RAM_MENU_LEGEND_ALTERNATE
	.ASC "SELECT B ERASE  A BACK"
.ENDM

.DEFINE I18N_LENGTH_RAM_ERROR 20
.MACRO I18N_RAM_ERROR
	.ASC "A RAM ERROR OCCURRED"
.ENDM

.DEFINE I18N_LENGTH_NO_FILES 20
.MACRO I18N_NO_FILES
	.ASC "   NO FILES FOUND   "
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
	.ASC "        BASIC  FUNCTIONS        "
	.ASC "                                "
	.ASC "  V-RAM                     --  "
	.ASC "  CG-RAM                    --  "
	.ASC "  OAM                       --  "
	.ASC "  WORK RAM                  --  "
	.ASC "  DMA                       --  "
	.ASC "  TIMER                     --  "
	.ASC "  SOUND CHECK               --  "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.DSB (30 - BIOS_VERSION.length - BIOS_VERSION_SUFFIX.length), $24
	  .ASC BIOS_VERSION
	  .ASC BIOS_VERSION_SUFFIX
	  .ASC "  "
	.ASC "                                "
.ENDM

.MACRO I18N_SELF_TEST_SCREEN_2
	.ASC "                                "
	.ASC "      EXPANSION  FUNCTIONS      "
	.ASC "                                "
	.ASC "  EXPANSION RAM             --  "
	.ASC "  BACKUP RAM                --  "
	.ASC "  CD-ROM DECODER            --  "
	.ASC "  CD-PLAYER I/F             --  "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.ASC "                                "
	.DSB (30 - BIOS_VERSION.length - BIOS_VERSION_SUFFIX.length), $24
	  .ASC BIOS_VERSION
	  .ASC BIOS_VERSION_SUFFIX
	  .ASC "  "
	.ASC "                                "
.ENDM

.DEFINE I18N_LENGTH_TEST_COMPLETE 16
.MACRO I18N_TEST_COMPLETE
	.ASC "TESTING COMPLETE"
.ENDM
