;
; Constants, directives and variables used for the BIOS program(s).
;

.LOROM

.MEMORYMAP
  SLOTSIZE $8000
  DEFAULTSLOT 0
  SLOT 0 $8000
.ENDME

.ROMBANKMAP
  BANKSTOTAL 4
  BANKSIZE $8000
  BANKS 4
.ENDRO

; Map ASCII to the representation used internally for text on screen, SRAM
; file names, etc. Japanses characters follow this from $28 onwards.
.ASCIITABLE
  MAP "0" TO "9" = $00
  MAP "A" TO "Z" = $0A
  MAP " "        = $24
  MAP "/"        = $25
  MAP "-"        = $26
  MAP "."        = $27
.ENDA

; BG1 is used for main background graphics
.DEFINE VRAM_PAGE_BG1                   $28
.DEFINE VRAM_ADDR_BG1                   (VRAM_PAGE_BG1 << 8)
.DEFINE TILE_OFFSET_BG1                 $0000

; BG3 is used to overlay text etc. on BG1.
; Writes are staged in a WRAM buffer then DMA-ed into VRAM.
.DEFINE WRAM_ADDR_BG3                   $1800
.DEFINE VRAM_PAGE_BG3                   $60
.DEFINE VRAM_ADDR_BG3                   (VRAM_PAGE_BG3 << 8)
; Text tiles start here
.DEFINE TILE_OFFSET_BG3                 $4000
.DEFINE FONT_TILESET_SIZE               $1000

; Minor variations on there stuff if stored, compared to the main program.
.DEFINE WRAM_ADDR_BG3_DIAG              $1000
.DEFINE VRAM_ADDR_BG3_DIAG              $6000
.DEFINE WRAM_ADDR_BG3_DIAG_CXD_READ     $1000
.DEFINE WRAM_ADDR_BG3_DIAG_CXD_WRITE    $1700
.DEFINE WRAM_ADDR_BG3_DIAG_CXD_UNKNOWN  $1000
.DEFINE VRAM_ADDR_BG3_DIAG_COMMS        $1800

.DEFINE CXD_DIAG_BUFFER_BANK            $7E
.DEFINE CXD_DIAG_BUFFER_ADDRESS         $2000

.DEFINE CDROM_TO_VRAM_BUFFER_BANK       $83
.DEFINE CDROM_TO_VRAM_BUFFER_ADDRESS    $C000

; Area of the screen used in most cases.
.DEFINE SCREEN_WIDTH                    32
.DEFINE SCREEN_HEIGHT                   28
; Electronic book code uses the full 32x32 area.
.DEFINE SCREEN_HEIGHT_RAW               32
; Handy precalculated values in bytes and words.
.DEFINE SCREEN_WIDTH_W                  SCREEN_WIDTH
.DEFINE SCREEN_WIDTH_B                  SCREEN_WIDTH_W * 2
.DEFINE SCREEN_SIZE_W                   (SCREEN_WIDTH_W * SCREEN_HEIGHT)
.DEFINE SCREEN_SIZE_B                   (SCREEN_WIDTH_B * SCREEN_HEIGHT)

; Handy combinations of INIDISP values used in the BIOS code.
.DEFINE INIDISP_MASTER_BRIGHTNESS_FULL  ($FF & INIDISP_MASTER_BRIGHTNESS)
.DEFINE INIDISP_BLANK                   (INIDISP_FORCED_BLANKING|INIDISP_MASTER_BRIGHTNESS_FULL)
.DEFINE INIDISP_NO_BLANK                INIDISP_MASTER_BRIGHTNESS_FULL

; Mechacon Commands (for BIOS functions)
.DEFINE MECHACON_SEEK_TR_INDX           $00
.DEFINE MECHACON_SEEK_MMSSFF            $01
.DEFINE MECHACON_STOP                   $02
.DEFINE MECHACON_PLAY                   $03
.DEFINE MECHACON_PAUSE                  $04
.DEFINE MECHACON_OPEN_CLOSE             $05
.DEFINE MECHACON_FAST_FORWARD           $06
.DEFINE MECHACON_FAST_REVERSE           $07
.DEFINE MECHACON_FORWARD                $08
.DEFINE MECHACON_REVERSE                $09
.DEFINE MECHACON_KEY_DIRECT             $0A
.DEFINE MECHACON_KEY_IGNORE             $0B
.DEFINE MECHACON_CONTINOUS              $0C
.DEFINE MECHACON_TRACK_PAUSE            $0D
.DEFINE MECHACON_INDEX_PAUSE            $0E
.DEFINE MECHACON_REQ_SUB_Q              $0F
.DEFINE MECHACON_REQ_STATUS             $10
.DEFINE MECHACON_NORMAL_SPEED           $11
.DEFINE MECHACON_DOUBLE_SPEED           $12
.DEFINE MECHACON_FLUSH                  $13

; Diagnostic routines index the Mechacon commands in a slightly different order.
.DEFINE DIAG_MECHACON_SEEK_TR_INDX      $00
.DEFINE DIAG_MECHACON_SEEK_MMSSFF       $01
.DEFINE DIAG_MECHACON_STOP              $02
.DEFINE DIAG_MECHACON_PLAY              $03
.DEFINE DIAG_MECHACON_PAUSE             $04
.DEFINE DIAG_MECHACON_OPEN_CLOSE        $05
.DEFINE DIAG_MECHACON_FAST_FORWARD      $06
.DEFINE DIAG_MECHACON_FAST_REVERSE      $07
.DEFINE DIAG_MECHACON_FORWARD           $08
.DEFINE DIAG_MECHACON_REVERSE           $09
.DEFINE DIAG_MECHACON_KEY_DIRECT        $0A
.DEFINE DIAG_MECHACON_KEY_IGNORE        $0B
.DEFINE DIAG_MECHACON_CONTINOUS         $0C
.DEFINE DIAG_MECHACON_TRACK_PAUSE       $0D
.DEFINE DIAG_MECHACON_INDEX_PAUSE       $0E
.DEFINE DIAG_MECHACON_NORMAL_SPEED      $0F
.DEFINE DIAG_MECHACON_DOUBLE_SPEED      $10
.DEFINE DIAG_MECHACON_REQ_SUB_Q         $11
.DEFINE DIAG_MECHACON_REQ_STATUS        $12
.DEFINE DIAG_MECHACON_FLUSH             $13

; Mechacon drive states
.DEFINE MECHACON_STATE_NO_DISC          $00
.DEFINE MECHACON_STATE_STOP             $01
.DEFINE MECHACON_STATE_PLAY             $02
.DEFINE MECHACON_STATE_PAUSE            $03
.DEFINE MECHACON_STATE_FAST_FORWARD     $04
.DEFINE MECHACON_STATE_FAST_REVERSE     $05
.DEFINE MECHACON_STATE_SLOW_FORWARD     $06
.DEFINE MECHACON_STATE_SLOW_REVERSE     $07
.DEFINE MECHACON_STATE_ACCESS_SEEK      $0A
.DEFINE MECHACON_STATE_ACCESS_TOC       $0B
.DEFINE MECHACON_STATE_TRAY_OPEN        $0C

.DEFINE MECHACON_DISC_TRACK_AUDIO       $00
.DEFINE MECHACON_DISC_TRACK_DATA        $01

; for cdrom_LoadFromDisc
.DEFINE SUPERCD_READ_MODE_VRAM          $80
.DEFINE SUPERCD_READ_MODE_LBA           $40
.DEFINE SUPERCD_READ_MODE_ADPCM         $20
.DEFINE SUPERCD_READ_MODE_SKIP          $10

.DEFINE SRAM_SIZE                       $2000
.DEFINE SRAM_BANK                       $90
.DEFINE SRAM_START                      $8000
.DEFINE SRAM_FREE_SPACE                 SRAM_START
.DEFINE SRAM_NUMBER_OF_FILES            $8002
.DEFINE SRAM_CHECKSUM                   $8003
.DEFINE SRAM_MAX_FILES                  32
.DEFINE SRAM_FILE_NAME_LENGTH           14
.DEFINE SRAM_DIR_BUFFER                 $0400
.DEFINE SRAM_DIR_ENTRY_LENGTH           (SRAM_FILE_NAME_LENGTH + 2)
.DEFINE SRAM_DIR_LENGTH                 (SRAM_FILE_NAME_LENGTH * SRAM_DIR_ENTRY_LENGTH)
.DEFINE SRAM_FILES_ON_SCREEN            8

.DEFINE CD_HEADER_BUFFER                $0400
.DEFINE CD_HEADER_SIZE                  40
.DEFINE CD_CODE_BUFFER_ADDRESS          $1000
.DEFINE CD_CODE_BUFFER_BANK             $00
.DEFINE CD_SECTOR_SIZE                  2048
.DEFINE CD_SECTOR_SIZE_MODE2            2336
.DEFINE CD_MAX_SUBQ_ERRORS              15

; Text data structure
.DEFINE TEXT_OFFSET_POSITION            0
.DEFINE TEXT_OFFSET_COLOUR              2
.DEFINE TEXT_OFFSET_LENGTH              4
.DEFINE TEXT_OFFSET_STRING              5

.DEFINE TEXT_COLOUR_WHITE               $2000  ; Depending on palette
.DEFINE TEXT_COLOUR_GREEN               $2000
.DEFINE TEXT_COLOUR_BLACK               $2200
.DEFINE TEXT_COLOUR_CYAN                $2400
.DEFINE TEXT_COLOUR_RED                 $2800
.DEFINE TEXT_COLOUR_YELLOW              $2C00

;
; Variables used by the main menu and some reused by various routines for the
; same purposes.
.ENUM $0200
  diag_joy1l_previous:                  db      ; Main menu variables, accessed mostly using
  diag_joy1h_previous:                  db      ; 8-bit addressing, so there's a lot of "& $FF"
  diag_joy1l:                           db      ; in the following code.
  diag_joy1h:                           db      ; "
  diag_delay_minor:                     db      ; "
  diag_delay_major:                     db      ; "
  diag_text_attributes:                 dw      ; Used to save colour attributes
  diag_top_tile:                        dw      ; VRAM address of top text tile
  diag_bottom_tile:                     dw      ; VRAM address of bottom text tile
  diag_menu_position:                   db      ; Used for main menu only
  diag_stack_pointer:                   dw      ; Used by CXD self test only
.ENDE

; Just for readability. Some calculations relating to this are done bit-wise,
; so any changes here will require changes there, too.
.DEFINE DIAG_MENU_ITEMS                 4

;
; Constant/Variables for Electronic Book
;
.DEFINE EBOOK_READ_BUFFER_BANK          $7E
.DEFINE EBOOK_READ_BUFFER_ADDRESS       $2000
.DEFINE EBOOK_CHAR_BUFFER_ADDRESS       $3800
.DEFINE EBOOK_START_BUFFER_ADDRESS      $5000
; Following buffer is reused for different purposes. The aliases here are just
; to aid readability.
.DEFINE EBOOK_CATALOG_BUFFER_ADDRESS    $8000
.DEFINE EBOOK_TEMP_BUFFER_ADDRESS       EBOOK_CATALOG_BUFFER_ADDRESS
.DEFINE EBOOK_PAGE_BUFFER_ADDRESS       EBOOK_CATALOG_BUFFER_ADDRESS

; Above with bank added.
.DEFINE EBOOK_READ_BUFFER_ADDRESS_L     (EBOOK_READ_BUFFER_BANK << 16) | EBOOK_READ_BUFFER_ADDRESS
.DEFINE EBOOK_CHAR_BUFFER_ADDRESS_L     (EBOOK_READ_BUFFER_BANK << 16) | EBOOK_CHAR_BUFFER_ADDRESS
.DEFINE EBOOK_START_BUFFER_ADDRESS_L    (EBOOK_READ_BUFFER_BANK << 16) | EBOOK_START_BUFFER_ADDRESS
.DEFINE EBOOK_CATALOG_BUFFER_ADDRESS_L  (EBOOK_READ_BUFFER_BANK << 16) | EBOOK_CATALOG_BUFFER_ADDRESS
.DEFINE EBOOK_TEMP_BUFFER_ADDRESS_L     EBOOK_CATALOG_BUFFER_ADDRESS_L
.DEFINE EBOOK_PAGE_BUFFER_ADDRESS_L     EBOOK_CATALOG_BUFFER_ADDRESS_L

.DEFINE EBOOK_VRAM_BUFFER_ADDRESS       $0000
.DEFINE EBOOK_VRAM_BUFFER_BANK          $7F
.DEFINE EBOOK_VRAM_BUFFER_ADDRESS_L     (EBOOK_VRAM_BUFFER_BANK << 16) | EBOOK_VRAM_BUFFER_ADDRESS

.DEFINE EBOOK_CATALOG_RECORD_SIZE       40
.DEFINE EBOOK_SCROLL_STEP               $0400 >> 8
.DEFINE EBOOK_GROUP_NAME_LENGTH         8

.ENUM $0090
  ebook_joypad_status_queue:            dsb 32
  ebook_joypad_status_queue_tail:       dw
  ebook_joypad_status_queue_head:       dw
  ebook_joypad_delay_l:                 db
  ebook_joypad_delay_h:                 db
.ENDE
.ENUM $00BE
  ebook_vram_buffer_pointer:            dw
  ebook_char_data_pointer:              dw
  ebook_char_data_bank:                 db
  ebook_character_code:                 dw
  ebook_screen_content_pointer:         dw
  ebook_graphics_init_flag:             db
  ebook_vram_quarter:                   db
  ebook_cursor_column:                  db
  ebook_cursor_row:                     db
  ebook_flag_link:                      db
  ebook_indent:                         db
  ebook_double_width:                   db
  ebook_scroll_l:                       db
  ebook_scroll_h:                       db
.ENDE

.ENUM $1000
  ebook_tmp:                            db
  ebook_group_dest_l:                   db
  ebook_group_dest_h:                   db
  ebook_group_dest_b:                   db
  ebook_group_name:                     dsb EBOOK_GROUP_NAME_LENGTH + 1
  ebook_src_l:                          db
  ebook_src_m:                          db
  ebook_src_h:                          db
  ebook_transfer_length:                dw
.ENDE

;
; Variables for diagnostics/self check routines.
;
.ENUM $1700
  ; VRAM
  self_test_vram_value:                 dw
  ; OAM
  self_test_oam_test_pattern_l:         db
  self_test_oam_test_pattern_h:         db
.ENDE
.ENUM $1708
  ; DMA
  self_test_dma_value:                  db
  ; Timer
  self_test_timer_stack_pointer:        dw
  self_test_timer_htime:                dw
  self_test_timer_vtime:                dw
  self_test_timer_ophct:                dw
  self_test_timer_opvct:                dw
  ; Mechacon
  self_test_mechacon_response_buffer:   dsb 6
  self_test_mechacon_nmi_counter:       dw
  self_test_mechacon_xxx:               dw
  self_test_mechacon_last_response_l:   db
  self_test_mechacon_last_response_h:   db
  self_test_mechacon_response_length:   dw
.ENDE

.DEFINE CXD_READ_BUFFER_LENGTH          50
.DEFINE CXD_WRITE_BUFFER_LENGTH         20

.DEFINE CXD_TEST_MODE_READ              0
.DEFINE CXD_TEST_MODE_WRITE             1
.DEFINE CXD_TEST_MODE_UNKNOWN           2

.DEFINE CXD_IRQ_ALL                     0
.DEFINE CXD_IRQ_1TIME                   1
.DEFINE CXD_IRQ_IGNORE                  2

.ENUM $0200
  cxd_write_cursor:                     db
  cxd_read_cursor:                      db
  cxd_joypad_l:                         db
  cxd_joypad_h:                         db
  cxd_joypad_l_raw:                     db
  cxd_joypad_h_raw:                     db
  cxd_joypad_delay_l:                   db
  cxd_joypad_delay_h:                   db
  cxd_digit_offset:                     dw
  cxd_test_mode:                        db
  cxd_write_buffer:                     dw
  cxd_read_buffer:                      dw
  cxd_irq_read_buffer:                  dw
  cxd_test_transfer_length:             dw
  cxd_irq_mode:                         db
  cxd_register_buffer_write:            dsb CXD_WRITE_BUFFER_LENGTH
  cxd_register_buffer_read:             dsb CXD_READ_BUFFER_LENGTH
.ENDE
.ENUM $02A2
  cxd_tmp_bg_attributes:                db
.ENDE

; Some areas reused for comms test.
.DEFINE COMMS_DIRECT                    $0200
.ENUM COMMS_DIRECT
  comms_command_cursor:                 db
  comms_joypad_l:                       db
  comms_joypad_h:                       db
  comms_joypad_l_raw:                   db
  comms_joypad_h_raw:                   db
  comms_joypad_delay_l:                 db
  comms_joypad_delay_h:                 db
  comms_param_dirty_flag:               db
.ENDE
.ENUM $020A
  comms_command_pending_flag:           db
  comms_last_mechacon_nibble:           dw
  comms_command_position:               dw
  comms_response_position:              dw
.ENDE
.ENUM $0212
  comms_param_cursor:                   db
  comms_param_data_pointer:             dw
.ENDE
.ENUM $022A
  comms_param_buffer_ttii:              dsb 4
  comms_param_buffer_mmssff:            dsb 6
.ENDE
.ENUM $0292
  comms_tile_attributes:                dw
  comms_top_tile_address:               dw
  comms_bottom_tile_address:            dw
.ENDE

.DEFINE comms_param_buffer              comms_param_buffer_ttii
.DEFINE comms_command_buffer            $0800
.DEFINE comms_command_params_ttii       comms_command_buffer + $0C
.DEFINE comms_command_params_mmssff     comms_command_buffer + $2C

.DEFINE comms_serve_buffer_struct       $0A80
.DEFINE comms_receive_buffer_struct     $0AA6


; Some areas reused again for ADPCM test.
.DEFINE ADPCM_DIRECT                    $0200
.ENUM ADPCM_DIRECT
  adpcm_joypad_l:                       db
  adpcm_joypad_h:                       db
  adpcm_joypad_l_raw:                   db
  adpcm_joypad_h_raw:                   db
  adpcm_joypad_delay_l:                 db
  adpcm_joypad_delay_h:                 db
  adpcm_file:                           db
  adpcm_channel:                        db
  adpcm_minute:                         db
  adpcm_second:                         db
  adpcm_frame:                          db
  adpcm_emphasis:                       db
  adpcm_mono_stereo:                    db
  adpcm_level:                          db
  adpcm_ci:                             db
  adpcm_cursor:                         db
  adpcm_command_toggle:                 db
  adpcm_hdr_min:                        db
  adpcm_hdr_sec:                        db
  adpcm_hdr_block:                      db
  adpcm_hdr_mode:                       db
  adpcm_shdr_file:                      db
  adpcm_shdr_ch:                        db
  adpcm_shdr_s_mode:                    db
  adpcm_shdr_ci:                        db
  adpcm_sts:                            db
  adpcm_hdrflg:                         db
.ENDE

.DEFINE adpcm_params                    adpcm_file
.DEFINE adpcm_hdr_mmssff                adpcm_hdr_min

.ENUM $021E
  adpcm_mechacon_command_timer:         db
  adpcm_mechacon_command_retry:         db
  adpcm_mechacon_command_number:        db
  adpcm_unused:                         db
  adpcm_mechacon_last_response_nibble:  db
  adpcm_mechacon_command_status:        db
  adpcm_mechacon_command_position:      dw
  adpcm_comamnd_paramter_buffer:        db  ; size??
.ENDE
.ENUM $022A
  adpcm_param_buffer_mmssff:            db  ; size??
.ENDE
.ENUM $0230
  adpcm_mechacon_response_buffer:       db  ; size??
.ENDE
.ENUM $02F2
  adpcm_tile_attributes:                dw
  adpcm_top_tile_address:               dw
  adpcm_bottom_tile_address:            dw
.ENDE

;
; Constants/variables for main program
;
.DEFINE SUPERCD_ERROR_NO_DISC                 $01
.DEFINE SUPERCD_ERROR_NOT_DATA_TRACK          $02
.DEFINE SUPERCD_ERROR_BAD_RESPONSE            $03
.DEFINE SUPERCD_ERROR_BAD_SUBQ                $04
.DEFINE SUPERCD_ERROR_MECHACON_TIMEOUT        $05
.DEFINE SUPERCD_ERROR_MECHACON_MISMATCH       $06
.DEFINE SUPERCD_ERROR_MECHACON_TIMEOUT_ADPCM  $0A

; Values for `supercd_mechacon_command_status`
.DEFINE SUPERCD_MECHACON_COMMAND_BUSY                 $80  ; Command initiated/busy.
.DEFINE SUPERCD_MECHACON_COMMAND_WAIT_RESPONSE        $20  ; Nibble sent, awaiting response from Mechacon.
.DEFINE SUPERCD_MECHACON_COMMAND_COMPLETE_UNEXPECTED  $10 ; Command completed with unexpected response.
.DEFINE SUPERCD_MECHACON_COMMAND_UNEXPECTED           $08  ; Unexpected response nibble/mismatch.

.DEFINE SUPERCD_MECHACON_COMMAND_TIMEOUT      FPS/5
.DEFINE SUPERCD_MECHACON_COMMAND_RETRIES      3

.DEFINE SUPERCD_BLOCK_BUFFER_SIZE             32
.DEFINE SUPERCD_BLOCK_BUFFER_STEP             4


.ENUM $0600
  ram_menu_text_struct_position:        dw
  ram_menu_text_struct_attirbutes:      dw
  ram_menu_text_struct_length:          db
  ram_menu_text_struct_string:          dsb 20
  ram_menu_unused_word_1:               dw
  ram_menu_number_of_files:             dw
.ENDE
.ENUM $061F
  ram_menu_file_to_delete:              dw
  ram_menu_scroll_offset:               dw
  ram_menu_unused_word_2:               dw
  ram_menu_selected_file:               db
  ram_menu_draw_file_number:            dw
  ram_menu_joypad_delay_up              db
  ram_menu_joypad_delay_down            db
  ram_menu_b_button_lock                db
.ENDE

.ENUM $1F00 ; When used for source MMSSFF
  supercd_src_mm:                       db
  supercd_src_ss:                       db
  supercd_src_ff:                       db
.ENDE
.ENUM $1F00 ; When used for source 24-bit LBA
  supercd_src_l:                        db
  supercd_src_m:                        db
  supercd_src_h:                        db
  supercd_read_mode:                    db
  supercd_dest_addr_l:                  db
  supercd_dest_addr_h:                  db
  supercd_dest_addr_b:                  db
  supercd_transfer_length:              dw
  supercd_max_subq_errors:              db
  supercd_mechacon_error_code:          db
.ENDE

; Mechacon command parameter/response buffers.
.ENUM $1F0F
  supercd_track_h:                      db  ; Command parameter $80 for CxxxxF
  supercd_track_l:                      db
  supercd_index_h:                      db
  supercd_index_l:                      db

  supercd_minutes_tens_bcd:             db  ; Command parameter $84 for BxxxxxxF
  supercd_minutes_units_bcd:            db
  supercd_seconds_tens_bcd:             db
  supercd_seconds_units_bcd:            db
  supercd_frames_tens_bcd:              db
  supercd_frames_units_bcd:             db

  supercd_disc_track_type:              db  ; Response $80 for D51FxxxxxF
  supercd_mechacon_unknown_1:           db
  supercd_drive_status:                 db
  supercd_mechacon_unknown_3:           db
  supercd_mechacon_unknown_4:           db

  supercd_subq_00:                      db  ; Response $85 for D50FxxxxxxxxxxxxxxxxF
  supercd_subq_01:                      db
  supercd_subq_02:                      db
  supercd_subq_03:                      db
  supercd_subq_04:                      db
  supercd_subq_05:                      db
  supercd_subq_track_tens_bcd:          db
  supercd_subq_track_units_bcd:         db
  supercd_subq_index_tens_bcd:          db
  supercd_subq_index_units_bcd:         db
  supercd_subq_minutes_tens_bcd:        db  ; Are these for ADPCM or regular??
  supercd_subq_minutes_units_bcd:       db
  supercd_subq_seconds_tens_bcd:        db
  supercd_subq_seconds_units_bcd:       db
  supercd_subq_frames_tens_bcd:         db
  supercd_subq_frames_units_bcd:        db

  supercd_mechacon_final_nibble:        db  ; Response $95

  supercd_joypad1_status_l:             db
  supercd_joypad1_status_h:             db
  supercd_joypad2_status_l:             db
  supercd_joypad2_status_h:             db
  supercd_adpcm_file_and_channel:       dw
.ENDE

; Start addresses of Mechacon of buffers in the above.
.DEFINE supercd_comamnd_paramter_buffer   supercd_track_h
.DEFINE supercd_mechacon_response_buffer  supercd_disc_track_type
.DEFINE supercd_mechacon_status_buffer    supercd_mechacon_response_buffer
.DEFINE supercd_mechacon_subq_buffer      supercd_subq_00

.ENUM $1F40
  supercd_saved_minutes_bcd:            db
  supercd_saved_seconds_bcd:            db
  supercd_saved_frames_bcd:             db
  supercd_read_buffer_l:                db
  supercd_read_buffer_h:                db
  supercd_read_buffer_b:                db
  supercd_vram_dma_byte_count:          dw
  supercd_vram_dma_length:              dw
  supercd_vram_dma_block_count:         dw
  supercd_not_sure                      db
  supercd_cxd_hdr_min:                  db
  supercd_cxd_hdr_sec:                  db
  supercd_cxd_hdr_block:                db
  supercd_cxd_hdr_mode:                 db
  supercd_cxd_subheader_file:           db
  supercd_cxd_subheader_channel:        db
  supercd_cxd_subheader_submode:        db
  supercd_cxd_subheader_coding:         db
  supercd_cxd_sts:                      db
  supercd_cxd_hdrflg:                   db
.ENDE
.ENUM $1F59
  supercd_cxd_buffer_start_l:           db
  supercd_cxd_buffer_start_h:           db
  supercd_xfer_flags:                   db
  supercd_block_id:                     db
  supercd_failed_seeks_limit:           db
  supercd_mechacon_command_timer:       db
  supercd_mechacon_command_retry:       db
  supercd_mechacon_command_number:      db
  supercd_mechacon_last_response_nibble: db
  supercd_mechacon_command_status:      db
  supercd_mechacon_command_position:    dw
  supercd_read_frame:                   db
  supercd_read_flag:                    db
  ; 32 byte buffer used in chunks of 4 bytes
  ;  0: id/count (+$80 for VRAM mode) from supercd_block_id
  ;  1: mm, from supercd_saved_minutes_bcd
  ;  2: ss, from supercd_saved_seconds_bcd
  ;  3: ff, from supercd_saved_frames_bcd
  supercd_block_buffer:                 dsb SUPERCD_BLOCK_BUFFER_SIZE
  supercd_block_buffer_index:           dw
  supercd_saved_stack_pointer:          dw
  supercd_lba_convert_l:                db
  supercd_lba_convert_m:                db
  supercd_lba_convert_h:                db
.ENDE
.ENUM $1FF0
  supercd_nmitimen:                     db
.ENDE
.ENUM $1FF8
  supercd_bios_nmi_vector_opcode:       db
  supercd_bios_nmi_vector_address:      dw
  supercd_bios_nmi_vector_bank:         db

  supercd_bios_irq_vector_opcode:       db
  supercd_bios_irq_vector_address:      dw
  supercd_bios_irq_vector_bank:         db
.ENDE

.DEFINE supercd_bios_vector_table       supercd_bios_nmi_vector_opcode
.DEFINE VECTOR_TABLE_SIZE               8
.DEFINE VECTOR_TABLE_OPCODE             $5C ; JMP/JML
