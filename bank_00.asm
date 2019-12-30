;
; Reset vector points here, to the main program. This bank contains the title
; screen, all the BIOS functions, the Backup RAM menu and the Electronic Book
; implementation.
;
                 reset: BRA start

            title_text: .DB "Super Disc boot ROM ver.0.95 Jul. 14, 1992 by Tomomi Abe at SONY "

                 start: SEI                             ; Disable interrupts
                        STZ.W NMITIMEN                  ; Disable PPU interrupts
                        CLC
                        XCE
                        REP   #$10
                        SEP   #$20
                        LDX.W #$01FF                    ; Init stack
                        TXS
                        LDA.B #$00                      ; Bank 0
                        PHA
                        PLB

                        LDA.B #INIDISP_BLANK            ; Blank the screen
                        STA.W INIDISP

                        LDA.B #NMITIMEN_JOYPAD          ; Enable joypad interrupts
                        STA.W supercd_nmitimen          ; Shadow variable
                        STA.W NMITIMEN

                        STZ.W MDMAEN                    ; Disable MDMA
                        STZ.W HDMAEN                    ; Disable HDMA

                        ; Basic initialisation.
                        JSR.W sub_init_vectors
                        JSR.W sub_init_ppu_and_on_chip
                        JSR.W load_title_screen_tiles

                        ; These two bytes don't seem to be used for anything
                        ; particular, nor do they seem to have much to do with
                        ; the CD code buffer, as their location would suggest.
                        STZ.W CD_CODE_BUFFER_ADDRESS + 2
                        STZ.W CD_CODE_BUFFER_ADDRESS + 3

                        ; 1 second delay.
                        LDX.W #0
    title_delay_frames: LDA.W HVBJOY                    ; Wait for vblank
                        BPL   title_delay_frames
   title_delay_frames2: LDA.W HVBJOY
                        BMI   title_delay_frames2
                        INX
                        CPX.W #FPS
                        BNE   title_delay_frames

                        ; Screen on, draw the Super Disc logo.
                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        JSR.W draw_main_logo_animated

                        ; Not used any time soon, but the title screen, disc
                        ; loader and Electronic Book implementation are just
                        ; one giant long routine.
                        JSR.W ebook_clear_vars

                        ; Quick check for CD-ROM hardware.
                        JSL.L bios_InitDetect
                        BCC   cdrom_drive_found

                        ; No (or no functional) CD system, but allow access to
                        ; diagnostic menu.
                        LDX.W #title_status_no_cd_system
                        JSR.W sub_render_text
                        LDA.B #(NMITIMEN_VBLANK|NMITIMEN_JOYPAD)
                        STA.W NMITIMEN

      title_no_cd_loop: LDA.W supercd_joypad1_status_l
                        AND.B #(JOYPAD_L_A | JOYPAD_L_X)        ; A+X pressed
                        CMP.B #(JOYPAD_L_A | JOYPAD_L_X)
                        BNE   title_no_cd_loop
                        JML.L diagnostic_menu

                        ; CD-ROM system found, check status and disc type.
     cdrom_drive_found: LDA.B #MECHACON_REQ_STATUS
                        JSR.W mechacon_command_wrapper
                        LDA.W supercd_disc_track_type
                        AND.B #MECHACON_DISC_TRACK_DATA
                        BEQ   cdrom_init_sram_test
                        LDA.B #MECHACON_STOP
                        JSR.W mechacon_command_wrapper

                        ; SRAM test, will also initialise if necessary.
  cdrom_init_sram_test: JSL.L bios_SramTest
                        LDA.B #(NMITIMEN_VBLANK|NMITIMEN_JOYPAD)
                        STA.W supercd_nmitimen
                        STA.W NMITIMEN

                        ; Main loop, keeps checking and reacting to the CD-ROM
                        ; drive status as well as handling joypad controls.
     title_joypad_loop:
    title_check_select: LDA.W supercd_joypad1_status_h  ; Get joypad status
                        AND.B #JOYPAD_H_SELECT          ; Select button pressed
                        BEQ   title_check_ax
                        JSR.W ram_file_menu             ; Go to Backup RAM menu

                        ; RAM menu exited, go back to title screen
                        LDA.B #INIDISP_BLANK            ; Blank screen
                        STA.W INIDISP
                        JSR.W load_title_screen_tiles   ; Restore tiles
                        JSR.W draw_main_logo_fast       ; Redraw logo
                        LDA.B #INIDISP_NO_BLANK         ; Screen back on
                        STA.W INIDISP
                        BRA   title_joypad_loop

                        ; A+X pressed? Jump to diagnostic menu (bank $01)
        title_check_ax: LDA.W supercd_joypad1_status_l
                        AND.B #(JOYPAD_L_A | JOYPAD_L_X)
                        CMP.B #(JOYPAD_L_A | JOYPAD_L_X)
                        BNE   title_ax_not_pressed
                        JML.L diagnostic_menu

                        ; Check Mechacon and signal any errors.
  title_ax_not_pressed: LDA.B #MECHACON_REQ_STATUS
                        JSR.W mechacon_command_wrapper
                        BCC   title_mechacon_ok
                        ; Mechacon error
                        LDX.W #title_status_mcom_error
                        JSR.W sub_render_text
                        JMP.W title_no_cd_loop

                        ; Is there a disc inserted?
     title_mechacon_ok: LDA.W supercd_drive_status
                        BNE   title_not_no_disc
                        LDX.W #title_status_no_disc
                        JSR.W sub_render_text
                        BRA   title_joypad_loop

                        ; Is the tray open?
     title_not_no_disc: CMP.B #MECHACON_STATE_TRAY_OPEN
                        BNE   title_tray_not_open
                        LDX.W #title_status_tray_open
                        JSR.W sub_render_text
                        BRA   title_joypad_loop

                        ; Is the Mechacon accessing the disc?
   title_tray_not_open: CMP.B #MECHACON_STATE_ACCESS_SEEK
                        BEQ   title_access_disc
                        CMP.B #MECHACON_STATE_ACCESS_TOC
                        BNE   title_skip_access
     title_access_disc: LDX.W #title_status_access
                        JSR.W sub_render_text
                        JMP.W title_joypad_loop

                        ; Not accessing, check the disc/track type...
                        ; Audio CD?
     title_skip_access: LDA.W supercd_disc_track_type
                        AND.B #MECHACON_DISC_TRACK_DATA
                        BNE   title_not_music_disc
                        LDX.W #title_status_music_disc
                        JSR.W sub_render_text
                        JMP.W title_joypad_loop

                        ; Check Mechacon status again
  title_not_music_disc: LDA.W supercd_drive_status
                        CMP.B #MECHACON_STATE_PAUSE
                        BEQ   title_drive_idle
                        CMP.B #MECHACON_STATE_PLAY
                        BNE   title_drive_playing
                        ; Stop the drive
                        LDA.B #MECHACON_STOP
                        JSR.W mechacon_command_wrapper
                        JMP.W title_joypad_loop

   title_drive_playing: CMP.B #MECHACON_STATE_STOP
                        BEQ   title_drive_idle
                        ; Any other state, give up.
                        LDX.W #title_status_not_defined
                        JSR.W sub_render_text
                        JMP.W title_no_cd_loop

                        ; Disc present and drive stopped. Display "PUSH START"
                        ; and wait for it.
      title_drive_idle: LDX.W #title_status_push_start
                        JSR.W sub_render_text
                        LDA.W supercd_joypad1_status_h
                        AND.B #JOYPAD_H_START
                        BNE   title_load_disc
                        JMP.W title_joypad_loop

                        ; Try to load the disc...
       title_load_disc: LDX.W #title_status_now_loading
                        JSR.W sub_render_text

                        JSL.L bios_DecoderDataMode

                        ; Check for a disc header at LBA 000100
                        LDX.W #$0010
                        STX.W supercd_src_l
                        LDA.B #$00
                        STA.W supercd_src_h
                        LDA.B #SUPERCD_READ_MODE_LBA
                        STA.W supercd_read_mode

                        LDX.W #CD_HEADER_BUFFER         ; Set destination
                        STX.W supercd_dest_addr_l
                        STZ.W supercd_dest_addr_b       ; Bank 0

                        LDX.W #CD_SECTOR_SIZE
                        STX.W supercd_transfer_length

                        LDA.B #CD_MAX_SUBQ_ERRORS
                        STA.W supercd_max_subq_errors
                        JSL.L bios_LoadFromDisc

                        BCS   title_disc_error          ; C=1 indicates error

                        ; Sector loaded OK. Does it contain a Super Disc header?
                        JSR.W super_disc_check_hdr
                        BMI   title_not_super_disc
                        BNE   title_load_ebook

                        ; Disc is Super Disc format.
                        ; Load the boot sector at LBA 000000
                        LDX.W #$0000
                        STX.W supercd_src_l
                        LDA.B #$00
                        STA.W supercd_src_h
                        LDA.B #SUPERCD_READ_MODE_LBA
                        STA.W supercd_read_mode

                        LDX.W #CD_CODE_BUFFER_ADDRESS
                        STX.W supercd_dest_addr_l
                        LDA.B #CD_CODE_BUFFER_BANK
                        STA.W supercd_dest_addr_b

                        LDX.W #CD_SECTOR_SIZE
                        STX.W supercd_transfer_length

                        LDA.B #CD_MAX_SUBQ_ERRORS
                        STA.W supercd_max_subq_errors
                        JSL.L bios_LoadFromDisc

                        BCS   title_disc_error

                        ; Boot code read OK.
                        REP   #$20
                        LDA.W #0
                        TCD
                        SEP   #$20
                        ; Jump to loaded code!
                        JML.L CD_CODE_BUFFER_ADDRESS + $80

                        ; Various errors jump to these labels and drop through
                        ; to have the message renderd to screen.
      title_disc_error: LDX.W #title_status_disc_error
                        BRA   title_render_error

  title_not_super_disc: LDX.W #title_status_not_super_disc
                        BRA   title_render_error

       title_not_ebook: LDX.W #title_status_not_eb

    title_render_error: JSR.W sub_render_text           ; Display message

                        LDA.B #MECHACON_OPEN_CLOSE      ; Eject the disc
                        JSR.W mechacon_command_wrapper

                        ; Switch to audio mode briefly. After the following
                        ; delay, returning to the main joypad loop will switch
                        ; back to data mode. This might reset the controller in
                        ; some way, so the next disc inserted can be detected
                        ; and/or read properly.
                        JSL.L bios_DecoderAudioMode
                        PHY
                        PHX
                        LDY.W #$2000
     title_error_delay: LDX.W #$00FF
    title_error_delay2: DEX
                        BNE   title_error_delay2
                        DEY
                        BNE   title_error_delay

                        PLX
                        PLY
                        JMP.W title_joypad_loop

;
; Electronic Book code follows...
;
; This is a limited implementation of Sony's Data Discman standard which came
; out around the time the original BIOS code was written. Information on the
; format is thin on the ground, so terminology used here may not be accurate.
;
; There are some issues with missing characters (e.g. Kanji) due to references
; beyond the end of the ROM. Either the dump was incomplete or the original ROM
; was too small for everything to fit. Bank $03 does however contain JIS X 0208
; character sets (excluding Kanji, but including e.g. Greek and Cyrillic).
;
; These issues may explain why this functionality was effectively disabled in
; the original code by disabling the detection of the Electronic Book header.
;
      title_load_ebook: LDX.W #title_status_electronic_book
                        JSR.W sub_render_text

            ebook_main: ; The LBA of the Electronic Book table of contents is
                        ; at offsets $9E-$A0 in the block read previously.
                        LDX.W CD_HEADER_BUFFER + $9E
                        STX.W supercd_src_l
                        LDA.W CD_HEADER_BUFFER + $A0
                        STA.W supercd_src_h
                        LDA.B #SUPERCD_READ_MODE_LBA
                        STA.W supercd_read_mode
                        LDX.W #EBOOK_READ_BUFFER_ADDRESS
                        STX.W supercd_dest_addr_l
                        LDA.B #EBOOK_READ_BUFFER_BANK
                        STA.W supercd_dest_addr_b
                        LDX.W #CD_SECTOR_SIZE
                        STX.W supercd_transfer_length
                        LDA.B #CD_MAX_SUBQ_ERRORS
                        STA.W supercd_max_subq_errors
                        JSL.L bios_LoadFromDisc

                        ; Failed, declare it's not an Electronic Book disc.
                        BCS   title_not_ebook

                        ; Otherwsie, search for "CATALOG" tag in TOC, which
                        ; contans the items for the main menu.
                        LDY.W #data_ebook_catalog
                        JSR.W ebook_sub_load_tagged
                        BCS   title_not_ebook

                        ; Catalog read OK, initialise and set up for main menu.
                        JSR.W ebook_init_vectors
                        JSR.W ebook_init_graphics
                        ; This "cursor" is not something the user can see but
                        ; just how the code tracks where it's writing on the
                        ; screen.
                        LDA.B #$01
                        STA.W ebook_cursor_row
                        LDA.B #$00
                        STA.W ebook_cursor_column

                        ; Write "contents" header to screen.
                        LDX.W #data_ebook_text_this_ebook_contains
                        JSR.W ebook_render_text

                        ; Draw the catalog/main menu for the current disc.
                        PHB
                        LDA.B #EBOOK_READ_BUFFER_BANK
                        PHA
                        PLB
                        LDA.B #$02                                      ; Leave space for header.
                        STA.W ebook_cursor_row
                        LDA.L EBOOK_CATALOG_BUFFER_ADDRESS_L + 1        ; Number of items
                        LDX.W #EBOOK_CATALOG_BUFFER_ADDRESS + $12       ; Start of text, first item.
ebook_menu_render_item: PHA
                        LDA.B #$02                                      ; Leave space for arrow.
                        STA.W ebook_cursor_column
                        PHX
                        JSR.W ebook_render_text
                        PLX
                        REP   #$20
                        TXA
                        CLC
                        ADC.W #EBOOK_CATALOG_RECORD_SIZE                ; Next item
                        TAX
                        SEP   #$20
                        PLA
                        INC.W ebook_cursor_row
                        DEC   A
                        BNE   ebook_menu_render_item

                        PLB
                        LDA.B #$02                                      ; Leave space for title.
                        STA.W ebook_cursor_row

                        ; The menu is a simple list with an arrow controlled by
                        ; the user with up/down. Any changes in selection will
                        ; jump back here.
     ebook_menu_update: PHP
                        SEP   #$20

ebook_menu_wait_vblank: LDA.W RDNMI
                        BPL   ebook_menu_wait_vblank
                        LDA.W RDNMI
                        PLP

                        ; Draw the arrow (item selection pointer)
                        LDA.B #$00
                        STA.W ebook_cursor_column
                        LDX.W #data_ebook_arrow
                        JSR.W ebook_render_text

                        ; Check the Mechacon status. If the drive is ever not
                        ; paused, jump all the was back to the title screen.
                        ; Seems harsh, but maybe this indicates some kind of
                        ; error state.
ebook_menu_joypad_loop: LDA.B #MECHACON_REQ_STATUS
                        JSR.W mechacon_command_wrapper
                        LDA.W supercd_drive_status
                        CMP.B #MECHACON_STATE_PAUSE
                        BEQ   ebook_check_joypad
                        JMP.W reset

    ebook_check_joypad: JSR.W ebook_wait_for_joypad
                        LDA.W supercd_joypad1_status_h
                        BMI   ebook_b_pressed           ; $80 == B button

                        ; No B, up or down? Loop back.
                        AND.B #(JOYPAD_H_UP | JOYPAD_H_DOWN)
                        BEQ   ebook_menu_joypad_loop

                        ; Check for up/down.
                        LDA.W supercd_joypad1_status_h
                        AND.B #(JOYPAD_H_UP | JOYPAD_H_DOWN)
                        CMP.B #JOYPAD_H_UP
                        BNE   ebook_down_pressed

                        ; Up pressed
                        LDA.W ebook_cursor_row
                        CMP.B #$03                      ; Upper limit
                        BMI   ebook_menu_joypad_loop

                        ; Overwrite the arrow with a space.
                        LDA.B #$00
                        STA.W ebook_cursor_column
                        LDX.W #data_ebook_space
                        JSR.W ebook_render_text

                        ; Up one row
                        LDA.W ebook_cursor_row
                        DEC   A
                        STA.W ebook_cursor_row
                        BRA   ebook_menu_update

    ebook_down_pressed: LDA.L EBOOK_CATALOG_BUFFER_ADDRESS_L + 1
                        INC   A
                        CMP.W ebook_cursor_row  ; Limit (last item)
                        BEQ   ebook_menu_joypad_loop

                        ; Overwrite the arrow with a space.
                        LDA.B #$00
                        STA.W ebook_cursor_column
                        LDX.W #data_ebook_space
                        JSR.W ebook_render_text

                        ; Down one row.
                        LDA.W ebook_cursor_row
                        INC   A
                        STA.W ebook_cursor_row
                        BRA   ebook_menu_update

                        ; Pressing B loads selected item.
       ebook_b_pressed: LDA.W ebook_cursor_row
                        DEC A                   ; Compensate for header.
                        DEC A
                        ASL A                   ; x2 to use as word pointer
                        STA.W ebook_tmp

                        STZ.W ebook_group_dest_l
                        JSR.W ebook_clear_vram_buffers

                        ; Display "searching" text.
                        LDA.B #$01
                        STA.W ebook_cursor_row
                        LDA.B #$00
                        STA.W ebook_cursor_column
                        LDX.W #data_ebook_text_searching
                        JSR.W ebook_render_text

                        ; Get offset into CATALOG block where we should find
                        ; the GROUP record.
                        REP   #$20
                        LDX.W ebook_tmp         ; Item offset
                        LDA.W data_ebook_group_record_offsets,X
                        TAX

                        ; Copy group name.
                        SEP   #$20
                        LDY.W #$0000
 ebook_loop_group_name: LDA.L EBOOK_CATALOG_BUFFER_ADDRESS_L + $30,X
                        STA.W ebook_group_name,Y
                        INX
                        INY
                        CPY.W #EBOOK_GROUP_NAME_LENGTH
                        BNE   ebook_loop_group_name

                        ; Ensure string is $00 terminated
                        LDA.B #$00
                        STA.W ebook_group_name,Y

                        ; Use the resulting "tag" data structure to load data
                        ; for the selected group.
                        LDY.W #ebook_group_dest_l
                        JSR.W ebook_load_indirect
                        BCC   ebook_check_start
                        JMP.W ebook_joypad_loop@skip_failed

                        ; Find the START block/record for selected group and
                        ; load the data it points to.
     ebook_check_start: LDY.W #data_ebook_start
                        JSR.W ebook_sub_load_tagged
                        BCC   ebook_found_start
                        JMP.W ebook_joypad_loop@skip_failed

                        ; Identifiers for "tagged" blocks. Each is saved to its
                        ; own buffer if found (see first 3 bytes).
      data_ebook_start: .DW EBOOK_START_BUFFER_ADDRESS
                        .DB EBOOK_READ_BUFFER_BANK
                        .DB "START", $00

    data_ebook_catalog: .DW EBOOK_CATALOG_BUFFER_ADDRESS
                        .DB EBOOK_READ_BUFFER_BANK
                        .DB "CATALOG", $00

                        ; START block followed, we should now have the table
                        ; of contents for the selected group/book.
     ebook_found_start: LDX.W #CD_SECTOR_SIZE * 2
                        STX.W ebook_transfer_length
                        LDY.W #data_ebook_page_index_record
                        JSR.W ebook_load_blocks
                        BCS   ebook_done_with_pages

                        ; Found page index. Read through the records.
                        LDA.B #EBOOK_READ_BUFFER_BANK
                        PHA
                        PLB
                        LDX.W #0
                        LDY.W #0
 ebook_loop_page_index: SEP   #$20
                        LDA.L EBOOK_TEMP_BUFFER_ADDRESS_L + CD_SECTOR_SIZE,X
                        INX
                        STA.W EBOOK_READ_BUFFER_ADDRESS + (CD_SECTOR_SIZE * 2),Y

                        LDA.L EBOOK_TEMP_BUFFER_ADDRESS_L + CD_SECTOR_SIZE,X
                        STA.W EBOOK_READ_BUFFER_ADDRESS + (CD_SECTOR_SIZE * 2) + $10,Y
                        INX

                        INY
                        REP   #$20
                        TYA
                        AND.W #$000F    ; Only interested in first 16 bytes.
                        BNE   ebook_loop_page_index

                        TYA
                        CLC
                        ADC.W #16       ; Skip 16 bytes, brings us to next "$1F" page index record.
                        TAY
                        CPX.W #CD_SECTOR_SIZE
                        BMI   ebook_loop_page_index

                        SEP   #$20
                        PHK
                        PLB
                        BRA   ebook_done_with_pages

data_ebook_page_index_record:
                        .DW EBOOK_TEMP_BUFFER_ADDRESS
                        .DB EBOOK_READ_BUFFER_BANK
                        .DB $F1,$00

                        ; Look for any "$F2" records, indicating custom
                        ; character data to read from disc.
 ebook_done_with_pages: LDX.W #CD_SECTOR_SIZE * 2
                        STX.W ebook_transfer_length
                        LDY.W #data_ebook_char_data_record
                        JSR.W ebook_load_blocks
                        BCS   ebook_done_with_char_data

                        ; Copy the whole of the second block, if found.
                        LDX.W #0
ebook_loop_char_data_copy:
                        LDA.L EBOOK_TEMP_BUFFER_ADDRESS_L + CD_SECTOR_SIZE,X
                        STA.L EBOOK_CHAR_BUFFER_ADDRESS_L,X
                        INX
                        CPX.W #CD_SECTOR_SIZE
                        BNE   ebook_loop_char_data_copy
                        BRA   ebook_done_with_char_data

data_ebook_char_data_record:
                        .DW EBOOK_TEMP_BUFFER_ADDRESS
                        .DB EBOOK_READ_BUFFER_BANK
                        .DB $F2,$00

                        ; Find a "$00" record and load 32K from where it points.
                        ; This is the first of the book content itself.
ebook_done_with_char_data:
                        LDX.W #CD_SECTOR_SIZE * 16
                        STX.W ebook_transfer_length
                        LDY.W #data_ebook_zero_record
                        JSR.W ebook_load_blocks
                        BCC   ebook_group_loaded
                        JMP.W ebook_joypad_loop@skip_failed

data_ebook_zero_record: .DW EBOOK_PAGE_BUFFER_ADDRESS
                        .DB EBOOK_READ_BUFFER_BANK
                        .DB $00,$00


    ebook_group_loaded: JSR.W ebook_clear_group_vars
                        ;
                        ; Main loop for browsing loaded group/book.
                        ;
     ebook_joypad_loop: JSR.W ebook_update_screen_content

           @next_frame: PHP
                        SEP   #$20

          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI
                        PLP

       @check_mechacon: LDA.B #MECHACON_REQ_STATUS
                        JSR.W mechacon_command_wrapper
                        LDA.W supercd_drive_status
                        CMP.B #MECHACON_STATE_PAUSE
                        BEQ   @mechacon_paused
                        JMP.W reset

      @mechacon_paused: JSR.W ebook_wait_for_joypad

                        LDA.W supercd_joypad1_status_h
                        AND.B #JOYPAD_H_SELECT
                        BNE   @select_pressed

                        LDA.W supercd_joypad1_status_h
                        AND.B #(JOYPAD_H_UP | JOYPAD_H_DOWN)
                        BEQ   @check_mechacon

                        LDA.W supercd_joypad1_status_h
                        AND.B #(JOYPAD_H_UP | JOYPAD_H_DOWN)
                        CMP.B #JOYPAD_H_UP
                        BNE   @down_pressed

                        ; Up pressed.
                        LDA.W ebook_scroll_h
                        BEQ   @next_frame               ; Can't go < 0
                        SEC
                        SBC.B #EBOOK_SCROLL_STEP        ; Scroll up
                        STA.W ebook_scroll_h
                        BRA   @next_frame

         @down_pressed: LDA.W ebook_scroll_h
                        CLC
                        ADC.B #EBOOK_SCROLL_STEP

                        ; Particular scroll positions will require a refresh
                        ; of the screen contents.
                        CMP.B #76
                        BEQ   @trigger_refresh
                        CMP.B #204
                        BEQ   @trigger_refresh

                        ; Not special, no refresh required. Just loop as usual.
                        STA.W ebook_scroll_h
                        BRA   @next_frame

      @trigger_refresh: STA.W ebook_scroll_h
                        ; Branching back here instead of @next_screen means the
                        ; screen contents will get updated.
                        BRA   ebook_joypad_loop

                        ; Select pressed, reset/back to top.
       @select_pressed: JSR.W ebook_clear_vram_buffers
                        LDA.B #$01
                        STA.W ebook_cursor_row
                        LDA.B #$00
                        STA.W ebook_cursor_column
                        STZ.W ebook_scroll_h
                        LDX.W #data_ebook_text_searching
                        JSR.W ebook_render_text
                        JMP.W ebook_main

          @skip_failed: JSR.W ebook_clear_vram_buffers
                        LDA.B #$01
                        STA.B ebook_cursor_row
                        LDA.B #$00
                        STA.B ebook_cursor_column
                        STZ.W ebook_scroll_h
                        LDX.W #data_ebook_text_function_not_available
                        JSR.W ebook_render_text
                        PHY
                        PHX

                        ; Delay to show message before resetting.
                        LDY.W #$0400
          @delay_outer: LDX.W #$00FF
          @delay_inner: DEX
                        BNE   @delay_inner
                        DEY
                        BNE   @delay_outer
                        PLX
                        PLY
                        JMP.W ebook_main


                        ; "This function is not available"
data_ebook_text_function_not_available:
                        .DB $24,$33,$24,$4E,$35,$21,$47,$3D
                        .DB $24,$4F,$3B,$48,$24,$28,$24,$5E
                        .DB $24,$3B,$24,$73,$00,$00

                        ; Offsets in CATALOG at which group data is to be
                        ; found (Simply group number * $28 bytes).
data_ebook_group_record_offsets:
                        .DW  0 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  1 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  2 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  3 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  4 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  5 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  6 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  7 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  8 * EBOOK_CATALOG_RECORD_SIZE
                        .DW  9 * EBOOK_CATALOG_RECORD_SIZE
                        .DW 10 * EBOOK_CATALOG_RECORD_SIZE
                        .DW 11 * EBOOK_CATALOG_RECORD_SIZE

                        ;
                        ; Electronic Book strings, not yet internationalised...
                        ;

                        ; "This Electronic Book contains"
data_ebook_text_this_ebook_contains:
                        .DB $24,$33,$24,$4E,$45,$45,$3B,$52
                        .DB $25,$56,$25,$43,$25,$2F,$24,$4F
                        .DB $00,$00

                        ; "Now searching"
data_ebook_text_searching:
                        .DB $24,$3F,$24,$40,$24,$24,$24,$5E
                        .DB $38,$21,$3A,$77,$43,$66,$00,$00

      data_ebook_arrow: .DB $22,$2A,$00,$00     ; -> (arrow)

      data_ebook_space: .DB $21,$21,$00,$00     ; IDSP (space)

                        ; Wait for an event to appear on the joypad status
                        ; queue, then return it.
 ebook_wait_for_joypad: JSR.W ebook_process_joypad
                        BCS   ebook_wait_for_joypad
                        STX.W supercd_joypad1_status_l
                        RTS

                        ; Unreferenced/unused?
       data_ebook_84fa: .DB $01,$00


; Search the loaded block for a record with a specific "tag". If found, extract
; the LBA and data length in that record and load this into memory.
; Data structure passed in Y.
 ebook_sub_load_tagged: JSR.W ebook_search_tag
                        BCS   @not_found

                        ; X now points to standard record with requested tag.
                        ; LBA at offset 2-5 (MSB unused)
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + 2,X
                        STA.W supercd_src_l
                        STA.W ebook_src_l
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + 3,X
                        STA.W supercd_src_m
                        STA.W ebook_src_m
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + 4,X
                        STA.W supercd_src_h
                        STA.W ebook_src_h

                        REP   #$20
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + $0C,X       ; Length (MSW)
                        BEQ   @set_length       ; Length < $10000

                        LDA.W #$8000            ; Round up to 32K
                        BRA   @save_length

           @set_length: LDA.L EBOOK_READ_BUFFER_ADDRESS_L + $0A,X       ; Length (LSW)

          @save_length: STA.W ebook_transfer_length
                        SEP   #$20
                        BRA   ebook_load_blocks@load_block

            @not_found: ; If jumped here due to error, carry will still be set.
                        RTS


                        ; Y points to:
                        ;  0-2: destination address (ll, hh, bb)
                        ;  3-4: search term (word)
     ebook_load_blocks: REP   #$20
                        LDX.W #0
                 @loop: TXA
                        CMP.L EBOOK_START_BUFFER_ADDRESS_L
                        BEQ   @exit_error
                        LDA.L EBOOK_START_BUFFER_ADDRESS_L + $10,X
                        CMP.W $0003,Y           ; Found our byte?
                        BEQ   @match
                        TXA
                        CLC
                        ADC.W #16               ; Next record
                        TAX
                        BRA   @loop

           @exit_error: SEP   #$20
                        SEC
                        RTS

                        ; At offset $14 is... an offset. This is the number of
                        ; blocks to skip to reach the corresponding page.
                @match: LDA.L EBOOK_START_BUFFER_ADDRESS_L + $14,X
                        XBA
                        DEC   A                 ; Compensate for "header" block

                        ; Add the offset to current LBA.
                        SEP   #$20
                        CLC
                        ADC.W ebook_src_l
                        STA.W supercd_src_l
                        XBA
                        ADC.W ebook_src_m
                        STA.W supercd_src_m
                        LDA.B #$00              ; Add just the carry.
                        ADC.W ebook_src_h
                        STA.W supercd_src_h

                        ; `ebook_sub_load_tagged` jumps here to load data.
           @load_block: LDA.B #SUPERCD_READ_MODE_LBA
                        STA.W supercd_read_mode
                        LDA.W $0000,Y
                        STA.W supercd_dest_addr_l
                        LDA.W $0001,Y
                        STA.W supercd_dest_addr_h
                        LDA.W $0002,Y
                        STA.W supercd_dest_addr_b
                        LDX.W ebook_transfer_length
                        STX.W supercd_transfer_length
                        LDA.B #CD_MAX_SUBQ_ERRORS
                        STA.W supercd_max_subq_errors
                        JSL.L bios_LoadFromDisc
                        RTS

                        ; Called with a GROUP "tag" data structure with fake
                        ; destination info (reads go to the regular buffer).
   ebook_load_indirect: PHX
                        PHY
                        JSR.W ebook_search_tag
                        BCS   @exit
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + 2,X
                        STA.W supercd_src_l
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + 3,X
                        STA.W supercd_src_m
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + 4,X
                        STA.W supercd_src_h
                        LDA.B #SUPERCD_READ_MODE_LBA
                        STA.W supercd_read_mode
                        LDA.B #EBOOK_READ_BUFFER_BANK
                        STA.W supercd_dest_addr_b
                        REP   #$20
                        LDA.W #EBOOK_READ_BUFFER_ADDRESS
                        STA.W supercd_dest_addr_l
                        LDA.L EBOOK_READ_BUFFER_ADDRESS_L + $0A,X
                        CMP.W #$2000
                        BMI   @skip

                        LDA.W #$2000    ; Minimum of 8K

                 @skip: STA.W supercd_transfer_length
                        SEP   #$20
                        LDA.B #CD_MAX_SUBQ_ERRORS
                        STA.W supercd_max_subq_errors
                        JSL.L bios_LoadFromDisc
                        BCS   @exit
                        CLC

                 @exit: PLY
                        PLX
                        RTS


                        ; Search loaded block for a standard record with a
                        ; particular name/tag (e.d. CATALOG/START).
                        ; Data structure passed in Y.
                        ; If found, carry flag is cleared and offset to record
                        ; is returned in X.
      ebook_search_tag: LDX.W #0
         @loop_records: LDA.L EBOOK_READ_BUFFER_ADDRESS_L,X ; Get record length
                        BEQ   @exit_error
                        PHA
                        PHX
                        REP   #$20
                        TXA
                        CLC
                        ADC.W #$0021    ; Offset into record where tag is expected.
                        TAX
                        SEP   #$20
                        PHY             ; Save for later

                        ; Compare string, starting 3 bytes in to skip
                        ; destination info in passed data structure.
          @loop_string: LDA.W $3,Y
                        BEQ   @exit_ok
                        CMP.L EBOOK_READ_BUFFER_ADDRESS_L,X
                        BNE   @mismatch
                        INX
                        INY
                        BRA   @loop_string

             @mismatch: ; Tag not found in this record, try the next.
                        PLY             ; Back to original address passed in Y.
                        PLX
                        PLA
                        REP   #$20
                        AND.W #$00FF
                        STA.W ebook_tmp ; Track the offset
                        TXA
                        CLC
                        ADC.W ebook_tmp ; Becomes our returned value (X).
                        TAX
                        SEP   #$20
                        BRA   @loop_records

           @exit_error: SEC
                        RTS

              @exit_ok: PLY
                        PLX
                        PLA
                        CLC
                        RTS

;
; Check if CD is Super Disc format
;
; Returns: B = 0 if valid.
;
  super_disc_check_hdr: LDX.W #0
@loop_check_super_disc: LDA.W CD_HEADER_BUFFER,X        ; Buffer in WRAM
                        CMP.W data_header_super_disc,X  ; Validation data
                        BNE   @not_superdisc
                        INX
                        CPX.W #CD_HEADER_SIZE
                        BNE   @loop_check_super_disc

                        LDA.B #$00                      ; Zero = Super Disc
                        RTS

        @not_superdisc:
.IFNDEF ORIGINAL
                        ; The original code did not check for the Electronic
                        ; Book variant.
                        LDX.W #0
     @loop_check_ebook: LDA.W CD_HEADER_BUFFER,X        ; Buffer in WRAM
                        CMP.W data_header_ebook,X       ; Validation data
                        BNE   @not_ebook
                        INX
                        CPX.W #CD_HEADER_SIZE
                        BNE   @loop_check_ebook

                        LDA.B #$01                      ; Non-zero = Electronic Book
                        RTS
            @not_ebook:
.ENDIF
                        LDA.B #$FF                      ; Negative = no match.
                        RTS


                        ; Super Disc header data for comparison above.
data_header_super_disc: .DB $01
                        .DB "CD001"
                        .DB $01,$00
                        .DB "SUPERDISC       " ; "SUPERDISC" + 23 spaces
                        .DB "                "

                        ; Electronic Book disc header.
     data_header_ebook: .DB  $01
                        .DB  "CD001"
                        .DB  $01,$00
                        .DSB 32, $00

;
; BIOS function wrapper with basic error handling, used internally
;
mechacon_command_wrapper:
                        PHX
                        LDX.W #$0003

                 @send: PHA
                        JSL.L bios_SendMechaconCmd
                        PLA
                        BCC   @done
                        DEX
                        BNE   @send
                        LDX.W #title_status_no_cd_system
                        JSR.W sub_render_text

                 @loop: BRA @loop       ; Loop forever (well, until interrupt)

                 @done: PLX
                        RTS


;
; Title screen status strings, see `sub_render_text` for description
;
; Position text at a certain row, centered horizontally.
.DEFINE TITLE_STATUS_OFFSET ($02D0 - (I18N_LENGTH_STATUS / 2))
.DEFINE TITLE_STATUS_LENGTH I18N_LENGTH_STATUS

.IFDEF ORIGINAL
                        .ORGA $86AD
.ENDIF

title_status_access:    .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_YELLOW
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_ACCESS

title_status_push_start:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_WHITE
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_PUSH_START

title_status_now_loading:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_WHITE
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_NOW_LOADING

title_status_no_disc:   .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_YELLOW
                        .DB  TITLE_STATUS_LENGTH,
                        I18N_STATUS_NO_DISC

title_status_no_cd_system:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_NO_CD_ROM_SYSTEM

title_status_disc_error:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_DISC_ERROR

title_status_not_super_disc:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_NOT_SUPER_DISC

title_status_music_disc:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_CYAN
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_MUSIC_DISC

title_status_ipl_not_found:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_IPL_NOT_FOUND

title_status_mcom_error:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_M_COM_ERROR

title_status_not_defined:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_NOT_DEFINED

title_status_tray_open: .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_YELLOW
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_TRAY_OPEN

title_status_not_eb:    .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_RED
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_NOT_EB

title_status_electronic_book:
                        .DW  TITLE_STATUS_OFFSET
                        .DW  TEXT_COLOUR_WHITE
                        .DB  TITLE_STATUS_LENGTH
                        I18N_STATUS_ELECTRONIC_BOOK


      sub_init_vectors: LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode

                        LDX.W #dummy_handler
                        STX.W supercd_bios_nmi_vector_address
                        STX.W supercd_bios_irq_vector_address

                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank
                        RTS

         dummy_handler: RTI


    ebook_init_vectors: STZ.W NMITIMEN
                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        LDX.W #ebook_nmi_vram_dma
                        STX.W supercd_bios_nmi_vector_address
                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        LDA.B #(NMITIMEN_VBLANK|NMITIMEN_JOYPAD)
                        STA.W NMITIMEN
                        RTS


  sub_init_ppu_and_on_chip:
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP
                        LDA.B #$63
                        STA.W OBSEL
                        LDA.B #$00
                        STA.W OAMADDL
                        STA.W OAMADDH
                        LDA.B #$09              ; 16 colour, 8x8 tiles
                        STA.W BGMODE
                        LDA.B #$00
                        STA.W MOSAIC
                        LDA.B #VRAM_PAGE_BG1
                        STA.W BG1SC
                        STZ.W BG2SC
                        LDA.B #VRAM_PAGE_BG3
                        STA.W BG3SC
                        STZ.W BG4SC
                        LDA.B #(TILE_OFFSET_BG1 >> 12)
                        STA.W BG12NBA
                        LDA.B #(TILE_OFFSET_BG3 >> 12)
                        STA.W BG34NBA
                        LDA.B #$00
                        STA.W BG1HOFS
                        STA.W BG1HOFS
                        STA.W BG1VOFS
                        STA.W BG1VOFS
                        STA.W BG2HOFS
                        STA.W BG2HOFS
                        STA.W BG2VOFS
                        STA.W BG2VOFS
                        STA.W BG3HOFS
                        STA.W BG3HOFS
                        LDA.B #$FF
                        STA.W BG3VOFS
                        STA.W BG3VOFS
                        LDA.B #$00
                        STA.W BG4HOFS
                        STA.W BG4HOFS
                        STA.W BG4VOFS
                        STA.W BG4VOFS
                        LDA.B #$80              ; autoincrement
                        STA.W VMAIN
                        LDA.B #$00
                        STA.W VMADDL
                        STA.W VMADDH
                        STA.W M7SEL
                        STA.W M7A
                        LDA.B #$01
                        STA.W M7A
                        LDA.B #$00
                        STA.W M7B
                        STA.W M7B
                        STA.W M7C
                        STA.W M7C
                        STA.W M7D
                        LDA.B #$01
                        STA.W M7D
                        LDA.B #$00
                        STA.W M7X
                        STA.W M7X
                        STA.W M7Y
                        STA.W M7Y
                        STA.W CGADD
                        STA.W W12SEL
                        STA.W W34SEL
                        STA.W WOBJSEL
                        STA.W WH0
                        STA.W WH1
                        STA.W WH2
                        STA.W WH3
                        STA.W WBGLOG
                        STA.W WOBJLOG
                        LDA.B #$05              ; Enable BG1 and BG3
                        STA.W TM
                        STA.W TS
                        LDA.B #$00
                        STA.W TMW
                        STA.W TSW
                        LDA.B #$30
                        STA.W CGWSEL
                        LDA.B #$00
                        STA.W CGADSUB
                        LDA.B #$E0
                        STA.W COLDATA
                        LDA.B #$08
                        STA.W SETINI
                        LDA.B #NMITIMEN_JOYPAD
                        STA.W NMITIMEN
                        LDA.B #$FF
                        STA.W WRIO
                        LDA.B #$00
                        STA.W WRMPYA
                        STA.W WRMPYB
                        STA.W WRDIVL
                        STA.W WRDIVH
                        STA.W WRDIVB
                        STA.W HTIMEL
                        STA.W HTIMEH
                        STA.W VTIMEL
                        STA.W VTIMEH
                        STA.W MDMAEN
                        STA.W HDMAEN
                        RTS


load_title_screen_tiles:
                        LDX.W #$0000                    ; Set VRAM address 0000
                        STX.W VMADDL
                        LDX.W #data_dma_clear_vram
                        JSR.W sub_copy_gfx_data         ; Clears VRAM

                        LDX.W #TILE_OFFSET_BG1          ; Load greyscale tiles
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_greyscale
                        JSR.W sub_copy_gfx_data

                        STZ.W CGADD                     ; Clear palette
                        LDX.W #data_dma_clear_palette
                        JSR.W sub_copy_gfx_data

                        LDX.W #TILE_OFFSET_BG1 + $100   ; Load title screen tiles
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_title
                        JSR.W sub_copy_gfx_data

                        LDX.W #VRAM_ADDR_BG1            ; Load title screen tile map
                        STX.W VMADDL
                        LDX.W #0
        @loop_tile_map: LDA.L tilemap_title_screen,X
                        STA.W VMDATAL
                        LDA.B #$04                      ; Attributes always $04
                        STA.W VMDATAH
                        INX
                        CPX.W #SCREEN_SIZE_W
                        BNE   @loop_tile_map

                        LDA.B #$10                      ; Load first part of palette data
                        STA.W CGADD
                        LDX.W #data_dma_palette_1
                        JSR.W sub_copy_gfx_data

                        ; Font tiles are loaded without DMA so that they may be
                        ; manipulated before writing to VRAM.
                        LDX.W #TILE_OFFSET_BG3
                        STX.W VMADDL
                        REP   #$20
                        LDX.W #0
   @loop_font_colour_1: LDA.L gfx_font_tiles,X
                        AND.W #$00FF                    ; Bitwise manipulation
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @loop_font_colour_1

                        ; Same as above, just with different bitwise operations,
                        ; producing different colours in the resulting tiles.
                        LDX.W #TILE_OFFSET_BG3 + FONT_TILESET_SIZE
                        STX.W VMADDL
                        LDX.W #0
   @loop_font_colour_2: LDA.L gfx_font_tiles,X
                        AND.W #$00FF                    ; Bitwise manipulation
                        EOR.W #$FFFF                    ; Flip the bits (reverse video)
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @loop_font_colour_2

                        SEP   #$20                      ; Load second part of palette data
                        STZ.W CGADD
                        LDX.W #data_dma_palette_2
                        JSR.W sub_copy_gfx_data

                        ; Fill BG3 with blank spaces (`clear_overlay_text` does
                        ; the same thing but probably postdates this code).
                        REP   #$20
                        LDX.W #0
                        LDA.W #$2048                    ; $48 = "space" character, attributes $20
            @loop_fill: STA.W WRAM_ADDR_BG3,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @loop_fill

                        ; BG3 DMA copy (WRAM -> VRAM)
                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram
                        JSR.W sub_copy_gfx_data

                        STZ.W ebook_graphics_init_flag
                        RTS


; Data for `sub_copy_gfx_data` routine:
;  byte   0: DMA params
;  byte   1: B-Bus address ($21xx)
;  byte 2-4: source (low, high, bank)
;  byte 5-6: count (low, ligh)

                        ; clear VRAM
   data_dma_clear_vram: .DB $09         ; no autoinc source
                        .DB (VMDATAL & $FF)
                        .DW data_dma_zero_byte
                        .DB $00         ; bank 0
                        .DW $0000       ; 0000 = roll over, so $10000 (64Kb)

                        ; load first set of ROM tiles
data_dma_gfx_greyscale: .DB $01         ; autoinc source, bytes interleaved
                        .DB (VMDATAL & $FF)
                        .DW gfx_tiles_greyscale
                        .DB $00         ; bank 0
                        .DW $0200       ; 512 bytes

                        ; clear palette data
data_dma_clear_palette: .DB $09         ; no autoinc source
                        .DB (CGDATA & $FF)
                        .DW data_dma_zero_byte
                        .DB $00         ; bank 0
                        .DW $0100       ; 256 bytes

                        ; load second set of ROM tiles
    data_dma_gfx_title: .DB $01         ; autoinc source, bytes interleaved
                        .DB (VMDATAL & $FF)
                        .DW gfx_title_screen_tiles
                        .DB $00         ; bank 0
                        .DW $2000       ; 8K bytes

                        ; load palette part 1 from ROM
    data_dma_palette_1: .DB $00         ; autoinc source, single bytes
                        .DB (CGDATA & $FF)
                        .DW data_palette_part_1
                        .DB $00         ; bank 0
                        .DW $0020       ; 32 bytes

                        ; load palette part 2 from ROM
    data_dma_palette_2: .DB $00         ; autoinc source, single bytes
                        .DB (CGDATA & $FF)
                        .DW data_palette_part_2
                        .DB $00         ; bank 0
                        .DW $0020       ; 32 bytes

                        ; transfer tiles from BG3 WRAM buffer to VRAM
 data_dma_gfx_wram_to_vram:
                        .DB $01         ; autoinc source, bytes interleaved
                        .DB (VMDATAL & $FF)
                        .DW WRAM_ADDR_BG3
                        .DB $00         ; bank 0
                        .DW SCREEN_SIZE_B

                        ; zero byte, for clear operations above
    data_dma_zero_byte: .DB $00


     sub_copy_gfx_data: LDA.W 0,X
                        STA.W DMAP0

                        LDA.W 1,X
                        STA.W BBAD0

                        LDA.W 2,X
                        STA.W A1T0L

                        LDA.W 3,X
                        STA.W A1T0H

                        LDA.W 4,X
                        STA.W A1B0

                        LDA.W 5,X
                        STA.W DAS0L

                        LDA.W 6,X
                        STA.W DAS0H

                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        RTS


                        ;     B             G            R
   data_palette_part_2: .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($1F << 10) | ($1F << 5) | ($1F)
                        .DW ($1F << 10) | ($1F << 5) | ($1F)
                        .DW ($16 << 10) | ($0C << 5) | ($09)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($1F << 10) | ($1F << 5) | ($00)
                        .DW ($0E << 10) | ($0C << 5) | ($09)
                        .DW ($0E << 10) | ($0C << 5) | ($09)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($04 << 10) | ($04 << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($1F << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)


                        ; RAM menu setup.
                        ; Load tiles and initialise the background
     ram_menu_bg_setup: LDA.B #INIDISP_BLANK    ; blank screen
                        STA.W INIDISP

                        ; Tile data is compressed with a simple RLE algorithm.
                        ; One byte indicates the length of a run, the following
                        ; word is the data to be repeated that many times. Ends
                        ; when a block with zero length is found.
                        LDX.W #TILE_OFFSET_BG1  ; load BG1 tiles
                        STX.W VMADDL
            @read_tile: SEP   #$20
                        LDA.L gfx_ram_menu_tiles,X
                        BEQ   @tiles_loaded     ; break when first byte = 0
                        INX
                        REP   #$20
                        AND.W #$00FF            ; save size in Y
                        TAY
                        LDA.L gfx_ram_menu_tiles,X

           @write_tile: STA.W VMDATAL           ; write to VRAM
                        DEY
                        BNE   @write_tile       ; until data exhausted

                        INX
                        INX
                        BRA   @read_tile

         @tiles_loaded: LDX.W #VRAM_ADDR_BG1    ; point to BG1 screen data
                        STX.W VMADDL

                        ; Load tile map
                        REP   #$20
                        LDX.W #0
         @loop_tilemap: LDA.L data_tilemap_ram_menu,X
                        AND.W #$00FF
                        ORA.W #$0800            ; attributes $08
                        STA.W VMDATAL
                        INX
                        CPX.W #SCREEN_SIZE_W
                        BNE   @loop_tilemap

                        ; load palette
                        SEP   #$20
                        LDA.B #$20              ; palette address = second part
                        STA.W CGADD
                        LDX.W #0
         @loop_palette: LDA.W data_palette_ram_menu,X
                        STA.W CGDATA
                        INX
                        CPX.W #32               ; bytes of data
                        BNE   @loop_palette

                        LDA.B #INIDISP_NO_BLANK ; screen back on
                        STA.W INIDISP
                        RTS


                        ; 32 bytes palette data for above
 data_palette_ram_menu: .DB $00,$00,$D0,$6A,$5A,$6B,$29,$6A
                        .DB $4A,$29,$B0,$66,$89,$55,$D6,$5A
                        .DB $52,$4A,$FC,$0B,$FA,$73,$9F,$2B
                        .DB $69,$23,$DF,$06,$28,$2A,$00,$00


; Clear BG3 by filling it with "space" characters.
; Required because tile 0 is not blank, but the top of the "0" character.
    clear_overlay_text: REP   #$20
                        LDX.W #0
                        LDA.W #$2048            ; top of the "space" character, attributes $20
            @tile_loop: STA.W WRAM_ADDR_BG3,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @tile_loop

                        SEP   #$20
                        PHP
                        SEP   #$20
          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        LDX.W #VRAM_ADDR_BG3    ; DMA the buffer to screen
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram
                        JSR.W sub_copy_gfx_data
                        RTS


; Draw text to screen
; Input: X = pointer to text data structure
;   byte 0-1: position
;   byte 2-3: colour (attributes)
;   byte   4: length
;   byte  5+: text, chars encoded as per sram file names
       sub_render_text: JSR.W render_text_to_wram  ; render text to a WRAM buffer
                        JSR.W render_text_to_vram  ; use DMA to copy buffer to VRAM
                        RTS

   render_text_to_wram: .DEFINE tmp_top_tile_address    $00BA
                        .DEFINE tmp_bottom_tile_address $00BC
                        .DEFINE tmp_text_colour         $00B8

                        PHP
                        PHD
                        REP   #$20
                        LDA.W #0
                        TCD

                        LDA.W TEXT_OFFSET_POSITION,X    ; bytes 0+1 (position/offset)
                        ASL   A
                        CLC
                        ADC.W #WRAM_ADDR_BG3            ; start of WRAM buffer
                        STA.W tmp_top_tile_address      ; location of top font tile
                        CLC
                        ADC.W #SCREEN_WIDTH_B           ; next row of screen
                        STA.W tmp_bottom_tile_address   ; location of bottom font tile

                        LDA.W TEXT_OFFSET_COLOUR,X      ; bytes 2+3 (colour)
                        STA.W tmp_text_colour           ; store for later

                        LDA.W TEXT_OFFSET_LENGTH,X      ; byte 4 (length)
                        AND.W #$00FF                    ; max 255

                        LDY.W #0
           @loop_chars: PHA                             ; save loop counter
                        LDA.W TEXT_OFFSET_STRING,X      ; offset from start of string
                        INX
                        AND.W #$00FF
                        ASL   A                         ; multiply index by 2 (tiles per char)
                        ORA.W tmp_text_colour           ; OR in the colour attributes
                        STA.B (tmp_top_tile_address),Y  ; write character to WRAM (2 tiles)
                        INC   A                         ; bottom character is always at top+1
                        STA.B (tmp_bottom_tile_address),Y

                        INY
                        INY
                        PLA                             ; restore loop counter
                        DEC   A
                        BNE   @loop_chars               ; until end of string

                        PLD
                        PLP
                        RTS


                        ; Copy BG3 WRAM buffer to VRAM
   render_text_to_vram: PHP
                        SEP   #$20
                        PHP
                        SEP   #$20

        @wait_vblank:   LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        LDX.W #VRAM_ADDR_BG3
                        STX.W VMADDL

                        LDX.W #data_dma_gfx_wram_to_vram
                        JSR.W sub_copy_gfx_data
                        PLP
                        RTS


; Looks like unused copies of RAM menu text in alternate colours and with
; a slightly different legend.
ram_menu_text_backup_ram:
                        .DW $0030 - (I18N_LENGTH_TITLE_BACKUP_RAM / 2)
                        .DW TEXT_COLOUR_GREEN
                        .DB I18N_LENGTH_TITLE_BACKUP_RAM
                        I18N_TITLE_BACKUP_RAM

ram_menu_text_legend:   .DW $0306
                        .DW TEXT_COLOUR_GREEN
                        .DB I18N_LENGTH_RAM_MENU_LEGEND
                        I18N_RAM_MENU_LEGEND_ALTERNATE

gfx_tiles_greyscale:    .INCLUDE "gfx/greyscale.inc"


; Draw the main Super Disc logo, appearing from left to right.
draw_main_logo_animated:
                        .DEFINE tmp_tile_address $00B6

                        LDX.W #VRAM_ADDR_BG1 + $87
                        STX.W tmp_tile_address
                        LDY.W #0

          @loop_column: PHP
                        SEP   #$20

          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank

                        LDA.W RDNMI
                        PLP
                        LDX.W tmp_tile_address
                        LDA.B #$0B              ; 11 rows

             @loop_row: PHA
                        STX.W VMADDL
                        LDA.W tile_map_main_logo,Y
                        STA.W VMDATAL
                        LDA.B #$04              ; high byte of attributes
                        STA.W VMDATAH
                        INY
                        REP   #$20
                        TXA
                        CLC
                        ADC.W #$0020            ; next row
                        TAX
                        SEP   #$20
                        PLA
                        DEC   A
                        BNE   @loop_row

                        LDX.W tmp_tile_address
                        INX
                        STX.W tmp_tile_address

                        ; delay...
                        PHY
                        PHX
                        LDY.W #$0014
          @delay_outer: LDX.W #$00FF
          @delay_inner: DEX
                        BNE   @delay_inner
                        DEY
                        BNE   @delay_outer

                        PLX
                        PLY
                        CPY.W #(11*16)          ; size of map
                        BNE   @loop_column
                        RTS


; Draw the main Super Disc logo.
; Same as above but without the delays/animation.
  draw_main_logo_fast:  LDX.W #VRAM_ADDR_BG1 + $87
                        STX.W tmp_tile_address
                        LDY.W #0

          @loop_column: LDX.W tmp_tile_address
                        LDA.B #$0B              ; 11 rows

             @loop_row: PHA
                        STX.W VMADDL
                        LDA.W tile_map_main_logo,Y
                        STA.W VMDATAL
                        LDA.B #$04              ; high byte of attributes
                        STA.W VMDATAH
                        INY
                        REP   #$20
                        TXA
                        CLC
                        ADC.W #$0020            ; next row
                        TAX
                        SEP   #$20
                        PLA
                        DEC   A
                        BNE   @loop_row

                        LDX.W tmp_tile_address
                        INX
                        STX.W tmp_tile_address
                        CPY.W #(11*16)          ; size of map
                        BNE   @loop_column
                        RTS

;
; Electronic Book routines for lower-level stuff.
;
                        ; NMI hander for electronic book.
                        ; Does DMA from WRAM "frame buffer" to VRAM for display.
    ebook_nmi_vram_dma: REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        SEP   #$20
                        LDA.B #$00
                        PHA
                        PLB
                        JSL.L bios_WramToVramDMA
                        LDA.W ebook_graphics_init_flag          ; In EB mode, gfx ready.
                        BEQ   @get_joypad_status                ; Skip the VRAM copy

                        ; The VRAM copy is split into quarters. Each NMI one
                        ; quarter is copied.
                        SEP   #$10
                        LDX.W ebook_vram_quarter
                        TXA
                        INC   A
                        INC   A
                        AND.B #$07
                        STA.W ebook_vram_quarter

                        ; Get the addresses for this quarter.
                        REP   #$20
                        LDA.W data_ebook_nmi_vram_table,X       ; VRAM addresses
                        STA.W VMADDL
                        LDA.W data_ebook_nmi_wram_table,X       ; Source addresses

                        ; Apply scrolling offset.
                        CLC
                        ADC.B ebook_scroll_l
                        STA.W A1T5L

                        SEP   #$20
                        REP   #$10

                        LDA.B #$01                              ; Direct table, 2 bytes interleaved (for VRAM)
                        STA.W DMAP5

                        LDA.B #EBOOK_VRAM_BUFFER_BANK
                        STA.W A1B5

                        LDA.B #(VMDATAL & $FF)                  ; B-Bus address
                        STA.W BBAD5

                        LDX.W #SCREEN_SIZE_B * 2                ; Byte count
                        STX.W DAS5L

                        LDA.B #MDMAEN_CHANNEL5                  ; Enable/trigger
                        STA.W MDMAEN

    @get_joypad_status: REP   #$20
                        LDA.W JOY1L
                        BNE   @button_pressed
                        STZ.W supercd_joypad1_status_l
                        BRA   @end

       @button_pressed: CMP.W supercd_joypad1_status_l
                        BEQ   @no_change

                        ; Button pressed since last checked.
                        SEP   #$20
                        STZ.W ebook_joypad_delay_h      ; Reset counter
                        STZ.W ebook_joypad_delay_l
                        REP   #$20
                        BRA   @store_status

            @no_change: TAX
                        SEP   #$20
                        LDA.W ebook_joypad_delay_l
                        CMP.B #32
                        BPL   @delay_reached

                        INC   A                         ; Increment counter
                        STA.W ebook_joypad_delay_l
                        BRA   @end

        @delay_reached: INC   A
                        AND.B #$2F                      ; Auto-repeat
                        STA.W ebook_joypad_delay_l
                        AND.B #$03
                        BNE   @end
                        REP   #$20
                        TXA

         @store_status: LDX.W ebook_joypad_status_queue_tail
                        PHA                             ; Save JOY1L/H
                        TXA
                        INC   A                         ; Increment pointer
                        INC   A
                        AND.W #$001F                    ; Limit to buffer bounds
                        CMP.W ebook_joypad_status_queue_head
                        BEQ   @queue_full

                        STA.W ebook_joypad_status_queue_tail
                        PLA                             ; Restore JOY1L/H
                        STA.W ebook_joypad_status_queue,X
                        BRA   @end

           @queue_full: PLA

                  @end: REP #$20
                        PLB
                        PLY
                        PLX
                        PLA
                        RTI

                        ; VRAM addressed in quarters.
data_ebook_nmi_vram_table:
                        .DW $0000, $0700, $0E00, $1500

                        ; Source data locations for each quarter (bank 7F).
data_ebook_nmi_wram_table:
                        .DW $0000, $0E00, $1C00, $2A00

                        ; Return pending joypad status in X.
                        ; Carry set == queue empty.
  ebook_process_joypad: LDX.W ebook_joypad_status_queue_head
                        CPX.W ebook_joypad_status_queue_tail
                        BEQ   @queue_empty

                        REP   #$20
                        LDA.W ebook_joypad_status_queue,X
                        PHA             ; Save status
                        TXA             ; Update queue admin
                        INC   A
                        INC   A
                        AND.W #$001F
                        STA.W ebook_joypad_status_queue_head
                        PLX             ; Saved status => X
                        SEP   #$20
                        CLC
                        RTS

          @queue_empty: SEC
                        RTS


      ebook_clear_vars: LDX.W #0
                        STX.W ebook_joypad_status_queue_tail
                        STX.W ebook_joypad_status_queue_head
                        STZ.W $00B4
                        STZ.W $00B5
                        RTS


   ebook_init_graphics: LDA.B #$00
                        STA.W NMITIMEN
                        LDA.B #INIDISP_FORCED_BLANKING
                        STA.W INIDISP
                        LDA.B #$04      ; Enable BG3
                        STA.W TM
                        STA.W TS
                        LDA.B #VRAM_PAGE_BG3
                        STA.W BG3SC
                        LDA.B #$00
                        STA.W BG34NBA
                        REP   #$20
                        LDA.W #VRAM_ADDR_BG3
                        STA.W VMADDL

                        LDX.W #0
     @fill_screen_loop: LDA.L data_ebook_screen,X
                        STA.W VMDATAL
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @fill_screen_loop

                        STZ.B ebook_flag_link
                        STZ.B ebook_scroll_l
                        STZ.B ebook_scroll_h
                        JSR.W ebook_clear_vram_buffers
                        LDA.B #$80
                        STA.W ebook_graphics_init_flag
                        STZ.W ebook_vram_quarter
                        LDA.B #(NMITIMEN_VBLANK|NMITIMEN_JOYPAD)
                        STA.W NMITIMEN
                        PHP
                        SEP   #$20

         @wait_vblank1: LDA.W RDNMI
                        BPL   @wait_vblank1
                        LDA.W RDNMI
                        PLP
                        PHP
                        SEP   #$20

         @wait_vblank2: LDA.W RDNMI
                        BPL   @wait_vblank2
                        LDA.W RDNMI
                        PLP
                        PHP
                        SEP   #$20

         @wait_vblank3: LDA.W RDNMI
                        BPL   @wait_vblank3
                        LDA.W RDNMI
                        PLP
                        PHP
                        SEP   #$20

         @wait_vblank4: LDA.W RDNMI
                        BPL   @wait_vblank4
                        LDA.W RDNMI
                        PLP

                        ; Screen back on
                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        RTS


                        ; Clears 7F:0000 - 7F:FFFF
ebook_clear_vram_buffers:
                        REP   #$20
                        LDA.W #$0000
                        TAX

                 @loop: STA.L EBOOK_VRAM_BUFFER_ADDRESS_L,X
                        INX
                        INX
                        CPX.W #$0000
                        BNE   @loop
                        SEP   #$20
                        RTS


                        ; Data passed in X
     ebook_render_text: REP   #$20
                        LDA.B ebook_cursor_row
                        AND.W #$000F                    ; 0000 0000 0000 HHHH
                        ASL A
                        ASL A                           ; 0000 0000 00HH HH00
                        XBA                             ; 00HH HH00 0000 0000
                        STA.B ebook_vram_buffer_pointer
                        LDA.B ebook_cursor_column
                        AND.W #$001F                    ; 0000 0000 000L LLLL
                        ASL A
                        ASL A
                        ASL A
                        ASL A
                        ASL A                           ; 0000 00LL LLL0 0000
                        ORA.B ebook_vram_buffer_pointer ; 00HH HHLL LLL0 0000
                        STA.B ebook_vram_buffer_pointer

                 @loop: REP   #$20
                        LDA.W 0,X
                        BEQ   @exit                     ; Stop at $00 terminator
                        XBA
                        INX
                        INX
                        JSR.W ebook_render_jis_character
                        SEP   #$20
                        LDA.B ebook_cursor_column
                        CMP.B #$20
                        BMI   @loop

                 @exit: SEP   #$20
                        RTS


ebook_clear_group_vars: STZ.B ebook_indent
                        STZ.B ebook_vram_buffer_pointer
                        STZ.B ebook_vram_buffer_pointer + 1
                        STZ.B ebook_screen_content_pointer
                        STZ.B ebook_screen_content_pointer + 1
                        RTS


ebook_update_screen_content:
                        PHX
                        PHY
                        ; Cursor to 0,0
                        STZ.B ebook_cursor_column
                        STZ.B ebook_cursor_row

                        LDX.B ebook_screen_content_pointer

                        ; Check if we've reached the end of the buffer.
                        CPX.W #CD_SECTOR_SIZE * 16
                        BMI   @next_row

                        ; This NOP might be a place holder for code that would
                        ; be required to load the next section of the book.
                        NOP

             @next_row: SEP   #$20
                        LDA.B ebook_cursor_row
                        CMP.B #SCREEN_HEIGHT_RAW
                        BMI   @next_char
                        JMP.W @exit_error

            @next_char: REP   #$20
                        LDA.L EBOOK_PAGE_BUFFER_ADDRESS_L,X
                        XBA
                        STA.B ebook_character_code
                        INX
                        INX
                        SEP   #$20
                        XBA
                        CMP.B #$1F              ; Control code
                        BNE   @not_control_code

                        ; Is a control code, get the other byte
                        XBA
                        CMP.B #$04              ; Start of link
                        BNE   @check_end_link

                        ; Start of link, set the flag
                        LDA.B #$FF
                        STA.B ebook_flag_link
                        BRA   @next_char

       @check_end_link: CMP.B #$05              ; End of link
                        BNE   @check_lf
                        STZ.B ebook_flag_link
                        BRA   @next_char

             @check_lf: CMP.B #$0A              ; Line feed
                        BNE   @not_lf
                        JSR.W ebook_new_line
                        BRA   @next_char

               @not_lf: CMP.B #$09
                        BNE   @not_extended

                        ; Extended control code, needs next word too.
                        REP   #$20
                        LDA.L EBOOK_PAGE_BUFFER_ADDRESS_L,X
                        XBA
                        INX
                        INX
                        CMP.W #$0001            ; End of indent
                        BNE   @check_indent

                        ; Reset the indent value to 0.
                        SEP   #$20
                        STZ.B ebook_indent
                        JMP.W @next_char

         @check_indent: CMP.W #$0004            ; Start indent
                        BNE   @nop_and_next_char

                        ; Set indent - subsequent lines will be indented until
                        ; end of indent code is encountered.
                        SEP   #$20
                        LDA.B #$02
                        STA.B ebook_indent
                        JMP.W @next_char

                        ; Not an extended code, what to do depends solely on
                        ; the byte after the "$1F".
         @not_extended: CMP.B #$02
                        BEQ   @no_action
                        CMP.B #$0E
                        BEQ   @no_action
                        CMP.B #$0F
                        BEQ   @no_action
                        CMP.B #$10
                        BEQ   @no_action
                        CMP.B #$11
                        BEQ   @no_action
                        CMP.B #$61
                        BEQ   @no_action
                        CMP.B #$65
                        BEQ   @no_action
                        CMP.B #$33
                        BEQ   @skip_two_bytes
                        CMP.B #$41
                        BEQ   @skip_two_bytes
                        CMP.B #$45
                        BEQ   @skip_two_bytes
                        CMP.B #$53
                        BNE   @nop_and_next_char

                        ; Skip 6 + 2 bytes, 8 in total.
                        INX
                        INX
                        INX
                        INX
                        INX
                        INX

       @skip_two_bytes: INX
                        INX

            @no_action: JMP.W @next_char

    @nop_and_next_char: NOP
                        JMP.W @next_char

     @not_control_code: LDA.B ebook_flag_link
                        BNE   @not_in_link

                        JSR.W ebook_render_link_char
                        BCS   @exit_error
                        JMP.W @next_row

          @not_in_link: JSR.W ebook_render_eb_char
                        BCS   @exit_error
                        JMP.W @next_row

           @exit_error: STX.B ebook_screen_content_pointer
                        SEP   #$20
                        PLY
                        PLX
                        RTS


                        ; Render character that's part of a link.
ebook_render_link_char: PHP
                        LDA.B ebook_cursor_column
                        CMP.B #SCREEN_WIDTH_W - 3
                        BMI   @skip_newline
                        CMP.B #SCREEN_WIDTH_W - 1
                        BPL   @insert_newline

                        ; Exactly column 30, check for characters which belong
                        ; on this line or should at the start of the next.
                        REP   #$20
                        LDA.B ebook_character_code
                        CMP.W #$212B            ; Dakuten (dots)
                        BMI   @skip_newline
                        CMP.W #$2147            ; Apostrophe
                        BMI   @insert_newline
                        CMP.W #$215C            ; Plus sign
                        BPL   @insert_newline
                        AND.W #$0001            ; Odd numbered codes
                        BNE   @skip_newline

       @insert_newline: JSR.W ebook_new_line
                        BCS   @exit_end_of_screen

         @skip_newline: SEP   #$20
                        LDA.B ebook_cursor_column
                        BNE   @no_indent
                        LDA.B ebook_indent
                        BEQ   @no_indent

                        REP   #$20
                        LDA.W #$2121    ; IDSP (space character)
                        JSR.W ebook_render_jis_character

            @no_indent: REP   #$20
                        LDA.B ebook_character_code
                        JSR.W ebook_render_jis_character
                        PLP
                        CLC
                        RTS

   @exit_end_of_screen: PLP
                        SEC
                        RTS


ebook_render_jis_character:
                        PHX
                        CMP.W #$7E7F                    ; Max valid JIS code
                        BMI   @under_max

                        ; Above max, special codes for something...?
                        SEP   #$20
                        XBA                             ; Get upper byte
                        CMP.B #$A4
                        BNE   @rom_char

                        ; Tile data in WRAM
                        LDA.B #$7E
                        STA.B ebook_char_data_bank
                        XBA                             ; Get low byte back
                        SEC
                        SBC.B #$21                      ; First char in any JIS table.
                        REP   #$20
                        AND.W #$00FF                    ; 0000 0000 1111 1111
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A                         ; 0001 1111 1110 0000 (x 32 == size of char data in words)
                        CLC
                        ADC.W #$3000                    ; 0011 0000 0000 0000 (offset)
                        STA.B ebook_char_data_pointer   ; 0100 1111 1110 0000
                        BRA   @write_to_memory

             @rom_char: REP   #$20
                        LDA.W #$2121                    ; IDSP (space character)

            @under_max: SEC
                        SBC.W #$2100                    ; Start of JIS charset
                        BPL   @over_2100
                        LDA.W #$0021

            @over_2100: XBA
                        SEP   #$20
                        PHA
                        LDA.B #$03                      ; Start with bank $03
                        STA.B ebook_char_data_bank
                        PLA
            @next_bank: CMP.B #$0A
                        BMI   @break
                        SEC
                        SBC.B #$0A
                        INC.B ebook_char_data_bank
                        BRA   @next_bank

                @break: REP   #$20
                        XBA                             ; Set bank in B
                        ASL   A                         ; x2 makes word pointer
                        TAX
                        LDA.L ebook_jis_char_map,X
                        STA.B ebook_char_data_pointer

                        ; Write tile data to a WRAM buffer, later rendered to
                        ; VRAM in NMI handler using DMA.
      @write_to_memory: LDY.W #0
                        LDX.B ebook_vram_buffer_pointer
       @loop_char_data: LDA.B [ebook_char_data_pointer],Y
                        AND.W #$00FF
                        STA.L EBOOK_VRAM_BUFFER_ADDRESS_L,X
                        INY
                        INX
                        INX
                        CPY.W #32                       ; Words, for 8x8 tile.
                        BNE   @loop_char_data

                        STX.B ebook_vram_buffer_pointer
                        INC.B ebook_cursor_column       ; Move "cursor" + 1 word
                        INC.B ebook_cursor_column
                        PLX
                        RTS

                        ; Render a character represented by the Electronic
                        ; Book standard codes. Already checked for any special
                        ; circumstances, like control codes and whether we're
                        ; in the middle of a link (these have their own
                        ; special routines).
ebook_render_eb_char:   PHX
                        LDA.B ebook_character_code + 1  ; high byte
                        CMP.B #$A1
                        BNE   @not_custom

                        ; Custom character data in RAM (copied from disc).
                        LDA.B #$7E
                        STA.B ebook_char_data_bank
                        LDA.B ebook_character_code      ; low byte
                        SEC
                        SBC.B #$21
                        REP   #$20
                        AND.W #$00FF                    ; 0000 0000 1111 1111
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A                         ; 0000 1111 1111 0000
                        CLC
                        ADC.W #EBOOK_CHAR_BUFFER_ADDRESS
                        STA.B ebook_char_data_pointer
                        BRA   @write_to_memory

           @not_custom: REP   #$20
                        STZ.B ebook_double_width        ; For now, assume single width
                        LDA.B ebook_character_code
                        CMP.W #$2120                    ; First JIS character
                        BMI   @round_up

                        CMP.W #$2577                    ; Max JIS katakana
                        BMI   @calculate_offset

             @round_up: LDA.W #$2120

     @calculate_offset: SEC
                        SBC.W #$2120                    ; Map $2120 to $0000
                        ASL   A                         ; x2 to make word pointer
                        TAX
                        LDA.L ebook_eb_char_map,X
                        STA.B ebook_character_code
                        XBA
                        SEP   #$20
                        STA.B ebook_double_width        ; Anything non-zero
                        LDA.B ebook_cursor_column

                        ; Check if we're near the end of the line.
                        CMP.B #SCREEN_WIDTH_W - 2
                        BMI   @skip_newline
                        CMP.B #SCREEN_WIDTH_W - 1
                        BPL   @do_newline

                        ; On column 30, so coming up to a new line. Before we
                        ; do that, check for some characters which belong on
                        ; the current line or would look wrong on the next.
                        LDA.B ebook_character_code
                        CMP.B #$20                      ; Space
                        BEQ   @skip_newline
                        CMP.B #$2C                      ; Comma
                        BEQ   @skip_newline
                        CMP.B #$2E                      ; Full stop
                        BEQ   @skip_newline
                        CMP.B #$A4
                        BEQ   @skip_newline
                        CMP.B #$A1
                        BEQ   @skip_newline
                        CMP.B #$29                      ; Close parentheses
                        BEQ   @skip_newline
                        CMP.B #$5D                      ; Close square bracket
                        BEQ   @skip_newline

           @do_newline: JSR.W ebook_new_line
                        BCS   @exit_end_of_screen

         @skip_newline: LDA.B ebook_cursor_column
                        BNE   @no_indent
                        LDA.B ebook_indent
                        BEQ   @no_indent

                        ; Indent one space.
                        REP   #$20
                        LDA.W #$2121
                        JSR.W ebook_render_jis_character

            @no_indent: SEP   #$20
                        LDA.B #$0D
                        STA.B ebook_char_data_bank

                        REP   #$20
                        LDA.B ebook_character_code
    @loop_char_columns: AND.W #$00FF
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A                         ; x 16
                        CLC
                        ADC.W #data_ebook_char_tiles
                        STA.B ebook_char_data_pointer

      @write_to_memory: LDY.W #$0000
                        LDX.B ebook_vram_buffer_pointer

       @loop_char_data: LDA.B [ebook_char_data_pointer],Y
                        AND.W #$00FF
                        STA.L EBOOK_VRAM_BUFFER_ADDRESS_L,X
                        INY
                        INX
                        INX
                        CPY.W #16
                        BNE   @loop_char_data

                        SEP   #$20
                        STX.B ebook_vram_buffer_pointer
                        INC.B ebook_cursor_column
                        LDA.B ebook_double_width
                        BEQ   @exit_ok

                        ; Reset double width, will exit on next iteration.
                        STZ.B ebook_double_width
                        REP   #$20
                        BRA   @loop_char_columns

              @exit_ok: CLC
   @exit_end_of_screen: PLX
                        RTS


                        ; Fill current row to the end and place cursor at the
                        ; start of a new one.
                        ; Carry set if this sends the cursor off screen.
        ebook_new_line: PHP
                        PHX
                        LDX.B ebook_vram_buffer_pointer

     @until_end_of_row: SEP   #$20
                        LDA.B ebook_cursor_column
                        CMP.B #SCREEN_WIDTH_W
                        BPL   @done
                        INC.B ebook_cursor_column
                        REP   #$20

                        LDY.W #0
                        LDA.W #0
                 @copy: STA.L EBOOK_VRAM_BUFFER_ADDRESS_L,X
                        INY
                        INX
                        INX
                        CPY.W #16
                        BNE   @copy
                        BRA   @until_end_of_row

                 @done: STX.B ebook_vram_buffer_pointer
                        INC.B ebook_cursor_row
                        LDA.B ebook_cursor_row
                        CMP.B #SCREEN_HEIGHT_RAW
                        BPL   @exit_end_of_screen
                        STZ.B ebook_cursor_column
                        PLX
                        PLP
                        CLC
                        RTS

   @exit_end_of_screen: PLX
                        PLP
                        SEC
                        RTS

                        ;
                        ; End of Electronic Book code.
                        ;

                        ;
                        ; RAM file menu
                        ; Accessed by pressing select on title screen
                        ;
         ram_file_menu: PHP
                        SEP   #$20
                        REP   #$10
                        STZ.W ram_menu_joypad_delay_up
                        STZ.W ram_menu_joypad_delay_down
                        STZ.W ram_menu_b_button_lock

                        JSR.W ram_menu_bg_setup              ; load BG tiles

          @draw_screen: JSR.W clear_overlay_text             ; "clear" (fill) BG3 text
                        LDX.W #data_ram_menu_title           ; "BACKUP RAM"
                        JSR.W sub_render_text

                        LDX.W #data_ram_menu_legend          ; Legend text, fits between icons
                        JSR.W sub_render_text

                        LDY.W #SRAM_DIR_BUFFER               ; destination for RAM directory
                        JSL.L bios_SramGetDirectory

                        BCC   @dir_ok
                        LDX.W #data_ram_menu_error           ; RAM error
                        JMP.W @exit_with_error

               @dir_ok: STA.W ram_menu_number_of_files
                        ; Make a 16-bit value from the 8 we were given. High
                        ; byte is always 0.
                        STZ.W ram_menu_number_of_files + 1
                        CMP.B #0
                        BNE   @files_found
                        JMP.W @no_files

          @files_found: LDA.B #$00
                        STA.W ram_menu_selected_file
                        LDA.B #$24      ; " "
                        STA.W ram_menu_text_struct_string
                        ; Padding after text.
                        STA.W ram_menu_text_struct_string + 18
                        STA.W ram_menu_text_struct_string + 19
                        LDA.B #$27      ; "." after sequence number
                        STA.W ram_menu_text_struct_string + 3
                        LDA.B #20
                        STA.W ram_menu_text_struct_length
                        LDX.W #0
                        STX.W ram_menu_scroll_offset
                        STX.W ram_menu_draw_file_number

           @next_frame: PHP
                        SEP   #$20

          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        ; Point to area at the top of the file list box.
                        LDX.W #VRAM_ADDR_BG1 + $6F
                        STX.W VMADDL

                        ; Check if already scrolled to top of list.
                        LDA.W ram_menu_scroll_offset
                        BEQ   @no_scroll_up

                        ; Draw an "up" arrow at the top of the list, indicating
                        ; more files above.
                        LDX.W #$0803
                        STX.W VMDATAL
                        LDX.W #VRAM_ADDR_BG1 + $8F      ; 1 row down
                        STX.W VMADDL
                        LDX.W #$0806
                        STX.W VMDATAL
                        LDX.W #$0807
                        STX.W VMDATAL
                        BRA   @check_scroll_down

                        ; Overwrite the "up" arrow with normal background.
         @no_scroll_up: LDX.W #$0802
                        STX.W VMDATAL
                        LDX.W #VRAM_ADDR_BG1 + $8F      ; 1 row down
                        STX.W VMADDL
                        LDX.W #$0805
                        STX.W VMDATAL
                        STX.W VMDATAL

                        ; Check if all files fit on screen (with scrolling).
    @check_scroll_down: LDA.W ram_menu_scroll_offset
                        CLC
                        ADC.B #SRAM_FILES_ON_SCREEN
                        CMP.W ram_menu_number_of_files
                        BPL   @no_scroll_down

                        ; Draw a "down" arrow at the bottom of the list,
                        ; indicating more files below.
                        LDX.W #VRAM_ADDR_BG1 + $2AF
                        STX.W VMADDL
                        LDX.W #$080E
                        STX.W VMDATAL
                        LDX.W #$080F
                        STX.W VMDATAL
                        LDX.W #VRAM_ADDR_BG1 + $2CF
                        STX.W VMADDL
                        LDX.W #$0811
                        STX.W VMDATAL
                        BRA   @draw_file_list

                        ; Overwrite the "down" arrow with normal background.
       @no_scroll_down: LDX.W #VRAM_ADDR_BG1 + $2AF
                        STX.W VMADDL
                        LDX.W #$080D
                        STX.W VMDATAL
                        STX.W VMDATAL
                        LDX.W #VRAM_ADDR_BG1 + $2CF
                        STX.W VMADDL
                        LDX.W #$0804
                        STX.W VMDATAL

       @draw_file_list: LDX.W #$00A6    ; Top-left of the file list box.
                        STX.W ram_menu_text_struct_position
                        ; Figure out which file entry to write in the top slot.
                        LDX.W ram_menu_scroll_offset
                        STX.W ram_menu_draw_file_number

                        ; Draw the visible part of the file list.
                        LDY.W #0
      @draw_file_entry: PHY
                        LDX.W #TEXT_COLOUR_GREEN
                        TYA
                        CMP.W ram_menu_selected_file
                        BNE   @not_selected

                        LDX.W #TEXT_COLOUR_BLACK
         @not_selected: STX.W ram_menu_text_struct_attirbutes
                        SEP   #$20
                        STZ.W ram_menu_text_struct_string + 1   ; "0"
                        LDA.W ram_menu_draw_file_number
                        INC A

                        ; Create the sequence number in front of each entry.
      @until_under_ten: CMP.B #10
                        BMI   @got_units
                        SEC
                        SBC.B #10
                        INC.W ram_menu_text_struct_string + 1   ; increment tens
                        BRA   @until_under_ten

                        ; Replace leading 0 with a space.
            @got_units: STA.W ram_menu_text_struct_string + 2
                        LDA.W ram_menu_text_struct_string + 1
                        BNE   @copy_file_name
                        LDA.B #$24              ; Space
                        STA.W ram_menu_text_struct_string + 1

       @copy_file_name: REP   #$20
                        LDA.W ram_menu_draw_file_number
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A                 ; x 16 (== bytes per dir entry)
                        TAY
                        SEP   #$20

                        LDA.B #SRAM_FILE_NAME_LENGTH
                        LDX.W #0
            @copy_char: XBA
                        LDA.W SRAM_DIR_BUFFER,Y
                        ; Text comes after the sequence number and "."
                        STA.W ram_menu_text_struct_string + 4,X
                        INX
                        INY
                        XBA
                        DEC A
                        BNE   @copy_char

                        ; Text structure built in memory, time to draw to WRAM.
                        LDX.W #ram_menu_text_struct_position
                        JSR.W render_text_to_wram

                        ; Set up for the next entry:
                        ; Point to next row on screen.
                        REP   #$20
                        LDA.W ram_menu_text_struct_position
                        CLC
                        ADC.W #SCREEN_WIDTH_B
                        STA.W ram_menu_text_struct_position

                        ; Increment the number of the file to draw.
                        SEP   #$20
                        INC.W ram_menu_draw_file_number
                        PLY
                        INY
                        ; Break if already the last file.
                        CPY.W ram_menu_number_of_files
                        BEQ   @all_entries_draw

                        ; Otherwise draw again while there's still space.
                        CPY.W #SRAM_FILES_ON_SCREEN
                        BNE   @draw_file_entry

                        ; Now all file entries have been draw, copy the whole
                        ; list to VRAM.
     @all_entries_draw: JSR.W render_text_to_vram

                        ; Process controls
          @joypad_loop: LDA.W supercd_joypad1_status_h
                        AND.B #JOYPAD_H_DOWN
                        BEQ   @check_joypad_up

                        ; When "down" pressed...
                        STZ.W ram_menu_joypad_delay_up
                        LDA.W ram_menu_joypad_delay_down
                        INC   A
                        STA.W ram_menu_joypad_delay_down

                        CMP.B #$01
                        BEQ   @scroll_down

                        ; Auto-repeat
                        CMP.B #16
                        BMI   @suppress_joypad_down
                        LDA.W ram_menu_joypad_delay_down
                        AND.B #$1F
                        ORA.B #$10
                        STA.W ram_menu_joypad_delay_down
                        AND.B #$01
                        BNE   @suppress_joypad_down

          @scroll_down: LDA.W ram_menu_selected_file
                        INC   A
                        CLC
                        ADC.W ram_menu_scroll_offset
                        CMP.W ram_menu_number_of_files
                        BEQ   @suppress_joypad_down
                        LDA.W ram_menu_selected_file
                        INC   A
                        CMP.B #SRAM_FILES_ON_SCREEN
                        BEQ   @scroll_down_limit
                        STA.W ram_menu_selected_file
                        BRA   @suppress_joypad_down

    @scroll_down_limit: LDA.W ram_menu_scroll_offset
                        CLC
                        ADC.B #SRAM_FILES_ON_SCREEN
                        CMP.W ram_menu_number_of_files
                        BEQ   @suppress_joypad_down
                        INC.W ram_menu_scroll_offset

 @suppress_joypad_down: JMP.W @break_and_next_frame

      @check_joypad_up: LDA.W supercd_joypad1_status_h
                        AND.B #JOYPAD_H_UP
                        BEQ   @check_joypad_buttons

                        ; When "up" pressed...
                        STZ.W ram_menu_joypad_delay_down
                        LDA.W ram_menu_joypad_delay_up
                        INC   A
                        STA.W ram_menu_joypad_delay_up
                        CMP.B #$01
                        BEQ   @scroll_up

                        ; Auto-repeat
                        CMP.B #16
                        BMI   @suppress_joypad_up
                        LDA.W ram_menu_joypad_delay_up
                        AND.B #$1F
                        ORA.B #$10
                        STA.W ram_menu_joypad_delay_up
                        AND.B #$01
                        BNE   @suppress_joypad_up

            @scroll_up: LDA.W ram_menu_selected_file
                        BEQ   @scroll_up_limit
                        DEC   A
                        STA.W ram_menu_selected_file
                        BRA   @suppress_joypad_up

      @scroll_up_limit: LDA.W ram_menu_scroll_offset
                        BEQ   @suppress_joypad_up
                        DEC.W ram_menu_scroll_offset

   @suppress_joypad_up: JMP.W @break_and_next_frame

 @check_joypad_buttons: STZ.W ram_menu_joypad_delay_down
                        STZ.W ram_menu_joypad_delay_up

                        LDA.W supercd_joypad1_status_h
                        AND.B #JOYPAD_H_Y
                        BNE   @exit_ok                  ; Y pressed - exit menu

                        LDA.W supercd_joypad1_status_h
                        BPL   @unlock_b_button          ; Break if not B pressed

                        ; B pressed, delete file. But only if the lock is free.
                        LDA.W ram_menu_b_button_lock
                        BNE   @to_joypad_loop

                        ; Lock the B button to supress auto-repeat, which would
                        ; quickly delete all our files.
                        LDA.B #$01
                        STA.W ram_menu_b_button_lock
                        JSR.W @delete_file              ; delete this file
                        JMP.W @draw_screen              ; start over - get new dir list and redraw everything

 @break_and_next_frame: JMP.W @next_frame

      @unlock_b_button: STZ.W ram_menu_b_button_lock
       @to_joypad_loop: JMP.W @joypad_loop

              @exit_ok: PLP
                        RTS

             @no_files: ; Display the "no files" message.
                        SEP   #$20
                        LDX.W #data_ram_menu_no_files

                        ; Will jump here if RAM error
      @exit_with_error: JSR.W sub_render_text
                        ; Wait until Y pressed to exit.
           @wait_for_y: LDA.W supercd_joypad1_status_h
                        AND.B #JOYPAD_H_Y
                        BEQ   @wait_for_y
                        PLP
                        RTS

          @delete_file: REP   #$20
                        LDA.W ram_menu_selected_file
                        CLC
                        ADC.W ram_menu_scroll_offset
                        AND.W #$00FF
                        STA.W ram_menu_file_to_delete
                        ASL A
                        ASL A
                        ASL A
                        ASL A                           ; x16 (entry size)
                        ADC.W #SRAM_DIR_BUFFER
                        TAY
                        SEP   #$20
                        JSL.L bios_SramDeleteFile       ; file name passed in Y
                        RTS


   data_ram_menu_title: .DW $002B
                        .DW TEXT_COLOUR_CYAN
                        .DB I18N_LENGTH_TITLE_BACKUP_RAM
                        I18N_TITLE_BACKUP_RAM

  data_ram_menu_legend: .DW $0306
                        .DW TEXT_COLOUR_CYAN
                        .DB I18N_LENGTH_RAM_MENU_LEGEND
                        I18N_RAM_MENU_LEGEND

data_ram_menu_no_files: .DW $00B0 - (I18N_LENGTH_NO_FILES / 2)
                        .DW TEXT_COLOUR_RED
                        .DB I18N_LENGTH_NO_FILES
                        I18N_NO_FILES

   data_ram_menu_error: .DW $00B0 - (I18N_LENGTH_RAM_ERROR / 2)
                        .DW TEXT_COLOUR_RED
                        .DB I18N_LENGTH_RAM_ERROR
                        I18N_RAM_ERROR


.IFDEF ORIGINAL
                        ;; (almost) blank tile data from here to 00:A000
                        ; This doesn't appear to be used for anthing.
                        .ORGA $9511
                        .INCLUDE "padding/bank_00_1.inc"
.ENDIF


;; 16K bytes of data from here until 00:E000, which is the bios function jump
;; table. This whole lot is DMA-ed into VRAM although it's not all tile data,
;; some tile maps and palette data is in here too.

                        .ORGA $A000
gfx_title_screen_tiles: ; Super Disc logo and title screen tile data
                        ; (169 x 8x8 x 4bpp tiles)
                        .INCLUDE "gfx/title.inc"

                        ; Tile map of the title screen background without logo.
tilemap_title_screen:   .DSB SCREEN_WIDTH_W, $01 ; blank
                         ; Yellow/red corners
                        .DB $01,$10,$11,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$12,$13,$01
                        .DB $01,$14,$15,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$16,$17,$01
                        .DSB SCREEN_WIDTH_W, $01 ; blank lines
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        ; Big red "Super Disc" text
                        .DB $01,$01,$01,$01,$01,$01,$01,$01 $68,$69,$01,$01,$01,$01,$01,$01 $01,$6A,$6B,$6C,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01
                        .DB $01,$01,$01,$01,$01,$01,$01,$01 $6D,$6E,$6F,$70,$71,$72,$73,$74 $75,$76,$77,$78,$79,$7A,$7B,$7C $01,$01,$01,$01,$01,$01,$01,$01
                        .DB $01,$01,$01,$01,$01,$01,$01,$01 $7D,$7E,$7F,$80,$81,$82,$83,$84 $01,$85,$86,$87,$88,$89,$8A,$8B $01,$01,$01,$01,$01,$01,$01,$01
                        .DB $01,$01,$01,$01,$01,$01,$01,$01 $8C,$8D,$8E,$8F,$90,$91,$92,$93 $01,$94,$95,$96,$97,$98,$99,$9A $01,$01,$01,$01,$01,$01,$01,$01
                        ; Copyright message
                        .DB $01,$01,$01,$01,$01,$01,$01,$01 $01,$9B,$9C,$9D,$9E,$9F,$01,$01 $A0,$01,$01,$01,$A1,$A2,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01
                        .DB $01,$01,$01,$01,$01,$01,$01,$01 $01,$A3,$A4,$A5,$A6,$A7,$A8,$A9 $AA,$AB,$AC,$AD,$AE,$AF,$B0,$01 $01,$01,$01,$01,$01,$01,$01,$01
                        .DSB SCREEN_WIDTH_W, $01 ; blank lines
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        ; Blue/green corners
                        .DB $01,$B1,$B2,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$B3,$B4,$01
                        .DB $01,$B5,$B6,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$01,$01,$01 $01,$01,$01,$01,$01,$B7,$B8,$01
                        .DSB SCREEN_WIDTH_W, $01 ; blank


                        ; Title screen logo tile map (16 x 11 = 176 bytes)
                        ; Ordered in columns, as this is how it's drawn on the screen.
.IFDEF ORIGINAL
                        .ORGA $BB80
.ENDIF
    tile_map_main_logo: .DB $01,$25,$2B,$32,$38,$3E,$43,$49,$4F,$55,$01
                        .DB $18,$26,$06,$06,$06,$3F,$07,$4A,$06,$06,$5B
                        .DB $19,$06,$2C,$33,$39,$40,$44,$4B,$50,$06,$5C
                        .DB $1A,$06,$2D,$07,$3A,$06,$45,$07,$07,$56,$5D
                        .DB $1B,$06,$2E,$34,$3B,$06,$46,$07,$51,$57,$5E
                        .DB $1C,$27,$06,$35,$3C,$41,$06,$4C,$52,$06,$5F
                        .DB $1D,$28,$2F,$36,$07,$42,$06,$06,$06,$58,$60
                        .DB $1E,$07,$07,$07,$07,$07,$47,$4D,$53,$59,$61
                        .DB $1F,$07,$07,$07,$07,$07,$07,$07,$07,$07,$62
                        .DB $20,$07,$07,$07,$07,$07,$07,$07,$07,$07,$63
                        .DB $21,$07,$07,$07,$07,$07,$07,$07,$07,$07,$64
                        .DB $22,$07,$07,$07,$07,$07,$07,$07,$07,$07,$65
                        .DB $23,$07,$07,$07,$07,$07,$07,$07,$07,$07,$66
                        .DB $24,$29,$07,$07,$07,$07,$07,$07,$07,$07,$67
                        .DB $01,$2A,$30,$07,$07,$07,$07,$07,$07,$5A,$01
                        .DB $01,$01,$31,$37,$3D,$07,$48,$4E,$54,$01,$01

.IFDEF ORIGINAL
                        ; padding
                        .DSB 16, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DB      $80 ; 1-bit anomaly here...
                        .DSB 31, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
.ENDIF

.IFDEF ORIGINAL
                        .ORGA $BF00
.ENDIF
                        ; Palette for title screen background.
                        ;     B             G            R
   data_palette_part_1: .DW ($00 << 10) | ($00 << 5) | ($00)    ; black
                        .DW ($1D << 10) | ($19 << 5) | ($18)    ; light blue
                        .DW ($0B << 10) | ($0E << 5) | ($0E)    ; brown
                        .DW ($0B << 10) | ($1B << 5) | ($1F)    ; yellow
                        .DW ($1F << 10) | ($1F << 5) | ($1F)    ; white
                        .DW ($00 << 10) | ($00 << 5) | ($1D)    ; red
                        .DW ($1D << 10) | ($19 << 5) | ($18)    ; light blue
                        .DW ($1D << 10) | ($0C << 5) | ($00)    ; cyan
                        .DW ($1F << 10) | ($00 << 5) | ($00)    ; blue
                        .DW ($00 << 10) | ($1F << 5) | ($00)    ; green
                        .DW ($00 << 10) | ($00 << 5) | ($00)    ; black
                        .DW ($00 << 10) | ($00 << 5) | ($00)    ; black
                        .DW ($00 << 10) | ($00 << 5) | ($00)    ; black
                        .DW ($00 << 10) | ($00 << 5) | ($00)    ; black
                        .DW ($00 << 10) | ($00 << 5) | ($00)    ; black
                        .DW ($00 << 10) | ($00 << 5) | ($00)    ; black

.IFDEF ORIGINAL
                        ; padding
                        .ORGA $BF20
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
.ENDIF

                        .ORGA $C000
        gfx_font_tiles: ; 256 tiles / 128 characters of font tiles
                        .INCLUDE "gfx/font.inc"

                        .ORGA $D000
    gfx_ram_menu_tiles: ; compressed tile data, 48 tiles total
                        .INCLUDE "gfx/ram_menu.inc"

.IFDEF ORIGINAL
                        ; padding
                        .ORGA $D400
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
                        .DSB 32, $FF
                        .DSB 32, $00
.ENDIF

                        ; 32 x 28 RAM menu screen tile map starts here
.IFDEF ORIGINAL
                        .ORGA $D500
.ENDIF
 data_tilemap_ram_menu: ; Bevels for title
                        .DSB SCREEN_WIDTH_W, $00
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $01
                        .DSB SCREEN_WIDTH_W, $02
                        ; Bordered area for files/error messages.
                        .DB $04,$04,$04,$04,$04,$04  $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$08  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $09,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A,$0B  $04,$04,$04,$04,$04
                        .DB $04,$04,$04,$04,$04,$04  $0C,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$0D,$10  $04,$04,$04,$04,$04
                        ; Blank line
                        .DSB SCREEN_WIDTH_W, $04
                        ; Controller icons:
                        ;             ....joypad....                          yellow button                          green button
                        .DB $04,$04  $04,$12,$13,$04  $04,$04,$04,$04,$04,$04  $14,$15,$04  $04,$04,$04,$04,$04,$04  $16,$17,$04  $04,$04,$04,$04,$04,$04,$04,$04
                        .DB $04,$04  $18,$19,$1A,$1B  $04,$04,$04,$04,$04,$04  $1C,$1D,$1E  $04,$04,$04,$04,$04,$04  $1F,$20,$21  $04,$04,$04,$04,$04,$04,$04,$04
                        .DB $04,$04  $22,$23,$24,$25  $04,$04,$04,$04,$04,$04  $26,$27,$28  $04,$04,$04,$04,$04,$04  $29,$2A,$2B  $04,$04,$04,$04,$04,$04,$04,$04
                        .DB $04,$04  $04,$2C,$2D,$04  $04,$04,$04,$04,$04,$04  $04,$2E,$04  $04,$04,$04,$04,$04,$04  $04,$2F,$04  $04,$04,$04,$04,$04,$04,$04,$04
                        ; Blank line
                        .DSB SCREEN_WIDTH_W, $04

.IFDEF ORIGINAL
                        .ORGA $D880
                        ; This doesn't appear to be used for anthing.
                        .INCLUDE "padding/bank_00_2.inc"
.ENDIF

                        ;
                        ; BIOS function jump table
                        ;
                        .ORGA $E000
       bios_InitDetect: JMP.W cdrom_InitDetect
     bios_LoadFromDisc: JMP.W cdrom_LoadFromDisc
  bios_SendMechaconCmd: JMP.W cdrom_SendMechaconCommand
    bios_WramToVramDMA: JMP.W cdrom_WramToVramDMA
     bios_PollMechacon: JMP.W cdrom_PollMechacon
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
         bios_SramTest: JMP.W cdrom_SramTest
 bios_SramGetDirectory: JMP.W cdrom_SramGetDirectory
     bios_SramSaveFile: JMP.W cdrom_SramSaveFile
     bios_SramLoadFile: JMP.W cdrom_SramLoadFile
   bios_SramDeleteFile: JMP.W cdrom_SramDeleteFile
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
                        JMP.W cdrom_invalidBiosFunction
  bios_DecoderDataMode: JMP.W cdrom_DecoderDataMode
 bios_DecoderAudioMode: JMP.W cdrom_DecoderAudioMode
bios_DecoderTestDecInt: JMP.W cdrom_DecoderTestDecInt

                        ; invalid function handler, just returns
cdrom_invalidBiosFunction:
                        RTL

                        ; Another ACSII description but mentioning a different (older) version and date
                        ; than that at the top of the ROM. Doesn't appear to be used for anything but would
                        ; be easily found here, right after the jump tables.
           title_text2: .DB " Super Disc BIOS program ver.0.93 by Tomomi Abe. May. 26 1992 at SONY. "


; Parameters
;  $1F00-02: Source address (24-bit LBA or MM:SS:FF)
;  $1F03: Read mode flag
;    bit 7    VRAM mode:      0 = write to WRAM, 1 = also copy WRAM to VRAM
;    bit 6    Source address: 0 = MM:SS:FF, 1 = LBA
;    bit 5    ADPCM mode:     0 = No, 1 = Yes - play ADPCM until end
;    bit 4    ADPCM skip:     0 = No, 1 = Skip everything except ADPCM
;    bit 3-0  Unused (0)
;  $1F04-06: Destination address (24-bit WRAM or 16bit VRAM address)
;  $1F07   : Transfer length
;  $1F09   : Max sub-q mismatches allowed
;  $1F33-34: file and channel bytes for ADPCM mode
; Returns
;  A = Error code
;  Carry: 0 = OK, 1 = Error
cdrom_LoadFromDisc:     PHP
                        REP #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        SEP   #$20
                        LDA.B #$00
                        PHA
                        PLB
                        STZ.W supercd_mechacon_error_code

                        LDA.W supercd_read_mode
                        AND.B #SUPERCD_READ_MODE_LBA
                        BEQ   @load_mmssff

                        ; LBA mode - convert to MM:SS:FF
                        LDA.W supercd_src_l
                        CLC
                        ADC.B #$96              ; +150 (skip lead-in)
                        STA.W supercd_lba_convert_l

                        LDA.W supercd_src_m
                        ADC.B #$00              ; carry to middle byte
                        STA.W supercd_lba_convert_m

                        LDA.W supercd_src_h
                        ADC.B #$00              ; carry to high byte
                        STA.W supercd_lba_convert_h

                        LDX.W supercd_lba_convert_m
                        STX.W WRDIVL
                        LDA.B #$4B              ; 75 (frames/sec)
                        STA.W WRDIVB
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        LDA.W RDDIVL            ; division result
                        STA.W supercd_lba_convert_h

                        LDA.W supercd_lba_convert_l
                        STA.W WRDIVL
                        LDA.W RDMPYL            ; division remainder
                        STA.W WRDIVH
                        LDA.B #$4B              ; 75 (frames/sec)
                        STA.W WRDIVB
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        LDA.W RDDIVL            ; division result
                        STA.W supercd_lba_convert_m
                        LDA.W RDMPYL            ; division remainder
                        STA.W supercd_src_ff

                        LDX.W supercd_lba_convert_m
                        STX.W WRDIVL
                        LDA.B #60               ; 60 (sec/min)
                        STA.W WRDIVB
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        NOP
                        LDA.W RDDIVL            ; division result
                        STA.W supercd_src_mm
                        LDA.W RDMPYL            ; division remainder
                        STA.W supercd_src_ss

          @load_mmssff: SEP   #$10
                        ; BCD conversion
                        LDX.W supercd_src_mm
                        LDA.W data_load_cdrom_conv,X
                        STA.W supercd_saved_minutes_bcd

                        LDX.W supercd_src_ss
                        LDA.W data_load_cdrom_conv,X
                        STA.W supercd_saved_seconds_bcd

                        LDX.W supercd_src_ff
                        LDA.W data_load_cdrom_conv,X
                        STA.W supercd_saved_frames_bcd

                        REP   #$10
                        LDA.W supercd_read_mode
                        BPL   @wram_mode          ; bit 7 == VRAM mode

                        ; For VRAM mode
                        LDX.W supercd_transfer_length
                        STX.W supercd_vram_dma_length

                        LDX.W #$0000
                        STX.W supercd_block_buffer_index

                        ; Set destination to WRAM area used for VRAM mode.
                        LDX.W #CDROM_TO_VRAM_BUFFER_ADDRESS
                        STX.W supercd_read_buffer_l
                        LDA.B #CDROM_TO_VRAM_BUFFER_BANK
                        STA.W supercd_read_buffer_b

                        STZ.W supercd_vram_dma_block_count
                        LDA.B #SUPERCD_READ_MODE_VRAM
                        STA.W supercd_block_id
                        LDA.B #$00              ; value for supercd_xfer_flags
                        BRA   @check_adpcm

                        ; For WRAM mode
            @wram_mode: ; Set destination to that passed in parameters
                        LDX.W supercd_dest_addr_l
                        STX.W supercd_read_buffer_l
                        LDA.W supercd_dest_addr_b
                        STA.W supercd_read_buffer_b
                        STZ.W supercd_block_id
                        LDA.B #SUPERCD_READ_MODE_SKIP

          @check_adpcm: STA.W supercd_xfer_flags
                        LDA.W supercd_read_mode
                        AND.B #SUPERCD_READ_MODE_ADPCM
                        BEQ   @load_adpcm
                        LDA.W supercd_xfer_flags
                        ORA.B #$08
                        STA.W supercd_xfer_flags
                        BRA   @check_skip

           @load_adpcm: LDA.W supercd_xfer_flags
                        ORA.B #$02
                        STA.W supercd_xfer_flags

           @check_skip: LDA.W supercd_read_mode
                        AND.B #SUPERCD_READ_MODE_SKIP
                        BEQ   @no_skip

                        LDA.W supercd_xfer_flags
                        ORA.B #(SUPERCD_READ_MODE_ADPCM|SUPERCD_READ_MODE_SKIP)
                        STA.W supercd_xfer_flags

              @no_skip: STZ.W supercd_read_flag

            @loop_seek: JSR.W sub_split_mmssff
                        LDA.B #$08
                        STA.W supercd_failed_seeks_limit
                        LDA.B #75                               ; 75 frames
                        STA.W supercd_read_frame
                        LDA.B #MECHACON_SEEK_MMSSFF
                        JSR.W issue_mechacon_command
                        BCC   @seek_ok
                        JMP.W @exit_error                       ; carry set = seek error - bail out

              @seek_ok: LDA.W supercd_mechacon_final_nibble     ; Usually the final "F"
                        CMP.B #$0B
                        BNE   @last_not_b
                        LDA.B #SUPERCD_ERROR_BAD_RESPONSE       ; last digit was "B" indicating...?
                        JMP.W @pause_and_exit_error             ; pause drive and return with error (c = 1)

           @last_not_b: LDA.W supercd_xfer_flags
                        AND.B #$20
                        BNE   @play
                        LDA.W supercd_xfer_flags
                        ORA.B #$80
                        STA.W supercd_xfer_flags

                 @play: LDA.B #MECHACON_PLAY
                        JSR.W issue_mechacon_command
                        BCC   @play_ok
                        JMP.W @exit_error

              @play_ok: LDA.W supercd_xfer_flags
                        AND.B #$BF
                        STA.W supercd_xfer_flags
          @loop_status: PHP
                        SEP   #$20

          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI
                        PLP
                        LDA.B #MECHACON_REQ_STATUS
                        JSR.W issue_mechacon_command
                        BCC   @status_ok
                        JMP.W @exit_error

            @status_ok: LDA.W supercd_drive_status
                        BNE   @disc_found                  ; 0 == no disc
                        LDA.B #SUPERCD_ERROR_NO_DISC
                        JMP.W @pause_and_exit_error

           @disc_found: CMP.B #MECHACON_STATE_PAUSE
                        BEQ   @loop_seek
                        CMP.B #MECHACON_STATE_ACCESS_TOC
                        BNE   @not_seek_toc
                        LDA.B #MECHACON_STOP
                        JSR.W issue_mechacon_command
                        BCC   @loop_seek
                        JMP.W @exit_error

         @not_seek_toc: LDA.W supercd_disc_track_type
                        AND.B #MECHACON_DISC_TRACK_DATA
                        BNE   @data_track
                        LDA.B #SUPERCD_ERROR_NOT_DATA_TRACK
                        JMP.W @pause_and_exit_error

           @data_track: LDA.W supercd_xfer_flags
                        AND.B #$40
                        BNE   @check_frame
                        BRA   @error_check_flags

                        LDA.W supercd_drive_status
                        CMP.B #MECHACON_STATE_PLAY
                        BNE   @error_check_flags
                        LDA.W supercd_read_flag
                        BEQ   @error_check_flags
                        STZ.W supercd_read_flag
                        LDA.B #MECHACON_REQ_SUB_Q
                        JSR.W issue_mechacon_command
                        BCS   @exit_error

                        LDA.W supercd_minutes_tens_bcd
                        CMP.W supercd_subq_minutes_tens_bcd
                        BMI   @error_bad_subq
                        BNE   @error_check_flags

                        LDA.W supercd_minutes_units_bcd
                        CMP.W supercd_subq_minutes_units_bcd
                        BMI   @error_bad_subq
                        BNE   @error_check_flags

                        LDA.W supercd_seconds_tens_bcd
                        CMP.W supercd_subq_seconds_tens_bcd
                        BMI   @error_bad_subq
                        BNE   @error_check_flags

                        LDA.W supercd_seconds_units_bcd
                        CMP.W supercd_subq_seconds_units_bcd
                        BMI   @error_bad_subq
                        BNE   @error_check_flags

                        LDA.W supercd_frames_tens_bcd
                        CMP.W supercd_subq_frames_tens_bcd
                        BMI   @error_bad_subq
                        BNE   @error_check_flags

          @check_frame: DEC.W supercd_read_frame
                        BNE   @error_check_flags       ; last frame?
                        DEC.W supercd_max_subq_errors
                        BEQ   @error_bad_subq
                        JMP.W @loop_seek

       @error_bad_subq: LDA.B #SUPERCD_ERROR_BAD_SUBQ
                        BRA   @pause_and_exit_error

    @error_check_flags: LDA.W supercd_xfer_flags
                        AND.B #$32
                        CMP.B #$32
                        BEQ   @pause_and_exit
                        JMP.W @loop_status

       @pause_and_exit: STZ.W supercd_xfer_flags
                        STZ.W supercd_mechacon_error_code
                        LDA.B #MECHACON_PAUSE
                        JSR.W issue_mechacon_command
                        BCS   @exit_error
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        CLC
                        RTL

 @pause_and_exit_error: STA.W supercd_mechacon_error_code
                        LDA.B #MECHACON_PAUSE
                        JSR.W issue_mechacon_command

           @exit_error: STZ.W supercd_xfer_flags
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        SEC
                        RTL

                        ; split saved MM:SS:FF into nibbles per byte
      sub_split_mmssff: LDA.W supercd_saved_minutes_bcd
                        LSR A
                        LSR A
                        LSR A
                        LSR A
                        STA.W supercd_minutes_tens_bcd          ; high digit of minute

                        LDA.W supercd_saved_minutes_bcd
                        AND.B #$0F
                        STA.W supercd_minutes_units_bcd         ; low digit of minutes

                        LDA.W supercd_saved_seconds_bcd
                        LSR A
                        LSR A
                        LSR A
                        LSR A
                        STA.W supercd_seconds_tens_bcd          ; high digit of seconds

                        LDA.W supercd_saved_seconds_bcd
                        AND.B #$0F
                        STA.W supercd_seconds_units_bcd         ; low digit of seconds

                        LDA.W supercd_saved_frames_bcd
                        LSR A
                        LSR A
                        LSR A
                        LSR A
                        STA.W supercd_frames_tens_bcd           ; high digit of frames

                        LDA.W supercd_saved_frames_bcd
                        AND.B #$0F
                        STA.W supercd_frames_units_bcd          ; low digit of frames
                        RTS

                        ; lookup table for cdrom_LoadFromDisc
                        ; values 00 to 99 for track/index/minutes BCD conversion
  data_load_cdrom_conv: .DB $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
                        .DB $10,$11,$12,$13,$14,$15,$16,$17,$18,$19
                        .DB $20,$21,$22,$23,$24,$25,$26,$27,$28,$29
                        .DB $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
                        .DB $40,$41,$42,$43,$44,$45,$46,$47,$48,$49
                        .DB $50,$51,$52,$53,$54,$55,$56,$57,$58,$59
                        .DB $60,$61,$62,$63,$64,$65,$66,$67,$68,$69
                        .DB $70,$71,$72,$73,$74,$75,$76,$77,$78,$79
                        .DB $80,$81,$82,$83,$84,$85,$86,$87,$88,$89
                        .DB $90,$91,$92,$93,$94,$95,$96,$97,$98,$99

      bios_nmi_handler: REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        SEP   #$20
                        LDA.B #$00
                        PHA
                        PLB
                        INC.W supercd_mechacon_command_timer
                        LDA.W supercd_xfer_flags
                        BPL   @wait
                        LDA.W supercd_read_mode
                        BPL   @wait
                        JSR.W wram_to_vram_dma

                 @wait: LDA.W HVBJOY
                        AND.B #$01
                        BNE   @wait

                        LDX.W JOY1L
                        STX.W supercd_joypad1_status_l
                        LDX.W JOY2L
                        STX.W supercd_joypad2_status_l
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        RTI


   cdrom_WramToVramDMA: PHP
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        SEP   #$20
                        LDA.B #$00
                        PHA
                        PLB
                        INC.W supercd_mechacon_command_timer
                        LDA.W supercd_xfer_flags
                        BPL   @exit

                        LDA.W supercd_read_mode
                        BPL   @exit

                        JSR.W wram_to_vram_dma

                 @exit: REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        RTL


      wram_to_vram_dma: LDX.W #0
                        STX.W supercd_vram_dma_byte_count

                        ; Find next VRAM mode block in the buffer.
           @next_block: LDA.W supercd_block_buffer,X
                        BPL   @skip_block                       ; Not a VRAM mode block

                        AND.B #($FF~SUPERCD_READ_MODE_VRAM)     ; Remove VRAM flag bit
                        CMP.W supercd_vram_dma_block_count      ; Expected block number?
                        BNE   @skip_block

                        STA.W supercd_block_buffer,X
                        INC.W supercd_vram_dma_block_count
                        REP   #$20
                        LDY.W supercd_vram_dma_byte_count
                        BNE   @continue

                        ; Zero bytes written, so first block.
                        PHA
                        ; Get the destination address for this block.
                        LDA.W wram_to_vram_dma_block_address_table - 1,X
                        AND.W #$FF00
                        STA.W A1T6L
                        PLA

             @continue: LDA.W supercd_vram_dma_byte_count
                        CLC
                        ADC.W #CD_SECTOR_SIZE
                        CMP.W supercd_vram_dma_length
                        BMI   @fits_in_sector

                        LDA.W supercd_vram_dma_length
       @fits_in_sector: STA.W supercd_vram_dma_byte_count
                        SEP   #$20

           @skip_block: ; Advance to next block record in buffer (+4 bytes)
                        INX
                        INX
                        INX
                        INX
                        CPX.W #SUPERCD_BLOCK_BUFFER_SIZE
                        BNE   @next_block

                        ; Nothing more to copy - done.
                        LDY.W supercd_vram_dma_byte_count
                        BEQ   @exit

                        ; Set up the DMA copy.
                        REP   #$20
                        LDA.W supercd_dest_addr_l
                        STA.W VMADDL
                        LDA.W supercd_vram_dma_byte_count
                        LSR A
                        CLC
                        ADC.W supercd_dest_addr_l
                        STA.W supercd_dest_addr_l
                        SEP   #$20
                        LDA.B #(DMAP_CPU_TO_BBUS|DMAP_SINGLE_BYTES)
                        STA.W DMAP6
                        LDA.B #(VMDATAL & $FF)
                        STA.W BBAD6
                        LDA.B #CDROM_TO_VRAM_BUFFER_BANK
                        STA.W A1B6
                        LDX.W supercd_vram_dma_byte_count
                        STX.W DAS6L
                        LDA.B #MDMAEN_CHANNEL6
                        STA.W MDMAEN

                        ; Calculate bytes remaining.
                        REP   #$20
                        LDA.W supercd_vram_dma_length
                        SEC
                        SBC.W supercd_vram_dma_byte_count
                        STA.W supercd_vram_dma_length
                        SEP   #$20
                        BNE   @exit

                        LDA.W supercd_xfer_flags
                        ORA.B #$10                      ; DMA complete??
                        STA.W supercd_xfer_flags

                 @exit: RTS


                        ; Table for `wram_to_vram_dma` containing destination
                        ; addresses for each block, so first block to $C000,
                        ; second to $C8000, etc. Alligned into four byte blocks
                        ; for easy indexing, but referenced as words from the
                        ; byte before the label, hence the one byte offset.
                        ; (The RTS doesn't matter, as the word is ANDed with
                        ; $FF00 before use.)
wram_to_vram_dma_block_address_table:
                        .DB $C0,$00,$00
                        .DW $C800,$0000
                        .DW $D000,$0000
                        .DW $D800,$0000
                        .DW $E000,$0000
                        .DW $E800,$0000
                        .DW $F000,$0000
                        .DW $F800,$0000
                        .DW $0000,$0000
                        .DW $0800,$0000
                        .DW $1000,$0000
                        .DW $1800,$0000
                        .DW $2000,$0000
                        .DW $2800,$0000
                        .DW $3000,$0000
                        .DW $3800,$0000
                        .DB $00


      cdrom_InitDetect: PHP
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        PHK
                        PLB
                        SEP   #$20
                        SEI

                        LDX.W #0			         ; clear/init variables

                        STZ.W supercd_src_mm
                        STZ.W supercd_src_ss
                        STZ.W supercd_src_ff
                        STZ.W supercd_read_mode

                        STX.W supercd_dest_addr_l               ; word, so supercd_dest_addr_m too
                        STZ.W supercd_dest_addr_b
                        STZ.W supercd_transfer_length
                        STZ.W supercd_mechacon_error_code
                        STZ.W supercd_max_subq_errors

                        LDA.B #$80
                        STA.W supercd_not_sure

                        STZ.W supercd_xfer_flags
                        STZ.W supercd_mechacon_command_timer
                        STZ.W supercd_mechacon_command_number
                        STZ.W supercd_mechacon_command_status

                        LDX.W #0
                        STX.W supercd_mechacon_command_position

                        STZ.W supercd_track_h                   ; seek_tr_index
                        STZ.W supercd_track_l                   ; more nibbles of above
                        STZ.W supercd_index_h                   ; more nibbles of above
                        STZ.W supercd_index_l                   ; more nibbles of above

                        STZ.W supercd_minutes_tens_bcd          ; seek_mmssff
                        STZ.W supercd_minutes_units_bcd         ; more nibbles of above
                        STZ.W supercd_seconds_tens_bcd          ; more nibbles of above
                        STZ.W supercd_seconds_units_bcd         ; more nibbles of above
                        STZ.W supercd_frames_tens_bcd           ; more nibbles of above
                        STZ.W supercd_frames_units_bcd          ; more nibbles of above

                        STZ.W supercd_joypad1_status_l          ; joypad status
                        STZ.W supercd_joypad1_status_h
                        STZ.W supercd_joypad2_status_l
                        STZ.W supercd_joypad2_status_h

                        LDX.W #0
                        STX.W supercd_block_buffer_index
          @buffer_loop: STZ.W supercd_block_buffer,X
                        INX
                        CPX.W #SUPERCD_BLOCK_BUFFER_SIZE
                        BNE   @buffer_loop

                        STZ.W supercd_mechacon_error_code
                        STZ.W supercd_max_subq_errors

                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode

                        LDX.W #bios_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #bios_irq_handler
                        STX.W supercd_bios_irq_vector_address

                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank

                        ; Slurp up any stray bytes from the Mechacon until a
                        ; non-negative value is returned.
                        LDX.W #256
        @mechacon_loop: DEX
                        BEQ   @exit_error
                        LDA.W SCD_MECHACON
                        BMI   @mechacon_loop

                        LDA.B #$0E
                        STA.W SCD_MYSTERY

                        LDA.W supercd_nmitimen          ; not overwritten here, so...
                        ORA.B #NMITIMEN_VBLANK          ; ...temporarily enabled
                        STA.W NMITIMEN
                        CLI

                        LDA.B #MECHACON_REQ_STATUS
                        JSR.W issue_mechacon_command
                        BCS   @exit_error
                        LDA.W supercd_nmitimen          ; restore original value
                        STA.W NMITIMEN

                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        CLC
                        CLI
                        RTL

           @exit_error: LDA.B #$00
                        STA.W SCD_MYSTERY
                        LDA.W supercd_nmitimen          ; restore original value
                        STA.W NMITIMEN
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        SEC
                        RTL


      bios_irq_handler: REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20
                        PHB
                        LDA.B #$00
                        PHA
                        PLB
                        LDA.B #CXD_RD_INTSTS
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA
                        AND.B #CXD_INTSTS_DECINT
                        BEQ   @skip_cxd_service
                        JSR.W cxd_service_irq

     @skip_cxd_service: LDA.W SCD_MECHACON
                        BPL   @skip_mechacon_service
                        JSR.W mechacon_service_int

@skip_mechacon_service: REP #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        RTI


    cdrom_PollMechacon: REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20
                        PHB
                        LDA.B #$00
                        PHA
                        PLB
                        LDA.W SCD_MECHACON
                        BPL   @skip_service
                        JSR.W mechacon_service_int

         @skip_service: REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        RTL


       cxd_service_irq: LDA.B #CXD_WR_INTCLR
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_INTCLR_DECINT
                        STA.W SCD_CXD_DATA

                        LDA.W supercd_xfer_flags
                        BEQ   @jump_to_reset_and_exit

                        LDA.B #$FF
                        STA.W supercd_read_flag
                        LDA.B #CXD_RD_STS
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA
                        STA.W supercd_cxd_sts

                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDRFLG
                        STA.W supercd_cxd_hdrflg
                        BNE   @jump_to_reset_and_exit
                                                        ; due to autoincrement, following reads are from...
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_MIN
                        STA.W supercd_cxd_hdr_min
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_SEC
                        STA.W supercd_cxd_hdr_sec
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_BLOCK
                        STA.W supercd_cxd_hdr_block
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_MODE
                        STA.W supercd_cxd_hdr_mode

                        CMP.B #$02
                        BEQ   @handle_mode_2

                        LDA.W supercd_cxd_sts
                        AND.B #CXD_RD_STS_ECCOK         ; Error correction OK
                        BEQ   @jump_to_reset_and_exit

                        LDA.B #CXD_RD_CMADR_L           ; Get buffer address
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA
                        CLC
                        ADC.B #4                        ; Skip header in first 4 bytes
                        JMP.W @get_cmadr_h

@jump_to_reset_and_exit:
                        JMP.W @reset_and_exit

        @handle_mode_2: LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_FILE
                        STA.W supercd_cxd_subheader_file
                        LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_CH
                        STA.W supercd_cxd_subheader_channel
                        LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_S_MODE
                        STA.W supercd_cxd_subheader_submode
                        LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_CI
                        STA.W supercd_cxd_subheader_coding
                        LDA.W supercd_cxd_subheader_submode
                        AND.B #$64
                        CMP.B #$64
                        BNE   @not_adpcm
                        LDA.W supercd_xfer_flags
                        AND.B #$08
                        BEQ   @adpcm_mismatch
                        LDA.W supercd_cxd_subheader_file        ; Compare file
                        CMP.W supercd_adpcm_file_and_channel
                        BNE   @adpcm_mismatch
                        LDA.W supercd_cxd_subheader_channel     ; Compare channel
                        CMP.W supercd_adpcm_file_and_channel + 1
                        BNE   @adpcm_mismatch
                        LDA.W supercd_cxd_subheader_submode
                        AND.B #$81
                        BEQ   @exit_adpcm
                        LDA.W supercd_xfer_flags
                        AND.B #$F7
                        ORA.B #$02
                        STA.W supercd_xfer_flags

           @exit_adpcm: LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_ADPEN         ; ADPCM mode enable
                        STA.W SCD_CXD_DATA
                        RTS

       @adpcm_mismatch: LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #$00                      ; "normal" data mode
                        STA.W SCD_CXD_DATA
                        RTS

            @not_adpcm: LDA.W supercd_cxd_sts
                        AND.B #CXD_RD_STS_ECCOK         ; Error correction OK
                        BEQ   @jump_to_reset_and_exit
                        LDA.W SCD_CXD_DATA
                        CLC
                        ADC.B #12                       ; Skip header and Mode 2 subheader

          @get_cmadr_h: STA.W supercd_cxd_buffer_start_l
                        LDA.W SCD_CXD_DATA              ; CXD_RD_CMADR_H
                        ADC.B #$00                      ; Add any carry due to offset
                        STA.W supercd_cxd_buffer_start_h
                        LDA.W supercd_xfer_flags
                        BPL   @reset_and_exit
                        AND.B #$20
                        BNE   @reset_and_exit
                        LDA.W supercd_read_mode
                        BPL   @check_mmssff
                        LDX.W supercd_block_buffer_index
                        LDA.W supercd_block_buffer,X
                        BMI   @reset_and_exit

         @check_mmssff: LDA.W supercd_cxd_hdr_min
                        CMP.B #$75                      ; BCD minutes
                        BPL   @reset_and_exit
                        LDA.W supercd_cxd_hdr_sec
                        CMP.B #$60                      ; BCD seconds
                        BPL   @reset_and_exit
                        LDA.W supercd_cxd_hdr_block
                        CMP.B #$75                      ; BCD frames
                        BPL   @reset_and_exit
                        LDA.W supercd_cxd_hdr_min
                        CMP.W supercd_saved_minutes_bcd
                        BMI   @reset_and_exit
                        BNE   @mmssff_mismatch
                        LDA.W supercd_cxd_hdr_sec
                        CMP.W supercd_saved_seconds_bcd
                        BMI   @reset_and_exit
                        BNE   @mmssff_mismatch
                        LDA.W supercd_cxd_hdr_block
                        CMP.W supercd_saved_frames_bcd
                        BMI   @reset_and_exit
                        BNE   @mmssff_mismatch
                        BRA   @mmssff_ok

      @mmssff_mismatch: DEC.W supercd_failed_seeks_limit
                        BNE   @reset_and_exit
                        LDA.W supercd_xfer_flags
                        ORA.B #$40
                        STA.W supercd_xfer_flags

       @reset_and_exit: LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        STZ.W SCD_CXD_DATA
                        RTS

            @mmssff_ok: LDA.B #CXD_WR_DMAADRC_L         ; Set DMA address to start of data
                        STA.W SCD_CXD_INDEX
                        LDA.W supercd_cxd_buffer_start_l
                        STA.W SCD_CXD_DATA              ; CXD_WR_DMAADRC_L
                        LDA.W supercd_cxd_buffer_start_h
                        STA.W SCD_CXD_DATA              ; CXD_WR_DMAADRC_H

                        REP   #$20
                        LDA.W supercd_transfer_length
                        CMP.W #CD_SECTOR_SIZE
                        BMI   @dma_read                 ; Round up to 1 sector
                        LDA.W #CD_SECTOR_SIZE

             @dma_read: STA.W DAS7L
                        PHA
                        LDA.W supercd_transfer_length
                        SEC
                        SBC.W DAS7L
                        STA.W supercd_transfer_length
                        PLA
                        SEP   #$20
                        STA.W SCD_CXD_DATA              ; CXD_WR_DMAXFRC_L
                        XBA
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A
                        ORA.B #$08
                        STA.W SCD_CXD_DATA              ; CXD_WR_DMAXFRC_H
                        SEP   #$20
                        LDA.B #DMAP_BBUS_TO_CPU
                        STA.W DMAP7
                        LDA.B #(SCD_CXD_DATA & $FF)
                        STA.W BBAD7
                        LDX.W supercd_read_buffer_l
                        STX.W A1T7L
                        LDA.W supercd_read_buffer_b
                        STA.W A1B7
                        LDA.B #MDMAEN_CHANNEL7
                        STA.W MDMAEN

                        LDA.B #CXD_WR_INTCLR
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_INTCLR_DMACMP        ; DMA complete
                        STA.W SCD_CXD_DATA

                        LDA.W supercd_read_mode
                        BPL   @not_vram_mode
                        LDX.W supercd_block_buffer_index

                        LDA.W supercd_block_id
                        STA.W supercd_block_buffer,X
                        LDA.W supercd_saved_minutes_bcd
                        STA.W supercd_block_buffer + 1,X
                        LDA.W supercd_saved_seconds_bcd
                        STA.W supercd_block_buffer + 2,X
                        LDA.W supercd_saved_frames_bcd
                        STA.W supercd_block_buffer + 3,X

                        INC.W supercd_block_id
                        LDA.W supercd_block_buffer_index
                        CLC
                        ADC.B #SUPERCD_BLOCK_BUFFER_STEP
                        AND.B #SUPERCD_BLOCK_BUFFER_SIZE - 1
                        STA.W supercd_block_buffer_index
                        LDA.W supercd_read_buffer_h
                        CLC
                        ADC.B #(CD_SECTOR_SIZE >> 8)
                        ORA.B #$C0
                        STA.W supercd_read_buffer_h
                        BRA   @check_for_end

        @not_vram_mode: LDA.W supercd_read_buffer_h
                        CLC
                        ADC.B #(CD_SECTOR_SIZE >> 8)
                        STA.W supercd_read_buffer_h

        @check_for_end: LDX.W supercd_transfer_length
                        BNE   @increment_mmssff
                        LDA.W supercd_xfer_flags
                        ORA.B #$20
                        STA.W supercd_xfer_flags

     @increment_mmssff: SED                             ; BCD mode
                        LDA.W supercd_saved_frames_bcd
                        CLC
                        ADC.B #$01
                        STA.W supercd_saved_frames_bcd
                        CMP.B #$75
                        BMI   @exit_ok
                        STZ.W supercd_saved_frames_bcd
                        LDA.W supercd_saved_seconds_bcd
                        CLC
                        ADC.B #$01
                        STA.W supercd_saved_seconds_bcd
                        CMP.B #$60
                        BMI   @exit_ok
                        STZ.W supercd_saved_seconds_bcd
                        INC.W supercd_saved_minutes_bcd

              @exit_ok: CLD                             ; End BCD mode
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        STZ.W SCD_CXD_DATA
                        RTS


  mechacon_service_int: ; A contains byte just read from Mechacon by IRQ handler.
                        AND.B #$0F
                        STA.W supercd_mechacon_last_response_nibble

                        ; At this stage, command position points to the encoded
                        ; response/action byte.
                        LDX.W supercd_mechacon_command_position
                        INX
                        ; Now point to the following command byte that will be
                        ; send to the Mechacon next time.
                        STX.W supercd_mechacon_command_position
                        DEX

                        ; Get the response/action byte.
                        LDA.L data_mechacon_command_table,X
                        ; $80 indicates there's info in response nibble that
                        ; should be saved somewhere.
                        BMI   @save_response

                        ; If the nibble from the Mechacon matches what we were
                        ; expecting, we're done here.
                        CMP.W supercd_mechacon_last_response_nibble
                        BEQ   @done

                        ; Signal a mismatch.
                        LDA.W supercd_mechacon_command_status
                        ORA.B #SUPERCD_MECHACON_COMMAND_UNEXPECTED
                        STA.W supercd_mechacon_command_status
                        BRA   @done

        @save_response: ; Clearing the $80 bit gives the index into the buffer
                        ; at which this nibble should be saved.
                        AND.B #$1F
                        SEP   #$10
                        TAX
                        LDA.W supercd_mechacon_last_response_nibble
                        STA.W supercd_mechacon_response_buffer,X
                        REP #$10

                 @done: LDA.W supercd_mechacon_command_status
                        ; Clear the "waiting" flag.
                        AND.B #($FF~SUPERCD_MECHACON_COMMAND_WAIT_RESPONSE)
                        STA.W supercd_mechacon_command_status
                        JSR.W mechacon_continue_command
                        RTS


  cdrom_SendMechaconCommand:
                        PHP
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        SEP   #$20
                        PHA
                        LDA.B #$00
                        PHA
                        PLB
                        PLA
                        JSR.W issue_mechacon_command
                        BCS   @exit_error

                        ; Exit OK
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        CLC
                        RTL

           @exit_error: REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        SEC
                        RTL


issue_mechacon_command: STA.W supercd_mechacon_command_number
                        LDA.B #SUPERCD_MECHACON_COMMAND_RETRIES
                        STA.W supercd_mechacon_command_retry

                @retry: JSR.W mechacon_start_command

                        ; Command has been triggered, so Mechacon interaction
                        ; is handled by the IRQ handler. Nothing to do but loop
                        ; here until it's completed, an error is detected, or a
                        ; timeout occurs.
                 @loop: LDA.W supercd_mechacon_command_status
                        AND.B #SUPERCD_MECHACON_COMMAND_COMPLETE_UNEXPECTED
                        BNE   @mismatch
                        LDA.W supercd_mechacon_command_timer
                        CMP.B #SUPERCD_MECHACON_COMMAND_TIMEOUT
                        BPL   @timeout
                        LDA.W supercd_mechacon_command_status
                        BMI   @loop     ; While SUPERCD_MECHACON_COMMAND_BUSY

                        ; Exit OK
                        CLC
                        RTS

              @timeout: DEC.W supercd_mechacon_command_retry
                        BEQ   @exit_timeout
                        JSR.W mechacon_flush
                        BCC   @retry

         @exit_timeout: LDA.B #SUPERCD_ERROR_MECHACON_TIMEOUT
                        STA.W supercd_mechacon_error_code
                        SEC
                        RTS

             @mismatch: DEC.W supercd_mechacon_command_retry
                        BEQ   @exit_mismatch
                        JSR.W mechacon_flush
                        BCC   @retry

        @exit_mismatch: LDA.B #SUPERCD_ERROR_MECHACON_MISMATCH
                        STA.W supercd_mechacon_error_code
                        SEC
                        RTS


        mechacon_flush: LDA.W supercd_mechacon_command_number
                        PHA
                        LDA.B #MECHACON_FLUSH
                        STA.W supercd_mechacon_command_number
                        JSR.W mechacon_start_command

                 @loop: LDA.W supercd_mechacon_command_timer
                        CMP.B #SUPERCD_MECHACON_COMMAND_TIMEOUT
                        BPL   @exit_error
                        LDA.W supercd_mechacon_command_status
                        BMI   @loop

                        ; Exit OK
                        PLA
                        STA.W supercd_mechacon_command_number
                        CLC
                        RTS

           @exit_error: PLA
                        STA.W supercd_mechacon_command_number
                        SEC
                        RTS


; Two entry points to this subroutine...
mechacon_start_command: LDA.B #SUPERCD_MECHACON_COMMAND_BUSY
                        STA.W supercd_mechacon_command_status
                        REP   #$20
                        ; Point supercd_mechacon_command_position to the first
                        ; byte in the command table entry for this command number.
                        LDA.W supercd_mechacon_command_number
                        AND.W #$00FF
                        ASL A
                        TAX
                        LDA.L data_mechacon_command_table,X
                        STA.W supercd_mechacon_command_position
                        SEP   #$20
; Second entry point here
mechacon_continue_command:
                        LDX.W supercd_mechacon_command_position
                        INX
                        STX.W supercd_mechacon_command_position ; Now points to encoded response/action.
                        DEX
                        LDA.L data_mechacon_command_table,X     ; Get the command nibble
                        BPL   @send_nibble                      ; "Regular" (constant) command nibble.

                        CMP.B #$FF                              ; Signals no more nibbles to send
                        BEQ   @command_complete

                        SEP   #$10
                        AND.B #$0F                              ; Lower nibble of command byte is position of parameter.
                        TAX
                        LDA.W supercd_comamnd_paramter_buffer,X ; Get command parameter at offset X
                        REP   #$10

          @send_nibble: STA.W SCD_MECHACON
                        STZ.W supercd_mechacon_command_timer
                        LDA.W supercd_mechacon_command_status
                        ORA.B #SUPERCD_MECHACON_COMMAND_WAIT_RESPONSE
                        STA.W supercd_mechacon_command_status
                        BRA   @return

     @command_complete: LDA.W supercd_mechacon_command_status
                        AND.B #SUPERCD_MECHACON_COMMAND_UNEXPECTED
                        BEQ   @exit_complete
                        ORA.B #SUPERCD_MECHACON_COMMAND_COMPLETE_UNEXPECTED

        @exit_complete: ; Clear any error flags.
                        AND.B #($FF ~ (SUPERCD_MECHACON_COMMAND_BUSY|SUPERCD_MECHACON_COMMAND_UNEXPECTED))
                        STA.W supercd_mechacon_command_status

               @return: RTS


                        ; Mechacon command table
data_mechacon_command_table:
                        ; First, offsets from here to the beginning of the corresponding command string.
                        .DW $0028  ; 00
                        .DW $0035  ; 01
                        .DW $0046  ; 02
                        .DW $004F  ; 03
                        .DW $0058  ; 04
                        .DW $0061  ; 05
                        .DW $006A  ; 06
                        .DW $0073  ; 07
                        .DW $007C  ; 08
                        .DW $0085  ; 09
                        .DW $00A0  ; 0A
                        .DW $00A9  ; 0B
                        .DW $00B2  ; 0C
                        .DW $00BB  ; 0D
                        .DW $00C4  ; 0E
                        .DW $00DF  ; 0F
                        .DW $010A  ; 10
                        .DW $00CD  ; 11
                        .DW $00D6  ; 12
                        .DW $011F  ; 13

                        ; Now the command "strings" as pointed to from above. Pairs of bytes contain
                        ; the nibble to send to the Mechacon, followed by the expected response.
                        ; Both of these values can however be encoded by setting $80 in said byte.
                        ;
                        ; For command nibbles (i.e. those sent to the Mechacon) the $80 indicates that
                        ; the nibble is part of a parameter and should be fetched from memory. Masking
                        ; off $80 gives the offset into this "parameter buffer", so the parameters for
                        ; e.g. BxxxxxxF (seek mm:ss:ff) are fetched from offsets 4-9 (encoded $84-$89).
                        ;
                        ; Setting $80 in corresponding response nibble indictaes that this should be
                        ; saved to a buffer. Again, masking off the $80 masked gives the offset into
                        ; that buffer. This means that the response from e.g. "D51FxxxxxF" (status
                        ; request) gets written to bytes 0-4 of the buffer (encoded $80-$84). For all
                        ; commands except "F" the final nibble of the response is saved at offset $15
                        ; (encoded to $95) so this can easily be checked for, regardless of the length
                        ; of the command.
                        ;
                        ; These pairs of command/response bytes are terminated by a single $FF.

                        ;     C       x       x       x       x       F
                        .DB $0C,$0F,$80,$0F,$81,$0F,$82,$0F,$83,$0F,$0F,$95
                        .DB $FF

                        ;     B       x       x       x       x       x       x       F
                        .DB $0B,$0F,$84,$0F,$85,$0F,$86,$0F,$87,$0F,$88,$0F,$89,$0F,$0F,$95
                        .DB $FF

                        ;     D       0       x       F
                        .DB $0D,$0F,$00,$0F,$01,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$00,$0F,$02,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$00,$0F,$03,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$00,$0F,$04,$0F,$0F,$95
                        .DB $FF

                        ;     D       1       x       F
                        .DB $0D,$0F,$01,$0F,$00,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$01,$0F,$01,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$01,$0F,$02,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$01,$0F,$03,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$01,$0F,$04,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$01,$0F,$05,$0F,$0F,$95
                        .DB $FF

                        ;     D       4       x       F
                        .DB $0D,$0F,$04,$0F,$00,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$04,$0F,$01,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$04,$0F,$02,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$04,$0F,$03,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$04,$0F,$04,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$04,$0F,$05,$0F,$0F,$95
                        .DB $FF
                        .DB $0D,$0F,$04,$0F,$06,$0F,$0F,$95
                        .DB $FF

                        ;     D       5       0       F
                        .DB $0D,$0F,$05,$0F,$00,$0F,$0F,$0F
                        .DB $00,$85,$00,$86,$00,$87,$00,$88
                        .DB $00,$89,$00,$8A,$00,$8B,$00,$8C
                        .DB $00,$8D,$00,$8E,$00,$8F,$00,$90
                        .DB $00,$91,$00,$92,$00,$93,$00,$94
                        .DB $0F,$95
                        .DB $FF

                        ;     D       5       1       F
                        .DB $0D,$0F,$05,$0F,$01,$0F,$0F,$0F
                        .DB $00,$80,$01,$81,$02,$82,$03,$83
                        .DB $04,$84,$0F,$95
                        .DB $FF

                        ;     F
                        .DB $0F,$0A
                        .DB $FF

cdrom_DecoderTestDecInt:
                        PHP
                        REP   #$30
                        PHA
                        PHX
                        SEP   #$20
                        REP   #$10

                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_CHPRST
                        STA.W SCD_CXD_DATA

                        LDA.B #CXD_WR_DECCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_DECCTL_AUTODIST
                        STA.W SCD_CXD_DATA

                        LDX.W #0
         @loop_vectors: LDA.W supercd_bios_vector_table,X
                        PHA
                        INX
                        CPX.W #VECTOR_TABLE_SIZE
                        BNE   @loop_vectors

                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode

                        LDX.W #decint_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #decint_irq_hanlder
                        STX.W supercd_bios_irq_vector_address

                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank

                        LDA.B #$04
                        STA.W SCD_MYSTERY
                        LDA.B #CXD_WR_DECCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #(CXD_DECCTL_AUTODIST|CXD_DECCTL_DECMD_RTC)
                        STA.W SCD_CXD_DATA
                        LDA.B #CXD_INTMSK_DECINT
                        STA.W SCD_CXD_DATA  ; to CXD_WR_INTMSK

                        TSX
                        STX.W supercd_saved_stack_pointer
                        LDX.W #0
                        LDY.W #0
                        LDA.B #NMITIMEN_VBLANK
                        STA.W NMITIMEN
                        CLI

                        ; loop waiting for interrupt
             @nop_loop: NOP
                        NOP
                        BRA   @nop_loop


                        ; NMI handler for TestDecint
    decint_nmi_handler: ; Time one second.
                        INY
                        CPY.W #FPS
                        BEQ   @second_up
                        RTI

            @second_up: SEI                     ; disable interrupts
                        STZ.W NMITIMEN          ; on PPU as well

                        REP   #$20
                        TXA
                        LDX.W supercd_saved_stack_pointer
                        TXS
                        CMP.W #70               ; Single speed (75 +/- 5)
                        BMI   @exit_error
                        CMP.W #80
                        BMI   @exit_ok
                        CMP.W #140              ; Single speed (150 +/- 10)
                        BMI   @exit_error
                        CMP.W #160
                        BPL   @exit_error

              @exit_ok: SEP   #$20
                        LDX.W #(VECTOR_TABLE_SIZE - 1)
              @ok_loop: PLA
                        STA.W supercd_bios_vector_table,X
                        DEX
                        BPL   @ok_loop

                        REP   #$30
                        PLX
                        PLA
                        PLP
                        CLC
                        RTL

           @exit_error: SEP   #$20
                        LDX.W #(VECTOR_TABLE_SIZE - 1)
           @error_loop: PLA
                        STA.W supercd_bios_vector_table,X
                        DEX
                        BPL   @error_loop

                        REP   #$30
                        PLX
                        PLA
                        PLP
                        SEC
                        RTL


                        ; IRQ handler for Test Decint
    decint_irq_hanlder: LDA.B #CXD_RD_INTSTS
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA
                        AND.B #CXD_INTSTS_DECINT

                        BEQ   @exit

                        LDA.B #CXD_WR_INTCLR    ; Ack/clear
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_INTCLR_DECINT
                        STA.W SCD_CXD_DATA

                        INX

                 @exit: RTI


 cdrom_DecoderDataMode: PHP
                        SEP   #$20
                        PHA
                        PHB
                        PHK
                        PLB

                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #$00
                        STA.W SCD_CXD_DATA

                        LDA.B #CXD_WR_DECCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #(CXD_DECCTL_AUTOCI|CXD_DECCTL_AUTODIST|CXD_DECCTL_DECMD_RTC)
                        STA.W SCD_CXD_DATA

                        LDA.B #CXD_INTMSK_DECINT
                        STA.W SCD_CXD_DATA              ; CXD_WR_INTMSK
                        PLB
                        PLA
                        PLP
                        RTL


cdrom_DecoderAudioMode: PHP
                        SEP   #$20
                        PHA
                        PHB
                        PHK
                        PLB
                        SEI
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_CD_DA         ; audio mode
                        STA.W SCD_CXD_DATA
                        LDA.B #$00                      ; Disable decoder interrupts
                        STA.W SCD_CXD_DATA              ; CXD_WR_DECCTL
                        PLB
                        PLA
                        PLP
                        RTL

; Tests checksum, free memory, number of files.
; Automatically reinitialises SRAM on error.
; Returns
;   Carry: 0 = OK, 1 = error
        cdrom_SramTest: PHP
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        PHK
                        PLB
                        REP   #$20

                        ; check free space is valid
                        LDA.L (SRAM_BANK << 16 | SRAM_FREE_SPACE)
                        BMI   @exit_error               ; < zero
                        CMP.W #(SRAM_SIZE - 3)
                        BPL   @exit_error               ; > max available
.IFNDEF ORIGINAL
                        ; Remember for later
                        TAX
.ENDIF
                        ; check number of files
                        SEP   #$20
                        LDA.L (SRAM_BANK << 16 | SRAM_NUMBER_OF_FILES)
                        CMP.B #SRAM_MAX_FILES+1
                        BPL   @exit_error               ; too many files
.IFNDEF ORIGINAL
                        ; Check for special case of zero files and zero free
                        ; space, in which case SRAM must be formatted.
                        CMP.B #0
                        BNE   @skip
                        REP   #$20
                        TXA
                        SEP   #$20
                        BEQ   @exit_error
                 @skip:
.ENDIF
                        JSR.W sram_verify_checksum
                        BCS   @exit_error               ; bad checksum

                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        CLC                             ; no error
                        RTL

           @exit_error: ; initialise SRAM
                        SEP   #$20
                        SEI
                        JSR.W sram_unlock

                        ; set free space to 8K - 4 bytes (reserved for vars)
                        LDA.B #$FC
                        STA.L (SRAM_BANK << 16 | SRAM_FREE_SPACE)
                        LDA.B #$1F
                        STA.L (SRAM_BANK << 16 | SRAM_FREE_SPACE)+1

                        ; zero files
                        LDA.B #$00
                        STA.L (SRAM_BANK << 16 | SRAM_NUMBER_OF_FILES)

                        JSR.W sram_update_checksum

                        STZ.W SCD_SRAM_LOCK
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        SEC                             ; flag error
                        RTL

; Parameters
;   Y: destination address for directory data ($200 bytes)
; Returns
;   Carry: 0 = OK, 1 = error
;   A: number of files in use
cdrom_SramGetDirectory: .DEFINE tmp_cdrom_SramGetDirectory_number_of_files $001F00

                        PHP
                        REP #$30
                        PHX
                        PHY
                        SEP   #$20

                        JSR.W sram_verify_checksum
                        BCS   @exit_error               ; bad checksum

                        LDA.L (SRAM_BANK << 16 | SRAM_NUMBER_OF_FILES)
                        BMI   @exit_error               ; < 0 files

                        CMP.B #SRAM_MAX_FILES+1
                        BPL   @exit_error               ; too many files

                        LDA.B #$00
                        STA.L tmp_cdrom_SramGetDirectory_number_of_files
                        LDX.W #$0004

           @loop_entry: LDA.L $001F00
                        CMP.L (SRAM_BANK << 16 | SRAM_NUMBER_OF_FILES)
                        BEQ   @break
                        INC   A
                        STA.L tmp_cdrom_SramGetDirectory_number_of_files
                        PHX

                        LDA.B #$0E
            @loop_char: XBA
                        LDA.L (SRAM_BANK << 16 | SRAM_START),X
                        STA.W $0000,Y
                        INX
                        INY
                        XBA
                        DEC   A
                        BNE   @loop_char

                        REP   #$20
                        LDA.L (SRAM_BANK << 16 | SRAM_START),X  ; store file size
                        STA.W $0000,Y

                        PLX
                        TXA
                        CLC
                        ADC.W $0000,Y
                        ADC.W #SRAM_DIR_ENTRY_LENGTH            ; next entry
                        TAX
                        SEP   #$20
                        INY
                        INY
                        BRA   @loop_entry

                @break: ; fill any remaining entries with zeros
       @loop_remaining: LDA.L tmp_cdrom_SramGetDirectory_number_of_files
                        CMP.B #SRAM_MAX_FILES
                        BEQ   @exit_ok
                        INC   A
                        STA.L tmp_cdrom_SramGetDirectory_number_of_files

                        LDA.B #$00              ; pad with zeros
                        LDX.W #SRAM_DIR_ENTRY_LENGTH
         @loop_padding: STA.W $0000,Y
                        INY
                        DEX
                        BNE   @loop_padding
                        BRA   @loop_remaining

              @exit_ok: LDA.L (SRAM_BANK << 16 | SRAM_NUMBER_OF_FILES)
                        REP   #$30
                        PLY
                        PLX
                        PLP
                        CLC
                        RTL

           @exit_error: LDA.W #$C200
                        BMI   cdrom_SramSaveFile@defrag
                        PLX
                        PLP
                        SEC
                        RTL

; Parameters:
;   Y: Pointer to file name (14 bytes), length (2 bytes), data address (2 bytes)
; Returns
;   Carry: 0 = OK, 1 = file not found
    cdrom_SramSaveFile: .DEFINE tmp_cdrom_SramSaveFile_space_required $001F01

                        PHP
                        REP   #$30
                        PHX
                        PHY
                        PHB
                        REP   #$20
                        LDA.W SRAM_FILE_NAME_LENGTH,Y                   ; requested file size
                        CLC
                        ADC.W #SRAM_DIR_ENTRY_LENGTH                    ; add size of directory entry
                        STA.L tmp_cdrom_SramSaveFile_space_required
                        SEP   #$20
                        SEI
                        JSR.W sram_unlock
                        JSR.W sram_find_file
                        BCS   @not_found

                        ; file exists
                        LDA.B #SRAM_BANK
                        PHA
                        PLB
.IFDEF ORIGINAL
                        ; File exists, so it makes no sense to fail if the
                        ; maximum number of files has already been reached.
                        LDA.W SRAM_NUMBER_OF_FILES
                        CMP.B #SRAM_MAX_FILES
                        BPL   @exit_error                               ; no slots available
.ENDIF
                        REP   #$20
                        LDA.L tmp_cdrom_SramSaveFile_space_required
                        CMP.W SRAM_START+SRAM_FILE_NAME_LENGTH,X        ; size of existing file
                        BEQ   @do_write                                 ; same size, simple overwrite

                        LDA.W SRAM_FREE_SPACE                           ; check for enough space
                        CLC
                        ADC.W SRAM_START+SRAM_FILE_NAME_LENGTH,X        ; size of existing file (counts as free here)
                        ADC.W #SRAM_DIR_ENTRY_LENGTH                    ; size of directory entry
                        SEC
                        SBC.L tmp_cdrom_SramSaveFile_space_required
                        BCC   @exit_error                               ; not enough space

                        STA.W SRAM_FREE_SPACE
                        PHX
                        PHY
                        TXA
                        CLC
                        ADC.W SRAM_START+$0E,X                          ; add file size
                        ADC.W #SRAM_DIR_ENTRY_LENGTH                    ; size of directory entry
                        TAY
                        TXA
                        BRA   @find_end

            @not_found: LDA.B #SRAM_BANK
                        PHA
                        PLB
.IFNDEF ORIGINAL
                        ; More logical place to check if maximum number of files
                        ; has been reached.
                        LDA.W SRAM_NUMBER_OF_FILES
                        CMP.B #SRAM_MAX_FILES
                        BPL   @exit_error                               ; no slots available
.ENDIF
                        REP   #$20
                        LDA.W SRAM_FREE_SPACE
                        SEC
                        SBC.L tmp_cdrom_SramSaveFile_space_required
                        BCC   @exit_error

                        STA.W SRAM_FREE_SPACE
                        INC.W SRAM_NUMBER_OF_FILES
                        PHX
                        PHY
                        TXA
                        TAY
             @find_end: CLC
                        ADC.L tmp_cdrom_SramSaveFile_space_required
                        TAX

               @defrag: JSR.W sram_defrag
                        PLY
                        PLX

             @do_write: PLB
                        REP   #$20
                        LDA.L tmp_cdrom_SramSaveFile_space_required
                        BRA   @check_complete

                 @loop: PHA
                        SEP   #$20
                        LDA.W $0000,Y
                        STA.L (SRAM_BANK << 16 | SRAM_START),X
                        INX
                        INY
                        REP   #$20
                        PLA
                        DEC   A
       @check_complete: CMP.W #0
                        BNE   @loop

                        SEP   #$20
                        JSR.W sram_update_checksum
                        STZ.W SCD_SRAM_LOCK
                        REP   #$30
                        PLY
                        PLX
                        PLP
                        CLC
                        RTL

           @exit_error: PLB
                        SEP   #$20
                        STZ.W SCD_SRAM_LOCK
                        REP   #$30
                        PLY
                        PLX
                        PLP
                        SEC
                        RTL

; Shuffle data around in SRAM to make it contiguous
           sram_defrag: PHP
                        PHX
                        PHY
                        REP   #$20
                        TYA                     ; compare X to Y
                        STA.L $001F03
                        TXA
                        CMP.L $001F03
                        BEQ   @exit             ; same? done
                        BPL   @setup_move_down  ; X < Y

                        ; X > Y : move data up
                        SEP   #$20
         @loop_copy_up: LDA.W SRAM_START,Y
                        STA.W SRAM_START,X
                        INX
                        INY
                        CPX.W #SRAM_SIZE
                        BNE   @loop_copy_up
                        BRA   @exit

                        ; X < Y : move data down
      @setup_move_down: LDA.W #SRAM_SIZE
                        SEC
                        SBC.L $001F03
                        STA.L $001F03
                        TXA
                        CLC
                        ADC.L $001F03
                        TAX
                        LDY.W #SRAM_SIZE
                        LDA.L $001F03
       @loop_copy_down: PHA
                        DEX
                        DEY
                        SEP   #$20
                        LDA.W SRAM_START,Y
                        STA.W SRAM_START,X
                        REP   #$20
                        PLA
                        DEC   A
                        BNE   @loop_copy_down

                 @exit: PLY
                        PLX
                        PLP
                        RTS

; Parameters:
;   Y: Pointer to file name (14 bytes), length (2 bytes), destination address (2 bytes)
; Returns
;   Carry: 0 = OK, 1 = file not found
    cdrom_SramLoadFile: PHP
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20
                        JSR.W sram_find_file
                        BCS   @exit_not_found
                        REP   #$20
                        LDA.W SRAM_FILE_NAME_LENGTH,Y           ; file size
                        SEC
                        SBC.L (SRAM_BANK << 16 | SRAM_START)+$0E,X
                        BCC   @skip_size

                        LDA.W #0
            @skip_size: PHA
                        LDA.W $000E,Y
                        CMP.L (SRAM_BANK << 16 | SRAM_START)+$0E,X
                        BMI   @copy_data
                        LDA.L (SRAM_BANK << 16 | SRAM_START)+$0E,X

            @copy_data: PHA
                        SEP   #$20
                        LDA.L (SRAM_BANK << 16 | SRAM_START)+$10,X
                        STA.W $0010,Y
                        INX
                        INY
                        REP   #$20
                        PLA
                        DEC A
                        BNE   @copy_data
                        PLX
                        SEP   #$20

                        LDA.B #$00              ; pad rest with zeros
              @padding: CPX.W #0
                        BEQ   @exit_ok
                        DEX
                        STA.W $0010,Y
                        INY
                        BRA   @padding

              @exit_ok: REP #$30
                        PLY
                        PLX
                        PLA
                        PLP
                        CLC
                        RTL

       @exit_not_found: REP #$30
                        PLY
                        PLX
                        PLA
                        PLP
                        SEC
                        RTL


  cdrom_SramDeleteFile: PHP
                        REP   #$30
                        PHX
                        PHY
                        PHB
                        SEP   #$20
                        JSR.W sram_find_file
                        BCS   @not_found

                        ; found  - continue
                        SEI
                        JSR.W sram_unlock

                        LDA.B #$90                      ; bank $90
                        PHA
                        PLB
                        REP   #$20
                        TXA
                        CLC
                        ADC.W SRAM_START+SRAM_FILE_NAME_LENGTH,X
                        CLC
                        ADC.W #SRAM_DIR_ENTRY_LENGTH
                        TAY

                        ; update free space
                        LDA.W SRAM_FREE_SPACE
                        CLC
                        ADC.W SRAM_START+SRAM_FILE_NAME_LENGTH,X          ; file size
                        CLC
                        ADC.W #SRAM_DIR_ENTRY_LENGTH    ; next entry
                        STA.W SRAM_FREE_SPACE

                        JSR.W sram_defrag

                        SEP   #$20
                        DEC.W SRAM_NUMBER_OF_FILES

                        JSR.W sram_update_checksum
                        STZ.W SCD_SRAM_LOCK
                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLP
                        CLC
                        RTL

            @not_found: REP #$30
                        PLB
                        PLY
                        PLX
                        PLP
                        SEC
                        RTL


  sram_update_checksum: PHA
                        PHX
                        LDA.B #$00
                        STA.L (SRAM_BANK << 16 | SRAM_CHECKSUM)

                        LDX.W #0
                 @loop: CLC
                        ADC.L (SRAM_BANK << 16 | SRAM_START),X
                        INX
                        CPX.W #SRAM_SIZE
                        BNE   @loop

                        EOR.B #$FF
                        INC   A
                        STA.L (SRAM_BANK << 16 | SRAM_CHECKSUM)
                        PLX
                        PLA
                        RTS


  sram_verify_checksum: PHA
                        PHX

                        LDA.B #$00
                        LDX.W #$0000
                 @loop: CLC
                        ADC.L (SRAM_BANK << 16 | SRAM_START),X
                        INX
                        CPX.W #SRAM_SIZE
                        BNE   @loop

                        CMP.B #$00
                        BNE   @exit_error

              @exit_ok: PLX
                        PLA
                        CLC
                        RTS

           @exit_error: PLX
                        PLA
                        SEC
                        RTS


        sram_find_file: LDA.B #$00
                        STA.L $001F00
                        LDX.W #$0004

                 @loop: LDA.L $001F00
                        CMP.L (SRAM_BANK << 16 | SRAM_NUMBER_OF_FILES)
                        BEQ   @exit_not_found
                        INC   A
                        STA.L $001F00
                        PHX
                        PHY

                        LDA.B #SRAM_FILE_NAME_LENGTH
       @loop_character: PHA
                        LDA.L (SRAM_BANK << 16 | SRAM_START),X
                        CMP.W $0000,Y
                        BEQ   @match
                        BMI   @next_file
                        BRA   @exit_not_found_with_stack

                @match: INX
                        INY
                        PLA
                        DEC   A
                        BNE   @loop_character

                        ; exit - found
                        PLY
                        PLX
                        CLC
                        RTS

            @next_file: PLA
                        PLY
                        PLX
                        REP   #$20
                        TXA
                        CLC
                        ADC.L (SRAM_BANK << 16 | SRAM_START)+$0E,X
                        CLC
                        ADC.W #$0010
                        TAX
                        SEP   #$20
                        BRA   @loop

@exit_not_found_with_stack:
                        PLA
                        PLY
                        PLX

       @exit_not_found: SEC
                        RTS


           sram_unlock: PHA
                        LDA.B #$FF
                        STA.L $0021E5
                        LDA.B #$0F

                 @loop: STA.L $0021E0
                        DEC A
                        BNE @loop
                        PLA
                        RTS

.IFDEF ORIGINAL
                        ; Data until header and vector table looks like old tiles or padding data
                        ; doesn't seem to be referrenced anywhere.
                        .ORGA $EE5A
                        .INCLUDE "padding/bank_00_3.inc"
.ENDIF


                        .ORGA $F800
                        ; APU data used during self test (from bank $01).
                        ; Each block:
                        ;   bytes 0-1: length
                        ;   bytes 2-3: address
                        ;   bytes 4+ : data
self_test_sound_apu_data:
                        .DW $0001
                        .DW $0080
                        .DB $00

                        .DW $00C6
                        .DW $0200
                        .DB $CD,$7F,$BD,$3F,$35,$02,$E5,$F4,$00,$C4,$80,$E5,$F4,$00,$64,$80
                        .DB $F0,$F9,$C4,$80,$E8,$5C,$C5,$F2,$00,$E8,$FF,$C5,$F3,$00,$E8,$FF
                        .DB $9C,$D0,$FD,$E8,$00,$C5,$F3,$00,$E8,$4C,$C5,$F2,$00,$E4,$80,$C5
                        .DB $F3,$00,$2F,$D7,$0F,$8D,$80,$DC,$30,$0B,$CC,$F2,$00,$F6,$46,$02
                        .DB $C5,$F3,$00,$2F,$F2,$6F,$10,$10,$00,$08,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$7F,$00,$00,$00,$10,$10,$00,$0A,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$7F,$00,$00,$00,$10,$10,$00,$0C,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$10,$10,$00,$10,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$10,$10,$00,$14,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$10,$10,$00,$18,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$00,$3C,$00,$00,$10,$10,$00,$20,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$20,$E0,$00,$00,$10,$10,$00,$28,$00,$FF,$E0,$7F,$00,$00
                        .DB $00,$00,$00,$00,$00,$00

                        .DW $0010
                        .DW $3C00
                        .DB $00,$40,$2D,$40,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

                        .DW $0010
                        .DW $3E00
                        .DB $00,$FF,$E0,$B8,$04,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

                        .DW $0060
                        .DW $4000
                        .DB $02,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$15,$10,$21,$11,$11,$11
                        .DB $22,$11,$7A,$44,$22,$22,$32,$11,$21,$10,$0F,$6A,$0F,$ED,$DC,$CC
                        .DB $BB,$BB,$AA,$AA,$6A,$BA,$BA,$CB,$CC,$DD,$EE,$FF,$00,$6A,$12,$23
                        .DB $33,$45,$54,$56,$66,$66,$6A,$55,$56,$55,$44,$32,$22,$21,$0F,$6A
                        .DB $FF,$ED,$DC,$CC,$BB,$BB,$AA,$AA,$6B,$AB,$BB,$BB,$CC,$DD,$EE,$FF
                        .DB $00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

                        .DW $0000
                        .DW $0200

.IFDEF ORIGINAL
                        ; Data until header and vector table looks like old tiles or padding data
                        ; doesn't seem to be referrenced anywhere.
                        .ORGA $F95F
                        .INCLUDE "padding/bank_00_4.inc"
.ENDIF

.IFNDEF SNES_HEADER
                        ; Original image has an unusual header, filled entirely with $FF.
                        ; Following code will provide that (plus a warning when assembled).
                        .ORGA $FFC0
                        .DSB 32, $FF
.ENDIF
