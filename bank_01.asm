;
; Bank $01 program: "secret" diagnostics menu accessed with A+X
;
; This bank is split into four equal sections:
;
;   8000 - main menu + self test
;   A000 - CXD1800
;   E000 - COMMUNICATION (Mechacon)
;   C000 - ADPCM (also contains electronic book screen data used in bank $00)
;
; Each is pretty much a stand-alone program so there's a lot of duplicate code,
; much of which will look very familiar if you've already seen bank $00. They
; do however all use the same font tiles straight out of bank $00.
;

;
; Main "MONITOR MENU" routine.
;
       diagnostic_menu: REP   #$10
                        SEP   #$20

                        ; Disable interrupts
                        SEI
                        STZ.W NMITIMEN

                        ; Blank the screen
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP

                        ; Set NMI interrupt handler
                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        LDX.W #diag_menu_handler
                        STX.W supercd_bios_nmi_vector_address
                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        PHK
                        PLB

                        ; Set offset to variables with 8-bit adresses.
                        REP   #$20
                        LDA.W #(diag_joy1l_previous & $FF00)
                        TCD
                        SEP   #$20

                        ; Draw main menu to screen.
                        JSR.W diag_init_graphics
                        LDX.W #data_tilemap_main_menu
                        JSR.W diag_draw_screen_text

                        ; Initialise variables
                        STZ.B (diag_joy1l_previous & $FF)
                        STZ.B (diag_joy1h_previous & $FF)
                        STZ.B (diag_joy1l & $FF)
                        STZ.B (diag_joy1h & $FF)
                        STZ.B (diag_menu_position & $FF)

                        JSR.W diag_highlight_menu

                        ; Enable interrupts and un-blank the screen
                        LDA.B #(NMITIMEN_VBLANK | NMITIMEN_JOYPAD)
                        STA.W NMITIMEN
                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP

                        ; Main loop: check controls and update menu or jump to
                        ; subroutine as necessary.
   diag_main_menu_loop: JSR.W diag_menu_check_joypad
                        LDA.B diag_joy1h_previous & $FF
                        AND.B #JOYPAD_H_B
                        BEQ   @update_menu

                        ; Button pressed, run selected item.
                        LDA.B diag_menu_position & $FF

                        CMP.B #1
                        BEQ   @jump_self_test
                        CMP.B #2
                        BEQ   @jump_adpcm_test
                        CMP.B #3
                        BEQ   @jump_comms_test
                        CMP.B #4
                        BEQ   @jump_cxd_test

                        ; No match, must be item 0 ("EXIT")
                        ; Reset CXD and return to main program (title screen)
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_CHPRST
                        STA.W SCD_CXD_DATA
                        JML.L reset             ; back to main program

                        ; Jump table for above. These routines don't return, so
                        ; they are required to jump back to the main menu when
                        ; finished.
       @jump_self_test: JMP.W self_test_routine
      @jump_adpcm_test: JML.L adpcm_test_routine
      @jump_comms_test: JML.L comms_test_screen
        @jump_cxd_test: JML.L cxd_test_screen

          @update_menu: REP   #$20
                        LDA.B diag_joy1l_previous & $FF
                        AND.W #(JOYPAD_H_UP << 8 | JOYPAD_L_L)
                        BEQ   @up_not_pressed

                        ; Move selection up
                        SEP   #$20
                        JSR.W diag_unhighlight_menu
                        LDA.B diag_menu_position & $FF
                        DEC   A
                        BPL   @in_lower_bounds
                        LDA.B #DIAG_MENU_ITEMS          ; wrap to bottom
      @in_lower_bounds: STA.B diag_menu_position & $FF
                        JSR.W diag_highlight_menu
                        BRA   diag_main_menu_loop

       @up_not_pressed: LDA.B diag_joy1l_previous & $FF
                        AND.W #(JOYPAD_H_DOWN << 8 | JOYPAD_L_R)
                        BEQ   @down_not_pressed

                        SEP   #$20
                        JSR.W diag_unhighlight_menu
                        LDA.B diag_menu_position & $FF
                        INC   A
                        CMP.B #(DIAG_MENU_ITEMS + 1)
                        BNE   @in_upper_bounds
                        LDA.B #0                        ; wrap to top
    @in_upper_bounds:   STA.B diag_menu_position & $FF

                        JSR.W diag_highlight_menu
                        BRA   diag_main_menu_loop

    @down_not_pressed:  SEP   #$20
                        BRA   diag_main_menu_loop


; Colour the currenty selected menu item.
   diag_highlight_menu: REP   #$20
                        LDA.B diag_menu_position & $FF
                        AND.W #$00FF
                        CLC
                        ADC.W #3                                ; row offset
                        XBA
                        LSR A
                        TAX

                        LDY.W #0
                        SEP   #$20
                 @loop: LDA.B #(TEXT_COLOUR_YELLOW >> 8)
                        STA.W (WRAM_ADDR_BG3_DIAG + 1),X        ; top/bottom tile
                        STA.W (WRAM_ADDR_BG3_DIAG + SCREEN_WIDTH_B + 1),X
                        INX
                        INX
                        INY
                        CPY.W #SCREEN_WIDTH_W                   ; end of row
                        BNE   @loop
                        RTS

; Un-colour the currenty selected menu item.
 diag_unhighlight_menu: REP #$20
                        LDA.B diag_menu_position & $FF
                        AND.W #$00FF
                        CLC
                        ADC.W #3                                ; row offset
                        XBA
                        LSR A
                        TAX

                        LDY.W #0
                        SEP #$20
                 @loop: LDA.B #(TEXT_COLOUR_WHITE >> 8)
                        STA.W (WRAM_ADDR_BG3_DIAG + 1),X        ; top/bottom tile
                        STA.W (WRAM_ADDR_BG3_DIAG + SCREEN_WIDTH_B + 1),X
                        INX
                        INX
                        INY
                        CPY.W #SCREEN_WIDTH_W                   ; end of row
                        BNE   @loop
                        RTS


diag_menu_check_joypad:
                 @loop: LDX.B diag_joy1l & $FF
                        CPX.B (diag_joy1l_previous & $FF)
                        BNE   @break

                        LDA.B diag_delay_major & $FF
                        CMP.B #$20
                        BMI   @loop

                        AND.B #$03
                        BNE   @loop

                        INC.B (diag_delay_major & $FF)
                        RTS

                @break: STZ.B (diag_delay_major & $FF)          ; reset delay counter
                        STX.B (diag_joy1l_previous & $FF)
                        RTS


    diag_init_graphics: ; Clear all VRAM
                        LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_clear_vram_diag
                        JSR.W diag_dma_copy

                        ; Load greyscale tiles
                        LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_greyscale_diag
                        JSR.W diag_dma_copy

                        ; Clear palette.
                        STZ.W CGADD
                        LDX.W #data_dma_clear_palette_diag
                        JSR.W diag_dma_copy

                        ; Load font tiles (bank $00, normal)
                        LDX.W #TILE_OFFSET_BG3
                        STX.W VMADDL
                        REP   #$20
                        LDX.W #0
          @loop_font_1: LDA.L gfx_font_tiles,X
                        AND.W #$00FF
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @loop_font_1

                        ; Load font tiles (bank $00, reverse video)
                        LDX.W #TILE_OFFSET_BG3 + FONT_TILESET_SIZE
                        STX.W VMADDL
                        LDX.W #0
          @loop_font_2: LDA.L gfx_font_tiles,X
                        AND.W #$00FF
                        EOR.W #$FFFF
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @loop_font_2

                        ; Load palette
                        SEP   #$20
                        STZ.W CGADD
                        LDX.W #data_dma_palette_diag
                        JSR.W diag_dma_copy

                        ; "Clear" (fill) text buffer.
                        REP   #$20
                        LDX.W #$0000
                        LDA.W #$2048    ; "space" character and attributes.
            @loop_fill: STA.W WRAM_ADDR_BG3_DIAG,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @loop_fill

                        ; Copy above WRAM buffer to VRAM
                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_diag
                        JSR.W diag_dma_copy
                        RTS

; Data for DMA copy routine:
;  byte   0: DMA params
;  byte   1: B-Bus address ($21xx)
;  byte 2-4: source (low, high, bank)
;  byte 5-6: count (low, ligh)
data_dma_clear_vram_diag:
                        .DB $09                 ; no autoinc source
                        .DB (VMDATAL & $FF)
                        .DW dma_data_zero_byte_diag
                        .DB $01                 ; bank 1
                        .DW $0000

data_dma_gfx_greyscale_diag:
                        .DB $01                 ; autoinc source, bytes interleaved
                        .DB (VMDATAL & $FF)
                        .DW gfx_tiles_greyscale_diag
                        .DB $01                 ; bank 1
                        .DW $0200               ; 512 bytes

data_dma_clear_palette_diag:
                        .DB $09                 ; no autoinc source
                        .DB (CGDATA & $FF)
                        .DW dma_data_zero_byte_diag
                        .DB $01                 ; bank 1
                        .DW $0100

data_dma_palette_diag:  .DB $00                 ; autoinc source, single bytes
                        .DB (CGDATA & $FF)
                        .DW data_palette_diag
                        .DB $01                 ; bank 1
                        .DW 32

data_dma_gfx_wram_to_vram_diag:
                        .DB $01                 ; autoinc source, bytes interleaved
                        .DB (VMDATAL & $FF)
                        .DW $1000
                        .DB $00                 ; bank 0
                        .DW SCREEN_SIZE_B       ; 1 screen full

dma_data_zero_byte_diag:
                        .DB $00


     diag_menu_handler: REP   #$20
                        REP   #$10
                        PHA
                        PHB
                        PHX
                        PHY
                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_diag
                        JSR.W diag_dma_copy

                        LDA.W JOY1H
                        STA.W diag_joy1h
                        LDA.W JOY1L
                        STA.W diag_joy1l

                        ; Use delay periods to control the speed at which menu
                        ; selection changes will register.
                        LDA.W diag_delay_minor
                        INC   A
                        BEQ   @minor_delay_expired

                        STA.W diag_delay_minor
  @minor_delay_expired: LDA.W diag_delay_major
                        INC A
                        CMP.B #$30
                        BNE   @major_delay_not_expired

                        LDA.B #$20
@major_delay_not_expired:
                        STA.W diag_delay_major
                        REP   #$20
                        PLY
                        PLX
                        PLB
                        PLA
                        RTI


         diag_dma_copy: LDA.W 0,X
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
     data_palette_diag: .DW ($0A << 10) | ($00 << 5) | ($00)
                        .DW ($1F << 10) | ($1F << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($0A << 10) | ($00 << 5) | ($00)
                        .DW ($1C << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($0A << 10) | ($00 << 5) | ($00)
                        .DW ($1F << 10) | ($1F << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($1F << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)

                        ; unused?
             data_8252: .DB $5F,$29,$E0,$7F,$00,$00,$00,$00

; Input:
;   X = pointer to text data structure:
;     byte 0-1: screen offset/position
;     byte 2-3: OR-ed into memory to set attributes
;     byte 4-5: length of data following
;     byte 6+ : tile data (single bytes)
 diag_draw_screen_text: PHX
                        ; get offset/position
                        LDA.W 1,X
                        XBA
                        LDA.W 0,X
                        ASL A
                        ASL A
                        REP #$20
                        LSR A
                        CLC
                        ADC.W #WRAM_ADDR_BG3_DIAG       ; add offset to start of screen
                        TAY

                        LDA.W 2,X                       ; attributes
                        STA.W diag_text_attributes

                        LDA.W 4,X                       ; length
                 @loop: PHA
                        LDA.W 6,X                       ; tile data
                        INX

                        AND.W #$00FF                    ; apply attributes
                        ASL A
                        ORA.W diag_text_attributes

                        STA.W 0,Y                       ; top tile
                        INC A
                        STA.W SCREEN_WIDTH_B,Y          ; bottom of tile
                        INY
                        INY
                        TYA
                        AND.W #(SCREEN_WIDTH_B - 1)     ; limit/wrap
                        BNE   @no_wrap
                        TYA
                        CLC
                        ADC.W #SCREEN_WIDTH_B           ; next row
                        TAY

              @no_wrap: PLA
                        DEC   A
                        BNE   @loop

                        SEP   #$20
                        PLX
                        RTS


; Diagnostic screen text, formatted for `diag_draw_screen_text`.
data_tilemap_main_menu: .DW $0000       ; offset/position
                        .DW TEXT_COLOUR_WHITE
                        .DW I18N_LENGTH_MONITOR_MENU
                        I18N_MONITOR_MENU


gfx_tiles_greyscale_diag:
                        .INCLUDE "gfx/greyscale.inc"

;
; Routine for menu item 1, "SELF CHECK"
;
     self_test_routine: PHK
                        PLB
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP
                        STZ.W NMITIMEN
                        SEI

                        LDA.B #$04                      ; Enable BG3
                        STA.W TS
                        STA.W TM

                        STZ.W SCD_MYSTERY

                        JSR.W self_test_load_palette
                        ; VRAM test runs while screen is still blanked, for obvious reasons.
                        JSR.W self_test_vram

                        ; Load the first screen
                        LDX.W #data_tilemap_self_test_screen_1
                        JSR.W self_test_draw_tilemap

                        LDX.W #$006E
                        JSR.W self_test_show_ok_ng      ; VRAM test result

                        ; Short delay... to allow DMA to complete?
                        PHY
                        PHX
                        LDY.W #$0100
        @delay_1_outer: LDX.W #$00FF
        @delay_1_inner: DEX
                        BNE   @delay_1_inner
                        DEY
                        BNE   @delay_1_outer
                        PLX
                        PLY

                        JSR.W self_test_palette         ; CG-RAM
                        JSR.W self_test_load_palette    ; Restore after test
                        LDX.W #$008E
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_oam             ; OAM
                        LDX.W #$00AE
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_wram            ; WORK RAM
                        LDX.W #$00CE
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_dma             ; DMA
                        LDX.W #$00EE
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_timer           ; TIMER
                        LDX.W #$010E
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_sound           ; SOUND CHECK
                        LDX.W #$012E
                        JSR.W self_test_show_ok_ng

                        ; Short delay between first and second screens
                        PHY
                        PHX
                        LDY.W #$0500
        @delay_2_outer: LDX.W #$00FF
        @delay_2_inner: DEX
                        BNE   @delay_2_inner
                        DEY
                        BNE   @delay_2_outer
                        PLX
                        PLY

                        ; Load the second screen
                        LDX.W #data_tilemap_self_test_screen_2
                        JSR.W self_test_draw_tilemap

                        JSR.W self_test_exp_ram         ; EXPANSION RAM
                        LDX.W #$006E
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_backup_ram      ; BACKUP RAM
                        LDX.W #$008E
                        JSR.W self_test_show_ok_ng

                        JSL.L self_test_cxd             ; CD-ROM DECODER
                        LDX.W #$00AE
                        JSR.W self_test_show_ok_ng

                        JSR.W self_test_mechacon        ; CD-PLAYER I/F
                        LDX.W #$00CE
                        JSR.W self_test_show_ok_ng

                        ; Show "test compete" message.
                        LDX.W #data_text_test_complete
                        JSR.W self_test_render_text

                        ; Few seconds delay before returning to main menu.
                        PHY
                        PHX
                        LDY.W #$2000
        @delay_3_outer: LDX.W #$00FF
        @delay_3_inner: DEX
                        BNE   @delay_3_inner
                        DEY
                        BNE   @delay_3_outer

                        PLX
                        PLY
                        JML.L diagnostic_menu

data_text_test_complete:
                        .DW $02D0 - (I18N_LENGTH_TEST_COMPLETE / 2)
                        .DW TEXT_COLOUR_YELLOW
                        .DB I18N_LENGTH_TEST_COMPLETE
                        I18N_TEST_COMPLETE

data_tilemap_self_test_screen_1:
                        I18N_SELF_TEST_SCREEN_1

data_tilemap_self_test_screen_2:
                        I18N_SELF_TEST_SCREEN_2

;
; Draw 32x15 charaters to the screen.
;
self_test_draw_tilemap: PHP
                        PHX
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP

                        ; Load font
                        LDX.W #TILE_OFFSET_BG3
                        STX.W VMADDL
                        REP   #$20
                        LDX.W #0
            @load_font: LDA.L gfx_font_tiles,X
                        AND.W #$00FF
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @load_font

                        ; Load palette
                        SEP #$20
                        STZ.W CGADD
                        LDX.W #data_dma_palette_diag
                        JSR.W diag_dma_copy

                        ; Calculate/store positions of first tile
                        REP   #$20
                        LDA.W #$0000
                        CLC
                        ADC.W #VRAM_ADDR_BG3_DIAG               ; Add offset to start of screen
                        STA.W diag_top_tile
                        CLC
                        ADC.W #SCREEN_WIDTH_W                   ; Next row
                        STA.W diag_bottom_tile

                        PLX
                        LDY.W #15                               ; 15 rows of text
             @draw_row: PHY
                        LDY.W #SCREEN_WIDTH_W                   ; 32 columns
            @draw_char: LDA.W diag_top_tile
                        STA.W VMADDL
                        LDA.W $0000,X
                        AND.W #$00FF
                        ASL A
                        ORA.W #TEXT_COLOUR_WHITE                ; OR attributes
                        STA.W VMDATAL
                        PHA
                        LDA.W diag_bottom_tile
                        STA.W VMADDL
                        PLA
                        INC A
                        STA.W VMDATAL
                        INX
                        INC.W diag_top_tile
                        INC.W diag_bottom_tile
                        DEY
                        BNE   @draw_char

                        LDA.W diag_top_tile
                        CLC
                        ADC.W #SCREEN_WIDTH_W                   ; Next row
                        STA.W diag_top_tile
                        CLC
                        ADC.W #SCREEN_WIDTH_W                   ; Next row
                        STA.W diag_bottom_tile
                        PLY
                        DEY
                        BNE   @draw_row

                        ; Load another palette(?)
                        SEP   #$20
                        STZ.W CGADD
                        LDX.W #data_dma_palette_self_test
                        JSR.W diag_dma_copy

                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        PLP
                        RTS

self_test_load_palette: PHP
                        PHP
                        SEP #$20

          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        STZ.W CGADD
                        LDX.W #data_dma_palette_self_test
                        JSR.W diag_dma_copy
                        PLP
                        RTS


data_dma_palette_self_test:
                        .DB $00
                        .DB (CGDATA & $FF)
                        .DW data_palette_self_test
                        .DB $01
                        .DW 32

                        ;     B             G            R
data_palette_self_test: .DW ($0C << 10) | ($00 << 5) | ($00)
                        .DW ($1F << 10) | ($1F << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($0C << 10) | ($00 << 5) | ($00)
                        .DW ($1F << 10) | ($1F << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($0C << 10) | ($00 << 5) | ($00)
                        .DW ($04 << 10) | ($04 << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($0C << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($1F << 5) | ($1F)
                        .DW ($00 << 10) | ($00 << 5) | ($00)
                        .DW ($00 << 10) | ($00 << 5) | ($00)

;
; Print green "OK" or red "NG" next to appropriate test's name.
;
; Input:
;   X = offset into screen at which to write the text
;
  self_test_show_ok_ng: PHP
                        REP   #$20
                        LDA.W #0
                        ADC.W #0                        ; Get carry flag, indicating result
                        PHA
                        ASL   A                         ; x2
                        ADC.W #self_test_text_ok_ng     ; Index into text
                        TAY

                        TXA                             ; Offset/position parameter
                        ASL   A
                        CLC
                        ADC.W #VRAM_ADDR_BG3_DIAG       ; Add to start of screen
                        STA.W diag_top_tile
                        CLC
                        ADC.W #SCREEN_WIDTH_W           ; Next row
                        STA.W diag_bottom_tile

                        PLA                             ; Calculate colour attribute
                        INC   A
                        XBA
                        ASL   A
                        ASL   A
                        ORA.W #$2000                    ; Attribute mask
                        STA.W diag_text_attributes

                        PHP
                        SEP   #$20
          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        LDA.W #2                        ; Number of characters
            @draw_char: PHA
                        LDX.W diag_top_tile
                        STX.W VMADDL
                        LDA.W $0000,Y
                        AND.W #$00FF
                        ASL   A
                        ORA.W diag_text_attributes
                        STA.W VMDATAL
                        LDX.W diag_bottom_tile
                        STX.W VMADDL
                        INC   A
                        STA.W VMDATAL
                        INY
                        INC.W diag_top_tile
                        INC.W diag_bottom_tile
                        PLA
                        DEC   A
                        BNE   @draw_char

                        PLP
                        RTS

;
; Strings for above.
;
  self_test_text_ok_ng: .ASC "OK"
                        .ASC "NG"


;
; Render text to screen
; Input:
;   X = pointer to structure containing text:
;     byte 0-1: offset/position
;     byte 2-3: colour attributes
;     byte 4  : length of text
;     byte 5  : beginning of text string
;
 self_test_render_text: PHP
                        PHP
                        SEP #$20

          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        REP   #$20
                        LDA.W 0,X                       ; Offset/position
                        CLC
                        ADC.W #VRAM_ADDR_BG3_DIAG       ; Add to start of screen
                        STA.W diag_top_tile
                        CLC
                        ADC.W #SCREEN_WIDTH_W           ; Next row
                        STA.W diag_bottom_tile

                        LDA.W 2,X                       ; Colour attributes
                        STA.W diag_text_attributes

                        LDA.W 4,X                       ; Length (max $FF)
                        AND.W #$00FF
            @draw_char: PHA
                        LDY.W diag_top_tile
                        STY.W VMADDL
                        LDA.W 5,X                       ; Character in string
                        AND.W #$00FF
                        ASL   A
                        ORA.W diag_text_attributes
                        STA.W VMDATAL
                        LDY.W diag_bottom_tile
                        STY.W VMADDL
                        INC   A
                        STA.W VMDATAL
                        INX
                        INC.W diag_top_tile
                        INC.W diag_bottom_tile
                        PLA
                        DEC   A
                        BNE   @draw_char

                        PLP
                        RTS

;
; CXD-1800 self test
;
                        .DEFINE tmp_self_test_cxd_stack_pointer $020D

         self_test_cxd: PHP
                        REP   #$30
                        PHA
                        PHX
                        SEP   #$20
                        REP   #$10

                        ; Set up the decoder...
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_CHPRST
                        STA.W SCD_CXD_DATA

                        LDA.B #CXD_WR_DRVIF
                        STA.W SCD_CXD_INDEX
                        LDA.B #(CXD_DRVIF_XSLOW | CXD_DRVIF_LCHLOW | CXD_DRVIF_BCKMD1 | CXD_DRVIF_LSB1ST | CXD_DRVIF_CLKLOW)
                        STA.W SCD_CXD_DATA

                        LDA.B #CXD_WR_DECCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_DECCTL_AUTODIST
                        STA.W SCD_CXD_DATA

                        ; Save current vector table onto the stack.
                        LDX.W #0
                 @loop: LDA.W supercd_bios_vector_table,X
                        PHA
                        INX
                        CPX.W #VECTOR_TABLE_SIZE
                        BNE   @loop

                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode

                        LDX.W #self_test_cxd_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #self_test_cxd_irq_handler
                        STX.W supercd_bios_irq_vector_address

                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank

                        LDA.B #$04
                        STA.W SCD_MYSTERY

                        LDA.B #CXD_WR_DECCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #(CXD_DECCTL_AUTODIST | CXD_DECCTL_DECMD_RTC)
                        STA.W SCD_CXD_DATA

                        LDA.B #CXD_INTMSK_DECINT        ; to CXD_WR_INTMSK
                        STA.W SCD_CXD_DATA

                        ; Save stack pointer so we can later recover the saved vector table.
                        ; This also allows the NMI handler to return for us - notice this
                        ; subroutine has no return!
                        TSX
                        STX.W diag_stack_pointer

                        LDX.W #$0000
                        LDY.W #$0000

                        LDA.B #NMITIMEN_VBLANK
                        STA.W NMITIMEN

                        CLI

                        ; Loop forever
       @wait_interrupt: NOP
                        NOP
                        BRA   @wait_interrupt


self_test_cxd_nmi_handler:
                        INY                     ; Count V-blanks
                        CPY.W #60               ; 60 == 1 second
                        BEQ   @one_second
                        RTI

           @one_second: SEI                     ; Disable interrupts
                        STZ.W NMITIMEN
                        REP   #$20
                        TXA                     ; X = number of DecInts IRQs received.

                        ; Restore stack pointer to recover the saved vector table. This also
                        ; allows an RTL out of this routine, as if it were from `self_test_cxd`.
                        LDX.W diag_stack_pointer
                        TXS

                        ; Determine results based on number of DecInts.
                        CMP.W #70
                        BMI   @exit_error
                        CMP.W #80
                        BMI   @exit_ok
                        CMP.W #140
                        BMI   @exit_error
                        CMP.W #160
                        BPL   @exit_error

              @exit_ok: SEP   #$20
                        LDX.W #(VECTOR_TABLE_SIZE - 1)

                        ; Restore vector table
              @loop_ok: PLA
                        STA.W supercd_bios_vector_table,X
                        DEX
                        BPL   @loop_ok

                        REP   #$30
                        PLX
                        PLA
                        PLP
                        CLC
                        RTL

           @exit_error: SEP   #$20
                        LDX.W #(VECTOR_TABLE_SIZE - 1)

                        ; Restore vector table
           @loop_error: PLA
                        STA.W supercd_bios_vector_table,X
                        DEX
                        BPL   @loop_error

                        REP   #$30
                        PLX
                        PLA
                        PLP
                        SEC
                        RTL


self_test_cxd_irq_handler:
                        LDA.B #CXD_RD_INTSTS            ; Get interrupt status
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA
                        AND.B #CXD_INTSTS_DECINT

                        BEQ   @exit                     ; Exit if no interrupt

                        LDA.B #CXD_WR_INTCLR            ; Ack/clear interrupt
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_INTCLR_DECINT
                        STA.W SCD_CXD_DATA

                        INX                             ; Keep count in X
                 @exit:
                        RTI

;
; WRAM test
;
; Checks simple writes/reads, DMA and mirrored addresses.
;
        self_test_wram: PHP
                        SEP   #$20
                        REP   #$10
                        PLA
                        STA.W DMAP7
                        PLX
                        STX.W A1T7L

                        REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
             @write_7e: STA.L $7E0000,X
                        INX
                        INX
                        INC   A
                        CPX.W #$0000
                        BNE   @write_7e

                        LDA.W #$FFFF
             @write_7f: STA.L $7F0000,X
                        INX
                        INX
                        DEC   A
                        CPX.W #$0000
                        BNE   @write_7f

                        ; DMA banks 00-3F
                        SEP   #$20
                        LDA.B #$80
                        STA.W DMAP0
                        LDA.B #$34
                        STA.W BBAD0
                        LDA.B #$00
                        STA.W A1B0
               @dma_00: LDX.W #$8000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC   A
                        STA.W A1B0
                        CMP.B #$40
                        BNE   @dma_00

                        ; DMA banks 40-7D
                        LDA.B #$80
                        STA.W DMAP0
                        LDA.B #$34
                        STA.W BBAD0
                        LDA.B #$40
                        STA.W A1B0
               @dma_40: LDX.W #$0000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC   A
                        STA.W A1B0
                        CMP.B #$7E
                        BNE   @dma_40

                        ; DMA banks 80-C0
                        SEP   #$20
                        LDA.B #$80
                        STA.W DMAP0
                        LDA.B #$34
                        STA.W BBAD0
                        LDA.B #$80
                        STA.W A1B0
               @dma_80: LDX.W #$8000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC   A
                        STA.W A1B0
                        CMP.B #$C0
                        BNE @dma_80

                        ; DMA banks C0-FF
                        LDA.B #$80
                        STA.W DMAP0
                        LDA.B #$34
                        STA.W BBAD0
                        LDA.B #$C0
                        STA.W A1B0
               @dma_c0: LDX.W #$0000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC   A
                        STA.W A1B0
                        CMP.B #$00
                        BNE   @dma_c0

                        REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
           @rewrite_7e: CMP.L $7E0000,X
                        BNE   @jmp_exit_error
                        INX
                        INX
                        INC   A
                        CPX.W #$0000
                        BNE   @rewrite_7e

                        LDA.W #$FFFF
           @rewrite_7f: CMP.L $7F0000,X
                        BNE   @jmp_exit_error
                        INX
                        INX
                        DEC   A
                        CPX.W #$0000
                        BNE   @rewrite_7f

                        ; Check mirror in banks 00-3F
                        SEP   #$20
                        PLX
                        LDA.B #$00
                        PHA
                        PLB
                        PHX
   @verify_mirror_bank: REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
   @verify_mirror_addr: CMP.W $0000,X
                        BNE   @jmp_exit_error
                        INX
                        INX
                        INC   A
                        CPX.W #$2000
                        BNE   @verify_mirror_addr
                        PLX
                        PHB
                        SEP   #$20
                        PLA
                        INC   A
                        PHA
                        PLB
                        PHX
                        CMP.B #$40
                        BNE   @verify_mirror_bank

                        ; Check system area in banks 80-BF
                        PLX
                        LDA.B #$80
                        PHA
                        PLB
                        PHX
   @verify_system_bank: REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
   @verify_system_addr: CMP.W $0000,X
                        BNE   @jmp_exit_error
                        INX
                        INX
                        INC   A
                        CPX.W #$2000
                        BNE   @verify_system_addr
                        PLX
                        PHB
                        SEP   #$20
                        PLA
                        INC   A
                        PHA
                        PLB
                        PHX
                        CMP.B #$C0
                        BNE   @verify_system_bank

                        BRA   @continue
       @jmp_exit_error: BRA   @exit_error

             @continue: REP   #$20
                        LDA.W #$FFFF
                        LDX.W #$0000
        @last_write_7e: STA.L $7E0000,X
                        INX
                        INX
                        DEC   A
                        CPX.W #$0000
                        BNE   @last_write_7e

                        LDA.W #$0000
        @last_write_7f: STA.L $7F0000,X
                        INX
                        INX
                        INC   A
                        CPX.W #$0000
                        BNE   @last_write_7f

                        REP   #$20
                        LDA.W #$FFFF
                        LDX.W #$0000
       @last_verify_7e: CMP.L $7E0000,X
                        BNE   @exit_error
                        INX
                        INX
                        DEC   A
                        CPX.W #$0000
                        BNE  @last_verify_7e

                        LDA.W #$0000
       @last_verify_7f: CMP.L $7F0000,X
                        BNE   @exit_error
                        INX
                        INX
                        INC   A
                        CPX.W #$0000
                        BNE   @last_verify_7f

                        ; Exit OK
                        SEP   #$20
                        LDA.B #$01
                        PHA
                        PLB
                        LDX.W A1T7L
                        PHX
                        LDA.W DMAP7
                        PHA
                        PLP
                        CLC
                        RTS

           @exit_error: SEP #$20
                        LDA.B #$01
                        PHA
                        PLB
                        LDX.W A1T7L
                        PHX
                        LDA.W DMAP7
                        PHA
                        PLP
                        SEC
                        RTS

;
; VRAM test
;
        self_test_vram: PHP
                        SEP   #$20
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP

                        LDA.B #$80              ; Auto-increment on
                        STA.W VMAIN

                        REP   #$30
                        LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #$0000
              @write_1: STX.W VMDATAL
                        INX
                        CPX.W #$8000
                        BNE   @write_1

                        LDA.W #$0000
                        STA.W self_test_vram_value
                        LDX.W #$0000
             @verify_1: STX.W VMADDL
                        LDA.W RDVRAML
                        CMP.W self_test_vram_value
                        BNE   @exit_error
                        INC.W self_test_vram_value
                        INX
                        CPX.W #$8000
                        BNE   @verify_1

                        LDX.W #$0000
                        STX.W VMADDL
                        LDA.W #$FFFF
                        STA.W self_test_vram_value
                        LDX.W #$0000
              @write_2: STX.W VMADDL
                        LDA.W self_test_vram_value
                        STA.W VMDATAL
                        DEC   A
                        STA.W self_test_vram_value
                        INX
                        CPX.W #$8000
                        BNE   @write_2

                        LDX.W #$0000
                        STX.W VMADDL
                        LDA.W #$FFFF
                        STA.W self_test_vram_value
                        LDX.W #$0000
             @verify_2: STX.W VMADDL
                        LDA.W RDVRAML
                        CMP.W self_test_vram_value
                        BNE   @exit_error
                        DEC   A
                        STA.W self_test_vram_value
                        INX
                        CPX.W #$8000
                        BNE   @verify_2

                        ; Exit OK
                        PLP
                        CLC
                        RTS

           @exit_error: PLP
                        SEC
                        RTS


     self_test_palette: PHP
                        SEP   #$20
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP
                        LDA.B #$7F
                        XBA

                        LDA.B #$00
                        LDX.W #0
              @write_1: STA.L $7F0000,X
                        INX
                        INC A
                        XBA
                        AND.B #$7F
                        STA.L $7F0000,X
                        INX
                        DEC A
                        XBA
                        CPX.W #$0200
                        BNE   @write_1

                        JSR.W dma_restore_palette
                        JSR.W dma_save_palette

                        REP   #$20
                        LDX.W #0
             @verify_1: LDA.L $7F1000,X
                        AND.W #$7FFF
                        CMP.L $7F0000,X
                        BNE   @exit_error
                        INX
                        INX
                        CPX.W #$0200
                        BNE   @verify_1

                        SEP   #$20
                        LDA.B #$00
                        XBA
                        LDA.B #$FF
                        LDX.W #0
              @write_2: STA.L $7F0000,X
                        INX
                        DEC A
                        XBA
                        AND.B #$7F
                        STA.L $7F0000,X
                        INX
                        INC A
                        XBA
                        CPX.W #$0200
                        BNE   @write_2

                        JSR.W dma_restore_palette
                        JSR.W dma_save_palette

                        REP   #$20
                        LDX.W #0
             @verify_2: LDA.L $7F1000,X
                        AND.W #$7FFF
                        CMP.L $7F0000,X
                        BNE   @exit_error
                        INX
                        INX
                        CPX.W #$0200
                        BNE   @verify_2

                        SEP   #$20
                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        PLP
                        CLC
                        RTS

           @exit_error: SEP #$20
                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        PLP
                        SEC
                        RTS


      dma_save_palette: PHP
                        SEP   #$20
                        STZ.W CGADD
                        LDA.B #$80                      ; B-Bus to CPU
                        STA.W DMAP0
                        LDA.B #(RDCGRAM & $FF)
                        STA.W BBAD0
                        LDX.W #$1000
                        STX.W A1T0L
                        LDA.B #$7F                      ; Bank (WRAM)
                        STA.W A1B0
                        LDX.W #$0200
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        PLP
                        RTS

   dma_restore_palette: PHP
                        SEP   #$20
                        STZ.W CGADD
                        LDA.B #$00                      ; CPU to B-Bus
                        STA.W DMAP0
                        LDA.B #(CGDATA & $FF)
                        STA.W BBAD0
                        LDX.W #$0000
                        STX.W A1T0L
                        LDA.B #$7F                      ; Bank (WRAM)
                        STA.W A1B0
                        LDX.W #$0200
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        PLP
                        RTS

;
; OAM self test
;
         self_test_oam: .DEFINE SELF_TEST_OAM_WRAM_BUFFER        $7F0000
                        .DEFINE SELF_TEST_OAM_WRAM_BUFFER_COPY   $7F1000

                        PHP
                        SEP #$20

                        ; First pass
                        LDX.W #$00FF
                        STX.W self_test_oam_test_pattern_l

                        ; Fill WRAM buffer with test data.
                        LDX.W #0
         @write_wram_1: LDA.W self_test_oam_test_pattern_l
                        STA.L SELF_TEST_OAM_WRAM_BUFFER,X
                        INX
                        DEC   A
                        STA.W self_test_oam_test_pattern_l
                        LDA.W self_test_oam_test_pattern_h
                        STA.L SELF_TEST_OAM_WRAM_BUFFER,X
                        INC   A
                        STA.W self_test_oam_test_pattern_h
                        INX
                        CPX.W #OAM_SIZE
                        BNE   @write_wram_1

                        ; Write WRAM buffer to OAM
                        JSR.W self_test_oam_write

                        ; Read back OAM to a separate buffer and compare (16x)
                        LDY.W #16
        @verify_loop_1: JSR.W self_test_oam_read
                        LDX.W #0
             @verify_1: LDA.L SELF_TEST_OAM_WRAM_BUFFER,X
                        CMP.L SELF_TEST_OAM_WRAM_BUFFER_COPY,X
                        BNE   @exit_error
                        INX
                        CPX.W #OAM_SIZE
                        BNE   @verify_1
                        DEY
                        BNE   @verify_loop_1

                        ; Second pass, same as above with inverted test pattern.
                        LDX.W #$FF00
                        STX.W self_test_oam_test_pattern_l
                        LDX.W #0
         @write_wram_2: LDA.W self_test_oam_test_pattern_l
                        STA.L SELF_TEST_OAM_WRAM_BUFFER,X
                        INX
                        INC   A
                        STA.W self_test_oam_test_pattern_l
                        LDA.W self_test_oam_test_pattern_h
                        STA.L SELF_TEST_OAM_WRAM_BUFFER,X
                        INX
                        DEC   A
                        STA.W self_test_oam_test_pattern_h
                        CPX.W #OAM_SIZE
                        BNE   @write_wram_2

                        JSR.W self_test_oam_write

                        LDY.W #16
        @verify_loop_2: JSR.W self_test_oam_read
                        LDX.W #0
             @verify_2: LDA.L SELF_TEST_OAM_WRAM_BUFFER,X
                        CMP.L SELF_TEST_OAM_WRAM_BUFFER_COPY,X
                        BNE   @exit_error
                        INX
                        CPX.W #OAM_SIZE
                        BNE   @verify_2
                        DEY
                        BNE   @verify_loop_2

                        PLP
                        CLC
                        RTS

           @exit_error: PLP
                        SEC
                        RTS


; DMA OAM to a WRAM buffer.
; NOTE: read and writes are to separate buffers.
    self_test_oam_read: PHP
                        SEP   #$20
          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        LDX.W #$0000
                        STX.W OAMADDL
                        LDA.B #DMAP_BBUS_TO_CPU
                        STA.W DMAP0
                        LDA.B #(RDOAM & $FF)
                        STA.W BBAD0
                        LDX.W #(SELF_TEST_OAM_WRAM_BUFFER_COPY & $FFFF)
                        STX.W A1T0L
                        LDA.B #(SELF_TEST_OAM_WRAM_BUFFER_COPY >>16)
                        STA.W A1B0
                        LDX.W #OAM_SIZE
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        RTS

; DMA a WRAM buffer to OAM
; NOTE: read and writes are to separate buffers.
   self_test_oam_write: PHP
                        SEP #$20
          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI

                        PLP
                        LDX.W #$0000
                        STX.W OAMADDL
                        LDA.B #DMAP_CPU_TO_BBUS
                        STA.W DMAP0
                        LDA.B #(OAMDATA & $FF)
                        STA.W BBAD0
                        LDX.W #(SELF_TEST_OAM_WRAM_BUFFER & $FFFF)
                        STX.W A1T0L
                        LDA.B #(SELF_TEST_OAM_WRAM_BUFFER >> 16)
                        STA.W A1B0
                        LDX.W #OAM_SIZE
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        RTS

;
; DMA self test
; Just tests read/writes of registers, doesn't do any actual DMA.
;
         self_test_dma: PHP
                        SEP #$20
                        REP #$10

                        STZ.W MDMAEN            ; Disable DMA for now
                        STZ.W HDMAEN

                        ; Write to all DMA registers with ascending values.
                        LDA.B #0
                        LDX.W #0
   @fill_registers_asc: STA.W DMAP0,X
                        INC   A
                        INX
                        CPX.W #$80
                        BNE   @fill_registers_asc

                        LDA.B #$00
                        STA.W self_test_dma_value

                        LDX.W #0
           @verify_asc: LDA.W DMAP0,X
                        CMP.W self_test_dma_value
                        BNE   @exit_error

                        AND.B #$0F
                        CMP.B #$0A
                        BNE   @in_use_asc

                        ; Skip the unused/mirrored locations ($43xA-43xF)
                        INC.W self_test_dma_value
                        INC.W self_test_dma_value
                        INC.W self_test_dma_value
                        INC.W self_test_dma_value
                        INC.W self_test_dma_value
                        INX
                        INX
                        INX
                        INX
                        INX
           @in_use_asc: INC.W self_test_dma_value
                        INX
                        CPX.W #$80
                        BNE   @verify_asc

                        ; Write to all DMA registers with descending values.
                        LDA.B #$FF
                        LDX.W #0
  @fill_registers_desc: STA.W DMAP0,X
                        DEC   A
                        INX
                        CPX.W #$80
                        BNE   @fill_registers_desc

                        LDA.B #$FF
                        STA.W self_test_dma_value
                        LDX.W #$0000
          @verify_desc: LDA.W DMAP0,X
                        CMP.W self_test_dma_value
                        BNE   @exit_error

                        AND.B #$0F
                        CMP.B #$05
                        BNE   @in_use_desc

                        ; Skip the unused/mirrored locations ($43xA-43xF)
                        DEC.W self_test_dma_value
                        DEC.W self_test_dma_value
                        DEC.W self_test_dma_value
                        DEC.W self_test_dma_value
                        DEC.W self_test_dma_value
                        INX
                        INX
                        INX
                        INX
                        INX
          @in_use_desc: DEC.W self_test_dma_value
                        INX
                        CPX.W #$80
                        BNE   @verify_desc

                        PLP
                        CLC
                        RTS

           @exit_error: PLP
                        SEC
                        RTS

;
; Timer self test
;
       self_test_timer: PHP
                        SEP   #$24
                        REP   #$10
                        TSX
                        STX.W self_test_timer_stack_pointer
                        STZ.W NMITIMEN

                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        LDX.W #@nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank

                        ; Set initial counters
                        LDX.W #128
                        STX.W self_test_timer_htime
                        LDX.W #1
                        STX.W self_test_timer_vtime

           @wait_vlank: LDA.W RDNMI
                        BPL   @wait_vlank
                        LDA.W RDNMI

                        LDX.W self_test_timer_htime
                        STX.W HTIMEL
                        LDX.W self_test_timer_vtime
                        STX.W VTIMEL

                        LDA.W TIMEUP
                        LDA.B #(NMITIMEN_VBLANK | NMITIMEN_HV_IRQ)
                        STA.W NMITIMEN

                        ; Sleep until interrupt...
                        WAI

                        ; Latch and save counters
                        LDA.W SLHV
                        STZ.W NMITIMEN
                        LDA.W STAT78

                        LDA.W OPHCT
                        STA.W self_test_timer_ophct
                        LDA.W OPHCT
                        AND.B #$01
                        STA.W self_test_timer_ophct + 1

                        LDA.W OPVCT
                        STA.W self_test_timer_opvct
                        LDA.W OPVCT
                        AND.B #$01
                        STA.W self_test_timer_opvct + 1

                        ; Verify results
                        REP   #$20
                        LDA.W self_test_timer_opvct
                        CMP.W self_test_timer_vtime
                        BNE   @exit_error

                        LDA.W self_test_timer_ophct
                        SEC
                        SBC.W self_test_timer_htime
                        BCC   @exit_error

                        CMP.W #$0030
                        BCS   @exit_error

                        SEP   #$20
                        LSR.W self_test_timer_htime
                        ASL.W self_test_timer_vtime
                        BNE   @wait_vlank

                        PLP
                        CLC
                        RTS

          @nmi_handler: STZ.W NMITIMEN
                        LDX.W self_test_timer_stack_pointer
                        TXS
           @exit_error: PLP
                        SEC
                        RTS


;
; Mechacon self test
;
    self_test_mechacon: PHP
                        SEI
                        STZ.W NMITIMEN
                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode

                        LDX.W #self_test_mechacon_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #self_test_mechacon_irq_handler
                        STX.W supercd_bios_irq_vector_address

                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank

                        STZ.W self_test_mechacon_xxx
                        STZ.W self_test_mechacon_last_response_h
                        LDX.W #0
                        STX.W self_test_mechacon_response_length
                        STZ.W self_test_mechacon_nmi_counter

                        LDA.B #$08
                        STA.W SCD_MYSTERY

                        CLI
                        LDA.B #$80
                        STA.W NMITIMEN

                        LDX.W #data_self_test_mechacon_flush
                        JSR.W self_test_mechacon_execute_command
                        BCS   @exit_error

                        LDX.W #data_self_test_mechacon_req_status
                        JSR.W self_test_mechacon_execute_command
                        BCS   @exit_error

                        ; Exit OK
                        STZ.W NMITIMEN
                        SEI
                        PLP
                        CLC
                        RTS

           @exit_error: STZ.W NMITIMEN
                        SEI
                        PLP
                        SEC
                        RTS


self_test_mechacon_nmi_handler:
                        REP   #$20
                        INC.W self_test_mechacon_nmi_counter
                        RTI


self_test_mechacon_irq_handler:
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20

                        ; Store response nibble
                        LDA.W SCD_MECHACON
                        AND.B #$0F
                        STA.W self_test_mechacon_last_response_l

                        ; Increment length
                        LDX.W self_test_mechacon_response_length
                        INX
                        STX.W self_test_mechacon_response_length
                        DEX

                        LDA.W 0,X
                        BMI   @has_response

                        CMP.W self_test_mechacon_last_response_l
                        BEQ   @continue

                        LDA.W self_test_mechacon_last_response_h
                        ORA.B #$08
                        STA.W self_test_mechacon_last_response_h
                        BRA   @continue

         @has_response: AND.B #$1F
                        SEP   #$10
                        TAX
                        LDA.W self_test_mechacon_last_response_l
                        STA.W self_test_mechacon_response_buffer,X
                        REP   #$10

             @continue: LDA.W self_test_mechacon_last_response_h
                        AND.B #$DF
                        STA.W self_test_mechacon_last_response_h
                        JSR.W self_test_mechacon_send_next
                        REP   #$30
                        PLY
                        PLX
                        PLA
                        RTI

; Input:
;   X = pointer to data
self_test_mechacon_execute_command:
                        JSR.W self_test_mechacon_send_first

                 @loop: LDA.W self_test_mechacon_last_response_h
                        BPL   @exit_ok

                        AND.B #$10
                        BNE   @exit_bad_response

                        LDA.W self_test_mechacon_nmi_counter
                        CMP.B #(60/5)           ; 1/5 sec.
                        BMI   @loop

                        BRA   @exit_timeout

              @exit_ok: CLC
                        RTS

         @exit_timeout: LDA.B #$05
                        SEC
                        RTS

    @exit_bad_response: LDA.B #$06
                        SEC
                        RTS


; Sub has two entry points
self_test_mechacon_send_first:
                        LDA.B #$80
                        STA.W self_test_mechacon_last_response_h
                        STX.W self_test_mechacon_response_length

self_test_mechacon_send_next:
                        LDX.W self_test_mechacon_response_length
                        INX
                        STX.W self_test_mechacon_response_length
                        DEX

                        LDA.W 0,X
                        BMI   @has_response

                        STA.W SCD_MECHACON
                        STZ.W self_test_mechacon_nmi_counter
                        LDA.W self_test_mechacon_last_response_h
                        ORA.B #$20
                        STA.W self_test_mechacon_last_response_h
                        BRA   @exit

         @has_response: LDA.W self_test_mechacon_last_response_h
                        AND.B #$08
                        BEQ   @done

                        ORA.B #$10
                 @done: AND.B #$77
                        STA.W self_test_mechacon_last_response_h

                 @exit: RTS

; data for sub_self_test_mechacon
data_self_test_mechacon_req_status:
                        .DB $0D,$0F
                        .DB $05,$0F
                        .DB $01,$0F
                        .DB $0F,$0F
                        .DB $00,$80
                        .DB $01,$81
                        .DB $02,$82
                        .DB $03,$83
                        .DB $04,$84
                        .DB $0F,$85
                        .DB $FF

data_self_test_mechacon_flush:
                        .DB $0F,$85
                        .DB $FF


;
; Expansion RAM self test
;
     self_test_exp_ram: PHP
                        SEP   #$20
                        REP   #$10
                        PLA
                        STA.W DMAP7
                        PLX
                        STX.W A1T7L
                        REP   #$20

                        LDA.W #$0000
                        LDX.W #$0000
             @write_80: STA.L $808000,X
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_80

                        LDA.W #$FFFF
                        LDX.W #$0000
             @write_81: STA.L $818000,X
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_81

                        LDA.W #$0000
                        LDX.W #$0000
             @write_82: STA.L $828000,X
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE @write_82

                        LDA.W #$FFFF
                        LDX.W #$0000
             @write_83: STA.L $838000,X
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_83

                        LDA.W #$0000
                        LDX.W #$0000
             @write_84: STA.L $848000,X
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_84

                        LDA.W #$FFFF
                        LDX.W #$0000
             @write_85: STA.L $858000,X
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_85

                        LDA.W #$0000
                        LDX.W #$0000
             @write_86: STA.L $868000,X
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_86

                        LDA.W #$FFFF
                        LDX.W #$0000
             @write_87: STA.L $878000,X
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @write_87

                        SEP   #$20
                        LDA.B #$80              ; B-Bus to CPU
                        STA.W DMAP0
                        LDA.B #(MPYL & $FF)
                        STA.W BBAD0
                        LDA.B #$00              ; Bank 0
                        STA.W A1B0
               @dma_00: LDX.W #$8000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC A
                        STA.W A1B0
                        CMP.B #$40
                        BNE   @dma_00

                        LDA.B #$80              ; B-Bus to CPU
                        STA.W DMAP0
                        LDA.B #(MPYL & $FF)
                        STA.W BBAD0
                        LDA.B #$40              ; Bank $40
                        STA.W A1B0
               @dma_40: LDX.W #$0000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC A
                        STA.W A1B0
                        CMP.B #$80
                        BNE   @dma_40

                        SEP   #$20
                        LDA.B #$80              ; B-Bus to CPU
                        STA.W DMAP0
                        LDA.B #(MPYL & $FF)
                        STA.W BBAD0
                        LDA.B #$88              ; Bank $88
                        STA.W A1B0
               @dma_88: LDX.W #$8000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC A
                        STA.W A1B0
                        CMP.B #$C0
                        BNE   @dma_88

                        LDA.B #$80              ; B-Bus to CPU
                        STA.W DMAP0
                        LDA.B #(MPYL & $FF)
                        STA.W BBAD0
                        LDA.B #$C0              ; Bank $C0
                        STA.W A1B0
               @dma_c0: LDX.W #$0000
                        STX.W A1T0L
                        STX.W DAS0L
                        LDA.B #MDMAEN_CHANNEL0
                        STA.W MDMAEN
                        LDA.W A1B0
                        INC A
                        STA.W A1B0
                        CMP.B #$00
                        BNE   @dma_c0

                        REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
            @verify_80: CMP.L $808000,X
                        BNE   @jmp_exit_1
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_80

                        LDA.W #$FFFF
                        LDX.W #$0000
            @verify_81: CMP.L $818000,X
                        BNE   @jmp_exit_1
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_81

                        LDA.W #$0000
                        LDX.W #$0000
            @verify_82: CMP.L $828000,X
                        BNE   @jmp_exit_1
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_82

                        LDA.W #$FFFF
                        LDX.W #$0000
            @verify_83: CMP.L $838000,X
                        BNE   @jmp_exit_1
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_83
                        BRA   @continue_1

           @jmp_exit_1: JMP.W @exit_error

           @continue_1: REP #$20
                        LDA.W #$0000
                        LDX.W #$0000
            @verify_84: CMP.L $848000,X
                        BNE   @jmp_exit_1
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_84

                        LDA.W #$FFFF
                        LDX.W #$0000
            @verify_85: CMP.L $858000,X
                        BNE   @jmp_exit_1
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_85

                        LDA.W #$0000
                        LDX.W #$0000
            @verify_86: CMP.L $868000,X
                        BNE   @jmp_exit_1
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_86

                        LDA.W #$FFFF
                        LDX.W #$0000
            @verify_87: CMP.L $878000,X
                        BNE   @jmp_exit_1
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @verify_87

                        LDA.W #$FFFF
                        LDX.W #$0000
           @rewrite_80: STA.L $808000,X
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_80

                        LDA.W #$0000
                        LDX.W #$0000
           @rewrite_81: STA.L $818000,X
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_81

                        LDA.W #$FFFF
                        LDX.W #$0000
           @rewrite_82: STA.L $828000,X
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_82

                        LDA.W #$0000
                        LDX.W #$0000
           @rewrite_83: STA.L $838000,X
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_83

                        LDA.W #$FFFF
                        LDX.W #$0000
           @rewrite_84: STA.L $848000,X
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_84

                        LDA.W #$0000
                        LDX.W #$0000
           @rewrite_85: STA.L $858000,X
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_85

                        LDA.W #$FFFF
                        LDX.W #$0000
           @rewrite_86: STA.L $868000,X
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_86

                        LDA.W #$0000
                        LDX.W #$0000
           @rewrite_87: STA.L $878000,X
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @rewrite_87

                        LDA.W #$FFFF
                        LDX.W #$0000
          @reverify_80: CMP.L $808000,X
                        BNE   @jmp_exit_error_2
                        DEC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_80

                        LDA.W #$0000
                        LDX.W #$0000
          @reverify_81: CMP.L $818000,X
                        BNE   @jmp_exit_error_2
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_81

                        LDA.W #$FFFF
                        LDX.W #$0000
          @reverify_82: CMP.L $828000,X
                        BNE   @jmp_exit_error_2
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_82

                        LDA.W #$0000
                        LDX.W #$0000
          @reverify_83: CMP.L $838000,X
                        BNE   @jmp_exit_error_2
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_83

                        BRA   @continue_2

     @jmp_exit_error_2: JMP.W @exit_error

           @continue_2: LDA.W #$FFFF
                        LDX.W #$0000
          @reverify_84: CMP.L $848000,X
                        BNE  @jmp_exit_error_2
                        DEC  A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_84

                        LDA.W #$0000
                        LDX.W #$0000
          @reverify_85: CMP.L $858000,X
                        BNE   @exit_error
                        INC   A
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_85

                        LDA.W #$FFFF
                        LDX.W #$0000
          @reverify_86: CMP.L $868000,X
                        BNE   @exit_error
                        XBA
                        DEC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_86

                        LDA.W #$0000
                        LDX.W #$0000
          @reverify_87: CMP.L $878000,X
                        BNE   @exit_error
                        XBA
                        INC   A
                        XBA
                        INX
                        INX
                        CPX.W #$8000
                        BNE   @reverify_87

                        ; Exit OK
                        SEP   #$20
                        LDA.B #$01
                        PHA
                        PLB
                        LDX.W A1T7L
                        PHX
                        LDA.W DMAP7
                        PHA
                        PLP
                        CLC
                        RTS

           @exit_error: SEP #$20
                        LDA.B #$01
                        PHA
                        PLB
                        LDX.W A1T7L
                        PHX
                        LDA.W DMAP7
                        PHA
                        PLP
                        SEC
                        RTS

;
; Sound self test
;
       self_test_sound: PHP
                        SEP   #$20
                        REP   #$10
                        JSR.W self_test_sound_init

                        LDX.W #3                ; play 3 times

           @play_scale: LDA.B #$01              ; loop through 8 notes
            @play_note: STA.W APUI00
                        PHY
                        PHX

                        ; Delay, to let the note play.
                        LDY.W #$0200
     @delay_note_outer: LDX.W #$00FF
     @delay_note_inner: DEX
                        BNE   @delay_note_inner
                        DEY
                        BNE   @delay_note_outer
                        PLX
                        PLY

                        ASL   A                 ; shift pitch up
                        BNE   @play_note

                        ; Delay between scales
                        PHY
                        PHX
                        LDY.W #$0300
  @delay_between_outer: LDX.W #$00FF
  @delay_between_inner: DEX
                        BNE   @delay_between_inner
                        DEY
                        BNE   @delay_between_outer
                        PLX
                        PLY
                        DEX

                        BNE   @play_scale

                        STZ.W APUI00            ; silence
                        PLP
                        RTS


  self_test_sound_init: PHP
                        PHB
                        LDA.B #$00              ; Use bank 0, where sound data is stored.
                        PHA
                        PLB
                        REP   #$20

.IFNDEF ORIGINAL
                        ; Fix for infinite loop if the sound test is performed
                        ; more than once. If initialisation has already been
                        ; done (indictaed by $BB63) then return immediately.
                        LDA.W #$BB63
                        CMP.W APUI00
                        BEQ   @exit
.ENDIF

                        LDY.W #0

                        LDA.W #$BBAA            ; APU outputs this when ready
       @wait_apu_ready: CMP.W APUI00
                        BNE   @wait_apu_ready

                        SEP   #$20
                        LDA.B #$CC              ; Start command
                        BRA   @load_address

       @init_load_data: LDA.W self_test_sound_apu_data,Y
                        INY
                        XBA
                        LDA.B #$00
                        BRA   @skip_load_data

            @load_data: XBA
                        LDA.W self_test_sound_apu_data,Y
                        INY
                        XBA

            @wait_ack2: CMP.W APUI00
                        BNE   @wait_ack2

                        INC   A
       @skip_load_data: REP   #$20
                        STA.W APUI00
                        SEP   #$20
                        DEX
                        BNE   @load_data

            @wait_ack3: CMP.W APUI00
                        BNE   @wait_ack3

         @next_command: ADC.B #$03
                        BEQ   @next_command

         @load_address: PHA
                        REP   #$20

                        ; Get data block length
                        LDA.W self_test_sound_apu_data,Y
                        INY
                        INY
                        TAX

                        ; Get destination address
                        LDA.W self_test_sound_apu_data,Y
                        INY
                        INY
                        STA.W APUI02            ; Set address

                        SEP   #$20

                        CPX.W #1                ; Get carry flag into A
                        LDA.B #0
                        ROL A

                        STA.W APUI01            ; Send command
                        ADC.B #$7F

                        PLA
                        STA.W APUI00

             @wait_ack: CMP.W APUI00
                        BNE   @wait_ack

                        BVS   @init_load_data

                 @exit: PLB
                        PLP
                        RTS


; WRAM location into which a backup of the SRAM contents is saved, so these are
; preserved after testing.
.DEFINE tmp_self_test_backup_ram_backup $7F8000

;
; Backup SRAM self test
;
  self_test_backup_ram: PHP
                        SEP   #$20
                        REP   #$10

                        ; Back up current SRAM contents to WRAM.
                        LDX.W #$0000
               @backup: LDA.L (SRAM_BANK << 16 | SRAM_START),X
                        STA.L tmp_self_test_backup_ram_backup,X
                        INX
                        CPX.W #SRAM_SIZE
                        BNE   @backup

                        JSR.W self_test_sram_unlock

                        REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
      @write_acsending: STA.L (SRAM_BANK << 16 | SRAM_START),X
                        INX
                        INX
                        INC   A
                        CPX.W #SRAM_SIZE
                        BNE   @write_acsending

                        SEP   #$20
                        JSR.W self_test_sram_lock

                        REP   #$20
                        LDA.W #$0000
                        LDX.W #$0000
     @verify_ascending: CMP.L (SRAM_BANK << 16 | SRAM_START),X
                        BNE   @exit_error
                        INX
                        INX
                        INC   A
                        CPX.W #SRAM_SIZE
                        BNE   @verify_ascending

                        SEP   #$20
                        JSR.W self_test_sram_unlock

                        REP   #$20
                        LDA.W #$FFFF
                        LDX.W #$0000
     @write_descending: STA.L (SRAM_BANK << 16 | SRAM_START),X
                        INX
                        INX
                        DEC   A
                        CPX.W #SRAM_SIZE
                        BNE   @write_descending

                        SEP   #$20
                        JSR.W self_test_sram_lock

                        REP   #$20
                        LDA.W #$FFFF
                        LDX.W #$0000
    @verify_descending: CMP.L (SRAM_BANK << 16 | SRAM_START),X
                        BNE   @exit_error
                        INX
                        INX
                        DEC   A
                        CPX.W #SRAM_SIZE
                        BNE   @verify_descending

                        ; Exit OK
                        SEP   #$20
                        JSR.W self_test_backup_ram_restore
                        PLP
                        CLC
                        RTS

           @exit_error: SEP   #$20
                        JSR.W self_test_backup_ram_restore
                        PLP
                        SEC
                        RTS


; Write backed-up contents back to SRAM
self_test_backup_ram_restore:
                        JSR.W self_test_sram_unlock

                        LDX.W #$0000
                 @loop: LDA.L tmp_self_test_backup_ram_backup,X
                        STA.L (SRAM_BANK << 16 | SRAM_START),X
                        INX
                        CPX.W #SRAM_SIZE
                        BNE   @loop

                        JSR.W self_test_sram_lock

                        RTS

; Unlock SRAM:
;   1. Write $FF to first register
;   2. Write $0F..$00 to second register
 self_test_sram_unlock: LDA.B #$FF
                        STA.W SCD_SRAM_UNLOCK1

                        LDA.B #$0F
                 @loop: STA.W SCD_SRAM_UNLOCK2
                        DEC   A
                        BNE   @loop
                        RTS

   self_test_sram_lock:
.IFDEF ORIGINAL
                        ; The conventional method of locking the SRAM is to
                        ; write $00 to SCD_SRAM_LOCK. Supposedly this has the
                        ; same effect?
                        LDA.B #$FF
                        STA.W SCD_SRAM_UNLOCK1
.ELSE
                        STZ.W SCD_SRAM_LOCK
.ENDIF
                        RTS


.IFDEF ORIGINAL
                        ; data/padding until end of section
                        .ORGA $97DF
                        .INCLUDE "padding/bank_01_1.inc"
.ENDIF

                        ; CXD1800 test routines
                        .ORGA $A000
                        .INCLUDE "diag/cxd1800_test.asm"
.IFDEF ORIGINAL
                        ; looks like data/padding/junk from here to :C000
                        .ORGA $AF36
                        .INCLUDE "padding/bank_01_2.inc"
.ENDIF

                        ; ADPCM test routines
                        .ORGA $C000
                        .INCLUDE "diag/adpcm_test.asm"
.IFDEF ORIGINAL
                        ; padding
                        .ORGA $CD5C
                        .INCLUDE "padding/bank_01_3.inc"
.ENDIF

                        ; Mechacon test routines
                        .ORGA $E000
                        .INCLUDE "diag/comms_test.asm"

.IFDEF ORIGINAL
                        ; padding
                        .ORGA $EF3F
                        .DB $00

                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

                        .DB $00,$10,$01,$80,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00

                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00

                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                        .DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$30,$00,$00,$00,$04
                        .DB $00,$00,$00,$00,$00,$00,$00,$00
                        .DB $00,$00,$00,$00,$00,$00,$00,$00
.ENDIF

                        .ORGA $F000
     data_ebook_screen: .INCLUDE "gfx/ebook_screen.inc"

.IFDEF ORIGINAL
                        ; padding
                        .ORGA $F700
                        .INCLUDE "padding/bank_01_4.inc"
.ENDIF
