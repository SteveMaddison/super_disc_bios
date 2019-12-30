;
; CXD1800 test routines
;
       cxd_test_screen: REP   #$10
                        SEP   #$20
                        PHK
                        PLB
                        REP   #$20
                        LDA.W #$0200
                        TCD
                        SEP   #$20
                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP
                        SEI
                        STZ.W NMITIMEN

                        JSR.W cxd_init_graphics
                        JSR.W cxd_init_vectors_and_screen

                        LDA.B #(NMITIMEN_VBLANK | NMITIMEN_JOYPAD)
                        STA.W NMITIMEN
                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        CLI

 cxd_joypad_loop_write: JSR.W cxd_process_joypad
                        SEP   #$10
                        LDA.B cxd_write_cursor & $FF
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W cxd_register_buffer_write,X
                        STA.B cxd_write_buffer & $FF
                        SEP   #$20
                        REP   #$10
                        LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_SELECT
                        BEQ   @check_b

                        ; Select pressed, return to the main diagnostic menu.
                        JML.L diagnostic_menu

              @check_b: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_B
                        BEQ   @check_y
                        JSR.W cxd_write_register
                        BRA   cxd_joypad_loop_write

              @check_y: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_Y
                        BEQ   @check_x
                        LDA.B #CXD_TEST_MODE_WRITE
                        STA.B cxd_test_mode & $FF
                        JMP.W cxd_joypad_loop_read

              @check_x: LDA.B cxd_joypad_l & $FF
                        AND.B #JOYPAD_L_X
                        BEQ   @check_r
                        LDA.B cxd_irq_mode & $FF
                        BEQ   @set_irq_1time
                        CMP.B #CXD_IRQ_1TIME
                        BEQ   @set_irq_ignore

                        ; Set IRQ all
                        STZ.B cxd_irq_mode & $FF
                        CLI
                        LDX.W #screen_data_cxd_irq_all
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read
                        JMP.W cxd_joypad_loop_write

        @set_irq_1time: INC.B cxd_irq_mode & $FF
                        LDX.W #screen_data_cxd_irq_1time
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read
                        JMP.W cxd_joypad_loop_write

       @set_irq_ignore: INC.B cxd_irq_mode & $FF
                        SEI
                        LDX.W #screen_data_cxd_irq_ignore
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read
                        JMP.W cxd_joypad_loop_write

              @check_r: LDA.B cxd_joypad_l & $FF
                        AND.B #JOYPAD_L_R
                        BEQ   @check_l
                        JSR.W cxd_unhightlight_write_cursor
                        STZ.B cxd_digit_offset & $FF
                        LDA.B cxd_write_cursor & $FF
                        INC   A
                        CMP.B #cxd_test_mode & $FF
                        BMI   @inc_ok
                        ; If past the end, wrap to zero.
                        LDA.B #$00
               @inc_ok: STA.B cxd_write_cursor & $FF
                        JSR.W cxd_hightlight_write_cursor
                        JMP.W cxd_joypad_loop_write

              @check_l: LDA.B cxd_joypad_l & $FF
                        AND.B #JOYPAD_L_L
                        BEQ   @check_dpad
                        JSR.W cxd_unhightlight_write_cursor
                        STZ.B cxd_digit_offset & $FF
                        LDA.B cxd_write_cursor & $FF
                        DEC   A
                        CMP.B #$FF
                        BNE   @dec_ok
                        LDA.B #$09
               @dec_ok: STA.B cxd_write_cursor & $FF
                        JSR.W cxd_hightlight_write_cursor
                        JMP.W cxd_joypad_loop_write

           @check_dpad: LDA.B cxd_joypad_h & $FF
                        AND.B #(JOYPAD_H_UP | JOYPAD_H_DOWN | JOYPAD_H_LEFT | JOYPAD_H_RIGHT)
                        BNE   @check_right
                        JMP.W cxd_joypad_loop_write

          @check_right: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_RIGHT
                        BEQ   @check_left
                        JSR.W cxd_unhightlight_write_cursor
                        SEP   #$10
                        LDA.B cxd_digit_offset & $FF
                        INC   A
                        LSR   A
                        LDY.B cxd_write_cursor & $FF
                        CMP.W data_cxd_write_length,Y
                        BEQ   @digit_inc_ok
                        INC.B cxd_digit_offset & $FF
         @digit_inc_ok: REP   #$10
                        JSR.W cxd_hightlight_write_cursor
                        BRA   @check_down

           @check_left: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_LEFT
                        BEQ   @check_down
                        JSR.W cxd_unhightlight_write_cursor
                        LDA.B cxd_digit_offset & $FF
                        BEQ   @digit_dec_ok
                        DEC   A
                        STA.B cxd_digit_offset & $FF
         @digit_dec_ok: JSR.W cxd_hightlight_write_cursor

           @check_down: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_DOWN
                        BEQ   @check_up
                        SEP   #$10
                        LDY.B cxd_write_cursor & $FF
                        LDA.W data_cxd_write_length,Y
                        ASL   A
                        SEC
                        SBC.B cxd_digit_offset & $FF
                        DEC   A
                        ASL   A
                        TAX
                        LDA.B cxd_write_cursor & $FF
                        ASL   A
                        TAY
                        REP   #$20
                        LDA.B cxd_write_buffer & $FF
                        SEC
                        SBC.W data_cxd_digit_values,X
                        STA.B cxd_write_buffer & $FF
                        STA.W cxd_register_buffer_write,Y
                        SEP   #$20
                        REP   #$10
                        JSR.W cxd_update_digits_write
                        JMP.W cxd_joypad_loop_write

             @check_up: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_UP
                        BEQ   @done
                        SEP   #$10
                        LDY.B cxd_write_cursor & $FF
                        LDA.W data_cxd_write_length,Y
                        ASL   A
                        SEC
                        SBC.B cxd_digit_offset & $FF
                        DEC   A
                        ASL   A
                        TAX
                        LDA.B cxd_write_cursor & $FF
                        ASL   A
                        TAY
                        REP   #$20
                        LDA.B cxd_write_buffer & $FF
                        CLC
                        ADC.W data_cxd_digit_values,X
                        STA.B cxd_write_buffer & $FF
                        STA.W cxd_register_buffer_write,Y
                        REP   #$10
                        SEP   #$20

                 @done: JSR.W cxd_update_digits_write
                        JMP.W cxd_joypad_loop_write


                        ; Values of each of the hexadecimal digits.
 data_cxd_digit_values: .DW $0001,$0010,$0100,$1000


  cxd_joypad_loop_read: JSR.W cxd_process_joypad
                        LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_SELECT
                        BEQ   @check_b

                        ; Select pressed, return to the main diagnostic menu.
                        JML.L diagnostic_menu

              @check_b: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_B
                        BEQ   @check_y
                        JSR.W cxd_read_register
                        JSR.W cxd_update_digits_read
                        BRA   cxd_joypad_loop_read

              @check_y: LDA.B cxd_joypad_h & $FF
                        AND.B #JOYPAD_H_Y
                        BEQ   @check_x
                        LDA.B #CXD_TEST_MODE_READ
                        STA.B cxd_test_mode & $FF
                        JMP.W cxd_joypad_loop_write

              @check_x: LDA.B cxd_joypad_l & $FF
                        AND.B #JOYPAD_L_X
                        BEQ   @check_r
                        LDA.B cxd_irq_mode & $FF
                        BEQ   @set_irq_1time
                        CMP.B #CXD_IRQ_1TIME
                        BEQ   @set_irq_ignore
                        STZ.B cxd_irq_mode & $FF
                        CLI
                        LDX.W #screen_data_cxd_irq_all
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read
                        JMP.W cxd_joypad_loop_read

        @set_irq_1time: INC.B cxd_irq_mode & $FF
                        LDX.W #screen_data_cxd_irq_1time
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read
                        JMP.W cxd_joypad_loop_read

       @set_irq_ignore: INC.B cxd_irq_mode & $FF
                        SEI
                        LDX.W #screen_data_cxd_irq_ignore
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read
                        JMP.W cxd_joypad_loop_read

              @check_r: LDA.B cxd_joypad_l & $FF
                        AND.B #JOYPAD_L_R
                        BEQ   @check_l
                        JSR.W cxd_unhighlight_read_cursor
                        LDA.B cxd_read_cursor & $FF
                        INC   A
                        CMP.B #$12
                        BMI   @inc_ok
                        LDA.B #$00
               @inc_ok: STA.B cxd_read_cursor & $FF
                        JSR.W cxd_highlight_read_cursor
                        JMP.W cxd_joypad_loop_read

              @check_l: LDA.B cxd_joypad_l & $FF
                        AND.B #JOYPAD_L_L
                        BEQ   @done
                        JSR.W cxd_unhighlight_read_cursor
                        LDA.B cxd_read_cursor & $FF
                        DEC   A
                        CMP.B #$FF
                        BNE   @dec_ok
                        LDA.B #$11
               @dec_ok: STA.B cxd_read_cursor & $FF
                        JSR.W cxd_highlight_read_cursor
                 @done: JMP.W cxd_joypad_loop_read


    cxd_write_register: SEI
                        SEP   #$10
                        LDA.B cxd_write_cursor & $FF
                        TAX
                        LDA.W data_cxd_write_index,X
                        STA.W SCD_CXD_INDEX
                        LDY.B #$00
                        LDA.W data_cxd_write_length,X
           @write_byte: PHA
                        LDA.W cxd_write_buffer,Y
                        STA.W SCD_CXD_DATA
                        INX
                        INY
                        PLA
                        DEC   A
                        BNE   @write_byte
                        REP   #$10
                        CLI
                        RTS

     cxd_read_register: SEI
                        SEP   #$10
                        LDA.B cxd_read_cursor & $FF
                        TAX
                        ASL   A
                        TAY
                        LDA.W data_cxd_read_index,X
                        STA.W SCD_CXD_INDEX
                        LDA.W data_cxd_read_length,X
            @read_byte: PHA
                        LDA.W SCD_CXD_DATA
                        STA.W cxd_register_buffer_read,Y
                        INY
                        PLA
                        DEC   A
                        BNE   @read_byte
                        REP   #$10
                        CLI
                        RTS


cxd_irq_read_and_update_digits:
                        LDA.B #1
        @loop_register: SEP   #$10
                        PHA
                        TAX
                        ASL   A
                        TAY
                        LDA.W data_cxd_read_index,X
                        STA.W SCD_CXD_INDEX

                        PHY
                        LDA.W data_cxd_read_length,X
            @read_byte: PHA
                        LDA.W SCD_CXD_DATA
                        STA.W cxd_register_buffer_read,Y
                        INY
                        PLA
                        DEC   A
                        BNE   @read_byte

                        PLY
                        REP   #$20
                        LDA.W cxd_register_buffer_read,Y
                        STA.B cxd_irq_read_buffer & $FF
                        LDA.W data_cxd_digit_offsets_read,Y
                        REP   #$10
                        TAY
                        SEP   #$20
                        LDA.W data_cxd_read_length,X
                        CMP.B #1
                        BEQ   @write

                        INY
                        INY
                        INY
                        INY

                @write: LDX.W #0
           @loop_digit: PHA
                        LDA.W cxd_irq_read_buffer,X
                        PHA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + SCREEN_WIDTH_B,Y
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 2,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 2 + SCREEN_WIDTH_B,Y
                        DEY
                        DEY
                        DEY
                        DEY
                        INX
                        PLA
                        DEC   A
                        BNE   @loop_digit

                        PLA
                        INC   A
                        CMP.B #$12
                        BNE   @loop_register
                        REP   #$10
                        RTS


 cxd_irq_data_transfer: LDA.W cxd_register_buffer_read + 14
                        BEQ   @mode2

                        LDA.W cxd_register_buffer_read + 20
                        AND.B #$20
                        BNE   @mode2

                        LDX.W #CD_SECTOR_SIZE
                        BRA   @transfer

                @mode2: LDX.W #CD_SECTOR_SIZE_MODE2

             @transfer: STX.B cxd_test_transfer_length & $FF
                        LDA.B #CXD_WR_DMAADRC_L
                        STA.W SCD_CXD_INDEX

                        LDA.W cxd_register_buffer_read + 24
                        CLC
                        ADC.B #$04
                        STA.W SCD_CXD_DATA      ; CXD_WR_DMAADRC_L
                        LDX.W #6                ; DMAADRC
                        JSR.W cxd_update_low_digits_write

                        LDA.W cxd_register_buffer_read + 25
                        STA.W SCD_CXD_DATA      ; CXD_WR_DMAADRC_H
                        LDX.W #6                ; DMAADRC
                        JSR.W cxd_update_high_digits_write

                        LDA.W cxd_test_transfer_length
                        STA.W SCD_CXD_DATA      ; CXD_WR_DMAXFRC_L
                        LDX.W #7                ; DMAXFRC
                        JSR.W cxd_update_low_digits_write

                        LDA.W cxd_test_transfer_length + 1
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A
                        ORA.B #$08
                        STA.W SCD_CXD_DATA      ; CXD_WR_DMAXFRC_H

                        LDX.W #7                ; DMAXFRC
                        JSR.W cxd_update_high_digits_write

                        LDA.B #DMAP_BBUS_TO_CPU
                        STA.W DMAP7
                        LDA.B #SCD_CXD_DATA & $FF
                        STA.W BBAD7
                        LDX.W #CXD_DIAG_BUFFER_ADDRESS
                        STX.W A1T7L
                        LDA.B #CXD_DIAG_BUFFER_BANK
                        STA.W A1B7
                        LDX.B cxd_test_transfer_length & $FF
                        STX.W DAS7L
                        LDA.B #MDMAEN_CHANNEL7
                        STA.W MDMAEN

                        LDA.B #CXD_WR_INTCLR
                        STA.W SCD_CXD_INDEX

                        LDA.W cxd_register_buffer_read + 2
                        ORA.B #CXD_INTCLR_DECINT
                        STA.W SCD_CXD_DATA

                        LDX.W #4                ; INTCLR
                        JSR.W cxd_update_one_byte_digits_write
                        RTS


cxd_update_one_byte_digits_write:
                        SEP   #$10
                        PHA
                        PHA
                        PHA
                        TXA
                        ASL   A
                        TAX
                        PLA
                        STA.W cxd_register_buffer_write,X
                        REP   #$20
                        LDA.W data_cxd_digit_offsets_write,X
                        REP   #$10
                        TAY
                        SEP   #$20
                        PLA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + SCREEN_WIDTH_B,Y
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 2,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 2 + SCREEN_WIDTH_B,Y
                        RTS

cxd_update_low_digits_write:
                        SEP   #$10
                        PHA
                        PHA
                        PHA
                        TXA
                        ASL   A
                        TAX
                        PLA
                        STA.W cxd_register_buffer_write,X
                        REP   #$20
                        LDA.W data_cxd_digit_offsets_write,X
                        REP   #$10
                        TAY
                        SEP   #$20
                        PLA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 4,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 68,Y
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 6,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 70,Y
                        RTS


cxd_update_high_digits_write:
                        SEP   #$10
                        PHA
                        PHA
                        PHA
                        TXA
                        ASL   A
                        TAX
                        PLA
                        STA.W cxd_register_buffer_write + 1,X
                        REP   #$20
                        LDA.W data_cxd_digit_offsets_write,X
                        REP   #$10
                        TAY
                        SEP   #$20
                        PLA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + SCREEN_WIDTH_B,Y
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 2,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 2 + SCREEN_WIDTH_B,Y
                        RTS


    cxd_process_joypad: LDX.B cxd_joypad_l_raw & $FF
                        CPX.B cxd_joypad_l & $FF
                        BNE   @no_change
                        LDA.B cxd_joypad_delay_h & $FF
                        CMP.B #32
                        BMI   cxd_process_joypad
                        AND.B #$03
                        BNE   cxd_process_joypad
                        INC.B cxd_joypad_delay_h & $FF
                        RTS

            @no_change: STZ.B cxd_joypad_delay_h & $FF
                        STX.B cxd_joypad_l & $FF
                        RTS


cxd_hightlight_write_cursor:
                        SEP   #$10
                        LDA.B cxd_write_cursor & $FF
                        ASL   A
                        TAX
                        PHX
                        REP   #$20
                        LDA.W data_cxd_cursor_offsets_write,X
                        REP   #$10
                        TAX
                        SEP   #$20

                        LDA.B #7
                 @loop: PHA
                        LDA.B #$28
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 65,X
                        INX
                        INX
                        PLA
                        DEC   A
                        BNE   @loop

                        SEP   #$10
                        PLX
                        REP   #$20
                        LDA.W data_cxd_digit_offsets_write,X
                        REP   #$10
                        CLC
                        ADC.B cxd_digit_offset & $FF
                        ADC.B cxd_digit_offset & $FF
                        TAX
                        SEP   #$20
                        LDA.B #$2C
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 65,X
                        RTS


cxd_unhightlight_write_cursor:
                        SEP   #$10
                        LDA.B cxd_write_cursor & $FF
                        ASL   A
                        TAX
                        PHX
                        REP   #$20
                        LDA.W data_cxd_cursor_offsets_write,X
                        REP   #$10
                        TAX
                        SEP   #$20

                        LDA.B #7
                 @loop: PHA
                        LDA.B #$20
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 65,X
                        INX
                        INX
                        PLA
                        DEC   A
                        BNE   @loop

                        SEP   #$10
                        PLX
                        REP   #$20
                        LDA.W data_cxd_digit_offsets_write,X
                        REP   #$10
                        CLC
                        ADC.B cxd_digit_offset & $FF
                        ADC.B cxd_digit_offset & $FF
                        TAX
                        SEP   #$20
                        LDA.B #$20
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 65,X
                        RTS


data_cxd_cursor_offsets_write:
                        .DW $0200
                        .DW $0210
                        .DW $0220
                        .DW $0230
                        .DW $0300
                        .DW $0310
                        .DW $0320
                        .DW $0330
                        .DW $0400
                        .DW $0410


cxd_highlight_read_cursor:
                        SEP   #$10
                        LDA.B cxd_read_cursor & $FF
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W data_cxd_cursor_offsets_read,X
                        REP   #$10
                        TAX
                        SEP   #$20

                        LDA.B #7
                 @loop: PHA
                        LDA.B #$28
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 65,X
                        INX
                        INX
                        PLA
                        DEC   A
                        BNE   @loop
                        RTS


cxd_unhighlight_read_cursor:
                        SEP   #$10
                        LDA.B cxd_read_cursor & $FF
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W data_cxd_cursor_offsets_read,X
                        REP   #$10
                        TAX
                        SEP   #$20

                        LDA.B #7
                 @loop: PHA
                        LDA.B #$20
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 65,X
                        INX
                        INX
                        PLA
                        DEC   A
                        BNE   @loop
                        RTS


data_cxd_cursor_offsets_read:
                        .DW $0200
                        .DW $0210
                        .DW $0220
                        .DW $0230
                        .DW $0300
                        .DW $0310
                        .DW $0320
                        .DW $0330
                        .DW $0400
                        .DW $0410
                        .DW $0420
                        .DW $0430
                        .DW $0500
                        .DW $0510
                        .DW $0520
                        .DW $0530
                        .DW $0600
                        .DW $0610


cxd_update_digits_write:
                        SEP   #$10
                        LDA.B cxd_write_cursor & $FF
                        TAY
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W data_cxd_digit_offsets_write,X
                        REP   #$10
                        TAX
                        SEP   #$20
                        LDA.W data_cxd_write_length,Y
                        CMP.B #1
                        BEQ   @write

                        ; Skip to right-most digits.
                        INX
                        INX
                        INX
                        INX

                @write: LDY.W #0
                 @loop: PHA
                        LDA.W cxd_write_buffer,Y
                        PHA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE,X
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + SCREEN_WIDTH_B,X
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 2,X
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE + 2 + SCREEN_WIDTH_B,X
                        DEX
                        DEX
                        DEX
                        DEX
                        INY
                        PLA
                        DEC   A
                        BNE   @loop
                        RTS


data_cxd_digit_offsets_write:
                        .DW $0286
                        .DW $0296
                        .DW $02A6
                        .DW $02B6
                        .DW $0386
                        .DW $0396
                        .DW $03A6
                        .DW $03B6
                        .DW $0486
                        .DW $0496


cxd_update_digits_read: SEP   #$10
                        LDA.B cxd_read_cursor & $FF
                        TAY
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W cxd_register_buffer_read,X
                        STA.B cxd_read_buffer & $FF
                        LDA.W data_cxd_digit_offsets_read,X
                        REP   #$10
                        TAX
                        SEP   #$20
                        LDA.W data_cxd_read_length,Y
                        CMP.B #1
                        BEQ   @write

                        ; Skip to right-most digits.
                        INX
                        INX
                        INX
                        INX

                @write: LDY.W #0
                 @loop: PHA
                        LDA.W cxd_read_buffer,Y
                        PHA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ,X
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + SCREEN_WIDTH_B,X
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 2,X
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ + 2 + SCREEN_WIDTH_B,X
                        DEX
                        DEX
                        DEX
                        DEX
                        INY
                        PLA
                        DEC   A
                        BNE   @loop
                        RTS


data_cxd_digit_offsets_read:
                        .DW $0286
                        .DW $0296
                        .DW $02A6
                        .DW $02B6
                        .DW $0386
                        .DW $0396
                        .DW $03A6
                        .DW $03B6
                        .DW $0486
                        .DW $0496
                        .DW $04A6
                        .DW $04B6
                        .DW $0586
                        .DW $0596
                        .DW $05A6
                        .DW $05B6
                        .DW $0686
                        .DW $0696


       cxd_irq_handler: REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20
                        LDA.B cxd_irq_mode & $FF
                        CMP.B #CXD_IRQ_IGNORE
                        BEQ   @mask_all
                        JSR.W cxd_irq_read_and_update_digits
                        JSR.W cxd_irq_data_transfer
                        LDA.B cxd_irq_mode & $FF
                        CMP.B #CXD_IRQ_ALL
                        BEQ   @exit

             @mask_all: LDA.B #CXD_WR_INTMSK
                        STA.W SCD_CXD_INDEX
                        LDA.B #$00
                        STA.W SCD_CXD_DATA

                        LDX.W #3                ; INTMSK
                        JSR.W cxd_update_one_byte_digits_write

                 @exit: REP   #$30
                        PLY
                        PLX
                        PLA
                        RTI


cxd_init_vectors_and_screen:
                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode
                        LDX.W #cxd_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #cxd_irq_handler
                        STX.W supercd_bios_irq_vector_address
                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank
                        STZ.B cxd_joypad_delay_h & $FF
                        STZ.B cxd_test_mode & $FF
                        STZ.B cxd_digit_offset & $FF
                        STZ.B (cxd_digit_offset + 1) & $FF
                        STZ.B cxd_irq_mode & $FF
                        STZ.B cxd_write_cursor & $FF
                        STZ.B cxd_read_cursor & $FF

                        LDX.W #screen_data_cxd_write
                        JSR.W cxd_render_text_write
                        LDX.W #screen_data_cxd_read
                        JSR.W cxd_render_text_read
                        JSR.W cxd_hightlight_write_cursor
                        JSR.W cxd_highlight_read_cursor
                        LDX.W #screen_data_cxd_irq_all
                        JSR.W cxd_render_text_write
                        JSR.W cxd_render_text_read

                        LDX.W #0
         @clear_buffer: STZ.W cxd_register_buffer_write,X
                        INX
                        CPX.W #CXD_WRITE_BUFFER_LENGTH + CXD_READ_BUFFER_LENGTH
                        BNE   @clear_buffer

                        LDA.B #$04
                        STA.W SCD_MYSTERY
                        RTS


                        ; Conversion from internal representation of CXD1800
                        ; registers and the real ones (different for reads and
                        ; writes) and the sizes of these registers.
  data_cxd_write_index: .DB $01,$02,$03,$04,$05,$06,$07,$09,$0B,$0D
   data_cxd_read_index: .DB $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0E,$0F,$18,$1A,$1C
 data_cxd_write_length: .DB $01,$01,$01,$01,$01,$01,$02,$02,$02,$01
  data_cxd_read_length: .DB $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$01,$01,$02,$02,$02


screen_data_cxd_read:   .DW $0000
                        .DW $2000
                        .DW (32*14)
                        .ASC "                                "
                        .ASC "  CXD1800 TEST PRGRAM VER 1.00  "
                        .ASC "    READ MODE  /                "
                        .ASC "                                "
                        .ASC "00-DMA  01-INST 02-STS  03-HFLG "
                        .ASC "   00      00      00      00   "
                        .ASC "X4-HMIN X5-HSEC X6-HBLK X7-HMOD "
                        .ASC "   00      00      00      00   "
                        .ASC "08-SFIL 09-SCH  0A-SMOD 0B-SCI  "
                        .ASC "   00      00      00      00   "
                        .ASC "0C-CMAD XE-MDFM XF-ADPC 18-DMXF "
                        .ASC "   0000    00      00      0000 "
                        .ASC "1A-DMAD 1C-DRAD                 "
                        .ASC "   0000    0000                 "

screen_data_cxd_write:  .DW $0000
                        .DW $2000
                        .DW (32*14)
                        .ASC "                                "
                        .ASC "  CXD1800 TEST PRGRAM VER 1.00  "
                        .ASC "    WRITE MODE /                "
                        .ASC "                                "
                        .ASC "X1-DRIF X2-CHCT X3-DECT X4-INMS "
                        .ASC "   00      00      00      00   "
                        .ASC "X5-INCL X6-CI   X7-DMAD X9-DMXF "
                        .ASC "   00      00      0000    0000 "
                        .ASC "XB-DRAD 0D-PLBA                 "
                        .ASC "   0000    00                   "
                        .ASC "                                "
                        .ASC "                                "
                        .ASC "                                "
                        .ASC "                                "

screen_data_cxd_irq_ignore:
                        .DW $0211
                        .DW $2800
                        .DW 10
                        .ASC "IRQ-IGNORE"

screen_data_cxd_irq_1time:
                        .DW $0211
                        .DW $2800
                        .DW 10
                        .ASC "IRQ-1TIME "

screen_data_cxd_irq_all:
                        .DW $0211
                        .DW $2800
                        .DW 10
                        .ASC "IRQ-ALL   "

; unused string??
screen_data_cxd_data_dump:
                        .DW $0204
                        .DW $2800
                        .DW 11
                        .ASC "DATA DUMP /"

   cxd_unused_ppu_init: LDA.B #$8F
                        STA.W INIDISP
                        LDA.B #$63
                        STA.W OBSEL
                        LDA.B #$00
                        STA.W OAMADDL
                        STA.W OAMADDH
                        LDA.B #$09
                        STA.W BGMODE
                        LDA.B #$00
                        STA.W MOSAIC
                        LDA.B #$28
                        STA.W BG1SC
                        STZ.W BG2SC
                        LDA.B #$60
                        STA.W BG3SC
                        STZ.W BG4SC
                        LDA.B #$00
                        STA.W BG12NBA
                        LDA.B #$04
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
                        STA.W BG3VOFS
                        STA.W BG3VOFS
                        STA.W BG4HOFS
                        STA.W BG4HOFS
                        STA.W BG4VOFS
                        STA.W BG4VOFS
                        LDA.B #$80
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
                        LDA.B #$05
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
                        LDA.B #$01
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

     cxd_init_graphics: LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_clear_vram_cxd
                        JSR.W cxd_dma_copy

                        LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_greyscale_cxd
                        JSR.W cxd_dma_copy

                        STZ.W CGADD
                        LDX.W #data_dma_clear_palette_cxd
                        JSR.W cxd_dma_copy

                        LDX.W #TILE_OFFSET_BG3
                        STX.W VMADDL
                        REP   #$20
                        LDX.W #0
   @loop_font_colour_1: LDA.L gfx_font_tiles,X
                        AND.W #$00FF
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @loop_font_colour_1

                        LDX.W #TILE_OFFSET_BG3 + FONT_TILESET_SIZE
                        STX.W VMADDL
                        LDX.W #0
   @loop_font_colour_2: LDA.L gfx_font_tiles,X
                        AND.W #$00FF
                        EOR.W #$FFFF
                        STA.W VMDATAL
                        INX
                        CPX.W #FONT_TILESET_SIZE
                        BNE   @loop_font_colour_2

                        ; Load palette
                        SEP   #$20
                        STZ.W CGADD
                        LDX.W #data_dma_palette_cxd
                        JSR.W cxd_dma_copy

                        REP   #$20
                        LDX.W #0
                        LDA.W #$2048    ; Space character
@loop_clear_write_screen:
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_WRITE,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @loop_clear_write_screen

                        LDX.W #0
                        LDA.W #$2048    ; Space character
@loop_clear_read_screen:
                        STA.W WRAM_ADDR_BG3_DIAG_CXD_READ,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @loop_clear_read_screen

                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_cxd_write
                        JSR.W cxd_dma_copy
                        RTS


; Data for DMA copy routine:
;  byte   0: DMA params
;  byte   1: B-Bus address ($21xx)
;  byte 2-4: source (low, high, bank)
;  byte 5-6: count (low, ligh)
data_dma_clear_vram_cxd:
                        .DB $09
                        .DB $18
                        .DW data_dma_zero_byte_cxd
                        .DB $01
                        .DW $0000

data_dma_gfx_greyscale_cxd:
                        .DB $01
                        .DB VMDATAL & $FF
                        .DW gfx_tiles_greyscale_cxd
                        .DB $01
                        .DW $0200

data_dma_clear_palette_cxd:
                        .DB $09
                        .DB CGDATA & $FF
                        .DW data_dma_zero_byte_cxd
                        .DB $01
                        .DW $0100

data_dma_palette_cxd:   .DB $00
                        .DB CGDATA & $FF
                        .DW data_cxd_palette
                        .DB $01
                        .DW 32

data_dma_gfx_wram_to_vram_cxd_write:
                        .DB $01
                        .DB VMDATAL & $FF
                        .DW WRAM_ADDR_BG3_DIAG_CXD_WRITE
                        .DB $00
                        .DW SCREEN_SIZE_B

data_dma_gfx_wram_to_vram_cxd_read:
                        .DB $01
                        .DB VMDATAL & $FF
                        .DW WRAM_ADDR_BG3_DIAG_CXD_READ
                        .DB $00
                        .DW SCREEN_SIZE_B

data_dma_gfx_wram_to_vram_cxd_unknown:
                        .DB $01
                        .DB VMDATAL & $FF
                        .DW WRAM_ADDR_BG3_DIAG_CXD_UNKNOWN
                        .DB $00
                        .DW SCREEN_SIZE_B

data_dma_zero_byte_cxd: .DB $00


.IFDEF ORIGINAL
                        .ORGA $AC02
.ENDIF
      cxd_nmi_handler:  REP   #$20
                        REP   #$10
                        PHA
                        PHB
                        PHX
                        PHY
                        SEP   #$20

                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDA.W cxd_test_mode
                        CMP.B #CXD_TEST_MODE_WRITE
                        BEQ   @update_write_screen
                        CMP.B #CXD_TEST_MODE_UNKNOWN
                        BEQ   @update_unknown_screen
                        LDX.W #data_dma_gfx_wram_to_vram_cxd_write
                        BRA   @dma_copy
  @update_write_screen: LDX.W #data_dma_gfx_wram_to_vram_cxd_read
                        BRA   @dma_copy
@update_unknown_screen: LDX.W #data_dma_gfx_wram_to_vram_cxd_unknown

             @dma_copy: JSR.W cxd_dma_copy
                        LDA.W JOY1H
                        STA.W cxd_joypad_h_raw
                        LDA.W JOY1L
                        STA.W cxd_joypad_l_raw
                        LDA.W cxd_joypad_delay_l
                        INC   A
                        BEQ   @rollover
                        STA.W cxd_joypad_delay_l
             @rollover: LDA.W cxd_joypad_delay_h
                        INC   A
                        CMP.B #(FPS / 5 * 4)
                        BNE   @save_delay_h
                        LDA.B #$20
         @save_delay_h: STA.W cxd_joypad_delay_h
                        REP   #$20
                        PLY
                        PLX
                        PLB
                        PLA
                        RTI


          cxd_dma_copy: LDA.W 0,X
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


      data_cxd_palette: .DB $00,$28,$FF,$7F,$00,$00,$00,$00
                        .DB $00,$28,$00,$70,$00,$00,$00,$00
                        .DB $00,$28,$E0,$7F,$00,$00,$00,$00
                        .DB $00,$00,$FF,$03,$00,$00,$00,$00

             data_aca6: .DB $5F,$29,$E0,$7F,$00,$00,$00,$00


 cxd_render_text_write: PHX
                        LDA.W 1,X
                        XBA
                        LDA.W 0,X
                        ASL   A
                        ASL   A
                        REP   #$20
                        LSR   A
                        CLC
                        ADC.W #WRAM_ADDR_BG3_DIAG_CXD_WRITE
                        TAY
                        LDA.W 2,X
                        STA.W cxd_tmp_bg_attributes
                        LDA.W 4,X
                 @loop: PHA
                        LDA.W 6,X
                        INX
                        AND.W #$00FF
                        ASL   A
                        ORA.W cxd_tmp_bg_attributes
                        STA.W 0,Y
                        INC   A
                        STA.W SCREEN_WIDTH_B,Y
                        INY
                        INY
                        TYA
                        AND.W #SCREEN_WIDTH_B - 1
                        BNE   @same_row
                        TYA
                        CLC
                        ADC.W #SCREEN_WIDTH_B
                        TAY
             @same_row: PLA
                        DEC   A
                        BNE   @loop
                        SEP   #$20
                        PLX
                        RTS

 cxd_render_text_read: PHX
                        LDA.W 1,X
                        XBA
                        LDA.W 0,X
                        ASL   A
                        ASL   A
                        REP   #$20
                        LSR   A
                        CLC
                        ADC.W #WRAM_ADDR_BG3_DIAG_CXD_READ
                        TAY
                        LDA.W 2,X
                        STA.W cxd_tmp_bg_attributes
                        LDA.W 4,X
                 @loop: PHA
                        LDA.W 6,X
                        INX
                        AND.W #$00FF
                        ASL   A
                        ORA.W cxd_tmp_bg_attributes
                        STA.W 0,Y
                        INC   A
                        STA.W SCREEN_WIDTH_B,Y
                        INY
                        INY
                        TYA
                        AND.W #SCREEN_WIDTH_B - 1
                        BNE   @same_row
                        TYA
                        CLC
                        ADC.W #SCREEN_WIDTH_B
                        TAY
             @same_row: PLA
                        DEC   A
                        BNE   @loop
                        SEP   #$20
                        PLX
                        RTS

.IFDEF ORIGINAL
                        .ORGA $AD36
.ENDIF
gfx_tiles_greyscale_cxd:
                        .INCLUDE "gfx/greyscale.inc"
