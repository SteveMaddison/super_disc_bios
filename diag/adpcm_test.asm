;
; ADPCM test routines
;
    adpcm_test_routine: REP   #$10
                        REP   #$20
                        LDA.W #ADPCM_DIRECT
                        TCD
                        SEP   #$20
                        PHK
                        PLB

                        SEI
                        STZ.W NMITIMEN
                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode
                        LDX.W #adpcm_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #adpcm_irq_hanlder
                        STX.W supercd_bios_irq_vector_address
                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank

                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP

                        JSR.W adpcm_init_graphics

                        STZ.B adpcm_file & $FF
                        STZ.B adpcm_channel & $FF
                        STZ.B adpcm_cursor & $FF
                        STZ.B adpcm_command_toggle & $FF

                        LDA.B #(NMITIMEN_VBLANK | NMITIMEN_JOYPAD)
                        STA.W NMITIMEN

                        LDA.B #$0C
                        STA.W SCD_MYSTERY

                        CLI

                        LDX.W #data_tilemap_adpcm_menu
                        JSR.W adpcm_render_text

                        JSR.W adpcm_highlight_cursor

                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP

                        JSR.W adpcm_init_cxd_and_mechacon

                        LDA.B #DIAG_MECHACON_REQ_SUB_Q
                        JSR.W adpcm_send_mechacon_command

     adpcm_joypad_loop: JSR.W adpcm_process_joypad
                        LDA.B adpcm_joypad_h & $FF
                        AND.B #JOYPAD_H_SELECT
                        BEQ   @check_b

                        ; Select pressed, return to main diagnostics menu.
                        JMP.W diagnostic_menu

              @check_b: LDA.B adpcm_joypad_h & $FF
                        AND.B #JOYPAD_H_B
                        BEQ   @check_r

                        ; B pressed, seek and play.
                        SEP   #$10
                        LDX.B #0
                        LDY.B #0
           @set_mmssff: LDA.B 8,X
                        PHX
                        TAX
                        LDA.W data_bcd_minutes,X
                        PHA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        STA.W adpcm_param_buffer_mmssff,Y       ; High nibble
                        INY
                        PLA
                        AND.B #$0F
                        STA.W adpcm_param_buffer_mmssff,Y       ; Low nibble
                        INY
                        PLX
                        INX
                        CPX.B #$03              ; 3 bytes == 6 nibbles
                        BNE   @set_mmssff

                        REP   #$10
                        LDA.B #DIAG_MECHACON_SEEK_MMSSFF
                        JSR.W adpcm_send_mechacon_command
                        LDA.B #DIAG_MECHACON_PLAY
                        JSR.W adpcm_send_mechacon_command
                        BRA   adpcm_joypad_loop

              @check_r: LDA.B adpcm_joypad_l & $FF
                        AND.B #JOYPAD_L_R
                        BEQ   @check_l
                        JSR.W adpcm_unhighlight_cursor
                        LDA.B adpcm_cursor & $FF
                        INC   A
                        CMP.B #$05
                        BMI   @cursor_inc_ok
                        LDA.B #$00
        @cursor_inc_ok: STA.B adpcm_cursor & $FF
                        JSR.W adpcm_highlight_cursor
                        JMP.W adpcm_joypad_loop

              @check_l: LDA.B adpcm_joypad_l & $FF
                        AND.B #JOYPAD_L_L
                        BEQ   @check_y
                        JSR.W adpcm_unhighlight_cursor
                        LDA.B adpcm_cursor & $FF
                        BNE   @cursor_dec_ok
                        LDA.B #$05
        @cursor_dec_ok: DEC   A
                        STA.B adpcm_cursor & $FF
                        JSR.W adpcm_highlight_cursor
                        JMP.W adpcm_joypad_loop

              @check_y: LDA.B adpcm_joypad_h & $FF
                        AND.B #JOYPAD_H_Y
                        BEQ   @check_up
                        LDA.B adpcm_command_toggle & $FF
                        EOR.B #$01
                        STA.B adpcm_command_toggle & $FF
                        CLC
                        ADC.B #DIAG_MECHACON_REQ_SUB_Q
                        JSR.W adpcm_send_mechacon_command
                        JMP.W adpcm_joypad_loop

             @check_up: LDA.B adpcm_joypad_h & $FF
                        AND.B #JOYPAD_H_UP
                        BEQ   @check_down
                        SEP   #$10
                        LDX.B adpcm_cursor & $FF
                        LDA.W adpcm_params,X
                        CMP.W data_adpcm_digit_offsets,X
                        BNE   @digit_inc_ok
                        LDA.B #$FF
         @digit_inc_ok: INC   A
                        STA.W adpcm_params,X
                        REP   #$10
                        JSR.W adpcm_update_params
                        JMP.W adpcm_joypad_loop

           @check_down: LDA.B adpcm_joypad_h & $FF
                        AND.B #JOYPAD_H_DOWN
                        BEQ   @done
                        SEP   #$10
                        LDX.B adpcm_cursor & $FF
                        LDA.W adpcm_params,X
                        BNE   @digit_dec_ok
                        LDA.W data_adpcm_digit_offsets,X
                        INC   A
         @digit_dec_ok: DEC   A
                        STA.W adpcm_params,X
                        REP   #$10
                        JSR.W adpcm_update_params
                        JMP.W adpcm_joypad_loop

           @done: JMP.W adpcm_joypad_loop


data_adpcm_digit_offsets:
                        .DB $63,$0F,$4A,$3B,$4A


   adpcm_update_mmssff: SEP   #$10
                        LDA.W adpcm_hdr_min
                        CMP.B #$75
                        BPL   @draw_invalid
                        LDA.W adpcm_hdr_sec
                        CMP.B #$60
                        BPL   @draw_invalid
                        LDA.W adpcm_hdr_block
                        CMP.B #$75
                        BPL   @draw_invalid

                        LDX.B #$00
                        LDY.B #$00
          @draw_mmssff: LDA.B adpcm_hdr_mmssff & $FF,X
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG + $4A4,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG + $4E4,Y
                        LDA.B $11,X
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG + $4A6,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG + $4E6,Y
                        INX
                        TYA
                        CLC
                        ADC.B #6
                        TAY
                        CMP.B #18
                        BNE   @draw_mmssff

                        REP   #$10
                        RTS

         @draw_invalid: LDY.B #$00
                 @loop: LDA.B #$4C              ; Top of "-" character
                        STA.W WRAM_ADDR_BG3_DIAG + $4A4,Y
                        STA.W WRAM_ADDR_BG3_DIAG + $4A6,Y
                        INC   A                 ; Bottom of "-" character
                        STA.W WRAM_ADDR_BG3_DIAG + $4E4,Y
                        STA.W WRAM_ADDR_BG3_DIAG + $4E6,Y
                        TYA
                        CLC
                        ADC.B #6
                        TAY
                        CMP.B #18
                        BNE   @loop

                        REP   #$10
                        RTS


adpcm_update_ci_text:   LDA.W adpcm_sts
                        AND.B #CXD_RD_STS_ADPBSY
                        BEQ   @clear

                        ; Extract the various information form the CI field.
                        LDA.B adpcm_ci & $FF
                        AND.B #$03
                        INC   A
                        STA.B adpcm_mono_stereo & $FF

                        LDA.B adpcm_ci & $FF
                        LSR   A
                        LSR   A
                        PHA
                        AND.B #$0F
                        INC   A
                        STA.B adpcm_level & $FF

                        PLA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        AND.B #$01
                        INC   A
                        STA.B adpcm_emphasis & $FF
                        BRA   @update_text

                @clear: ; Index zero in each table is all spaces.
                        STZ.B adpcm_mono_stereo & $FF
                        STZ.B adpcm_level & $FF
                        STZ.B adpcm_emphasis & $FF

          @update_text: ; Update mono/stereo text.
                        REP   #$20
                        LDA.B adpcm_mono_stereo & $FF
                        AND.W #$0003
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A         ; x16 = record length
                        CLC
                        ADC.W #data_text_table_mono_stereo
                        TAX
                        SEP   #$20
                        JSR.W adpcm_render_text

                        ; Update level text.
                        REP   #$20
                        LDA.B adpcm_level & $FF
                        AND.W #$0007
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A         ; x16 = record length
                        CLC
                        ADC.W #data_text_table_lev
                        TAX
                        SEP   #$20
                        JSR.W adpcm_render_text

                        ; Update emphasis text.
                        REP   #$20
                        LDA.B adpcm_emphasis & $FF
                        AND.W #$0003
                        ASL   A
                        ASL   A
                        ASL   A
                        ASL   A         ; x16 = record length
                        CLC
                        ADC.W #data_text_table_emp
                        TAX
                        SEP   #$20
                        JSR.W adpcm_render_text
                        RTS


   adpcm_update_params: SEP   #$10
                        LDA.B adpcm_cursor & $FF
                        TAX
                        ASL   A
                        TAY
                        LDA.W adpcm_params,X
                        TAX
                        LDA.W data_bcd_minutes,X
                        PHA
                        PHA
                        REP   #$20
                        LDA.W data_adpcm_cursor_offsets,Y
                        REP   #$10
                        TAY
                        SEP   #$20
                        PLA
                        LSR   A
                        LSR   A
                        LSR   A
                        LSR   A
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG + SCREEN_WIDTH_B,Y
                        PLA
                        AND.B #$0F
                        ASL   A
                        STA.W WRAM_ADDR_BG3_DIAG + 2,Y
                        INC   A
                        STA.W WRAM_ADDR_BG3_DIAG + 2 + SCREEN_WIDTH_B,Y
                        RTS


adpcm_highlight_cursor: SEP   #$10
                        LDA.B adpcm_cursor & $FF
                        ASL   A         ; x2 to make word pointer
                        TAX
                        REP   #$20
                        LDA.W data_adpcm_cursor_offsets,X
                        REP   #$10
                        TAX
                        SEP   #$20
                        LDA.B #$2C      ; Attributes (yellow)
                        STA.W WRAM_ADDR_BG3_DIAG + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG + 1 + SCREEN_WIDTH_B,X
                        STA.W WRAM_ADDR_BG3_DIAG + 3,X
                        STA.W WRAM_ADDR_BG3_DIAG + 3 + SCREEN_WIDTH_B,X
                        RTS


adpcm_unhighlight_cursor:
                        SEP   #$10
                        LDA.B adpcm_cursor & $FF
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W data_adpcm_cursor_offsets,X
                        REP   #$10
                        TAX
                        SEP   #$20
                        LDA.B #$20      ; Attributes (white)
                        STA.W WRAM_ADDR_BG3_DIAG + 1,X
                        STA.W WRAM_ADDR_BG3_DIAG + 1 + SCREEN_WIDTH_B,X
                        STA.W WRAM_ADDR_BG3_DIAG + 3,X
                        STA.W WRAM_ADDR_BG3_DIAG + 3 + SCREEN_WIDTH_B,X
                        RTS


                        ; Offsets into screen buffer for each of the cursor
                        ; positions.
data_adpcm_cursor_offsets:
                        .DW $0230       ; File
                        .DW $02B0       ; Channel
                        .DW $03A4       ; Access time MM
                        .DW $03AA       ; Access time SS
                        .DW $03B0       ; Access time FF


  adpcm_process_joypad: JSR.W adpcm_update_ci_text
                        JSR.W adpcm_update_mmssff
                        LDX.B adpcm_joypad_l_raw & $FF
                        CPX.B adpcm_joypad_l & $FF
                        BNE   @no_change

                        LDA.B adpcm_joypad_delay_h & $FF
                        CMP.B #$20
                        BMI   adpcm_process_joypad
                        AND.B #$03
                        BNE   adpcm_process_joypad
                        INC.B adpcm_joypad_delay_h & $FF
                        RTS
            @no_change: STZ.B adpcm_joypad_delay_h & $FF
                        STX.B adpcm_joypad_l & $FF
                        RTS


adpcm_init_cxd_and_mechacon:
                        PHP
                        REP   #$30
                        PHA
                        PHX
                        PHY
                        PHB
                        PHK
                        PLB
                        SEP   #$20
                        SEI

                        STZ.B $08
                        STZ.B $09
                        STZ.B $0A

                        LDA.B #$80

                        STZ.B $1B
                        STZ.B adpcm_mechacon_command_timer & $FF
                        STZ.B adpcm_mechacon_command_number & $FF
                        STZ.B adpcm_mechacon_command_status & $FF

                        LDX.W #$0000
                        STX.B adpcm_mechacon_command_position & $FF

                        ; Reset
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_CHPRST
                        STA.W SCD_CXD_DATA

                        ; "Normal" mode
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #$00
                        STA.W SCD_CXD_DATA

                        ; Set up drive interface.
                        LDA.B #CXD_WR_DRVIF
                        STA.W SCD_CXD_INDEX
                        LDA.B #(CXD_DRVIF_XSLOW | CXD_DRVIF_LCHLOW | CXD_DRVIF_BCKMD1 | CXD_DRVIF_LSB1ST | CXD_DRVIF_CLKLOW)
                        STA.W SCD_CXD_DATA

                        ; Set up decoder.
                        LDA.B #CXD_WR_DECCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #(CXD_DECCTL_AUTOCI | CXD_DECCTL_MODESEL | CXD_DECCTL_FORMSEL | CXD_DECCTL_AUTODIST | CXD_DECCTL_DECMD_RTC)
                        STA.W SCD_CXD_DATA      ; CXD_WR_DECCTL

                        ; Enable DecInt
                        LDA.B #CXD_INTMSK_DECINT
                        STA.W SCD_CXD_DATA      ; CXD_WR_INTMSK

                        ; Flush any old Mechacon data.
                        LDX.W #256
        @read_mechacon: DEX
                        BEQ   @exit_error
                        LDA.W SCD_MECHACON
                        BMI   @read_mechacon

                        LDA.B #$0C
                        STA.W SCD_MYSTERY

                        REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        CLC
                        CLI
                        RTS

           @exit_error: REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        PLP
                        SEC
                        RTS


     adpcm_irq_hanlder: REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20
                        PHB
                        PHK
                        PLB
                        LDA.B #CXD_RD_INTSTS
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA
                        AND.B #CXD_INTSTS_DECINT
                        BEQ   @check_mechacon
                        JSR.W adpcm_service_decint

       @check_mechacon: LDA.W SCD_MECHACON
                        BPL   @exit
                        JSR.W adpcm_service_mechacon

                 @exit: REP   #$30
                        PLB
                        PLY
                        PLX
                        PLA
                        RTI


  adpcm_service_decint: LDA.B #CXD_WR_INTCLR
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_INTCLR_DECINT
                        STA.W SCD_CXD_DATA

                        LDX.W $0012

                        LDA.B #CXD_RD_STS
                        STA.W SCD_CXD_INDEX
                        LDA.W SCD_CXD_DATA              ; CXD_RD_STS
                        STA.B adpcm_sts & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDRFLG
                        STA.B adpcm_hdrflg & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_MIN
                        STA.B adpcm_hdr_min & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_SEC
                        STA.B adpcm_hdr_sec & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_BLOCK
                        STA.B adpcm_hdr_block & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_HDR_MODE
                        STA.B adpcm_hdr_mode & $FF

                        CMP.B #$02
                        BEQ   @mode2

                        STZ.B adpcm_shdr_file & $FF
                        STZ.B adpcm_shdr_ch & $FF
                        STZ.B adpcm_shdr_s_mode & $FF
                        STZ.B adpcm_shdr_ci & $FF
                        BRA   @no_adpcm

                @mode2: LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_FILE
                        STA.B adpcm_shdr_file & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_CH
                        STA.B adpcm_shdr_ch & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_S_MODE
                        STA.B adpcm_shdr_s_mode & $FF
                        LDA.W SCD_CXD_DATA              ; CXD_RD_SHDR_CI
                        STA.B adpcm_shdr_ci & $FF

                        LDA.B adpcm_shdr_s_mode & $FF
                        AND.B #$7F
                        CMP.B #$64
                        BNE   @no_adpcm
                        LDA.B adpcm_file & $FF
                        BEQ   @enable_adpcm
                        CMP.B adpcm_shdr_file & $FF
                        BNE   @no_adpcm

         @enable_adpcm: LDA.B adpcm_shdr_ch & $FF
                        CMP.B adpcm_channel & $FF
                        BNE   @no_adpcm
                        LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #CXD_CHPCTL_ADPEN
                        STA.W SCD_CXD_DATA
                        LDA.B adpcm_shdr_ci & $FF
                        STA.B adpcm_ci & $FF
                        RTS

             @no_adpcm: LDA.B #CXD_WR_CHPCTL
                        STA.W SCD_CXD_INDEX
                        LDA.B #$00
                        STA.W SCD_CXD_DATA
                        RTS


adpcm_service_mechacon: AND.B #$0F
                        STA.B adpcm_mechacon_last_response_nibble & $FF
                        LDX.B adpcm_mechacon_command_position & $FF
                        INX
                        STX.B adpcm_mechacon_command_position & $FF
                        DEX
                        LDA.L data_adpcm_mechacon_command_table,X
                        BMI   @save_response
                        CMP.B adpcm_mechacon_last_response_nibble & $FF
                        BEQ   @done
                        LDA.B adpcm_mechacon_command_status & $FF
                        ORA.B #SUPERCD_MECHACON_COMMAND_UNEXPECTED
                        STA.B adpcm_mechacon_command_status & $FF
                        BRA   @done

        @save_response: AND.B #$1F
                        SEP   #$10
                        TAX
                        LDA.B adpcm_mechacon_last_response_nibble & $FF
                        STA.W adpcm_mechacon_response_buffer,X
                        REP   #$10

                 @done: LDA.B adpcm_mechacon_command_status & $FF
                        AND.B #$DF
                        STA.B adpcm_mechacon_command_status & $FF
                        JSR.W adpcm_continue_mechacon_command
                        RTS


adpcm_send_mechacon_command:
                        STA.B adpcm_mechacon_command_number & $FF
                        LDA.B #SUPERCD_MECHACON_COMMAND_RETRIES
                        STA.B adpcm_mechacon_command_retry & $FF

                @retry: JSR.W adpcm_start_mechacon_command

                 @loop: LDA.B adpcm_mechacon_command_status & $FF
                        AND.B #SUPERCD_MECHACON_COMMAND_COMPLETE_UNEXPECTED
                        BNE   @mismatch
                        LDA.B adpcm_mechacon_command_timer & $FF
                        CMP.B #SUPERCD_ERROR_MECHACON_TIMEOUT_ADPCM
                        BPL   @timeout
                        LDA.W adpcm_mechacon_command_status
                        BMI   @loop

                        CLC
                        RTS

             @mismatch: DEC.B adpcm_mechacon_command_retry & $FF
                        BEQ   @exit_mismatch
                        JSR.W adpcm_mechacon_flush
                        BCC   @retry
        @exit_mismatch: SEC
                        RTS

              @timeout: DEC.B adpcm_mechacon_command_retry & $FF
                        BEQ   @exit_timeout
                        JSR.W adpcm_mechacon_flush
                        BCC   @retry
         @exit_timeout: SEC
                        RTS


  adpcm_mechacon_flush: LDA.W adpcm_mechacon_command_number
                        PHA
                        LDA.B #DIAG_MECHACON_FLUSH
                        STA.B adpcm_mechacon_command_number & $FF
                        JSR.W adpcm_start_mechacon_command

                 @loop: LDA.B adpcm_mechacon_command_timer & $FF
                        CMP.B #SUPERCD_ERROR_MECHACON_TIMEOUT_ADPCM
                        BPL   @exit_error
                        LDA.B adpcm_mechacon_command_status & $FF
                        BMI   @loop

                        PLA
                        STA.W adpcm_mechacon_command_number
                        CLC
                        RTS

           @exit_error: PLA
                        STA.W adpcm_mechacon_command_number
                        SEC
                        RTS


                        ; Two entry points to this subroutine.
adpcm_start_mechacon_command:
                        LDA.B #SUPERCD_MECHACON_COMMAND_BUSY
                        STA.W adpcm_mechacon_command_status
                        REP   #$20
                        LDA.W adpcm_mechacon_command_number
                        AND.W #$00FF
                        ASL   A
                        TAX
                        LDA.L data_adpcm_mechacon_command_table,X
                        STA.W adpcm_mechacon_command_position
                        SEP   #$20
                        ; Second entry point, above falls through.
adpcm_continue_mechacon_command:
                        LDX.B adpcm_mechacon_command_position & $FF
                        INX
                        STX.B adpcm_mechacon_command_position & $FF
                        DEX
                        LDA.W data_adpcm_mechacon_command_table,X
                        BPL   @send_nibble

                        CMP.B #$FF
                        BEQ   @command_complete
                        SEP   #$10
                        AND.B #$0F
                        TAX
                        LDA.W adpcm_comamnd_paramter_buffer,X
                        REP   #$10

          @send_nibble: STA.W SCD_MECHACON
                        STZ.B adpcm_mechacon_command_timer & $FF
                        LDA.B adpcm_mechacon_command_status & $FF
                        ORA.B #SUPERCD_MECHACON_COMMAND_WAIT_RESPONSE
                        STA.B adpcm_mechacon_command_status & $FF
                        BRA   @return

     @command_complete: LDA.B adpcm_mechacon_command_status & $FF
                        AND.B #SUPERCD_MECHACON_COMMAND_UNEXPECTED
                        BEQ   @exit_complete
                        ORA.B #SUPERCD_MECHACON_COMMAND_COMPLETE_UNEXPECTED
        @exit_complete: AND.B #$77
                        STA.B adpcm_mechacon_command_status & $FF

               @return: RTS


                        ; Mechacon commands, see `data_mechacon_command_table`
                        ; in bank $01 for explanation.
data_adpcm_mechacon_command_table:
                        .DW $0028 ; 00
                        .DW $0035 ; 01
                        .DW $0046 ; 02
                        .DW $004F ; 03
                        .DW $0058 ; 04
                        .DW $0061 ; 05
                        .DW $006A ; 06
                        .DW $0073 ; 07
                        .DW $007C ; 08
                        .DW $0085 ; 09
                        .DW $008E ; 0A
                        .DW $0097 ; 0B
                        .DW $00A0 ; 0C
                        .DW $00A9 ; 0D
                        .DW $00B2 ; 0E
                        .DW $00CD ; 0F
                        .DW $00F8 ; 10
                        .DW $00BB ; 11
                        .DW $00C4 ; 12
                        .DW $010D ; 13

                        ; C...F
                        .DB $0C,$0F,$80,$0F,$81,$0F,$82,$0F,$83,$0F,$0F,$95,$FF

                        ; B...F
                        .DB $0B,$0F,$84,$0F,$85,$0F,$86,$0F,$87,$0F,$88,$0F,$89,$0F,$0F,$95,$FF

                        ; D0xF
                        .DB $0D,$0F,$00,$0F,$01,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$00,$0F,$02,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$00,$0F,$03,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$00,$0F,$04,$0F,$0F,$95,$FF

                        ; D1xF
                        .DB $0D,$0F,$01,$0F,$00,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$01,$0F,$01,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$01,$0F,$02,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$01,$0F,$03,$0F,$0F,$95,$FF

                        ; D4xF
                        .DB $0D,$0F,$04,$0F,$00,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$04,$0F,$01,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$04,$0F,$02,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$04,$0F,$03,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$04,$0F,$04,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$04,$0F,$05,$0F,$0F,$95,$FF
                        .DB $0D,$0F,$04,$0F,$06,$0F,$0F,$95,$FF

                        ; D50F
                        .DB $0D,$0F,$05,$0F,$00,$0F,$0F,$0F
                        .DB $00,$85,$00,$86,$00,$87,$00,$88
                        .DB $00,$89,$00,$8A,$00,$8B,$00,$8C
                        .DB $00,$8D,$00,$8E,$00,$8F,$00,$90
                        .DB $00,$91,$00,$92,$00,$93,$00,$94
                        .DB $0F,$95,$FF

                        ; D51F
                        .DB $0D,$0F,$05,$0F,$01,$0F,$0F,$0F,$00
                        .DB $80,$01,$81,$02,$82,$03,$83,$04,$84,$0F,$95,$FF

                        ; F
                        .DB $0F,$0A,$FF

data_tilemap_adpcm_menu:
                        .DW $0000
                        .DW $2000
                        .DW (32*14)
                        .ASC "                                "
                        .ASC "   ADPCM TEST PROGRAM VER 1.1   "
                        .ASC "                                "
                        .ASC "                                "
                        .ASC "     FILE NUMBER      - 00      "
                        .ASC "     CHANNEL NUMBER   - 00      "
                        .ASC "                                "
                        .ASC "     ACCESS TIME  00M00S00F     "
                        .ASC "                                "
                        .ASC "     CURRENT TIME 00M00S00F     "
                        .ASC "                                "
                        .ASC "                                "
                        .ASC "                                "
                        .ASC "                                "

data_bcd_minutes:       .DB $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
                        .DB $10,$11,$12,$13,$14,$15,$16,$17,$18,$19
                        .DB $20,$21,$22,$23,$24,$25,$26,$27,$28,$29
                        .DB $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
                        .DB $40,$41,$42,$43,$44,$45,$46,$47,$48,$49
                        .DB $50,$51,$52,$53,$54,$55,$56,$57,$58,$59
                        .DB $60,$61,$62,$63,$64,$65,$66,$67,$68,$69
                        .DB $70,$71,$72,$73,$74,$75,$76,$77,$78,$79
                        .DB $80,$81,$82,$83,$84,$85,$86,$87,$88,$89
                        .DB $90,$91,$92,$93,$94,$95,$96,$97,$98,$99

; Tables of text structures, all entries padded out to 16 bytes each for easy indexing.
data_text_table_emp:    .DW  $0B04
                        .DW  $2800
                        .DW  9
                        .ASC "         "
                        .DB  $00

                        .DW  $0B04
                        .DW  $2800
                        .DW  9
                        .ASC "EMP OFF /"
                        .DB  $00

                        .DW  $0B04
                        .DW  $2800
                        .DW  9
                        .ASC "EMP ON  /"
                        .DB  $00

data_text_table_lev:    .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC "        "
                        .DB  $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV B /"
                        .DB $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV C /"
                        .DB $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV - /"
                        .DB $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV - /"
                        .DB $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV A /"
                        .DB $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV - /"
                        .DB $00,$00

                        .DW  $0B0D
                        .DW  $2800
                        .DW  8
                        .ASC " LEV - /"
                        .DB $00,$00

data_text_table_mono_stereo:
                        .DW  $0B15
                        .DW  $2800
                        .DW  7
                        .ASC "       "
                        .DB $00,$00,$00

                        .DW  $0B15
                        .DW  $2800
                        .DW  7
                        .ASC " MONO  "
                        .DB  $00,$00,$00

                        .DW  $0B15
                        .DW  $2800
                        .DW  7
                        .ASC " STEREO"
                        .DB $00,$00,$00

adpcm_init_ppu_and_dma: LDA.B #$8F
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


   adpcm_init_graphics: LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_clear_vram_adpcm
                        JSR.W sub_copy_gfx_data_adpcm

                        LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_greyscale_adpcm
                        JSR.W sub_copy_gfx_data_adpcm

                        STZ.W CGADD
                        LDX.W #data_dma_clear_palette_adpcm
                        JSR.W sub_copy_gfx_data_adpcm

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

                        SEP   #$20
                        STZ.W CGADD
                        LDX.W #data_dma_palette_adpcm
                        JSR.W sub_copy_gfx_data_adpcm

                        REP   #$20
                        LDX.W #$0000
                        LDA.W #$2048
    @loop_clear_screen: STA.W WRAM_ADDR_BG3_DIAG,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @loop_clear_screen

                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_adpcm
                        JSR.W sub_copy_gfx_data_adpcm

                        RTS

; Data for DMA copy routine:
;  byte   0: DMA params
;  byte   1: B-Bus address ($21xx)
;  byte 2-4: source (low, high, bank)
;  byte 5-6: count (low, ligh)
data_dma_clear_vram_adpcm:
                        .DB $09
                        .DB (VMDATAL & $FF)
                        .DW data_dma_zero_byte_adpcm
                        .DB $01
                        .DW $0000

data_dma_gfx_greyscale_adpcm:
                        .DB $01
                        .DB (VMDATAL & $FF)
                        .DW gfx_tiles_greyscale_adpcm
                        .DB $01
                        .DW $0200

data_dma_clear_palette_adpcm:
                        .DB $09
                        .DB (CGDATA & $FF)
                        .DW data_dma_zero_byte_adpcm
                        .DB $01
                        .DW $0100

data_dma_palette_adpcm: .DB $00
                        .DB (CGDATA & $FF)
                        .DW data_palette_adpcm
                        .DB $01
                        .DW 32

data_dma_gfx_wram_to_vram_adpcm:
                        .DB $01
                        .DB (VMDATAL & $FF)
                        .DW WRAM_ADDR_BG3_DIAG
                        .DB $00
                        .DW SCREEN_SIZE_B

data_dma_zero_byte_adpcm:
                        .DB $00


     adpcm_nmi_handler: REP   #$20
                        REP   #$10
                        PHA
                        PHB
                        PHX
                        PHY
                        SEP   #$20

                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_adpcm
                        JSR.W sub_copy_gfx_data_adpcm

                        LDA.W JOY1H
                        STA.W adpcm_joypad_h_raw
                        LDA.W JOY1L
                        STA.W adpcm_joypad_l_raw

                        INC.W adpcm_mechacon_command_timer

                        LDA.W adpcm_joypad_delay_l
                        INC   A
                        BEQ   @rollover
                        STA.W adpcm_joypad_delay_l

             @rollover: LDA.W adpcm_joypad_delay_h
                        INC   A
                        CMP.B #$30
                        BNE   @save_delay_h
                        LDA.B #$20

         @save_delay_h: STA.W adpcm_joypad_delay_h
                        REP   #$20
                        PLY
                        PLX
                        PLB
                        PLA
                        RTI


sub_copy_gfx_data_adpcm:
                        LDA.W 0,X
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


    data_palette_adpcm: .DB $00,$28,$FF,$7F,$00,$00,$00,$00
                        .DB $00,$28,$00,$70,$00,$00,$00,$00
                        .DB $00,$28,$E0,$7F,$00,$00,$00,$00
                        .DB $00,$00,$FF,$03,$00,$00,$00,$00

                        ; unused?
             data_CAE3: .DB $5F,$29,$E0,$7F,$00,$00,$00,$00


                        ; Looks like this routine is never used.
    adpcm_clear_screen: REP   #$20
                        LDX.W #0
                        LDA.W #$2048
    @loop_clear_screen: STA.W WRAM_ADDR_BG3_DIAG,X
                        INX
                        INX
                        CPX.W #$0700
                        BNE   @loop_clear_screen

                        SEP   #$20
                        PHP
                        SEP   #$20
          @wait_vblank: LDA.W RDNMI
                        BPL   @wait_vblank
                        LDA.W RDNMI
                        PLP

                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_adpcm
                        JSR.W sub_copy_gfx_data_adpcm
                        RTS


     adpcm_render_text: PHX
                        LDA.W 1,X
                        XBA
                        LDA.W 0,X
                        ASL   A
                        ASL   A
                        REP   #$20
                        LSR   A
                        CLC
                        ADC.W #WRAM_ADDR_BG3_DIAG
                        TAY
                        LDA.W 2,X
                        STA.W adpcm_tile_attributes

                        LDA.W 4,X
          @render_char: PHA
                        LDA.W 6,X
                        INX
                        AND.W #$00FF
                        ASL   A
                        ORA.W adpcm_tile_attributes
                        STA.W 0,Y
                        INC   A
                        STA.W SCREEN_WIDTH_B,Y
                        INY
                        INY
                        TYA
                        AND.W #SCREEN_WIDTH_B - 1
                        BNE   @next_char
                        ; Next row
                        TYA
                        CLC
                        ADC.W #SCREEN_WIDTH_B
                        TAY
            @next_char: PLA
                        DEC   A
                        BNE   @render_char

                        SEP   #$20
                        PLX
                        RTS


                        .ORGA $CB5C
gfx_tiles_greyscale_adpcm:
                        .INCLUDE "gfx/greyscale.inc"
