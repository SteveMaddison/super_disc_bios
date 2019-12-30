;
; Mechacon test program
;
     comms_test_screen: REP   #$10
                        SEP   #$20
                        PHK
                        PLB
                        SEP   #$20

                        LDA.B #NMITIMEN_JOYPAD
                        STA.W NMITIMEN

                        LDA.B #INIDISP_BLANK
                        STA.W INIDISP

                        SEI
                        JSR.W comms_init_graphics
                        JSR.W comms_init_vectors_and_commands

                        LDA.B #$08
                        STA.W SCD_MYSTERY

                        STZ.W comms_command_pending_flag
                        STZ.W comms_joypad_delay_h

                        LDA.B #$01
                        STZ.W comms_param_buffer_ttii
                        STA.W comms_param_buffer_ttii + 1
                        STZ.W comms_param_buffer_ttii + 2
                        STA.W comms_param_buffer_ttii + 3

                        STZ.W comms_param_buffer_mmssff
                        STZ.W comms_param_buffer_mmssff + 1
                        STZ.W comms_param_buffer_mmssff + 2
                        STZ.W comms_param_buffer_mmssff + 3
                        STZ.W comms_param_buffer_mmssff + 4
                        STZ.W comms_param_buffer_mmssff + 5

                        STZ.W comms_param_data_pointer
                        STZ.W comms_param_cursor
                        STZ.W comms_param_dirty_flag
                        STZ.W comms_command_cursor

                        LDX.W #comm_test_data_e981
                        JSR.W comms_render_text
                        LDX.W #comm_test_data_e9a5
                        JSR.W comms_render_text
                        LDX.W #comm_test_data_E967
                        JSR.W comms_render_text
                        LDX.W #comm_test_data_E974
                        JSR.W comms_render_text

                        JSR.W comms_reset_params
                        STZ.W comms_joypad_l
                        STZ.W comms_joypad_h
                        STZ.W comms_joypad_l_raw
                        STZ.W comms_joypad_h_raw

                        LDA.B #(NMITIMEN_VBLANK | NMITIMEN_JOYPAD)
                        STA.W NMITIMEN

                        LDA.B #INIDISP_NO_BLANK
                        STA.W INIDISP
                        CLI
                        PHY
                        PHX

                        LDY.W #$0080
          @delay_outer: LDX.W #$00FF
          @delay_inner: DEX
                        BNE   @delay_inner
                        DEY
                        BNE   @delay_outer

                        PLX
                        PLY

     comms_joypad_loop: JSR.W comms_process_joypad
                        LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_SELECT
                        BEQ   @check_b
                        JML.L diagnostic_menu

              @check_b: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_B
                        BEQ   @check_x
                        LDA.W comms_command_pending_flag
                        BNE   @check_x
                        JSR.W comms_update_params
                        LDA.W comms_command_cursor
                        JSR.W comms_start_command
                        BRA   comms_joypad_loop

              @check_x: LDA.W comms_joypad_l
                        AND.B #JOYPAD_L_X
                        BEQ   @check_r
                        LDA.W comms_command_pending_flag
                        BEQ   @check_r
                        LDA.W comms_joypad_delay_l
                        CMP.B #$F0
                        BMI   @check_r
                        LDA.B #$13
                        STA.W comms_command_cursor
                        JSR.W comms_reset_params
                        LDA.W comms_command_cursor
                        JSR.W comms_start_command
                        BRA   comms_joypad_loop

              @check_r: LDA.W comms_joypad_l
                        AND.B #JOYPAD_L_R
                        BEQ   @check_l
                        LDA.W comms_command_cursor
                        INC   A
                        CMP.B #$13
                        BMI   @inc_ok
                        LDA.B #$00
               @inc_ok: STA.W comms_command_cursor
                        JSR.W comms_reset_params
                        BRA   comms_joypad_loop

              @check_l: LDA.W comms_joypad_l
                        AND.B #JOYPAD_L_L
                        BEQ   @check_ttii
                        LDA.W comms_command_cursor
                        DEC   A
                        CMP.B #$FF
                        BNE   @dec_ok
                        LDA.B #$12
               @dec_ok: STA.W comms_command_cursor
                        JSR.W comms_reset_params
                        BRA   comms_joypad_loop

           @check_ttii: LDA.W comms_command_cursor
                        BEQ   @check_dpad_ttii
                        JMP.W @check_mmssff

      @check_dpad_ttii: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_RIGHT
                        BEQ   @check_left_ttii
                        LDA.W comms_param_cursor
                        CMP.B #$02
                        BNE   @check_cursor_max_ttii
                        ; Skip the separator between TT and II.
                        INC   A
                        INC   A
@check_cursor_max_ttii: CMP.B #$06
                        BEQ   @cursor_inc_ok_ttii
                        INC   A
   @cursor_inc_ok_ttii: STA.W comms_param_cursor
                        LDA.W comms_param_data_pointer
                        INC   A
                        CMP.B #$04
                        BEQ   @check_down_ttii
                        STA.W comms_param_data_pointer
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag
                        BRA   @check_down_ttii

      @check_left_ttii: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_LEFT
                        BEQ   @check_down_ttii
                        LDA.W comms_param_cursor
                        CMP.B #$05
                        BNE   @check_cursor_min_ttii
                        ; Skip the separator between TT and II.
                        DEC   A
                        DEC   A
@check_cursor_min_ttii: CMP.B #$01
                        BEQ   @cursor_dec_ok_ttii
                        DEC   A
   @cursor_dec_ok_ttii: STA.W comms_param_cursor
                        LDA.W comms_param_data_pointer
                        BEQ   @check_down_ttii
                        DEC   A
                        STA.W comms_param_data_pointer
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag

      @check_down_ttii: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_DOWN
                        BEQ   @check_up_ttii
                        SEP   #$10
                        LDX.W comms_param_data_pointer
                        LDA.W comms_param_buffer_ttii,X
                        DEC   A
                        BPL @param_dec_ok_ttii
                        LDA.B #$09
    @param_dec_ok_ttii: STA.W comms_param_buffer_ttii,X
                        LDX.W comms_param_cursor
                        STA.W comms_command_params_ttii,X
                        REP   #$10
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag
                        BRA   @update_param_ttii

        @check_up_ttii: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_UP
                        BEQ   @update_param_ttii
                        SEP   #$10
                        LDX.W comms_param_data_pointer
                        LDA.W comms_param_buffer_ttii,X
                        INC   A
                        CMP.B #$0A
                        BNE   @param_inc_ok_ttii
                        LDA.B #$00
    @param_inc_ok_ttii: STA.W comms_param_buffer_ttii,X
                        LDX.W comms_param_cursor
                        STA.W comms_command_params_ttii,X
                        REP   #$10
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag

    @update_param_ttii: LDA.W comms_param_dirty_flag
                        BEQ   @param_clean_ttii
                        STZ.W comms_param_dirty_flag
                        JSR.W comms_update_params
     @param_clean_ttii: JMP.W comms_joypad_loop

         @check_mmssff: LDA.W comms_command_cursor
                        CMP.B #$01
                        BEQ   @check_dpad_mmssff
                        JMP.W @done

    @check_dpad_mmssff: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_RIGHT
                        BEQ   @check_left_mmssff
                        LDA.W comms_param_cursor
                        CMP.B #$01
                        BNE   @check_inc_separator
                        INC   A
  @check_inc_separator: CMP.B #$04
                        BNE   @check_cursor_max_mmssff
                        INC   A
@check_cursor_max_mmssff:
                        CMP.B #$07
                        BEQ   @cursor_inc_ok_mmssff
                        INC   A
 @cursor_inc_ok_mmssff: STA.W comms_param_cursor
                        LDA.W comms_param_data_pointer
                        INC   A
                        CMP.B #$06
                        BEQ   @check_down_mmssff
                        STA.W comms_param_data_pointer
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag
                        BRA   @check_down_mmssff

    @check_left_mmssff: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_LEFT
                        BEQ   @check_down_mmssff
                        LDA.W comms_param_cursor
                        CMP.B #$06
                        BNE   @check_dec_separator
                        DEC   A
  @check_dec_separator: CMP.B #$03
                        BNE   @check_cursor_min_mmssff
                        DEC   A
@check_cursor_min_mmssff:
                        CMP.B #$00
                        BEQ   @cursor_dec_ok_mmssff
                        DEC   A
 @cursor_dec_ok_mmssff: STA.W comms_param_cursor
                        LDA.W comms_param_data_pointer
                        BEQ   @check_down_mmssff
                        DEC   A
                        STA.W comms_param_data_pointer
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag

    @check_down_mmssff: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_DOWN
                        BEQ   @check_up_mmssff
                        SEP   #$10
                        LDX.W comms_param_data_pointer
                        LDA.W comms_param_buffer_mmssff,X
                        DEC   A
                        BPL @param_dec_ok_mmssff
                        LDA.B #$09
  @param_dec_ok_mmssff: STA.W comms_param_buffer_mmssff,X
                        LDX.W comms_param_cursor
                        STA.W comms_command_params_mmssff,X
                        REP   #$10
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag
                        JSR.W comms_update_params
                        BRA   @update_param_mmssff

      @check_up_mmssff: LDA.W comms_joypad_h
                        AND.B #JOYPAD_H_UP
                        BEQ   @update_param_mmssff
                        SEP   #$10
                        LDX.W comms_param_data_pointer
                        LDA.W comms_param_buffer_mmssff,X
                        INC   A
                        CMP.B #$0A
                        BNE   @param_inc_ok_mmssff
                        LDA.B #$00
  @param_inc_ok_mmssff: STA.W comms_param_buffer_mmssff,X
                        LDX.W comms_param_cursor
                        STA.W comms_command_params_mmssff,X
                        REP   #$10
                        LDA.B #$FF
                        STA.W comms_param_dirty_flag
  @update_param_mmssff: LDA.W comms_param_dirty_flag
                        BEQ   @done
                        STZ.W comms_param_dirty_flag
                        JSR.W comms_update_params
                 @done: JMP.W comms_joypad_loop


  comms_process_joypad: LDX.W comms_joypad_l_raw
                        CPX.W comms_joypad_l
                        BNE   @no_change
                        LDA.W comms_joypad_delay_h
                        CMP.B #$20
                        BMI   comms_process_joypad
                        AND.B #$07
                        BNE   comms_process_joypad
                        INC.W comms_joypad_delay_h
                        RTS

            @no_change: STZ.W comms_joypad_delay_h
                        STX.W comms_joypad_l
                        RTS


                        ; Two entry points to this subroutine.
    comms_reset_params: LDA.W comms_command_cursor
                        BNE   @non_zero
                        LDA.B #$01
                        STA.W comms_param_cursor
                        STZ.W comms_param_data_pointer
                        BRA   comms_update_params

             @non_zero: CMP.B #$01
                        BNE   comms_update_params
                        STZ.W comms_param_cursor
                        STZ.W comms_param_data_pointer
                        ; Second entry point (above falls through).
   comms_update_params: PHP
                        REP   #$20
                        LDA.W comms_command_cursor
                        AND.W #$00FF
                        ASL A
                        PHA
                        ASL A
                        ASL A
                        ASL A
                        ASL A
                        CLC
                        ADC.W #comms_command_buffer
                        TAX
                        JSR.W comms_render_text

                        ; Update the command string.
                        PLX
                        PHX
                        LDA.W data_mechacon_command_string_table,X
                        TAX
                        SEP   #$20
                        LDY.W #0
         @loop_command: LDA.W 0,X
                        BPL   @write_command
                        ; Substitute parameter: remove $80 and use as offset.
                        AND.B #$7F
                        PHX
                        PHY
                        SEP   #$10
                        TAX
                        LDA.W comms_param_buffer,X
                        REP   #$10
                        PLY
                        PLX
        @write_command: STA.W comms_serve_buffer_struct + 5,Y
                        INX
                        INY
                        CPY.W #21
                        BNE   @loop_command

                        ; Update SERVE struct.
                        LDX.W #$2400    ; Attributes/colour
                        STX.W comms_serve_buffer_struct + 2
                        LDA.B #21
                        STA.W comms_serve_buffer_struct + 4
                        LDX.W #comms_serve_buffer_struct
                        JSR.W comms_render_text

                        ; Reset attributes after rendering.
                        LDX.W #$2000
                        STX.W comms_serve_buffer_struct + 2
                        STZ.W comms_serve_buffer_struct + 4

                        ; Update the response string.
                        PLX
                        REP   #$20
                        LDA.W data_mechacon_command_response_table,X
                        TAX
                        SEP   #$20
                        LDY.W #0
        @loop_response: LDA.W 0,X
                        BPL @write_response
                        ; Bogus response, use a "." character.
                        LDA.B #$27
       @write_response: STA.W comms_receive_buffer_struct + 5,Y
                        INX
                        INY
                        CPY.W #21
                        BNE   @loop_response

                        ; Update RECEIVE struct.
                        LDX.W #$2400    ; Attributes/colour
                        STX.W comms_receive_buffer_struct + 2
                        LDA.B #21       ; Length
                        STA.W comms_receive_buffer_struct + 4
                        LDX.W #comms_receive_buffer_struct
                        JSR.W comms_render_text

                        ; Reset attributes after rendering.
                        LDX.W #$2000
                        STX.W comms_receive_buffer_struct + 2
                        STZ.W comms_receive_buffer_struct + 4

                        LDA.W comms_command_cursor
                        CMP.B #$02
                        BPL   @exit

                        ; Colour parameters for commands < 2
                        SEP   #$10
                        LDA.W comms_param_cursor
                        ASL A
                        TAX
                        LDA.B #$2C
                        STA.W VRAM_ADDR_BG3_DIAG_COMMS + $223,X
                        STA.W VRAM_ADDR_BG3_DIAG_COMMS + $263,X
                        REP   #$10
                 @exit: PLP
                        RTS


     comms_irq_handler: REP   #$30
                        PHA
                        PHX
                        PHY
                        SEP   #$20
                        LDA.W SCD_MECHACON
                        BPL   @exit


                        AND.B #$0F
                        LDY.W comms_last_mechacon_nibble
                        INY
                        STY.W comms_last_mechacon_nibble
                        STA.W comms_receive_buffer_struct + 4,Y
                        INC.W comms_receive_buffer_struct + 4
                        REP   #$10
                        LDX.W #comms_receive_buffer_struct
                        JSR.W comms_render_text
                        JSR.W comms_continue_command
                 @exit: REP   #$30
                        PLY
                        PLX
                        PLA
                        RTI


                        ; Two entry points for this subroutine.
   comms_start_command: SEP   #$10
                        ASL   A
                        TAX
                        REP   #$20
                        LDA.W #0
                        STA.W comms_last_mechacon_nibble
                        LDA.W data_mechacon_command_string_table,X
                        DEC   A
                        STA.W comms_command_position
                        LDA.W data_mechacon_command_response_table,X
                        DEC   A
                        STA.W comms_response_position
                        SEP   #$20
                        REP   #$10
                        LDA.B #$FF
                        STA.W comms_command_pending_flag
                        ; Second entry point (above falls through).
comms_continue_command: LDY.W comms_command_position
                        INY
                        STY.W comms_command_position
                        LDA.W 0,Y
                        BPL   @send_nibble
                        ; Substitute parameter: remove $80 and use as offset.
                        AND.B #$0F
                        TAX
                        LDA.W comms_param_buffer,X
          @send_nibble: CMP.B #$24              ; Space signals end
                        BEQ   @exit
                        STA.W SCD_MECHACON
                        INC.W comms_serve_buffer_struct + 4
                        REP   #$10
                        LDX.W #comms_serve_buffer_struct
                        JSR.W comms_render_text
                        RTS

                 @exit: STZ.W comms_command_pending_flag
                        RTS


comms_init_vectors_and_commands:
                        LDA.B #VECTOR_TABLE_OPCODE
                        STA.W supercd_bios_nmi_vector_opcode
                        STA.W supercd_bios_irq_vector_opcode
                        LDX.W #comms_nmi_handler
                        STX.W supercd_bios_nmi_vector_address
                        LDX.W #comms_irq_handler
                        STX.W supercd_bios_irq_vector_address
                        PHK
                        PLA
                        STA.W supercd_bios_nmi_vector_bank
                        STA.W supercd_bios_irq_vector_bank

                        ; Initialise string data structures in WRAM for all
                        ; the commands.
                        LDA.B #$14
                        LDX.W #0
                        LDY.W #0
         @copy_command: PHA
                        REP   #$20
                        LDA.W #$010A    ; Screen position/offset
                        STA.W comms_command_buffer,Y
                        INY
                        INY
                        LDA.W #$2800    ; Attributes/colour
                        STA.W comms_command_buffer,Y
                        INY
                        INY
                        SEP   #$20
                        LDA.B #16       ; Length
                        STA.W comms_command_buffer,Y
                        INY

                        LDA.B #16
          @copy_string: PHA
                        LDA.W data_mechacon_command_name_table,X
                        STA.W comms_command_buffer,Y
                        INX
                        INY
                        PLA
                        DEC   A
                        BNE   @copy_string

                        INY
                        INY
                        INY
                        INY
                        INY
                        INY
                        INY
                        INY
                        INY
                        INY
                        INY
                        PLA
                        DEC   A
                        BNE   @copy_command

                        ; Make two more structures for the SERVE and RECEIVE
                        ; strings. Starts with screen offset/position.
                        LDX.W #$018A
                        STX.W comms_serve_buffer_struct
                        LDX.W #$020A
                        STX.W comms_receive_buffer_struct

                        ; Attributes/colour
                        LDX.W #$2400
                        STX.W comms_serve_buffer_struct + 2
                        STX.W comms_receive_buffer_struct + 2

                        ; String Length
                        LDA.B #21
                        STA.W comms_serve_buffer_struct + 4
                        STA.W comms_receive_buffer_struct + 4

                        ; Fill with spaces for now.
                        LDA.B #$24      ; Space tile
                        LDX.W #0
                 @loop: STA.W comms_serve_buffer_struct + 5,X
                        STA.W comms_receive_buffer_struct + 5,X
                        INX
                        CPX.W #26
                        BMI   @loop

                 @wait: LDA.W SCD_MECHACON
                        BMI   @wait
                        RTS

                        ; Mechacon command tables
                        ; Note: these are NOT indexed by the command number used by the BIOS function
                        ; but based on the order of the command strings themselves.
data_mechacon_command_string_table:
                        .DW data_mechacon_command_string_seek_tr_indx   ; 00
                        .DW data_mechacon_command_string_seek_mmssff    ; 01
                        .DW data_mechacon_command_string_stop           ; 02
                        .DW data_mechacon_command_string_play           ; 03
                        .DW data_mechacon_command_string_pause          ; 04
                        .DW data_mechacon_command_string_open_close     ; 05
                        .DW data_mechacon_command_string_fast_forward   ; 06
                        .DW data_mechacon_command_string_fast_reverse   ; 07
                        .DW data_mechacon_command_string_forward        ; 08
                        .DW data_mechacon_command_string_reverse        ; 09
                        .DW data_mechacon_command_string_key_direct     ; 0A
                        .DW data_mechacon_command_string_key_ignore     ; 0B
                        .DW data_mechacon_command_string_continous      ; 0C
                        .DW data_mechacon_command_string_track_pause    ; 0D
                        .DW data_mechacon_command_string_index_pause    ; 0E
                        .DW data_mechacon_command_string_normal_speed   ; 0F
                        .DW data_mechacon_command_string_double_speed   ; 10
                        .DW data_mechacon_command_string_req_sub_q      ; 11
                        .DW data_mechacon_command_string_req_status     ; 12
                        .DW data_mechacon_command_string_flush          ; 13

data_mechacon_command_response_table:
                        .DW data_mechacon_command_response_seek_tr_indx ; 00
                        .DW data_mechacon_command_response_seek_mmssff  ; 01
                        .DW data_mechacon_command_response_stop         ; 02
                        .DW data_mechacon_command_response_play         ; 03
                        .DW data_mechacon_command_response_pause        ; 04
                        .DW data_mechacon_command_response_open_close   ; 05
                        .DW data_mechacon_command_response_fast_forward ; 06
                        .DW data_mechacon_command_response_fast_reverse ; 07
                        .DW data_mechacon_command_response_forward      ; 08
                        .DW data_mechacon_command_response_reverse      ; 09
                        .DW data_mechacon_command_response_key_direct   ; 0A
                        .DW data_mechacon_command_response_key_ignore   ; 0B
                        .DW data_mechacon_command_response_continous    ; 0C
                        .DW data_mechacon_command_response_track_pause  ; 0D
                        .DW data_mechacon_command_response_index_pause  ; 0E
                        .DW data_mechacon_command_response_normal_speed ; 0F
                        .DW data_mechacon_command_response_double_speed ; 10
                        .DW data_mechacon_command_response_req_sub_q    ; 11
                        .DW data_mechacon_command_response_req_status   ; 12
                        .DW data_mechacon_command_response_flush        ; 13

data_mechacon_command_string_seek_tr_indx:
                        .ASC "C"
                        .DB  $80,$81,$82,$83
                        .ASC "F                "
data_mechacon_command_response_seek_tr_indx:
                        .ASC "FFFFFF                "

data_mechacon_command_string_seek_mmssff:
                        .ASC "B"
                        .DB  $84,$85,$86,$87,$88,$89
                        .ASC "F              "
data_mechacon_command_response_seek_mmssff:
                        .ASC "FFFFFFFF              "

data_mechacon_command_string_stop:
                        .ASC "D01F                  "
data_mechacon_command_response_stop:
                        .ASC "FFFF                  "

data_mechacon_command_string_play:
                        .ASC "D02F                  "
data_mechacon_command_response_play:
                        .ASC "FFFF                  "

data_mechacon_command_string_pause:
                        .ASC "D03F                  "
data_mechacon_command_response_pause:
                        .ASC "FFFF                  "

data_mechacon_command_string_open_close:
                        .ASC "D04F                  "
data_mechacon_command_response_open_close:
                        .ASC "FFFF                  "

data_mechacon_command_string_fast_forward:
                        .ASC "D10F                  "
data_mechacon_command_response_fast_forward:
                        .ASC "FFFF                  "

data_mechacon_command_string_fast_reverse:
                        .ASC "D11F                  "
data_mechacon_command_response_fast_reverse:
                        .ASC "FFFF                  "

data_mechacon_command_string_forward:
                        .ASC "D12F                  "
data_mechacon_command_response_forward:
                        .ASC "FFFF                  "

data_mechacon_command_string_reverse:
                        .ASC "D13F                  "
data_mechacon_command_response_reverse:
                        .ASC "FFFF                  "

data_mechacon_command_string_key_direct:
                        .ASC "D40F                  "
data_mechacon_command_response_key_direct:
                        .ASC "FFFF                  "

data_mechacon_command_string_key_ignore:
                        .ASC "D41F                  "
data_mechacon_command_response_key_ignore:
                        .ASC "FFFF                  "

data_mechacon_command_string_continous:
                        .ASC "D42F                  "
data_mechacon_command_response_continous:
                        .ASC "FFFF                  "

data_mechacon_command_string_track_pause:
                        .ASC "D43F                  "
data_mechacon_command_response_track_pause:
                        .ASC "FFFF                  "

data_mechacon_command_string_index_pause:
                        .ASC "D44F                  "
data_mechacon_command_response_index_pause:
                        .ASC "FFFF                  "

data_mechacon_command_string_normal_speed:
                        .ASC "D45F                  "
data_mechacon_command_response_normal_speed:
                        .ASC "FFFF                  "

data_mechacon_command_string_double_speed:
                        .ASC "D46F                  "
data_mechacon_command_response_double_speed:
                        .ASC "FFFF                  "

data_mechacon_command_string_req_sub_q:
                        .ASC "D50F0000000000000000F "
data_mechacon_command_response_req_sub_q:
                        .ASC "FFFF"
                        .DB  $85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F,$90,$91,$92,$93,$94
                        .ASC "F "

data_mechacon_command_string_req_status:
                        .ASC "D51F01234F            "
data_mechacon_command_response_req_status:
                        .ASC "FFFF"
                        .DB $80,$81,$82,$83,$84
                        .ASC "F            "

data_mechacon_command_string_flush:
                        .ASC "F                     "
data_mechacon_command_response_flush:
                        .ASC "F                     "

; Textual names of commands
data_mechacon_command_name_table:
                        .ASC "ACCESS T01/I01  " ; 00
                        .ASC "ACCESS 00/00/00 " ; 01
                        .ASC "STOP            " ; 02
                        .ASC "PLAY            " ; 03
                        .ASC "PAUSE           " ; 04
                        .ASC "OPEN / CLOSE    " ; 05
                        .ASC "FAST FORWARD    " ; 06
                        .ASC "FAST REVERSE    " ; 07
                        .ASC "FORWARD         " ; 08
                        .ASC "REVERSE         " ; 09
                        .ASC "KEY DIRECT      " ; 0A
                        .ASC "KEY IGNORE      " ; 0B
                        .ASC "CONTINUOUS PLAY " ; 0C
                        .ASC "AUTO TRACK PAUSE" ; 0D
                        .ASC "AUTO INDEX PAUSE" ; 0E
                        .ASC "NORMAL SPEED    " ; 0F
                        .ASC "DOUBLE SPEED    " ; 10
                        .ASC "Q-DATA REQUEST  " ; 11
                        .ASC "STATUS REQUEST  " ; 12
                        .ASC "RESTORE         " ; 13

   comm_test_data_E967: .DW  $0182
                        .DW  $2000
                        .DB  8
                        .ASC "SERVE  -"

   comm_test_data_E974: .DW  $0202
                        .DW  $2000
                        .DB  8
                        .ASC "RECEIVE-"

   comm_test_data_e981: .DW  $0041
                        .DW  $2000
                        .DB  31
                        .ASC "CD-PLAYER TEST PRGRAM VER 1.00 "

   comm_test_data_e9a5: .DW  $0102
                        .DW  $2000
                        .DB  8
                        .ASC "SELECT -"

                        ; ASCII strings for Mechacon status.
data_comms_mechacon_status:
                        .DB "NO DISC "
                        .DB "STOP    "
                        .DB "PLAY    "
                        .DB "PAUSE   "
                        .DB "Fast Rev"
                        .DB "Fast Fwd"
                        .DB "Slow Rev"
                        .DB "Slow Fwd"
                        .DB "(undef) "
                        .DB "(undef) "
                        .DB "ACCESS  "
                        .DB "TOC READ"
                        .DB "(undef) "
                        .DB "(undef) "
                        .DB "(undef) "
                        .DB "(undef) "


comms_init_ppu_and_dma: LDA.B #$8F
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


   comms_init_graphics: LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_clear_vram_comms
                        JSR.W comms_dma_copy

                        LDX.W #$0000
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_greyscale_comms
                        JSR.W comms_dma_copy

                        STZ.W CGADD
                        LDX.W #data_dma_clear_palette_comms
                        JSR.W comms_dma_copy

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
                        LDX.W #data_dma_palette_comms
                        JSR.W comms_dma_copy

                        REP   #$20
                        LDX.W #0
                        LDA.W #$2048
    @loop_clear_screen: STA.W VRAM_ADDR_BG3_DIAG_COMMS,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
                        BNE   @loop_clear_screen

                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_comms
                        JSR.W comms_dma_copy

                        RTS

; Data for DMA copy routine:
;  byte   0: DMA params
;  byte   1: B-Bus address ($21xx)
;  byte 2-4: source (low, high, bank)
;  byte 5-6: count (low, ligh)
data_dma_clear_vram_comms:
                        .DB $09         ; no autoinc source
                        .DB (VMDATAL & $FF)
                        .DW data_dma_zero_byte_comms
                        .DB $01         ; bank 1
                        .DW $0000       ; 0000 = roll over, so $10000 = 64K

data_dma_gfx_greyscale_comms:
                       .DB $01
                       .DB (VMDATAL & $FF)
                       .DW gfx_tiles_greyscale_comms
                       .DB $01
                       .DW $0200

data_dma_clear_palette_comms:
                       .DB $09
                       .DB (CGDATA & $FF)
                       .DW data_dma_zero_byte_comms
                       .DB $01
                       .DW $0100

data_dma_palette_comms:
                       .DB $00
                       .DB (CGDATA & $FF)
                       .DW data_palette_comms
                       .DB $01
                       .DW 32

data_dma_gfx_wram_to_vram_comms:
                       .DB $01
                       .DB (VMDATAL & $FF)
                       .DW VRAM_ADDR_BG3_DIAG_COMMS
                       .DB $00
                       .DW SCREEN_SIZE_B

data_dma_zero_byte_comms:
                        .DB $00

   comms_nmi_handler:   REP   #$20
                        REP   #$10
                        PHA
                        PHB
                        PHX
                        PHY
                        SEP   #$20
                        LDX.W #VRAM_ADDR_BG3_DIAG
                        STX.W VMADDL
                        LDX.W #data_dma_gfx_wram_to_vram_comms
                        JSR.W comms_dma_copy

                 @wait: LDA.W HVBJOY
                        AND.B #$01
                        BNE   @wait

                        LDA.W JOY1H
                        STA.W comms_joypad_h_raw
                        LDA.W JOY1L
                        STA.W comms_joypad_l_raw
                        LDA.W comms_joypad_delay_l
                        INC   A
                        BEQ   @rollover
                        STA.W comms_joypad_delay_l
             @rollover: LDA.W comms_joypad_delay_h
                        INC   A
                        CMP.B #$30
                        BNE   @save_delay_h
                        LDA.B #$20
         @save_delay_h: STA.W comms_joypad_delay_h
                        REP   #$20
                        PLY
                        PLX
                        PLB
                        PLA
                        RTI


        comms_dma_copy: LDA.W 0,X
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


    data_palette_comms: .DB $00,$28,$FF,$7F,$00,$00,$00,$00
                        .DB $00,$28,$00,$70,$00,$00,$00,$00
                        .DB $00,$28,$E0,$7F,$00,$00,$00,$00
                        .DB $00,$00,$FF,$03,$00,$00,$00,$00

                        ; unused??
             data_EC84: .DB $5F,$29,$E0,$7F,$00,$00,$00,$00


                        ; Looks like this routine is never used.
    comms_clear_screen: REP   #$20
                        LDX.W #0
                        LDA.W #$2048
    @loop_clear_screen: STA.W VRAM_ADDR_BG3_DIAG_COMMS,X
                        INX
                        INX
                        CPX.W #SCREEN_SIZE_B
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
                        LDX.W #data_dma_gfx_wram_to_vram_comms
                        JSR.W comms_dma_copy

                        RTS


     comms_render_text: PHP
                        PHD
                        REP   #$20
                        LDA.W #COMMS_DIRECT
                        TCD
                        LDA.W 0,X
                        ASL   A
                        CLC
                        ADC.W #VRAM_ADDR_BG3_DIAG_COMMS
                        STA.W comms_top_tile_address
                        CLC
                        ADC.W #SCREEN_WIDTH_B
                        STA.W comms_bottom_tile_address
                        LDA.W 2,X
                        STA.W comms_tile_attributes
                        LDA.W 4,X
                        AND.W #$00FF
                        LDY.W #0
          @render_char: PHA
                        LDA.W 5,X
                        INX
                        AND.W #$00FF
                        ASL A
                        ORA.W comms_tile_attributes
                        STA.B (comms_top_tile_address & $FF),Y
                        INC   A
                        STA.B (comms_bottom_tile_address & $FF),Y
                        INY
                        INY
                        PLA
                        DEC   A
                        BNE   @render_char
                        PLD
                        PLP
                        RTS

.IFDEF ORIGINAL
                        ; Unused copy of above.
comms_render_text_copy: PHP
                        PHD
                        REP   #$20
                        LDA.W #COMMS_DIRECT
                        TCD
                        LDA.W 0,X
                        ASL   A
                        CLC
                        ADC.W #VRAM_ADDR_BG3_DIAG_COMMS
                        STA.W comms_top_tile_address
                        CLC
                        ADC.W #SCREEN_WIDTH_B
                        STA.W comms_bottom_tile_address
                        LDA.W 2,X
                        STA.W comms_tile_attributes
                        LDA.W 4,X
                        AND.W #$00FF
                        LDY.W #0
          @render_char: PHA
                        LDA.W 5,X
                        INX
                        AND.W #$00FF
                        ASL   A
                        ORA.W comms_tile_attributes
                        STA.B (comms_top_tile_address & $FF),Y
                        INC   A
                        STA.B (comms_bottom_tile_address & $FF),Y
                        INY
                        INY
                        PLA
                        DEC   A
                        BNE   @render_char
                        PLD
                        PLP
                        RTS
.ENDIF

.IFDEF ORIGINAL
                        .ORGA $ED3F
.ENDIF
gfx_tiles_greyscale_comms:
                        .INCLUDE "gfx/greyscale.inc"
