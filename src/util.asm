atommc3_version_expected:
        .byte "ATOMMC2 V3", 0

atommc3_version_actual = $EFD0

atommc3_type:
        .byte 0

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; atommc3_init - determine type of atommc and check ROM version

atommc3_detect:
        LDX #$FF

atommc3_check_loop:
        INX
        LDA atommc3_version_expected, X
        BEQ atommc3_check_done
        CMP atommc3_version_actual, X
        BEQ atommc3_check_loop

        JSR STROUT
        .byte "WARNING: ATOMMC3 ROM NOT PRESENT", 10, 13
        NOP
        JSR OSRDCH

atommc3_check_done:
        LDX #0
        LDA $B404               ; 0 = AVR, not 0 = PIC
        BEQ atommc3_avr
        DEX
atommc3_avr:
        STX atommc3_type        ; AVR=$0000, = PIC=$FF
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; "Init" commands that are used in several places
; TODO: Check these really don't need any delay / handshaking....

prepare_read_data:
        LDA #CMD_INIT_READ
        BNE write_cmd_reg

prepare_write_data:
        LDA #CMD_INIT_WRITE
        ; fall through to write_cmd_reg

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write command + wait

write_cmd_reg:
        STA ACMD_REG
        BIT atommc3_type
        BPL WaitUntilRead       ; AVR
        BMI inter_write_delay   ; PIC

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write latch + wait

write_latch_reg:
        STA ALATCH_REG
        BIT atommc3_type
        BPL WaitUntilRead       ; AVR
        BMI inter_write_delay   ; PIC

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write data + wait

write_data_reg:
        STA AWRITE_DATA_REG
        BIT atommc3_type
        BPL WaitUntilRead       ; AVR
        BMI inter_write_delay   ; PIC

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Wait + Read data

read_data_reg:
        BIT atommc3_type
        BMI read_data_reg_pic
read_data_reg_avr:
        JSR WaitUntilWritten
        LDA AREAD_DATA_REG
        RTS
read_data_reg_pic:
        JSR data_read_delay
        LDA AREAD_DATA_REG
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Short delay
;
; Enough to intersperse 2 writes to the FATPIC.
;
inter_write_delay:
        BIT atommc3_type
        BPL data_read_delay
        PHA
        LDA #16
        BNE write_delay

data_write_delay:
        PHA
        LDA #4
write_delay:
        SEC
@loop:
        SBC #1
        BNE @loop
        PLA
data_read_delay:
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Fast command
; - On the PIC, command port write followed by interwrite delay
; - On the AVR, this is the same as slow_cmd

fast_cmd:
        BIT atommc3_type
        BPL slow_cmd
        JSR write_cmd_reg
        LDA ACMD_REG
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Fast command, command port write followed by interwrite delay on PIC,
; Simply an alias for "jsr slow_cmd" on AVR.

slow_cmd:
        JSR write_cmd_reg
        BIT atommc3_type
        BPL slow_cmd_avr
slow_cmd_pic:
        LDA #0
        SEC
slow_cmd_loop:
        SBC #1
        BNE slow_cmd_loop
        LDA ACMD_REG
        BMI slow_cmd_pic       ; loop until command done bit (bit 7) is cleared
        RTS
slow_cmd_avr:
        JSR WaitWhileBusy       ; Keep waiting until not busy
        LDA ACMD_REG            ; get status for client
        RTS

WaitUntilRead:
        LDA ASTATUS_REG         ; Read status reg
        AND #MMC_MCU_READ       ; Been read yet ?
        BNE WaitUntilRead       ; nope keep waiting
        RTS

WaitUntilWritten:
        LDA ASTATUS_REG         ; Read status reg
        AND #MMC_MCU_WROTE      ; Been written yet ?
        BEQ WaitUntilWritten    ; nope keep waiting
        RTS

WaitWhileBusy:
        LDA ASTATUS_REG         ; Read status reg
        AND #MMC_MCU_BUSY       ; MCU still busy ?
        BNE WaitWhileBusy       ; yes keep waiting
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read filename from $100 to $140
;
; Input  $9A = pointer just after command
;
; Output $140 contains filename, terminated by $0D
;
read_filename:
        JSR read_optional_filename

        CPX #0                  ; chec the filename length > 0
        BNE filename_ok

syn_error:
        JMP COSSYN              ; generate a SYN? ERROR 135


;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read Optional filename from $100 to $140
;
; Input  $9A = pointer just after command
;
; Output $140 contains filename, terminated by $0D
;
read_optional_filename:
        LDX #0
        LDY $9a

@filename1:
        JSR SKIPSPC
        CMP #$22
        BEQ @filename5

@filename2:
        CMP #$0d
        BEQ @filename3

        STA NAME,x
        INX
        INY
        LDA $100,y
        CMP #$20
        BNE @filename2

@filename3:
        LDA #$0d
        STA NAME,x
        STY $9a
        RTS

@filename5:
        INY
        LDA $100,y
        CMP #$0d
        BEQ syn_error

        STA NAME,x
        INX
        CMP #$22
        BNE @filename5

        DEX
        INY
        LDA $100,Y
        CMP #$22
        BNE @filename3

        INX
        BCS @filename5

filename_ok:
        RTS
