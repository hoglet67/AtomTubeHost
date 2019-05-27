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
.ifdef AVR
        JMP WaitUntilRead
.else
        JMP inter_write_delay
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write latch + wait

write_latch_reg:
        STA ALATCH_REG
.ifdef AVR
        JMP WaitUntilRead
.else
        JMP inter_write_delay
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write data + wait

write_data_reg:
        STA AWRITE_DATA_REG
.ifdef AVR
        JMP WaitUntilRead
.else
        JMP data_write_delay
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Wait + Read data

read_data_reg:
.ifdef AVR
        JSR WaitUntilWritten
.else
        JSR data_read_delay
.endif
        LDA AREAD_DATA_REG
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Short delay
;
; Enough to intersperse 2 writes to the FATPIC.
;
inter_write_delay:
.ifndef AVR
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
.endif
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Fast command
; - On the PIC, command port write followed by interwrite delay
; - On the AVR, this is the same as slow_cmd

fast_cmd:
.ifndef AVR
        JSR write_cmd_reg
        LDA ACMD_REG
        RTS
.else
   ; fall through to slow_cmd
.endif


;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Fast command, command port write followed by interwrite delay on PIC,
; Simply an alias for "jsr slow_cmd" on AVR.

slow_cmd:
        JSR write_cmd_reg

.ifndef AVR
slow_cmd_loop:
        LDA #0
        SEC
slow_cmd_delay_loop:
        SBC #1
        BNE slow_cmd_delay_loop

        LDA ACMD_REG
        BMI slow_cmd_loop       ; loop until command done bit (bit 7) is cleared
.else
        JSR WaitWhileBusy       ; Keep waiting until not busy
        LDA ACMD_REG            ; get status for client
.endif
        RTS

.ifdef AVR
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
.endif

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
