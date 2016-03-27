;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; "Init" commands that are used in several places
; TODO: Check these really don't need any delay / handshaking....

prepare_read_data:
   lda   #CMD_INIT_READ
   bne   write_cmd_reg

prepare_write_data:
   lda   #CMD_INIT_WRITE
   ; fall through to write_cmd_reg

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write command + wait

write_cmd_reg:
   sta   ACMD_REG
.ifdef AVR
   jmp   WaitUntilRead
.else
   jmp   inter_write_delay
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write latch + wait

write_latch_reg:
   sta   ALATCH_REG
.ifdef AVR
   jmp   WaitUntilRead
.else
   jmp   inter_write_delay
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Write data + wait

write_data_reg:
   sta   AWRITE_DATA_REG
.ifdef AVR
   jmp   WaitUntilRead
.else
   jmp   data_write_delay
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
; Wait + Read data

read_data_reg:
.ifdef AVR
   jsr   WaitUntilWritten
.else
   jsr   data_read_delay
.endif
   lda   AREAD_DATA_REG
   rts

.ifndef AVR
;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Short delay
;
; Enough to intersperse 2 writes to the FATPIC.
;
inter_write_delay:
   pha
   lda   #16
   bne   write_delay
data_write_delay:
   pha
   lda   #4
write_delay:
   sec
@loop:
   sbc   #1
   bne   @loop
   pla
data_read_delay:
   rts
.endif

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Fast command
; - On the PIC, command port write followed by interwrite delay
; - On the AVR, this is the same as slow_cmd

fast_cmd:
.ifndef AVR
   jsr   write_cmd_reg
   lda   ACMD_REG
   rts
.else
   ; fall through to slow_cmd
.endif


;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Fast command, command port write followed by interwrite delay on PIC,
; Simply an alias for "jsr slow_cmd" on AVR.

slow_cmd:
   jsr   write_cmd_reg

.ifndef AVR
slow_cmd_loop:
   lda   #0
   sec
slow_cmd_delay_loop:
   sbc   #1
   bne   slow_cmd_delay_loop

   lda   ACMD_REG
   bmi   slow_cmd_loop       ; loop until command done bit (bit 7) is cleared
.else
   jsr   WaitWhileBusy       ; Keep waiting until not busy
   lda   ACMD_REG            ; get status for client
.endif
   rts

.ifdef AVR
WaitUntilRead:
   lda   ASTATUS_REG         ; Read status reg
   and   #MMC_MCU_READ       ; Been read yet ?
   bne   WaitUntilRead       ; nope keep waiting
   rts

WaitUntilWritten:
   lda   ASTATUS_REG         ; Read status reg
   and   #MMC_MCU_WROTE      ; Been written yet ?
   beq   WaitUntilWritten    ; nope keep waiting
   rts

WaitWhileBusy:
   lda   ASTATUS_REG         ; Read status reg
   and   #MMC_MCU_BUSY       ; MCU still busy ?
   bne   WaitWhileBusy       ; yes keep waiting
   rts
.endif
