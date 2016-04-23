NEXT_PARAM      = $C231              ; OS calls
READ_PARAM      = $C8BC
PRTDIGIT        = $F80B
COS_POST_TEST   = $FA76

STACKPOINTER    = $4                 ; STANDARD ATOM ADDRESS
STACK1          = $16

.macro FNADDR addr
   .byte $80 + >addr, <addr
.endmacro

;=================================================================
; STAR-COMMAND INTERPRETER
; Kees Van Oss' version of the CLI interpreter
;=================================================================
star_com:
   ldx   #$ff                   ; set up pointers
   cld
star_com1:
   ldy   #0
   jsr   SKIPSPC
   dey
star_com2:
   iny
   inx

star_com3:
   lda   com_tab,x              ; look up star-command
   bmi   star_com5
   cmp   $100,y
   beq   star_com2
   dex
star_com4:
   inx
   lda   com_tab,x
   bpl   star_com4
   inx
   lda   $100,y
   cmp   #46                    ; '.'
   bne   star_com1
   iny
   dex
   bcs   star_com3

star_com5:
   sty   $9a

   ldy   $3                     ; save command pointers
   sty   tmp_ptr3
   ldy   $5
   sty   tmp_ptr5
   ldy   $6
   sty   tmp_ptr6
   ldy   #<$100
   sty   $5
   ldy   #>$100
   sty   $6
   ldy   $9a
   sty   $3

   and   #$7F                   ; the commands are all < $8000
   sta   $53                    ; execute star command
   lda   com_tab+1,x
   sta   $52
   ldx   #0
   jsr   comint6

   ldy   tmp_ptr5               ; restore command pointers
   sty   $5
   ldy   tmp_ptr6
   sty   $6
   ldy   tmp_ptr3
   sty   $3

   lda   #$0d
   sta   ($5),y

   rts

comint6:
   jmp   ($0052)


com_tab:
   .byte "DIN"
   FNADDR star_din

   .byte "DOUT"
   FNADDR star_dout

   .byte "DDISKS"
   FNADDR star_ddisks

   .byte "ATOM"
   FNADDR star_atom

   FNADDR star_arbitrary

star_arbitrary:
   jmp   OSCLI

;-----------------------------------------------------------------
; *DIN <drv>,<filename>
;
;   Mount SD-diskimage <filename> to drive <drv>
;   When the diskimage is already linked to another drive, an error
;   is generated and the drive is cleared
;
;    <drv> 0-3
;    <filename> max 13 chars
;-----------------------------------------------------------------

star_din:
   jsr   get_drive_num          ; read drive num
   pha                          ; save drive num
   ldy   $3
   sty   $9a
   jsr   read_filename          ; read diskimage name into $140
   sty   $3
   jsr   chk_end_command
   pla                          ; restore drive num
   jmp   sd_mount

;-----------------------------------------------------------------
; *DOUT <drv>
;
;   Unmount SD-diskimage <filename> from drive <drv>
;
;    <drv> 0-3
;-----------------------------------------------------------------

star_dout:
   jsr   get_drive_num          ; read drive num
   pha                          ; save drive num
   jsr   chk_end_command
   pla                          ; restore drive num
   jmp   sd_unmount

;-----------------------------------------------------------------
; *DDISKS
;
;   List drives with installed diskimages
;-----------------------------------------------------------------

star_ddisks:
   jsr   chk_end_command

   lda   #CMD_GET_IMG_NAME      ; command read imagenames
   jsr   slow_cmd
   jsr   prepare_read_data      ; reset pointer
   ldx   #0                     ; set drive num

ddisks1:
   txa                          ; print drive num
   jsr   PRTDIGIT
   lda   #':'                   ; print ':'
   jsr   OSWRCH

ddisks2:
   jsr   read_data_reg
   jsr   OSWRCH
   bne   ddisks2

   jsr   OSCRLF
   inx                          ; next drive
   cpx   #4
   bne   ddisks1

return: 
   rts
        
;-----------------------------------------------------------------
; *ATOM
;
;   - disable via interrupts
;   - release tube client
;   - close open files
;   - restore brkvec
;   - exit tube client, return to Atom basic
;-----------------------------------------------------------------

star_atom:

   lda   #$7F                     ; Disable all interrupts
   sta   ViaIER
   jsr   TubeFree                 ; Release the tube
   ldy   #0                       ; Close all open files
   jsr   OSSHUT
   lda   BrkSave+0                ; Restore BRKV
   sta   BRKV+0
   lda   BrkSave+1
   sta   BRKV+1
   pla                            ; we were called as subroutine
   pla                            ; clear stack with 4 pulls
   pla                              
   pla                              
   rts                            ; this will end the tube client

;-----------------------------------------------------------------
; sd_init
;-----------------------------------------------------------------
sd_init:
   lda   #CMD_VALID_IMG_NAMES     ; Command = SDDOS load drive config
   jmp   slow_cmd
        
;-----------------------------------------------------------------
; sd_mount
;
;   Mount filename at $140 to drive cur_drive
;
;   Input : A         = drive to mount
;           $140      = filename
;   Output: filestatus
;-----------------------------------------------------------------

sd_mount:
   pha                          ; Save drive num
   jsr   prepare_write_data     ; Reset globalbufferpointer
   pla                          ; Restore drive num

   ldx   #0                     ; Globalbuffer(0)=drive num
loop_m:
   jsr   write_data_reg
   lda   NAME,x
   inx
   cmp   #$0d
   bne   loop_m

   lda   #0                     ; Globalbuffer(x)=stringterminator
   jsr   write_data_reg

   lda   #CMD_FILE_OPEN_IMG     ; Command = SDDOS mount
   jsr   slow_cmd
   cmp   #$40
   bcs   bad_image
   rts

bad_image:
   jsr   STROUT                 ; print error
   .byte "IMAGE?"
   nop
   brk
        
;-----------------------------------------------------------------
; sd_unmount
;
;   Unmount drive cur_drive
;
;   Input : A = drive to unmount
;   Output: -
;-----------------------------------------------------------------

sd_unmount:
   and   #3
   jsr   write_latch_reg        ; Send drive num
   lda   #CMD_IMG_UNMOUNT       ; Command = SDDOS unmount
   jmp   slow_cmd

;-----------------------------------------------------------------
; GET_DRIVE_NUM
;
;   Input : -
;   Output: drive num 0-3 or DRIVE? error
;-----------------------------------------------------------------

get_drive_num:
   jsr   read_num               ; read parameter
   cmp   #4
   bcs   bad_drive
   rts

bad_drive:
   jsr   STROUT                 ; print error
   .byte "DRIVE?"
   nop
   brk

;-----------------------------------------------------------------
; Read numeric parameter or calculate expression
;
;   Input : -
;   Output: A=byte0, X=byte1, Y=byte2
;-----------------------------------------------------------------

read_num:
   jsr   READ_PARAM             ; read expression on stack
   jsr   NEXT_PARAM             ; next param

   ldy   #0                     ; reset stackpointer
   sty   STACKPOINTER
   lda   STACK1                 ; put byte0 in a
   rts

;-----------------------------------------------------------------
; Check for end of command, #0D
;-----------------------------------------------------------------

chk_end_command:
   ldy   $3
   jmp   COS_POST_TEST
