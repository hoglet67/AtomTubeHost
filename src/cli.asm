NEXT_PARAM      = $C231              ; OS calls
READ_PARAM      = $C8BC
PRTDIGIT        = $F80B
COS_POST_TEST   = $FA76

STACKPOINTER    = $4                 ; STANDARD ATOM ADDRESS
STACK1          = $16

.macro FNADDR addr
   .byte <($80 + >addr), <addr
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

   .byte "COPRO"
   FNADDR star_copro

   .byte "COSPEED"
   FNADDR star_cospeed

   .byte "COMEM"
   FNADDR star_comem

   .byte "CORESET"
   FNADDR star_coreset

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
; *COPRO <n>
;-----------------------------------------------------------------
star_copro:
   jsr   read_num               ; read parameter
   sta   TubeS4
   rts

;-----------------------------------------------------------------
; *COSPEED <n>
;-----------------------------------------------------------------
star_cospeed:
   jsr   read_num               ; read parameter
   pha
   lda   #0
   sta   TubeS2
   pla
   sta   TubeS3
   rts

;-----------------------------------------------------------------
; *COMEM <n>
;-----------------------------------------------------------------
star_comem:
   jsr   read_num               ; read parameter
   pha
   lda   #1
   sta   TubeS2
   pla
   sta   TubeS3
   rts

;-----------------------------------------------------------------
; *CORESET
;-----------------------------------------------------------------
star_coreset:
   jmp   TubeReset

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
   sta tmp_drive		; Save drivenr

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

   jsr star_fatinfo		; Get disksize index
   cpx #$ff
   beq unmount

   ldy tmp_drive		; Put disk params in act_disk table
   lda disk_sect,x
   sta act_disk_sect,y
   lda disk_int,x
   sta act_disk_int,y

   rts

unmount:
   lda tmp_drive		; Wrong size, unmount disk
   jsr sd_unmount

bad_image:
   jsr   STROUT                 ; print error
   .byte "IMAGE?"
   nop
   brk

tmp_drive:	.byte 0
        
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

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; *FATINFO [filename]
;
; Shows fat filesystem file info - size on disk, sector, fptr and attrib.
;
star_fatinfo:


;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; opens a file for reading, then gets the file info
;
; this is used by fatinfo, exec, and rload

open_filename_getinfo:
   jsr   open_file_read		; invokes error handler if return code > 64

   jsr   set_rwptr_to_name	; get the FAT file size - text files won't have ATM headers
   lda   #CMD_FILE_GETINFO
   jsr   slow_cmd

   ldx   #13			; Read FAT data
   jsr read_data_buffer

   ldx  #$ff			; Check if disksize in table
print_loop:
   inx
   lda table_disksize_lb,x
   cmp #$ff
   beq not_found_error
   cmp NAME+1
   bne print_loop

   lda table_disksize_hb,x	; lb found, check hb
   cmp NAME+2
   bne print_loop

disk_found:
   rts				; Disksize in table, return index in X

not_found_error:
   ldx #$ff			; Disksize not found, return $ff in X
   rts
   
;----------------------------------------------------
; Disksize table
;
; Atom disksize SD SS 40tr 10sect = 100 KB ($019000)
; BBC disksize  SD DS 80tr 10sect = 400 KB ($064000)
;----------------------------------------------------

table_disksize_hb:	.byte $01,$06,$ff	; hb disksize/256
table_disksize_lb:	.byte $90,$40,$ff	; lb disksize/256

;---------------------------------------
; Disktype parameters  0,  1
;---------------------------------------

disk_sect:	.byte 10, 20	; Number of sectors per (image) track
disk_int:	.byte  0, 10	; Number of sectors to skip to read interleaved data

;---------------------------------------
; Drive parameters     0,  1,  2,  3
;---------------------------------------

act_disk_sect:	.byte  0,  0,  0,  0
act_disk_int:	.byte  0,  0,  0,  0

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read data to memory from the pic's buffer
;
; data may be from another source other than file, ie getfileinfo
; x = number of bytes to read (0 = 256)
; (RWPTR) points to store
;
read_data_buffer:
   jsr   prepare_read_data

   ldy   #0

@loop:
   jsr   read_data_reg
   sta   (RWPTR),y
   iny
   dex
   bne   @loop

return_ok:
   rts

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; set RWPTR to point to NAME  (i.e. $140)
;
; this is called 5 times, so making it a subroutine rather than a macro
; saves 4 * (8 - 3) - 9 = 11 bytes!
set_rwptr_to_name:
   lda   #<NAME
   sta   RWPTR
   lda   #>NAME
   sta   RWPTR+1
   rts

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read filename, then fall through to open_file_read

open_filename_read:
   jsr   read_filename          ; copy filename from $100 to $140
   ; fall through to open_file_read 

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Open file for read or write
;
; $140 = name
; a = read/write $01 = read, $11 = write
;

open_file_read:
   lda   #CMD_FILE_OPEN_READ
   jsr   open_file
   jmp   expect64orless

open_file_write:
   lda   #CMD_FILE_OPEN_WRITE

; Falls through to
open_file:
   pha
   jsr   send_name
   pla
   jmp   slow_cmd

send_name:
   jsr   prepare_write_data

send_additional_name:
   ldx   #0
   beq   @pumpname

@nextchar:
   jsr   write_data_reg
   inx

@pumpname:
   lda   NAME,x			; write filename to filename buffer
   cmp   #$0d
   bne   @nextchar

   lda   #0			; terminate the string
   jmp   write_data_reg

expect64orless:
   cmp   #STATUS_COMPLETE+1
   bcc   return_ok
   ; fall through to report_disk_failure


;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; report a file system error
;
report_disk_failure:
   and   #ERROR_MASK
   pha                          ; save error code
   tax                          ; error code into x
   ldy   #$ff                   ; string indexer

@findstring:
   iny                          ; do this here because we need the z flag below
   lda   diskerrortab,y
   bne   @findstring            ; zip along the string till we find a zero

   dex                          ; when this bottoms we've found our error
   bne   @findstring
   pla                          ; restore error code
   tax                          ; error code in X
   lda   TUBE_FLAG
   cmp   #TUBE_ENABLED
   beq   @tubeError

@printstring:
   iny
   lda   diskerrortab,y
   jsr   OSWRCH
   bne   @printstring
   brk

@tubeError:
   iny                          ; store index for basic BRK-alike hander
   tya
   clc
   adc   #<diskerrortab
   sta   $d5
   lda   #>diskerrortab
   adc   #0
   sta   $d6
   jmp   L0409                  ; error code in X (must be non zero)

diskerrortab:
   .byte $00
   .byte "DISK FAULT",$00
   .byte "INTERNAL ERROR",$00
   .byte "NOT READY",$00
   .byte "NOT FOUND",$00
   .byte "NO PATH",$00
   .byte "INVALID NAME",$00
   .byte "ACCESS DENIED",$00
   .byte "EXISTS",$00
   .byte "INVALID OBJECT",$00
   .byte "WRITE PROTECTED",$00
   .byte "INVALID DRIVE",$00
   .byte "NOT ENABLED",$00
   .byte "NO FILESYSTEM",$00
   .byte $00                    ; mkfs error
   .byte "TIMEOUT",$00
   .byte "EEPROM ERROR",$00
   .byte "FAILED",$00
   .byte "TOO MANY",$00
   .byte "SILLY",$0d
