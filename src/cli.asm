NEXT_PARAM      = $C231         ; OS calls
READ_PARAM      = $C8BC
PRTDIGIT        = $F80B
COS_POST_TEST   = $FA76

STACKPOINTER    = $4            ; STANDARD ATOM ADDRESS
STACK1          = $16

.macro FNADDR addr
        .byte <($80 + >addr), <addr
.endmacro

;=================================================================
; STAR-COMMAND INTERPRETER
; Kees Van Oss' version of the CLI interpreter
;=================================================================
star_com:
        LDX #$ff                ; set up pointers
        CLD
star_com1:
        LDY #0
        JSR SKIPSPC
        DEY
star_com2:
        INY
        INX

star_com3:
        LDA com_tab,x           ; look up star-command
        BMI star_com5
        CMP $100,y
        BEQ star_com2
        DEX
star_com4:
        INX
        LDA com_tab,x
        BPL star_com4
        INX
        LDA $100,y
        CMP #46                 ; '.'
        BNE star_com1
        INY
        DEX
        BCS star_com3

star_com5:
        STY $9a

        LDY $3                  ; save command pointers
        STY tmp_ptr3
        LDY $5
        STY tmp_ptr5
        LDY $6
        STY tmp_ptr6
        LDY #<$100
        STY $5
        LDY #>$100
        STY $6
        LDY $9a
        STY $3

        AND #$7F                ; the commands are all < $8000
        STA $53                 ; execute star command
        LDA com_tab+1,x
        STA $52
        LDX #0
        JSR comint6

        LDY tmp_ptr5            ; restore command pointers
        STY $5
        LDY tmp_ptr6
        STY $6
        LDY tmp_ptr3
        STY $3

        LDA #$0d
        STA ($5),y

        RTS

comint6:
        JMP ($0052)


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
        JMP OSCLI

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
        JSR get_drive_num       ; read drive num
        PHA                     ; save drive num
        LDY $3
        STY $9a
        JSR read_filename       ; read diskimage name into $140
        STY $3
        JSR chk_end_command
        PLA                     ; restore drive num
        JMP sd_mount

;-----------------------------------------------------------------
; *DOUT <drv>
;
;   Unmount SD-diskimage <filename> from drive <drv>
;
;    <drv> 0-3
;-----------------------------------------------------------------

star_dout:
        JSR get_drive_num       ; read drive num
        PHA                     ; save drive num
        JSR chk_end_command
        PLA                     ; restore drive num
        JMP sd_unmount

;-----------------------------------------------------------------
; *DDISKS
;
;   List drives with installed diskimages
;-----------------------------------------------------------------

star_ddisks:
        JSR chk_end_command

        LDA #CMD_GET_IMG_NAME   ; command read imagenames
        JSR slow_cmd
        JSR prepare_read_data   ; reset pointer
        LDX #0                  ; set drive num

ddisks1:
        TXA                     ; print drive num
        JSR PRTDIGIT
        LDA #':'                ; print ':'
        JSR OSWRCH

ddisks2:
        JSR read_data_reg
        JSR OSWRCH
        BNE ddisks2

        JSR OSCRLF
        INX                     ; next drive
        CPX #4
        BNE ddisks1

return:
        RTS

;-----------------------------------------------------------------
; *ATOM
;
;   - disable via interrupts
;   - release tube client
;   - close open files
;   - disable tube transfers in AtoMMC
;   - restore brkvec
;   - exit tube client, return to Atom basic
;-----------------------------------------------------------------

star_atom:

        LDA #$7F                ; Disable all interrupts
        STA ViaIER
        JSR TubeFree            ; Release the tube
        LDY #0                  ; Close all open files
        JSR OSSHUT
        LDA #0
        STA TubeFlag            ; Disable tube transfers in AtoMMC
        LDA BrkSave+0           ; Restore BRKV
        STA BRKV+0
        LDA BrkSave+1
        STA BRKV+1
        PLA                     ; we were called as subroutine
        PLA                     ; clear stack with 4 pulls
        PLA
        PLA
        RTS                     ; this will end the tube client

;-----------------------------------------------------------------
; *COPRO <n>
;-----------------------------------------------------------------
star_copro:
        JSR read_num            ; read parameter
        STA TubeS4
        RTS

;-----------------------------------------------------------------
; *COSPEED <n>
;-----------------------------------------------------------------
star_cospeed:
        JSR read_num            ; read parameter
        PHA
        LDA #0
        STA TubeS2
        PLA
        STA TubeS3
        RTS

;-----------------------------------------------------------------
; *COMEM <n>
;-----------------------------------------------------------------
star_comem:
        JSR read_num            ; read parameter
        PHA
        LDA #1
        STA TubeS2
        PLA
        STA TubeS3
        RTS

;-----------------------------------------------------------------
; *CORESET
;-----------------------------------------------------------------
star_coreset:
        JMP TubeReset

;-----------------------------------------------------------------
; sd_init
;-----------------------------------------------------------------
sd_init:
        LDA #CMD_VALID_IMG_NAMES ; Command = SDDOS load drive config
        JMP slow_cmd

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
        PHA                     ; Save drive num
        JSR prepare_write_data  ; Reset globalbufferpointer
        PLA                     ; Restore drive num
        STA tmp_drive           ; Save drivenr

        LDX #0                  ; Globalbuffer(0)=drive num
loop_m:
        JSR write_data_reg
        LDA NAME,x
        INX
        CMP #$0d
        BNE loop_m

        LDA #0                  ; Globalbuffer(x)=stringterminator
        JSR write_data_reg

        LDA #CMD_FILE_OPEN_IMG  ; Command = SDDOS mount
        JSR slow_cmd
        CMP #$40
        BCS bad_image

        JSR star_fatinfo        ; Get disksize index
        CPX #$ff
        BEQ unmount

        LDY tmp_drive           ; Put disk params in act_disk table
        LDA disk_sect,x
        STA act_disk_sect,y
        LDA disk_int,x
        STA act_disk_int,y

        RTS

unmount:
        LDA tmp_drive           ; Wrong size, unmount disk
        JSR sd_unmount

bad_image:
        JSR STROUT              ; print error
        .byte "IMAGE?"
        NOP
        BRK

tmp_drive:
       .byte 0

;-----------------------------------------------------------------
; sd_unmount
;
;   Unmount drive cur_drive
;
;   Input : A = drive to unmount
;   Output: -
;-----------------------------------------------------------------

sd_unmount:
        AND #3
        JSR write_latch_reg     ; Send drive num
        LDA #CMD_IMG_UNMOUNT    ; Command = SDDOS unmount
        JMP slow_cmd

;-----------------------------------------------------------------
; GET_DRIVE_NUM
;
;   Input : -
;   Output: drive num 0-3 or DRIVE? error
;-----------------------------------------------------------------

get_drive_num:
        JSR read_num            ; read parameter
        CMP #4
        BCS bad_drive
        RTS

bad_drive:
        JSR STROUT              ; print error
        .byte "DRIVE?"
        NOP
        BRK

;-----------------------------------------------------------------
; Read numeric parameter or calculate expression
;
;   Input : -
;   Output: A=byte0, X=byte1, Y=byte2
;-----------------------------------------------------------------

read_num:
        JSR READ_PARAM          ; read expression on stack
        JSR NEXT_PARAM          ; next param

        LDY #0                  ; reset stackpointer
        STY STACKPOINTER
        LDA STACK1              ; put byte0 in a
        RTS

;-----------------------------------------------------------------
; Check for end of command, #0D
;-----------------------------------------------------------------

chk_end_command:
        LDY $3
        JMP COS_POST_TEST

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
        JSR open_file_read      ; invokes error handler if return code > 64

        JSR set_rwptr_to_name   ; get the FAT file size - text files won't have ATM headers
        LDA #CMD_FILE_GETINFO
        JSR slow_cmd

        LDX #13                 ; Read FAT data
        JSR read_data_buffer

        LDX #$ff                ; Check if disksize in table
print_loop:
        INX
        LDA table_disksize_lb,x
        CMP #$ff
        BEQ not_found_error
        CMP NAME+1
        BNE print_loop

        LDA table_disksize_hb,x ; lb found, check hb
        CMP NAME+2
        BNE print_loop

disk_found:
        RTS                     ; Disksize in table, return index in X

not_found_error:
        LDX #$ff                ; Disksize not found, return $ff in X
        RTS

;----------------------------------------------------
; Disksize table
;
; Atom disksize SD SS 40tr 10sect = 100 KB ($019000)
; BBC disksize  SD DS 80tr 10sect = 400 KB ($064000)
;----------------------------------------------------

table_disksize_hb:
        .byte $01,$06,$ff       ; hb disksize/256

table_disksize_lb:
             .byte $90,$40,$ff  ; lb disksize/256

;---------------------------------------
; Disktype parameters  0,  1
;---------------------------------------

disk_sect:
             .byte 10, 20       ; Number of sectors per (image) track

disk_int:
             .byte  0, 10       ; Number of sectors to skip to read interleaved data

;---------------------------------------
; Drive parameters     0,  1,  2,  3
;---------------------------------------

act_disk_sect:
             .byte  0,  0,  0,  0

act_disk_int:
             .byte  0,  0,  0,  0

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read data to memory from the pic's buffer
;
; data may be from another source other than file, ie getfileinfo
; x = number of bytes to read (0 = 256)
; (RWPTR) points to store
;
read_data_buffer:
        JSR prepare_read_data

        LDY #0

@loop:
        JSR read_data_reg
        STA (RWPTR),y
        INY
        DEX
        BNE @loop

return_ok:
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; set RWPTR to point to NAME  (i.e. $140)
;
; this is called 5 times, so making it a subroutine rather than a macro
; saves 4 * (8 - 3) - 9 = 11 bytes!
set_rwptr_to_name:
        LDA #<NAME
        STA RWPTR
        LDA #>NAME
        STA RWPTR+1
        RTS

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Read filename, then fall through to open_file_read

open_filename_read:
        JSR read_filename       ; copy filename from $100 to $140
        ; fall through to open_file_read

;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; Open file for read or write
;
; $140 = name
; a = read/write $01 = read, $11 = write
;

open_file_read:
        LDA #CMD_FILE_OPEN_READ
        JSR open_file
        JMP expect64orless

open_file_write:
        LDA #CMD_FILE_OPEN_WRITE

; Falls through to
open_file:
        PHA
        JSR send_name
        PLA
        JMP slow_cmd

send_name:
        JSR prepare_write_data

send_additional_name:
        LDX #0
        BEQ @pumpname

@nextchar:
        JSR write_data_reg
        INX

@pumpname:
        LDA NAME,x              ; write filename to filename buffer
        CMP #$0d
        BNE @nextchar

        LDA #0                  ; terminate the string
        JMP write_data_reg

expect64orless:
        CMP #STATUS_COMPLETE+1
        BCC return_ok
        ; fall through to report_disk_failure


;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~;~~
;
; report a file system error
;
report_disk_failure:
        AND #ERROR_MASK
        PHA                     ; save error code
        TAX                     ; error code into x
        LDY #$ff                ; string indexer

@findstring:
        INY                     ; do this here because we need the z flag below
        LDA diskerrortab,y
        BNE @findstring         ; zip along the string till we find a zero

        DEX                     ; when this bottoms we've found our error
        BNE @findstring
        PLA                     ; restore error code
        TAX                     ; error code in X
        LDA TUBE_FLAG
        CMP #TUBE_ENABLED
        BEQ @tubeError

@printstring:
        INY
        LDA diskerrortab,y
        JSR OSWRCH
        BNE @printstring
        BRK

@tubeError:
        INY                     ; store index for basic BRK-alike hander
        TYA
        CLC
        ADC #<diskerrortab
        STA $d5
        LDA #>diskerrortab
        ADC #0
        STA $d6
        JMP L0409               ; error code in X (must be non zero)

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
        .byte $00               ; mkfs error
        .byte "TIMEOUT",$00
        .byte "EEPROM ERROR",$00
        .byte "FAILED",$00
        .byte "TOO MANY",$00
        .byte "SILLY",$0d
