wake_time_on        =   4     
ring_wake_on        =   7

IntFlags            =   $1
ADBInt              =   0

LastADBCMD          =   $5

ADBStatus           =   $6
adb_newcmd          =   1
adb_autopoll        =   2
adb_srq             =   3
adb_noreply         =   4
adb_buscontention   =   5
adb_error           =   7

ADBCMDLength        =   $7

CommandByte         =   $11
ByteCount           =   $12

WriteLocation       =   $1B

PowerFlags          =   $1D
charger_connected   =   0
hichg_on            =   1
hichg_overflow      =   2
battery_dead        =   3
battery_low         =   4
charger_changed     =   5

ref5V_Level         =   $1F
lowBatteryLevel     =   $71
deadBatteryLevel    =   $72
HICHGLevel          =   $73

cmd_readADB         =   $28
cmd_readINT         =   $78

readBit             =   3           ; Commands $x8-$xF are read commands

;A/D and Input register
AD_Reg              =   $EF 
AD_Ref              =   0           ; +5V via 100k/100k voltage divider
AD_Batt             =   1           ; Battery voltage from A/D line
In_Reg              =   $EE
SOUND_LATCH         =   2
OFF_HOOK            =   3

; A/D control register
AD_Ctrl_Reg         =   $F3
AD_Conv             =   3

Int_Ctrl_Reg        =   $FE
Int2_Enb            =   0
Int2_Req            =   1
AD_Int_Req          =   5           ; AD interrupt request bit
                                    ; Shared with Timer 1 interrupt bit
                                    ; When bit 3 of A/D control register = 1,
                                    ; the interrupt is from A/D
Int1_Req            =   7           ; Interrupt 1 request bit

PWM_Ctrl_Reg        =   $F5

IWM_CNTRL           =   0
SCC_CNTRL           =   1
HD_PWR              =   2
MODEM_PWR           =   3
SERIAL_PWR          =   4
SOUND_PWR           =   5
MINUS_5V            =   6
SYS_PWR             =   7

Port_P1             =   $E2
Port_P1_DIR         =   $E3
NC                  =   0           ; Not connected, probably for testing
AKD                 =   1           ; AKD
STOP_CLK            =   2           ; Stop clocks to save power
CHRG_ON             =   3           ; CHRG_ON* - Charger connected (active low)
KBD_RST             =   4           ; KBD_RST* - Keyboard controller reset (active low)
HICHG               =   5           ; HICHG
RING_DETECT         =   6           ; RING_DETECT
MODEM_AB            =   7           ; MODEM_A/B

; Port P2
VIA_Com             =   $E4         ; 8-bit communication bus with VIA
VIA_Com_DIR         =   $E5
Dir_Read            =   %00000000
Dir_Write           =   %11111111

Port_P3             =   $E8
Port_P3_DIR         =   $E9
RESET               =   0
SYS_RST             =   1
VIA_TEST            =   2
SOUND_OFF           =   3
OneSec              =   4           ; 1SEC* - One second interrupt (active low)
PMINT               =   5           ; PMINT*
PMACK               =   6           ; PMACK*
PMREQ               =   7           ; PMREQ*

Port_P4             =   $E9
Port_P4_DIR         =   $EB
ADB_Out             =   0
ADB_In              =   1
DISP_BLANK          =   2           ; DISP_BLANK* - Turns the display off (active low)
MODEM_INS           =   3           ; MODEM_INS* - When a modem is installed (active low)


.org    $E800
PMGRResetEntry
    bra ColdEntry
PMVers1
.byte   $2
PMVers2
.byte   $B5
SelfTestVal1
.byte   $76
.byte   $44
string_Matt
.byte   "MATT"
string_Bugs
.byte   "Bugs"
.byte   $A8
.byte   "Copyright Apple Computer Inc  1987, 1988, 1989 Written by Michael Hanlon "
ColdEntry
    ldy #8
    bra Entry2
ResetEntry
    ldy #0
Entry2
    ldx #$BF
    txs
    clt
    sei
    cld
    jsr FUN_EF84
    bcc LAB_E884
    ldx #0
    lda #0
InitRAM
    sta $2F,X
    inx
    bpl InitRAM
    sta PowerFlags
    sta $21
    ldm #590-512,lowBatteryLevel
    ldm #574-512,deadBatteryLevel
    ldm #712-512,HICHGLevel
    jsr FUN_EF84
    sta $AF
LAB_E884
    lda #0
    sta $0
    sta ADBStatus
    jsr CombineTime
    cmp Time
    beq LAB_E89A
    ldx #9
    lda #0
LAB_E895
    sta $24,X
    dex
    bpl LAB_E895
LAB_E89A
    lda Port_P0
    and #4
    ora #$40
    sty $1
    tst $1
    beq InitPorts
    lda #%10011111
InitPorts
    sta Port_P0
    ldm #%11111111,Port_P0_DIR
    ldm #%11000001,Port_P1
    ldm #%10110101,Port_P1_DIR
    ldm #%11111111,VIA_Com
    ldm #Dir_Read,VIA_Com_DIR
    ldm #%11110101,Port_P3
    ldm #%01111010,Port_P3_DIR
    ldm #%1110,Port_P4
    ldm #%0101,Port_P4_DIR
    ldm #0,Timer_Ctrl_Reg
    ldm #0,Int_Ctrl_Reg
    ldm #4,Timer_1_2_Prescaler
    ldm #1,Timer_X_Prescaler
    ldm #1,Timer_X_Prescaler
    lda $2E
    bmi LAB_E8DE
    beq LAB_E8DE
    cmp #$20
    bcc LAB_E8E1
LAB_E8DE
    ldm #4,$2E
LAB_E8E1:
    lda $2E
    sta $13
    jsr SetContrast
    seb 0,PWM_Ctrl_Reg              ; Turn PWM on
    jsr batteryNow
    seb KBD_RST,Port_P1
    cpy #0
    beq LAB_E8F9
    bbc VIA_TEST,Port_P3,LAB_E8F9
    jmp Sleep
LAB_E8F9
    ldx #2
LAB_E8FB
    bit Int_Ctrl_Reg
    bpl LAB_E8FB
    jsr FUN_EB97
    dex
    bne LAB_E8FB
    seb SYS_RST,Port_P3
    seb SOUND_PWR,Port_P0
    clb SOUND_PWR,Port_P0
    clb Int2_Req,Int_Ctrl_Reg
    seb Int2_Enb,Int_Ctrl_Reg
    cli
CommandReceive
    ldm #Dir_Read,VIA_Com_DIR
.LAB_E913
    bit Int_Ctrl_Reg
    bmi .LAB_E94A
    bbs adb_newcmd,ADBStatus,.ADBAction
    bbs PMREQ,Port_P3,.LAB_E913
    jsr ReceiveCommand
    bcs CommandReceive
    lda CommandByte
    cmp cmd_readINT
    bne .readADBCheck
    jsr readINT
    bra CommandReceive
.readADBCheck:
    cmp cmd_readADB
    bne .OtherCommands
    jsr readADB
    bra CommandReceive
.OtherCommands
    and #$F0                        ; Discard the subcommand number
    lsr A
    lsr A
    lsr A
    tax
    lda CMDTable,X
    sta $1B
    lda CMDTable+1,X
    sta $1C
    jsr ($1B)                       ; Jump to the command handler
    bra CommandReceive
.LAB_E94A
    ldm #7,VIA_Com
    ldm #Dir_Write,VIA_Com_DIR
    jsr FUN_EB97
    bbs ADBInt,IntFlags,.noADBAction
    bbs adb_newcmd,ADBStatus,.ADBAction
    bbc adb_autopoll,ADBStatus,.noADBAction
.ADBAction
    ldm #7,VIA_Com
    ldm #Dir_Write,VIA_Com_DIR
    lda ADBStatus
    and #%101
    sta ADBStatus
    lda $3
    sta ADBCMDLength
    lda LastADBCMD
    and #$F                         ; Check for a reset command
    bne .ADBNotReset
    seb ADB_Out,Port_P4
    ldx #0
    jsr DelayLoop
    jsr DelayLoop
    jsr DelayLoop
    clb ADB_Out,Port_P4
    seb adb_noreply,ADBStatus
    bra .ADBFinish
.ADBNotReset
    jsr ADBCMDDo
    bcc .LAB_E98B
    seb adb_noreply,ADBStatus
    bra .ADBFinish
.LAB_E98B
    jsr FUN_ECC3
    bcc .ADBFinish
    bbc adb_autopoll,ADBStatus,.ADBFinish
    bbc adb_srq,ADBStatus,.noADBAction
.ADBFinish
    seb ADBInt,IntFlags
.noADBAction
    lda IntFlags
    and #%111
    beq .noInterrupts
    clb PMINT,Port_P3
    seb 1,$0
    seb PMINT,Port_P3
.noInterrupts
    jmp CommandReceive
;
ReceiveCommand
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta CommandByte
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta ByteCount
    rts
WriteToLocation
    ldm #$13,$1B                    ; Set start address to $13
    ldm #$00,$1C
WriteBytesRAM
    ldy #0                          ; Initialize pointer offset
WriteBytesLoop
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta ($1B),Y
    iny
    dec ByteCount
    bne WriteBytesLoop
    clc
ReceiveCommand_Exit
    rts
PM_ReceiveByte_Wait
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bbc PMREQ,Port_P3,ReceiveByte
    bra ReceiveByte_Fail
ReceiveByte
    lda VIA_Com
    clb PMACK,Port_P3
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
    bbs PMREQ,Port_P3,ReceiveByte_Done
ReceiveByte_Fail
    seb PMACK,Port_P3
    sec
    rts
ReceiveByte_Done
    seb PMACK,Port_P3
    clc
    rts
ReturnDataToHost
    ldm #$13,WriteLocation
    ldm #$00,WriteLocation+1
ReturnDataToHost2
    lda CommandByte
    jsr SendByte
    bcs ReturnDataExit
    lda ByteCount
    jsr SendByte
    bcs ReturnDataExit
    tst $12
    bcs ReturnDataExit
    ldy #0
ReturnDataLoop
    lda ($1B),Y
    jsr SendByte
    bcs ReturnDataExit
    iny
    dec ByteCount
    bne ReturnDataLoop
    clc
ReturnDataExit
    rts
SendByte
    bbc PMREQ,Port_P3,.Fail
    ldm #Dir_Write,VIA_Com_DIR      ; Set direction to output
    sta VIA_Com                     ; Send byte to VIA
    clb PMACK,Port_P3
    bbc PMREQ,Port_P3,.Done         ; Check for transfer acknowledge
    ldx #0                          ; Initialize loop counter
.Loop
    bbc PMREQ,Port_P3,.Done         ; Check for transfer acknowledge
    dex                             ; Decrement loop counter
    bbc PMREQ,Port_P3,.Done         ; Check for transfer acknowledge
    bne .Loop                       ; Loop until timeout
.Fail
    seb PMACK,Port_P3
    ldm #Dir_Read,VIA_Com_DIR       ; Set direction back to input
    sec                             ; Return failure
    rts
.Done
    seb PMACK,Port_P3
    ldm #Dir_Read,VIA_Com_DIR       ; Set direction back to input
    clc                             ; Return success
    rts
BatteryManage
    jsr ReadBattery
    lda HICHGLevel
    cmp #732-512                    ; Compare max HICHG level
    bcc .ref5V_Check                ; Skip if it's lower than the max
    lda #732-512                    ; Set it back to the max
.ref5V_Check
    cmp ref5V_Level
    bcs .LAB_EA6D
.LAB_EA6D
    lda PowerFlags
    asl A
    asl A
    asl A
    eor Port_P1
    bbs 3,A,.LAB_EA7D
    seb 1,IntFlags
    seb charger_changed,PowerFlags
    jsr batteryNow
.LAB_EA7D
    clb charger_connected,PowerFlags
    clb battery_dead,PowerFlags
    clb 6,PowerFlags
    bbc CHRG_ON,Port_P1,.HICHG_OnCheck
    lda lowBatteryLevel
    cmp #602-512
    bcs .RestoreDefaults
    cmp #571-512
    bcc .RestoreDefaults
    lda deadBatteryLevel
    cmp #602-512
    bcs .RestoreDefaults
    cmp #571-512
    bcc .RestoreDefaults
    lda lowBatteryLevel
    cmp deadBatteryLevel
    bcc .RestoreDefaults
.LAB_EAA0
    lda lowBatteryLevel
    lsr A
    sta $20
    lda deadBatteryLevel
    lsr A
    clc
    adc $20
    sta $20
    lda BatteryLevel
    bcc .LAB_EABC
    bbc battery_low,PowerFlags,.LAB_EAD5
    clb battery_low,PowerFlags
    seb 1,IntFlags
    bra .LAB_EAD5
.LAB_EABC
    seb battery_low,PowerFlags
    seb 1,IntFlags
    cmp $20
    bcs .LAB_EAD5
    seb 6,PowerFlags
    cmp deadBatteryLevel
    bcs .LAB_EAD5
    seb battery_dead,PowerFlags
    bbs 6,$0,.LAB_EAD5
    bbc VIA_TEST,Port_P3,.LAB_EAD5
    jmp Sleep
.LAB_EAD5
    ldm #0,$21
    rts
.RestoreDefaults
    ldm #590-512,lowBatteryLevel    ; Set default low battery level
    ldm #574-512,deadBatteryLevel   ; Set default dead battery level
    ldm #712-512,HICHGLevel         ; Set default HICHG level
    jsr FUN_EF84
    sta $AF
    bra .LAB_EAA0
.HICHG_OnCheck
    seb charger_connected,PowerFlags
    clb battery_low,PowerFlags
    lda $21
    bne .LAB_EB03
    seb HICHG,Port_P1
    seb 5,$14
    seb hichg_on,PowerFlags
    ldm #1,$21
    ldm #1,$22
    ldm #1,$23
    clb hichg_overflow,PowerFlags
    rts
.LAB_EB03
    cmp #2
    bcs .HICHG_OffCheck
    lda BatteryLevel
    cmp #720-512
    bcs .LAB_EB1B
    bbs hichg_overflow,PowerFlags,.LAB_EB32
    inc $22
    bne .LAB_EB32
    inc $23
    bne .LAB_EB32
    seb hichg_overflow,PowerFlags
    rts
.LAB_EB1B
    ldm #2,$21
    rts
.HICHG_OffCheck
    bne .LAB_EB32
    dec $22
    bne .LAB_EB32
    dec $23
    bne .LAB_EB32
    ldm #3,$21
    clb HICHG,Port_P1
    clb 5,$14
    clb hichg_on,PowerFlags
.LAB_EB32
    lda BatteryLevel
    cmp #602-512
    bcc .LAB_EAD5
    rts
ReadBattery
    clb AD_Conv,AD_Ctrl_Reg
    ldm #AD_Batt,AD_Ctrl_Reg
    seb AD_Conv,AD_Ctrl_Reg
    clb AD_Int_Req,Int_Ctrl_Reg
    ldm #0,AD_Start_Addr
.WaitLoop1
    lda Int_Ctrl_Reg
    nop
    bbc AD_Int_Req,A,.WaitLoop1
    lda AD_Reg
    clc
    adc BatteryLevel
    ror A
    sta BatteryLevel
    clb AD_Conv,AD_Ctrl_Reg
    ldm #AD_Ref,AD_Ctrl_Reg
    seb AD_Conv,AD_Ctrl_Reg
    clb AD_Int_Req,Int_Ctrl_Reg
    ldm #0,AD_Start_Addr
.WaitLoop2:
    lda Int_Ctrl_Reg
    nop
    bbc AD_Int_Req,A,.WaitLoop2
    lda AD_Reg
    sta ref5V_Level
    clb AD_Conv,AD_Ctrl_Reg
    rts
batteryNow
    clb AD_Conv,AD_Ctrl_Reg         ; Clear conversion bit
    ldm #AD_Batt,AD_Ctrl_Reg        ; Reading from the battery A/D pin
    seb AD_Conv,AD_Ctrl_Reg         ; Set conversion bit
    clb AD_Int_Req,Int_Ctrl_Reg
    ldm #0,AD_Start_Addr
.WaitLoop1
    lda Int_Ctrl_Reg
    nop
    bbc AD_Int_Req,A,.WaitLoop1
    lda AD_Reg
    sta BatteryLevel
    clb AD_Conv,AD_Ctrl_Reg
    ldm #AD_Ref,AD_Ctrl_Reg
    seb AD_Conv,AD_Ctrl_Reg
    clb AD_Int_Req,Int_Ctrl_Reg
    ldm #0,AD_Start_Addr
.WaitLoop2
    lda Int_Ctrl_Reg
    nop
    bbc AD_Int_Req,A,.WaitLoop2
    lda AD_Reg
    sta ref5V_Level
    clb AD_Conv,AD_Ctrl_Reg
    rts
FUN_EB97
    clb Int1_Req,Int_Ctrl_Reg
    clb 5,$0
    bbs 6,$0,.Skip
    bbs VIA_TEST,Port_P3,.Skip
    ldm #%01001000,Port_P0
    seb NC,Port_P1
.Skip
    dec $24
    beq .LAB_EBAB
    rts
.LAB_EBAB
    clb OneSec,Port_P3
    seb OneSec,Port_P3
    ldm #$3C,$24
    inc timeA
    bne .LAB_EBC0
    inc timeB
    bne .LAB_EBC0
    inc timeC
    bne .LAB_EBC0
    inc timeD
.LAB_EBC0
    jsr CombineTime
    sta Time
    bbc wake_time_on,$0,.LAB_EBE8
    bbc 6,$0,.LAB_EBE8
    bbc 5,$0,.LAB_EBE8
    lda $2D
    cmp timeA
    bne .LAB_EBE8
    lda $2C
    cmp timeB
    bne .LAB_EBE8
    lda $2B
    cmp timeC
    bne .LAB_EBE8
    lda $2A
    cmp timeD
    seb 5,$0
.LAB_EBE8
    jsr BatteryManage
    rts
;
ADBCMDDo
    lda ADBStatus
    and %101
    sta ADBStatus
    lda LastADBCMD
    sta $4                          ; Save a copy of the last ADB command
    seb ADB_Out,Port_P4
    ldx #131                        ; Load delay counter
    jsr DelayLoop
    nop
    clb ADB_Out,Port_P4
    ldx #6                          ; Load delay counter
    jsr DelayLoop
    bbc ADB_In,Port_P4,.LAB_EC58
    nop 
    ldy #8
    bra .LAB_EC10
.LAB_EC0B
    ldx #1
    jsr DelayLoop
    nop
.LAB_EC10
    seb ADB_Out,Port_P4
    ldx #2
    jsr DelayLoop
    rol $4
    bcc .LAB_EC21
    clb ADB_Out,Port_P4
    bbc ADB_In,Port_P4,.LAB_EC58
    bra .LAB_EC23
.LAB_EC21
    jsr DelayExit
.LAB_EC23
    ldx #1
    jsr DelayLoop
    clb ADB_Out,Port_P4
    bbc ADB_In,Port_P4
    dey
    bne .LAB_EC0B
    ldx #1
    jsr DelayLoop
    nop
    nop
    seb ADB_Out,Port_P4
    ldx #9
    jsr DelayLoop
    clb ADB_Out,Port_P4
    bbc ADB_In,Port_P4,.LAB_EC42
.LAB_EC40
    bra .LAB_EC60
.LAB_EC42
    ldx #9
.LAB_EC44
    bbs ADB_In,Port_P4,.LAB_EC40
    dex
    bne .LAB_EC44
    ldx #19
.LAB_EC4C
    bbs ADB_In,Port_P4.LAB_EC54
    dex
    bne .LAB_EC4C
    bra .LAB_EC58
.LAB_EC54
    seb adb_srq,ADBStatus           ; Set SRQ flag
    bra .LAB_EC60
.LAB_EC58
    seb adb_buscontention,ADBStatus
    seb adb_error,ADBStatus
    clb ADB_Out,Port_P4
    sec
    rts
.LAB_EC60
    tst ADBCMDLength
    bne .LAB_EC66
    clc
    rts
.LAB_EC66
    ldy #8
    ldx ADBCMDLength
    dec ADBCMDLength
    lda 7,X
    sta $4
    ldx #25
    jsr DelayLoop
    seb ADB_Out,Port_P4
    nop
    ldx #3
    jsr DelayLoop
    clb ADB_Out,Port_P4
    nop
    nop
    ldx #6
    jsr DelayLoop
    bbc ADB_In,Port_P4,.LAB_EC58
    bra .LAB_EC8D
.LAB_EC88
    ldx #1
    jsr DelayLoop
    nop
.LAB_EC8D
    seb ADB_Out,Port_P4
    ldx #2
    jsr DelayLoop
    rol $4
    bcc .LAB_EC9E
    clb ADB_Out,Port_P4
    bbc ADB_In,Port_P4,.LAB_EC58
    bra .LAB_ECA0
.LAB_EC9E
    jsr DelayExit
.LAB_ECA0
    ldx #1
    jsr DelayLoop
    clb ADB_Out,Port_P4
    bbc ADB_In,Port_P4,.LAB_EC58
    dey
    bne .LAB_EC88
    ldy #8
    ldx ADBCMDLength
    lda 7,X
    sta $4
    dec ADBCMDLength
    bpl .LAB_EC8D
    nop
    seb ADB_Out,Port_P4
    ldx #9
    jsr DelayLoop
    clb ADB_Out,Port_P4
    clc
    rts
FUN_ECC3
    ldm #0,ADBCMDLength
    txs
    stx $2
    ldx #$10
    txs
    lda #1
    ldy #9
    ldx #16
.LAB_ECD2
    bbc ADB_In,Port_P4,.LAB_ECE6
    dex
    bbc ADB_In,Port_P4,.LAB_ECE6
    bne .LAB_ECD2
    bra .LAB_ECDD
.LAB_ECDD
    seb adb_error,ADBStatus
    seb adb_noreply,ADBStatus
    ldx $2
    txs
    sec
    rts
.LAB_ECE6
    ldx #3
    bbs ADB_In,Port_P4,.LAB_ECDD
.LAB_ECEB
    bbs ADB_In,Port_P4,.LAB_ECF6
    dex
    bbs ADB_In,Port_P4,.LAB_ECF6
    bne .LAB_ECBB
    bra .LAB_ECDD
.LAB_ECF6
    nop
    nop
    nop
    nop
    bit $2
    bbc ADB_In,Port_P4,.LAB_ECDD
    bbc ADB_In,Port_P4,.LAB_ECDD
    bbc ADB_In,Port_P4,.LAB_ECDD
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bbc ADB_In,Port_P4,.LAB_ED28
    bra .LAB_ECDD
.LAB_ED28
    bbs ADB_In,Port_P4,.LAB_ED8E
    bbs ADB_In,Port_P4,.LAB_ED8B
.LAB_ED2E
    bbs ADB_In,Port_P4,.LAB_ED88
    bbs ADB_In,Port_P4,.LAB_ED85
.LAB_ED34
    bbs ADB_In,Port_P4,.LAB_ED82
    bbs ADB_In,Port_P4,.LAB_ED7F
    bbs ADB_In,Port_P4,.LAB_ED7C
    bbs ADB_In,Port_P4,.LAB_ED79
    bbs ADB_In,Port_P4,.LAB_ED76
    bbs ADB_In,Port_P4,.LAB_ED73
    bbs ADB_In,Port_P4,.LAB_ED70
    bbs ADB_In,Port_P4,.LAB_ED6D
    bbs ADB_In,Port_P4,.LAB_ED6A
    bbs ADB_In,Port_P4,.LAB_ED67
    bbs ADB_In,Port_P4,.LAB_ED64
    bbs ADB_In,Port_P4,.LAB_ED61
    bbs ADB_In,Port_P4,.LAB_ED5E
.LAB_ED5B
    jmp .LAB_ECDD
.LAB_ED5E
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED61
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED64
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED67
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED6A
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED6D
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED70
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED73
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED76
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED79
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED7C
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED7F
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED82
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED85
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED88
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED8B
    bbc ADB_In,Port_P4,.LAB_EDC5
.LAB_ED8E
    bbc ADB_In,Port_P4,.LAB_EDC5
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    bbc ADB_In,Port_P4,.LAB_EDDD
    cmp #1
    bne .LAB_ED5B
    cpy #8
    bcs .LAB_ED5B
    bra .LAB_EDF0
.LAB_EDC5
    asl A
    bcc .LAB_EDED
    pha
    lda #1
    bbs ADB_In,Port_P4.LAB_EDD4
    dey
    beq .LAB_EDDA
    jmp .LAB_ED34
.LAB_EDD4
    dey
    beq .LAB_EDDA
    jmp .LAB_ED88
.LAB_EDDA
    jmp .LAB_ECDD
.LAB_EDDD
    sec
    rol A
    bcc .LAB_EDED
    pha
    lda #1
    bbs ADB_In,Port_P4,.LAB_EDD4
    dey
    beq .LAB_EDDA
    jmp .LAB_ED34
.LAB_EDED
    jmp .LAB_ED2E
.LAB_EDF0
    ldx $2
    txs
    tya
    eor #$FF
    clc
    adc #$A
    sta ADBCMDLength
    tay
    beq #$EF0F
    ldx #8
.LAB_EE00
    lda 8,X
    pha
    dex
    dey
    bne .LAB_EE00
    ldx ADBCMDLength
.LAB_EE0F
    clc
    rts
CMDTable
.word   Invalid_Command 
.word   Power_Command
.word   ADB_Command
.word   Time_PRAM_Command
.word   Contrast_Command
.word   Modem_Command
.word   Battery_Command
.word   Sleep_Command
.word   Timer_Command
.word   Sound_Command
.word   Invalid_Command
.word   Invalid_Command
.word   Invalid_Command
.word   Invalid_Command
.word   PMGR_Command
.word   Invalid_Command
Invalid_Command
    clc
    rts
Power_Command
    bbs readBit,CommandByte,.powerRead
    tst ByteCount
    bne .powerCntl
    rts
.powerCntl
    lda ByteCount
    pha
    jsr WriteToLocation
    pla
    sta ByteCount
    ldx #0
.ControlLoop
    lda $13,X
    bpl .TurnOn                     ; Turning devices on?
    eor #$FF
    and Port_P0
    sta $2
    lda $13,X
    and #%01000000
    ora $2
    sta Port_P0
    bra .Next
.Next
    inx                             ; Increment memory counter
    dec ByteCount                   ; Decrement loop counter
    bne .ControlLoop                ; Loop if unfinished
    clc
    rts
.TurnOn
    and #$7F
    ora Port_P0
    sta $2
    lda $13,X
    and #%01000000
    eor #%01111111
    and $2
    sta Port_P0
    bra .Next
.powerRead
    ldm #1,ByteCount
    lda Port_P0
    and #%01111111
    eor #%00111111
    seb SOUND_PWR,A
    bbs SOUND_LATCH,In_Reg,.ReturnData
    clb SOUND_PWR,A
.ReturnData
    sta $13
    jsr ReturnDataToHost
    rts
readADB
    ldm #0,$1C                      
    ldm #LastADBCMD,$1B             ; Point to last ADB command, status         
    lda ADBCMDLength                ; Load length of ADB data
    clc
    adc #3                          ; Send three bytes, plus ADB data
    sta ByteCount
    jsr ReturnDataToHost2           ; Send the data to the host
    bcs .exit
    bbs 1,$0,.exit
    clb ADBInt,IntFlags             ; Clear interrupt
.exit
    rts
ADB_Command
    lda CommandByte
    cmp cmd_pMgrADB
    beq .pMgrADB
    cmp cmd_pMgrADBoff
    bne .bad_command
.pMgrADBoff
    ldm #1,ADBStatus
    clb ADBInt,IntFlags
    clc
    rts
.pMgrADB
    jsr WriteToLocation
    clb ADBInt,IntFlags
    lda $13                         ; Read ADB command
    sta LastADBCMD
    lda $14                         ; Read ADB status
    sta ADBStatus
    seb adb_newcmd,ADBStatus
    ldy #0
    ldx $15
    beq .LAB_EECE
.LAB_EEC5
    lda $16,Y
    sta 7,X
    iny
    dex
    bne .LAB_EEC5
.LAB_EECE
    sty $3
    sty ADBCMDLength
    clc
    rts
.bad_command
    sec
    rts
Time_PRAM_Command
    bbs readBit,CommandByte,.timeRead
    lda CommandByte
    cmp cmd_timeWrite
    bne .pramWrite
.timeWrite
    ldm #$25,$1B
    ldm #0,$1C
    ldm #4,ByteCount
    jsr WriteBytesRAM
    jsr CombineTime
    sta Time
    rts
.pramWrite
    cmp cmd_pramWrite
    bne .xPramWrite
    ldm #$3F,$1B
    ldm #0,$1C
    ldm #16,ByteCount
    jsr WriteBytesRAM
    ldm #$37,$1B
    ldm #4,ByteCount
    jsr WriteBytesRAM
    jsr FUN_EF84
    sta $AF
    rts
.xPramWrite
    ldm #2,ByteCount
    jsr WriteToLocation             ; Get locationa and length from sender
    lda $13                         ; Retrieve location to write to
    clc
    adc #$2F
    sta $1B                         ; Store adjusted location to write to
    ldm #0,$1C
    lda $14                         ; Retrieve number of bytes to write
    sta ByteCount                   ; Store number of bytes to write
    jsr WriteBytesRAM
    jsr FUN_EF84
    sta $AF
    rts
.timeRead
    lda CommandByte
    cmp cmd_timeRead
    bne .pramRead
    ldm #timeD,WriteLocation
    ldm #0,WriteLocation+1
    ldm #4,ByteCount
    jsr ReturnDataToHost2
    rts
.pramRead
    cmp cmd_pramRead
    bne .xPramRead
    ldm #0,$1C
    ldm #$3F,$1B
    ldm #16,ByteCount
    ldy #0
    lda CommandByte
    jsr SendByte
    bcs .exit
    lda #$14
    jsr SendByte
    bcs .exit
    jsr ReturnDataLoop
    ldm #$37,$1B
    ldm #4,ByteCount
    ldy #0
    jsr ReturnDataLoop
.exit
    rts
.xPramRead
    ldm #2,ByteCount
    jsr WriteToLocation             ; Get location and length from sender
    lda $13
    clc
    adc #$2F
    sta $1B
    ldm #0,$1C
    lda $14
    sta ByteCount
    jsr ReturnDataToHost2
    rts
FUN_EF84
    txa
    pha
    ldx #0
    lda #0
    clc
.LAB_EF8B
    adc #$2F,X
    inx
    bpl .LAB_EF8B
    sta $2
    cmp $AF
    bne .LAB_EFA3
    sta $2
    bne .LAB_EFA9
    ldx #0
.LAB_EF9C
    ora #$2F,X
    bne .LAB_EFA9
    inx
    bpl .LAB_EF9C
.LAB_EFA3
    pla
    tax
    lda $2
    sec
    rts
.LAB_EFA9
    pla
    tax
    lda $2
    clc
    rts
; CombineTime
CombineTime
    lda #0
    clc
    adc timeD
    adc timeC
    adc timeB
    adc timeA
    rts
; Contrast_Command
Contrast_Command
    bbc readBit,CommandByte,.SetContrastReceive
.GetContrast
    lda $2E
    sta $13
    ldm #1,ByteCount
    jsr ReturnDataToHost
    rts
.SetContrastReceive
    ldm #1,ByteCount
    jsr WriteToLocation
    ;bra SetContrast
; SetContrast
SetContrast
    lda $13
    and #%00011111
    tax
    sta $2E
    lda #$42
    sec 
    sbc .ContrastTable,X
    sta Timer_2
    lda .ContrastTable,X
    sta Timer_1
    clc
    rts
.ContrastTable
.byte   2
.byte   4
.byte   6
.byte   8
.byte   10
.byte   12
.byte   14
.byte   16
.byte   18
.byte   20
.byte   22
.byte   24
.byte   26
.byte   28
.byte   30
.byte   32
.byte   34
.byte   36
.byte   38
.byte   40
.byte   42
.byte   44
.byte   46
.byte   48
.byte   50
.byte   52
.byte   54
.byte   56
.byte   58
.byte   60
.byte   62
.byte   64
; Modem_Command
Modem_Command
    bbs readBit,CommandByte,.modemRead
    ldm #1,ByteCount
    jsr WriteToLocation
    clb ring_wake_on,$0
    bbc RingWakeEnable,$13,.ModemAB
    seb ring_wake_on,$0
.ModemAB
    bbc ModemAorB,$13,.SetModemB
    seb MODEM_AB,Port_P1
    bra .ModemPower
.SetModemB
    clb MODEM_AB,Port_P1
.ModemPower
    bbc ModemPwr,$13,.ModemOff
    clb MODEM_PWR,Port_P0
    clc
    rts
.ModemOff
    seb MODEM_PWR,Port_P0
    clc
    rts
.modemRead
    ldm #0,$13
    bbs MODEM_PWR,Port_P0,.CheckModemAorB
    seb ModemPwr,$13
.CheckModemAorB
    bbc MODEM_AB,Port_P1,.CheckRingWake
    seb ModemAorB,$13
.CheckRingWake
    bbc ring_wake_on,$0,.CheckModemInstalled
    seb RingWakeEnable,$13
.CheckModemInstalled
    bbs MODEM_INS,Port_P4,.CheckRingDetect
    seb ModemInstalled,$13
.CheckRingDetect
    bbs RING_DETECT,Port_P1,.CheckModemHook
    seb RingDetect,$13
.CheckModemHook
    bbs 3,In_Reg,.done
    seb ModemHook,$13
.done
    ldm #1,ByteCount
    jsr ReturnDataToHost
    rts
; Battery_Command
;
; Read power status and battery voltage
Battery_Command
    lda CommandByte
    cmp cmd_batteryRead
    beq .batteryRead
    cmp cmd_batteryNow
    bne .exit
    jsr batteryNow
.batteryRead
    ldm #PowerFlags,$1B
    ldm #0,$1C
    ldm #3,ByteCount
    jsr ReturnDataToHost2
    bcs .exit
    bbs 1,$0,.exit
    clb 2,IntFlags
    clb 1,IntFlags
    clb charger_changed,PowerFlags
.exit
    rts
; readINT
;
; Read Power Manager interrupt data (for the host to service the interrupt)
readINT
    lda IntFlags
    sta $13
    ldm #1,ByteCount
    jsr ReturnDataToHost
    clb 3,IntFlags
    clb 1,$0
    rts
; Sleep_Command
Sleep_Command
    lda CommandByte
    cmp cmd_sleepReq
    bne .fail
    ldm #4,ByteCount
    jsr WriteToLocation
    ldm #$70,CommandByte
    ldx #4
.CompareLoop
    dex
    bmi .LAB_F0A3
    lda string_Matt,X
    cmp $13,X
    beq .CompareLoop
    ldm #$AA,CommandByte
.LAB_F0A3
    ldm #0,ByteCount
    jsr ReturnDataToHost2
    bcs .fail
    lda CommandByte
    cmp #$70
    beq Sleep
.fail
    sec
    rts
; Sleep
Sleep
    clb DISP_BLANK,Port_P4          ; Turn off the display
    sei                             ; Disable interrupts
    bit Int_Ctrl_Reg
    bpl Sleep
    jsr FUN_EB97
    ldx #$BF
    txs
.LAB_F0C0
    bit Int_Ctrl_Reg
    bpl .LAB_F0C0
    jsr FUN_EB97
.LAB_F0C7
    bit Int_Ctrl_Reg
    bpl .LAB_F0C7
    jsr FUN_EB97
    seb SOUND_PWR,Port_P0
    clb SOUND_PWR,Port_P0
    lda Port_P0
    seb SYS_PWR,A
    sta $13
    lda Port_P1
    sta $14
    clb SOUND_OFF,Port_P3
    clb 0,PWM_Ctrl_Reg              ; Turn PWM off
    clb SYS_RST,Port_P3
    clb 2,$0
    seb 6,$0
    ldm #%10011111,Port_P0
    bra .LAB_F0F2
.LAB_F0EA
    jsr FUN_EB97
    bbs AKD,Port_P1,.LAB_F0F2
    seb 2,$0
.LAB_F0F2
    clb STOP_CLK,Port_P1
    seb STOP_CLK,Port_P1
    bbs 6,PowerFlags,.LAB_F0EA
    bbc 2,$0,.LAB_F0FF
    bbs AKD,Port_P1,.LAB_F10F
.LAB_F0FF
    bbs 5,$0,.LAB_F108
    bbc ring_wake_on,$0,.LAB_F0EA
    bbs RING_DETECT,Port_P1,.LAB_F0EA
.LAB_F108
    sei
    clb Int1_Req,Int_Ctrl_Reg
    clb 5,$0
    clb wake_time_on,$0
.LAB_F10F
    sei                             ; Disable interrupts
    clb 6,$0
    lda ADBStatus
    and #4
    sta ADBStatus
    lda $13
    sta Port_P0
    lda $14
    sta Port_P1
.LAB_F120
    bit Int_Ctrl_Reg
    bpl .LAB_F127
    jsr FUN_EB97
.LAB_F127
    bbs AKD,Port_P1,.LAB_F120
    clb SYS_PWR,Port_P0             ; Enable system power
    clb KBD_RST,Port_P1             ; Hold keyboard controller in reset
    seb 0,PWM_Ctrl_Reg              ; Turn PWM on
    seb DISP_BLANK,Port_P4          ; Turn on the display
    ldx #2
.LAB_F134
    bit Int_Ctrl_Reg
    bpl .LAB_F134
    jsr FUN_EB97
    dex
    bne LAB_F134
    seb KBD_RST,Port_P1             ; Bring keyboard controller out of reset
    seb SYS_RST,Port_P3             ; Bring system out of reset
    clb Int2_Req,Int_Ctrl_Reg
    seb Int2_Enb,Int_Ctrl_Reg
    cli                             ; Enable interrupts
    jmp CommandReceive
; Timer_Command
Timer_Command
    lda CommandByte
    cmp cmd_timerSet
    bne .disableWakeUp
.timerSet
    bbs readBit,CommandByte,.timerRead  ; Useless check
    ldm #4, ByteCount
    ldm #wakeupTimeA,WriteLocation
    ldm #0,WriteLocation+1
    jsr WriteBytesRAM
    seb wake_time_on,$0
    rts
.disableWakeUp
    cmp cmd_disableWakeUp
    bne .timerRead
    clb wake_time_on,$0
    rts
.timerRead
    ldm #5,ByteCount
    lda wakeupTimeA
    sta $13
    lda wakeupTimeB
    sta $14
    lda wakeupTimeC
    sta $15
    lda wakeupTimeD
    sta $16
    ldm #0,$17
    bbc wake_time_on,$0,.LAB_F184
    seb 0,$17
.LAB_F184
    jsr ReturnDataToHost
    rts
; Sound_Command
Sound_Command
    bbs readBit,CommandByte,.soundRead
    ldm #1,ByteCount
    jsr WriteToLocation
    bbc 0,$13,.TurnSoundOn
    seb SOUND_OFF,Port_P3
    bra .SoundLatch
.TurnSoundOn
    clb SOUND_OFF,Port_P3
.SoundLatch
    bbc 1,$13,.exit
    seb SOUND_PWR,Port_P0
    clb SOUND_PWR,Port_P0
.exit
    rts
.soundRead
    ldm #0,$13
    bbc SOUND_OFF,Port_P3,.LAB_F1AA
    seb 0,$13
.LAB_F1AA
    bbs SOUND_LATCH,In_Reg,.LAB_F1AF
    seb 1,$13
.LAB_F1AF
    ldm #1,ByteCount
    jsr ReturnDataToHost
    rts
; PMGR_Command
PMGR_Command
    lda CommandByte
    cmp cmd_PmgrSoftReset
    bne .PmgrSelfTest
.PmgrSoftReset
    jsr ReturnDataToHost2
    bcs .fail
    jmp InitPorts
.PmgrSelfTest
    cmp cmd_PmgrSelfTest
    bne .readPmgrVers
    ldm #0,$13
    jsr SelfTest1
    ror $13
    jsr SelfTest2
    ror $13
    jsr SelfTest3
    ror $13
    ror $13
    ror $13
    ror $13
    ror $13
    ror $13
    ldm #1,ByteCount
    jsr ReturnDataToHost
    rts
.readPmgrVers
    cmp cmd_readPmgrVers
    bne .LAB_F200
    lda PMVers1
    sta $13
    lda PMVers2
    sta $14
    ldm #2,ByteCount
    jsr ReturnDataToHost
    rts
.LAB_F200
    lda ByteCount
    sta $2
    jsr WriteToLocation
    lda $13
    sta $1C
    lda $14
    sta $1B
    ldx $2
    beq .fail
    dex
    beq .fail
    dex
    beq .LAB_F22A
    lda CommandByte
    cmp cmd_writePmgrRAM
    bne .LAB_F22E
    ldy #0
.writeLoop
    lda $15,Y
    sta ($1B),Y
    iny
    dex
    bne .writeLoop
.LAB_F22A
    clc
    rts
.fail
    sec
    rts
.LAB_F22E
    ldx $15
    beq .LAB_F238
    stx $12
.LAB_F234
    jsr ReturnDataToHost2
    rts
.LAB_F238
    ldm #0,ByteCount
    bra .LAB_F234
SelfTest1
    lda #$E8
    sta $1C
    ldy #6
    lda #0
    sta $14
    sta $1B
.LAB_F249
    clc
    adc ($1B),Y
    bcc .LAB_F250
    inc $14
.LAB_F250
    cmp #$80
    rol $14
    rol A
    iny
    bne .LAB_F249
    bit Int_Ctrl_Reg
    bpl .LAB_F265
    sta $15
    jsr FUN_EB97
    lda $15
    ldy #0
.LAB_F265
    inc $1C
    bne .LAB_249
    ldx $14
    tay
    ora $14
    bne .check
    dex
    dey
.check
    cpx SelfTestVal1
    bne .fail
    cpy SelfTestVal1+1
    bne .fail
    clc
    rts
.fail
    sec
    rts
SelfTest2
    php
    sei
    sec
    ldy #8
.LAB_F285
    ldx TestTableA,Y
    lda 0,X
    sta $14,Y
    dey
    bpl .LAB_F285
    ldy #8
.LAB_F292
    lda TestTableB,Y
    ldx TestTableA,Y
    sta 0,X
    dey
    bpl .LAB_F292
    ldy #8
.LAB_F29F
    lda TestTableB,Y
    ldx TestTableA,Y
    eor 0,X
    bne .LAB_F2CD
    dey
    bpl .LAB_F29F
    ldy #8
.LAB_F2AE
    lda TestTableB,Y
    eor #$FF
    ldx TestTableA,Y
    sta 0,X
    dey
    bpl .LAB_F2AE
    ldy #8
.LAB_F2BD
    lda TestTableB,Y
    eor #$FF
    ldx TestTableA,Y
    eor 0,X
    bne .LAB_F2CD
    dey
    bpl .LAB_F2BD
    clc
.LAB_F2CD
    ldy #8
.LAB_F2CF
    ldx TestTableA,Y
    lda $14,Y
    sta 0,X
    dey
    bpl .LAB_F2CF
    rol A
    plp
    ror A
    rts
TestTableA
.byte   0
.byte   1
.byte   2
.byte   4
.byte   8
.byte   16
.byte   32
.byte   64
TestTableB
.byte   240
.byte   225
.byte   210
.byte   195
.byte   180
.byte   165
.byte   150
.byte   135
.byte   120
SelfTest3
    ldx #$C0
.LAB_F2F2
    dex
    cpx #$FF
    beq .exit
    ldy 0,X
    lda #0
    sec
.LAB_F2FC
    rol A
    sta 0,X
    cmp 0,X
    bne .LAB_F321
    eor #$FF
    sta 0,X
    cmp 0,X
    bne .LAB_F321
    eor #$FF
    clc
    bne .LAB_F2FC
    sty 0,X
    bit Int_Ctrl_Reg
    bpl .LAB_F2F2
    stx $15
    jsr FUN_EB97
    ldx $15
    bra .LAB_F2F2
.exit
    clc
    rts
.LAB_F321
    sty 0,x
    sec
    rts
Reset_Interrupt
    nop
    clb SYS_RST,Port_P3
.LAB_F328
    bit Int_Ctrl_Reg
    bpl .LAB_F328
    jsr FUN_EB97
.LAB_F32F
    bit Int_Ctrl_Reg
    bpl .LAB_F32F
    jsr FUN_EB97
    bbc RESET,Port_P3,.LAB_F32F
    jmp ResetEntry
Timer_2_Vec
    rti
Timer_1_Vec
    rti
Timer_X_Vec
    rti
Interrupt_1
    rti

.org    $FF00
.byte   $A2
.byte   $03
DelayLoop
    dex
    bne DelayLoop
DelayExit
    rts

.org    $FFF4
.word   Reset_Interrupt
.word   Timer_2_Vec
.word   Timer_1_Vec
.word   Timer_X_Vec
.word   Interrupt_1
.word   PMGRResetEntry
