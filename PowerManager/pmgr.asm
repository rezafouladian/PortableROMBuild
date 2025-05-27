ref5V_Level         =   $1F
lowBatteryLevel     =   $71
deadBatteryLevel    =   $72
HICHGLevel          =   $73

cmd_readINT         =   $78

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
    sta $1D
    sta $21
    ldm #590-512,lowBatteryLevel
    ldm #574-512,deadBatteryLevel
    ldm #712-512,HICHGLevel
    jsr FUN_EF84
    sta $AF
LAB_E884
    lda #0
    sta $0
    sta $6
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
    ldm #%00000000,VIA_Com_DIR
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
    seb 0,PWM_Ctrl_Reg
    jsr batteryNow
    seb KBD_RST,Port_P1
    cpy #0
    beq LAB_E8F9
    bbc VIA_TEST,Port_P3
    jmp LAB_F0B3
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
    clb 1,Int_Ctrl_Reg
    seb 0,Int_Ctrl_Reg
    cli
CommandReceive
    ldm #0,VIA_Com_DIR
.LAB_E913
    bit Int_Ctrl_Reg
    bmi .LAB_E94A
    bbs 1,$6,LAB_E95C
    bbs PMREQ,Port_P3,.LAB_E913
    jsr ReceiveCommand
    bcs CommandReceive
    lda $11
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
    and #$F0
    lsr A
    lsr A
    lsr A
    tax
    lda CMDTableLow,X
    sta $1B
    lda CMDTableHigh,X
    sta $1C
    jsr (1B)
    bra CommandReceive
.LAB_E94A
    ldm #7,VIA_Com
    ldm #$FF,VIA_Com_DIR
    jsr FUN_EB97
    bbs 0,$1,LAB_E998
    bbs 1,$6,LAB_E95C
    bbc 2,$6,LAB_E998
LAB_E95C
    ldm #7,VIA_Com
    ldm #$FF,VIA_Com_DIR
    lda $6
    and #%101
    sta $6
    lda $3
    sta $7
    lda $5
    and #$F
    bne LAB_E982
    seb ADB_Out,Port_P4
    ldx #0
    jsr DelayLoop
    jsr DelayLoop
    jsr DelayLoop
    clb ADB_Out,Port_P4
    seb 4,$6
    bra LAB_E996
LAB_E982
    jsr FUN_EBEC
    bcc LAB_E98B
    seb 4,$6
    bra LAB_E996
LAB_E98B
    jsr FUN_ECC3
    bcc LAB_E996
    bbc 2,$6,LAB_E996
    bbc 3,$6,LAB_E998
LAB_E996
    seb 0,$1
LAB_E998
    lda $1
    and #%111
    beq LAB_E9A4
    clb PMINT,Port_P3
    seb 1,$0
    seb PMINT,Port_P3
LAB_E9A4
    jmp CommandReceive
ReceiveCommand
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta $11
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta $12
    rts
WriteToLocation
    ldm #$13,$1B                ; Set start address to $13
    ldm #$00,$1C
WriteBytesRAM
    ldy #0
WriteBytesLoop
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta ($1B),Y
    iny
    dec $12
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
    ldm #$13,$1B
    ldm #$00,$1C
ReturnDataToHost2
    lda $11
    jsr FUN_EA38
    bcs .exit
    lda $12
    jsr FUN_EA38
    bcs .exit
    tst $12
    bcs .exit
    ldy #0
.Loop
    lda ($1B),Y
    jsr FUN_EA38
    bcs .exit
    iny
    dec $12
    bne .Loop
    clc
.exit
    rts
FUN_EA38
    bbc PMREQ,Port_P3,.Fail
    ldm #%11111111,VIA_Com_DIR
    sta VIA_Com
    clb PMACK,Port_P3
    bbc PMREQ,Port_P3,.Done
    ldx #0
.Loop
    bbc PMREQ,Port_P3,.Done
    dex
    bbc PMREQ,Port_P3,.Done
    bne .Loop
.Fail
    seb PMACK,Port_P3
    ldm #0,VIA_Com_DIR
    sec
    rts
.Done
    seb PMACK,Port_P3
    ldm #0,VIA_Com_DIR
    clc
    rts
FUN_EA5E
    jsr FUN_EB39
    lda HICHGLevel
    cmp #732-512
    bcc LAB_EA69
    lda #732-512
LAB_EA69
    cmp ref5V_Level
    bcs LAB_EA6D
LAB_EA6D
    lda $1D
    asl A
    asl A
    asl A
    eor Port_P1
    bbs 3,A,LAB_EA7D
    seb 1,$1
    seb 5,$1D
    jsr batteryNow
LAB_EA7D
    clb 0,$1D
    clb 3,$1D

