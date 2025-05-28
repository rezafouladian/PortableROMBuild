CommandByte         =   $11
ByteCount           =   $12

ref5V_Level         =   $1F
lowBatteryLevel     =   $71
deadBatteryLevel    =   $72
HICHGLevel          =   $73

cmd_readINT         =   $78

readBit             =   3           ; Commands $x8-$xF are read commands

AD_Ref              =   0           ; +5V via 100k/100k voltage divider
AD_Batt             =   1           ; Battery voltage from A/D line
AD_Conv             =   3

AD_Int_Req          =   5           ; AD interrupt request bit
                                    ; Shared with Timer 1 interrupt bit
                                    ; When bit 3 of A/D control register = 1,
                                    ; the interrupt is from A/D
Int1_Req            =   7           ; Interrupt 1 request bit

Port_P1             =   $E2
NC                  =   0
AKD                 =   1
STOP_CLK            =   2           ; Stop clocks to save power
CHRG_ON             =   3           ; CHRG_ON* - Charger connected (active low)
KBD_RST             =   4           ; KBD_RST* - Keyboard controller reset (active low)
HICHG               =   5           ;
RING_DETECT         =   6
MODEM_AB            =   7

OneSec              =   4           ; 1SEC* - One second interrupt (active low)


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
    and #$F0
    lsr A
    lsr A
    lsr A
    tax
    lda CMDTable,X
    sta $1B
    lda CMDTable+1,X
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
    sta CommandByte
    jsr PM_ReceiveByte_Wait
    bcs ReceiveCommand_Exit
    sta ByteCount
    rts
WriteToLocation
    ldm #$13,$1B                    ; Set start address to $13
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
    clb 6,$1D
    bbc CHRG_ON,Port_P1,HICHG_OnCheck
    lda lowBatteryLevel
    cmp #602-512
    bcs LAB_EAD9
    cmp #571-512
    bcc LAB_EAD9
    lda deadBatteryLevel
    cmp #602-512
    bcs LAB_EAD9
    cmp #571-512
    bcc LAB_EAD9
    lda lowBatteryLevel
    cmp deadBatteryLevel
    bcc LAB_EAD9
LAB_EAA0
    lda lowBatteryLevel
    lsr A
    sta $20
    lda deadBatteryLevel
    lsr A
    clc
    adc $20
    sta $20
    lda BatteryLevel
    bcc LAB_EABC
    bbc 4,$1D,LAB_EAD5
    clb 4,$1D
    seb 1,$1
    bra LAB_EAD5
LAB_EABC
    seb 4,$1D
    seb 1,$1
    cmp $20
    bcs LAB_EAD5
    seb 6,$1D
    cmp deadBatteryLevel
    bcs LAB_EAD5
    seb 3,$1D
    bbs 6,$0,LAB_EAD5
    bbc VIA_TEST,Port_P3,LAB_EAD5
    jmp LAB_F0B3
LAB_EAD5
    ldm #0,$21
    rts
LAB_EAD9
    ldm #590-512,lowBatteryLevel
    ldm #574-512,deadBatteryLevel
    ldm #712-512,HICHGLevel
    jsr FUN_EF84
    sta $AF
    bra LAB_EAA0
HICHG_OnCheck
    seb 0,$1D
    clb 4,$1D
    lda $21
    bne LAB_EB03
    seb HICHG,Port_P1
    seb 5,$14
    seb 1,$1D
    ldm #1,$21
    ldm #1,$22
    ldm #1,$23
    clb 2,$1D
    rts
LAB_EB03
    cmp #2
    bcs HICHG_OffCheck
    lda BatteryLevel
    cmp #720-512
    bcs LAB_EB1B
    bbs 2,$1D,LAB_EB32
    inc $22
    bne LAB_EB32
    inc $23
    bne LAB_EB32
    seb 2,$1D
    rts
LAB_EB1B
    ldm #2,$21
    rts
HICHG_OffCheck
    bne LAB_EB32
    dec $22
    bne LAB_EB32
    dec $23
    bne LAB_EB32
    ldm #3,$21
    clb HICHG,Port_P1
    clb 5,$14
    clb 1,$1D
LAB_EB32
    lda BatteryLevel
    cmp #602-512
    bcc LAB_EAD5
    rts
FUN_EB39
    clb 3,AD_Ctrl_Reg
    ldm #AD_Batt,AD_Ctrl_Reg
    seb 3,AD_Ctrl_Reg
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
    bbs $6,$0,.Skip
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
    bbc 4,$0,.LAB_EBE8
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
    jsr FUN_EA5E
    rts
FUN_EBEC
    lda $6
    and #5
    sta $6
    lda $5
    sta $4
    seb ADB_Out,Port_P4
    ldx #131
    jsr DelayLoop
    nop
    clb ADB_Out,Port_P4
    ldx #6
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
    seb 3,$6
    bra .LAB_EC60
.LAB_EC58
    seb 5,$6
    seb 7,$6
    clb ADB_Out,Port_P4
    sec
    rts
.LAB_EC60
    tst $7
    bne .LAB_EC66
    clc
    rts
.LAB_EC66
    ldy #8
    ldx $7
    dec $7
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
    ldx $7
    lda 7,X
    sta $4
    dec $7
    bpl .LAB_EC8D
    nop
    seb ADB_Out,Port_P4
    ldx #9
    jsr DelayLoop
    clb ADB_Out,Port_P4
    clc
    rts
FUN_ECC3
    ldm #0,$7
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
    seb 7,$6
    seb 4,$6
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
    sta $7
    tay
    beq #$EF0F
    ldx #8
.LAB_EE00
    lda 8,X
    pha
    dex
    dey
    bne .LAB_EE00
    ldx $7
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
.LAB_EE46
    lda $13,X
    bpl .LAB_EE61
    eor #$FF
    and Port_P0
    sta $2
    lda $13,X
    and #%01000000
    ora $2
    sta Port_P0
    bra .LAB_EE5A
.LAB_EE5A
    inx
    dec ByteCount
    bne .LAB_EE46
    clc
    rts
.LAB_EE61
    and #$7F
    ora Port_P0
    sta $2
    lda $13,X
    and #%01000000
    eor #%01111111
    and $2
    sta Port_P0
    bra .LAB_EE5A
.powerRead
    ldm #1,ByteCount
    lda Port_P0
    and #%01111111
    eor #%00111111
    seb 5,A
    bbs 2,In_Reg,$EE81
    clb 5,A
.LAB_EE81
    sta $13
    jsr ReturnDataToHost
    rts
readADB
    ldm #1,$1C
    ldm #5,$1B
    lda $7
    clc
    adc #3
    sta ByteCount
    jsr ReturnDataToHost2
    bcs .exit
    bbs 1,$0,.exit
    clb 0,$1
.exit
    rts
ADB_Command
    lda CommandByte
    cmp cmd_pMgrADB
    beq .pMgrADB
    cmp cmd_pMgrADBoff
    bne .bad_command
.pMgrADBoff
    ldm #1,$6
    clb 0,$1
    clc
    rts
.pMgrADB
    jsr WriteToLocation
    clb 0,$1
    lda $13
    sta $5
    lda $14
    sta $6
    seb 1,$6
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
    sty $7
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
    jsr WriteToLocation
    lda $13
    clc
    adc #$2F
    sta $1B
    ldm #0,$1C
    lda $14
    sta ByteCount
    jsr WriteBytesRAM
    jsr FUN_EF84
    sta $AF
    rts
.timeRead
    lda CommandByte
    cmp cmd_timeRead
    bne .pramRead
    ldm #timeD,$1B
    ldm #0,$1C
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
    jsr FUN_EA38
    bcs .exit
    lda #$14
    jsr FUN_EA38
    bcs .exit
    jsr LAB_EA2A
    ldm #$37,$1B
    ldm #4,ByteCount
    ldy #0
    jsr LAB_EA2A
.exit
    rts
.xPramRead
    ldm #2,ByteCount
    jsr WriteToLocation
    lda $13
    clc
    adc #$2F
    sta $1B
    ldm #0,$1C
    lda $14
    sta ByteCount
    jsr ReturnDataToHost2
    rts
.FUN_EF84
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
CombineTime
    lda #0
    clc
    adc timeD
    adc timeC
    adc timeB
    adc timeA
    rts
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
Modem_Command
    bbs readBit,CommandByte,.modemRead
    ldm #1,ByteCount
    jsr WriteToLocation
    clb 7,$0
    bbc RingWakeEnable,$13,.ModemAB
    seb 7,$0
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
    bbc 7,$0,.CheckModemInstalled
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
Battery_Command
    lda CommandByte
    cmp cmd_batteryRead
    beq .batteryRead

