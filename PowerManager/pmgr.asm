lowBatteryLevel     EQU $71
deadBatteryLevel    EQU $72
HICHGLevel          EQU $73


.org    $E800
PMGRResetEntry:
    bra ColdEntry
PMVers1:
.byte   $2
PMVers2:
.byte   $B5
SelfTestVal1:
.byte   $76
.byte   $44
string_Matt:
.byte   "MATT"
string_Bugs:
.byte   "Bugs"
.byte   $A8
.byte   "Copyright Apple Computer Inc  1987, 1988, 1989 Written by Michael Hanlon "
ColdEntry:
    ldy #8
    bra Entry2
ResetEntry:
    ldy #0
Entry2:
    ldx #$BF
    txs
    clt
    sei
    cld
    jsr FUN_EF84
    bcc LAB_E884
    ldx #0
    lda #0
InitRAM:
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
LAB_E884:
    lda #0
    sta $0
    sta $6
    jsr CombineTime
    cmp Time
    beq LAB_E89A
    ldx #9
    lda #0
LAB_E895:
    sta $24,X
    dex
    bpl LAB_E895
LAB_E89A:
    lda Port_P0
    and #4
    ora #$40
    sty $1
    tst $1
    beq InitPorts
    lda #%10011111
InitPorts:
    sta Port_P0
