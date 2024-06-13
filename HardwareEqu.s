BaseOfROM       EQU         $900000

Clock16M        EQU         $FE0000

RAMconfigBase   EQU         $FE0200

WarmStart       EQU         $CFC
PowerMgrVars    EQU         $D18
HeapStart       EQU         $1E00
HeapEnd         EQU         $114
CPUFlag         EQU         $12F
WhichBox        EQU         $CB3
ADBBase         EQU         $CF8
MemTop          EQU         $108
MMUType         EQU         $CB1
PWMValue        EQU         $138
Lvl1DT          EQU         $192
VIA             EQU         $1D4                    ; Pointer to VIA
SysCom          EQU         $100                    ; Start of system communication area
BufPtr          EQU         $10C
ExpandMem       EQU         $2B6
SysZone         EQU         $2A6
TheZone         EQU         $118
ApplZone        EQU         $2AA
RAMBase         EQU         $2B2
NTSC            EQU         $B3E
PWMBuf1         EQU         $B0A
PWMBuf2         EQU         $312
SoundBase       EQU         $266
ROMBase         EQU         $2AE
ROM85           EQU         $28E
ASCBase         EQU         $CC0
SMGlobals       EQU         $CC4
HWCfgFlags      EQU         $B22
OneOne          EQU         $A02
MinusOne        EQU         $A06
SwitcherTPtr    EQU         $286
JADBProc        EQU         $6B8
DSAlertTab      EQU         $2BA
FSFCBLen        EQU         $3F6
DskVerify       EQU         $12C
LoadTrap        EQU         $12D
MmInOK          EQU         $12E
SysEvtMask      EQU         $144
JKybdTask       EQU         $21A                    ; Keyboard VBL task hook [pointer]
StkLowPt        EQU         $110                    ; Lowest stack as measured in VBL task [pointer]
Ticks           EQU         $16A                    ; Tick count, time since boot [long]
MBState         EQU         $172                    ; Current mouse button state [byte]
MBTicks         EQU         $16E                    ; Tick count at last mouse button press [long]
SysFontFam      EQU         $BA6
WidthTabHandle  EQU         $B2A
TESysJust       EQU         $BAC
WordRedraw      EQU         $BA5
SysVersion      EQU         $15A
AlarmSt         EQU         $21F
GNEFilter       EQU         $29A
KeyThresh       EQU         $18E
KeyRepThresh    EQU         $190
KeyRepTime      EQU         $18A                    ; Tick count when key was last repeated [long]
DoubleTime      EQU         $2F0
CaretTime       EQU         $2F4
SPClikCaret     EQU         $209
SPKbd           EQU         $206
FlEvtMask       EQU         $25E
IAZNotify       EQU         $33C
AlarmState      EQU         $21F
Lev1AutoVector  EQU         $64
Lev2AutoVector  EQU         $68
TimeDBRA        EQU         $D00
IllegalInstructionVector    EQU $10
JFetch          EQU         $8F4
VBLQueue        EQU         $160
TimeSCCDB       EQU         $D02
TimeSCSIDB      EQU         $B24
DtskQHdr_Flags  EQU         $D92
ADBDelay        EQU         $CEA
SCSIBase        EQU         $C00
SCSIDMA         EQU         $C04
SCSIHsk         EQU         $C08
IWM             EQU         $1E0
SCCRd           EQU         $1D8
SCCWr           EQU         $1DC
PollProc        EQU         $13E
ScrnBase        EQU         $824
ScreenBytes     EQU         $C24
RowBits         EQU         $C20
ColLines        EQU         $C22
ScreenRow       EQU         $106
VertRRate       EQU         $D30
ScrVRes         EQU         $102
JHideCursor     EQU         $800
JShowCursor     EQU         $804
GrafBegin       EQU         $800
GrafEnd         EQU         $8F2
MTemp           EQU         $828
MickeyBytes     EQU         $D6A
CrsrNew         EQU         $8CE
MouseMask       EQU         $8D6
UnitNtryCnt     EQU         $1D2
UTableBase      EQU         $11C
ROMMapInsert    EQU         $B9E
Lo3Bytes        EQU         $31A
MinStack        EQU         $31E
DefltStack      EQU         $322
MMDefFlags      EQU         $326
SysMap          EQU         $A58
TopMapHndl      EQU         $A50
ResetStackPtr   EQU         $0
MonkeyLives     EQU         $100
KeyMap          EQU         $174
SCSIPoll        EQU         $C2F
DrvQHdr         EQU         $308
BtDskRfn        EQU         $B34
BootDrive       EQU         $210
BootMask        EQU         $B0E
MacsBugDMnext   EQU         $CAC
Scratch20       EQU         $1E4


VIA_Base        EQU         $F70000
VIA_BufB        EQU         $F70000
VIA_IFR         EQU         $F71A00
VIA_IER         EQU         $F71C00
VIA_ACR         EQU         $F71600
VIA_T2_H        EQU         $F71200
VIA_T2_L        EQU         $F71000
VIA_DDR_A       EQU         $F70600
VIA_DDR_B       EQU         $F70400
VIA_ORA         EQU         $F71E00
VIA_PCR         EQU         $F71800
VIA_T1C_L       EQU         $F70800
VIA_T1C_H       EQU         $F70A00

SCCRBase        EQU         $FD0000
SCCWBase        EQU         $FD8000
SCCR_aCtl       EQU         SCCRBase+2
SCCR_bCtl       EQU         SCCRBase+0
SCCW_aCtl       EQU         SCCWBase+2
SCCW_bCtl       EQU         SCCWBase+0
SCCR_aData      EQU         SCCRBase+6
SCCW_aData      EQU         SCCWBase+6


DBase           EQU         $F6E1FF
q6L             EQU         DBase+$1800
q6H             EQU         DBase+$1A00
q7L             EQU         DBase+$1C00
q7H             EQU         DBase+$1E00
ph3L            EQU         DBase+$C00
mtrOff          EQU         DBase+$1000


SCSI_Base       EQU         $F90000
SCSIrd          EQU         $F90000
SCSIwr          EQU         $F90001
sICRread        EQU         $F90010
sICRwrite       EQU         $F90011
sMRwrite        EQU         $F90021
sTCRwrite       EQU         $F90031
sCSRread        EQU         $F90040
sCSRwrite       EQU         $F90041
sReset          EQU         $F90070
sBSR            EQU         $F90050

DiagROM         EQU         $F80000
DiagROM1        EQU         $F80080

Video_Base      EQU         $FA8000

Sound_Base      EQU         $FB0000
ascMode         EQU         $FB0801
ascChipControl  EQU         $FB0802
ascFifoControl  EQU         $FB0803
ascFifoInt      EQU         $FB0804
ascWaveOneShot  EQU         $FB0805
ascVolControl   EQU         $FB0806
ascClockRate    EQU         $FB0807
ascTestReg      EQU         $FB080F

MapperBase      EQU         $FC0000
AccessBase      EQU         $FC0200
