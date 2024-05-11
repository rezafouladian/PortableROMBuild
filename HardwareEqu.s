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
DiskVars_FlEvtMask  EQU     $25E
IAZNotify       EQU         $33C
AlarmState      EQU         $21F
Lev1AutoVector  EQU         $64
TimeDBRA        EQU         $D00
IllegalInstructionVector    EQU $10




VIA_Base        EQU         $F70000
VIA_IFR         EQU         $F71A00
VIA_IER         EQU         $F71C00
VIA_ACR         EQU         $F71600
VIA_T2_H        EQU         $F71200
VIA_T2_L        EQU         $F71000

SCCWBase        EQU         $FD8000

DBase           EQU         $F6E1FF

SCSIread        EQU         $F90000
SCSIwrite       EQU         $F90001
sICRread        EQU         $F90010
sICRwrite       EQU         $F90011

TROM1           EQU         $F80000
TROM2           EQU         $F80080

Video_Base      EQU         $FA8000

Sound_Base      EQU         $FB0000
