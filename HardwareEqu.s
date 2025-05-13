HeapStart       EQU         $1E00
DtskQHdr_Flags  EQU         $D92
JHideCursor     EQU         $800
JShowCursor     EQU         $804
MacsBugDMnext   EQU         $CAC

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


SCCR_aCtl       EQU         SCCRBase+2
SCCR_bCtl       EQU         SCCRBase+0
SCCW_aCtl       EQU         SCCWBase+2
SCCW_bCtl       EQU         SCCWBase+0
SCCR_aData      EQU         SCCRBase+6
SCCW_aData      EQU         SCCWBase+6

q6L             EQU         DBase+$1800
q6H             EQU         DBase+$1A00
q7L             EQU         DBase+$1C00
q7H             EQU         DBase+$1E00
ph3L            EQU         DBase+$C00
mtrOff          EQU         DBase+$1000

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

ascMode         EQU         $FB0801
ascChipControl  EQU         $FB0802
ascFifoControl  EQU         $FB0803
ascFifoInt      EQU         $FB0804
ascWaveOneShot  EQU         $FB0805
ascVolControl   EQU         $FB0806
ascClockRate    EQU         $FB0807
ascTestReg      EQU         $FB080F



