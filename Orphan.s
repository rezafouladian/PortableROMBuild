            org     $904DB6
EMT1010_TrapDispatch:

            org     $904FBA
Unimplemented:

            org     $904F40
InitDispatcher:

            org     $926B6E
DispTable:

            org     $927250
RomRsrcHead:

            org     $9046C0
WakeUp:
            move    #$2700,SR
            movea.l PowerMgrVars,A2

        



            org     $9051CE
InitIntHandler:

            
            org     $915B58
NMInit:

            org     $90613A
InitTimeMgr:


            org     $909920
DoEject:




            org     $904DAC
InitQueue:
            clr.w   (A1)+

            org     $906A96
InitSCSIMgr:

            org     $9053B8
InitADB:

            org     $916E4C
InitGestalt:


            org     $905C42
ADBProc:

            org     $905678
FDBShiftInt_VIA2:

            org     $91642E
NMGNEFilter:

            org     $90651E
FSIODNETbl:

            org     $904AA8
ErrorBeep1:
            lea     $904BDC,A4
            bra     DoBeep
ErrorBeep2:
            lea     $904BFC,A4
            bra     DoBeep
ErrorBeep3:
            lea     $904C10,A4
            bra     DoBeep
ErrorBeep4:
            lea     $904C28,A4
DoBeep:

            org     $904A98
BootBeep:

            org     $907316
SCSILoad:


            org     $9269CC
CrsrDevHandleVBL:

            org     $92679C
InitCrTable:
