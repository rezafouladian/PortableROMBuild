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

            



            org     $90371E
SysErrInit:
            suba.l  A0,A0



            org     $9051CE
InitIntHandler:

            
            org     $915B58
NMInit:

            org     $90613A
InitTimeMgr:


            org     $90353A
CritErr:

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

            org     $903060
BootMe:

            org     $904080
BatInt:

            org     $904074
EnvInt:

            org     $90422E
SndWatch:

            org     $9040A0
BatWatch:

            org     $90402A
PMgrInt:

            org     $905C42
ADBProc:

            org     $905678
FDBShiftInt_VIA2:

            org     $91642E
NMGNEFilter:

            org     $90651E
FSIODNETbl:


            org     $9019F8
SetupBases:
    
            org     $901A80
TMRestart_SubVIA:

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

            org     $901AB4
StoreResults:

            org     $901AB8
WrXByte:

            org     $901A62
StartTimer:

            org     $9025C2
TestSCSI:

            org     $90215A
MapRamUniqTest:

            org     $90210E
MapRamDataTest:

            org     $902064
NoTest:

            org     $901F90
ExtRAMTest:

            org     $9027DE
PRAMTest:

            org     $901E2E
RevMod3Test:

            org     $901F22
RomTest:

            org     $901FEE
AddrLineTest:

            org     $9026A6
TestASC:

            org     $902444
ViaTest:

            org     $90229A
SccLoopTest:

            org     $90235A
SccTimerTest:

            org     $904A98
BootBeep:

            org     $902322
SccRegTest:

            org     $903EA6
SizeMemory:

            org     $902068
VramAddrTest:

            org     $9020E2
VramDataTest:

            org     $901AD8
RdXByte:


            org     $9269CC
CrsrDevHandleVBL:

            org     $92679C
InitCrTable:
