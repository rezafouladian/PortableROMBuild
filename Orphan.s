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

            org     $900BBC
StartTest1:
            movea.w #$2000,SP
            moveq   #0,D7
            moveq   #0,D6

            org     $90079E
InitVIA:
            movea.l #VIA_Base,A0

            org     $900808
InitSCC:
            movea.l #SCCWBase,A0

            org     $900754
InitIWM:
            movea.l #DBase,A0
            ;moveq   #IWMInitMode,D0
.L1:
            move.b  #$BE,($C00,A0)

            org     $900732
InitSCSI:
            lea     SCSIwrite,A0
            clr.b   ($10,A0)

            org     $900E96
RamTest:
            movem.l A6-A0/D7-D0,-(SP)



            org     $90371E
SysErrInit:
            suba.l  A0,A0

            org     $9007C0
VIATimerEnables:
            movea.l #VIA_Base,A0



            org     $9051CE
InitIntHandler:


            org     $900970
InitMemMgr:


            

            org     $90098E
InitRsrcMgr:
            
            org     $915B58
NMInit:

            org     $90613A
InitTimeMgr:


            org     $90353A
CritErr:

            org     $9009B0
GoofyDoEject:





            org     $90099E
InitDTQueue:

            org     $900836
InitVidGlobals:



            org     $904DAC
InitQueue:
            clr.w   (A1)+

            org     $906A96
InitSCSIMgr:

            org     $900902
InitIOMgr:

            org     $9053B8
InitADB:

            org     $90088A
InitCrsrMgr:

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
