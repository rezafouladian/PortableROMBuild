            INCLUDE 'HardwareEqu.s'
            INCLUDE 'ConstEqu.s'
            INCLUDE 'Orphan.s'
            INCLUDE 'ROMTools/TrapMacros.s'

            IFND PortableAbs
PortableAbs EQU     0
            ENDIF
            
            IF PortableAbs
            opt     o10-                            ; Prevent optimization from add/sub to lea
            opt     o4-                             ; Prevent optimization from move.l to moveq
            opt     ol-
            opt     og-
            opt     o11-
            ENDIF

            ; Add PC to JMP when matching original ROM
            macro   jpp
            IF PortableAbs
                jmp (\1,PC)
            ELSE
                jmp \1
            ENDIF
            endm

            ; Add PC to JSR when matching original ROM
            macro   jsp
            IF PortableAbs
                jsr (\1,PC)
            ELSE
                jsr \1
            ENDIF
            endm

            macro BigLea
                lea (\1-*).l,\2
                lea (*-6,PC,\2.l),\2
            endm

            macro BSR6
                lea .\@,A6
                jmp (\1,PC)
            .\@:
            endm

            macro BigJsr
                lea (\1-*).l,\2
                jsr (*-6,PC,\2.l)
            endm

            ; This delay is used for SCC access during tests
            ; It is defined as a macro here to quickly redefine it for faster processors if needed.
            macro _SCCDelay
                move.w  (SP),(SP)
            endm

            org     BaseOfROM
ROMChecksum dc.l    $96CA3846
StartPC     dc.l    ResetEntry
ROMVersion  dc.b    $3
            dc.b    $7A
StBoot      jpp     StartBoot
BadDisk     jpp     StartBoot
            dc.w    0
PatchFlags  dc.b    0
            dc.b    0
            dc.l    ForeignOS-BaseOfROM
RomRsrc     dc.l    RomRsrcHead-BaseOfROM
Eject       jpp     GoofyDoEject
DispOff     dc.l    DispTable-BaseOfROM
Critical    jpp     CritErr
ResetEntry  jpp     StartBoot
RomLoc      dc.b    0
            dc.b    0
myROMSums   dc.l    $961F12
            dc.l    $AA82FA
            dc.l    0
            dc.l    0
            dc.l    $40000
ForeignOS   dc.l    InitDispatcher-BaseOfROM
            dc.l    EMT1010_TrapDispatch-BaseOfROM
            dc.l    Unimplemented-BaseOfROM
            dc.l    EMT1010_TrapDispatch-BaseOfROM
            dc.l    0
            
StartBoot:
            move    #1<<SupervisorBit|1<<IPL2|1<<IPL1|1<<IPL0,SR    ; Disable processor interrupts
            lea     BaseOfROM,A0
            move.l  A0,D0
            bne.b   .L1
            move.l  #BaseOfROM,D0
            jmp     (.L1,PC,D0.l)
.L1:
            tst.w   Clock16M                        ; 
            move.w  #RAMconfigInit,RAMconfigBase
            cmpi.l  #sleepConst,WarmStart           ; Check if we're resuming from sleep
            bne.b   .L2
            move.l  #wmStConst,WarmStart            ; 
            jpp     WakeUp                          ; Perform wake from sleep tasks
; Not returning from sleep, start up normally
.L2:
            jpp     StartTest1
StartInit1:
            bsr.w   InitVIA
            bsr.w   InitSCC
            bsr.w   InitIWM
            bsr.w   InitSCSI
            bsr.w   WhichCPU                        ; Get CPU type in low word of D7
            bsr.w   WhichBoard                      ; Get logic board type in high word of D7
            cmpi.l  #wmStConst,WarmStart            ; Is this a cold start
            beq.b   .L1                             ; Skip the RAM test if it's a warm start
            movea.l A5,A1
            movea.l SP,A0
            jsp     RamTest
            movea.l SP,A1
            suba.l  A0,A0                           ; Test from the beginning of RAM
            movea.l A5,SP
            jsp     RamTest
            movea.l A1,SP
.L1:
            move.l  WarmStart,-(SP)                 ; Save WarmStart value
            lea     SysCom,A0                       ; A0 = pointer to start of system globals
            lea     HeapStart,A1                    ; A1 = pointer to end of system globals
            bsr.w   FillWithOnes                    ; Fill system globals with ones
            move.l  (SP)+,WarmStart                 ; Restore WarmStart value
            move.b  D7,CPUFlag                      ; Save type of CPU we have
            swap    D7
            move.b  D7,WhichBox
            move.l  A6,MemTop                       ; Save the top of available memory
            BSR6    SysErrInit
            bsr.w   SetUpTimeK                      ; Initialize TimeDBRA and TimeSCCDB
            bsr.w   VIATimerEnables
            clr.b   MMUType
            movem.l DiagROM1,D0/A0
            cmpi.l  #TROMCode,D0
            bne.b   .L3
            lea     BootRetry,A1
            jmp     (A0)
.L3:
            bsr.w   InitHiMemGlobals                ; Set up high memory
; BootRetry
BootRetry:
            move    #$2700,SR                       ; Disable interrupts
            movea.l VIA,A1
            move.b  #$4,(VIA_IER-VIA_Base,A1)
            bsr.w   InitGlobalVars                  ; Initialize a bunch of global variables
            BigJsr  InitIntHandler,A0
            bsr.w   InitDispatcher
            bsr.w   GetPRAM
            bsr.w   InitMemMgr
            bsr.w   SetUpSysAppZone
            bsr.w   InitSwitcherTable
            bsr.w   InitPMgrVars
            bsr.w   InitRsrcMgr
            BigJsr  NMInit,A0
            BigJsr  InitTimeMgr,A0
            bsr.w   InitADBvars
            bsr.w   InitShutdownMgr
            bsr.w   InitDTQueue
            move    #$2000,SR
            bsr.w   InitVidGlobals
            bsr.w   CompBootStack
            movea.l A0,SP                           ; Set the stack pointer there
            suba.w  #BootStackSize,A0               ; Give ourselves some stack space
            _SetApplLimit                           ; Don't let the system heap crash our stack
            lea     DrvQHdr.w,A1
            jsp     InitQueue
            BigJsr  InitSCSIMgr,A0
            bsr.w   InitIOMgr
            BigJsr  InitADB,A0
            bsr.w   InitCrsrMgr
            moveq   #$70,D0                         
            _NewPtrSysClear
            move.l  A0,ExpandMem
            movea.l A0,A4
            move.w  #$106,(A0)+
            move.l  #$70,(A0)
.P5         lea     InitGestalt-.P5,A0
            jsr     (.P5,PC,A0.l)
            movea.l SysZone,a0
            movea.l (A0),A0
            adda.w  #$4000,A0
            _SetApplBase
            movea.l SysZone,A0
            move.l  A0,TheZone
            move.l  A0,ApplZone
            move.l  (A0),HeapEnd
            lea     ($400,SP),A6
            lea     ($190,SP),A5
            lea     (-$10,SP),SP
            lea     ($C,SP),A0
            move.l  A0,($8,SP)
            move.l  A0,($4,SP)
            moveq   #$5B,D0
            move.b  D0,(A0)
            move.w  #1,($2,SP)
            move.w  #powerCntl,(SP)
            movea.l SP,A0
            _PMgrOp
            lea     ($10,SP),SP
            bsr.w   DrawBeepScreen
            move.l  #wmStConst,WarmStart
            bra.w   BootMe
; JmpTblInit
;
; Sets up RAM based jump tables from ROM routine offset tables
;
; Inputs:   A0  Pointer to table of routine word offsets (A0-relative)
;           A1  Pointer to destination jump table in RAM
;           D1  Number of vectors to install - 1
; Outputs:  A0  Pointer to next entry in offset table
;           A1  Pointer to next jump table entry in RAM
JmpTblInit:
            move.l  A0,D0
.JmpTbl2:
            moveq   #0,D2
            move.w  (A0)+,D2
            add.l   D0,D2
            move.l  D2,(A1)+
            dbf     D1,.JmpTbl2
            rts
; FillWithOnes
;
; Fills a longword aligned memory range with all 1's
; 
; Inputs:   A0  Pointer to starting RAM location
;           A1  Pointer to ending RAM location
; Outputs:  A0
;           A1
FillWithOnes:
            move.l  A1,D0
            sub.l   A0,D0
            lsr.l   #2,D0
            moveq   #-$1,D1
.FillLoop:
            move.l  D1,(A0)+
            subq.l  #1,D0
            bne.b   .FillLoop
            rts
CompBootStack:
            move.l  BufPtr,D0                       ; Get top of usable memory
            lsr.l   #1,D0                           ; Divide by two
            movea.l D0,A0
            suba.w  #$400,A0
            rts
SetUpSysAppZone:
            lea     SysHeap,A0
            _InitZone
            move.l  TheZone,SysZone
            move.l  SysZone,RAMBase
            movea.l SysZone,A0
            move.l  A0,ApplZone
            movea.l (bkLim,A0),A0
            move.l  A0,HeapEnd
            bsr.b   CompBootStack
            cmpa.l  SP,A0
            bls     .L3
            movea.l SP,A0
.L3:
            suba.w  #BootStackSize,A0
            _SetApplLimit
            rts
SysHeap:
            dc.l    HeapStart                       ; Start address
            dc.l    HeapStart+SysZoneSize           ; Size
            dc.w    2*dfltMasters                   ; Number of master pointers
            dc.l    0                               ; No growzone proc
DrawBeepScreen:
            pea     (-$4,A5)
            _InitGraf
            pea     (-$200,A6)
            _OpenPort
            movea.l (A5),A2
            pea     (-$6C,A2)
            _SetCursor
            _PenNormal
            tst.b   NTSC
            bne.b   .L1
            lea     (-$74,A2),A0
            move.l  A0,-(SP)
            pea     (-$18,A2)
            _FillRect
            rts
.L1:
            moveq   #0,D0
            movea.l PowerMgrVars,A0
            tst.b   (NTSCcopy,A0)
            bpl.b   .L2
            moveq   #-1,D0
.L2:
            cmpi.b  #2,NTSC
            bne.b   .L4
            movea.l #Video_Base,A0
            move.w  #8000,D1
            subq.w  #1,D1
.L3:
            move.l  D0,(A0)+
            dbf     D1,.L3
.L4:
            lea     (-$74,A2),A0
            move.l  A0,-(SP)
            lea     ($9FA),A1
            move.l  A1,-(SP)
            move.l  A1,-(SP)
            move.l  (A0)+,(A1)+
            move.l  (A0),(A1)
            move.l  #$FFFDFFFD,-(SP)
            _InsetRect
            move.l  #$30003,-(SP)                   ; Make pen 3 pixels wide
            _PenSize
            move.l  #$160016,-(SP)                  ; Rounding factor ($00100010 + 2 * $00030003)
            _FrameRoundRect
            _PenNormal
            move.l  #$100010,-(SP)
            pea     (-$18,A2)
            _FillRoundRect
            rts
InitShutdownMgr:
            clr.w   -(SP)
            _Shutdown
            rts
InitPMgrVars:
            move.l  #158,D0
            _NewPtrSysClear
            move.l  A0,PowerMgrVars
            movea.l A0,A2
            lea     (SleepQHdr,A2),A1
            jsp     InitQueue
            st      (TOdirtyFlag,A2)
            moveq   #1,D0
            move.l  D0,(LastAct,A2)
            move.l  D0,(LastHd,A2)
            move.b  #CPUSpeed16MHz,(SaveSpeedo,A2)
            lea     BatInt,A1
            move.l  A1,(vBatInt,A2)
            lea     EnvInt,A1
            move.l  A1,(vEnvInt,A2)
            lea     (SwVBLTask,A2),A0
            move.w  #1,($4,A0)
            move.w  #SndWFreq,($A,A0)
            lea     SndWatch,A1
            move.l  A1,($6,A0)
            _VInstall
            lea     (BatVBLTask,A2),A0
            move.w  #1,($4,A0)
            move.w  #BatFreq,($A,A0)
            lea     BatWatch,A1
            move.l  A1,($6,A0)
            _VInstall
            lea     PMgrInt,A0
            move.l  A0,Lvl1DT+$10
            lea     (-$10,SP),SP
            lea     ($C,SP),A0
            move.l  A0,($8,SP)
            move.l  A0,($4,SP)
            clr.l   (A0)
            clr.w   ($2,SP)
            move.w  #pMgrADBoff,(SP)
            movea.l SP,A0
            _PMgrOp
            move.l  #$100070,D0
            _ReadXPRam
            move.b  (VidMode-PmgrPramBase,A0),(NTSCcopy,A2)
            move.b  (NTSCcopy,A2),NTSC
            bclr.b  #7,NTSC
            tst.w   (A0)
            beq.b   .L1
            tst.w   (PmgrStatusFlags-PmgrPramBase,A0)
            bne.b   .L2
.L1:
            move.b  #DfltSlpTime,(SlpTimeOut-PmgrPramBase,A0)
            move.b  #DfltHDTime,(HDTimeOut-PmgrPramBase,A0)
            move.w  #$4E3E,(PmgrStatusFlags-PmgrPramBase,A0)
            move.l  #$4*$10000+PmgrPramBase,D0
            _WriteXPRam
.L2:
            move.b  (SlpTimeOut-PmgrPramBase,A0),(SleepTime,A2)
            move.b  (HDTimeOut-PmgrPramBase,A0),(HDTime,A2)
            move.w  (PmgrStatusFlags-PmgrPramBase,A0),(LowWarn,A2)
            lea     ($10,SP),SP
            movea.l (VIA),A1
            move.b  #%10010000,(VIA_IFR-VIA_Base,A1)
            move.b  #%10010000,(VIA_IER-VIA_Base,A1)
            rts
InitADBvars:
            move.l  #FBDBSize,D0                    ; Get local data area length
            _NewPtrSysClear                         ; Allocate space on heap and clear
            move.l  A0,ADBBase                      ; Save pointer to ADBBase
.P1         lea     (ADBProc-.P1).l,A0              ; Get the ADBProc
            lea     (.P1,PC,A0.l),A0
            move.l  A0,JADBProc                     ; Install it into JADBProc vector
.P2         lea     (FDBShiftInt_VIA2-.P2).l,A0
            lea     (.P2,PC,A0.l),A0
            move.l  A0,(Lvl1DT+8)
            rts
InitHiMemGlobals:
            move.w  #-$1,PWMValue                   ; Set current PWM speed invalid
            movea.l A5,A0
            suba.w  #$2FF,A0
            move.l  A0,PWMBuf1
            move.l  A0,PWMBuf2
            subq.w  #1,A0
            move.l  A0,SoundBase
            move.l  A0,BufPtr                       ; Save pointer to top of usable RAM
            rts
            IF PortableAbs
            org     $900460
            ENDIF
InitGlobalVars:
            lea     BaseOfROM,A0                    ; Point to ROMBase
            move.l  A0,ROMBase
            moveq   #-1,D0
            move.l  D0,SMGlobals
            move.l  #Sound_Base,ASCBase
            move.b  #$7F,ROM85
            move.w  #$C500,HWCfgFlags
            move.l  #$10001,OneOne                  ; Setup magic constants
            moveq   #-1,D0
            move.l  D0,MinusOne                     ; Setup MinusOne
            bsr.w   InitSCCGlobals
            bsr.w   InitIWMGlobals
            bsr.w   InitVIAGlobals
            bsr.w   InitSCSIGlobals
            clr.l   DSAlertTab
            move.w  MinusOne,FSFCBLen               ; Mark that FS needs (re)initialization
            BigLea  FSIODNETbl,A0                   ; Point to the offset table
            lea     JFetch,A1                       ; Point to first jump table entry
            moveq   #2,D1                           ; There are 3 vectors
            bsr.w   JmpTblInit
            clr.b   DskVerify                       ; No disk verify
            clr.b   LoadTrap                        ; No trap before launch
            clr.b   MmInOK                          ; Inital memory manager checks
            clr.w   SysEvtMask                      ; Don't allow any events to be posted
            clr.l   JKybdTask                       ; No keyboard task yet
            clr.l   StkLowPt                        ; Set stack low at this time (turn off check during VBL)
            lea     VBLQueue,A1
            jsp     InitQueue                       ; Initialize VBL queue header
            clr.l   Ticks                           ; Clear system tick count
            move.b  #$80,MBState                    ; Set current mouse button state to up
            clr.l   MBTicks                         ; Clear timestamp for mouse button
            clr.l   SysFontFam                      ; Clear SysFontFam and SysFontSize
            clr.l   WidthTabHandle
            clr.w   TESysJust
            clr.b   WordRedraw
            jsp     InitCrsrVars
            clr.w   SysVersion
            bclr.b  #0,AlarmState
            BigLea  NMGNEFilter,A0
            move.l  A0,GNEFilter
            clr.l   IAZNotify                       ; No InitApplZone notify proc
            move.w  #$FF7F,FlEvtMask                ; Init for disable of DIP flushes
            rts
            IF PortableAbs
            org     $900524
            ENDIF
SwitchGoodies:
            dc.l    $4000400
            dc.l    $E001000
            dc.l    $B800026
            dc.l    $BAE0052
            dc.l    $3760002
            dc.l    $3840002
            dc.l    $3520004
            dc.l    $2F80002
            dc.l    $3980004
            dc.l    $D540004
            dc.l    $0
            dc.l    $0
            dc.l    $4
            dc.l    $0
SwitchLen   EQU     *-SwitchGoodies
WDCBSWOS:
            dc.w    $28
PMPSWOS:
            dc.w    $2E
InitSwitcherTable:
            moveq   #SwitchLen,D0                   ; Allocate a switcher table
            _NewPtrSysClear                         ; in the system heap
            movea.l A0,A1                           ; Make it the destination
            lea     SwitchGoodies,A0                ; Load table
            moveq   #SwitchLen,D0                   ; Copy it - needs to be updated
            _BlockMove                              ; with WDCBSwitch, PMSPSwitch later
            move.l  A1,SwitcherTPtr                 ; Set the pointer up for Switcher
            rts
GetPRAM:
            _InitUtil
            moveq   #0,D1
            move.b  SPKbd,D1
            moveq   #$F,D0
            and.w   D1,D0
            bne.b   .L1
            moveq   #$48,D0
.L1:
            add.w   D0,D0
            move.w  D0,KeyRepThresh
            lsr.w   #4,D1
            bne.b   .L2
            move.w  #$1FFF,D1
.L2:
            lsl.w   #2,D1
            move.w  D1,KeyThresh
            move.b  SPClikCaret,D1
            moveq   #$F,D0
            and.b   D1,D0
            lsl.b   #2,D0
            move.l  D0,CaretTime
            lsr.b   #2,D1
            moveq   #$3C,D0
            and.b   D1,D0
            move.l  D0,DoubleTime
            rts
            IF PortableAbs
            org     $9005B2
            ENDIF
; WhichCPU
;
; Determines which CPU is installed.
; 
; 0 = 68000
; 1 = 68010
; 2 = 68020
; 3 = 68030
WhichCPU:
            machine mc68040
            lea     IllegalInstructionVector,A1
            move.l  (A1),-(SP)                      ; Save the vector
            lea     .ILException,A0                 
            move.l  A0,(A1)                         ; Replace it with our exception handler
            movea.l SP,A0
            clr.w   -(SP)
            moveq   #cpu68020,D7
            move.w  #$2909,D0
            movec   D0,CACR
            movec   CACR,D0
            bclr.l  #8,D0                           ; Clear Enable Data Cache bit
            beq.b   .L2
            movec   D0,CACR
            moveq   #cpu68030,D7
            bra.b   .L2
; If we're here then an exception occurred and the CPU is a 68000 or 68010
.ILException:
            moveq   #cpu68010,D7
            cmpi.w  #$10,($6,SP)
            beq.b   .L2
            moveq   #cpu68000,D7
.L2:
            movea.l A0,SP
            move.l  (SP)+,(A1)                      ; Restore the exception handler
            rts
            machine mc68000
; WhichBoard
;
; Determine the logic board (BoxFlag) and put it in the high word of D7.
WhichBoard:
            swap    D7                              ; Move the CPU type to the high word
            clr.w   D7                              ; Clear the low word
            move.b  #boxSE,D7                       ; Default to Macintosh SE
            lea     .L1,A0                          ; Get our ROM location
            move.l  A0,D1
            btst.l  #23,D1                          ; Is the ROM in 0x900000 or higher?
            beq.b   .L1                             ; No, skip
            move.b  #boxPortable,D7                 ; Set BoxFlag to 4 (Portable)
.L1:
            swap    D7                              ; Move BoxFlag into high word
            rts
            IF PortableAbs
            org     $90060C
            ENDIF
SetUpTimeK:
            move    SR,-(SP)
            move.l  Lev1AutoVector,-(SP)
            movea.l #VIA_Base,A1
            bclr.b  #$5,(VIA_ACR-VIA_Base,A1)
            move.b  #$FF,(VIA_T2_H-VIA_Base,A1)
            move.b  #$A0,(VIA_IER-VIA_Base,A1)
            andi    #$F8FF,SR                       ; Enable interrupts
            move.l  SP,D1
            lea     (VIAIntForTimeDBRA),A0
            move.l  A0,Lev1AutoVector
            moveq   #-1,D0
            bra.b   .L3
.L1:
            move.b  #$F,(VIA_T2_L-VIA_Base,A1)
            move.b  #$3,(VIA_T2_H-VIA_Base,A1)
.L2:
            dbf     D0,.L2
            bra.b   VIAIntForTimeDBRA
.L3:
            bra.b   .L1
VIAIntForTimeDBRA:
            tst.b   (VIA_T2_L-VIA_Base,A1)
            not.w   D0
            move.w  D0,TimeDBRA
            movea.l D1,SP
            andi    #$F8FF,SR                       ; Enable interrupts
            lea     VIAIntForTimeSCCDB,A0
            move.l  A0,(Lev1AutoVector)
            movea.l #SCCRBase,A0
            moveq   #-1,D0
            bra.b   .L3
.L1:
            move.b  #$F,(VIA_T2_L-VIA_Base,A1)
            move.b  #$3,(VIA_T2_H-VIA_Base,A1)
.L2:
            btst.b  #0,(SCCR_bCtl-SCCRBase,A0)
            dbf     D0,.L2
            bra.b   VIAIntForTimeSCCDB
.L3:
            bra.b   .L1
VIAIntForTimeSCCDB:
            tst.b   (VIA_T2_L-VIA_Base,A1)
            not.w   D0
            move.w  D0,TimeSCCDB
            movea.l D1,SP
            andi    #$F8FF,SR
            lea     VIAIntForTimeSCSIDB,A0
            move.l  A0,Lev1AutoVector
            movea.l #SCSI_Base,A0
            moveq   #-1,D0
            bra.b   .L3
.L1:
            move.b  #$F,(VIA_T2_L-VIA_Base,A1)
            move.b  #$3,(VIA_T2_H-VIA_Base,A1)
.L2:
            btst.b  #0,(sCSRread-SCSI_Base,A0)
            dbf     D0,.L2
            bra.b   VIAIntForTimeSCSIDB
.L3:
            bra.b   .L1
VIAIntForTimeSCSIDB:
            tst.b   (VIA_T2_L-VIA_Base,A1)
            not.w   D0
            move.w  D0,TimeSCSIDB
            movea.l D1,SP
            andi    #$F8FF,SR
            lea     VIAIntForADBDelay,A0
            move.l  A0,Lev1AutoVector
            lea     (VIA_IER-VIA_Base,A1),A0
            moveq   #-1,D0
            bra.b   .L3
.L1:
            move.b  #$F,(VIA_T2_L-VIA_Base,A1)
            move.b  #$3,(VIA_T2_H-VIA_Base,A1)
.L2:
            btst.b  #0,(A0)
            dbf     D0,.L2
            bra.b   VIAIntForADBDelay
.L3:
            bra.b   .L1
VIAIntForADBDelay:
            tst.b   (VIA_T2_L-VIA_Base,A1)
            not.w   D0
            move.w  D0,ADBDelay
            movea.l D1,SP
            move.b  #$20,(VIA_IER-VIA_Base,A1)
            move.l  (SP)+,Lev1AutoVector
            move    (SP)+,SR
            rts
InitSCSIGlobals:
            move.l  #SCSIrd,SCSIBase
            move.l  #SCSI_Base+$200,SCSIDMA
            move.l  #SCSI_Base+$200,SCSIHsk
            rts
InitSCSI:
            lea     SCSIwr,A0
            clr.b   (sICRwrite-SCSIwr,A0)
            clr.b   (sMRwrite-SCSIwr,A0)
            clr.b   (sTCRwrite-SCSIwr,A0)
            clr.b   (sCSRwrite-SCSIwr,A0)
            rts
InitIWMGlobals:
            move.l  #DBase,IWM
            rts
InitIWM:
            movea.l #DBase,A0
            moveq   #IWMInitMode,D0
.L1:
            move.b  #$BE,(ph3L-DBase,A0)
            move.b  #$F8,(ph3L-DBase,A0)
            tst.b   (q7L-DBase,A0)
            tst.b   (mtrOff-DBase,A0)
            tst.b   (q6H-DBase,A0)
            move.b  (q7L-DBase,A0),D2
            btst.l  #5,D2
            bne.b   .L1
            and.b   D0,D2
            cmp.b   D0,D2
            beq.b   .Exit
            move.b  D0,(q7H-DBase,A0)
            tst.b   (q7L-DBase,A0)
            bra.b   .L1
.Exit:
            tst.b   (q6L-DBase,A0)
            rts
InitVIAGlobals:
            move.l  #VIA_Base,VIA
            rts
InitVIA:
            movea.l #VIA_Base,A0
            clr.b   (VIA_ORA-VIA_Base,A0)
            move.b  #$FF,(VIA_DDR_A-VIA_Base,A0)
            move.b  #$DF,(VIA_BufB-VIA_Base,A0)
            move.b  #$B9,(VIA_DDR_B-VIA_Base,A0)
            move.b  #$7F,(VIA_IER-VIA_Base,A0)
            rts
VIATimerEnables:
            movea.l #VIA_Base,A0
            move.b  #$22,(VIA_PCR-VIA_Base,A0)
            move.b  #$83,(VIA_IER-VIA_Base,A0)
            rts
InitSCCGlobals:
            move.l  #SCCWBase,SCCWr
            move.l  #SCCRBase,SCCRd
            clr.l   PollProc
            rts
Gary:
            dc.b    $9,$40,$4,$4C
            dc.b    $2,$0,$3,$C0
            dc.b    $F,$0,$0,$10
            dc.b    $0,$10,$1,$0
            dc.b    $9,$80,$4,$4C
            dc.b    $3,$C0,$F,$0
            dc.b    $0,$10,$0,$10
            dc.b    $1,$0
InitSCC:
            movea.l #SCCWBase,A0
            movea.l #SCCRBase,A1
            lea     Gary,A2
            moveq   #$10,D1
            bsr.b   WriteSCC
            addq.l  #2,A0
            addq.l  #2,A1
            moveq   #$E,D1
            bsr.b   WriteSCC
            rts
WriteSCC:
            move.b  (A1),D2
            bra.b   .L2
.L1:
            move.l  (SP),(SP)
            move.l  (SP),(SP)
            move.b  (A2)+,(A0)
.L2:
            dbf     D1,.L1
            rts
InitVidGlobals:
            move.l  #Video_Base,ScrnBase
            move.l  #hcVideoSize,ScreenBytes
            move.w  #VideoWidth,RowBits
            move.w  #VideoHeight,ColLines
            cmpi.b  #NTSCmode,NTSC
            bne.b   .L1
            move.w  #NTSCMaxX,RowBits
            addq.l  #NTSCOffset,ScrnBase
.L1:
            move.w  #80,ScreenRow
            move.w  #60,VertRRate
            move.l  #$480048,ScrVRes
            rts
InitCrsrVars:
            lea     GrafBegin,A0
            lea     GrafEnd,A1
.ZeroLoop:
            clr.w   (A0)+
            cmpa.l  A1,A0
            bcs.b   .ZeroLoop
            rts
InitCrsrMgr:
            move.l  #$F000F,D0
            lea     MTemp,A0
            move.l  D0,(A0)+
            move.l  D0,(A0)+
            move.l  D0,(A0)+
            clr.l   (A0)+
            move.w  ColLines,(A0)+
            move.w  RowBits,(A0)
            moveq   #TotalSize,D0                   ; Allocate space for mouse tracking pointer
            _NewPtrSys                              ; Put in system heap
            move.l  A0,MickeyBytes                  ; Save globals pointer
            move.w  #1,(ADBCount,A0)
            move.w  #8,(MaxCnt,A0)
            clr.w   (Error,A0)
            lea     MouseBytes,A1
            adda.w  #GSize,A0
            move.l  (A1)+,(A0)+
            move.l  (A1),(A0)
            lea     GrafEnd,A1
            BigLea  CrsrDevHandleVBL,A0
            move.l  A0,-(A1)
            move.w  #6,-(A1)
            BigLea  InitCrTable,A0
            lea     JHideCursor,A1
            moveq   #8-1,D1
            bsr.w   JmpTblInit
            moveq   #-1,D0
            move.w  D0,CrsrNew
            move.l  D0,MouseMask
            rts
MouseBytes  dc.b    4,10,15,255,255,83,77,72
InitIOMgr:
            moveq   #$40,D0
            move.w  D0,UnitNtryCnt
            asl.l   #2,D0
            _NewPtrSysClear
            move.l  A0,UTableBase
            suba.w  #$32,SP
            movea.l SP,A0
            clr.b   ($1B,A0)
            lea     DiskName,A1
            move.l  A1,($12,A0)
            _Open
            lea     EDiskName,A1
            move.l  A1,($12,A0)
            _Open
            lea     SndName,A1
            move.l  A1,($12,A0)
            _Open
            adda.w  #$32,SP
            subq.w  #4,SP
            move.l  #'SERD',-(SP)
            clr.w   -(SP)
            move.w  #$FFFF,(ROMMapInsert)
            _GetResource
            move.l  (SP)+,D0
            beq.b   .Exit
            movea.l D0,A0
            movea.l (A0),A0
            jsr     (A0)
.Exit:
            rts
DiskName:
            dc.b    SndName-*-1
            dc.b    '.Sony'
SndName:
            dc.b    EDiskName-*-2
            dc.b    '.Sound '
EDiskName:
            dc.b    InitMemMgr-*-2
            dc.b    '.EDisk',$0
InitMemMgr:
            move.l  #$FFFFFF,Lo3Bytes
            move.l  #$400,MinStack
            move.l  #$2000,DefltStack
            clr.w   MMDefFlags
            rts
InitRsrcMgr:
            st      SysMap
            clr.l   TopMapHndl
            clr.w   -(SP)
            _InitResources
            addq.w  #2,SP
            rts
InitDTQueue:
            lea     DtskQHdr_Flags,A1
            jsp     InitQueue
            lea     $90528A,A1
            move.l  A1,$D9C
            rts
GoofyDoEject:
.P1:        lea     (DoEject-.P1),A0
            jmp     (.P1,PC,A0.l)
TMVectors:
            dc.l    $2000                           ; Initial stack pointer
            dc.l    StartTest1                      ; Initial program counter
            dc.l    .BusError                       ; Bus error veector
            dc.l    .AdrError                       ; Address error vector
            dc.l    .IllError                       ; Illegal instruction error vector
            dc.l    .ZerError
            dc.l    .ChkError
            dc.l    .TrapV
            dc.l    .PrivError
            dc.l    .Trace
            dc.l    .LineA
            dc.l    .LineF
            dc.l    .Unassigned
            dc.l    .CPProtocol
            dc.l    .FormatX
            dc.l    .SpurInterrupt
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .SpurInterrupt
            dc.l    .IntLevel1
            dc.l    .IntLevel2
            dc.l    .IntLevel3
            dc.l    .IntLevel4
            dc.l    .IntLevel5
            dc.l    .IntLevel6
            dc.l    .IntLevel7
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .TrapInst
            dc.l    .FPCP1
            dc.l    .FPCP2
            dc.l    .FPCP3
            dc.l    .FPCP4
            dc.l    .FPCP5
            dc.l    .FPCP6
            dc.l    .FPCP7
            dc.l    .Unassigned
            dc.l    .PMMUConfig
            dc.l    .PMMUIllegal
            dc.l    .PMMUAccess
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
            dc.l    .Unassigned
.BusError:
            btst.l  #beok,D7                        ; Are bus errors expected?
            beq.b   .BusError2
            movea.l A5,SP
            jmp     (A6)
.BusError2:
            ori.w   #BECode,D7
            bra.w   ExceptionHandler
.AdrError:
            ori.w   #ADCode,D7
            bra.w   ExceptionHandler
.IllError:
            ori.w   #ILCode,D7
            bra.w   ExceptionHandler
.ZerError:
            ori.w   #ZDCode,D7
            bra.w   ExceptionHandler
.ChkError:
            ori.w   #CICode,D7
            bra.w   ExceptionHandler
.TrapV:
            ori.w   #TPCode,D7
            bra.w   ExceptionHandler
.PrivError:
            ori.w   #PVCode,D7
            bra.w   ExceptionHandler
.Trace:
            ori.w   #TECode,D7
            bra.w   ExceptionHandler
.LineA:
            ori.w   #ATCode,D7
            bra.w   ExceptionHandler
.LineF:
            ori.w   #FTCode,D7
            bra.w   ExceptionHandler
.Unassigned:
            ori.w   #UNCode,D7
            bra.w   ExceptionHandler
.CPProtocol:
            ori.w   #CPCode,D7
            bra.w   ExceptionHandler
.FormatX:
            ori.w   #FMCode,D7
            bra.w   ExceptionHandler
.SpurInterrupt:
            ori.w   #SICode,D7
            bra.w   ExceptionHandler
.TrapInst:
            ori.w   #TNCode,D7
            bra.b   ExceptionHandler
.IntLevel1:
            ori.w   #L1Code,D7
            bra.b   ExceptionHandler
.IntLevel2:
            ori.w   #L2Code,D7
            bra.b   ExceptionHandler
.IntLevel3:
            ori.w   #L3Code,D7
            bra.b   ExceptionHandler
.IntLevel4:
            ori.w   #L4Code,D7
            bset.l  #nmi,D7
            bra.b   ExceptionHandler
.IntLevel5:
            ori.w   #L5Code,D7
            bset.l  #nmi,D7
            bra.b   ExceptionHandler
.IntLevel6:
            ori.w   #L6Code,D7
            bset.l  #nmi,D7
            bra.b   ExceptionHandler
.IntLevel7:
            ori.w   #L7Code,D7
            bset.l  #nmi,D7
            bra.b   ExceptionHandler
.FPCP1:
            ori.w   #F1Code,D7
            bra.b   ExceptionHandler
.FPCP2:
            ori.w   #F2Code,D7
            bra.b   ExceptionHandler
.FPCP3:
            ori.w   #F3Code,D7
            bra.b   ExceptionHandler
.FPCP4:
            ori.w   #F4Code,D7
            bra.b   ExceptionHandler
.FPCP5:
            ori.w   #F5Code,D7
            bra.b   ExceptionHandler
.FPCP6:
            ori.w   #F6Code,D7
            bra.b   ExceptionHandler
.FPCP7:
            ori.w   #F7Code,D7
            bra.b   ExceptionHandler
.PMMUConfig:
            ori.w   #PCCode,D7
            bra.b   ExceptionHandler
.PMMUIllegal:
            ori.w   #PICode,D7
            bra.b   ExceptionHandler
.PMMUAccess:
            ori.w   #PACode,D7
ExceptionHandler:
            bset.l  #excp,D7                        ; Set exception flag
            move.l  SP,D6                           ; Save stack pointer
            movea.w #$2000,SP                       ; Initialize stack pointer
            jpp     Error1Handler                   ; Handle error
StartTest1:
            movea.w #$2000,SP                       ; Initialize stack pointer
            moveq   #0,D7                           ; Clear flags register
            moveq   #0,D6                           ; Clear minor error register
            lea     TMVectors,A0                    ; Get address of vectors
            suba.l  A1,A1                           ; Clear A1
            move.w  #64-1,D0                        ; Number of vectors
.VectorLoop:
            move.l  (A0)+,(A1)+
            dbf     D0,.VectorLoop
            move    #$2300,SR                       ; Allow SCC interrupts
            move.l  #TROMCode,D1
            move.l  DiagROM,D0
            cmp.l   D1,D0
            bne.b   VIASetup
            lea     VIASetup,A1
            movem.l DiagROM,D0/A0
            jmp     (A0)
VIASetup:
            bclr.l  #beok,D7
            moveq   #0,D1
            move.w  #ErrVIA1,D7
            lea     VIA_Base,A1
            bset.b  #PMreq,(VIA_BufB-VIA_Base,A1)
            move.b  #%10111001,(VIA_DDR_B-VIA_Base,A1)
            bclr.b  #SyncM,(VIA_BufB-VIA_Base,A1)
            lea     VIA_Base,A2
            btst.b  #TestJumper,(VIA_BufB-VIA_Base,A2)  ; Is the test "jumper" installed?
            bne.b   PowerManagerInit
            bset.l  #test,D7                            ; Jumper found, flag bit for later

PowerManagerInit:
            move.w  #ErrPmgrTurnOn,D7
            lea     .L1,A6
            move.w  #(powerCntl<<8|1<<0),D0
            move.l  #(0<<pTurnOn|1<<pModem)<<24,D1
            jpp     QuasiPwrMgr
.L1:
            move.w  A0,D6
            tst.l   D6
            bne.w   Error1Handler
            lea     .L2,A6
            move.w  #(powerCntl<<8|1<<0),D0
            move.l  #(1<<pTurnOn|1<<pMinus5V|1<<pSerDrvr|1<<pSCC|1<<pIWM)<<24,D1
            jpp     QuasiPwrMgr
.L2:
            move.w  A0,D6
            tst.l   D6
            bne.w   Error1Handler
            move.w  #ErrROM,D7
            BSR6    StartUpROMTest
            tst.l   D6
            bne.w   Error1Handler
            clr.w   D7
            move.w  #ErrPmgrSt,D7
            move.w  #(PmgrSelfTest<<8|0<<0),D0
            BSR6    QuasiPwrMgr
            move.w  A0,D6
            swap    D6
            move.w  D1,D6
            swap    D6
            tst.l   D6                              ; Any error?
            bne.w   Error1Handler                   ; Yes, handle error
            move.w  #ErrVidAddr,D7                  ; Load video address test error code $83
            BSR6    VramAddrTest
            tst.l   D6                              ; Any error?
            bne.w   Error1Handler                   ; Yes, handle error
            move.w  #ErrVidRAM,D7                   ; Load video RAM test error code $82
            BSR6    VramDataTest
            tst.l   D6
            bne.w   Error1Handler
            lea     Sound_Base,SP
            move.w  #(readINT<<8|0<<0),D0
            BSR6    QuasiPwrMgr
            btst.l  #oneSecIntFlag,D1
            beq.b   .L8
            clr.l   WarmStart
.L8:
            move.w  #ErrSCSI,D7
            lea     SCSI_Base,A2
            move.b  (sICRread-SCSI_Base,A2),D0
            move.w  #ErrIWM,D7
            lea     DBase,A2
            tst.b   (q7L-DBase,A2)
            move.b  (q6H-DBase,A2),D0
            move.w  #ErrData,D7
            suba.l  A0,A0
            BSR6    DataBusTest
            tst.l   D6
            bne.w   Error1Handler
            move.w  #ErrSizeMem,D7
            lea     .L10,A6
            move.l  #$22BBF0E1,D0
            jpp     SizeMemory
.L10:
            move.w  #(xPramRead<<8|2<<0),D0
            move.l  #$46010000,D1
            BSR6    QuasiPwrMgr
            cmpa.w  #0,A0
            bne.w   Error1Handler
            movea.l D6,A0
            move    A0,USP
            lsr.l   #8,D1
            suba.l  D1,A0
            cmpa.l  #$80000,A0
            blt.b   .L12
            move.l  A0,D6
.L12:
            movea.l D6,SP
            moveq   #0,D6
            lea     Sound_Base,A3
            moveq   #$28,D0
            bsr.w   BootBeep
            clr.b   (ascFifoInt-Sound_Base,A3)
            moveq   #0,D6
            lea     .L13,A6
            move.w  #ErrSCCReg,D7
            jpp     SccRegTest
.L13:
            lea     .L14,A6
            tst.l   D6
            bne.w   NCErrorHandler
.L14:
            lea     .L15,A6
            move.w  #ErrSCCTimer,D7
            jpp     SccTimerTest
.L15:
            lea     .L16,A6
            tst.l   D6
            bne.w   NCErrorHandler
.L16:
            lea     .L17,A6
            move.w  #ErrSCCLoop,D7
            jpp     SccLoopTest
.L17:
            lea     .L18,A6
            tst.l   D6
            bne.w   NCErrorHandler
.L18:
            lea     .L19,A6
            move.w  #ErrVIATest,D7
            jpp     ViaTest
.L19:
            lea     .L20,A6
            tst.l   D6
            bne.w   NCErrorHandler
.L20:
            lea     .L21,A6
            move.w  #ErrASCTest,D7
            jpp     TestASC
.L21:
            lea     .L22,A6
            tst.l   D6
            bne.w   NCErrorHandler
.L22:
            lea     .L23,A6
            move.w  #ErrPramTest,D7
            jpp     PRAMTest
.L23:
            lea     .L24,A6
            tst.l   D6
            bne.w   NCErrorHandler
.L24:
            move.w  #ErrRAMC,D7
            movea.l SP,A0
            subq.w  #8,A0
            BSR6    DataBusTest
            tst.l   D6
            bne.w   .L27
            move.w  #ErrAddr,D7
            movea.l SP,A0
            BSR6    AddrLineTest
            tst.l   D6
            bne.b   .L27
            clr.w   D7
.L27:
            lea     TMVectors,A0
            suba.l  A1,A1
            move.w  #64-1,D0
.L28:
            move.l  (A0)+,(A1)+
            dbf     D0,.L28
            tst.l   D6
            bne.w   Error1Handler
            btst.l  #test,D7
            bne.w   PostProc
            move.w  #(xPramRead<<8|2<<0),D0
            move.l  #$46010000,D1
            BSR6    QuasiPwrMgr
            move.w  A0,D6
            tst.l   D6
            bne.w   Error1Handler
            move    USP,A6
            movea.l A6,A5
            move.l  A6,D0
            lsr.l   #8,D1
            sub.l   D1,D0
            cmpi.l  #$80000,D0
            blt.b   .L30
            movea.l D0,A5
.L30:
            move.l  A5,D0
            lsr.l   #1,D0
            bclr.l  #0,D0                           ; Align stack pointer
            movea.l D0,SP
            jpp     StartInit1
TJump:
            dc.w    SizeMemory-TJump
            dc.w    DataBusTest-TJump
            dc.w    Mod3Test-TJump
            dc.w    AddrLineTest-TJump
            dc.w    RomTest-TJump
            dc.w    RevMod3Test-TJump
            dc.w    ExtRAMTest-TJump
            dc.w    NoTest-TJump
MaxTest:
            dc.w    8
NJump:
            dc.w    MapRamDataTest-NJump
            dc.w    MapRamUniqTest-NJump
            dc.w    VramDataTest-NJump
            dc.w    VramAddrTest-NJump
            dc.w    SccRegTest-NJump
            dc.w    SccLoopTest-NJump
            dc.w    SccTimerTest-NJump
            dc.w    ViaTest-NJump
            dc.w    TestSCSI-NJump
            dc.w    TestASC-NJump
            dc.w    PRAMTest-NJump
MaxNTst:
            dc.w    11
PostProc:
            tst.w   D7
            bne.b   .L1
            move.l  #$87654321,D6                   ; No, set good result code
.L1:
            bra.w   Error1Handler
RamTest:
            movem.l A6-A0/D7-D0,-(SP)
            move    SR,-(SP)
            moveq   #0,D6
            moveq   #0,D7
            move.w  #ErrRAMA,D7
            ori     #$300,SR
            BSR6    Mod3Test
            tst.l   D6
            bne.w   Error3Handler
            move    (SP)+,SR
            movem.l (SP)+,D0-D7/A0-A6
            rts
Error1Handler:
            lea     VIA_Base,A1
            bset.b  #PMreq,(VIA_BufB-VIA_Base,A1)
            move.b  #%10111001,(VIA_DDR_B-VIA_Base,A1)
            bclr.b  #SyncM,(VIA_BufB-VIA_Base,A1)
            lea     .L1,A6
            move.w  #(powerCntl<<8|1<<0),D0
            move.l  #(1<<pTurnOn|1<<pMinus5V|1<<pASC|1<<pSerDrvr|1<<pHD|1<<pSCC|1<<pIWM)<<24,D1
            jpp     QuasiPwrMgr
.L1:
            btst.l  #test,D7
            bne.b   .L3
            movea.l D7,A0
            move    A0,USP
            lea     Sound_Base,A3
            BSR6    ErrorBeep1
            move    USP,A0
            move.l  A0,D7
            jpp     CritErr
.L3:
            bset.l  #nosleep,D7
            bset.l  #MsgQ,D7
            bset.l  #timer,D7
            move.w  #sec,D4
            swap    D4
            BSR6    StartTimer
            bra.w   TMEntry1
Error3Handler:
            movea.l D7,A0
            move    A0,USP
            lea     Sound_Base,A3
            BSR6    ErrorBeep2
            move    USP,A0
            move.l  A0,D7
            jpp     CritErr
Error4Handler:
            movea.l D7,A0
            move    A0,USP
            lea     Sound_Base,A3
            BSR6    ErrorBeep3
            move    USP,A0
            move.l  A0,D7
            jpp     CritErr
NCErrorHandler:
            cmpi.l  #-1,D6
            bne.b   .L1
            moveq   #0,D6
            jmp     (A6)
.L1:
            movea.l A6,A4
            moveq   #$70,D1
            BSR6    RdXByte
            bset.l  #0,D1
            move.b  D1,D2
            moveq   #$70,D1
            BSR6    WrXByte
            moveq   #$78,D3
.L3:
            rol.l   #8,D6
            move.b  D6,D2
            move.b  D3,D1
            BSR6    WrXByte
            addq.w  #1,D3
            cmpi.w  #$7C,D3
            blt.b   .L3
.L5:
            rol.l   #8,D7
            move.b  D7,D2
            move.b  D3,D1
            BSR6    WrXByte
            addq.w  #1,D3
            cmpi.w  #$80,D3
            blt.b   .L5
            moveq   #0,D6
            jmp     (A4)
TestManager:
            movem.l A6-A0/D5-D0,-(SP)
            move    SR,-(SP)
            ori     #$300,SR
            tst.w   D0
            beq.b   .cmd0
            subq.w  #1,D0
            beq.w   .Exit
            subq.w  #1,D0
            beq.w   .cmd2
            subq.w  #1,D0
            beq.w   TMEntry0
            subq.w  #1,D0
            beq.b   .cmd4
            subq.w  #1,D0
            beq.b   .cmd5
            subq.w  #1,D0
            beq.w   .cmd6
            subq.w  #1,D0
            beq.w   .cmd7
            subq.w  #1,D0
            beq.w   TMcmd8
            bra.w   .Exit
.cmd0:
            moveq   #0,D6
            move.l  (A0),D7
            movea.l ($8,A0),A1
            movea.l ($4,A0),A0
.cmd4:
            move.w  D7,D1
            lsr.w   #8,D1
            cmp.w   MaxTest,D1
            bge.w   .Exit
            asl.w   #1,D1
            lea     TJump,A5
            move.w  (A5,D1),D1
            lea     (A5,D1),A5
.Loop:
            lea     .L1,A6
            jmp     (A5)
.L1:
            tst.l   D6
            beq.b   .noErr
            btst.l  #loop,D7
            beq.b   .TestStop
.TestLoop:
            lea     .TestLoop,A6
            jmp     (A5)
.TestStop:
            btst.l  #stop,D7
            bne.b   .StopTest
.noErr:
            subq.b  #1,D7
            bne.b   .Loop
.StopTest:
            btst.l  #pram,D7
            beq.b   .noStore
            BSR6    StoreResults
.noStore:
            btst.l  #boot,D7
            beq.w   .Exit
            lea     StBoot,A0
            jmp     (A0)
.cmd5:
            moveq   #0,D6
            move.l  (A0),D7
            movea.l (8,A0),A1
            movea.l (4,A0),A0
.cmd6:
            btst.l  #$8,D7
            beq.b   .L3
            moveq   #0,D1
            lea     .L2,A6
            bra.w   .L17
.L2:
            tst.l   D6
            bne.w   .L18
.L3:
            btst.l  #$9,D7
            beq.b   .L5
            moveq   #1,D1
            lea     .L4,A6
            bra.b   .L17
.L4:
            tst.l   D6
            bne.w   .L18
.L5:
            btst.l  #$A,D7
            beq.b   .L7
            moveq   #2,D1
            lea     .L6,A6
            bra.b   .L17
.L6:
            tst.l   D6
            bne.b   .L18
.L7:
            btst.l  #$B,D7
            beq.b   .L9
            moveq   #3,D1
            lea     .L8,A6
            bra.b   .L17
.L8:
            tst.l   D6
            bne.b   .L18
.L9:
            btst.l  #$C,D7
            beq.b   .L11
            moveq   #4,D1
            lea     .L10,A6
            bra.b   .L17
.L10:
            tst.l   D6
            bne.b   .L18
.L11:
            btst.l  #$D,D7
            beq.b   .L13
            moveq   #5,D1
            lea     .L12,A6
            bra.b   .L17
.L12:
            tst.l   D6
            bne.b   .L18
.L13:
            btst.l  #$E,D7
            beq.b   .L15
            moveq   #6,D1
            lea     .L14,A6
            bra.b   .L17
.L14:
            tst.l   D6
            bne.b   .L18
.L15:
            btst.l  #$F,D7
            beq.b   .L21
            moveq   #$7,D1
            lea     .L16,A6
            bra.b   .L17
.L16:
            tst.l   D6
            bne.b   .L18
            bra.b   .L21
.L17:
            asl.w   #$1,D1
            lea     TJump,A5
            move.w  (A5,D1),D1
            lea     (A5,D1),A5
            jmp     (A5)
.L18:
            btst.l  #$1D,D7
            beq.b   .L20
.L19:
            lea     .L19,A6
            jmp     (A5)
.L20:
            btst.l  #stop,D7
            bne.w   .StopTest
.L21:
            subq.b  #1,D7
            bne.w   .cmd6
            bclr.l  #$F,D7
            bne.w   .cmd6
            bra.w   .StopTest
.Exit:
            move    (SP)+,SR
.Exit2:
            movem.l (SP)+,D0-D5/A0-A6
            rts
.cmd2:
            move.w  (SP)+,D0
            bra.b   .Exit2
.cmd7:
            lea     Sound_Base,SP
            link.w  A4,#-$28
            lea     (-$28,A4),A1
            lea     Sound_Base,A2
            moveq   #0,D1
.L22:
            move.b  D1,(A1)+
            cmpa.l  A1,A2
            bhi.b   .L22
            lea     (-$12,A4),A1
            moveq   #$11,D1
.L23:
            move.b  (A0)+,(A1)+
            dbf     D1,.L23
            moveq   #0,D6
            move.l  (-$12,A4),D7
.L24:
            btst.l  #$A,D7
            beq.b   .L25
            move.w  #2,(-$26,A4)
            bsr.b   F34
            bne.b   .L28
.L25:
            btst.l  #$D,D7
            beq.b   .L26
            move.w  #5,(-$26,A4)
            bsr.b   F34
            bne.b   .L28
.L26:
            btst.l  #$E,D7
            beq.b   .L27
            move.w  #6,(-$26,A4)
            bsr.b   F34
            bne.b   .L28
.L27:
            bra.b   .L31
.L28:
            btst.l  #$1D,D7
            beq.b   .L30
.L29:
            bsr.b   F34
            bra.b   .L29
.L30:
            btst.l  #$1C,D7
            bne.b   .L32
.L31:
            subq.b  #1,D7
            bne.b   .L24
            bclr.l  #$F,D7
            bne.b   .L24
.L32:
            btst.l  #$1E,D7
            beq.b   .L33
            bsr.w   F35
.L33:
            lea     StBoot,A0
            jmp     (A0)
F34:
            moveq   #0,D6
            clr.b   (-$13,A4)
            move.w  (-$26,A4),D1
            suba.l  A0,A0
            movea.l #$10000,A1
            asl.w   #1,D1
            lea     TJump,A5
            move.w  (A5,D1),D1
            lea     (A5,D1),A5
            lea     .L2,A6
.L1:
            jmp     (A5)
.L2:
            adda.l  #$10000,A0
            adda.l  #$10000,A1
            addq.b  #1,(-$13,A4)
            tst.l   D6
            bne.b   .L4
.L3:
            cmpa.l  (-$A,A4),A1
            ble.b   .L1
            bra.b   .L6
.L4:
            move.l  D6,D2
            moveq   #0,D6
            addq.w  #1,(-$24,A4)
            move.b  (-$13,A4),D0
            subq.b  #1,D0
            move.w  (-$16,A4),D1
            cmpi.w  #9,D1
            bhi.b   .L5
            lea     (-$22,A4),A2
            move.b  D0,(A2,D1)
            addq.w  #1,D1
            ror.w   #8,D2
            move.b  D2,(A2,D1)
            addq.w  #1,D1
            ror.w   #8,D2
            move.b  D2,(A2,D1)
            addq.w  #1,D1
            move.w  D1,(-$16,A4)
.L5:
            bra.b   .L3
.L6:
            move.w  (-$24,A4),D6
            rts
F35:
            moveq   #100,D2
            moveq   #112,D1
            move.l  A4,-(SP)
            BSR6    WrXByte
            movea.l (SP)+,A4
            lea     (-$24,A4),A0
            lea     (-$17,A4),A1
            suba.l  A0,A1
            move.l  A1,D0
            moveq   #113,D1
            lea     .L3,A6
.L2:
            move.b  (A0)+,D2
            movem.l A6-A0/D7-D0,-(SP)
            jpp     WrXByte
.L3:
            movem.l (SP)+,D0-D7/A0-A6
            addq.b  #1,D1
            dbf     D0,.L2
            rts
TMcmd8:
            moveq   #0,D6
            move.l  (A0),D7
            movea.l ($8,A0),A1
            movea.l ($4,A0),A0
.L1:
            move.l  D7,D1
            andi.l  #$FF00,D1
            lsr.w   #8,D1
            subi.w  #128,D1
            cmp.w   MaxNTst,D1
            bge.b   .L6
            asl.w   #1,D1
            lea     NJump,A5
            move.w  (A5,D1),D1
            lea     (A5,D1),A5
            lea     .L2,A6
            jmp     (A5)
.L2:
            tst.l   D6
            bne.b   .L3
            bra.b   .L5
.L3:
            btst.l  #loop,D7
            beq.b   .L4
.L3_1:      
            lea     .L3_1,A6
            jmp     (A5)
.L4:
            btst.l  #stop,D7
            bne.b   .L6
.L5:
            subq.b  #1,D7
            bne.b   .L1
.L6:
            move    (SP)+,SR
            movem.l (SP)+,D0-D5/A0-A6
            rts
TMEntry0:
            moveq   #0,D6
            moveq   #0,D7
TMEntry1:
            move.w  #(powerCntl<<8|1<<0),D0
            move.l  #(1<<pTurnOn|1<<pMinus5V|1<<pASC|1<<pSerDrvr|1<<pHD|1<<pSCC|1<<pIWM)<<24,D1
            BSR6    QuasiPwrMgr
            lea     .checkForASC,A6
            move.w  #(powerCntl<<8|1<<0),D0
            move.l  #(0<<pTurnOn|1<<pModem)<<24,D1
            jpp     QuasiPwrMgr
.checkForASC:
            btst.l  #test,D7
            bne.b   .SkipSound
            movea.l D7,A3
            move    A3,USP
            lea     Sound_Base,A3
            BSR6    ErrorBeep4
            move    USP,A3
            move.l  A3,D7
.SkipSound:
            ori     #$300,SR
            moveq   #0,D5
            BSR6    SetupBases
getCmd:
            BSR6    GetChar
            tst.w   D5
            bpl.b   .gotChar
            btst.l  #nosleep,D7
            bne.w   TMRestart_Continue
            move.w  #(batteryRead<<8|0<<0),D0
            BSR6    QuasiPwrMgr
            btst.l  #excp,D1
            bne.w   TMRestart_Continue
            move.l  #$FFFF0800,D4
.L3:
            swap    D4
.L4:
            dbf     D4,.L4
            move.w  #(batteryRead<<8|0<<0),D0
            BSR6    QuasiPwrMgr
            btst.l  #excp,D1
            bne.w   TMRestart_Continue
            swap    D4
            dbf     D4,.L3
            move.w  #(sleepReq<<8|4<<0),D0
            move.l  #sleepSig,D1
            lea     TMRestart_Continue,A6
            jpp     QuasiPwrMgr
.gotChar:
            andi.b  #$7F,D5
            cmpi.b  #'*',D5
            bne.b   .checkCmd
            bset.l  #star,D7
            bra.w   TMRestart_Continue
.checkCmd:
            btst.l  #star,D7
            beq.w   DoTest_Invalid
            bclr.l  #star,D7
Service:
            cmpi.b  #'S',D5                         ; Is the command *S?
            bne.b   LoadAddr                        ; No, try the next command
            bclr.l  #MsgQ,D7
            bclr.l  #timer,D7
            bra.w   EchoCmd
; LoadAddr - (*L)
; 
; Set the load address register (A4)
LoadAddr:
            cmpi.b  #'L',D5                         ; Is the command *L?
            bne.b   ByteCnt                         ; No, try the next command
            moveq   #4,D2                           ; We're reading 4 bytes
            BSR6    GetNBytes                       ; Get the bytes from serial
            movea.l D1,A4                           ; Load the input into A4
            btst.l  #echo,D7
            beq.w   EchoCmd
            bclr.l  #crlf,D7
            move.l  D1,D0
            moveq   #4,D2
            lea     EchoCmd,A6
            jpp     PutNBytes
; ByteCnt - (*B)
; 
; Set byte count (D4)
ByteCnt:
            cmpi.b  #'B',D5
            bne.b   GetData
            moveq   #2,D2
            BSR6    GetNBytes
            move.w  D1,D4
            btst.l  #echo,D7
            beq.w   EchoCmd
            bclr.l  #crlf,D7
            move.l  D1,D0
            moveq   #2,D2
            lea     EchoCmd,A6
            jpp     PutNBytes
; GetData - (*D)
; 
; Get data from serial input, store the data, and generate a checksum.
; 
; Inputs:   A4      Start address
;           D4.hw   Number of bytes to load
; Output:   D6      Checksum
GetData:
            cmpi.b  #'D',D5
            bne.b   Checksum
            moveq   #0,D6
            subq.w  #1,D4
            bge.b   .GetData2
            clr.w   D4
.GetData2:
            move.w  #1,D2
            BSR6    GetNBytes
            move.b  D1,(A4)+
            moveq   #0,D0
            move.b  D1,D0
            add.l   D0,D6
            dbf     D4,.GetData2
            bra.w   EchoCmd
Checksum:
            cmpi.b  #'C',D5
            bne.b   Execute
            moveq   #0,D6
            subq.w  #1,D4
            bge.b   .Checksum1
            clr.w   D4
.Checksum1:
            move.b  (A4)+,D1
            moveq   #0,D0
            move.b  D1,D0
            add.l   D0,D6
            dbf     D4,.Checksum1
            bclr.l  #crlf,D7
            move.l  D6,D0
            moveq   #4,D2
            lea     EchoCmd,A6
            jpp     PutNBytes
Execute:
            cmpi.b  #'G',D5
            bne.b   LoadA0
            moveq   #4,D2
            BSR6    GetNBytes
            movea.l D1,A3
            move.b  #'*',D0
            BSR6    SendString
            move.b  #'G',D0
            BSR6    SendString
            bset.l  #crlf,D7
            moveq   #0,D2
            BSR6    PutNBytes
            bclr.l  #crlf,D7
            lea     EchoCmd,A6
            jmp     (A3)
LoadA0:
            cmpi.b  #'0',D5
            bne.b   LoadA1
            moveq   #4,D2
            BSR6    GetNBytes
            movea.l D1,A0
            btst.l  #echo,D7
            beq.w   EchoCmd
            bclr.l  #crlf,D7
            move.l  D1,D0
            moveq   #4,D2
            lea     EchoCmd,A6
            jpp     PutNBytes
LoadA1:
            cmpi.b  #'1',D5
            bne.b   SetCache
            moveq   #4,D2
            BSR6    GetNBytes
            movea.l D1,A1
            btst.l  #echo,D7
            beq.w   EchoCmd
            bclr.l  #crlf,D7
            move.l  D1,D0
            moveq   #4,D2
            lea     EchoCmd,A6
            jpp     PutNBytes
; SetCache - *2
;
; Set the cache control register (the Portable only has a 68HC000, no there is no CACR)
SetCache:
            cmpi.b  #'2',D5
            bne.b   MMUDis
            bra.w   EchoCmd                         ; No CACR, nothing to do.
; MMUDis - *3
;
; Disable MMU translation (no MMU on the Portable, so this test does nothing)
MMUDis:
            cmpi.b  #'3',D5
            bne.b   ClearResult
            bra.w   EchoCmd                         ; No MMU, nothing to do.
ClearResult:
            cmpi.b  #'4',D5
            bne.b   StartBootMsg
            clr.w   D7
            moveq   #0,D6
            bra.w   EchoCmd
StartBootMsg:
            cmpi.b  #'5',D5
            bne.b   CPUReset
            bset.l  #timer,D7
            bset.l  #MsgQ,D7
            move.w  #sec,D4
            swap    D4
            lea     EchoCmd,A6
            jpp     StartTimer
CPUReset:
            cmpi.b  #'6',D5
            bne.b   PreventSleep
            bra.w   EchoCmd
PreventSleep:
            cmpi.b  #'7',D5
            bne.b   cmdU
            bset.l  #nosleep,D7
            bra.w   EchoCmd
cmdU:
            cmpi.b  #'U',D5
            bne.b   cmdW
            bra.w   EchoCmd
cmdW:
            cmpi.b  #'W',D5
            bne.b   cmdQ
            bra.w   EchoCmd
cmdQ:
            cmpi.b  #'Q',D5
            bne.b   ASCIIMode
            bra.w   EchoCmd
ASCIIMode:
            cmpi.b  #'A',D5
            bne.b   HEXMode
            bset.l  #aski,D7
            bra.w   EchoCmd
HEXMode:
            cmpi.b  #'H',D5
            bne.b   SendResults                    ; No, go to the next command
            bclr.l  #aski,D7
            bra.w   EchoCmd
SendResults:
            cmpi.b  #'R',D5
            bne.b   MemDump                        ; No, go to the next command
            bclr.l  #crlf,D7
            move.l  D6,D0
            moveq   #4,D2
            BSR6    PutNBytes
            bclr.l  #crlf,D7
            move.w  D7,D0
            moveq   #2,D2
            lea     EchoCmd,A6
            jpp     PutNBytes
MemDump:
            cmpi.b  #'M',D5
            bne.b   EchoOn
            asr.w   #2,D4
            subq.w  #1,D4
            bge.b   .dump
            clr.w   D4
.dump:
            bset.l  #crlf,D7
.loop:
            move.l  (A4)+,D0
            lea     .MemDump4,A6
            moveq   #4,D2
            ; Idealy this would be a BSR6 but since the LEA instruction occurs before the MOVEQ
            ; in the original Portable ROM, this is not implemented right now.
            jpp     PutNBytes                       ; Output the bytes
.MemDump4:
            dbf     D4,.loop                        ; Loop until complete
            bra.w   EchoCmd
EchoOn:
            cmpi.b  #'E',D5
            bne.b   InitTestManager
            bset.l  #echo,D7
            bra.w   EchoCmd
InitTestManager:
            cmpi.b  #'I',D5
            bne.b   PwrMgrCmd
            jpp     StartTest1
PwrMgrCmd:
            cmpi.b  #'P',D5
            bne.b   DoCritTest
            movea.l D5,A3
            moveq   #2,D2
            BSR6    GetNBytes
            move.w  D1,D3
            tst.b   D3
            beq.b   .PwrMgrCmd4
            cmpi.b  #4,D3
            bls.b   .PwrMgrCmd3
            lea     EchoCmd,A6
            jpp     TMErrorSend
.PwrMgrCmd3:
            move.b  D3,D2
            andi.l  #$FF,D2
            BSR6    GetNBytes
.PwrMgrCmd4:
            move.w  D3,D0
            moveq   #4,D2
            sub.b   D0,D2
            asl.b   #3,D2
            asl.l   D2,D1
            BSR6    QuasiPwrMgr
            move.w  D0,D7
            move.l  D1,D6
            move.l  A3,D5
            bra.w   EchoCmd
DoCritTest:
            cmpi.b  #'T',D5
            bne.w   DoNonCritTest
            moveq   #4,D2
            BSR6    GetNBytes
            movea.l D1,A2
            move    A2,USP
            moveq   #2,D2
            BSR6    GetNBytes
            move.w  D1,D4
            move.w  #$1C,D2
            lsl.l   D2,D1
            or.l    D1,D7
            btst.l  #echo,D7
            beq.b   .DoCritTestLoop
            move    USP,A2
            move.l  A2,D0
            moveq   #4,D2
            BSR6    PutNBytes
            move.w  D4,D0
            moveq   #2,D2
            BSR6    PutNBytes
.DoCritTestLoop:
            moveq   #0,D6
            move    USP,A2
            move.l  A2,D1
            swap    D1
            move.b  #'?',D5
            cmp.w   MaxTest,D1
            bge.w   DoTest_Invalid
            asl.w   #1,D1
            lea     TJump,A5
            move.w  (A5,D1),D1
            lea     (A5,D1),A5
            lea     .DoCritTest4,A6
            jmp    (A5)
.DoCritTest4:
            tst.l   D6
            beq.b   .DoCritTestContinue
            BSR6    TMErrorSend
            btst.l  #loop,D7
            beq.b   .DoCritTestNoLoop
.DoCritTestErrLoop:
            lea     .DoCritTestErrLoop,A6
            jmp     (A5)
.DoCritTestNoLoop:
            btst.l  #stop,D7
            bne.b   .DoCritTestEcho
.DoCritTestContinue:
            move    USP,A2
            move.l  A2,D1
            subq.w  #1,D1
            movea.l D1,A2
            move    A2,USP
            tst.w   D1
            beq.b   .DoCritTestEcho
            BSR6    GetChar
            tst.w   D5
            bmi.b   .DoCritTestLoop
.DoCritTestEcho:
            move.l  #$540054,D5                     ; Fake a "T" to echo
            bra.w   EchoCmd
DoNonCritTest:
            cmpi.b  #'N',D5
            bne.w   DoTest_Invalid
            moveq   #4,D2
            BSR6    GetNBytes
            movea.l D1,A2
            move    A2,USP
            moveq   #2,D2
            BSR6    GetNBytes
            move.w  D1,D4
            move.w  #$1C,D2
            lsl.l   D2,D1
            or.l    D1,D7
            btst.l  #echo,D7
            beq.b   .DoNonCritTest4
            move    USP,A2
            move.l  A2,D0
            moveq   #4,D2
            BSR6    PutNBytes
            move.w  D4,D0
            moveq   #2,D2
            BSR6    PutNBytes
.DoNonCritTest4:
            moveq   #0,D6
            move    USP,A2
            move.l  A2,D1
            swap    D1
            move.b  #'?',D5
            subi.w  #$80,D1
            cmp.w   MaxNTst,D1
            bge.b   DoTest_Invalid
            asl.w   #1,D1
            lea     NJump,A5
            move.w  (A5,D1),D1
            lea     (A5,D1),A5
            lea     .DoNonCritTest5,A6
            jmp     (A5)
.DoNonCritTest5:
            move    USP,A2
            move.l  A2,D1
            swap    D1
            cmpi.w  #$84,D1
            beq.b   .DoNonCritTest6
            cmpi.w  #$85,D1
            beq.b   .DoNonCritTest6
            cmpi.w  #$86,D1
            bne.b   .DoNonCritTest7
.DoNonCritTest6:
            BSR6    SetupBases
.DoNonCritTest7:
            tst.l   D6
            beq.b   .DoNonCritTest11
            BSR6    TMErrorSend
            btst.l  #loop,D7
            beq.b   .DoNonCritTest10
.DoNonCritTest9:
            lea     .DoNonCritTest9,A6
            jmp     (A5)
.DoNonCritTest10:
            btst.l  #stop,D7
            bne.b   .DoNonCritTest13
.DoNonCritTest11:
            move    USP,A2
            move.l  A2,D1
            subq.w  #1,D1
            movea.l D1,A2
            move    A2,USP
            tst.w   D1
            beq.b   .DoNonCritTest13
            BSR6    GetChar
            tst.w   D5
            bmi.w   .DoNonCritTest4
.DoNonCritTest13:
            move.l  #$4E004E,D5
            bra.b   EchoCmd
DoTest_Invalid:
            clr.w   D0
            move.b  D5,D0
            lea     EchoCmd_End,A6
            jpp     SendString
EchoCmd:
            move.b  #'*',D0
            BSR6    SendString
            swap    D5
            move.b  D5,D0
            swap    D5
            BSR6    SendString
            bset.l  #crlf,D7
            moveq   #0,D2
            BSR6    PutNBytes
EchoCmd_End:
            bclr.l  #crlf,D7
TMRestart_Continue:
            btst.l  #MsgQ,D7                        ; Any messages queued?
            beq.b   .NoUnQ
            btst.l  #timer,D7
            beq.b   .NoTimer
            move.w  #sec,D0
            BSR6    TMRestart_SubVIA
            tst.w   D0
            bpl.b   .NoUnQ
.NoTimer:
            lea     .a1MsgLength,A1
            clr.w   D4
            move.b  (A1)+,D4
.L2:
            move.b  (A1)+,D0
            BSR6    SendString
            dbf     D4,.L2
            bclr.l  #crlf,D7
            bset.l  #aski,D7
            move.l  D6,D0
            moveq   #4,D2
            BSR6    PutNBytes
            move.w  D7,D0
            moveq   #2,D2
            BSR6    PutNBytes
            bclr.l  #aski,D7
            lea     .b1MsgLength,A1
            clr.w   D4
            move.b  (A1)+,D4
.L6:
            move.b  (A1)+,D0
            BSR6    SendString
            dbf     D4,.L6
            btst.l  #timer,D7
            bne.b   .NoUnQ
            bclr.l  #MsgQ,D7
.NoUnQ:
            bra.w   getCmd
.a1MsgLength:
            dc.b    6
.a1Msg:
            dc.b    '*APPLE*'
.b1MsgLength:
            dc.b    4
.b1Msg:
            dc.b    '*6*',$0D,$0A
            ; '*6*\r\n'
TMErrorSend:
            movea.l A6,A4
            lea     .ErrorMsgLength,A1
            clr.w   D1
            move.b  (A1)+,D1
.L1:
            move.b  (A1)+,D0
            BSR6    SendString
            dbf     D1,.L1
            jmp     (A4)
.ErrorMsgLength:
            dc.b    6
.ErrorMsg:
            dc.b    '*ERROR*'
GetChar:
            move.w  #$8000,D5
            btst.l  #SCCok,D7
            beq.b   .Exit
            lea     SCCRBase,A2
            btst.b  #RxCA,(SCCR_aCtl-SCCRBase,A2)
            beq.b   .Exit
            move.b  #1,D0
            lea     SCCWBase,A2
            move.b  D0,(SCCW_aCtl-SCCWBase,A2)
            lea     SCCRBase,A2
            move.b  (SCCR_aCtl-SCCRBase,A2),D0
            andi.b  #%1110000,D0
            beq.b   .L1
            lea     SCCWBase,A2
            move.b  #$30,(SCCW_aCtl-SCCWBase,A2)
.L1:
            bclr.l  #$F,D5
            lsl.l   #8,D0
            or.w    D0,D5
            lea     SCCRBase,A2
            move.b  (SCCR_aData-SCCRBase,A2),D5
            btst.l  #star,D7
            beq.b   .Exit
            move.b  D5,D0
            swap    D5
            move.b  D0,D5
            swap    D5
.Exit:
            jmp     (A6)
; CvtAscii
;
; Convert from hex ASCII to hex.
;
; Inputs:   D0.b    ASCII character
; Outputs:  D0.b    Converted nibble
CvtAscii:
            andi.w  #$7F,D0
            subi.b  #$30,D0
            bmi.b   .L2
            cmpi.b  #$16,D0
            bgt.b   .L2
            cmpi.b  #$9,D0
            ble.b   .L1
            subq.b  #$7,D0
.L1:
            jmp     (A6)
.L2:
            clr.w   D0
            jmp     (A6)
; GetNBytes
; 
; Get up to 4 bytes as input from the serial port.
;
; Inputs:   D2      Number of bytes to receive (max 4)
; Outputs:  D1      Received bytes
GetNBytes:
            movea.l A6,A5
            moveq   #0,D1
            btst.l  #aski,D7
            beq.b   .adjust
            asl.w   #1,D2
.adjust:
            subq.w  #1,D2
.L1:
            BSR6    GetChar
            tst.w   D5
            bmi.b   .L1
            move.b  D5,D0
            btst.l  #aski,D7
            beq.b   .L2
            BSR6    CvtAscii
            lsl.l   #4,D1
            or.b    D0,D1
            bra.b   .L3
.L2:
            lsl.l   #8,D1
            move.b  D0,D1
.L3:
            dbf     D2,.L1
            jmp     (A5)
PutNBytes:
            movea.l A6,A5
            move.l  D0,D3
            tst.w   D2
            beq.b   .L6
            move.w  D2,D1
            moveq   #4,D0
            sub.w   D1,D0
            mulu.w  #8,D0
            rol.l   D0,D3
            moveq   #0,D0
            btst.l  #aski,D7
            beq.b   .L1
            asl.w   #1,D2
.L1:
            subq.w  #1,D2
.L2:
            btst.l  #aski,D7
            bne.b   .L3
            rol.l   #8,D3
            move.b  D3,D0
            bra.b   .L5
.L3:
            rol.l   #4,D3
            move.b  D3,D0
            andi.b  #$F,D0
            cmpi.b  #$A,D0
            blt.b   .L4
            addq.b  #7,D0
.L4:
            addi.b  #$30,D0
.L5:
            BSR6    SendString
            dbf     D2,.L2
.L6:
            btst.l  #crlf,D7
            beq.b   .Exit
            move.b  #$D,D0
            BSR6    SendString
            move.b  #$A,D0
            BSR6    SendString
.Exit:
            jmp     (A5)
SendString:
            lea     SCCWBase,A2
            move.b  #$30,(SCCW_aCtl-SCCWBase,A2)
            move.b  D0,(SCCW_aData-SCCWBase,A2)
.L1:
            lea     SCCWBase,A2
            move.w  #1,D0
            move.b  D0,(SCCW_aCtl-SCCWBase,A2)
            lea     SCCRBase,A2
            btst.b  #0,(SCCR_aCtl-SCCRBase,A2)
            beq.b   .L1
            jmp     (A6)
SetupBases:
            lea     VIA_Base,A0
            btst.b  #TestJumper,(VIA_BufB-VIA_Base,A0)
            beq.b   .L1
            bset.b  #TestJumper,(VIA_DDR_B-VIA_Base,A0)
            bclr.b  #TestJumper,(VIA_BufB-VIA_Base,A0)
.L1:
            lea     SCCWBase,A0
            tst.b   (SCCW_bCtl-SCCWBase,A0)
            lea     PortSetUp,A1
.L2:        move.b  (A1)+,D0
            bmi.b   .Done
            move.b  D0,(SCCW_aCtl-SCCWBase,A0)
            move.b  (A1)+,(SCCW_aCtl-SCCWBase,A0)
            bra.b   .L2
.Done:
            bset.l  #SCCok,D7
            lea     SCCRBase,A0
            move.b  (SCCR_aData-SCCRBase,A0),D0
            lea     VIA_Base,A1
            move.b  #$3D,(VIA_DDR_B-VIA_Base,A1)
            bset.b  #SyncM,(VIA_BufB-VIA_Base,A1)
            jmp     (A6)
PortSetUp:
            dc.b    $9,$C0
            dc.b    $F,$0
            dc.b    $4,$4C
            dc.b    $B,$50
            dc.b    $E,$0
            dc.b    $C,$A
            dc.b    $D,$0
            dc.b    $E,$1
            dc.b    $A,$0
            dc.b    $3,$C1
            dc.b    $5,$EA
            dc.b    $1,$0
            dc.b    $FF,$FF
StartTimer:
            lea     VIA_Base,A2
            clr.b   (VIA_ACR-VIA_Base,A2)
            move.b  #$20,(VIA_IER-VIA_Base,A2)
            move.b  #$FF,(VIA_T2_L-VIA_Base,A2)
            move.b  #$FF,(VIA_T2_H-VIA_Base,A2)
            jmp     (A6)
TMRestart_SubVIA:
            bclr.l  #$F,D0
            lea     VIA_Base,A2
            btst.b  #5,(VIA_IFR-VIA_Base,A2)
            beq.b   .Exit
            move.b  #$20,(VIA_IER-VIA_Base,A2)
            move.b  #$FF,(VIA_T2_L-VIA_Base,A2)
            move.b  #$FF,(VIA_T2_H-VIA_Base,A2)
            swap    D4
            subq.w  #1,D4
            bpl.b   .L1
            move.w  D0,D4
            bset.l  #$F,D0
.L1:
            swap    D4
.Exit:
            jmp     (A6)
StoreResults:
            jmp     (A6)
Unknown_function_none:
            jmp     (A6)
WrXByte:
            movea.l A6,A3                           ; Save the return address
            move.w  #xPramWrite*$100+3,D0
            andi.w  #$7F,D1
            lsl.l   #8,D1
            move.b  #1,D1
            lsl.l   #8,D1
            move.b  D2,D1
            lsl.l   #8,D1
            BSR6    QuasiPwrMgr
            jmp     (A3)
RdXByte:
            movea.l A6,A3                           ; Save the return address
            move.w  #xPramRead*$100+2,D0
            andi.w  #$7F,D1
            lsl.l   #8,D1
            move.b  #1,D1
            lsl.l   #8,D1
            lsl.l   #8,D1
            BSR6    QuasiPwrMgr
            moveq   #$18,D0
            lsr.l   D0,D1
            jmp     (A3)
Unknown_function:
            movea.l A6,A5
            moveq   #$35,D1
            moveq   #$55,D2
            lea     VIA_Base,A0
            bclr.b  #TestJumper,(VIA_BufB-VIA_Base,A0)
            BSR6    Unknown_function_none
            move.w  D2,D1
            BSR6    Unknown_function_none
            lea     VIA_Base,A0
            bset.b  #TestJumper,(VIA_BufB-VIA_Base,A0)
            jmp     (A5)
; QuasiPwrMgr
; 
; Inputs:   D0.w    Command byte and number of bytes
;           D1.l    Up to 4 bytes to transfer or receive
; Outputs:  A0      0 if successful, error code otherwise
QuasiPwrMgr:
            movea.l A6,A5
            move    SR,D5
            swap    D5
            lea     VIA_Base,A1
            move.b  #$10,(VIA_IER-VIA_Base,A1)
            tst.w   Clock16M
            move.b  #$FF,(VIA_DDR_A-VIA_Base,A1)
            move.b  (VIA_ORA-VIA_Base,A1),D2
            swap    D2
            move.w  #1,D2
.L1:
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            swap    D5
            move    D5,SR
            swap    D5
            move.w  #1080,D5
.L2:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            beq.b   .L3
            cmpi.b  #$FF,(VIA_ORA-VIA_Base,A1)
            beq.b   .L4
.L3:
            dbf     D5,.L2                          ; Loop until timeout
            movea.w #$CD38,A0
            bra.w   .Exit
.L4:
            ori.w   #$300,SR
            rol.w   #8,D0
            move.b  D0,D5
            BSR6    QPM_Send
            beq.b   .L6
            ror.w   #8,D0
            dbf     D2,.L1
            bra.w   .Exit
.L6:
            clr.w   D2
            rol.w   #8,D0
            move.b  D0,D2
            move.b  D0,D5
            BSR6    QPM_Send
            bne.b   .Exit
            subq.w  #1,D2
            bmi.b   .L10
.L8:
            rol.l   #8,D1
            move.b  D1,D5
            BSR6    QPM_Send
            bne.b   .Exit
            dbf     D2,.L8
.L10:
            btst.l  #$B,D0
            beq.b   .L18
            move.l  #$2D690,D3
.L11:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            beq.b   .L12
            subq.l  #1,D3
            bne.b   .L11
            movea.w #$CD37,A0
            bra.b   .Exit
.L12:
            clr.w   D2
            BSR6    QPM_Receive
            bne.b   .Exit                           ; Exit if there was an error
            move.b  D5,D0
            BSR6    QPM_Receive
            bne.b   .Exit                           ; Exit if there was an error
            rol.w   #8,D0
            move.b  D5,D0
            cmpi.b  #4,D0
            bls.b   .L15
            move.b  #4,D0
.L15:
            move.b  D0,D2
            subq.b  #1,D2
            bmi.b   .L19
.L16:
            BSR6    QPM_Receive
            bne.b   .Exit                           ; Exit if there was an error
            rol.l   #8,D1
            move.b  D5,D1
            dbf     D2,.L16
.L18:
            moveq   #4,D2
            sub.b   D0,D2
            asl.b   #3,D2
            asl.l   D2,D1
.L19:
            suba.l  A0,A0
.Exit:
            move.b  #$FF,(VIA_DDR_A-VIA_Base,A1)
            swap    D2
            move.b  D2,(VIA_ORA-VIA_Base,A1)
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            move.b  #$90,(VIA_IER-VIA_Base,A1)
            swap    D5
            move    D5,SR
            cmpa.w  #0,A0
            jmp     (A5)                            ; Return to original caller
QPM_Send:
            suba.l  A0,A0                           ; Clear any error codes
            move.b  #$FF,(VIA_DDR_A-VIA_Base,A1)
            move.b  D5,(VIA_ORA-VIA_Base,A1)
            move.w  #460,D5                         ; Timeout loop counter
            bclr.b  #PMreq,(VIA_BufB-VIA_Base,A1)
.L22:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            beq.b   .L23
            dbf     D5,.L22                         ; Loop until timeout
            movea.w #$CD36,A0
            bra.b   QPM_DataEnd
.L23:
            move.w  #64,D5
            bset.b  #PMreq,(VIA_BufB-VIA_Base,A1)
.L24:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            bne.b   QPM_DataEnd
            dbf     D5,.L24                         ; Loop until timeout
            movea.w #$CD35,A0                       ; Set error: timeout send handshake finish
QPM_DataEnd:
            bset.b  #PMreq,(VIA_BufB-VIA_Base,A1)
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            cmpa.w  #0,A0                           ; If there was an error, set the zero register
            jmp     (A6)                            ; Return to main QuasiPwrMgr caller
QPM_Receive:
            suba.l  A0,A0                           ; Clear any error codes
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            move.w  #64,D5
.L27:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            beq.b   .L28
            dbf     D5,.L27
            movea.w #$CD34,A0
            bra.b   QPM_DataEnd
.L28:
            swap    D0
            bclr.b  #PMreq,(VIA_BufB-VIA_Base,A1)
            move.w  #64,D5
            move.b  (VIA_ORA-VIA_Base,A1),D0
.L29:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            bne.b   .L30
            dbf     D5,.L29
            movea.w #$CD33,A0
.L30:
            move.b  D0,D5
            swap    D0
            bra.b   QPM_DataEnd                     ; Finish up
STPWRMGR:
            movem.l A6-A0/D5-D2,-(SP)
            BSR6    QuasiPwrMgr
            movem.l (SP)+,D2-D5/A0-A6
            rts
DataBusTest:
            moveq   #1,D0
            moveq   #-2,D1
            move.w  #255,D2
.Loop:
            movem.l D0/D1,(A0)
            eor.l   D0,(A0)
            or.l    (A0),D6
            eor.l   D1,($4,A0)
            or.l    ($4,A0),D6
            rol.l   #1,D0
            rol.l   #1,D1
            dbf     D2,.Loop
            move.l  D6,D0
            swap    D0
            or.w    D0,D6
            andi.l   #$FFFF,D6
            jmp     (A6)                            ; Return
StartUpROMTest:
            moveq   #0,D0
            moveq   #0,D1
            lea     ROMChecksum,A0
            move.l  (A0)+,D4                        ; Load expected checksum
            move.l  #ROMSize/2-2,D3                 ; We're doing 
.Loop:
            move.w  (A0)+,D0                        ; Fetch a ROM word
            add.l   D0,D1                           ; Add to checksum
            subq.l  #1,D3                           ; Decrement by 1 (2 bytes)
            bne.b   .Loop                           ; Loop until done
            nop                                     ; Some nops for debug
            nop                                     ; (leave these here for emulator use)
            eor.l   D4,D1                           ; Result should be zero
            beq.b   .End
            move.w  #$FFFF,D6                       ; Set failed code in minor error register
.End:
            jmp     (A6)                            ; Return
; Mod3Test
; 
; Inputs:   A0      Start of memory to test
;           A1      End of memory to test
; Outputs:  D6      Failed bit mask
Mod3Test:
            movem.l .Mod3Pat,D0-D5
            movea.l A0,A2
            suba.w  #120,A1
            bra.b   .Fill120Start
.Fill120Loop:
            movem.l D0-D5,(A2)
            movem.l D0-D5,($18,A2)
            movem.l D0-D5,($30,A2)
            movem.l D0-D5,($48,A2)
            movem.l D0-D5,($60,A2)
            adda.w  #120,A2
.Fill120Start:
            cmpa.l  A1,A2
            ble.b   .Fill120Loop
            suba.w  #$FF94,A1
            moveq   #12,D5
            bra.b   .Fill12Start
.Fill12Loop:
            movem.l D0-D2,(A2)
            adda.w  D5,A2
.Fill12Start:
            cmpa.l  A1,A2
            ble.b   .Fill12Loop
            adda.w  D5,A1
            moveq   #4,D4
            cmpa.l  A2,A1
            beq.b   .FillDone
            move.l  D0,(A2)+
            moveq   #8,D4
            cmpa.l  A2,A1
            beq.b   .FillDone
            move.l  D1,(A2)+                        ; Write one more long
            moveq   #0,D4                           ; Case 2, offset 0
.FillDone:
            movea.l A0,A2                           ; Copy starting address
            move.l  A1,D3
            sub.l   A0,D3
            subq.l  #4,D3
            moveq   #$3F,D2
            and.w   D3,D2
            neg.w   D2                              ; Index backwards
            lsr.l   #6,D3                           ; 64 bytes per loop
            eor.l   D1,(A2)                         ; Write starting pattern
            addi.l  #$FFFFFFFF,D5                   ; Help any floating bits float up
            jmp     (.L2,PC,D2.w)                   ; Jump to the correct starting position
.L1:
            move.l  (A2)+,D2                        ; Read a pattern
            eor.l   D2,(A2)                         ; Generate next pattern
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
            move.l  (A2)+,D2
            eor.l   D2,(A2)
.L2:
            dbf     D3,.L1
            subi.l  #$10000,D3
            bpl.b   .L1
            cmp.l   D0,D1
            beq.b   .L3
            move.l  D0,D1
            bra.b   .FillDone
.L3:
            movem.l (-$C,A1),D0-D2
            movem.l (.Exit,PC,D4.w),D3-D5
            eor.l   D3,D0
            eor.l   D4,D1
            eor.l   D5,D2
            or.l    D2,D0
            or.l    D1,D0
            or.l    D0,D6
            swap    D0
            or.w    D0,D6
            andi.l  #$FFFF,D6
.Exit:
            jmp     (A6)
.Mod3Pat:
            dc.l    $6DB6DB6D
            dc.l    $B6DB6DB6
            dc.l    $DB6DB6DB
            dc.l    $6DB6DB6D
            dc.l    $B6DB6DB6
            dc.l    $DB6DB6DB
; RevMod3Test
; 
; Inputs:   A0      Start of memory to test
;           A1      End of memory to test
; Outputs:  D6      Failed bit mask
RevMod3Test:
            movem.l .RevMod3Pat,D0-D5
            movea.l A1,A2
            adda.w  #$78,A0
            bra.b   .Fill120Start
.Fill120Loop:
            movem.l D5-D0,-(A2)
            movem.l D5-D0,-(A2)
            movem.l D5-D0,-(A2)
            movem.l D5-D0,-(A2)
            movem.l D5-D0,-(A2)
.Fill120Start:
            cmpa.l  A2,A0
            ble.b   .Fill120Loop
            adda.w  #$FF94,A0
            bra.b   .Fill12Start
.Fill12Loop:
            movem.l D2-D0,-(A2)
.Fill12Start:
            cmpa.l  A2,A0
            ble.b   .Fill12Loop
            suba.w  #12,A0
            moveq   #8,D4
            cmpa.l  A2,A0
            beq.b   .FillDone
            move.l  D2,-(A2)
            moveq   #4,D4
            cmpa.l  A2,A0
            beq.b   .FillDone
            move.l  D1,-(A2)
            moveq   #0,D4
.FillDone:
            movea.l A1,A2
            subq.l  #4,A2
            move.l  A2,D3
            sub.l   A0,D3
            moveq   #$3F,D0
            and.w   D3,D0
            neg.w   D0
            lsr.l   #6,D3
            eor.l   D1,(A2)
            addi.l  #-1,D5
            jmp     (.L2,PC,D0.w)
.L1:
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
            move.l  (A2),D0
            eor.l   D0,-(A2)
.L2:
            dbf     D3,.L1
            subi.l  #$10000,D3
            bpl.b   .L1
            cmp.l   D2,D1
            beq.b   .L3
            move.l  D2,D1
            bra.b   .FillDone
.L3:
            movem.l (A0),D0-D2
            movem.l (.Exit,PC,D4.w),D3-D5
            eor.l   D3,D0
            eor.l   D4,D1
            eor.l   D5,D2
            or.l    D2,D0
            or.l    D1,D0
            or.l    D0,D6
            swap    D0
            or.w    D0,D6
            andi.l  #$FFFF,D6
.Exit:
            jmp     (A6)
.RevMod3Pat:
            dc.l    $6DB6DB6D
            dc.l    $B6DB6DB6
            dc.l    $DB6DB6DB
            dc.l    $6DB6DB6D
            dc.l    $B6DB6DB6
            dc.l    $DB6DB6DB
; RomTest
; 
; Run a ROM test on individual ROMs
;
; Outputs:  D6      Failed bit mask
RomTest:
            lea     myROMSums.l,A1
            lea     BaseOfROM,A0
            move.l  (A0)+,D0
            movea.l A1,A2
            moveq   #0,D0
            moveq   #0,D1
            moveq   #0,D2
            moveq   #0,D3
            moveq   #0,D4
            moveq   #0,D5
.L1:
            move.w  (A0)+,D5
            move.b  D5,D4
            add.l   D4,D1
            lsr.l   #8,D5
            move.b  D5,D4
            add.l   D4,D0
            cmpa.l  A0,A1
            bgt.b   .L1
            adda.w  #$10,A0
            lea     (BaseOfROM+ROMSize),A1
.L2:
            move.w  (A0)+,D5
            move.b  D5,D4
            add.l   D4,D1
            lsr.l   #8,D5
            move.b  D5,D4
            add.l   D4,D0
            cmpa.l  A0,A1
            bgt.b   .L2
            move.l  (A2)+,D4
            eor.l   D4,D0
            beq.b   .L3
            bset.l  #0,D6
.L3:
            move.l  (A2)+,D4
            eor.l   D4,D1
            beq.b   .L4
            bset.l  #1,D6
.L4:
            move.l  (A2)+,D4
            eor.l   D4,D2
            beq.b   .L5
            bset.l  #2,D6
.L5:
            move.l  (A2)+,D4
            eor.l   D4,D3
            beq.b   .L6
            bset.l  #3,D6
.L6:
            jmp     (A6)                            ; Return
; ExtRAMTest
; 
; Test RAM using a march pattern
;
; Inputs:   A0      Start of memory to test
;           A1      End of memory to test
; Outputs:  D6      Failed bit mask
ExtRAMTest:
            moveq   #0,D0
            moveq   #-1,D1
            movea.l A0,A2
            move.l  A1,D2
            sub.l   A0,D2
            lsr.l   #2,D2
            move.l  D2,D3
.L1:
            move.l  D0,(A2)+
            subq.l  #1,D2
            bne.b   .L1
            move.l  D3,D2
            movea.l A0,A2
.L2:
            tst.l   (A2)
            bne.b   .L7
            eor.l   D1,(A2)+
            subq.l  #1,D2
            bne.b   .L2
            move.l  D3,D2
.L3:
            eor.l   D1,-(A2)
            bne.b   .L7
            subq.l  #1,D2
            bne.b   .L3
            move.l  D3,D2
.L4:
            tst.l   (A2)
            bne.b   .L7
            eor.l   D1,(A2)+
            subq.l  #1,D2
            bne.b   .L4
            move.l  D3,D2
.L5:
            eor.l   D1,-(A2)
            bne.b   .L7
            subq.l  #1,D2
            bne.b   .L5
            move.l  D3,D2
.L6:
            tst.l   (A2)+
            bne.b   .L7
            subq.l  #1,D2
            bne.b   .L6
            jmp     (A6)
.L7:
            or.l    (A2),D6
            move.l  D6,D0
            swap    D0
            or.w    D0,D6
            andi.l  #$FFFF,D6                       ; Only return one word
            jmp     (A6)                            ; Return
; AddrLineTest
;
; 
AddrLineTest:
            move.l  A0,D0
            lea     $80000,A2
            cmpi.l  #$100000,D0
            ble.b   .L1
            lea     $200000,A2
.L1:
            cmpa.l  A2,A0
            bgt.b   .L2
            move.l  A2,D0
            lsr.l   #1,D0
            movea.l D0,A2
.L2:
            clr.l   ResetStackPtr
            moveq   #4,D5
.L3:
            movea.l D5,A3
            move.l  D5,(A3)
            lsl.l   #1,D5
            cmp.l   A2,D5
            ble.b   .L3
            suba.l  A3,A3
            move.l  (A3),D0
            bne.b   .Exit
            moveq   #4,D5
.L4:
            movea.l D5,A3
            move.l  (A3),D0
            eor.l   D5,D0
            bne.b   .Exit
            lsl.l   #1,D5
            cmp.l   A2,D5
            ble.b   .L4
            move.l  A2,D5
            lsl.l   #1,D5
.L5:
            lsr.l   #1,D5
            cmpi.l  #4,D5
            blt.b   .L6
            movea.l D5,A3
            move.l  D5,(A3)
            bra.b   .L5
.L6:
            suba.l  A3,A3
            move.l  (A3),D0
            bne.b   .Exit
            moveq   #4,D5
.L7:
            movea.l D5,A3
            move.l  (A3),D0
            eor.l   D5,D0
            bne.b   .Exit
            lsl.l   #1,D5
            cmp.l   A2,D5
            ble.b   .L7
            moveq   #0,D0
.Exit:
            move.l  D0,D6
            jmp     (A6)
NoTest:
            moveq   #0,D6
            jmp     (A6)
VramAddrTest:
            moveq   #0,D0
            lea     Video_Base,A0
            move.w  #$4000,D0
            moveq   #$1E,D1
            move.w  #$E0,D2
.wloop:
            move.b  D1,(A0,D0.w)
            eori.w  #$7FFF,D0
            move.b  D2,(A0,D0.w)
            subq.b  #1,D1
            ror.w   #4,D2
            subq.b  #1,D2
            rol.w   #4,D2
            eori.w  #$7FFF,D0
            lsr.w   #1,D0
            bne.b   .wloop
            move.w  #$4000,D0
            moveq   #$1E,D1
            move.w  #$E0,D2
.rdloop:
            move.b  (A0,D0.w),D3
            cmp.b   D1,D3
            bne.b   .failones
            eori.w  #$7FFF,D0
            move.b  (A0,D0.w),D3
            cmp.b   D2,D3
            bne.b   .failzeroes
            subq.b  #1,D1
            ror.w   #4,D2
            subq.b  #1,D2
            rol.w   #4,D2
            eori.w  #$7FFF,D0
            lsr.w   #1,D0
            bne.b   .rdloop
            bra.b   .done
.failones:
            move.w  D0,D6
            moveq   #$10,D2
            asl.l   D2,D6
            move.b  D1,D6
            asl.w   #8,D6
            move.b  D3,D6
            bra.b   .done
.failzeroes:
            move.w  D0,D6
            moveq   #$10,D1
            asl.l   D1,D6
            move.b  D2,D6
            asl.w   #8,D6
            move.b  D3,D6
.done:
            jmp     (A6)
VramDataTest:
            movea.l A6,A3
            lea     Video_Base,A0
            lea     Sound_Base,A1
            BSR6    Mod3Test
            tst.l   D6
            bne.b   .vidramfail
            BSR6    RevMod3Test
.vidramfail:
            move.l  D6,D0
            asr.w   #8,D0
            or.b    D0,D6
            movea.l A3,A6
            jmp     (A6)
; MapRamDataTest
;
; Write and verify patterns in the Portable's mapper registers.
MapRamDataTest:
            lea     MapperBase,A0
            lea     MapTestTable,A1
            move.w  MapTestTableSize,D0
.mwlp2:
            moveq   #1,D1
            move.b  (A1,D0.w),D2
.mwlp1:
            move.b  D2,(A0,D1.w)
            addq.l  #2,D1
            cmpi.b  #$1F,D1
            ble.b   .mwlp1
            moveq   #1,D1
.mrdlp1:
            move.b  (A0,D1.w),D3
            andi.b  #%111,D3
            cmp.b   D2,D3
            bne.b   .failmapram
            addq.l  #2,D1
            cmpi.b  #$1F,D1
            ble.b   .mrdlp1
            dbf     D0,.mwlp2
            bra.b   .Exit
.failmapram:
            move.b  #$10,D0
            move.b  D1,D6
            asl.l   D0,D6
            move.b  D2,D6
            asl.w   #8,D6
            move.b  D3,D6
.Exit:
            jmp     (A6)                            ; Return
MapRamUniqTest:
            lea     MapperBase,A0
            lea     MapTestTable2,A1
            movea.l A1,A2
            moveq   #1,D1
.wlpf:
            move.b  (A1)+,(A0,D1.w)
            addq.l  #2,D1
            cmpi.b  #$1F,D1
            ble.b   .wlpf
            lea     .L1,A3
            bra.b   .rdfwd1
.L1:
            tst.l   D6
            bne.b   .Exit
            lea     Restore,A1
            moveq   #$1F,D1
.L2:
            move.b  -(A1),(A0,D1.w)
            subq.l  #2,D1
            bpl.b   .L2
            lea     .Exit,A3
            bra.b   .rdfwd1
.Exit:
            jmp     (A6)
.rdfwd1:
            moveq   #1,D1
            movea.l A2,A1
.rdfwd2:
            move.b  (A0,D1.w),D3
            andi.b  #7,D3
            cmp.b   (A1)+,D3
            bne.b   .failrd
            addq.l  #$2,D1
            cmpi.b  #$1F,D1
            ble.b   .rdfwd2
            bra.b   .passrd
.failrd:
            move.b  #$10,D0
            move.b  D1,D6
            asl.l   D0,D6
            move.b  -(A1),D6
            asl.w   #8,D6
            move.b  D3,D6
.passrd:
            jmp     (A3)
MapTestTable:
            dc.b    %111
            dc.b    %000
            dc.b    %001
            dc.b    %010
            dc.b    %100
            dc.b    %110
            dc.b    %101
            dc.b    %011
MapTestTableSize:
            dc.w    *-MapTestTable-1
MapTestTable2:
            dc.b    0
            dc.b    1
            dc.b    2
            dc.b    3
            dc.b    4
            dc.b    5
            dc.b    6
            dc.b    7
            dc.b    7
            dc.b    6
            dc.b    5
            dc.b    4
            dc.b    3
            dc.b    2
            dc.b    1
            dc.b    0
Restore:
            movea.l #VIA_Base,A0
            bset.b  #SyncM,(VIA_Base-VIA_Base,A0)
            bset.b  #SyncM,(VIA_DDR_B-VIA_Base,A0)
            movea.l #SCCWBase,A0
            movea.l #SCCRBase,A1
            addq.w  #2,A0
            addq.w  #2,A1
            lea     ResetTbl,A2
            bsr.b   writescc2
            rts
; Code is unreachable?
            bsr.b   Restore
            lea     DefaultTbl,A2
            bsr.b   writescc2
            subq.w  #2,A0
            subq.w  #2,A1
            lea     DefaultTbl,A2
            bsr.b   writescc2
            rts
writescc2:
            move.w  (A2)+,D0
            move.b  (A1),D2
.loop:
            _SCCDelay
            move.b  (A2)+,(A0)
            dbf     D0,.loop
            rts        
scclp:
            move.w  $1A00,D0
.L1:
            dbf     D0,.L1
            tst.b   ($4,A1)
            move.w  #$FE,D1
.L2:
            moveq   #-1,D3
            moveq   #0,D6
            move.b  #$30,(A0)
            move.b  (SP),(SP)                       ; Delay (byte move instead of the usual word for some reason)
.L3:
            btst.b  #2,(A1)
            bne.b   .L4
            dbf     D3,.L3
            moveq   #1,D6
            bra.b   .Exit
.L4:
            move.b  D1,($4,A0)
            moveq   #-1,D3
.L5:
            move.b  #1,(A0)
            _SCCDelay
            btst.b  #0,(A1)
            bne.b   .L6
            dbf     D3,.L5
            moveq   #2,D6
            bra.b   .Exit
.L6:
            move.w  #-1,D3
.L7:
            btst.b  #0,(A1)
            bne.b   .L8
            dbf     D3,.L7
            moveq   #3,D6
            bra.b   .Exit
.L8:
            move.b  ($4,A1),D2
            cmp.b   D1,D2
            beq.b   .L9
            moveq   #4,D6
            bra.b   .Exit
.L9:
            move.b  #1,(A0)
            _SCCDelay
            move.b  (A1),D2
            andi.b  #$70,D2
            beq.b   .L10
            moveq   #5,D6
            bra.b   .Exit
.L10:
            dbf     D1,.L2
.Exit:
            rts
SccLoopTest:
            move    SR,-(SP)
            move    #$2700,SR
            bsr.w   Restore
            lea     MainTbl,A2
            bsr.w   writescc2
            lea     LoopTbl,A2
            bsr.w   writescc2
            bsr.w   scclp
            bne.b   .Exit
            bsr.w   Restore
            subq.w  #2,A1
            subq.w  #2,A0
            lea     MainTbl,A2
            bsr.w   writescc2
            lea     LoopTbl,A2
            bsr.w   writescc2
            bsr.w   scclp
            beq.b   .Exit
            addi.w  #$10,D6
.Exit:
            bsr.w   Restore
            move    (SP)+,SR
            jmp     (A6)
regLoop:
            moveq   #0,D6
            move.w  #$FF,D1
            clr.w   D4
            move.b  (A1),D0
            _SCCDelay
            move.b  D2,(A0)
            _SCCDelay
            move.b  (A1),D5
            _SCCDelay
.L1:
            move.b  D2,(A0)
            _SCCDelay
            and.b   D3,D4
            move.b  D4,(A0)
            _SCCDelay
            move.b  D2,(A0)
            _SCCDelay
            move.b  (A1),D0
            and.b   D3,D0
            cmp.b   D4,D0
            bne.b   .L2
            addq.w  #1,D4
            dbf     D1,.L1
            bra.b   .L3
.L2:
            moveq   #1,D6
.L3:
            move.b  D2,(A0)
            _SCCDelay
            move.b  D5,(A0)
            tst.w   D6
            rts
SccRegTest:
            bsr.w   Restore
            lea     regtests,A2
.L1:
            move.b  (A2),D2
            bmi.b   .L2
            move.b  ($1,A2),D3
            addq.w  #2,A2
            bsr.b   regLoop
            bne.b   .Exit
            bra.b   .L1
.L2:
            lea     bregtests,A2
            subq.w  #2,A0
            subq.w  #2,A1
.L3:
            move.b  (A2),D2
            bpl.b   .L4
            bra.b   .Exit
.L4:
            move.b  ($1,A2),D3
            addq.w  #2,A2
            bsr.b   regLoop
            bne.b   .Exit
            bra.b   .L3
.Exit:
            bsr.w   Restore
            jmp     (A6)
SccTimerTest:
            move    SR,-(SP)
            move    #$2700,SR
            movea.w #Lev2AutoVector,A3
            move.l  (A3),-(SP)                      ; Save the interrupt address
            lea     .L5,A0
            move.l  A0,(A3)
            bsr.w   Restore
            lea     TimerTbl,A2
            bsr.w   writescc2
            moveq   #1,D6
            moveq   #-1,D1
            move.l  #$50000,D0
            move    #$2100,SR
.L1:
            tst.w   D1
            beq.b   .L2
            subq.l  #1,D0
            bne.b   .L1
            bra.b   .L4
.L2:
            tst.w   D1
            bne.b   .L3
            subq.l  #1,D0
            bne.b   .L2
            moveq   #2,D6
            bra.b   .L4
.L3:
            move    #$2700,SR
            sub.l   D2,D3
            moveq   #3,D6
            cmpi.l  #$10000,D3
            bgt.b   .L4
            moveq   #4,D6
            cmpi.l  #$100,D3
            blt.b   .L4
            moveq   #0,D6
.L4:
            bsr.w   Restore
            move    #$2700,SR
            movea.w #Lev2AutoVector,A3
            move.l  (SP)+,(A3)                      ; Restore the interrupt address
            move    (SP)+,SR
            jmp     (A6)
.L5:
            movea.l #SCCWBase,A0
            move.b  #$10,(SCCW_aCtl-SCCWBase,A0)
            move.b  #$10,(SCCW_bCtl-SCCWBase,A0)
            move.l  D0,D2
            tst.w   D1
            bpl.b   .L6
            move.l  D2,D3
.L6:
            addq.w  #1,D1
            move.l  A0,-(SP)
            lea     VIA_Base,A0
            tst.b   (VIA_IER-VIA_Base,A0)
            tst.b   (VIA_IFR-VIA_Base,A0)
            movea.l (SP)+,A0
            rte
MainTbl:
            dc.w    $11
            dc.b    $9
            dc.b    $0
            dc.b    $4
            dc.b    $4C
            dc.b    $B
            dc.b    $50
            dc.b    $F
            dc.b    $0
            dc.b    $C
            dc.b    $A
            dc.b    $D
            dc.b    $0
            dc.b    $E
            dc.b    $1
            dc.b    $3
            dc.b    $C1
            dc.b    $5
            dc.b    $6A
ResetTbl:
            dc.w    $1
            dc.b    $9
            dc.b    $C0
DefaultTbl:
            dc.w    $7
            dc.b    $F
            dc.b    $8
            dc.b    $1
            dc.b    $1
            dc.b    $0
            dc.b    $10
            dc.b    $9
            dc.b    $8    
LoopTbl:
            dc.w    $3
            dc.b    $E
            dc.b    $11
            dc.b    $5
            dc.b    $68
TimerTbl:
            dc.w    $D
            dc.b    $9
            dc.b    $A
            dc.b    12
            dc.b    $FF
            dc.b    13
            dc.b    $FF
            dc.b    14
            dc.b    $3
            dc.b    15
            dc.b    $2
            dc.b    $0
            dc.b    $10
            dc.b    $1
            dc.b    $1
regtests:
            dc.b    $2
            dc.b    $FF
            dc.b    12
            dc.b    $FF
            dc.b    13
            dc.b    $FF
            dc.b    $F
            dc.b    $FA
            dc.b    $FF
            dc.b    $FF
bregtests:
            dc.b    $2
            dc.b    $0
            dc.b    $C
            dc.b    $FF
            dc.b    $D
            dc.b    $FF
            dc.b    $F
            dc.b    $FA
            dc.b    $FF
            dc.b    $FF
ViaTest:
            movea.w #Lev1AutoVector,A2
            move.l  (A2),-(SP)
            lea     LocalVIA1Int,A0
            move.l  A0,(A2)
            moveq   #0,D6
            movea.l #VIA_Base,A0
            andi.b  #$1F,(VIA_ACR-VIA_Base,A0)
            ori.b   #$40,(VIA_ACR-VIA_Base,A0)
            move.b  #$62,(VIA_IFR-VIA_Base,A0)
            move.b  #$E2,(VIA_IER-VIA_Base,A0)
            moveq   #0,D3
            moveq   #0,D4
            moveq   #0,D5
            move.b  #$FF,(VIA_T1C_L-VIA_Base,A0)
            move.b  #$FF,(VIA_T2_L-VIA_Base,A0)
            move.b  #$2,(VIA_T1C_H-VIA_Base,A0)
            move.b  #$3F,(VIA_T2_H-VIA_Base,A0)
            move.l  #$F0000,D0
            move    #$2000,SR
.L1:
            cmpi.w  #$A,D3
            beq.b   .L2
            subq.l  #1,D0
            bne.b   .L1
.L2:
            move.w  #$2700,SR
            moveq   #1,D6
            tst.w   D3
            beq.w   .Exit
            moveq   #2,D6
            cmpi.w  #$A,D3
            bne.w   .Exit
            moveq   #3,D6
            tst.w   D4
            beq.w   .Exit
            moveq   #4,D6
            cmpi.w  #$80,D4
            bmi.w   .Exit
            moveq   #5,D6
            cmpi.w  #$D0,D4
            bpl.w   .Exit
            moveq   #6,D6
            tst.w   D5
            beq.w   .Exit
            moveq   #7,D6
            cmpi.w  #1,D5
            bne.w   .Exit
            moveq   #8,D6
            moveq   #-1,D0
.L3:
            tst.b   (VIA_T1C_L-VIA_Base,A0)
            dbne    D0,.L3
            beq.w   .Exit
            moveq   #9,D6
            moveq   #-1,D0
.L4:
            tst.b   (VIA_T2_L-VIA_Base,A0)
            dbne    D0,.L4
            beq.b   .Exit
            andi.b  #$1F,(VIA_ACR-VIA_Base,A0)
            move.b  #$62,(VIA_IFR-VIA_Base,A0)
            move.b  #$E2,(VIA_IER-VIA_Base,A0)
            moveq   #0,D3
            moveq   #0,D4
            moveq   #0,D5
            move.b  #$FF,(VIA_T1C_L-VIA_Base,A0)
            move.b  #$FF,(VIA_T2_L-VIA_Base,A0)
            move.b  #$4,(VIA_T1C_H-VIA_Base,A0)
            move.b  #$1F,(VIA_T2_H-VIA_Base,A0)
            move.l  #$F0000,D0
            move    #$2000,SR
.L5:
            cmpi.w  #$A,D3
            beq.b   .L6
            subq.l  #1,D0
            bne.b   .L5
.L6:
            move    #$2700,SR
            moveq   #$A,D6
            tst.w   D3
            beq.b   .Exit
            moveq   #$B,D6
            tst.w   D4
            beq.b   .Exit
            moveq   #$C,D6
            cmpi.w  #1,D4
            bne.b   .Exit
            moveq   #$D,D6
            tst.w   D5
            beq.b   .Exit
            moveq   #$E,D6
            cmpi.w  #1,D5
            bne.b   .Exit
            moveq   #$F,D6
            moveq   #-1,D0
.L7:
            tst.b   (VIA_T2_L-VIA_Base,A0)
            dbne    D0,.L7
            beq.b   .Exit
            clr.w   D6
.Exit:
            move    #$2700,SR
            movea.l #VIA_Base,A0
            move.b  #$60,(VIA_IER-VIA_Base,A0)
            move    #$2300,SR
            movea.w #Lev1AutoVector,A2
            move.l  (SP)+,(A2)
            jmp     (A6)
LocalVIA1Int:
            movea.l #VIA_Base,A0
            move.b  (VIA_IFR-VIA_Base,A0),D2
            move.b  D2,(VIA_IFR-VIA_Base,A0)
            btst.l  #1,D2
            beq.b   .L1
            addq.w  #1,D3
.L1:
            btst.l  #6,D2
            beq.b   .L2
            addq.w  #1,D4
.L2:
            btst.l  #5,D2
            beq.b   .Exit
            addq.w  #1,D5
.Exit:
            rte
TestSCSI:
            movea.l #SCSIrd,A0
            movea.l #SCSIwr,A1
            move.b  #$80,(sICRwrite-SCSIwr,A1)
            moveq   #0,D0
            move.b  D0,(sICRwrite-SCSIwr,A1)
            tst.b   (sReset-SCSIrd,A0)
            moveq   #1,D6
            move.b  #$80,(sICRwrite-SCSIwr,A1)
            move.b  (sICRread-SCSIrd,A0),D1
            cmpi.b  #$80,D1
            bne.w   .Exit
            addq.w  #1,D6
            move.b  (sCSRread-SCSIrd,A0),D1
            andi.b  #$80,D1
            cmpi.b  #$80,D1
            bne.w   .Exit
            clr.b   (sICRwrite-SCSIwr,A1)
            addq.w  #1,D6
            move.l  A0,-(SP)
            lea     VIA_Base,A0
            tst.b   (VIA_IER-VIA_Base,A0)
            tst.b   (VIA_IFR-VIA_Base,A0)
            movea.l (SP)+,A0
            move.b  #$10,(sICRwrite-SCSIwr,A1)
            move.b  (sICRread-SCSIrd,A0),D1
            cmpi.b  #$10,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  (sBSR-SCSI_Base,A0),D1
            andi.b  #1,D1
            cmpi.b  #1,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  #$8,(sICRwrite-SCSIwr,A1)
            move.b  (sICRread-SCSIrd,A0),D1
            cmpi.b  #$8,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  (sCSRread-SCSIrd,A0),D1
            andi.b  #$40,D1
            cmpi.b  #$40,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  #4,(sICRwrite-SCSIwr,A1)
            move.b  (sICRread-SCSIrd,A0),D1
            cmpi.b  #4,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  (sCSRread-SCSIrd,A0),D1
            andi.b  #2,D1
            cmpi.b  #2,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  #2,(sICRwrite-SCSIwr,A1)
            move.b  (sICRread-SCSIrd,A0),D1
            cmpi.b  #2,D1
            bne.b   .Exit
            addq.w  #1,D6
            move.b  (sBSR-SCSIrd,A0),D1
            andi.b  #2,D1
            cmpi.b  #2,D1
            bne.b   .Exit
            moveq   #0,D6
.Exit:
            jmp     (A6)
TestASC:
            movem.l SP/A6-A0/D5-D0,-(SP)
            move    SR,-(SP)
            move    #$2700,SR
            moveq   #0,D6
            moveq   #0,D5
            moveq   #0,D4
            lea     Sound_Base,A0
            lea     sndrnold,A2
            lea     sndrdold,A3
            move.b  #$1C,(ascVolControl-Sound_Base,A0)
            move.b  (ascVolControl-Sound_Base,A0),D0
            andi.b  #$1C,D0
            bne.b   .GoTest
            lea     sndrnnew,A2
            lea     sndrdnew,A3
.GoTest:
            moveq   #0,D0
            adda.w  #$800,A0
.L1:
            move.b  (A2)+,D0
            beq.b   .Exit
            move.b  (A3)+,D4
            move.b  D4,(A0,D0.w)
            move.b  (A0,D0.w),D5
            cmpi.b  #2,D0
            bne.b   .L2
            andi.b  #$7F,D5
.L2:
            cmp.b   D4,D5
            bne.b   .L3
            bra.b   .L1
.L3:
            move.w  D0,D6
.Exit:
            bsr.b   snd_reg_init
            move    (SP)+,SR
            movem.l (SP)+,D0-D5/A0-A6/SP
            jmp     (A6)
snd_reg_init:
            move.b  #1,(ascMode-Sound_Base,A0)
            clr.b   (ascChipControl-Sound_Base,A0)
            move.b  #$80,(ascFifoControl-Sound_Base,A0)
            clr.b   (ascFifoControl-Sound_Base,A0)
            tst.b   (ascFifoInt-Sound_Base,A0)
            clr.b   (ascWaveOneShot-Sound_Base,A0)
            clr.b   (ascVolControl-Sound_Base,A0)
            clr.b   (ascClockRate-Sound_Base,A0)
            clr.b   (ascTestReg-Sound_Base,A0)
            rts
sndrnnew:
            dc.b    1,1,1,1
            dc.b    2,2,2,2
            dc.b    2,2,2,2
            dc.b    2,2,2,2
            dc.b    2,3,3,3
            dc.b    4,4,4,4
            dc.b    4,4,5,5
            dc.b    5,5,5,5
            dc.b    5,5,5,5
            dc.b    5,5,7,7
            dc.b    7,7,0,0
sndrdnew:
            dc.b    0,1,2,0
            dc.b    0,1,2,4,8,$10,$1E,$1D,$1B,$17,$F,$1F,0
            dc.b    0,$80,0
            dc.b    0,1,2,4,8,0
            dc.b    0,$80,1,2,4,8,7,$B,$D,$E,$F,0
            dc.b    0,2,3,0
sndrnold:
            dc.b    1,1,1,1
            dc.b    2,2,2,2
            dc.b    2,3,3,3
            dc.b    3,3,3,3
            dc.b    3,3,4,4
            dc.b    4,4,4,4
            dc.b    5,5,5,5
            dc.b    5,5,5,5
            dc.b    5,5,5,5
            dc.b    7,7,7,7
            dc.b    0,0
sndrdold:
            dc.b    0,1,2,0
            dc.b    0,1,2,3,0
            dc.b    0,1,2,$80,3,$81,$82,$83,0		
            dc.b    0,1,2,4,8,0 					
            dc.b    0,$80,1,2,4,8,7,$B,$D,$E,$F,0	
            dc.b    0,2,3,0
PRAMTest:
            moveq   #-1,D6
            jmp     (A6)                            ; Return
FindStartUpDevice:
            bsr.w   EmbarkOnSearch
            move.l  Ticks,-(SP)
            bsr.w   LoadSCSIDrivers
            bsr.w   WaitForInternal
            bra.b   .FirstEntry
.NextPass:
            move.l  Ticks,D0
            sub.l   (SP),D0
            cmpi.l  #7200,D0
            bls.b   .L1
            lea     (-$10,SP),SP
            lea     ($C,SP),A0
            move.l  #sleepSig,(A0)
            move.l  A0,(8,SP)
            move.l  A0,(4,SP)
            move.w  #4,(2,SP)
            move.w  #sleepReq,(SP)
            movea.l SP,A0
            _PMgrOp
            move.w  (A0),D0
            lea     ($10,SP),SP
            cmpi.w  #$70,D0
            bne.b   .L3
            movea.l ScrnBase,A0
            move.l  ScreenBytes,D1
            lsr.l   #2,D1
            subq.w  #1,D1
            moveq   #0,D0
.L1:
            move.l  D0,(A0)+
            dbf     D1,.L1
.L2:
            bra.b   .L2
.L3:
            bsr.w   PlanStrategy
            bsr.w   LoadSCSIDrivers
.TryAgain:
            bsr.w   VisualUpdate
.FirstEntry:
            bsr.w   FindNextCandidate
            beq.b   .NextPass
            bsr.w   SelectDevice
            bsr.w   CheckMouseEject
            beq.b   .TryAgain
            bsr.w   GetStartUpInfo
            beq.b   .GotIt
            bsr.w   ReactToFailure
            bra.b   .TryAgain
.GotIt:
            addq.l  #4,SP
            bra.w   ShowSuccess
GetDefaultStartup:
            move.l  #4<<16|$78<<0,D0
            _ReadXPRam
            rts
SetDefaultStartup:
            move.l  #4<<16|$78<<0,D0
            _WriteXPRam
            rts
InternalWait:
            move.l  A0,D1
            cmpi.w  #$9,D1
            bhi.b   .Exit
            lsl.w   #1,D1
            lea     .JumpTable,A1
            move.w  (A1,D1.w),D1
            jsr     (A1,D1.w)
.Exit:
            rts
.JumpTable:
            dc.w    IGetTimeOut-.JumpTable
            dc.w    ISetTimeOut-.JumpTable
            dc.w    IGetWaitFlags-.JumpTable
            dc.w    ISetWaitFlags-.JumpTable
            dc.w    IDisableDynWait-.JumpTable
            dc.w    IEnableDynWait-.JumpTable
            dc.w    IDisablePermWait-.JumpTable
            dc.w    IEnablePermWait-.JumpTable
EmbarkOnSearch:
            subq.l  #4,SP
            movea.l SP,A0
            _GetDefaultStartup
            move.w  (SP)+,D3
            move.w  (SP)+,D4
            suba.l  A2,A2
            lea     IsItFlopOrDef,A3
            tst.w   D4
            bne.b   .L1
            lea     IsItAnything,A3
.L1:
            movea.l Ticks,A4
            clr.w   D6
            rts
WaitForInternal:
            movem.l A2/D5,-(SP)
            _GetWaitFlags
            andi.b  #$80,D0
            bne.b   .RawExit
            _GetTimeOut
            bne.b   .UseGivenTime
            move.w  #$140,D0
.UseGivenTime:
            mulu.w  #$3C,D0
            move.l  D0,D5
            _DisablePermWait
.WaitForIt:
            move.w  KeyMap+6,D0
            cmpi.w  #CmdShiftOptDel,D0
            beq.b   .RawExit
            moveq   #0,D1
            move.b  SCSIPoll,D1
            andi.b  #7,D1
            moveq   #0,D0
            bset.l  D1,D0
            move.l  A0,-(SP)
            BigJsr  SCSILoad,A0
            movea.l (SP)+,A0
            beq.b   .Exit
            suba.l  A2,A2
            move.w  #$FFDF,D0
            sub.w   D1,D0
            move.w  D0,D1
.FindIntHD:
            bsr.w   NextDQEntry
            beq.b   .EndOfQueue
            cmp.w   ($8,A2),D1
            bne.b   .FindIntHD
            bra.b   .Exit
.EndOfQueue:
            cmp.l   Ticks,D5
            bcs.b   .NoDrivePresent
            moveq   #$F,D0
            add.l   Ticks,D0
.DelayLoop:
            cmp.l   Ticks,D0
            bcc.b   .DelayLoop
            bra.b   .WaitForIt
.NoDrivePresent:
            _DisableDynWait
.Exit:
            _EnablePermWait
.RawExit:
            movem.l (SP)+,D5/A2
            rts
LoadSCSIDrivers:
            moveq   #-1,D0
            move.w  KeyMap+6,D1
            cmpi.w  #CmdShiftOptDel,D1
            bne.b   .GoLoadD
            move.b  SCSIPoll,D1
            andi.b  #7,D1
            moveq   #-1,D0
            bclr.l  D1,D0
.GoLoadD:
            move.l  A0,-(SP)
            BigJsr  SCSILoad,A0
            movea.l (SP)+,A0
            rts
PlanStrategy:
            cmpi.w  #$FFFB,D4
            beq.b   .Exit
            lea     IsItAnything,A3
.Exit:
            rts
FindNextCandidate:
            bsr.w   NextDQEntry
            beq.b   .DoneLooking
            bsr.w   IsReject
            beq.b   FindNextCandidate
            jsr     (A3)
            bne.b   FindNextCandidate
.DoneLooking:
            move.l  A2,D0
            rts
NextDQEntry:
            move.l  A2,D0
            bne.b   .L1
            movea.l DrvQHdr+qHead,A2
            bra.b   .Exit
.L1:
            movea.l (qLink,A2),A2
.Exit:
            move.l  A2,D0
            rts
SelectDevice:
            lea     (-$400,A6),A0
            move.w  (8,A2),D0
            move.w  D0,($18,A0)
            move.w  D0,BtDskRfn
            move.w  (6,A2),D0
            move.w  D0,($16,A0)
            move.w  D0,BootDrive
            rts
CheckMouseEject:
            tst.b   MBState
            bmi.b   .DontEject
            bsr.w   EjectMe
.DontEject:
            rts
GetStartUpInfo:
            move.w  #1,(ioPosMode,A0)
            clr.l   (ioPosOffset,A0)
            move.l  A6,($20,A0)
            move.l  #$400,(ioByteCount,A0)
            _Read
            bne.b   .Exit
            cmpi.w  #'LK',(A6)
.Exit:
            rts
ReactToFailure:
            cmpi.w  #offLinErr,D0
            beq.b   .Exit
            cmpi.w  #noDriveErr,D0
            beq.b   .This1NoMo
            bsr.w   EjectMe
            beq.b   .ShowMe
.This1NoMo
            bsr.w   NeverAgain
.ShowMe:
            bsr.w   ShowDeviceFail
.Exit:
            rts
VisualUpdate:
            bset.l  #FlashEnable,D6
            beq.b   .Exit
            cmpa.l  Ticks,A4
            bhi.b   .Exit
            movea.l Ticks,A4
            adda.w  #$3C,A4
            bclr.l  #$A,D6
            beq.b   .L1
            move.b  #3,D6
.L1:
            bchg.l  #9,D6
            bne.b   .L2
            bsr.w   ShowPlainDisk
            bra.b   .Exit
.L2:
            tst.b   D6
            beq.b   .L3
            bsr.w   ShowDiskX
            subq.b  #1,D6
            bra.b   .Exit
.L3:
            bsr.w   ShowDiskQ
.Exit:
            rts
EnableXFlash:
            bset.l  #$A,D6
            rts
ShowSuccess:
            _HideCursor
            movem.l A6-A5,-(SP)
            BSR6    HappyMac
            movem.l (SP)+,A5-A6
            rts
EjectMe:
            lea     (MyIOPBA6,A6),A0
            move.w  #EjectCode,(csCode,A0)
            _Control
            rts
IsReject:
            bsr.w   InBootMask
            bne.b   .Exit
            move.w  (6,A2),D1
            move.w  BootMask,D0
            btst.l  D1,D0
.Exit:
            rts
IsItFlopOrDef:
            cmp.w   (8,A2),D4
            bne.b   IsItFloppy
            cmpi.w  #-1,D3
            beq.b   .Exit
            cmp.w   (6,A2),D3
            bne.b   IsItFloppy
.Exit:
            rts
IsItFloppy:
            cmpi.w  #FloppyRefNum,(dqRefNum,A2)
            rts
IsItAnything:
            cmp.w   D0,D0
            rts
NeverAgain:
            bsr.w   InBootMask
            bne.b   .Exit
            move.w  (6,A2),D1
            move.w  BootMask,D0
            bclr.l  D1,D0
            move.w  D0,BootMask
.Exit:
            rts
InBootMask:
            tst.w   ($6,A2)
            bmi.b   .Exit
            cmpi.w  #$F,(6,A2)
            bhi.b   .Exit
            moveq   #0,D0
.Exit:
            rts
ShowDeviceFail:
            bsr.w   IsItFloppy
            bne.b   .Exit
            bsr.w   EnableXFlash
.Exit:
            rts
HappyMac:
            bsr.w   EraseMyIcon
            lea     HappyIcon,A0
            jsp     PlotMyIcon
            jmp     (A6)
ShowPlainDisk:
            lea     DiskIcon,A0
            jpp     PlotMyIcon
ShowDiskQ:
            lea     QDiskIcon,A0
            jpp     PlotMyIcon
ShowDiskX:
            lea     XDiskIcon,A0
            jpp     PlotMyIcon
EraseMyIcon:
            movem.l A1-A0/D2-D0,-(SP)
            bsr.w   PSHIcnRect
            move.l  SP,-(SP)
            movea.l (A5),A0
            pea     (-$18,A0)
            _FillRect
            addq.w  #8,SP
            movem.l (SP)+,D0-D2/A0-A1
            rts
PlotMyIcon:
            move.l  A0,-(SP)
            bsr.w   PSHIcnRect
            move.l  SP,-(SP)
            move.l  ($C,SP),-(SP)
            bsr.w   PlotIcnN
            adda.w  #$C,SP
            rts
PlotIcnN:
            link.w  A6,#0
            movea.l (8,A6),A0
            movea.l (A5),A1
            movea.l (A1),A1
            move.l  #$200020,-(SP)
            clr.l   -(SP)
            move.w  #4,-(SP)
            move.l  A0,-(SP)
            move.l  #$200020,-(SP)
            clr.l   -(SP)
            move.w  #4,-(SP)
            pea     ($80,A0)
            pea     (-$E,A6)
            pea     (-$1C,A6)
            pea     ($2,A1)
            pea     (-$8,A6)
            pea     (-$16,A6)
            move.l  ($C,A6),-(SP)
            _CopyMask
            adda.w  #$1C,SP
            unlk    A6
            movea.l (SP)+,A0
            addq.w  #8,SP
            jmp     (A0)
PSHIcnRect:
            move.l  #$200020,-(SP)
            clr.l   -(SP)
            movea.l (A5),A0
            pea     (-$74,A0)
            pea     ($4,SP)
            bsr.w   CenterRect
            movea.l ($8,SP),A0
            move.l  ($4,SP),($8,SP)
            move.l  (SP)+,(SP)
            jmp     (A0)
HappyIcon:
            incbin  'HappyIcon.bin'
DiskIcon:
            incbin  'DiskIcon.bin'
QDiskIcon:
            incbin  'QDiskIcon.bin'
XDiskIcon:
            incbin  'XDiskIcon.bin'
IGetTimeOut:
            jsp     GetRawTimeOut
            andi.b  #$1F,D0
            rts
ISetTimeOut:
            move.b  D0,-(SP)
            jsp     GetRawTimeOut
            move.b  (SP)+,D1
            cmpi.b  #$1F,D1
            bls.b   .L1
            move.b  #$1F,D1
.L1:
            andi.b  #$E0,D0
            or.b    D1,D0
            jpp     SetRawTimeOut
IGetWaitFlags:
            jsp     GetRawTimeOut
            andi.b  #$E0,D0
            rts
ISetWaitFlags:
            move.b  D0,-(SP)
            jsp     GetRawTimeOut
            move.b  (SP)+,D1
            andi.b  #$E0,D1
            andi.b  #$1F,D0
            or.b    D1,D0
            jpp     SetRawTimeOut
IDisableDynWait:
            _GetWaitFlags
            bset.l  #7,D0
            _SetWaitFlags
            rts
IEnableDynWait:
            _GetWaitFlags
            bclr.l  #7,D0
            _SetWaitFlags
            rts
IDisablePermWait
            _GetWaitFlags
            bset.l  #6,D0
            _SetWaitFlags
            rts
IEnablePermWait:
            _GetWaitFlags
            bclr.l  #6,D0
            _SetWaitFlags
            rts
GetRawTimeOut:
            clr.b   -(SP)
            movea.l SP,A0
            move.l  #1<<16|1<<0,D0
            _ReadXPRam
            moveq   #0,D0
            move.b  (SP)+,D0
            rts
SetRawTimeOut:
            move.b  D0,-(SP)
            movea.l SP,A0
            move.l  #1<<16|1<<0,D0
            _WriteXPRam
            move.b  (SP)+,D0
            rts
BootMe:
            bsr.w   FindStartUpDevice
            movea.l SysZone,A0
            move.l  A0,TheZone
            move.l  A0,ApplZone
            move.l  (bkLim,A0),HeapEnd
            move.b  ($6,A6),D1
            cmpi.b  #BBOldExecVers,D1
            beq.b   .L1
            andi.b  #MyExecMask,D1
            cmpi.b  #MyExecMask,D1
            bne.b   .L2
.L1:
            jsr     ($2,A6)
.L2:
            move.w  ($7C,A6),D0
            _InitEvents
            move.w  ($7A,A6),D0
            _InitFS
            lea     (MyIOPBA6,A6),A0

            org     $903494
CenterRect:
            org     $9034C6
MouseInit:
            org     $903510
InitEvents:
            org     $90353A
CritErr:
            org     $903644
PutSymbol:
            org     $903654
PutIcon:
            org     $90371E
SysErrInit:
            org     $90379E
DebugProlog:
            org     $9037B0
ToDeepShit:
            org     $90380A
IRQException:
            org     $903822
SysErr2:
            org     $90386C
AllocFakeRgns:
            org     $9038BA
DSErrorHandler:
            org     $903C78
DbgCmd_DM:
            org     $903CF4
DbgCmd_SM:
            org     $903D1E
DbgCmd_TD:
            move.l  #$C30,MacsBugDMnext
            bra.w   DbgCmd_DM
DbgCmd_D_at:

            org     $903EA6
SizeMemory:
            movea.l A6,A4                           ; Save the return address
            move.l  D0,D2
            movea.l #MaxRAMSize-SlimSpaceSize,A3    ; Load our installed memory size, leaving room for SLIM cards
            moveq   #1<<SlimInstalled,D0            ; Load SLIM adapter installed bit to check
            and.w   AccessBase,D0                   ; Is a SLIM card adapter installed?
            bne.b   .L1                             ; Yes, do not test its range
            adda.l  #SlimSpaceSize,A3               ; No, free to use for RAM
.L1:
            suba.l  A0,A0                           ; Start at the beginning of RAM
.L2:
            move.l  (A0),D0                         ; Save the beginning of RAM
            move.l  #$1E0FBB22,(A0)                 ; Load our test pattern
            lea     .Pattern,A1
            move.l  (A1),D1
            move.l  (A1),D1
            bra.b   .L3
.Pattern:
            dc.l    $FFFFFFFF
.L3:
            cmpi.l  #$1E0FBB22,(A0)
            beq.b   .L4
            adda.l  #$100000,A0                     ;
            cmpa.l  A3,A0                           ;
            bne.b   .L2
            bra.w   Error1Handler
.L4:
            cmpi.l  #$22BBF0E1,D2                   ; Should we test the memory?
            bne.b   .L6
            movea.l A0,A1
            adda.w  #$400,A1                        ; Start past the end of our vectors
            movea.l A1,SP
            moveq   #0,D6
            BSR6    Mod3Test
            tst.l   D6
            bne.w   Error1Handler
.L6:
            move.l  A3,D0
            subi.l  #$100000,D0
.L7:
            movea.l D0,A0
            swap    D0
            move.w  D0,D1
            swap    D1
            move.w  D0,D1
            move.l  (A0),-(SP)
            move.l  D1,(A0)
            move.l  D1,(A0)
            swap    D0
            subi.l  #$100000,D0
            cmpi.l  #0,D0
            bne.b   .L7
            move.l  ResetStackPtr,-(SP)
            clr.l   ResetStackPtr
            move.l  SP,D6
            subi.l  #$80,D6
            movea.l D6,A1
            movea.l A1,A2
            clr.b   D3
            moveq   #0,D0
.L8:
            movea.l D0,A0
            swap    D0
            move.w  D0,D1
            swap    D1
            move.w  D0,D1
            move.l  #$FFFFFFFF,MonkeyLives
            move.l  #$FFFFFFFF,MonkeyLives
            move.l  (A0),D2
            cmp.w   D1,D2
            beq.b   .L9
            swap    D1
            swap    D2
            cmp.w   D1,D2
            bne.b   .L11
.L9:
            tst.b   D3
            bne.b   .L10
            moveq   #-1,D3
            move.l  A0,(A2)+
            clr.l   (A2)
.L10:
            addi.l  #$100000,(A2)
            bra.b   .L12
.L11:
            tst.b   D3
            beq.b   .L12
            clr.b   D3
            addq.l  #4,A2
.L12:
            addi.w  #$10,D0
            swap    D0
            cmp.l   A3,D0
            beq.b   .L13
            bra.b   .L8
.L13:
            tst.b   D3
            beq.b   .L14
            addq.l  #4,A2
.L14:
            move.l  #$FFFFFFFF,(A2)
            move.l  (4,A1),D6
            suba.l  A0,A0
.L15:
            move.l  (SP)+,(A0)
            adda.l  #$100000,A0
            cmpa.l  A3,A0
            bne.b   .L15
            movea.l A4,A6                           ; Restore return address
            jpp     .Exit
.Exit:
            jmp     (A6)                            ; Restore return address

            org     $904014
PMGRrecv:
            moveq   #0,D1
PMGRsend:
            move.l  A0,-(SP)
            move.l  A0,-(SP)
            move.w  D1,-(SP)
            move.w  D0,-(SP)
            movea.l SP,A0
            _PMgrOp
            lea     ($8,SP),SP
            movea.l (SP)+,A0
            rts
PMgrInt:
            subq.w  #4,SP
            movea.l SP,A0
            moveq   #$78,D0
            bsr.b   PMGRrecv
            move.b  (A0),D1
            addq.w  #4,SP
            tst.w   D0
            bne.b   .L1
            btst.l  #0,D1
            bne.b   .L2
            btst.l  #1,D1
            bne.b   .L3
            btst.l  #2,D1
            bne.b   .L4
.L1:
            rts
.L2:
            move.b (Lvl1DT+8),D0
            beq.b   .L1
            movea.l D0,A0
            jmp     (A0)
.L3:
            movea.l PowerMgrVars,A0
            move.l  (vBatInt,A0),D0
            beq.b   .L1
            movea.l D0,A0
            jmp     (A0)
.L4:
            movea.l PowerMgrVars,A0
            move.l  (vEnvInt,A0),D0
            beq.b   .L1
            movea.l D0,A0
            jmp     (A0)
EnvInt:
            subq.w  #4,SP
            movea.l SP,A0
            moveq   #batteryRead,D0
            bsr.b   PMGRrecv
            addq.w  #4,SP
            rts
BatInt:
            movea.l PowerMgrVars,A2
            subq.w  #4,SP
            movea.l SP,A0
            moveq   #batteryRead,D0
            bsr.b   PMGRrecv
            move.b  (A0),(Charger,A2)
            addq.w  #4,SP
            btst.b  #ChrgState,(Charger,A2)
            beq.b   .Exit
            st      (TOdirtyFlag,A2)
.Exit:
            rts
BatWatch:
            movea.l PowerMgrVars,A2
            tst.b   (SysTaskFlag,A2)
            bne.b   .didsystask
            lea     (BatVBLTask,A2),A0
            move.w  #BatFreq,(vblCount,A0)
            rts
.didsystask:
            bsr.w   GetLevel
            bne.b   .valid
            move.b  #-1,(LastLevel,A2)              ; Reset last level
            bra.b   .BatWatchOut
.valid:
            tst.b   D0
            bpl.b   .positive
            bsr.b   RemoveMsg
            move.b  D0,(LastLevel,A2)
            bra.b   .BatWatchOut
.positive:
            beq.b   .lesser
            tst.b   (LastLevel,A2)
            bmi.b   .lpowermode
            cmp.b   (LastLevel,A2),D0
            bls.b   .lesser
.lpowermode:
            bsr.b   RemoveMsg
            bsr.b   InstallMsg
            clr.b   (Level4Cnt,A2)
            move.b  D0,(LastLevel,A2)
            bra.b   .BatWatchOut
.lesser:
            cmpi.b  #4,(LastLevel,A2)
            bne.b   .BatWatchOut
            addq.b  #1,(LastLevel,A2)
.BatWatchOut:
            lea     (BatVBLTask,A2),A0
            move.w  #BatFreq,(vblCount,A0)
            tst.b   (TOdirtyFlag,A2)
            beq.b   .Exit
            clr.b   (TOdirtyFlag,A2)
            lea     Scratch20,A0
            move.l  #8<<16|$70<<0,D0
            _ReadXPRam
            move.b  ($7,A0),(SleepFlags,A2)
            move.w  (A0),(A2)
            _IdleUpdate
.Exit:
            rts
RemoveMsg:
            tst.b   (lpMSGvalid,A2)
            beq.b   .nomsg
            move.l  D0,-(SP)
            lea     (BNmQEntry,A2),A0
            _NMRemove
            move.l  (SP)+,D0
            clr.b   (lpMSGvalid,A2)
.nomsg:
            rts
InstallMsg:
            move.l  D0,-(SP)
            lea     (BNmQEntry,A2),A0
            move.w  #8,(4,A0)
            moveq   #-1,D2
            move.l  D2,(14,A0)
            moveq   #0,D2
            move.w  D2,($E,A0)
            lea     .nmproc,A1
            move.l  A1,($1C,A0)
            move.l  ($6E,A2),($10,A0)
            move.w  D0,D2
            subq.w  #1,D2
            lsl.w   #2,D2
            move.l  ($72,A2,D2.w),($18,A0)
            _NMInstall
            move.l  (SP)+,D0
            st      (lpMSGvalid,A2)
.nmproc:
            rts
GetLevel:
            subq.w  #4,SP
            movea.l SP,A0
            moveq   #batteryNow,D0
            bsr.w   PMGRrecv
            moveq   #0,D3
            move.b  (A0)+,(Charger,A2)
            move.b  (A0),D3
            addq.w  #4,SP
            btst.b  #ChrgState,(Charger,A2)
            beq.b   .ChargeSame
            st      (TOdirtyFlag,A2)
.ChargeSame:
            subq.b  #1,(BatQIndex,A2)
            bpl.b   .IndexOK
            move.b  #7,(BatQIndex,A2)
.IndexOK:
            moveq   #0,D0
            move.b  (BatQIndex,A2),D0
            move.b  D3,(BatQ,A2,D0.w)
            tst.b   (BatQ,A2)
            beq.b   GetLevel
            moveq   #0,D3
            move.b  (BatQ,A2),D0
            add.w   D0,D3
            move.b  (BatQ+1,A2),D0
            add.w   D0,D3
            move.b  (BatQ+2,A2),D0
            add.w   D0,D3
            move.b  (BatQ+3,A2),D0
            add.w   D0,D3
            move.b  (BatQ+4,A2),D0
            add.w   D0,D3
            move.b  (BatQ+5,A2),D0
            add.w   D0,D3
            move.b  (BatQ+6,A2),D0
            add.w   D0,D3
            move.b  (BatQ+7,A2),D0
            add.w   D0,D3
            lsr.w   #1,D3
            moveq   #0,D2
            move.b  (LowWarn,A2),D2
            moveq   #0,D1
            move.b  (Cutoff,A2),D1
            sub.w   D1,D2
            asl.w   #2,D1
            moveq   #4,D0
            add.w   D2,D1
            cmp.w   D1,D3
            bls.b   .foundLevel
            moveq   #3,D0
            add.w   D2,D1
            cmp.w   D1,D3
            bls.b   .foundLevel
            moveq   #2,D0
            add.w   D2,D1
            cmp.w   D1,D3
            bls.b   .foundLevel
            moveq   #1,D0
            add.w   D2,D1
            cmp.w   D1,D3
            bls.b   .foundLevel
            moveq   #0,D0
            add.w   D2,D1
            add.w   D2,D1
            cmp.w   D1,D3
            bls.b   .foundLevel
            moveq   #-1,D0
.foundLevel:
            lsr.w   #2,D3
            move.b  D3,(BatAvg,A2)
            tst.b   (BatQ,A2)
            rts
SndWatch:
            movea.l PowerMgrVars,A2
            lea     (SwVBLTask,A2),A0
            move.w  #SndWFreq,(vblCount,A0)
            subq.w  #4,SP
            movea.l SP,A0
            move.w  #soundRead,D0
            bsr.w   PMGRrecv
            move.b  (A0),D2
            beq.b   .Exit
            btst.l  #1,D2
            bne.b   .ClearLatch
            clr.b   (A0)
            moveq   #1,D1
            move.w  #soundSet,D0
            bsr.w   PMGRsend
            bra.b   .Exit
.ClearLatch:
            move.b  #sndOnClrLtch,(A0)
            moveq   #1,D1
            move.w  #soundSet,D0
            bsr.w   PMGRsend
            _IdleUpdate
.Exit:
            addq.w  #4,SP
            rts
PMgrOp:
            andi.w  #$600,D1
            beq.w   .L13
            cmpi.w  #$400,D1
            beq.b   .IdleEnableDisable
            bcs.b   .IdleUpdate
            bra.b   .L3
.IdleEnableDisable:
            move    SR,-(SP)
            ori     #$300,SR
            move.l  D0,D1
            movea.l PowerMgrVars,A1
            moveq   #0,D0
            tst.l   D1
            bpl.b   .L1
            move.b  (SaveSpeedo,A1),D0
            move    (SP)+,SR
            rts
.L1:
            bne.b   .L2
            subq.b  #1,(IdleFlagCnt,A1)
            bpl.b   .DontClr
            clr.b   (IdleFlagCnt,A1)
            bra.b   .DontClr
.L2:
            addq.b  #1,(IdleFlagCnt,A1)
.DontClr:
            move.b  (IdleFlagCnt,A1),D0
            move    (SP)+,SR
            rts
.IdleUpdate:
            move    SR,-(SP)
            ori     #$300,SR
            tst.w   Clock16M
            movea.l PowerMgrVars,A1
            move.l  (LastAct,A1),D0
            cmp.l   Ticks,D0
            bhi.b   .noLastActUpdate
            move.l  Ticks,(LastAct,A1)
.noLastActUpdate:
            move.b  #CPUSpeed16MHz,(SaveSpeedo,A1)
            move.l  (LastAct,A1),D0
            move    (SP)+,SR
            rts
.L3:
            bclr.l  #7,D0
            bne.w   .L7
            move.w  D0,D1
            moveq   #0,D2
            moveq   #0,D3
            moveq   #0,D4
            lea     (-$10,SP),SP
            lea     ($C,SP),A0
            move.l  A0,($8,SP)
            move.l  A0,($4,SP)
            bclr.l  #0,D1
            bne.b   .L4
            move.w  #modemRead,(SP)
            clr.w   ($2,SP)
            movea.l SP,A0
            _PMgrOp
            move.b  ($C,A0),D3
            btst.l  #3,D3
            beq.b   .L4
            move.l  #1<<16|PmgrPramBase+7<<0,D0
            _ReadXPRam
            btst.b  #2,(A0)
            bne.b   .L4
            move.b  D3,D0
            andi.b  #2,D0
            lsl.b   #1,D0
            cmp.b   D0,D1
            beq.b   .L5
.L4:
            ori.b   #$90,D2
            moveq   #0,D3
.L5:
            ori.b   #$C2,D2
            move.w  #$10,(SP)
            move.w  #1,($2,SP)
            move.b  D2,($C,SP)
            movea.l SP,A0
            _PMgrOp
            tst.b   D3
            beq.b   .L6
            moveq   #8,D0
            movea.l D0,A0
            _Delay
            move.b  #$88,($C,SP)
            move.w  #1,($2,SP)
            move.w  #powerCntl,(SP)
            movea.l SP,A0
            _PMgrOp
.L6:
            lea     ($10,SP),SP
            moveq   #0,D0
            rts
.L7:
            andi.w  #4,D0
            move.w  D0,D1
            moveq   #0,D2
            lea     (-$10,SP),SP
            lea     ($C,SP),A0
            move.l  A0,($8,SP)
            move.l  A0,($4,SP)
            move.w  #modemRead,(SP)
            clr.w   ($2,SP)
            movea.l SP,A0
            _PMgrOp
            move.b  ($C,A0),D3
            lea     PortAUse,A1
            tst.b   D1
            beq.b   .L8
            tst.b   (A1)+
.L8:
            tst.b   (A1)
            bmi.b   .L9
            btst.l  #0,D3
            beq.b   .L12
            andi.b  #$2,D3
            lsl.b   #1,D3
            cmp.b   D3,D1
            beq.b   .L10
            move.b  #$10,D2
            bra.b   .L11
.L9:
            move.b  #$52,D2
            move.b  ($C,A0),D3
            btst.l  #0,D3
            beq.b   .L11
.L10:
            move.b  #8,($C,SP)
            move.w  #1,($2,SP)
            move.w  #powerCntl,(SP)
            movea.l SP,A0
            _PMgrOp
            tst.b   D2
            beq.b   .L12
            moveq   #$21,D0
            movea.l D0,A0
            _Delay
.L11:
            tst.b   D2
            beq.b   .L12
            move.b  D2,($C,SP)
            move.w  #1,($2,SP)
            move.w  #powerCntl,(SP)
            movea.l SP,A0
            _PMgrOp
.L12:
            lea     ($10,SP),SP
            moveq   #0,D0
            rts
.L13:
            movem.l A2-A0/D4-D1,-(SP)
            move    SR,-(SP)
            movea.l VIA,A1
            move.b  (VIA_IER-VIA_Base,A1),D4
            andi.b  #$10,D4
            move.b  #$10,(VIA_IER-VIA_Base,A1)
            subq.l  #2,SP
            movea.l PowerMgrVars,A2
            cmpa.w  #-1,A2
            beq.b   .L14
            move.b  (SaveSpeedo,A2),D3
            move.w  D3,(SP)
            move.b  #CPUSpeed16MHz,(SaveSpeedo,A2)
.L14:
            tst.w   Clock16M
            move.b  #-1,(VIA_DDR_A-VIA_Base,A1)
            move.b  (VIA_ORA-VIA_Base,A1),D2
            swap    D2
            moveq   #7,D3
.L15:
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            move    ($2,SP),SR
            move.w  (TimeSCSIDB),D2
            lsl.w   #2,D2
.L16:
            btst.b  #1,(A1)
            beq.b   .L17
            move.b  (HeapStart,A1),D0
            cmpi.b  #-1,D0
            beq.b   .L18
.L17:
            dbf     D2,.L16
            move.l  #$FFFFCD38,D0
            bra.w   .L27
.L18:
            ori.w   #$300,SR
            tst.w   Clock16M
            lea     ($1,A0),A2
            moveq   #$40,D2
            bsr.w   FUN_0090455C
            beq.b   .L20
            moveq   #$40,D2
.L19:
            move    ($2,SP),SR
            dbf     D2,.L19
            dbf     D3,.L15
            bra.b   .L27
.L20:
            moveq   #0,D1
            lea     ($3,A0),A2
            move.b  (A2),D1
            bsr.w   FUN_00904558
            bne.b   .L27
            subq.w  #1,D1
            bmi.b   .L22
            movea.l ($4,A0),A2
.L21:
            bsr.w   FUN_00904558
            bne.b   .L27
            dbf     D1,.L21
.L22:
            move.b  ($1,A0),D0
            btst.l  #3,D0
            beq.b   .L26
            movea.l A0,A2
            addq.l  #1,A2
            move.w  TimeSCSIDB,D2
.L23:
            btst.b  #1,(A1)
            beq.b   .L24
            dbf     D2,.L23
            move.l  #$FFFFCD37,D0
            bra.b   .L27
.L24:
            clr.l   (A0)
            bsr.w   FUN_009045A0
            bne.b   .L27
            addq.l  #1,A2
            bsr.w   FUN_009045A0
            bne.b   .L27
            move.w  ($2,A0),D1
            movea.l ($8,A0),A2
            subq.w  #1,D1
            bmi.b   .L26
.L25:
            bsr.w   FUN_009045A0
            bne.b   .L27
            dbf     D1,.L25
.L26:
            moveq   #0,D0
.L27:
            move.b  #-1,(VIA_DDR_A-VIA_Base,A1)
            swap    D2
            move.b  D2,(VIA_ORA-VIA_Base,A1)
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            movea.l PowerMgrVars,A2
            cmpa.w  #-1,A2
            beq.b   .L28
            move.w  (SP),D3
            move.b  D3,(SaveSpeedo,A2)
            cmpi.b  #$10,D3
            beq.b   .L28
            tst.w   Clock1M
.L28:
            addq.l  #2,SP
            bset.l  #7,D4
            move.b  D4,(VIA_IER-VIA_Base,A1)
            move    (SP)+,SR
            movem.l (SP)+,D1-D4/A0-A2
            tst.w   D0
            rts
FUN_00904558:
            move.w  #$400,D2
FUN_0090455C:
            moveq   #0,D0
            move.b  #-1,(VIA_DDR_A-VIA_Base,A1)
            move.b  (A2)+,(VIA_ORA-VIA_Base,A1)
            bclr.b  #PMreq,(VIA_BufB-VIA_Base,A1)
.L1:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            beq.b   .L2
            dbf     D1,.L1
            move.l  #$FFFFCD36,D0
            bra.b   SharedReturn_90455C
.L2:
            moveq   #$40,D2
            bset.b  #PMreq,(VIA_BufB-VIA_Base,A1)
.L3:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            bne.b   SharedReturn_90455C
            dbf     D2,.L3
            move.l  #$FFFFCD35,D0
SharedReturn_90455C:
            bset.b  #PMreq,(VIA_BufB-VIA_Base,A1)
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            tst.l   D0
            rts
FUN_009045A0:
            moveq   #0,D0
            clr.b   (VIA_DDR_A-VIA_Base,A1)
            moveq   #64,D2
.L1:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            beq.b   .L2
            dbf     D2,.L1
            move.l  #$FFFFCD34,D0
            bra.b   SharedReturn_90455C
.L2:
            bclr.b  #PMreq,(VIA_BufB-VIA_Base,A1)
            moveq   #64,D2
            move.b  (VIA_ORA-VIA_Base,A1),(A2)+
.L3:
            btst.b  #PMack,(VIA_BufB-VIA_Base,A1)
            bne.b   SharedReturn_90455C
            dbf     D2,.L3
            move.l  #$FFFFCD33,D0
            bra.b   SharedReturn_90455C
GoToSleep:
            btst.l  #9,D1
            bne.w   SlpQInstall
            btst.l  #10,D1
            bne.w   SlpQRemove
            move    SR,-(SP)
            movem.l A6-A0/D7-D1,-(SP)
            move.w  D0,D1
            move.w  D0,D2
            cmpi.b  #1,D0
            beq.b   .L1
            cmpi.b  #2,D0
            beq.b   .L1
            cmpi.b  #6,D0
            bne.b   .L3
.L1:
            movea.l PowerMgrVars,A0
            move.l  (SleepNetHook,A0),D3
            beq.b   .L2
            movea.l D3,A1
            jsr     (A1)
.L2:
            bsr.w   CheckAppleTalk
            bne.b   .L3
            move.w  D2,D0
            bsr.w   DoQueue
            beq.b   .L4
.L3:
            move.w  D2,D0
            movem.l (SP)+,D1-D7/A0-A6
            move    (SP)+,SR
            rts
.L4:
            ori     #$700,SR
            move.l  ResetVector.w,-(SP)
            movea.l VIA,A0
            move.b  #-1,(VIA_DDR_A-VIA_Base,A0)
            
