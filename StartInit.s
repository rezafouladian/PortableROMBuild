            INCLUDE 'HardwareEqu.s'
            INCLUDE 'ConstEqu.s'
            INCLUDE 'Orphan.s'
            INCLUDE 'Traps.s'

            IFND PortableAbs
PortableAbs EQU     0
            ENDIF

            org     BaseOfROM
Checksum    dc.l    $96CA3846
StartPC     dc.l    ResetEntry
ROMVersion  dc.b    $3
            dc.b    $7A
StBoot      jmp     (StartBoot,PC)
BadDisk     jmp     (StartBoot,PC)
            dc.w    0
PatchFlags  dc.b    0
            dc.b    0
            dc.l    ForeignOS-BaseOfROM
RomRsrc     dc.l    RomRsrcHead-BaseOfROM
Eject       jmp     (GoofyDoEject,PC)
DispOff     dc.l    DispTable-BaseOfROM
Critical    jmp     (CritErr,PC)
ResetEntry  jmp     (StartBoot,PC)
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
            move    #$2700,SR                       ; Disable processor interrupts
            lea     BaseOfROM,A0
            move.l  A0,D0
            bne.b   .L1
            move.l  #BaseOfROM,D0
            jmp     (.L1,PC,D0*1)
            ; ^ Miscompare: (Original 4E FB 08 02) - (Assembled 4E FB 00 02)
            ; Location: 90006A
            ; Disassembled: jmp     (0x90006E,PC,D0w*0x1)
.L1:
            tst.w   Clock16M
            move.w  #RAMconfigInit,RAMconfigBase
            cmpi.l  #sleepConst,WarmStart
            bne.b   .L2
            move.l  #wmStConst,WarmStart
            jmp     (WakeUp,PC)
.L2:
            jmp     (StartTest1,PC)
StartInit1:
            bsr.w   InitVIA
            bsr.w   InitSCC
            bsr.w   InitIWM
            bsr.w   InitSCSI
            bsr.w   WhichCPU                        ; Get CPU type in low word of D7
            bsr.w   WhichBoard                      ; Get logic board type in high word of D7
            cmpi.l  #wmStConst,WarmStart
            beq.b   .L1
            movea.l A5,A1
            movea.l SP,A0
            jsr     (RamTest,PC)
            movea.l SP,A1
            suba.l  A0,A0
            movea.l A5,SP
            jsr     (RamTest,PC)
            movea.l A1,SP
.L1:
            move.l  WarmStart,-(SP)
            lea     SysCom,A0                       ; A0 = pointer to start of system globals
            lea     HeapStart,A1                    ; A1 = pointer to end of system globals
            bsr.w   FillWithOnes                    ; Fill system globals with ones
            move.l  (SP)+,WarmStart
            move.b  D7,CPUFlag                      ; Save type of CPU we have
            swap    D7
            move.b  D7,WhichBox
            move.l  A6,MemTop                       ; Save the top of available memory
            lea     .L2,A6
            jmp     (SysErrInit,PC)
.L2:
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
BootRetry:
            move    #$2700,SR                       ; Disable interrupts
            movea.l VIA,A1
            move.b  #$4,(VIA_IER-VIA_Base,A1)
            bsr.w   InitGlobalVars                  ; Initialize a bunch of global variables
.P0         lea     (InitIntHandler-.P0).l,A0
            jsr     (.P0,PC,A0)                     ; Intialize interrupt vectors and dispatch table
            ; ^ Miscompare: (Original 4E BB 88 F8) - (Assembled 4E BB 80 F8)
            ; Location: 900134
            bsr.w   InitDispatcher
            bsr.w   GetPRAM
            bsr.w   InitMemMgr
            bsr.w   SetUpSysAppZone
            bsr.w   InitSwitcherTable
            bsr.w   InitPMgrVars
            bsr.w   InitRsrcMgr
.P1         lea     (NMInit-.P1).l,A0
            jsr     (.P1,PC,A0)
.P2         lea     (InitTimeMgr-.P2).l,A0
            jsr     (.P2,PC,A0)
            bsr.w   InitADBvars
            bsr.w   InitShutdownMgr
            bsr.w   InitDTQueue
            move    #$2000,SR
            bsr.w   InitVidGlobals
            bsr.w   CompBootStack
            movea.l A0,SP
            IF PortableAbs
            dc.l    $90FC2000                       ; Temp fix to prevent assembler optimization
            ELSE
            suba.w  #$2000,A0
            ENDIF
            _SetApplLimit
            lea     ($308).w,A1
            jsr     InitQueue
.P3         lea     (InitSCSIMgr-.P3).l,A0
            jsr     (.P3,PC,A0)
            bsr.w   InitIOMgr
.P4         lea     (InitADB-.P4).l,A0
            jsr     (.P4,PC,A0)
            bsr.w   InitCrsrMgr
            moveq   #$70,D0
            _NewPtrSysClear
            move.l  A0,ExpandMem
            movea.l A0,A4
            move.w  #$106,(A0)+
            move.l  #$70,(A0)
.P5         lea     InitGestalt-.P5,A0
            jsr     (.P5,PC,A0)
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
JmpTblInit:
            move.l  A0,D0
.JmpTbl2:
            moveq   #0,D2
            move.w  (A0)+,D2
            add.l   D0,D2
            move.l  D2,(A1)+
            dbf     D1,.JmpTbl2
            rts
            IF PortableAbs
            org     $900234
            ENDIF
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
            IF PortableAbs
            org     $900244
            ENDIF
CompBootStack:
            move.l  BufPtr,D0                       ; Get top of usable memory
            lsr.l   #1,D0                           ; Divide by two
            movea.l D0,A0
            suba.w  #$400,A0
            rts
            IF PortableAbs
            org     $900252
            ENDIF
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
            suba.w  #$2000,A0
            _SetApplLimit
            rts
SysHeap:
            dc.l    HeapStart
            dc.l    HeapStart+SysZoneSize
            dc.w    4*dfltMasters
            dc.l    0
            IF PortableAbs
            org     $900290
            ENDIF
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
            movea.l Video_Base,A0
            move.w  #hcVideoSize,D1
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
            IF PortableAbs
            org     $90031C
            ENDIF
InitShutdownMgr:
            clr.w   -(SP)
            _Shutdown
            rts
            IF PortableAbs
            org     $900322
            ENDIF
InitPMgrVars:
            IF PortableAbs
            dc.l    $203C0000                       ; Temp fix to prevent assembler optimization
            dc.w    $009E
            ELSE
            move.l  #158,D0
            ENDIF
            _NewPtrSysClear
            move.l  A0,PowerMgrVars
            movea.l A0,A2
            lea     (SleepQHdr,A2),A1
            jsr     InitQueue
            st      (TOdirtyFlag,A2)
            moveq   #1,D0
            move.l  D0,(LastAct,A2)
            move.l  D0,(LastHd,A2)
            move.b  #16,(SaveSpeedo,A2)
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
            move.l  $40070,D0
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
            IF PortableAbs
            org     $900416
            ENDIF
InitADBvars:
            move.l  #FBDBSize,D0                    ; Get local data area length
            _NewPtrSysClear                         ; Allocate space on heap and clear
            move.l  A0,ADBBase                      ; Save pointer to ADBBase
.P1         lea     (ADBProc-.P1).l,A0              ; Get the ADBProc
            lea     (.P1,PC,A0),A0
            move.l  A0,JADBProc                     ; Install it into JADBProc vector
.P2         lea     (FDBShiftInt_VIA2-.P2),A0
            lea     (.P2,PC,A0),A0
            move.l  A0,(Lvl1DT+8)
            rts
            IF PortableAbs
            org     $900440
            ENDIF
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
.P1         lea     (FSIODNETbl-.P1),A0             ; Point to the offset table
            lea     (.P1,PC,A0),A0
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
            jsr     InitQueue                       ; Initialize VBL queue header
            clr.l   Ticks                           ; Clear system tick count
            move.b  #$80,MBState                    ; Set current mouse button state to up
            clr.l   MBTicks                         ; Clear timestamp for mouse button
            clr.l   SysFontFam                      ; Clear SysFontFam and SysFontSize
            clr.l   WidthTabHandle
            clr.w   TESysJust
            clr.b   WordRedraw
            jsr     InitCrsrVars
            clr.w   SysVersion
            bclr.b  #0,AlarmState
.P2         lea     (NMGNEFilter-.P2),A0
            lea     (.P2,PC,A0),A0
            move.l  A0,GNEFilter
            clr.l   IAZNotify                       ; No InitApplZone notify proc
            move.w  #$FF7F,FlEvtMask                ; Init for disable of DIP flushes
            rts
            IF PortableAbs
            org     $900524
            ENDIF
SwitchGoodies:
            dc.l    $40004000
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
WDCBSWOS:
            dc.w    $28
PMPSWOS:
            dc.w    $2E
SwitchLen   EQU     *-SwitchGoodies
            ;$38
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
            lea     IllegalInstructionVector,A1
            move.l  (A1),-(SP)                      ; Save the vector
            lea     .ILException,A0                 
            move.l  A0,(A1)                         ; Replace it with our exception handler
            movea.l SP,A0
            clr.w   -(SP)
            moveq   #cpu68020,D7
            move.w  #$2909,D0
            dc.l    $4E7B0002
            ; ^ movec   D0,CACR
            dc.l    $4E7A0002
            ;^ movec   CACR,D0
            bclr.l  #8,D0                           ; Clear Enable Data Cache bit
            beq.b   .L2
            dc.l    $4E7B0002
            ; ^ movec   D0,CACR
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
            IF PortableAbs
            org     $9005F0
            ENDIF
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
.P1         lea     (VIAIntForTimeDBRA-.P1),A0
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


            IF PortableAbs
            org     $9009BA
            ENDIF
TMVectors:
            dc.l    $2000                           ; Initial stack pointer
            dc.l    StartTest1                      ; Initial program counter
            dc.l    BusError                        ; Bus error veector
            dc.l    AdrError                        ; Address error vector
            dc.l    IllError                        ; Illegal instruction error vector
            dc.l    ZerError
            dc.l    ChkError
            dc.l    TrapV
            dc.l    PrivError
            dc.l    Trace
            dc.l    LineA
            dc.l    LineF
            dc.l    Unassigned
            dc.l    CPProtocol
            dc.l    FormatX
            dc.l    SpurInterrupt
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    SpurInterrupt
            dc.l    IntLevel1
            dc.l    IntLevel2
            dc.l    IntLevel3
            dc.l    IntLevel4
            dc.l    IntLevel5
            dc.l    IntLevel6
            dc.l    IntLevel7
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    TrapInst
            dc.l    FPCP1
            dc.l    FPCP2
            dc.l    FPCP3
            dc.l    FPCP4
            dc.l    FPCP5
            dc.l    FPCP6
            dc.l    FPCP7
            dc.l    Unassigned
            dc.l    PMMUConfig
            dc.l    PMMUIllegal
            dc.l    PMMUAccess
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
            dc.l    Unassigned
BusError:
            btst.l  #beok,D7                        ; Are bus errors expected?
            beq.b   .L1
            movea.l A5,SP
            jmp     (A6)
.L1:
            ori.w   #BECode,D7
            bra.w   ExceptionHandler
AdrError:
            ori.w   #ADCode,D7
            bra.w   ExceptionHandler
IllError:
            ori.w   #ILCode,D7
            bra.w   ExceptionHandler
ZerError:
            ori.w   #ZDCode,D7
            bra.w   ExceptionHandler
ChkError:
            ori.w   #CICode,D7
            bra.w   ExceptionHandler

ExceptionHandler:
            bset.l  #excp,D7                        ; Set exception flag
            move.l  SP,D6                           ; Save stack pointer
            movea.w #$2000,SP                       ; Initialize stack pointer
            jmp     Error1Handler                   ; Handle error
            IF PortableAbs
            org     $900BBC
            ENDIF
StartTest1:
            movea.w #$2000,SP                       ; Initialize stack pointer
            moveq   #0,D7                           ; Clear flags register
            moveq   #0,D6                           ; Clear minor error register
            lea     TMVectors,A0                    ; Get address of vectors
            suba.l  A1,A1                           ; Clear A1
            move.w  #63,D0                          ; Number of vectors
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
            move.w  #powerCntl*$100+1,D0
            move.l  #(1<<pTurnOn)*$1000000,D1
            jmp     QuasiPwrMgr
.L1:
            move.w  A0,D6
            tst.l   D6
            bne.w   Error1Handler
            lea     .L2,A6
            move.w  #powerCntl*$100+1,D0
            move.l  #(1<<pTurnOn|1<<pMinus5V|1<<pSerDrvr|1<<pSCC|1<<pIWM)*1000000,D1
            jmp     QuasiPwrMgr
.L2:
            move.w  A0,D6
            tst.l   D6
            bne.w   Error1Handler
            move.w  #ErrROM,D7
            lea     .L3,A6
            jmp     StartUpROMTest
.L3:
            tst.l   D6
            bne.w   Error1Handler
            clr.w   D7
            move.w  #ErrPmgrSt,D7
            move.w  #PmgrSelfTest*$100+0,D0
            lea     .L4,A6
            jmp     QuasiPwrMgr
.L4:
            move.w  A0,D6
            swap    D6
            move.w  D1,D6
            swap    D6
            tst.l   D6
            bne.w   Error1Handler
            move.w  #ErrVidAddr,D7
            lea     .L5,A6
            jmp     VramAddrTest
.L5:
            tst.l   D6
            bne.w   Error1Handler
            move.w  #ErrVidRAM,D7
            lea     .L6,A6
            jmp     VramDataTest
.L6:
            tst.l   D6
            bne.w   Error1Handler
            lea     Sound_Base,SP
            move.w  #readINT*$100+0,D0
            lea     .L7,A6
            jmp     QuasiPwrMgr
.L7:
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
            lea     .L9,A6
            jmp     DataBusTest
.L9:
            tst.l   D6
            bne.w   Error1Handler
            move.w  #ErrSizeMem,D7
            lea     .L10,A6
            move.l  #$22BBF0E1,D0
            jmp     SizeMemory
.L10:
            move.w  #xPramRead*$100+2,D0
            move.l  #$46010000,D1
            lea     .L11,A6
            jmp     QuasiPwrMgr
.L11:
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


            IF PortableAbs
            org     $901B28
            ENDIF
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
            lea     .L5,A6
            jmp     QPM_Send
.L5:
            beq.b   .L6
            ror.w   #8,D0
            dbf     D2,.L1
            bra.w   .Exit
.L6:
            clr.w   D2
            rol.w   #8,D0
            move.b  D0,D2
            move.b  D0,D5
            lea     .L7,A6
            jmp     QPM_Send
.L7:
            bne.w   .Exit
            subq.w  #1,D2
            bmi.b   .L10
.L8:
            rol.l   #8,D1
            move.b  D1,D5
            lea     .L9,A6
            jmp     QPM_Send
.L9:
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
            lea     .L13,A6
            jmp     QPM_Receive
.L13:
            bne.b   .Exit                           ; Exit if there was an error
            move.b  D5,D0
            lea     .L14,A6
            jmp     QPM_Receive
.L14:
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
            lea     .L17,A6
            jmp     QPM_Receive
.L17:
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
            lea     .L1,A6
            jmp     QuasiPwrMgr
.L1:
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
            jmp     (A6)
StartUpROMTest:
            moveq   #0,D0
            moveq   #0,D1
            lea     BaseOfROM,A0
            move.l  (A0)+,D4                        ; Load expected checksum
            move.l  #ROMSize/2-4,D3
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
            jmp     (A6)
Mod3Test:
            





