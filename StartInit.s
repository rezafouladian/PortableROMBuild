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
            movem.l TROM2,D0/A0
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
.P1         lea     (ADBProc-.P1).l,A0                  ; Get the ADBProc
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
            move.w  #-$1,PWMValue
            movea.l A5,A0
            suba.w  #$2FF,A0
            move.l  A0,PWMBuf1
            move.l  A0,PWMBuf2
            subq.w  #1,A0
            move.l  A0,SoundBase
            move.l  A0,BufPtr
            rts
            IF PortableAbs
            org     $900460
            ENDIF
InitGlobalVars:
            lea     BaseOfROM,A0
            move.l  A0,ROMBase
            moveq   #-1,D0
            move.l  D0,SMGlobals
            move.l  #Sound_Base,ASCBase
            move.b  #$7F,ROM85
            move.w  #$C500,HWCfgFlags
            move.l  #$10001,OneOne
            moveq   #-1,D0
            move.l  D0,MinusOne                     ; Setup MinusOne
            bsr.w   InitSCCGlobals
            bsr.w   InitIWMGlobals
            bsr.w   InitVIAGlobals
            bsr.w   InitSCSIGlobals
            clr.l   DSAlertTab
            move.w  MinusOne,FSFCBLen
.P1         lea     (FSIODNETbl-.P1),A0
            lea     (.P1,PC,A0),A0
            lea     $8F4,A1
            moveq   #2,D1
            bsr.w   JmpTblInit
            clr.b   DskVerify
            clr.b   LoadTrap
            clr.b   MmInOK
            clr.w   SysEvtMask
            clr.l   JKybdTask
            clr.l   StkLowPt
            lea     $160,A1
            jsr     InitQueue
            clr.l   Ticks
            move.b  #-$80,MBState
            clr.l   MBTicks
            clr.l   SysFontFam
            clr.l   WidthTabHandle
            clr.w   TESysJust
            clr.b   WordRedraw
            jsr     InitCrsrVars
            clr.w   SysVersion
            bclr.b  #0,AlarmState
.P2         lea     (NMGNEFilter-.P2),A0
            lea     (.P2,PC,A0),A0
            move.l  A0,GNEFilter
            clr.l   IAZNotify
            move.w  #-$81,DiskVars_FlEvtMask
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
InitSwitcherTable:
            moveq   #$38,D0
            _NewPtrSysClear
            movea.l A0,A1
            lea     SwitchGoodies,A0
            moveq   #$38,D0
            _BlockMove
            move.l  A1,SwitcherTPtr
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
WhichCPU:
            lea     $10,A1
            move.l  (A1),-(SP)
            lea     .L1,A0
            move.l  A0,(A1)
            movea.l SP,A0
            clr.w   -(SP)
            moveq   #2,D7
            move.w  #$2909,D0
            dc.l    $4E7B0002
            ; ^ movec   D0,CACR
            dc.l    $4E7A0002
            ;^ movec   CACR,D0
            bclr.l  #8,D0
            beq.b   .L2
            dc.l    $4E7B0002
            ; ^ movec   D0,CACR
            moveq   #3,D7
            bra.b   .L2
.L1:
            moveq   #1,D7
            cmpi.w  #$10,($6,SP)
            beq.b   .L2
            moveq   #0,D7
.L2:
            movea.l A0,SP
            move.l  (SP)+,(A1)
            rts

            IF PortableAbs
            org     $9005F0
            ENDIF
WhichBoard:
            swap    D7
            clr.w   D7
            move.b  #-1,D7
            lea     .L1,A0
            move.l  A0,D1
            btst.l  #23,D1
            beq.b   .L1
            move.b  #4,D7
.L1:
            swap    D7
            rts
            IF PortableAbs
            org     $90060C
            ENDIF
SetUpTimeK:
            move    SR,-(SP)

