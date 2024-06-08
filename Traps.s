    macro _Open
        dc.w    $A000
    endm

    macro _Read
        dc.w    $A002
    endm
    
    macro _Control
        dc.w    $A004
    endm

    macro _InitZone
        dc.w    $A019
    endm
    
    macro _SetApplLimit
        dc.w    $A02D
    endm

    macro _BlockMove
        dc.w    $A02E
    endm

    macro _VInstall
        dc.w    $A033
    endm

    macro _VRemove
        dc.w    $A034
    endm

    macro _InitUtil
        dc.w    $A03F
    endm

    macro _ReadXPRam
        dc.w    $A051
    endm

    macro _WriteXPRam
        dc.w    $A052
    endm

    macro _SetApplBase
        dc.w    $A057
    endm

    macro _InitFS
        dc.w    $A06C
    endm

    macro _InitEvents
        dc.w    $A06D
    endm
    
    macro _GetDefaultStartup
        dc.w    $A07D
    endm

    macro _PMgrOp
        dc.w    $A085
    endm

    macro _IdleUpdate
        dc.w    $A285
    endm

    macro _NewPtrSys
        dc.w    $A51E
    endm

    macro _NewPtrSysClear
        dc.w    $A71E
    endm

    macro _CopyMask
        dc.w    $A817
    endm

    macro _SetCursor
        dc.w    $A851
    endm

    macro _HideCursor
        dc.w    $A852
    endm

    macro _InitGraf
        dc.w    $A86E
    endm

    macro _OpenPort
        dc.w    $A86F
    endm

    macro _Shutdown
        dc.w    $A895
    endm

    macro _PenSize
        dc.w    $A89B
    endm

    macro _PenNormal
        dc.w    $A89E
    endm

    macro _FillRect
        dc.w    $A8A5
    endm

    macro _InsetRect
        dc.w    $A8A9
    endm

    macro _FrameRoundRect
        dc.w    $A8B0
    endm

    macro _PaintRoundRect
        dc.w    $A8B1
    endm

    macro _FillRoundRect
        dc.w    $A8B4
    endm

    macro _InitResources
        dc.w    $A995
    endm

    macro _GetResource
        dc.w    $A9A0
    endm

    macro _InternalWait
        dc.w    $A07F
    endm

    macro _GetTimeOut
        suba.l A0,A0
        _InternalWait
    endm

    macro _SetTimeOut
        movea.w #1,A0
        _InternalWait
    endm

    macro _GetWaitFlags
        movea.w #2,A0
        _InternalWait
    endm

    macro _SetWaitFlags
        movea.w #3,A0
        _InternalWait
    endm

    macro _DisableDynWait
        movea.w #4,A0
        _InternalWait
    endm

    macro _EnableDynWait
        movea.w $5,A0
        _InternalWait
    endm

    macro _DisablePermWait
        movea.w #6,A0
        _InternalWait
    endm

    macro _EnablePermWait
        movea.w #7,A0
        _InternalWait
    endm