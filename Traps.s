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

    macro _PMgrOp
        dc.w    $A085
    endm

    macro _IdleUpdate
        dc.w    $A285
    endm

    macro _NewPtrSysClear
        dc.w    $A71E
    endm

    macro _SetCursor
        dc.w    $A851
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
