            INCLUDE 'HardwareEqu.s'
            INCLUDE 'ConstEqu.s'
            INCLUDE 'Orphan.s'

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
ResetEntry  jmp     StartBoot
            dc.l    0
StartBoot: