RAMconfigInit   EQU         $6

sleepConst      EQU         'SARA'
wmStConst       EQU         'WLSC'

IWMInitMode     EQU         $17

TROMCode        EQU         $55AAAA55

powerCntl       EQU         $10

bkLim           EQU         $0                      ; Last block in zone [pointer}

SysZoneSize     EQU         $1800                   ; Default System Zone size

dfltMasters     EQU         32

SleepTime       EQU         $0
HDTime          EQU         $1
SleepQHdr       EQU         $2
SlpQFlags       EQU         $2
SlpQHead        EQU         $4
SaveSpeedo      EQU         $E
TOdirtyFlag     EQU         $11
LastAct         EQU         $1A
LastHd          EQU         $1E
LowWarn         EQU         $22
Cutoff          EQU         $23
NTSCcopy        EQU         $53
vBatInt         EQU         $64
vEnvInt         EQU         $68
BatVBLTask      EQU         $82
SwVBLTask       EQU         $90

pMgrADBoff      EQU         $21

PmgrPramBase    EQU         $70
SlpTimeOut      EQU         $70
HDTimeOut       EQU         $71
PmgrStatusFlags EQU         $72
PmgrOtherFlags  EQU         $73
Brightness      EQU         $74
VidMode         EQU         $75

DfltHDTime      EQU         4*60/15
DfltSlpTime     EQU         8*60/15

idlespeed       EQU         1
CPUSpeed16MHz   EQU         16

SndWFreq        EQU         60*10
BatFreq         EQU         60*1

FBDBSize        EQU         370

hcVideoSize     EQU         8000
