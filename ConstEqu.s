RAMconfigInit   EQU         $6

sleepConst      EQU         'SARA'
wmStConst       EQU         'WLSC'

IWMInitMode     EQU         $17

TROMCode        EQU         $55AAAA55

powerCntl       EQU         $10
timeRead        EQU         $38                     ; Read the time from the clock chip

bkLim           EQU         $0                      ; Last block in zone [pointer}

SysZoneSize     EQU         $1800                   ; Default System Zone size

dfltMasters     EQU         32

SleepTime       EQU         $0                      ; Copies of sleep timeout values
HDTime          EQU         $1                      ; Hard disk timeout
SleepQHdr       EQU         $2                      ; Sleep queue header
SlpQFlags       EQU         $2                      ; Sleep queue flags
SlpQHead        EQU         $4                      ; First sleep queue entry
SaveSpeedo      EQU         $E                      ; Current CPU speed
TOdirtyFlag     EQU         $11                     ; Timeouts are dirtry flag
LastAct         EQU         $1A                     ; Last user activity
LastHd          EQU         $1E                     ; Last hard disk activity
LowWarn         EQU         $22                     ; Low power warning threshold
Cutoff          EQU         $23                     ; Power manager cutoff voltage
NTSCcopy        EQU         $53                     ; Copy of NTSC byte, only used from restart to DrawBeepScreen
vBatInt         EQU         $64                     ; Vector to battery interrupt handler
vEnvInt         EQU         $68                     ; Vector to environment interrupt handler
BatVBLTask      EQU         $82                     ; Battery monitor VBL task
SwVBLTask       EQU         $90                     ; Sound Watch (SndWatch) VBL task

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
