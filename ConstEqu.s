RAMconfigInit   EQU         $6

sleepConst      EQU         'SARA'
wmStConst       EQU         'WLSC'

IWMInitMode     EQU         $17

TROMCode        EQU         $55AAAA55

powerCntl       EQU         $10
pMgrADBoff      EQU         $21                     ; Turn ADB auto-poll off
timeRead        EQU         $38                     ; Read the time from the clock chip
xPramRead       EQU         $3A                     ; Read extended PRAM
readINT         EQU         $78                     ; Get Power Manager interrupt data
PmgrSelfTest    EQU         $EC                     ; Run Power Manager self test

pIWM            EQU         0                       ; IWM power
pSCC            EQU         1                       ; SCC power
pHD             EQU         2                       ; SCSI hard drive power
pModem          EQU         3                       ; Modem power
pSerDrvr        EQU         4
pASC            EQU         5                       ; Sound power
pMinus5V        EQU         6
pTurnOn         EQU         7

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

PmgrPramBase    EQU         $70
SlpTimeOut      EQU         $70                     ; Sleep timeout, in seconds/15
HDTimeOut       EQU         $71                     ; Hard disk timeout, in seconds/15
PmgrStatusFlags EQU         $72
PmgrOtherFlags  EQU         $73
Brightness      EQU         $74                     ; Brightness level
VidMode         EQU         $75                     ; Video mode

DfltHDTime      EQU         4*60/15                 ; Default number of minutes before HD spin down timeout (4 min)
DfltSlpTime     EQU         8*60/15                 ; Default number of minutes before sleep timeout (8 min)

idlespeed       EQU         1                       ; Idle speed (1MHz equivalent)
CPUSpeed16MHz   EQU         16                      ; 16MHz CPU Speed

SndWFreq        EQU         60*10                   ; Sound Watch (SndWatch) VBL is called every 10 seconds
BatFreq         EQU         60*1                    ; Battery level monitor VBL is called every second

FBDBSize        EQU         370

hcVideoSize     EQU         8000

boxSE           EQU         $FF
boxPortable     EQU         4

cpu68000        EQU         0
cpu68010        EQU         1
cpu68020        EQU         2
cpu68030        EQU         3

PMreq           EQU         0                       ; Power manager handshake request
PMack           EQU         1
TestJumper      EQU         2                       ; Test jumper
SyncM           EQU         3                       ; Synchronous modem support
                                                    ; 0 = original Macintosh configuration
                                                    ; 1 = synchronous modem support, SCC channel A

ErrROM          EQU         $0001
ErrVIA1         EQU         $0006
ErrSCSI         EQU         $000B
ErrIWM          EQU         $000C
ErrData         EQU         $000E
ErrPmgrSt       EQU         $0010
ErrSizeMem      EQU         $0011
ErrPmgrTurnOn   EQU         $0014
ErrVidRAM       EQU         $0082
ErrVidAddr      EQU         $0083

BECode          EQU         $0100                   ; Bus error exception code
ADCode          EQU         $0200                   ; Address error exception code
ILCode          EQU         $0300                   ; Illegal instruction error exception code
ZDCode          EQU         $0400                   ; Zero divide error exception code
CICode          EQU         $0500
TPCode          EQU         $0600
PVCode          EQU         $0700

excp            EQU         24
test            EQU         26
beok            EQU         27

oneSecIntFlag   EQU         27
