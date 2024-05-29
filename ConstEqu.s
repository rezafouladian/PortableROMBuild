ROMSize         EQU         $40000

RAMconfigInit   EQU         $6

sleepConst      EQU         'SARA'
wmStConst       EQU         'WLSC'

IWMInitMode     EQU         $17

TROMCode        EQU         $55AAAA55

sleepSig        EQU         'MATT'

powerCntl       EQU         $10
pMgrADBoff      EQU         $21                     ; Turn ADB auto-poll off
xPramWrite      EQU         $32
timeRead        EQU         $38                     ; Read the time from the clock chip
xPramRead       EQU         $3A                     ; Read extended PRAM
batteryRead     EQU         $68
readINT         EQU         $78                     ; Get Power Manager interrupt data
sleepReq        EQU         $7F
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

hcVideoSize     EQU         $8000
VideoWidth      EQU         640
VideoHeight     EQU         400
NTSCMaxX        EQU         512
NTSCOffset      EQU         8
LCDmode         EQU         0
Mac2Mode        EQU         1
NTSCmode        EQU         2

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
ErrRAMA         EQU         $0003
ErrAddr         EQU         $0005
ErrVIA1         EQU         $0006
ErrRAMC         EQU         $0008
ErrSCSI         EQU         $000B
ErrIWM          EQU         $000C
ErrData         EQU         $000E
ErrPmgrSt       EQU         $0010
ErrSizeMem      EQU         $0011
ErrPmgrTurnOn   EQU         $0014
ErrVidRAM       EQU         $0082
ErrVidAddr      EQU         $0083
ErrSCCReg       EQU         $0084
ErrSCCLoop      EQU         $0085
ErrSCCTimer     EQU         $0086
ErrVIATest      EQU         $0087
ErrASCTest      EQU         $0089
ErrPramTest     EQU         $008A

BECode          EQU         $0100                   ; Bus error exception code
ADCode          EQU         $0200                   ; Address error exception code
ILCode          EQU         $0300                   ; Illegal instruction error exception code
ZDCode          EQU         $0400                   ; Zero divide error exception code
CICode          EQU         $0500
TPCode          EQU         $0600
PVCode          EQU         $0700
TECode          EQU         $0800
ATCode          EQU         $0900
FTCode          EQU         $0A00
UNCode          EQU         $0B00
CPCode          EQU         $0C00
FMCode          EQU         $0D00
SICode          EQU         $0E00
TNCode          EQU         $0F00
L1Code          EQU         $1000
L2Code          EQU         $1100
L3Code          EQU         $1200
L4Code          EQU         $1300
L5Code          EQU         $1400
L6Code          EQU         $1500
L7Code          EQU         $1600
F1Code          EQU         $1700
F2Code          EQU         $1800
F3Code          EQU         $1900
F4Code          EQU         $1A00
F5Code          EQU         $1B00
F6Code          EQU         $1C00
F7Code          EQU         $1D00
PCCode          EQU         $1E00
PICode          EQU         $1F00
PACode          EQU         $2000

sec             EQU         $C

MsgQ            EQU         16
SCCok           EQU         17
nosleep         EQU         18
star            EQU         19
aski            EQU         20
echo            EQU         21
timer           EQU         22
crlf            EQU         23
excp            EQU         24
nmi             EQU         25
test            EQU         26
beok            EQU         27
stop            EQU         28
loop            EQU         29
pram            EQU         30
boot            EQU         31

oneSecIntFlag   EQU         27

RxCA            EQU         0                       ; SCC Receive Character Available

BootStackSize   EQU         8*1024

ADBCount        EQU         0
MaxCnt          EQU         ADBCount+2
Err7            EQU         MaxCnt+2
Err6            EQU         Err7+2
Err5            EQU         Err6+2
Err4            EQU         Err5+2
Err3            EQU         Err4+2
Err2            EQU         Err3+2
Err1            EQU         Err2+2
Error           EQU         Err1+2
GSize           EQU         Error+2
TotalSize       EQU         GSize+8