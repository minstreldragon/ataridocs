; Atari Disk Drive 810
; ROM Revision C

cmdPtr = $01e8

; FDC 1771 (Floppy Disk Controller):
; FDC controller uses an inverted data bus!

zpFcCommand = $00
zpFcStatus = $00
zpFcTrack = $01
zpFcSector = $02
zpFcData = $03

zpSkewTablePtr = $e1
zpFormatSector = $e2
zpFormatTrack = $e3

; RAM 6810:
; $0080-$00ff: RAM on 6810

; $0180-$01ff: RAM on PIA 6532
cmdDeviceId = $0181
cmdCommand = $0182
cmdAux1 = $0183
cmdAux2 = $0184
cmdChecksum = $0185
driveStepper = $01cd            ; stepper values on Port B
driveCurTrack = $01ce           ; track of current head position

driveStatus = $01d0


PiaPortA = $0380                ; Port A Data
PiaDdrA = $0381                 ; Data Direction Register for Port A
PiaPortB = $0382                ; Port B Data
PiaDdrB = $0383                 ; Data Direction Register for Port B
PiaTimerRead = $039e
PiaTimerWrite = $039f

; PA0: Drive Code Number 0
; PA1: Drive Motor
; PA2: Drive Code Number 1
; PA3: not connected
; PA4: Write Protect sensor
; PA5: not connected
; PA6: IRQ Floppy Disk Controller
; PA7: DRQ Floppy Disk Controller

; PB0: Host Interface (Data In to Computer)
; PB1: Host Interface
; PB2: Stepper Motor 0
; PB3: Stepper Motor 1
; PB4: Stepper Motor 2
; PB5: Stepper Motor 3
; PB6: Host Interface (/Command)
; PB7: Host Interface (Data Out of Computer) available = 0

        .word $0800
        * = $0800

l800    cld
        lda #$0a
        sta PiaPortA            ; port A: drive motor output, others all input
        sta PiaDdrA             ; port A: start drive motor
        ldx #$3c
        stx PiaDdrB             ; port B: stepper motor pins output, others input
        stx $0384
        ldx #$00
        stx PiaPortB            ; port B: set stepper motor to %0000

        lda #$2f                ; command: Force Interrupt (11010000)
        sta zpFcCommand
        lda #$05
        sta $01dd
        lda #$e0
        sta $01d2
        lda #$00
        sta $01d3
        jsr lfcd

        ldy #$27            ; perform 39 half steps
        jsr lc8b
        sty $01             ; $01 = 0 (track?)
        dey
        sty zpFcSector      ; $02 = -1
        jsr lb2c
        jsr lc30
        ldx #$01
        stx $01eb

l841    ldx #$ff
        txs
        lda #$d8
        jsr setStepperParams
        sty $01cf

l84c    lda #$ff                ; command: Restore (00000000)
        sta zpFcCommand
        lda #$20
        jmp l880

        .byt $00,$00,$00
        .byt $00,$00,$00
        .byt $00,$00,$00
        .byt $00,$00,$00
        .byt $00,$00,$00
        l866 = * + 2
        .byt $00,$00,$00
        .byt $00,$00,$00
        .byt $00,$00,$00
        .byt $00,$00

sectorSkewTable
$086f
                                                ; first sector: $ed = $12
        .byt                             $fe    ;                                    $01
        .byt $fc,$fa,$f8,$f6,$f4,$f2,$f0,$ee    ; $03, $05, $07, $09, $0b, $0d, $0f, $11
        .byt $fd,$fb,$f9,$f7,$f5,$f3,$f1,$ef    ; $02, $04, $06, $08, $0a, $0c, $0e, $10

l880    jsr lb1e
l883    bit PiaPortA
        bvs l88e
l888    jsr lcab
        jmp l883
l88e    dec $01eb
        bpl l84c
l893    ldx #$80
        ldy $01cf
        beq l8aa
l89a    jsr _stepHead
        sty $01cf
        jsr lcab
l8a3    lda $0395
        bpl l8a3
l8a8    bmi l893
l8aa    lda #$08
        sta PiaPortA
        lda #$00
        sta PiaPortB
        lda #$df
        jsr lb25
        lda #$04
        jsr lb1e
        jmp lcab
l8c1    ldx cmdAux2
        lda cmdAux1
        beq l8d3
l8c9    cpx #$03
        bcs l8d7
l8cd    cmp #$d1
        bcs l8d9
l8d1    clc
        rts
l8d3    cpx #$00
        bne l8c9
l8d7    sec
        rts
l8d9    cpx #$02
        beq l8d7
l8dd    jmp l8d1
        .asc ""
        .byt $60

_wait
l8e1    sta $01df
_waitL1
        dey
        bne _waitL1
        dec $01df
        bne _waitL1
        rts

_stepHead
l8ed    lda #$29
        sta $039e
        lda PiaPortB            ; Port B
        and #$3c                ; stepper motor driver pins
        inx                     ; test stepper direction
        bmi _stepHeadDown
_stepHeadUp
l8fa    inc driveCurTrack
        lsr                     ; step up
        sta driveStepper
        and #$02                ; stepper value %0000(1) (too small)?
        beq _stepHeadJ1         ; no ->
        lda #$20                ; set stepper value to %1000
        ora driveStepper
        bpl _stepHeadJ2
_stepHeadDown
l90c    dec driveCurTrack
        asl                     ; step down
        sta driveStepper
        and #$40                ; stepper value %10000 (too large)?
        beq _stepHeadJ1         ; no ->
        lda #$04                ; set stepper value to %0001
        ora driveStepper
        bpl _stepHeadJ2
_stepHeadJ1
        lda driveStepper
_stepHeadJ2
        sta PiaPortB            ; Port B (drive stepper motor)
        dey                     ; dec. number of steps
        rts

l926    jsr l8c1
        bcs l96c
l92b    jsr lc77
        jsr lc43                ; issue command: seek
        jsr lfcd
        jsr lf4b                ; (step to track? set track, sector in FDC)
l937    lda #$77                ; command: Read Command (10001000)
        sta zpFcCommand
l93b    bvs l94c                ; IRQ flag set? ->
        lda $039c
_readSectorL2
        bit PiaPortA            ; DRQ flag set?
        bpl l93b
        lda zpFcData            ; read data byte from floppy controller
        inx
        sta $80,x               ; store byte in RAM buffer
        bpl _readSectorL2
l94c    jsr l970
        bcs l965
l951    lda #$43
l953    sta $0186
        jsr checkSectorSum
        sta $0187
        jsr _setPiaPortBtoWrite
        jsr le0f
        jmp l841
l965    bne l937
l967    lda #$45
        jmp l953
l96c    jmp ldc3
l96f    .byt $ad
        
l970 = * + 1


l970    lda $00
        sta $01d1
        sta $01d1
        eor #$ff
        bne l984
l979    clc
        lda #$fb
        jsr lb2e
l97f    jsr lf68
        dey
        rts

l984    cpy #$02
        bne l9a3
l988    and #$10
        beq l9a3
l98c    sty $01df
        jsr lfcd
        ldy #$2b                ; perform 43 half steps
        jsr lc8b
        lda $01da               ; track?
        jsr setStepperParams
        jsr lf18
        ldy $01df
l9a3    lda #$04
        jsr lb09
        sec
        jmp l97f


_formatDisk
l9ac    jsr lc77
        jsr lc43                ; issue command: seek
        jsr lfcd
        ldy #$2b                ; perform 43 half steps
        jsr lc8b
        jsr lf63
        lda PiaPortA            ; Port A
        and #$10                ; Write Protect Sensor
        beq l9cc
        lda #$08
        jsr lb09
        jmp la95
l9cc    lda #$ff                ; inverted 0x00 = Track 0
        sta zpFormatTrack
        jsr _formatTrack
l9d3    dec zpFormatTrack
        lda zpFormatTrack
        cmp #$d7                ; inverted 0x28 = Track 40
        beq l9e3
l9db    ldx #$80
        jsr l9e8
        jmp l9d3
l9e3    jmp laa3
l9e6    bcs l9e8

l9e8    jsr lb6d                ; wait for head to move

_formatTrack
l9eb
        lda #<sectorSkewTable-1
        sta $e0
        lda #>sectorSkewTable-1
        sta $e1
        lda $0394
        lda #$ed                ; firstr sector: 18 ($12 xor $ff)
        sta zpFormatSector
        ldy #$ff
        ldx #$ff
        lda #$0b                ; command: Write Track (11110100)
        sta zpFcCommand
        jsr la8d
        lda #$0d
        sta $039f
        ldy #$ff                ; write 255 times 0x00
        jsr la80                ; write data X with y repetitions
        stx $039f
        ldy #$03                ; 0xfc = Index Address Mark (xor 0xff)
        jsr la8d                ; write Index Address Mark
        ldy #$0b                ; write 11 times 0x00
        jsr la80                ; write data X with y repetitions
_formatSectorL1
la1c    ldy #$06                ; write 6 times
        jsr la80                ; write data X with y repetitions
        ldy #$01                ; 0xfe = ID Address Mark (xor 0xff)
        jsr la8d                ; write ID Address Mark
        ldy zpFormatTrack       ; track (inverted)
        jsr la8d                ; write track number
        ldy #$ff                ; 0x00
        jsr la8d                ; write a zero byte
        ldy zpFormatSector      ; sector (inverted)
        jsr la8d                ; write sector number
        ldy #$ff                ; 0x00
        jsr la8d                ; write a zero byte
        ldy #$08                ; 0xf7 (CRC byte 2)
        jsr la8d                ; write CRC byte 2
        ldy #$11                ; number of repetitions: 11
        jsr la80                ; write data X with y repetitions
        ldy #$04                ; 0xfb = Data Address Mark
        jsr la8d                ; write Data Address Mark
        ldy #$80                ; # of bytes to write: 128
        ldx #$00                ; 0xff
        jsr la80                ; write 128 times value 0xff
        ldy #$08                ; 0xf7 (CRC byte 2)
        jsr la8d                ; write CRC byte
        ldy #$09                ; number of repetitions: 9
        ldx #$ff                ; 0x00
        jsr la80                ; write data X with y repetitions
        jsr la8d                ; write one more 0x00(?)
        inc $e0                 ; inc. sector skew table pointer
        bmi la79
la63    jsr la8d
        lda ($e1,x)             ; load from ($e1+$ff) = ($e0) = $086e, $086f, $0870
        sta zpFormatSector
        jmp la6f
la6d    bvs la95
la6f    bit PiaPortA
        bpl la6d
la74    sty zpFcData
        jmp _formatSectorL1
la79    lda #$2f                ; command: Force Interrupt (11010000)
        sta zpFcCommand
        rts
la7e    bvs la95

la80    bit PiaPortA
        bpl la7e
la85    stx zpFcData
        dey
        bne la80
la8a    rts

la8b    bvs la95
la8d    bit PiaPortA            ; wait for DRQ from FDC
        bpl la8b
la92    sty zpFcData
        rts

                                ; report error?
la95    ldx #$ff
        stx $80
        stx $81
        lda zpFcStatus
        sta $01d1
        jmp le71
laa3    ldx #$00
        stx $01e1
laa8    lda driveCurTrack
        sta zpFcTrack
        lda #$fe
laaf    sta $01db               ; sector?
        sta zpFcSector
        ldy #$02
        sty $01e0
lab9    jsr lf81
        ldy #$01
        jsr l970
        bcs laf3
lac3    lda $01db               ; sector?
        sec
        sbc #$04
        cmp #$ea
        bne lacf
lacd    lda #$fb                ; (4)
lacf    cmp #$eb
        bne lad5
lad3    lda #$fc                ; (3)
lad5    cmp #$ec
        bne ladb
lad9    lda #$fd                ; (2)
ladb    cmp #$e9
        bne laaf
ladf    lda driveCurTrack
        cmp #$ff
        beq laf0
lae6    ldx #$00
        ldy #$01
        jsr lf18
        jmp laa8
laf0    jmp le58
laf3    dec $01e0
        bne lab9
laf8    ldx $01e1
        jmp le79

_startStopMotor
lafe    lda PiaPortA
        lsr
        lsr                     ; Drive Motor control into carry
        lda #$10
        bcs lb09
        lda #$00

_updateDriveStatus
lb09    ora $01d0
        sta $01d0
        rts

_setDriveStatus
lb10    pha
        lda #$00
        and $01d0
        sta $01d0
        pla
        jmp lb09

        .byt $60

lb1e    ora $01dd
        sta $01dd
        rts

lb25    and $01dd
        sta $01dd
        rts

lb2c    lda #$08
lb2e    and $01d0
        sta $01d0
        rts

lb35    lda PiaPortB            ; Port B
        bpl lb35
lb3a    txa                     ; write index
        beq lb67                ; if index = 0 -> done
        lda $0180,x             ; invert previously read byte
        eor #$ff
        sta $0180,x
lb45    nop
        nop
        nop
        ldy #$78                ; index: read 8 bit
lb4a    nop
        nop
        nop
        nop
        lda PiaPortB            ; read bit from serial IF to computer
        rol                     ; rotate data bit into carry (data in buffer)
        ror $0181,x             ; rotate data bit into buffer
        iny                     ; repeat for 8 bits
        bpl lb4a
        inx                     ; inc buffer pointer
        cpx $01de               ; all bytes read?
        bne lb35                ; no ->
        lda $0180,x
        eor #$ff
        sta $0180,x
        rts

lb67    nop
        nop
        nop
        jmp lb45

lb6d    lda #$02
        jsr _wait               ; wait $200
        jsr _stepHead
        rts

uploadSectorData
lb76    ldx #$00
        ldy #$07
        sty $01de
_uploadSectorDataL1
        lda PiaPortB            ; Port B
        bpl _uploadSectorDataL1
        txa
        beq lbb1                ; first byte? ->
        lda $7f,x
        eor #$ff                ; invert previously read byte
        sta $7f,x
lb8b    nop
        nop
        nop
        ldy #$78                ; index: read 8 bit
_uploadSectorDataL2
        nop
        nop
        nop
        lda $80
        lda PiaPortB            ; read bit from serial IF to computer
        rol                     ; rotate data bit into carry (data in buffer)
        ror $80,x               ; rotate data bit into buffer
        iny                     ; repeat for 8 bits
        bpl _uploadSectorDataL2
        inx                     ; inc buffer pointer
        bpl lbad                ; more bytes to read? ->
        ldx #$06
        jsr lb35                ; read byte to $0187 (checksum?)
        lda $f8,x
        eor #$ff                ; invert last byte in sector buffer
        sta $f8,x
        rts

lbad    nop
        jmp _uploadSectorDataL1
lbb1    nop
        nop
        nop
        jmp lb8b

_checkChecksum
lbb7    lda #$00                ; initialize checksum = 0
        clc                     ; start adding with carry = 0
_calcChecksumL1
        adc $0181,x             ; checksum += data byte
        php                     ; save carry
        inx                     ; inc. buffer offset
        cpx $01de               ; max offset reached?
        beq lbc8                ; yes ->
        plp                     ; restore carry
        jmp _calcChecksumL1
lbc8
        plp                     ; restore carry
        adc #$00                ; add final carry
        cmp $0181,x             ; compare with stored checksum
        rts

calcTrackSector
lbcf    lda cmdAux2             ; cmdAux2: total sector number, high byte
        sta $01d7
        lda cmdAux1             ; cmdAux1: total sector number, low byte
        sta $01d8
        ldx #$04                ; calculate: track  = sector number / 18
_calcTrkSecL1
        ror cmdAux2             ;            sector = sector number % 18
        ror
        dex
        bne _calcTrkSecL1
        ror cmdAux2

        ldx #$04
_calcTrkSecL2
        lsr cmdAux2
        dex
        bne _calcTrkSecL2

_calcTrkSecJ1
        asl
        cmp cmdAux2
        bcs _calcTrkSecJ2
        sta cmdAux1
        lsr
        eor #$ff                ; invert for FDC
        sta $01da               ; fdc track
        sec
        lda cmdAux2
        sbc cmdAux1
        eor #$ff                ; invert for FDC
        sta $01db               ; fdc sector
        rts

_calcTrkSecJ2
        lsr
        tax
        dex                     ; store A in X
        lda #$10
        clc
        adc cmdAux2
        sta cmdAux2
        txa                     ; restore A from X
        jmp _calcTrkSecJ1


setStepperParams
lc1b    ldx #$00                ; init stepper direction: up
        ldy #$00                ; init steps count
        sec
        sbc driveCurTrack       ; destination track - current track
        beq _setStpprParamsJ2   ; already at destination ->
        bpl _setStpprParamsJ1   ; stepping up ->
        ldx #$80                ; stepper direction: down
        eor #$ff
        clc                     ; invert steps count
        adc #$01
_setStpprParamsJ1
        tay
_setStpprParamsJ2
        rts

lc30    lda PiaPortA
        and #$10                ; write protect sensor
        beq lc3d
lc37    lda #$08
        jsr lb09
        rts
lc3d    lda #$f7
        jsr lb2e
        rts

lc43    lda PiaPortA
        lsr
        lsr                     ; Drive Motor into carry
        bcs lc59
        lda #$0a                ; drive motor on
        sta PiaPortA
        sta zpFcData
        lda #$16
        sta zpFcTrack
        lda #$ec                ; Command: Seek (00010011)
        sta zpFcCommand
lc59    lda #$01
        sta $01eb
        rts

lc5f    lda $01dd
        lsr
        lsr
        lsr
        bcc lc71
lc67    bit PiaPortA
        bvc lc67
lc6c    lda #$fb
        jsr lb25
lc71    lda #$10
        jsr lb10
        rts

lc77    bit PiaPortB            ; wait for /COMMAND from computer
        bvs lc77
sendSioAck
lc7c    lda #$41                ; 'A' (ACK) Acknowledge

sendSioByte
lc7e    sta $0186
        jsr _setPiaPortBtoWrite ; inc. $0383
        jsr le2f                ; send byte $0186 over SIO interface
        jsr _setPiaPortBtoRead  ; dec. $0383
        rts

lc8b    ldx #$24
        stx PiaPortB
lc90    jsr _stepHead
        bmi lc9d                ; finished with stepping? ->
lc95    lda $0395               ; wait for ???
        bpl lc95
        jmp lc90

lc9d    sty driveCurTrack
        txa
        ldx #$14                ; busy wait $1400
lca3    dey
        bne lca3
lca6    dex
        bne lca3
lca9    tax
        rts

lcab    lda PiaPortB
        lsr
        lsr
        bcs lce4
lcb2    lda $01dd
        lsr
        bcs lcd7
lcb8    bit PiaPortB
        bvc lccc
lcbd    bit $01dd
        bvc lcec
lcc2    lda #$29                ; decimal: 41
        jsr _wait               ; wait for 2900 cycles
        bit PiaPortB
        bvs lcec
lccc    lda $01dd
        and #$60
        bne lcd6
lcd3    jmp lcab
lcd6    rts

lcd7    lda #$29                ; decimal: 41
        jsr _wait
        lda #$fe
        jsr lb25
        jmp lcab

lce4    lda #$01
        jsr lb1e
        jmp lccc

lcec    lda #$80
        jsr lb1e
        ldx #$00                ; start dest. offset = 0
        lda #$05                ; end offset = 5
        sta $01de
        jsr lb35                ; read $0181-$0185 from computer
        ldx #$00                ; buffer offset: 0
        dec $01de               ; reduce end offset by checksum byte
        jsr _checkChecksum
        beq ld0a                ; checksum OK ->
ld05    lda #$02
        jsr lb1e
ld0a    ldx #$34                ; drive number: '4'
        lda PiaPortA            ; Port A
        and #$05                ; mask drive code
        beq _detectDriveNum3    ; drive code 00 ->
ld13    cmp #$04
        beq _detectDriveNum2    ; drive code 01 ->
ld17    cmp #$01
        beq _detectDriveNum4    ; drive code 10 ->
_detectDriveNum1
        dex
_detectDriveNum2
        dex
_detectDriveNum3
        dex
_detectDriveNum4
        cpx cmdDeviceId         ; compare with requested drive
        beq ld30                ; this drive is requested ->
ld23    bit PiaPortB
        bvs ld23
ld28    lda #$7d
        jsr lb25
        jmp lcab

ld30    jsr ld95
        jmp lcab

ld36    jsr sendSioAck
        ldx #$4f                ; buffer offset: 79
        ldy #$53                ; end offset: 83
        sty $01de
        jsr _checkChecksum
        sta $01d4
        lda #$43
        sta $0186
        jsr _setPiaPortBtoWrite
        ldx #$7a
        jsr le2f                ; send byte $0186 over SIO interface
ld53    ldy $0155,x
        sty $0186
        jsr le32
        bpl ld53
ld5e    jsr _setPiaPortBtoRead
        jsr lb2c
        jsr lafe
        jmp l841
ld6a    ldy #$00
        sty $01e4
        lda $01db               ; sector?
        eor #$ff
        sta $01e2
        lda driveCurTrack
        eor #$ff
        asl
        sta $01e3
        asl
        asl
        rol $01e4
        asl
        rol $01e4
        adc $01e3
        tay
        lda #$00
        adc $01e4
        jmp lddf
ld95    lda $01dd
        lsr
        lsr
        bcs ldc3
        lda cmdCommand          ; command (?)
        cmp #$4f                ; > 79 ?
        bcc ldbf                ; no ->
        cmp #$58                ; < 88 ?
        bcs ldbf                ; no ->
        and #$07                ; in range 80..87 (P-W): convert to number 0..7
        sta $01ea               ; multiply by 3
        asl
        adc $01ea
        adc #<cmdTable          ; offset = $0e8f + 3 * command
        sta cmdPtr              ; store in cmdPtr
        lda #$00
        adc #>cmdTable
        sta cmdPtr+1
        jmp (cmdPtr)            ; jump to command

ldbf    cmp #$21                ; commmand = 21 ('!'): Format
        beq lddb                ; yes -> format disk


ldc3    lda #$01
        jsr lb10
        jsr lafe
ldcb    bit PiaPortB
        bvs ldcb
ldd0    lda #$4e                ; 'N'
        jsr sendSioByte
        lda #$fd
        jsr lb25
        rts

lddb    jmp _formatDisk

ldde    lddf = * + 1
; Instruction parameter jumped to.
        ora ($8d,x)
lde0    cpx zpFcTrack
        tya
        clc
        adc $01e2
        sta $80,x
        lda #$00
        adc $01e4
        inx
        sta $80,x
        rts

checkSectorSum
ldf2    lda #$00
        tax                     ; buffer index = 0
        clc
_calcSctrChecksumL1
        adc $80,x               ; add sector data byte
        inx
        bpl _calcSctrChecksumL1 ; repeat for all 128 bytes in secctor
        adc #$00                ; finally, add carry
        cmp $0187               ; compare with transferred checksum
        rts

_setPiaPortBtoWrite
le01    ldx PiaPortB
        stx PiaPortB
        ldx PiaDdrB
        inx                     ; Set Port B to output
        stx PiaDdrB
        rts

le0f    ldx #$ff
        jsr le2f                ; send byte $0186 over SIO interface
le14    ldy $80,x
        sty $0186
        jsr le32
        bpl le14
le1e    ldy $0187
        sty $0186
        jsr le32

_setPiaPortBtoRead
le27    ldx PiaDdrB
        dex
        stx PiaDdrB
        rts

le2f    lda PiaPortB
le32    and #$fe
        sta PiaPortB
        ldy #$78                ; read 8 bits
        bit $80
_sendSioByteL1
le3b    bit $80                 ; strobe output bit (?)
        nop
        nop
        lsr                     ; prepare transfer byte
        ror $0186               ; rotate bit to send into carry
        rol                     ; rotate bit to send into output bit
        sta PiaPortB            ; send bit over SIO connection
        iny
        bpl _sendSioByteL1
le4a    lsr
        sec
        rol
        nop
        nop
        nop
        nop
        nop
        nop
        sta PiaPortB
        inx
        rts

le58    lda #$ff
        ldx $01e1
        sta $80,x
        inx
        sta $80,x
        lda $01dd
        and #$08
        bne le6c
le69    jmp l951
le6c    lda #$f7
        jsr lb25
le71    lda #$04
        jsr lb09
        jmp l967
le79    lda #$08
        jsr lb1e
        jsr ld6a
        inx
        stx $01e1
        cpx #$7e
        bne le8c
le89    jmp le58
le8c    jmp lac3

cmdTable
le8f    jmp lf37                ; Write / No Verify - $50 ('P')
le92    jmp ldc3                ; Read Spin - $51 ('Q')
le95    jmp l926                ; Read - $52 ('R')
le98    jmp ld36                ; Status - $53 ('S')
le9b    jmp ldc3                ; --
le9e    jmp ldc3                ; Motor On - $55 ('U')
lea1    jmp ldc3                ; Verify Sector - $56 ('V')
lea4    jmp lf32                ; Write/Verify - $57 ('W')

lea7    jsr lc77
        jsr lc43                ; issue command: seek
        jsr uploadSectorData
        jsr checkSectorSum
        bne lf01                ; error: checksum in uploaded data ->
        jsr sendSioAck
        jsr lfcd
        jsr lf4b
        lda PiaPortA
        and #$10                ; write protect on?
        bne lf0e                ; error: disk is write protected ->
lec5    lda #$57                ; command: Write Command (10100000)
        sta zpFcCommand
lec9    bvs lee5
lecb    lda $039c
        bit PiaPortA
        bmi ledd                ; -> send data buffer to floppy controller
led3    jmp lec9

led6    bvs lee5                ; IRQ (interrupt request)? ->
led8    bit PiaPortA
        bpl led6                ; DRQ (data request)? ->
ledd    inx
        lda $80,x               ; byte from RAM buffer
        sta zpFcData            ; write to floppy controller
        jmp led8

lee5    jsr l970
        lda #$45
        bcs lf42
leec    lda $01dd
        and #$08
        beq lefc
lef3    jsr lf9d
        bcc lefc
lef8    lda #$45
        bcs lf08
lefc    lda #$43
lefe    jmp lf08
lf01    lda #$12
        jsr lb10
        lda #$4e                ; 'N'
lf08    jsr sendSioByte
        jmp l841
lf0e    lda #$0c
        jsr lb09
        lda #$45
        jmp lf08

lf18    beq lf31
lf1a    jsr _stepHead
        beq lf27
lf1f    lda $0395
        bpl lf1f
lf24    jmp lf1a

lf27    lda #$50
        sta $039e
lf2c    lda $0395
        bpl lf2c
lf31    rts

lf32    lda #$08
        jsr lb1e


lf37    jsr l8c1
        bcs lf3f
lf3c    jmp lea7
lf3f    jmp ldc3
lf42    cpy #$00
        beq lf49
lf46    jmp lec5
lf49    beq lefe

lf4b    jsr calcTrackSector     ; calculate track/sector from Aux1, Aux2
        lda PiaPortB
        and #$3c                ; stepper motor configuration
        bne lf5a
        lda #$30
        sta PiaPortB
lf5a    lda $01da               ; track?
        jsr setStepperParams
        jsr lf18
lf63    jsr lc5f
        ldy #$04

lf68    lda #$2f                ; command: Force Interrupt (11010000)
        sta zpFcCommand
        ldx #$ff
lf6e    dex
        bne lf6e
        lda $01da               ; track?
        sta zpFcTrack
        lda $01db               ; sector?
        sta zpFcSector
        clv
        dex
        stx $039f
        rts

lf81    jsr lf68
        lda driveCurTrack
        sta zpFcTrack
        lda #$77                ; command: Read Command (10001000)
        sta zpFcCommand
lf8d    bvs lf9c
        lda $039c
lf92    bit PiaPortA
        bpl lf8d
lf97    lda zpFcData
        jmp lf92
lf9c    rts

lf9d    jsr lf68
        ldy #$01
lfa2    inx
        lda #$77                ; command: Read Command (10001000)
        sta zpFcCommand
lfa7    bvs lfba
lfa9    lda $039c
lfac    bit PiaPortA
        bpl lfa7
lfb1    lda zpFcData
        cmp $80,x               ; verify data with buffer(?)
        bne lfc7
lfb7    inx
        bne lfac
lfba    jsr l970
        bcc lfc1
lfbf    bpl lfa2
lfc1    lda #$f7
        jsr lb25
        rts
lfc7    jsr l9a3
        jmp lfc1

lfcd    lda $01dd
        and #$04
        beq lfd9
        lda #$50                ; decimal: 80
        jsr _wait
lfd9    rts

        .byt $00,$00,$00,$00,$00,$00
; 0fe0
        .byt $00,$00,$00,$00,$00,$00,$00,$00
        .byt $00,$00,$00,$00,$00,$00,$00,$00
        .byt $00,$00,$00,$00,$00,$00,$00,$00
        .byt $00,$00,$00,$00

        .word l800              ; reset vector?
        .word l800
