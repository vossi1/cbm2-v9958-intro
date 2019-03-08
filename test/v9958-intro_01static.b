; *************************************************************************************************
;                  CBM2-V9958-Card Yamaha V9938-V9958 CBM2-Intro / Vossi 02/2019
; *************************************************************************************************
!cpu 6502	; 6502, 6510, 65c02, 65816
!ct pet		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
; VDP write speed: Reg->Reg(Port 1,2,3) 2us, Reg->VRAM 8us, VRAM->VRAM 8us, VRAM->Reg 8us
; 	sta(zp),Y = 6 cycles @ 2MHz = 3us -> no wait nessesary
;	lda# + sta(zp),y = 8 cycles @ 2MHz = 4us -> add 5us wait
;	lda addr,x + sta(zp),y + inx + bne = 15 cycles @2MHz = 7.5us -> OK
; pass parameters to subroutine: AA = lowbyte, XX = highbyte / return AA or XXAA 

; switches
;ROM = 1		; assemble extension rom at $1000
PAL = 0			; PAL=1, NTSC=0		selects V9938/58 PAL RGB-output, NTSC has a higher picture
LINES = 212		; lines = 192 / 212
!ifdef 	ROM{!to "intro.bin", plain
} else{ 	!to "intro.prg", cbm }
!source "macros_6502.b"
; ***************************************** CONSTANTS *********************************************
FILL					= $00		; fills free memory areas with $00
V_NULL					= $ff		; VDP string End
VDPREG1					= $02		; VDP reg 1 value (mode bits M1+M2, screen disabled)
VDPREG18				= $0d		; VDP reg 18 value (V/H screen adjust, $0d = Sony PVM 9")
!if LINES=192 {VDPREG9	= $00|PAL*2	; VDP reg 9 value ($00 = NTSC, $02 = PAL)
	}else {VDPREG9		= $80|PAL*2}
VDPTUNE					= 0			; tune vdp waits in 1us steps
; ***************************************** ADDRESSES *********************************************
!addr evect				= $03f8		; warmstart vector
!addr bootcbm2			= $f9e0		; continue boot
VDPAddress				= $d900		; Port#0 RamWrite, #1 Control, #2 Palette, #4 RamRead, #5 Status
PatternTable			= $0000
PatternColorTable		= $2000
ScreenTable				= $3800
SpritePatternTable		= $1800
SpriteAttributeTable	= $1e00
SpriteColorTable		= SpriteAttributeTable - $200	; always $200 below sprite attribute table
!ifndef DUMMYADDRESS{!addr DUMMYADDRESS = $0000}
!addr VDPRamWrite		= VDPAddress			; VDP ports
!addr VDPControl		= VDPAddress+1
!addr VDPPalette		= VDPAddress+2
!addr VDPIndirect		= VDPAddress+3
!addr VDPRamRead		= VDPAddress+4
!addr VDPStatus			= VDPAddress+5
; ***************************************** ZERO PAGE *********************************************
!addr CodeBank			= $00	; *** bank select register	
!addr IndirectBank		= $01
ZP = $03						; *** start zero page pointers
										; IO pointers
VZP = $10						; *** start zero page VDP parameter
!addr vdp_counter		= VZP			; 8bit universal counter
!addr vdp_counter2		= VZP+$01		; 8bit universal counter
!addr vdp_calc			= VZP+$02		; 8bit universal calc memory
!addr vdp_bgcolor		= VZP+$03		; 8bit background color
!addr vdp_color			= VZP+$04		; 8bit color
!addr vdp_pointer		= VZP+$05		; 16bit universal pointer
!addr vdp_pointer2		= VZP+$07		; 16bit universal pointer
!addr vdp_data			= VZP+$09		; 16bit pointer to source data (bitmap or string)
!addr vdp_size			= VZP+$0b		; 16bit size for VdpCopy + VdpCopy16
!addr vdp_size_x		= VZP+$0d		; 16bit x size for VDP subroutines
!addr vdp_size_y		= VZP+$0f		; 16bit y size for VDP sobroutines
!addr sprite_group_min	= VZP+$11		; sprite group start
!addr sprite_group_max	= VZP+$12		; sprite group end
SZP = $30
!addr sprite_x			= SZP			; 32 sprite x positions
!addr sprite_y			= SZP+$20		; 32 sprite y positions
!addr sprite_p			= SZP+$40		; 32 sprite pattern
BZP = $90						; *** start zero page byte-variables
!addr vdpid				= BZP			; VDP ID
!addr counter			= BZP+$01		; 8bit universal counter
WZP = $a0						; *** start zero page word variables	
!addr counter16			= WZP			; 16bit universal counter
!addr pointer			= WZP+$02		; universal pointer
; ******************************************* MACROS **********************************************
!macro VDPWAIT .t{			; *** t x 1 us wait for VRAM write
	!do while .t > 0{
		nop								; each nop needs 1 us @ 2MHz
		!set .t = .t -1}
}
!macro VdpSetReg .r{			; *** set VDP Register
	sta VDPControl					; first writes data in A to control port #1
	lda # .r | $80						; writes register no. with bit#7 = 1 to Port #1
	sta VDPControl
}
; ********************* Y register must be $00 for all Vdp-Address subroutines ********************
!macro VdpWriteAddress{			; *** set VDP write vram address-pointer to XXAA
	sta VDPControl
	txa
	ora # $40							; bit#6 = 1 write
	sta VDPControl
} 
!macro VdpReadAddress{					; *** set VDP read vram address-pointer to XXAA
	sta VDPControl
	txa
	sta VDPControl
}
!initmem FILL
; ***************************************** ZONE INIT *********************************************
!ifdef ROM{
*= $1000
!zone init						; *** rom start
	jmp init							; jump to init
	jmp init
	!byte "c",$c2,$cd,"1"				; cbm-rom ident-bytes 'c'= no init, '1' = 4k-block 1
init:							; *** initialize bank regs and start main code ***
	sei									; disable interrupts
	lda #$0f							; switch indirect bank to 15
	sta IndirectBank
	jmp start							; jump to start
end:							; *** terminate program and boot ***
	lda #$cc
	sta evect							; set warmstart vector to $bbcc = BASIC
	lda #$bb
	sta evect+1
	jmp bootcbm2						; boot with init
*= $1020
} else {
*= $0400
}
; ***************************************** ZONE MAIN *********************************************
!zone main
start:							; *** main code starts here ***	
	jsr VdpInit
	jsr VdpOn

	ldx # 0
	txa
	clc
-	sta sprite_p,x
	adc # 4
	inx
	cpx # 16
	bne -

	ldx # 0
-	lda # 92
	sta sprite_y,x
	inx
	sta sprite_y,x
	inx
	lda # 92+16	
	sta sprite_y,x
	inx
	sta sprite_y,x
	inx
	cpx # 16
	bne -

	ldx # 0
	lda # 52
	clc
-	sta sprite_x,x
	inx
	sta sprite_x,x
	inx
	adc # 16
	sta sprite_x,x
	inx
	sta sprite_x,x
	inx
	adc # 16+8
	cpx # 16
	bne -

	jsr VdpSpriteGroup
stop:
	jmp stop
	jsr VdpOff	
;	jmp end								; end program
; ************************************* ZONE SUBROUTINES ******************************************
!zone subroutines
; *********************************** ZONE VDP_SUBROUTINES ****************************************
!zone vdp_subroutines
VdpInit:						; *** initialize VDP ***
	lda #$00
	tax
	+VdpSetReg 17						; write VDP regs fast indirect
	+VDPWAIT 4
-	lda VdpInitData,x
	sta VDPIndirect
	inx
	cpx # VdpInitDataEnd - VdpInitData
	bne -
	lda # VDPREG18
	+VdpSetReg 18						; set register 18 V/H display adjust L 7-1,0,f-8 R
								; * clear 16kB VRAM
	lda	#$00
	tax
	+VdpWriteAddress					; set VRAM write address to $XXAA = $0000, Bank Reg already $00
	ldy #$40							; set counter to $4000 - Y already $00	
-	sta VDPRamWrite						; vdp pause between WR only 5us - works!
	nop
	inx
	bne -
	dey
	bne -
								; * copy color-palette
	tya									; Y and X are already $00
	+VdpSetReg  16						; set VDP register 16 = palette pointer to $00 
-	lda PaletteData,x					; load palette-color to write
	sta VDPPalette
	inx
	cpx # PaletteDataEnd - PaletteData
	bne -
								; * copy sprite data to sprite pattern table
	+st16i vdp_pointer, SpriteData
	+st16i vdp_size, SpriteDataEnd - SpriteData
	+ldax16i SpritePatternTable			; VRAM address in XXAA
	jsr VdpCopy16
								; * copy sprite color data to sprite color table
	+st16i vdp_pointer, SpriteColorData
	+st16i vdp_size, SpriteColorDataEnd - SpriteColorData
	+ldax16i SpriteColorTable			; VRAM address in XXAA
	jsr VdpCopy16

	rts

VdpOn:							; *** enable screen ***
	lda # VDPREG1 | $40					; set mode reg 1 (M1+M2), bit#6 = 1 enables screen
	+VdpSetReg 1
	rts

VdpOff:							; *** disable screen ***
	lda # VDPREG1 & $bf					; set mode reg 1 (M1+M2), bit#6 = 1 enables screen
	+VdpSetReg 1
	rts

VdpStatus:						; *** read status register in A - return status in A
	lda # 1
	+VdpSetReg 15						; reg 15 = 1 initiates read status-reg 1
	+VDPWAIT 6						; wait for DVP
	lda VDPStatus 					; read status
	rts

VdpCopy:						; *** copy vdp_size bytes from pointer to VRAM at $XXAA ***
	+VdpWriteAddress
	ldy #$00
-	lda(vdp_pointer),y					; load data
	sta VDPRamWrite
	iny
	cpy vdp_size
	bne -
	rts

VdpCopy16:						; *** copy vdp_size bytes from pointer to VRAM at $XXAA ***
	+VdpWriteAddress
	inc vdp_size+1						; add 1 to size-highbyte for first run with X=lowsize
	ldy #$00
	ldx vdp_size
-	lda(vdp_pointer),y					; load data
	sta VDPRamWrite
	iny
	dex
	bne -
	dec vdp_size+1
	bne -
	rts

VdpSprite:						 ; *** write sprite attributes from sprite X to VDP
	tax									; safe as index
	asl									; 4 attribute bytes each sprite
	asl
	sta VDPControl
	lda # (>SpriteAttributeTable) | $40	; bit 8-13 attribute table + bit 6 for write VRAM
	sta VDPControl
	+VDPWAIT 3-VDPTUNE
	lda sprite_y,x
	sta VDPRamWrite
	+VDPWAIT 3-VDPTUNE
	lda sprite_x,x
	sta VDPRamWrite
	+VDPWAIT 3-VDPTUNE
	lda sprite_p,x
	sta VDPRamWrite
	rts

VdpSpriteGroup:					 ; *** write sprite attributes from sprite X to VDP
	lda sprite_group_min
	tax									; safe as index
	asl									; 4 attribute bytes each sprite
	asl
	sta VDPControl
	lda # (>SpriteAttributeTable) | $40	; bit 8-13 attribute table + bit 6 for write VRAM
	sta VDPControl
	+VDPWAIT 3-VDPTUNE
-	lda sprite_y,x
	sta VDPRamWrite
	+VDPWAIT 3-VDPTUNE
	lda sprite_x,x
	sta VDPRamWrite
	+VDPWAIT 3-VDPTUNE
	lda sprite_p,x
	sta VDPRamWrite
	+VDPWAIT 5-VDPTUNE
	sta VDPRamWrite						; dummy write for unused attribute 4
	cpx sprite_group_max
	beq +
	inx
	bne -
+	rts
; ****************************************** ZONE DATA ********************************************
!zone data
VdpInitData:	; graphics3-mode
!byte $04,VDPREG1,$03,$2f,$02,$36,$07,$10,$08,VDPREG9,$00,$00,$20,$f0,$00
	; reg  0: $04 mode control 1: text mode 2 (bit#1-3 = M3 - M5)
	; reg  1: $02 mode control 2: bit#1 16x16 sprites, bit#3-4 = M2-M1, #6 =1: display enable)
	; reg  2: $3b name (screen) table base address $3800 ( * $400 + bit#0+1 = 1)
	; reg  3: $ff pattern color table base address $2000 ( bit#7=A13 + bit#0-6 = 1)
	; reg  4: $03 pattern generator table base address $0000 ( bit#2-5=A13-A16 + bit#0+1 = 1)
	; reg  5: $3c sprite attribute table base address $1e00 (* $80)
	; reg  6: $03 sprite pattern (data) generator base address = $1800 (* $800)
	; reg  7: $10 text/overscan-backdrop color 
	; reg  8: $08 bit#3 = 1: 64k VRAM chips, bit#1 = 0 sprites disable, bit#5 0=transparent
	; reg  9: $80 bit#1 = NTSC/PAL, #2 = EVEN/ODD, #3 = interlace, #7 = 192/212 lines
	; reg 10: $00 pattern color table base address bit#0-2 = A14-A16
	; reg 11: $00 sprite attribute table base address bit#0-1 = A15-A16
	; reg 12: $20 text/background blink color
	; reg 13: $f0 blink periods ON/OFF - f0 = blinking off
	; reg 14: $00 VRAM write addresss bit#0-2 = A14-A16
VdpInitDataEnd:
; ***** Color Palette - 16 colors, 2 byte/color: RB, 0G each 3bit -> C64 VICII-colors *****
PaletteData:
	!byte $00,$00,$77,$07,$70,$01,$17,$06	;	0=black		1=white		2=red		3=cyan
	!byte $56,$02,$32,$06,$06,$02,$72,$07	;	4=violet	5=green		6=blue		7=yellow
	!byte $70,$03,$60,$02,$72,$03,$11,$01	;	8=orange	9=brown		10=lred		11dgrey
	!byte $33,$03,$54,$07,$27,$04,$55,$05	;	12=grey		13=lgreen	14=lblue	15=lgrey
PaletteDataEnd:

SpriteData:
!byte $07, $1f, $3f, $7f, $7f, $ff, $ff, $ff	; c upper left
!byte $ff, $fe, $fe, $fe, $fe, $fe, $fe, $fe
!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $80
!byte $00, $00, $00, $00, $00, $00, $00, $00

!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $01	; c upper right
!byte $00, $00, $00, $00, $00, $00, $00, $00
!byte $e0, $f8, $fc, $fe, $fe, $ff, $ff, $ff
!byte $ff, $7f, $7f, $7f, $00, $00, $00, $00

!byte $fe, $fe, $fe, $fe, $fe, $fe, $fe, $ff	; c lower left
!byte $ff, $ff, $ff, $7f, $7f, $3f, $1f, $07
!byte $00, $00, $00, $00, $00, $00, $00, $00
!byte $80, $ff, $ff, $ff, $ff, $ff, $ff, $ff

!byte $00, $00, $00, $00, $00, $00, $00, $00	; c lower right
!byte $01, $ff, $ff, $ff, $ff, $ff, $ff, $ff
!byte $00, $00, $00, $00, $7f, $7f, $7f, $ff
!byte $ff, $ff, $ff, $fe, $fe, $fc, $f8, $e0

!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $fe	; b upper left
!byte $fe, $fe, $fe, $fe, $fe, $ff, $ff, $ff
!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $00
!byte $00, $00, $00, $00, $00, $ff, $ff, $ff

!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $01	; b upper right
!byte $00, $00, $00, $00, $01, $ff, $ff, $ff
!byte $e0, $f8, $fc, $fe, $fe, $ff, $ff, $ff
!byte $ff, $7f, $7f, $ff, $ff, $ff, $fe, $fc

!byte $ff, $ff, $ff, $fe, $fe, $fe, $fe, $fe	; b lower left
!byte $fe, $ff, $ff, $ff, $ff, $ff, $ff, $ff
!byte $ff, $ff, $ff, $00, $00, $00, $00, $00
!byte $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff

!byte $ff, $ff, $ff, $01, $00, $00, $00, $00	; b lower right
!byte $01, $ff, $ff, $ff, $ff, $ff, $ff, $ff
!byte $fc, $fe, $ff, $ff, $ff, $7f, $7f, $ff
!byte $ff, $ff, $ff, $fe, $fe, $fc, $f8, $e0

!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	; m upper left
!byte $ff, $ff, $fe, $fe, $fe, $fe, $fe, $fe
!byte $f0, $f0, $f0, $f0, $f8, $f8, $f8, $f8
!byte $fc, $fc, $fc, $fc, $fe, $fe, $7e, $7e

!byte $0f, $0f, $0f, $0f, $1f, $1f, $1f, $1f	; m upper right
!byte $3f, $3f, $3f, $3f, $7f, $7f, $7e, $7e
!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
!byte $ff, $ff, $7f, $7f, $7f, $7f, $7f, $7f

!byte $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe	; m lower left
!byte $fe, $fe, $fe, $fe, $fe, $fe, $fe, $fe
!byte $7f, $7f, $3f, $3f, $3f, $3f, $1f, $1f
!byte $1f, $1f, $0f, $0f, $0f, $0f, $07, $07

!byte $fe, $fe, $fc, $fc, $fc, $fc, $f8, $f8	; m lower right
!byte $f8, $f8, $f0, $f0, $f0, $f0, $e0, $e0
!byte $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f
!byte $7f, $7f, $7f, $7f, $7f, $7f, $7f, $7f

!byte $0f, $07, $03, $03, $03, $03, $03, $03	; ii upper left
!byte $03, $03, $03, $03, $03, $03, $03, $03
!byte $fc, $f8, $f0, $f0, $f0, $f0, $f0, $f0
!byte $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0

!byte $3f, $1f, $0f, $0f, $0f, $0f, $0f, $0f	; ii upper right
!byte $0f, $0f, $0f, $0f, $0f, $0f, $0f, $0f
!byte $f0, $e0, $c0, $c0, $c0, $c0, $c0, $c0
!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0

!byte $03, $03, $03, $03, $03, $03, $03, $03	; ii lower left
!byte $03, $03, $03, $03, $03, $03, $07, $0f
!byte $f0, $f0, $f0, $f0, $f0, $f0, $f0, $f0
!byte $f0, $f0, $f0, $f0, $f0, $f0, $f8, $fc

!byte $0f, $0f, $0f, $0f, $0f, $0f, $0f, $0f	; ii lower right
!byte $0f, $0f, $0f, $0f, $0f, $0f, $1f, $3f
!byte $c0, $c0, $c0, $c0, $c0, $c0, $c0, $c0
!byte $c0, $c0, $c0, $c0, $c0, $c0, $e0, $f0
SpriteDataEnd:

SpriteColorData:
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,14,14,6,6,6,6,6,6,6,6,6,6,6,6,11
!byte 15,10,10,2,2,2,2,2,2,2,2,2,2,2,2,11
!byte 15,10,10,2,2,2,2,2,2,2,2,2,2,2,2,11
!byte 15,10,10,2,2,2,2,2,2,2,2,2,2,2,2,11
!byte 15,10,10,2,2,2,2,2,2,2,2,2,2,2,2,11
SpriteColorDataEnd: