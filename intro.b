; *************************************************************************************************		
;                             CBM2-V9958-Card Intro / Vossi 03/2019
; *************************************************************************************************
; basic copys init-code to bank 15 and starts at $0400
!cpu 6502	; 6502, 6510, 65c02, 65816
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
; VDP write speed: Reg->Reg(Port 1,2,3) 2us, Reg->VRAM 8us, VRAM->VRAM 8us, VRAM->Reg 8us
; 	sta(zp),Y = 6 cycles @ 2MHz = 3us -> no wait nessesary
;	lda# + sta(zp),y = 8 cycles @ 2MHz = 4us -> add 5us wait
;	lda addr,x + sta(zp),y + inx + bne = 15 cycles @2MHz = 7.5us -> OK
; pass parameters to subroutine: AA = lowbyte, XX = highbyte / return AA or XXAA 

; switches
;DEBUG = 1		; selects DEBUG code
PAL = 0			; PAL=1, NTSC=0		selects V9938/58 PAL RGB-output, NTSC has a higher picture
LINES = 212		; lines = 192 / 212
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
VDPAddress				= $d900		; Port#0 RamWrite, #1 Control, #2 Palette, #4 RamRead, #5 Status
PatternTable			= $0000
SpritePatternTable		= $1800
SpriteAttributeTable	= $1e00
SpriteColorTable		= SpriteAttributeTable - $200	; always $200 below sprite attribute table
!ifndef DUMMYADDRESS{!addr DUMMYADDRESS = $0000}
!addr vdpcopy16_data	= VdpCopy16CodePointer+1	; 16bit pointer to LDA-address in VdpCopy16
!addr vdpcopy_data		= VdpCopyCodePointer+1		; 16bit pointer to LDA-address in VdpCopy
; ***************************************** ZERO PAGE *********************************************
!addr CodeBank			= $00	; *** bank select register	
!addr IndirectBank		= $01
ZP = $33						; *** start zero page pointers
!addr VDPRamWrite		= ZP			; VDP pointer
!addr VDPControl		= ZP+$02
!addr VDPPalette		= ZP+$04
!addr VDPIndirect		= ZP+$06
!addr VDPRamRead		= ZP+$08
!addr VDPStatus			= ZP+$0a
										; IO pointers
VZP = $40						; *** start zero page VDP parameter
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
SZP = $60
!addr sprite_x			= SZP			; 32 sprite x positions
!addr sprite_y			= SZP+$20		; 32 sprite y positions
!addr sprite_p			= SZP+$40		; 32 sprite pattern
BZP = $c0						; *** start zero page byte-variables
!addr vdpid				= BZP			; VDP ID
!addr counter			= BZP+$01		; 8bit universal counter
WZP = $d0						; *** start zero page word variables	
!addr counter16			= WZP			; 16bit universal counter
!addr pointer			= WZP+$02		; universal pointer
;						= $f8-$fc		; reserved console source
!addr debug_fd			= $fd;-$ff		; 3 byte debug register for A, X, Y			
; ******************************************* MACROS **********************************************
!ifdef DEBUG{
!macro DEBUG{ jsr ConsoleDebug }
}
!macro VDPWAIT .t{			; *** t x 1 us wait for VRAM write
	!do while .t > 0{
		nop								; each nop needs 1 us @ 2MHz
		!set .t = .t -1}
}
!macro VdpSetReg .r{			; *** set VDP Register
	ldy #$00
	sta(VDPControl),y					; first writes data in A to control port #1
	lda # .r | $80						; writes register no. with bit#7 = 1 to Port #1
	sta(VDPControl),y
}
; ********************* Y register must be $00 for all Vdp-Address subroutines ********************
!macro VdpWriteAddress{			; *** set VDP write vram address-pointer to XXAA
	sta(VDPControl),y
	txa
	ora # $40							; bit#6 = 1 write
	sta(VDPControl),y
} 
!macro VdpReadAddress{					; *** set VDP read vram address-pointer to XXAA
	sta(VDPControl),y
	txa
	sta(VDPControl),y
}
; **************************************** BASIC LOADER *******************************************
!initmem FILL
*= $0003
	!byte $2f,$00,$0a,$00,$81,$49,$b2,$31,$30,$32,$34,$a4,$31,$30,$33,$39
	!byte $3a,$dc,$31,$3a,$41,$b2,$c2,$28,$49,$29,$3a,$dc,$31,$35,$3a,$97
	!byte $49,$2c,$41,$3a,$82,$3a,$9e,$31,$30,$32,$34,$00,$00,$00,$00,$00
	; 10 fori=1024to1039:bank1:a=peek(i):bank15:pokei,a:next:sys1024
	; $0033 - $00FF	zero page
	; $0100 - $01FF	cpu-stack
	; $0200 - $03FF	start-code
	; $0400 - $040F	init-code -> mirror in bank 15
	; $0410 - 		code / data
; ***************************************** ZONE INIT *********************************************
!zone init
*= $0400
init:							; *** initialize bank regs and start main code ***
	sei									; disable interupts
	lda #$01							; switch to bank 1
	sta CodeBank
	jmp start
end:							; *** terminate program and jump back to basic ***
	lda #$0f							; switch completely back to bank 15
	sta CodeBank
	sta IndirectBank
	cli									; enable interupts
	rts									; back to basic
; ***************************************** ZONE MAIN *********************************************
!zone main
*= $0200
start:							; *** main code starts here ***	
	lda #$0f							; set bank indirect reg to bank 15
	sta IndirectBank
	jsr ConsoleClear					; start console
	jsr InitZeroPage					; initialize all zero page pointers
	+ldax16i message_start
	jsr ConsoleText
	+ldax16i message_copyright
	jsr ConsoleText
	lda # 1								; read status register 1
	jsr VdpStatus
	lsr
	and #$1f							; isolate VDP identification bit #1-5
	sta vdpid
	bne +
	+ldax16i message_v9938				; ID = 0 -> V9938
	jsr ConsoleText
	jmp ++
+	cmp #$02
	bne +
	+ldax16i message_v9958				; ID = 1 -> V9958
	jsr ConsoleText
	jmp ++
+	+ldax16i message_novdp				; no vdp detected
	jsr ConsoleText
++	+ldax16i message_clearvram
	jsr ConsoleText
	
	jsr VdpInit
	lda vdpid							; ready message only with id ID 0 or 2 
	beq +
	cmp #$02
	bne ++
+	+ldax16i message_vdpready
	jsr ConsoleText
	jmp +
++	+ldax16i message_vdpunknown
	jsr ConsoleText
+
	jsr VdpOn

	lda # 0
	sta sprite_group_min
	lda # 15
	sta sprite_group_max

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
	inx
	sta sprite_x,x
	dex
	adc # 16
	sta sprite_x,x
	inx
	inx
	sta sprite_x,x
	inx
	adc # 16+8
	cpx # 16
	bne -

	jsr VdpSpriteGroup

	jmp end								; end program
; ************************************* ZONE SUBROUTINES ******************************************
!zone subroutines
InitZeroPage:					; *** init zero page addresses
	lda # >VDPAddress
	sta VDPRamWrite+1
	sta VDPControl+1
	sta VDPPalette+1
	sta VDPIndirect+1
	sta VDPRamRead+1
	sta VDPStatus+1
	ldx # <VDPAddress
	stx VDPRamWrite
	inx
	stx VDPControl
	inx
	stx VDPPalette
	inx
	stx VDPIndirect
	inx
	stx VDPRamRead
	inx
	stx VDPStatus
	rts
; *********************************** ZONE VDP_SUBROUTINES ****************************************
*= $0410
!zone vdp_subroutines
VdpInit:						; *** initialize VDP ***
	lda #$00
	tax
	+VdpSetReg 17						; write VDP regs fast indirect
	+VDPWAIT 4
-	lda VdpInitData,x
	sta(VDPIndirect),y
	inx
	cpx # VdpInitDataEnd - VdpInitData
	bne -
	lda # VDPREG18
	+VdpSetReg 18						; set register 18 V/H display adjust L 7-1,0,f-8 R
								; * clear 128kB VRAM (in mode 7 complete autoincrement)
	tya									; all regs $00
	tax	
	+VdpWriteAddress					; set VRAM write address to $XXAA = $0000, Bank Reg already $00
	+st16i counter16, $0200				; counter to $20000 bytes = 128kB
	tya									; VRAM init valua = $00
-	sta(VDPRamWrite),y					; vdp pause between WR only 5us - works!
	inx
	bne -
	dec counter16
	bne -
	dec counter16+1
	bne -
								; * copy color-palette
	+VdpSetReg  16						; set VDP reg 16 = palette pointer to $00, A, X are already $00
-	lda PaletteData,x					; load palette-color to write
	sta(VDPPalette),y
	inx
	cpx # PaletteDataEnd - PaletteData
	bne -
								; * copy sprite data to sprite pattern table
	+st16i vdpcopy16_data, SpriteData
	+st16i vdp_size, SpriteDataEnd - SpriteData
	+ldax16i SpritePatternTable			; VRAM address in XXAA
	jsr VdpCopy16
								; * copy sprite color data to sprite color table
	+st16i vdpcopy16_data, SpriteColorData
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
	+VDPWAIT 6							; wait for DVP
	lda(VDPStatus),y					; read status
	rts

VdpCopy:						; *** copy vdp_size bytes from pointer to VRAM at $XXAA ***
	ldy #$00
	+VdpWriteAddress
	ldx #$00
VdpCopyCodePointer:						; +1,+2 = data address in memory
-	lda DUMMYADDRESS,x					; load data
	sta(VDPRamWrite),y
	inx
	cpx vdp_size
	bne -
	rts

VdpCopy16:						; *** copy vdp_size bytes from pointer to VRAM at $XXAA ***
	ldy #$00
	+VdpWriteAddress
	inc vdp_size+1						; add 1 to count-highbyte because nessasary dec count+1
	ldx vdp_size
VdpCopy16CodePointer:					; +1,+2 = data address in memory
-	lda DUMMYADDRESS					; load data
	sta(VDPRamWrite),y
	+inc16 VdpCopy16CodePointer+1		; increase lda address
	dex
	bne -
	dec vdp_size+1
	bne -
	rts

VdpText:						; *** copy string vdp_textdata with vdp_color to VRAM at $XXAA ***
	ldy #$00
	sty vdp_counter2					; init string counter
	+stax16 vdp_pointer					; pointer to VRAM position A = x, X = y
---	+st16i vdp_pointer2, FontData		; init pointer2 to first character of font
	sty vdp_calc						; init calc for highbyte char at fontdata position, y already $00
	sty vdp_counter						; init counter for 8 bytes of the character data in font
	tya									; clear A for ORA = LDA
	ldy vdp_counter2
	ora(vdp_data),y						; load source data
	cmp # V_NULL
	beq +++								; V_NULL = end of string
	asl									; multiply charcode with 8 = position in font
	rol vdp_calc
	asl 
	rol vdp_calc
	asl
	rol vdp_calc
	clc	
	adc vdp_pointer2					; add character * 8 lowbyte to font pointer
	sta vdp_pointer2
	lda vdp_pointer2+1	
	adc vdp_calc						; add calced highbyte
	sta vdp_pointer2+1
	ldy #$00
--	+ldax16 vdp_pointer
	+VdpWriteAddress					; Y already $00
	tya
	ldy vdp_counter
	ora(vdp_pointer2),y					; load counter. byte of character-data from font 
	sta vdp_calc
	ldx #$08							; init bit counter X
	ldy #$00
-	asl vdp_calc
	lda vdp_bgcolor						; load background color
	bcc +								; pixel not 1 -> draw background color
	lda vdp_color						; load text color
+	sta(VDPRamWrite),y
	dex
	bne -								; last bit reached
	inc vdp_pointer+1					; next line of screen - VRAM pointer highbyte +1
	inc vdp_counter						; next byte of character data
	lda vdp_counter
	cmp #$08							; last byte of character
	bne --
	lda vdp_pointer						; x position +1 for next character
	clc
	adc #$08
	sta vdp_pointer
	lda vdp_pointer+1
	sbc #$07							; y position -7 -> first line of character, carry doesnt matter
	sta vdp_pointer+1
	inc vdp_counter2					; next character in string
	jmp ---
+++ rts

VdpSprite:						 ; *** write sprite attributes from sprite X to VDP
	tax									; safe as index
	asl									; 4 attribute bytes each sprite
	asl
	sta(VDPControl),y
	lda # ((>SpriteAttributeTable)&$3f)|$40	; bit 8-13 attribute table + bit 6 for write VRAM
	sta(VDPControl),y
	+VDPWAIT 3-VDPTUNE
	lda sprite_y,x
	sta(VDPRamWrite),y
	+VDPWAIT 3-VDPTUNE
	lda sprite_x,x
	sta(VDPRamWrite),y
	+VDPWAIT 3-VDPTUNE
	lda sprite_p,x
	sta(VDPRamWrite),y
	rts

VdpSpriteGroup:					 ; *** write sprite attributes from sprite X to VDP
	lda sprite_group_min
	tax									; safe as index
	asl									; 4 attribute bytes each sprite
	asl
	sta(VDPControl),y
	lda # ((>SpriteAttributeTable)&$3f)|$40	; bit 8-13 attribute table + bit 6 for write VRAM
	sta(VDPControl),y
	+VDPWAIT 3-VDPTUNE
-	lda sprite_y,x
	sta(VDPRamWrite),y
	+VDPWAIT 3-VDPTUNE
	lda sprite_x,x
	sta(VDPRamWrite),y
	+VDPWAIT 3-VDPTUNE
	lda sprite_p,x
	sta(VDPRamWrite),y
	+VDPWAIT 5-VDPTUNE
	sta(VDPRamWrite),y					; dummy write for unused attribute 4
	cpx sprite_group_max
	beq +
	inx
	bne -
+	rts

; ***************************************** ZONE DEBUG ********************************************
!zone debug_subroutine
!ifdef DEBUG{
ConsoleDebug:							; optional debug code prints A, X, Y
	php
	sta debug_fd
	stx debug_fd+1
	sty debug_fd+2
	+ldax16i string_debug_axy
	jsr ConsoleText
	lda debug_fd
	jsr ConsoleByte
	+ldax16i string_space
	jsr ConsoleText
	lda debug_fd+1
	jsr ConsoleByte
	+ldax16i string_space
	jsr ConsoleText
	lda debug_fd+2
	jsr ConsoleByte
	+ldax16i string_cr
	jsr ConsoleText
	lda debug_fd
	ldx debug_fd+1
	ldy debug_fd+2
	plp
	rts
}
; ********************************** ZONE CONSOLE_SUBROUTINES *************************************
!zone console_subroutines
!source "console.b"
; ****************************************** ZONE DATA ********************************************
!zone data
message_start !scr "Commodore-CBM2 with V9958 Hires-Color-Card!", C_CR, C_NULL
message_copyright !scr "Design (c) Vossi 01/2019 in Hamburg/Germany", C_CR, C_CR, C_NULL
message_clearvram !scr "Initializing VDP and clearing VRAM... ", C_CR, C_NULL
message_memory !scr "000 kB graphics memory detected", C_CR, C_NULL
message_nomemory !scr "No graphics memory detected!", C_CR, C_NULL
message_v9938 !scr "V9938 detected", C_CR, C_NULL
message_v9958 !scr "V9958 detected", C_CR, C_NULL
message_novdp !scr "No VDP detected! -> trying to initialize:", C_CR, C_NULL
message_vdpready !scr "VDP ready.", C_CR, C_NULL
message_vdpunknown !scr "VDP ?", C_CR, C_NULL
vdp_message !scr  "v9958: 32 sprites in 16 colors", V_NULL

VdpInitData:	; graphics3-mode
!byte $04,VDPREG1,$0e,$ff,$03,$3f,$03,$10,$08,VDPREG9,$00,$00,$10,$f0,$00
	; reg  0: $04 mode control 1: text mode 2 (bit#1-3 = M3 - M5)
	; reg  1: $02 mode control 2: bit#1 16x16 sprites, bit#3-4 = M2-M1, #6 =1: display enable)
	; reg  2: $0e name (screen) table base address $3800 ( * $400 )
	; reg  3: $ff pattern color table base address $2000 ( bit#7=A13 + bit#0-6 = 1)
	; reg  4: $03 pattern generator table base address $0000 ( bit#2-5=A13-A16 + bit#0+1 = 1)
	; reg  5: $3f sprite attribute table base address $1e00 (* $80 - bit#0+1 = 1)
	; reg  6: $03 sprite pattern (data) generator base address = $1800 (* $800)
	; reg  7: $10 text/overscan-backdrop color 
	; reg  8: $08 bit#3 = 1: 64k VRAM chips, bit#1 = 0 sprites enable, bit#5 0=transparent
	; reg  9: $80 bit#1 = NTSC/PAL, #2 = EVEN/ODD, #3 = interlace, #7 = 192/212 lines
	; reg 10: $00 pattern color table base address bit#0-2 = A14-A16
	; reg 11: $00 sprite attribute table base address bit#0-1 = A15-A16
	; reg 12: $20 text/background blink color
	; reg 13: $f0 blink periods ON/OFF - f0 = blinking off
	; reg 14: $00 VRAM write addresss bit#0-2 = A14-A16
VdpInitDataEnd:

PaletteData:
	!byte $00,$00,$27,$04,$17,$02,$05,$02	;	0=black/tra	1=blue7		2=blue6		3=blue5
	!byte $05,$01,$04,$01,$03,$00,$02,$00	;	4=blue4		5=blue3		6=blue2		7=blue1
	!byte $72,$03,$70,$02,$60,$01,$50,$00	;	8=red7		9=red6		10=red5		11=red4
	!byte $40,$00,$30,$00,$20,$00,$77,$07	;	12=red3		13=red2		14=red1		15=white
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
!byte 15,1,1,2,2,3,3,3,4,4,4,4,4,4,4,4
!byte 15,1,1,2,2,3,3,3,4,4,4,4,4,4,4,4
!byte  5,5,5,5,5,5,5,5,5,5,5,6,6,6,7,7
!byte  5,5,5,5,5,5,5,5,5,5,5,6,6,6,7,7
!byte 15,1,1,2,2,3,3,3,4,4,4,4,4,4,4,4
!byte 15,1,1,2,2,3,3,3,4,4,4,4,4,4,4,4
!byte  5,5,5,5,5,5,5,5,5,5,5,6,6,6,7,7
!byte  5,5,5,5,5,5,5,5,5,5,5,6,6,6,7,7
!byte 15,1,1,2,2,3,3,3,4,4,4,4,4,4,4,4
!byte 15,1,1,2,2,3,3,3,4,4,4,4,4,4,4,4
!byte  5,5,5,5,5,5,5,5,5,5,5,6,6,6,7,7
!byte  5,5,5,5,5,5,5,5,5,5,5,6,6,6,7,7
!byte 15,8,8,9,9,9,10,10,10,10,10,10,11,11,11,11
!byte 15,8,8,9,9,9,10,10,10,10,10,10,11,11,11,11
!byte 11,11,11,11,11,12,12,12,12,12,12,13,13,13,14,14
!byte 11,11,11,11,11,12,12,12,12,12,12,13,13,13,14,14
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80	; sprites 16-31 invisible (x=-32)
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
!byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
SpriteColorDataEnd:

!align $ff,$fe 	; align bitmaps to page border -2 (data at border) for fast indexed access!
BitmapData:
;	!binary "za.rgb"
!align $ff,$00 	; align to page border for fast indexed access!
FontData:
;	!binary "deadtest.fon"