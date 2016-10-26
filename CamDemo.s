	include exec/exec_lib.i
	include exec/execbase.i
	include exec/libraries.i
	include exec/types.i
	include graphics/graphics_lib.i
	include graphics/gfxbase.i
	include intuition/intuition_lib.i
	include macros.i

	SECTION CamDemoCode,CODE
	
********CONSTANTS**************
screenWidth	= 352
screenHeight	= 256
bitplaneSize	= screenWidth*screenHeight/8
screenBitplane	= screenWidth/8

logoWidth	= 128				;rounded up to multiple of 16
logoHeight	= 69
logoMargin	= (320-logoWidth)/2
logoBitplane	= logoWidth/8
logoBitWidth	= logoBitplane*3

fontWidth	= 288				;rounded up to multiple of 16
fontHeight	= 100
fontBitplanes	= 3
fontBitplane	= fontWidth/8

plotY		= 134
plotX		= screenWidth-32

************MUSIC PLAYER*******

P61mode	=1	;Try other modes ONLY IF there are no Fxx commands >= 20.
		;(f.ex., P61.new_ditty only works with P61mode=1)


;;    ---  options common to all P61modes  ---

usecode	=$c009601	;CHANGE! to the USE hexcode from P61con for a big 
		;CPU-time gain! (See module usecodes at end of source)
		;Multiple songs, single playroutine? Just "OR" the 
		;usecodes together!

		;...STOP! Have you changed it yet!? ;)
		;You will LOSE RASTERTIME AND FEATURES if you don't.

P61pl=usecode&$400000

split4	=0	;Great time gain, but INCOMPATIBLE with F03, F02, and F01
		;speeds in the song! That's the ONLY reason it's default 0.
		;So ==> PLEASE try split4=1 in ANY mode!
		;Overrides splitchans to decrunch 1 chan/frame.
		;See ;@@ note for P61_SetPosition.


splitchans=1	;#channels to be split off to be decrunched at "playtime frame"
		;0=use normal "decrunch all channels in the same frame"
		;Experiment to find minimum rastertime, but it should be 1 or 2
		;for 3-4 channels songs and 0 or 1 with less channels.

visuctrs=1	;enables visualizers in this example: P61_visuctr0..3.w 
		;containing #frames (#lev6ints if cia=1) elapsed since last
		;instrument triggered. (0=triggered this frame.)
		;Easy alternative to E8x or 1Fx sync commands.

asmonereport	=0	;ONLY for printing a settings report on assembly. Use
			;if you get problems (only works in AsmOne/AsmPro, tho)

p61system=0	;1=system-friendly. Use for DOS/Workbench programs.

p61exec	=0	;0 if execbase is destroyed, such as in a trackmo.

p61fade	=0	;enable channel volume fading from your demo

channels=4	;<4 for game sound effects in the higher channels. Incompatible
		; with splitchans/split4.

playflag=0	;1=enable music on/off capability (at run-time). .If 0, you can
		;still do this by just, you know, not calling P61_Music...
		;It's a convenience function to "pause" music in CIA mode.

p61bigjtab=0	;1 to waste 480b and save max 56 cycles on 68000.

opt020	=0	;1=enable optimizations for 020+. Please be 68000 compatible!
		;splitchans will already give MUCH bigger gains, and you can
		;try the MAXOPTI mode.

p61jump	=0	;0 to leave out P61_SetPosition (size gain)
		;1 if you need to force-start at a given position fex in a game

C	=0	;If you happen to have some $dffxxx value in a6, you can 
		;change this to $xxx to not have to load it before P61_Music.

clraudxdat=0	;enable smoother start of quiet sounds. probably not needed.

optjmp	=1	;0=safety check for jump beyond end of song. Clear it if you 
		;play unknown P61 songs with erroneous Bxx/Dxx commands in them

oscillo	=0	;1 to get a sample window (ptr, size) to read and display for 
		;oscilloscope type effects (beta, noshorts=1, pad instruments)
		;IMPORTANT: see ;@@ note about chipmem dc.w buffer.

quietstart=0	;attempt to avoid the very first click in some modules
		;IMPORTANT: see ;@@ note about chipmem dc.w buffer.

use1Fx=0	;Optional extra effect-sync trigger (*). If your module is free
		;from E commands, and you add E8x to sync stuff, this will 
		;change the usecode to include a whole code block for all E 
		;commands. You can avoid this by only using 1Fx. (You can 
		;also use this as an extra sync command if E8x is not enough, 
		;of course.)



;;    ---  CIA mode options (default) ---

	ifeq P61mode-1

p61cia	=1	;call P61_Music on the CIA interrupt instead of every frame.

lev6	=1	;1=keep the timer B int at least for setting DMA.
		;0="FBI mode" - ie. "Free the B-timer Interrupt".

		;0 requires noshorts=1, p61system=0, and that YOU make sure DMA
		;is set at 11 scanlines (700 usecs) after P61_Music is called.
		;AsmOne will warn you if requirements are wrong.

		;DMA bits will be poked in the address you pass in A4 to 
		;P61_init. (Update P61_DMApokeAddr during playing if necessary,
		;for example if switching Coppers.)

		;P61_Init will still save old timer B settings, and initialize
		;it. P61_End will still restore timer B settings from P61_Init.
		;So don't count on it 'across calls' to these routines.
		;Using it after P61_Init and before P61_End is fine.

noshorts=0	;1 saves ~1 scanline, requires Lev6=0. Use if no instrument is
		;shorter than ~300 bytes (or extend them to > 300 bytes).
		;It does this by setting repeatpos/length the next frame 
		;instead of after a few scanlines,so incompatible with MAXOPTI

dupedec	=0	;0=save 500 bytes and lose 26 cycles - I don't blame you. :)
		;1=splitchans or split4 must be on.

suppF01	=1	;0 is incompatible with CIA mode. It moves ~100 cycles of
		;next-pattern code to the less busy 2nd frame of a notestep.
		;If you really need it, you have to experiment as the support 
		;is quite complex. Basically set it to 1 and try the various 
		;P61modes, if none work, change some settings.

	endc

;;    ---  VBLANK mode options ---

	ifeq P61mode-2

p61cia	=0
lev6	=1	;still set sound DMA with a simple interrupt.
noshorts=0	;try 1 (and pad short instruments if nec) for 1 scanline gain
dupedec	=0
suppF01	=P61pl	;if 1, split4=1 may cause sound errors. but try it anyway. :)
	
	endc

;;    ---  COPPER mode options ---

	ifeq P61mode-3

p61cia	=0
lev6	=0	;don't set sound DMA with an interrupt.
		;(use the copper to set sound DMA 11 scanlines after P61_Music)
noshorts=1	;You must pad instruments < 300 bytes for this mode to work.
dupedec	=0
suppF01	=P61pl	;if 1, split4=1 may cause sound errors. but try it anyway. :)

	endc

;;    ---  MAXOPTI mode options ---

	ifeq P61mode-4

p61cia	=0
lev6	=0
noshorts=1	;You must pad instruments < 300 bytes for this mode to work.
dupedec	=1
suppF01	=P61pl	;if 1, split4=1 may cause sound errors. but try it anyway. :)
	endc


********START******************
Start:
	;bsr Beep
	;rts

	movem.l d1-a6,-(SP)
	bra.s Init
	
InitDone:
	bsr.w Main
	bra.w Exit
		
********SUBROUTINES************
Init:
	lea graphicsname,a1
	CALLEXEC OpenLibrary			;open graphics.library
	beq ExitError
	move.l d0,a6
	move.l d0,_GfxBase
	
	move.l gb_ActiView(a1),-(SP)		;push active view to stack
	move.l 4.w,a1
	move.l 38(a1),copperPtr
	move.l 38(a1),-(SP)			;push copper pointer to stack
	
	move.w $dff002,-(SP)			;push DMACON to stack
	move.l #$7fff,$dff096			;disable all DMA
	move.l #$87e0,$dff096			;enable bitplane and copper DMA
	move.w #$8440,$dff096			;enable blitter nasty DMA
	
	move.l #0,a1				;null view pointer
	CALLGRAF LoadView			;loads default view
	move.w #$20,$dff1dc			;switch to 50Hz mode
	CALLGRAF WaitTOF			;waits for top of frame
	CALLGRAF WaitTOF			;waits for top of next frame
	move.l _GfxBase,a1
	CALLEXEC CloseLibrary
	
	move.w $dff01c,-(SP)			;push INTENA to stack
	move.w #$7fff,$dff09a			;disable all interrupts
	
	move.l #copper,$dff080			;load copperlist
	
	IF 1=0
	moveq #0,d1
	lea screen,a1
	move.w #bitplaneSize/2-1,d0
_clearloop:
	move.w d1,(a1)+
	addq.w #1,d1
	dbf d0,_clearloop
	ENDC
	
	lea logo,a0				;load bitplane pointer
	lea logoBpP,a1				;where to poke logo bitplanes
	moveq #3-1,d0				;start counter
	
LogoBitplanePointerLoop:
	move.l a0,d1
	move.w d1,6(a1)				;put low bp address in copper
	swap d1
	move.w d1,2(a1)				;put high bp address in copper
	addq #8,a1				;point to next bitplane pointer
	lea logoBitplane(a0),a0
	dbf d0,LogoBitplanePointerLoop

	lea spriteP,a1				;load sprite pointers
	lea nullSprite,a0			;load null sprite pointer
	move.l a0,d1
	moveq #8-1,d0				;start counter
	
SpritePointerLoop:
	move.w d1,6(a1)				;store low byte
	swap d1
	move.w d1,2(a1)				;store high byte
	addq #8,a1				;move to next sprite
	dbf d0,SpritePointerLoop
	
	lea saturnFontEnd-8*2,a0
	lea fontPaletteP+2,a1
	moveq #8-1,d0
	
Coll:
	move.w (a0)+,(a1)+
	addq.w #2,a1
	dbf d0,Coll

*****
	lea fontPaletteP,a0
	move.w #$00a,2(a0) 
		
;;    ---  Call P61_Init  ---
	movem.l d0-a6,-(sp)
	lea Module1,a0
	sub.l a1,a1
	sub.l a2,a2
	moveq #0,d0
	;lea p61coppoke+3,a4		;only used in P61mode >=3
	jsr P61_Init
	movem.l (sp)+,d0-a6

	
	bra InitDone
	
Main:
	movem.l d0-a6,-(SP)
	
MainLoop:
	move.w #$02a,d0				;wait for end of frame
	bsr.w WaitRaster

	bsr.w BounceScroller

	bsr.s ScrollIt
	
	moveq #32,d2
	move.b lastChar,d0
	cmp.b #'I',d0
	bne.s _notI
	moveq #16,d2
_notI:
	move.w scrollCounter,d0
	addq.w #4,d0
	cmp.w d2,d0
	blo.s _NoWrap
	
	move.l scrollP,a0
	cmp.l #scrollTextWrap,a0
	blo.s _noPlot
	lea scrollText,a0
_noPlot:
	bsr.s PlotChar				;preserves a0
	addq.w #1,a0
	move.l a0,scrollP
	
	clr.w d0
_NoWrap:
	move.w d0,scrollCounter
	
	btst #6,$bfe001				;check left mouse click
	bne.s MainLoop
	movem.l (SP)+,d0-a6
	
	rts
	
ScrollIt:
bltx		= 0
blty		= 135
bltoffs		= blty*(screenBitplane*3)+bltx/8

blth		= 20
bltw		= screenWidth/16
bltskip		= 0					;modulo
brcorner	= (blth-1)*(screenBitplane*3)+bltw*2-2

	movem.l d0-a6,-(SP)
	
	bsr BlitWait
	
	move.l #$49f00002,$dff040
	move.l #$ffffffff,$dff044
	move.l #screen+bltoffs+brcorner,$dff050
	move.l #screen+bltoffs+brcorner,$dff054
	move.w #bltskip,$dff064
	move.w #bltskip,$dff066
	
	move.w #blth*3*64+bltw,$dff058
	movem.l (SP)+,d0-a6
	rts
	
PlotChar:					;a0=scrollP
row	= 288*3*20/8
column	= 4

	movem.l d0-a6,-(SP)
	bsr BlitWait
	
	clr d0
	move.b (a0)+,d0				;ASCII value
	move.b d0,lastChar
	
	sub.w #32,d0
	lea fontTable,a0
	move.b (a0,d0.w),d0
	divu #9,d0				;row
	move.l d0,d1
	swap d1					;remainder (column)
	
	mulu #row,d0
	mulu #column,d1
	
	add.l d1,d0				;offset into font bitmap
	add.l #saturnFont,d0
	
	move.l #$09f00000,$dff040				;BLTCON0
	move.l #$ffffffff,$dff044				;BLTAFWM
	move.l d0,$dff050					;BLTAPTH
	move.l #screen+screenBitplane*3*plotY+plotX/8,$dff054	;BLTDPTH
	move.w #fontBitplane-column,$dff064			;BLTAMOD		;BLTAMOD
	move.w #screenBitplane-column,$dff066			;BLTDMOD
	  
	move.w #20*3*64+2,$dff058				;BLTSIZE
	
	movem.l (SP)+,d0-a6
	rts
	
BlitWait:
	tst $dff002				;for compatibility
_waitblit:
	btst #6,$dff002				;check blitter busy bit
	bne.s _waitblit
	rts
	
WaitRaster:			;wait for rasterline d0.w. Modifies d0-d2/a0
	move.l #$1ff00,d2
	lsl.l #8,d0
	and.l d2,d0
	lea $dff004,a0
_wr:
	move.l (a0),d1	
	and.l d2,d1
	cmp.l d1,d0
	bne.s _wr
	rts

BounceScroller:
	movem.l d0-a6,-(SP)
	lea screen,a0				;pointer to 1st screen bitplane
	move.w bounceY,d0
	move.w bounceYAccel,d1
	add.w d1,bounceYSpeed
	add.w bounceYSpeed,d0
	bpl.s _noBounce
	move.w #16,bounceYSpeed			;bounce height
	clr.w d0
	
		
_noBounce:
	move.w d0,bounceY
	
	mulu #3*screenBitplane,d0				
	add.l d0,a0
	
	lea screenBpP,a1			;where to poke bitplane ptrs
	moveq #fontBitplanes-1,d0 
	
ScreenBitplanePointerLoop:
	move.l a0,d1
	move.w d1,6(a1)
	swap d1
	move.w d1,2(a1)
	addq #8,a1				;point to next bitplane pointer
	lea screenBitplane(a0),a0
	
	dbf d0,ScreenBitplanePointerLoop
	
	movem.l (SP)+,d0-a6
	rts

Beep:
	movem.l d0/a0/a6,-(SP)
	lea intuitionname,a1			;load intuition.library
	CALLEXEC OpenLibrary
	beq ExitError
	move.l d0,_IntuitionBase
	move.l #0,a0				;flash all screens
	CALLINT DisplayBeep			;beep
	move.l _IntuitionBase,a1
	CALLEXEC CloseLibrary			;close library
	movem.l (SP)+,d0/a0/a6
	rts

	
ExitError:
	moveq.l #1,d0
	rts
	
Exit:
	movem.l d0-a6,-(sp)
	jsr P61_End				;stop music
	movem.l (sp)+,d0-a6

	move.w (SP)+,d0				;restore INTENA
	or.w #$c000,d0
	move.w d0,$dff09a
	move.w (SP)+,d0				;restore DMACON
	move.w #$7fff,$dff096
	or.w #$8200,d0
	move.w d0,$dff096
	move.w #$000f,$dff096			;make sure sound DMA is off
	
	move.l (SP)+,a0				;store old copperlist
	
	lea graphicsname,a1			;load graphics.library
	CALLEXEC OpenLibrary
	beq ExitError
	;move.l d0,a6
	move.l d0,_GfxBase
	move.l (SP)+,a1
	CALLGRAF LoadView			;restore original screenmode
	move.l _GfxBase,a1
	CALLEXEC CloseLibrary			;close graphics.library
	
	move.l a0,$dff080			;restore copperlist
	lea copperPtr,a0
	move.l a0,$dff080

	movem.l (SP)+,d1-a6
	clr.l d0				;return 0
	rts
	

********** DATA  **********

gfxname:
	dc.b	"graphics.library",0
	even

********** PLAYROUTINE CODE **********

Playrtn:
	include "P6112-Play.i"

	
***********FAST RAM DATA**************
	SECTION CamDemoData,DATA
fontTable:
	dc.b 43,38
	dcb.b 5,0
	dc.b 42
	dcb.b 4,0
	dc.b 37,40,36,41
	dc.b 26,27,28,29,30,31,32,33,34,35
	dcb.b 5,0
	dc.b 39,0
	dc.b 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14
	dc.b 15,16,17,18,19,20,21,22,23,24,25
	EVEN

scrollP:
	dc.l scrollText
	
scrollText:
	dc.b "BOING BOING BOING FUCKING BOING "
scrollTextWrap:

lastChar:
	dc.b 0

	EVEN

scrollCounter:
	dc.w 0
	
bounceY:
	dc.w 48

bounceYSpeed:
	dc.w 0
	
bounceYAccel:
	dc.w -1

_GfxBase:
	dc.l 0

graphicsname:
	dc.b "graphics.library",0

_IntuitionBase:
	dc.l 0
	
intuitionname:
	dc.b "intuition.library",0

***********CHIP RAM DATA**************	
	SECTION CamDemoChipData,DATA_C
	
copper:
	dc.w $1fc,0				;AGA compatibility
	dc.w $100,$3200				;BPLCON set to 3 bitplane
	dc.w $8e,$3e81				;DIWSTRT
	dc.w $90,$2cc1				;DIWSTOP
	dc.w $92,$38+logoMargin/2		;DDFSTART
	dc.w $94,$d0-logoMargin/2		;DDFSTOP
	dc.w $108,logoBitWidth-logoBitplane	;modulo 0 odd
	dc.w $10a,logoBitWidth-logoBitplane	;modulo 0 even
	dc.w $102,0				;scrolling 0

logoBpP:
	dc.w $00e0,0				;BPL1PTH set to logo
	dc.w $00e2,0				;BPL1PTL set to logo	
	dc.w $00e4,0				;BPL2PTH set to logo
	dc.w $00e6,0				;BPL2PTL set to logo
	dc.w $00e8,0				;BPL3PTH set to logo
	dc.w $00ea,0				;BPL3PTL set to logo
	
spriteP:
	dc.w $120,0
	dc.w $122,0
	dc.w $124,0
	dc.w $126,0
	dc.w $128,0
	dc.w $12a,0
	dc.w $12c,0
	dc.w $12e,0
	dc.w $130,0
	dc.w $132,0
	dc.w $134,0
	dc.w $136,0
	dc.w $138,0
	dc.w $13a,0
	dc.w $13c,0
	dc.w $13e,0

topBorder:
	dc.w $180,$00a				;change bg colour to blue
	dc.w $182,$fff				;change fg colour to white
	
	dc.w $2c07,$fffe			;wait for line $2c
	dc.w $180,$0f0				;change bg colour to green
	dc.w $2d07,$fffe			;wait for next line

mainWindow:
	include CamLogoCopper.s

	dc.w $8107,$fffe
	dc.w $100,$3200
	dc.w $81df,$fffe
		
screenBpP:
	dc.w $e0,0
	dc.w $e2,0
	dc.w $e4,0
	dc.w $e6,0
	dc.w $e8,0
	dc.w $ea,0
	dc.w $108,screenBitplane*3-320/8
	dc.w $10a,screenBitplane*3-320/8
	dc.w $92,$38
	dc.w $94,$d0
	dc.w $100,fontBitplanes*$1000+$200
	
bottomBorder:
	dc.w $2d07,$fffe			;wait for line $2d
	dc.w $180,$000				;change bg colour to black
	dc.w $fe07,$fffe			;wait for line 254
	dc.w $180,$0f0				;change bg colour to green
	dc.w $ff07,$fffe			;wait for line 255
	dc.w $180,$000a				;change bg colour to blue

fontPaletteP:
	dc.w $180,0
	dc.w $182,0
	dc.w $184,0
	dc.w $186,0
	dc.w $188,0
	dc.w $18a,0
	dc.w $18c,0
	dc.w $18e,0
	
	dc.w $ffdf,$fffe
	dc.w $2c07,$fffe
	dc.w $180,$000a
	dc.w $2d07,$fffe
	dc.w $180,$0
	
	dc.w $ffff,$fffe			;end copperlist
copperEnd:

copperPtr:
	dc.l 0

Module1:
	incbin "P61.HappyBirthday"

saturnFont:
	INCBIN "SaturnFont.284x100x3"
saturnFontEnd:
	
nullSprite:
	dc.w $2a20,$2b00
	dc.w 0,0
	dc.w 0,0

logo:
	incbin CamLogoRaw123x69x3	
logoEnd:
	
	SECTION CamDemoBSS,BSS_C	
screen:
	dcb.b bitplaneSize*fontBitplanes,0
screenEnd:

