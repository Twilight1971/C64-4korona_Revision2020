
//------------------------------------------
//		4korona INTRO by TWILIGHT
//			for Revision 2020 	
//------------------------------------------

.var zp_base	= $20

      .pc = $0800 "logo charset"
      .import binary "animchar2.bin"			
      
      
 //     .pc = $0c00 "logo screen"
 //     .import binary "rasterlogo.piscr",2
      
      
.pc = $1000 "music"
      .import binary "X-Large_6 1000 1003 jeff.dat",2

.var musicplay = $1003
.var musicinit = $1000

//.pc = $0801 "Basic Program"
//:BasicUpstart($2000)
		.pc = $1600 "main code"



					jsr savepage
					ldy #$00
count1:				lda $0400,x
					cmp #$20
					bne count2
					inx
					bne count1
					inc count1+2
					inc count2+2
					lda count1+2
					cmp #$08
					bne count1
					lda #$04
					sta count1+2
					sta count2+2
					iny
					bne count1
					
					jmp jump1
					
count2:				dec $0400,x
					inx
					bne count1
					inc count1+2
					inc count2+2
					lda count1+2
					cmp #$08
					bne count1
					lda #$04
					sta count1+2
					sta count2+2
					iny
					bne count1
jump1:				

			lda #$08
			sta $0286
			jsr $e544
			lda #$00
			sta $d022
			lda #$0b
			sta $d020
			lda #$0b
			sta $d021
			lda #$00
			jsr musicinit
			jsr disable
			
			
				lda #<scrolltext1
				sta zp_base+0
				lda #>scrolltext1
				sta zp_base+1
				lda #<scrolltext1+6
				sta scrollread+1
				lda #>scrolltext1
				sta scrollread+2
			
			ldx #$00
lop11:		lda #$00
			sta $dbbe-40*3,x
			sta $dbbe-40*1,x
			inx
			cpx #$50
			bne lop11
			
			ldx #$00
!:			lda line1,x
			adc #$40
			sta $0fc0-40*1,x
			lda line2,x
			adc #$40
			sta $0fc0,x
			inx
			cpx #$27
			bne !-

			
        	sei
        	lda #$7F
        	sta $DC0D    
        	sta $DD0D    
        	lda $DC0D    
        	lda $DD0D    
        	lda #$01
        	sta $D01A    
        	lda #$31
        	sta $D012    
        	lda #<INT0
        	sta $0314    
        	lda #>INT0
        	sta $0315    
        	cli
        	
        	jmp *
        	
line1:        	
//     1234567890123456789012345678901234567890
.text "      4k0r0na intro revision 2k20       "

line2:
.text "            - welcome home -            "
        	
CheckKB:	lda $dc00    
        	eor $dc01    
        	asl
        	bne exit1     
        	rts
exit1:     	jmp exit


//----- RESTORE DEAD -----------------------------------------------
disable:        lda #<nmi             //Set NMI vector
                sta $0318
                sta $fffa
                lda #>nmi
                sta $0319
                sta $fffb
                lda #$81
                sta $dd0d             //Use Timer A
                lda #$01              //Timer A count ($0001)
                sta $dd04
                lda #$00
                sta $dd05
                lda #%00011001        //Run Timer A
                sta $dd0e
                rts

nmi:            rti
//----------------------------------------------------


INT0:    	
			lda $d021
			sta $d020
			lda $dd00
			and #$fc
			ora #$03
			sta $dd00
			dec screen+1
screen:		ldx #$00
			lda $1400,x
			sta $d018								
			lda #$d0				
			sta $d016				
			lda #$1b				
			sta $d011				
        	ldx #<INT1
        	ldy #>INT1
        	lda #$31
        	jsr INTe
 			jsr CheckKB
        	jsr musicplay
        	
        	jmp $ea81
        	
        	
      
      
        	
         
.align $0100
//------------------------------------------------------------------------------
// Rasters  logo oben

INT1:	   	ldy #$0d
lop1:      	dey
        	bpl lop1
        	nop
        	bit $EA
        	ldx #$00
INT1_J1: 	lda color1,x
			sta $D022    
        	sta $D020   
logoc1:    	lda color11,x
        	sta $D023   
        	nop
        	nop
        	inx
        	ldy #$06
INT1_J2: 	lda color1,x
			sta $D022    
        	sta $D020    
logoc11:   	lda color11,x
			sta $D023    
        	jsr Delay
        	bpl INT1_J2
        	nop
        	cpx #$a0    //c0
        	bne INT1_J1
        	
		//	lda #[[$0c00 & $3fff] / 64] | [[$d400 & $3fff] / 1024]
			
			lda $dd00
			and #$fc
			ora #$03
			sta $dd00
			
			
			lda #$32
			sta $d018


			lda $d020	
			sta $d021	
        	lda movs
        	sta $d016
        	ldx #<INT0
        	ldy #>INT0
        	lda #$f2
        	jsr INTe
        	
        	ldx #$aa
      !:  	dex
        	bne !-
        	lda #$c8
        	sta $d016
 			jsr scroll
        	jsr rasterro
        	jsr anim
        	jmp $ea81


//------------------------------------------------------------------------------
// Exit the interrupt
INTe:  	  	stx $0314    
        	sty $0315    
        	sta $D012    
        	inc $D019    
        	rts
//------------------------------------------------------------------------------
.align $0100
Delay:   	
			lda ($ea,x)
        	lda ($ea,x)
        	lda ($ea,x)
        	inx
        	dey
        	nop
        	nop
        	rts

//-------------------------------------------------------------------------
rasterro:	
			lda color11+$a0
            sta color11+$00
            ldx #$a0
cycle:      lda color11-$01,x
            sta color11+$00,x
            dex
            bne cycle
            
            
			lda color1+$00
            sta color1+$a0
            ldx #$00
cycle1:     lda color1+$01,x
            sta color1+$00,x
            inx
            cpx #$a0
            bne cycle1
            lda color1+$50
			sta $d027
			sta $d028
            
			
			rts


anim:			
			ldx #$00
!:			lda $0810,x
			sta $0890,x
			inx
			cpx #$08
			bne !-
			ldx #$00		
!:			lda $0818,x		
			sta $0810,x		
			inx
			cpx #$80
			bne !-
        	rts



//-------------------------------------------------------------------------
            
.var scrline     = $0fc0-40*3




movs:							.byte $c7
scrollbyte:						.byte $0f
scrollspeed_byte:				.byte $04
scroll:
scroller:				
				lda #$00							// stop routine
				cmp #$00
				beq !+
				dec scroller+1
				rts
!:						
				lda scrollbyte
				sec
				sbc scrollspeed_byte
				and #$07
				sta scrollbyte
				php
				lda scrollbyte
				ora #$00
				sta movs
				plp
				bcc !+
				rts
!:								
				.for (var i=0; i<40; i++) {
				lda scrline+i+$01
				sta scrline+i+$00
}											
				ldy #$00
				lda (zp_base),y
				bne !+ 
				lda #<scrolltext1
				sta zp_base+0
				lda #>scrolltext1
				sta zp_base+1
				rts
!:								
				cmp #$40						// stop und speed "commands" ignorieren
				bcc !+
				lda #$20						// und mit einem leerzeichen ersetzen
!:
				adc #$40
				sta scrline+$27	
scrollread:
				lda scrolltext1
				bne !+
				lda #<scrolltext1
				sta scrollread+1
				lda #>scrolltext1
				sta scrollread+2
				jmp ende
!:								
				cmp #$40							// stop "command"
				bne !+
				lda #$40							// "zeitverzögerung", kann indv. angepasst werden
				sta scroller+1
				jmp loop1

!:				cmp #$41							// speed "command"
				bcc !+
				sec
				sbc #$40
				sta scrollspeed_byte
				lda #$20							// "command" mit leerzeichen ersetzen
loop1:
!:
			    lda scrline+$01
				cmp #$40						// stop und speed "commands" ignorieren
				bcc !+
				lda #$20						// und mit einem leerzeichen ersetzen
!:
				inc zp_base+0
				bne !+
				inc zp_base+1
!:
				inc scrollread+1
				bne !+
				inc scrollread+2
!:			
ende:								
				rts
				


//-------------------------------------------------------------------------

        	
		
//-------------------------------------------------------------------------

.align $0100
color11:		
//.text "i ih ihe ihec ihecacehi cehi ehi hi ib ibj ibjg ibjgagjbi gjbi jbi bi ik kl klo klog klogagolk golk olk lk kf fk fkn fknc fkncacnkf cnkf nkf kf f i ib ibj ibjg ibjgagjbi gjbi jbi bi i"
//.text " a a a a a a a a a a a a a a a a a a a a a a a                                                                                                                                       "
.text "kkkkkkkkllllllllooooooooggggggggaaaaaaaaggggggggoooooooollllllllkkkkkkkkkkkkkkkkkkkkkkkkiiiiiiiihhhhhhhheeeeeeeeccccccccaaaaaaaacccccccceeeeeeeehhhhhhhhiiiiiiiikkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
//.text "i@ii@ihihhheheeececccgcgggagaaagagggogooojgjjjhjhhhbhbbbibiiififffkfkkknknnncncccgcgggagaaagagggcgcccncnnnknkkkfkfffifiiibibbbhbhhhjhjjjojooogogggagaaagagggogooojgjjjhjhhhbhbbbibiiiiiii"
//-------------------------------------------------------------------------

//.pc = $1500 "raster colors"
.align $0100
color1:			
.text "ffffffffkkkkkkkknnnnnnnnccccccccaaaaaaaaccccccccnnnnnnnnkkkkkkkkffffffffkkkkkkkkkkkkkkkkiiiiiiiibbbbbbbbhhhhhhhhjjjjjjjjggggggggaaaaaaaaggggggggjjjjjjjjhhhhhhhhbbbbbbbbiiiiiiiikkkkkkkkkkkkkkkkkkkkkkkkk"		
//.text "kkkkkkkkllllllllooooooooggggggggaaaaaaaaggggggggoooooooollllllllkkkkkkkkkkkkkkkkkkkkkkkkiiiiiiiihhhhhhhheeeeeeeeccccccccaaaaaaaacccccccceeeeeeeehhhhhhhhiiiiiiiikkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"

//.text "					 "		

/*

color31:	//	.text "                 fkncacnkf cnkf nkf kf f f fk fkn fknc fkncacnkf    "
			//	.text "                 ih ihe ihec iheca ihecaaacehi acehi cehi ehi hi    "
				.text "                 ib ibj ibjg ibjga ibhjgaaagjbi gjbi gjbi jbi ib    "
				
				
color33:		.text " k         k      k    k   k  k kk kkk kkkkk kkkkkkk kkkkkkkkkk"
                .text "kkkkkkkkkkkkkkkkkkk kkkkkkkkk kkkkkk kkkkk kkkk kk k   k    k      k        k           k    " 

// raster im logo oben					
color3:			   
			//	.text "                 klogagolk golk olk lk k k kl klo klog klogagolk    "
				.text "                 fk fkn fknc fknca fkncaaacnkf acnkf cnkf nkf kf    "
			//	.text "                 kl klo klog klogaaaaaaaaaaaaaaagolk golk olk lk    "
*/					



savepage:   
					lda $0318	
					sta s0318+1	
					lda $0319	
					sta s0319+1	
					ldx #$00
		!l:			lda $0200,x
					sta Page2S,x
					inx
					bne !l-
					rts




.pc = $0c00 "alien1 screen"     
.byte 3,1,15,14,12,11,10,9,8,8,7,7,7,7,8,8,9,9,11,11,12,14,0,1,3,4,6,8,9,11,13,15,1,3,5,8,10,12,5,3
.byte 1,15,13,12,19,9,8,7,6,5,19,5,5,5,6,21,21,21,10,21,11,21,15,21,21,21,5,21,21,21,12,21,21,21,4,21,21,21,3,1
.byte 15,13,19,10,8,19,6,5,4,19,3,3,19,3,4,21,6,6,8,21,10,21,14,21,1,3,5,21,8,10,11,21,15,1,3,21,8,10,1,15
.byte 13,11,19,8,19,19,19,19,19,19,19,1,19,1,2,21,4,4,7,21,9,21,14,21,1,2,4,21,7,9,11,21,15,1,3,21,7,10,0,13
.byte 11,9,19,19,19,3,19,19,19,15,19,19,19,15,0,21,21,2,7,7,21,10,14,21,1,2,4,21,21,8,10,21,21,21,2,21,21,21,14,12
.byte 9,7,19,19,19,19,19,19,19,19,19,19,19,13,14,21,1,1,6,21,8,21,14,21,1,2,4,21,7,8,10,12,14,21,2,4,6,21,13,10
.byte 8,6,4,19,19,19,19,19,19,19,19,19,10,10,11,21,15,15,6,21,9,21,15,21,1,3,4,21,7,8,10,11,13,21,1,3,6,21,11,9
.byte 7,4,3,1,19,19,19,19,19,19,19,8,8,8,9,21,13,13,6,21,10,21,0,21,2,3,5,21,7,8,10,11,13,21,1,3,5,21,11,8
.byte 6,4,2,0,19,12,11,10,8,7,19,6,5,5,5,21,21,21,9,21,13,21,0,21,21,21,4,21,21,21,9,21,21,21,0,21,21,21,9,6
.byte 4,2,1,19,14,12,10,9,8,7,6,19,5,4,5,6,9,8,9,9,12,14,4,4,4,5,6,7,8,9,10,12,13,15,1,3,5,8,8,6
.byte 4,1,15,13,11,10,8,6,5,4,3,1,0,15,14,12,8,8,8,8,5,4,3,4,4,4,5,6,7,19,10,11,13,15,1,19,5,7,8,5
.byte 3,0,20,20,20,9,20,6,20,3,20,20,20,15,20,20,20,8,20,20,20,4,20,20,20,6,6,7,8,9,19,12,13,15,19,3,5,7,7,4
.byte 2,0,20,12,10,8,20,5,20,2,20,15,14,12,20,8,4,4,20,11,8,6,20,6,6,7,7,8,9,19,19,19,19,19,19,19,5,8,6,4
.byte 1,15,20,11,9,8,20,4,20,1,20,14,12,11,20,6,3,3,20,12,10,8,20,8,8,8,8,9,19,19,12,19,19,19,2,19,19,8,6,3
.byte 1,14,20,20,9,7,5,20,1,0,20,12,11,9,20,20,2,2,20,20,20,10,20,20,20,9,9,19,19,19,19,19,19,19,19,19,19,19,6,3
.byte 1,14,20,10,8,6,20,3,20,15,20,12,10,8,20,4,2,2,14,14,20,11,11,10,20,10,10,19,19,19,19,19,19,19,19,19,19,19,5,3
.byte 0,14,20,10,8,6,20,2,20,15,20,11,9,7,20,4,2,2,14,14,20,12,12,11,20,11,11,19,12,19,19,19,19,19,19,19,8,19,5,3
.byte 0,14,20,10,8,6,20,2,20,14,20,11,9,7,20,4,2,2,15,15,20,13,13,12,20,12,13,19,14,19,0,1,2,3,5,19,9,19,5,3
.byte 0,14,20,20,20,5,20,2,20,14,20,20,20,7,20,20,20,2,20,20,20,14,20,20,20,13,14,14,15,0,19,19,3,19,19,8,10,12,6,3
.byte 1,14,12,10,8,6,4,2,0,14,12,11,9,7,6,4,3,3,1,1,0,15,15,15,15,15,15,15,0,1,2,3,4,6,7,9,11,13,1,2
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

      .pc = $2000 "alien2 screen2"   
.byte 3,1,15,14,12,11,10,9,8,8,7,7,7,7,8,8,9,9,11,11,12,14,0,1,3,4,6,8,9,11,13,15,1,3,5,8,10,12,5,3
.byte 1,15,13,12,19,9,8,7,6,5,19,5,5,5,6,20,20,20,10,20,11,20,15,20,20,20,5,20,20,20,12,20,20,20,4,20,20,20,3,1
.byte 15,13,11,10,8,19,6,5,4,19,3,3,3,3,4,20,6,6,8,20,10,20,14,20,1,3,5,20,8,10,11,20,15,1,3,20,8,10,1,15
.byte 13,11,9,8,19,19,19,19,19,19,19,1,1,1,2,20,4,4,7,20,9,20,14,20,1,2,4,20,7,9,11,20,15,1,3,20,7,10,0,13
.byte 11,9,8,19,19,3,19,19,19,15,19,19,15,15,0,20,20,2,7,7,20,10,14,20,1,2,4,20,20,8,10,20,20,20,2,20,20,20,14,12
.byte 9,7,19,19,19,19,19,19,19,19,19,19,19,13,14,20,1,1,6,20,8,20,14,20,1,2,4,20,7,8,10,12,14,20,2,4,6,20,13,10
.byte 8,6,19,19,19,19,19,19,19,19,19,19,19,10,11,20,15,15,6,20,9,20,15,20,1,3,4,20,7,8,10,11,13,20,1,3,6,20,11,9
.byte 7,4,19,1,19,19,19,19,19,19,19,8,19,8,9,20,13,13,6,20,10,20,0,20,2,3,5,20,7,8,10,11,13,20,1,3,5,20,11,8
.byte 6,4,19,0,19,12,11,10,8,7,19,6,19,5,5,20,20,20,9,20,13,20,0,20,20,20,4,20,20,20,9,20,20,20,0,20,20,20,9,6
.byte 4,2,1,15,14,19,19,9,19,19,6,5,5,4,5,6,9,8,9,9,12,14,4,4,4,5,6,7,8,9,10,12,13,15,1,3,5,8,8,6
.byte 4,1,15,13,11,10,8,6,5,4,3,1,0,15,14,12,8,8,8,8,5,4,3,4,4,4,5,6,7,19,10,11,13,15,1,19,5,7,8,5
.byte 3,0,21,21,21,9,21,6,21,3,21,21,21,15,21,21,21,8,21,21,21,4,21,21,21,6,6,19,8,9,19,12,13,15,19,3,5,19,7,4
.byte 2,0,21,12,10,8,21,5,21,2,21,15,14,12,21,8,4,4,21,11,8,6,21,6,6,7,7,19,9,19,19,19,19,19,19,19,5,19,6,4
.byte 1,15,21,11,9,8,21,4,21,1,21,14,12,11,21,6,3,3,21,12,10,8,21,8,8,8,8,19,19,19,12,19,19,19,2,19,19,19,6,3
.byte 1,14,21,21,9,7,5,21,1,0,21,12,11,9,21,21,2,2,21,21,21,10,21,21,21,9,9,19,19,19,19,19,19,19,19,19,19,19,6,3
.byte 1,14,21,10,8,6,21,3,21,15,21,12,10,8,21,4,2,2,14,14,21,11,11,10,21,10,10,11,19,19,19,19,19,19,19,19,19,9,5,3
.byte 0,14,21,10,8,6,21,2,21,15,21,11,9,7,21,4,2,2,14,14,21,12,12,11,21,11,11,12,12,19,19,19,19,19,19,19,8,10,5,3
.byte 0,14,21,10,8,6,21,2,21,14,21,11,9,7,21,4,2,2,15,15,21,13,13,12,21,12,13,13,14,19,0,1,2,3,5,19,9,11,5,3
.byte 0,14,21,21,21,5,21,2,21,14,21,21,21,7,21,21,21,2,21,21,21,14,21,21,21,13,14,14,19,0,1,2,3,4,6,8,19,12,6,3
.byte 1,14,12,10,8,6,4,2,0,14,12,11,9,7,6,4,3,3,1,1,0,15,15,15,15,15,15,15,0,1,2,3,4,6,7,9,11,13,1,2
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20


      .pc = $2400 "excess screen4"   
.byte 1,2,3,4,5,6,7,8,9,10,11,21,21,14,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10,11,12
.byte 2,3,20,20,20,20,20,9,21,21,12,21,21,15,20,20,20,20,20,9,21,21,21,21,21,3,20,20,20,20,20,5,21,21,21,21,21,11,12,13
.byte 3,4,20,20,7,8,20,10,21,21,13,21,21,14,20,20,11,10,20,8,21,21,5,4,21,2,20,20,3,4,20,6,21,21,9,10,21,12,13,14
.byte 4,5,20,20,8,9,10,11,21,21,14,21,21,13,20,20,10,9,8,7,21,21,4,3,2,1,20,20,4,5,6,7,21,21,10,11,12,13,14,15
.byte 5,6,20,20,9,10,11,12,21,21,15,21,21,12,20,20,9,8,7,6,21,21,3,2,1,2,20,20,5,6,7,8,21,21,11,12,13,14,15,14
.byte 6,7,20,20,10,11,12,13,21,21,21,21,21,11,20,20,8,7,6,5,21,21,2,1,2,3,20,20,6,7,8,9,21,21,12,13,14,15,14,13
.byte 7,8,20,20,20,20,13,14,15,14,21,12,11,10,20,20,7,6,5,4,21,21,21,21,3,4,20,20,20,20,20,10,21,21,21,21,21,14,13,12
.byte 8,9,20,20,12,13,14,15,21,21,21,21,21,9,20,20,6,5,4,3,21,21,2,3,4,5,6,7,8,20,20,11,12,13,14,21,21,13,12,11
.byte 9,10,20,20,13,14,15,14,21,21,11,21,21,8,20,20,5,4,3,2,21,21,3,4,5,6,7,8,9,20,20,12,13,14,15,21,21,12,11,10
.byte 10,11,20,20,14,15,14,13,21,21,10,21,21,7,20,20,4,3,2,1,21,21,4,5,6,7,8,9,10,20,20,13,14,15,14,21,21,11,10,9
.byte 11,12,20,20,15,14,13,12,21,21,9,21,21,6,20,20,3,2,1,2,21,21,5,6,7,8,9,10,11,20,20,14,15,14,13,21,21,10,9,8
.byte 12,13,20,20,14,13,12,11,21,21,8,21,21,5,20,20,2,1,2,3,21,21,6,7,8,9,10,11,12,20,20,15,14,13,12,21,21,9,8,7
.byte 13,14,20,20,13,12,11,10,21,21,7,21,21,4,20,20,1,2,3,4,21,21,7,8,9,10,11,12,13,20,20,14,13,12,11,21,21,8,7,6
.byte 14,15,20,20,12,11,10,9,21,21,6,21,21,3,20,20,2,3,4,5,21,21,8,9,10,11,12,13,14,20,20,13,12,11,10,21,21,7,6,5
.byte 15,14,20,20,11,10,9,8,21,21,5,21,21,2,20,20,3,4,5,6,21,21,9,10,11,12,13,14,15,20,20,12,11,10,9,21,21,6,5,4
.byte 14,13,20,20,10,9,8,7,21,21,4,21,21,1,20,20,4,5,6,7,21,21,10,11,12,13,14,15,14,20,20,11,10,9,8,21,21,5,4,3
.byte 13,12,20,20,9,8,20,6,21,21,3,21,21,2,20,20,5,6,20,8,21,21,11,12,21,14,20,14,13,20,20,10,21,8,7,21,21,4,3,2
.byte 12,11,20,20,20,20,20,5,21,21,2,21,21,3,20,20,20,20,20,9,21,21,21,21,21,15,20,20,20,20,20,9,21,21,21,21,21,3,2,1
.byte 11,10,9,8,7,6,5,4,21,21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,2
.byte 10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,2,3
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

.pc = $1400 "screentab"
.fill $15, $32
.fill $15, $82
.fill $15, $32
.fill $15, $82
.fill $15, $32
.fill $15, $82
.fill $15, $32
.fill $15, $82
.fill $2a, $92
.fill $2f, $92
.byte 

.pc = $1d00
Page2S:
//.fill $100,00


.pc = $1e00
exit:        
end2:		
relocator:
			
        	ldx #$00
!l:       	lda Page2S,x
        	sta $0200,x
       		inx
       		bne !l-
			sei
			lda #$37
			sta $01
			lda #$00
			ldy #$00
			ldx #$00
			sta $d01a
			stx $d01a
			sty $d01a
s0318:		lda #$00
			sta $0318
s0319:		lda #$00
			sta $0319
	   		jsr $e5a0
			jsr $fda3
       		lda #$00
       		sta $d020
       		sta $d021
       		ldx #$18    
       		lda #$00
cr6:    	sta $d400,x
       		dex
       		bpl cr6
       		lda #$00
       		sta $d011
       		lda #$14
       		sta $d018
       		lda #$c8
       		sta $d016
       		lda #$00
       		sta $0286
       		jsr $e544
       		lda #$1b
       		sta $d011
       		ldx #$00
cr4:   		lda credits,x
       		sta $0590,x
       		lda #$01
       		sta $d990,x
       		inx
       		cpx #$29
       		bne cr4
       		sei
       		nop
       		sei
       		lda #$31
       		sta $0314
       		lda #$ea
       		sta $0315
       		lda #$f0
       		sta $d01a
       		jsr $fda3
       		lda #$00
       		sta $d015
       		lda #$14
       		sta $d018
       		nop
       		nop
       		nop
       		ldx #$01
       		txa
cr1:    
       		sta $d900,x
       		sta $da00,x
       		sta $db00,x
       		inx
       		bne cr1
cr2:    	lda crcode,x
       		sta $0400,x
       		inx
       		cpx #$36
       		bne cr2
       		lda #$00
       		sta $01
       		jmp $0400
			nop
			nop
			nop
			nop
			nop
crcode:       
       		ldx #$00
cr3:    	lda $2a00,x
       		sta $0801,x
       		inx
       		bne cr3
       		inc $0407
       		inc $0404
       		bne  cr3
       		lda #$37
       		sta $01
       		cli
       		nop
       		nop
       		jmp $080d   // Gamestart Jump

//              01234567890123456789012345678901234567890				
credits: .text "code twilight   sid jeff                 "				

.pc = $2800
scrolltext1:						
//	   1234567890123456789012345678901234567890123456
.text "                     "
.text "            more than you deserve            CBA"							
.byte $40
.text "ABC"
.text "              code by twilight               CBA"							
//.byte $40
.text "ABC"
.text "            music by jeff (soren)            CBA"							
//.byte $40
.text "ABC                          "

.text "A hello dear viewers, twilight here. i hope you enjoy this streaming event and"  
.text " many thanks to the orga team for this new experience.  hope you like" 
.text " my little intro. i am very happy that after 27 years i can still code" 
.text " new intros.  have fun and have a nice weekend.              ABC"

.text "              stay healthy to:               CBA"							
//.byte $40
.text "ABC                       B"
.text "atlantis  fairlight  genesis project  hokuto force  laxity  mayday  nostalgia  onslaught  role  triad"	
.text "                    ABC  "
.byte $00
