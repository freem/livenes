VT02_num_regs = #$11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_reg_bits:
      .db "DDLCVVVV";4020
      .db "EPPPNSSS";4021
      .db "TTTTTTTT";4022
      .db "LLLLLTTT";4023

      .db "DDLCVVVV";4024
      .db "EPPPNSSS";4025
      .db "TTTTTTTT";4026
      .db "LLLLLTTT";4027

      .db "CRRRRRRR";4028
      .db "TTTTTTTT";402A
      .db "LLLLLTTT";402B

      .db "--LCVVVV";402C
      .db "L---PPPP";402E
      .db "LLLLL---";402F

      .db "vVLD21AA";4030
      .db "DDDDDDDD";4031
      .db "----NT43";4035
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_regs:
    .dw $4020,$4021,$4022,$4023 ; pulse3
    .dw $4024,$4025,$4026,$4027 ; pulse4
    .dw $4028,$402A,$402B       ; triangle2
    .dw $402C,$402E,$402F       ; noise2
    .dw $4030                   ; PCM/Output Control
    .dw $4031                   ; PCM Data
    .dw $4035                   ; Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_reg_user_defaults:
IFDEF INIT_VT02
     .db $00,$00,$00,$00 ; pulse3
     .db $00,$00,$00,$00 ; pulse4
     .db $00,$00,$00     ; triangle2
     .db $00,$00,$00     ; noise2
     .db $00             ; PCM/Out Control
     .db $00             ; PCM Data
     .db $00             ; Control
ELSE
     .db $30,$08,$00,$00 ; pulse3
     .db $30,$08,$00,$00 ; pulse4
     .db $80,$00,$00     ; triangle2
     .db $30,$00,$00     ; noise2
     .db $08             ; PCM/Out Control
     .db $00             ; PCM Data
     .db $0F             ; Control
ENDIF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;high/low bytes of start offset of bit 7 of each of the registers on screen
VT02_regHtbl:
   .db $2C,$2D,$2D,$2D ; pulse3
   .db $2C,$2D,$2D,$2D ; pulse4
   .db $2D,$2D,$2E     ; tri
   .db $2D,$2D,$2E     ; noise
   .db $2E,$2E         ; PCM
   .db $2E             ; Control
VT02_regLtbl:
   .db $E8,$08,$28,$48 ; pulse3
   .db $F8,$18,$38,$58 ; pulse4
   .db $C8,$E8,$08     ; tri
   .db $D8,$F8,$18     ; noise
   .db $88,$A8         ; PCM
   .db $98             ; Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_ch0:   .db "                                "     ;$2000
            .db "                                "     ;$2020
            .db "  "
            .db $1,$2,$3,$4,$5,$6,$7,$8,$9,$A ; logorow0
            .db "                    "     ;$2040
            .db "  "
            .db $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A; logorow1
            .db "livenes VT02 10/2016"     ;$2060
            .db "                                "     ;$2080
            .db "        Pulse3          Pulse4  "     ;$20A0
            .db "                                "     ;$20C0
            .db "   4020            4024         ",0   ;$20E0
VT02_ch1:   .db "   4021            4025         "     ;$2100
            .db "   4022            4026         "     ;$2120
            .db "   4023            4027         "     ;$2140
            .db "                                "     ;$2160
            .db "        Triangle2       Noise2  "     ;$2180
            .db "                                "     ;$21A0
            .db "   4028            402C         "     ;$21C0
            .db "   402A            402E         ",0   ;$21E0
VT02_ch2:   .db "   402B            402F         "     ;$2200
            .db "                                "     ;$2220
            .db "        PCM             Control "     ;$2240
            .db "                                "     ;$2260
            .db "   4030            4035         "     ;$2280
            .db "   4031                         "     ;$22A0
            .db "                                "     ;$22C0
            .db "                                ",0   ;$22E0
VT02_ch3:   .db "                                "     ;$2300
            .db "                                "     ;$2320
            .db "                                "     ;$2340
            .db "                                "     ;$2360
            .db "                                "     ;$2380
            .db "                                ",0   ;$23A0
;DO NOT THRASH ATTRIBUTES TABLE @ 23C0!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_row_to_reg_col0: ; col=0 row=X
    .db $00,$01,$02,$03     ; pulse1
    .db $08,$09,$0A         ; triangle
    .db $0E,$0F             ; DMC
    .db 0,0,0,0,0,0,0       ; (dummys)
VT02_row_to_reg_col1: ; col=1 row=X
    .db $04,$05,$06,$07     ; pulse2
    .db $0B,$0C,$0D         ; noise
    .db $12                 ; Control
    .db 0,0,0,0,0,0,0,0     ; (dummys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_maxy_col0 = #$08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_rowtbl0:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
    .db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VT02_maxy_col1 = #$07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_rowtbl1:    ; sprite y positions
    .db $38,$40,$48,$50,$70,$78,$80,$A0
    .db $A8,$B0,$B8,$C0,$C8,$D0,$D8,$E0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print0:
    ;name table 0 is $2000-$23FF
    lda #$2C
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    ldx #$FF
    ldy #0
ALTERNATE_ch_print0_loop:
    lda VT02_ch0,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print0_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print1:
    lda #$2D
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    ldx #$FF
    ldy #0
ALTERNATE_ch_print1_loop:
    lda VT02_ch1,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print1_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print2:
    lda #$2E
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    ldx #$FF
    ldy #0
ALTERNATE_ch_print2_loop:
    lda VT02_ch2,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print2_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_ch_print3:
    lda #$2F
    sta $2006  ;PPUADDR
    lda #$00
    sta $2006  ;PPUADDR
    ldx #$FF
    ldy #0
ALTERNATE_ch_print3_loop:
    lda VT02_ch3,y
    sta $2007 ;PPUDATA
    iny
    dex
    bne ALTERNATE_ch_print3_loop
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_set_cur:
    lda #0
    sta cursorY

    lda #1   ; yes ALTERNATE!
    sta cur_set

    lda #VT02_num_regs
    sta cur_num_regs

    lda #VT02_maxy_col0
    sta cur_maxy_col0

    lda #VT02_maxy_col1
    sta cur_maxy_col1

    ; creating pointer adresses for current register set
    lda #VT02_regs&255
    sta ptr_src
    lda #VT02_regs/256
    sta ptr_src+1    
    lda #cur_reg_table&255
    sta ptr_dst
    lda #0; zero page!
    sta ptr_dst+1       
    lda cur_num_regs
    asl ;a=a*2
    tay
    jsr memcpy    
    
    ;bunch of table pointers    
    lda #ALTERNATE_shadow
    sta cur_shadow_ptr
    lda #0
    sta cur_shadow_ptr+1    
    
    lda #VT02_reg_bits&255
    sta cur_reg_bits_ptr
    lda #VT02_reg_bits/256
    sta cur_reg_bits_ptr+1    
    
    lda #VT02_regHtbl&255
    sta cur_regHtbl
    lda #VT02_regHtbl/256
    sta cur_regHtbl+1    

    lda #VT02_regLtbl&255
    sta cur_regLtbl
    lda #VT02_regLtbl/256
    sta cur_regLtbl+1    

    lda #VT02_row_to_reg_col0&255
    sta cur_row_to_reg_col0
    lda #VT02_row_to_reg_col0/256
    sta cur_row_to_reg_col0+1    
    
    lda #VT02_row_to_reg_col1&255
    sta cur_row_to_reg_col1
    lda #VT02_row_to_reg_col1/256
    sta cur_row_to_reg_col1+1    

    rts        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_fill_shadow_with_defaults:
    ;ONLY DONE ONCE
    lda #VT02_reg_user_defaults&255
    sta ptr_src
    lda #VT02_reg_user_defaults/256
    sta ptr_src+1
    ;destination
    lda #ALTERNATE_shadow&255
    sta ptr_dst
    lda #ALTERNATE_shadow/256
    sta ptr_dst+1    
    ldy #VT02_num_regs 
    jsr memcpy    
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_extra_init:
    ; other VTxx hardware registers
    lda #$00
    sta $2010 ; VT03_VIDEOCONTROL0
    sta $2011 ; VT03_VIDEOCONTROL1

    ; enable VTxx second APU
    lda #$08
    sta $4030 ; VT03_DAMODE
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ALTERNATE_bank_init:
    ; second to last prg bank at $C000
    lda #$FE
    sta $4109 ; VT03_PRGBANK2

    ; last prg bank at $E000
    lda #$FF
    sta $410A ; VT03_PRGBANK3

    ; set up CHR-ROM
    lda #$00
    sta $2016 ; VT03_VROMBANK4 ($0000-$07FF)
    lda #$02
    sta $2017 ; VT03_VROMBANK5 ($0800-$0FFF)

    ldy #4
    sty $2012 ; VT03_VROMBANK0 ($1000-$13FF)
    iny
    sty $2013 ; VT03_VROMBANK1 ($1400-$17FF)
    iny
    sty $2014 ; VT03_VROMBANK2 ($1800-$1BFF)
    iny
    sty $2015 ; VT03_VROMBANK3 ($1C00-$1FFF)
    rts
