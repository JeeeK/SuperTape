;********************************
;*                              *
;*   S U P E R T A P E   C 6 4  *
;*                              *
;*   AUS C'T 1984, HEFT 10      *
;*                              *
;*   ERSTELLT 27/5/1990, JK     *
;*                              *
;********************************
;
         .MA RMB(N)        
HERE     .BA HERE+N        
         .RT               
;
.EQ CPORT   = $01    ; MP-PORT
.EQ PKTFL   = $02    ; FLAG
.EQ LOSPDL  = $46    ; TIMER
.EQ LOSPDH  = $81    ;  3600 BD
.EQ LOCMP   = $73
.EQ HISPDL  = $9C    ; TIMER
.EQ HISPDH  = $80    ;  7200 BD
.EQ HICMP   = $BB
.EQ TLIM    = $57
.EQ NLEN    = $B7  ; FILENAMELEN
.EQ PUFLEN  = $C6  ; TAST.PUFFER
.EQ LSL0    = $78  ; TIMER SCHREIBEN
.EQ LSL1    = $FF
.EQ HSL0    = $34
.EQ HSL1    = $78
         .BA TLIM+1        ; ZEICHEN
CHAR     ... RMB(1)        
SFLAG    ... RMB(1)        
BUFF     ... RMB(1)        
WTFLAG   ... RMB(1)        
OPFLAG   ... RMB(1)        
CHKSML   ... RMB(1)        
CHKSMH   ... RMB(1)        
POINTL   ... RMB(1)        
POINTH   ... RMB(1)        
POENL    ... RMB(1)        ;ENDPOINTER
POENH    ... RMB(1)        
;
         .BA $93           ;TEMPORAERER AKKU
TEMPA    ... RMB(1)        ;TEMP.AKKU
TEMPB    ... RMB(1)        
TEMPY    ... RMB(1)        ;Y TEMPORAER
;
.EQ STATUS  = $90
.EQ DN      = $BA ;DEV.NUM.
.EQ INBUF   = $BB ;ZEIGER FILENAME
.EQ SECA    = $B9 ;SEK.ADR.
.EQ BASAL   = $2B ;BASICSTART
.EQ BASAH   = BASAL+1
.EQ ANFL    = $C1 ;ANF.ADR.
.EQ ANFH    = ANFL+1
.EQ ENDL    = $AE ;END-ADR.
.EQ ENDH    = ENDL+1
.EQ MFLAG   = $C0 ;MOTOR EIN
.EQ STW     = $D9 ;TIMER START
.EQ TIM0    = $FB ;TIMER WERT '0'
.EQ TIM1    = TIM0+1 ; -"-    '1'
.EQ TPUF    = $277 ;TAST.PUFFER
;
.EQ IRQT    = $29F ;TEMP F IRQ
.EQ IRQV    = $314 ;IRQ VEKTOR
.EQ NMIV    = $318 ;NMI VEKTOR
.EQ PUFFER  = $33C ;EINGABEPUFFER
.EQ CNTAL   = $DC04 ;TIMER LOW
.EQ CNTAH   = CNTAL+1;     HIGH
.EQ CNTBL   = $DD04 ;TIMER LOW
.EQ CNTBH   = CNTBL+1;     HIGH
.EQ ICR     = $DC0D ;INT.REG.
.EQ ICRB    = $DD0D ;INT.REG.
.EQ CRA     = ICR+1 ;TIMER START
.EQ TST     = ICRB+1 ;TIMER START
.EQ VICRG   = $D011
;
.EQ WARM    = $A002 ;WARMSTART
.EQ BRES    = $FC93 ;MOT AUS
.EQ LDREG   = $FDA3
.EQ ERRM    = $A369 ;MELDUNG ERROR
.EQ ERRMEL  = $A43A ;SYNTAX ERR.
.EQ PRESSP  = $F817 ;PRESS PLAY
.EQ PRESSR  = $F838 ;PRESS RECORD
.EQ FOUND   = $F12F ;FOUND
.EQ LVM     = $F5D2 ;LOADING/VER.
.EQ BETRL   = $F4AB ;LOAD NORM.
.EQ BETRSA  = $F5ED ;SAVE NORM.
.EQ BSOUT   = $FFD2 ;CHAR.OUT
;
;
;
.BA $CA7D
;*****************************
;* DELAY-HOCHLAUFZEIT
;*****************************
;
DELAY    LDX #0            
         LDY #0            
DEL1     DEX               
         BNE DEL1          
         DEY               
         BNE DEL1          
         RTS               
;*****************************
;* NAMENSFEHLERMELDUNG
;*****************************
;
SERROR   LDY #0            
SER1     LDA SER2,Y        ;MELDUNG
         INY               
         JSR BSOUT         ;AUSGEBEN
         CMP #0            
         BNE SER1          
         PLA               
         PLA               
         RTS               
SER2     .TX "? FILENAME INVALID"
         .BY $0D,0         
;*****************************
;* IRQ-LESEROUTINE
;*
;* WERTET FALLENDE FLANKEN
;* AUS, (ZEITL.) ABSTAENDE
;* LASSEN AUF 0 UND 1 SIGNAL
;* SCHLIESSEN.
;*****************************
;
RDDATA   LDA ICR           ;IRQ RESET
         LDA #STW          ;TIMER STARTWORT
         LDX CNTAL         
         LDY CNTAH         
         STA CRA           ;TIMER STARTEN
         BMI RDBIT0        ;T=T2?
         SEC               
         ROR CHAR          
         BIT SFLAG         ;SYNCHR.?
         BMI RD1           
         BCC RD1           ;ALLE BITS?
         LDA CHAR          
         STA BUFF          
         LDA #$80          
         STA CHAR          ;ENDEMARKIERUNG
         STA WTFLAG        ;BYTE DA
RD1      CPY #$79          
         BCC RD2           ;T=T3
         CPX TLIM          
         BCS RD3           
RD2      LDA #0            ;T1
         STA OPFLAG        ;OUT-OF-PHASE FLAG
         SEC               
         BCS RDBIT         
RD3      LDA OPFLAG        ;PHASENVERSCHIEBUNG MERKEN
         EOR #$80          
         STA OPFLAG        
         BIT OPFLAG        ; ***
         BPL RD4           ;EXIT
RDBIT0   CLC               
RDBIT    ROR CHAR          
         BIT SFLAG         
         BMI RD4           ;SYCHR.
         BCC RD4           ;ALLES GELESEN?
         LDA CHAR          
         STA BUFF          
         LDA #$80          
         STA CHAR          ;ENDEMARKIERUNG
         STA WTFLAG        ;BYTE DA
RD4      PLA               
         TAY               
         PLA               
         TAX               
         PLA               
         RTI               
         RTS               ; ***
;
;*****************************
;* BYTE LESEN
;*****************************
;
RDBYTE   BIT WTFLAG        
         BPL RDBYTE        ;WARTEN
         STY WTFLAG        ;FLAG LOESCHEN
         LDA BUFF          
         RTS               
;
;*****************************
;* AUF BYTETAKT SYNCHRONISIEREN
;*****************************
;
INSYN    LDY #$80          ;BITWEISE
         STY SFLAG         ;EINLESEN
INS1     LDA CHAR          
         CMP #$16          ;SYNCBYTE
         BNE INS1          
         STY CHAR          
         LDY #10           ;NOCH 10 SYNCS
         STY SFLAG         ;BYTEWEISE
         STY WTFLAG        ;FLAG LOESCHEN
INS2     BIT WTFLAG        ;WARTEN
         BPL INS2          
         STY WTFLAG        
         LDA BUFF          
         CMP #$16          ;NOCH IMMER SYNC
         BNE INSYN         ;VON VORN
         DEY               
         BNE INS2          
INS3     JSR RDBYTE        
         CMP #$16          ;ALLE SYNCS
         BEQ INS3          ;LESEN
         RTS               
;
;*****************************
;* SUPERTAPE VERLASSEN
;*****************************
;
ENDSR    JSR BRES          ;SYS.RESET
         LDA #$47          
         STA NMIV          ;NMI-VEKTOR
         LDA #$FE          ;WIEDER-
         STA NMIV+1        ;HERSTELLEN
         CLC               
         CLI               
         RTS               
;
;*****************************
;* SUPERTAPE ABBRUCH
;*****************************
;
BRKSR    JSR ENDSR         
         JSR LDREG         
         JMP (WARM)        ;INS BASIC
;
;*****************************
;* NAMENSPRUEFUNG LOAD
;*****************************
;
LNAME    LDA NLEN          
         CMP #17           
         BCS LN4           ;ZU LANG
         LDY #0            
         STY PKTFL         ;PUNKTFLAG
         LDX #0            ;MISST FELDGROESSE
LN1      CPY NLEN          
         BCC LN2           
         RTS               ;OK
LN2      LDA (INBUF),Y     
         CMP #"."          
         BEQ LN5           
         INX               
LN3      INY               ;NAECHSTES
         BIT PKTFL         
         BPL LN1           ;KEIN PUNKT
         CPX #4            ;EXT.<4
         BCC LN1           
LN4      JMP SERROR        ;SYNTAX FEHLER
LN5      BIT PKTFL         
         BMI LN4           ;2. PUNKT
         DEC PKTFL         
         CPX #13           ;PUNKTPOS.
         BCS LN4           ;PUNKT ZU SPAET
         LDX #0            
         JMP LN3           ;EXT.FELD
;
;*****************************
;* DATENBLOCK LADEN
;*****************************
;
BFOUND   LDA SECA          ;SEK-ADR
         BNE LB1           
         LDA BASAL         ;BASIC
         STA POINTL        ;START
         LDA BASAH         
         STA POINTH        
         BNE LB2           
LB1      LDA PUFFER+17     
         STA POINTL        ;ABSOLUT
         LDA PUFFER+18     ;LOAD
         STA POINTH        
LB2      CLC               
         LDA POINTL        
         ADC PUFFER+19     
         STA ENDL          
         LDA POINTH        
         ADC PUFFER+20     
         STA ENDH          ;ENDADR
         BIT PUFFER+16     
         BPL LB3           ;3600 BAUD
         LDA #HISPDL       
         STA CNTAL         ;TIMER
         LDA #HISPDH       ; 7200
         STA CNTAH         
         LDA #HICMP        ;ZEITLIMIT F T3
         STA TLIM          
LB3      JSR INSYN         
         CMP #$C5          ;START
         BNE LBERR         
         STY CHKSML        ;PRUEFSUMME
         STY CHKSMH        ;LOESCHEN
LB4      BIT WTFLAG        
         BPL LB4           ;WARTEN
         STY WTFLAG        
         LDA BUFF          
         BIT TEMPB         ;VERIFY?
         BMI LBVERI        
         STA (POINTL),Y    
LB5      SEC               ; PRUEFSUMME
         ROR               ;BERECHNEN
LB6      BCC LB7           
         INC CHKSML        
         BNE LB7           
         INC CHKSMH        
LB7      LSR               
         BNE LB6           
         INC POINTL        
         BNE LB8           
         INC POINTH        
LB8      LDA POINTL        
         CMP ENDL          ;ENDE?
         BNE LB4           ;WEITER LESEN
         LDA POINTH        
         CMP ENDH          
         BNE LB4           
LB9      BIT WTFLAG        ;PRUEFSUMME
         BPL LB9           ;LESEN
         STY WTFLAG        
         LDA BUFF          
         CMP CHKSML        
LBERR    BNE LBERR1        
LB10     BIT WTFLAG        ;UND
         BPL LB10          ;TESTEN
         STY WTFLAG        
         LDA BUFF          
         CMP CHKSMH        
         BNE LBERR1        
         JSR LVM           ;MELDUNG
         LDA #" "          
         JSR BSOUT         
         JSR OUTNAM        
         JSR LBEND         
         LDY #3            ;BASIC
LB11     LDA PUFFER+12,Y   ;AUTOSTART
         CMP LBAUTO,Y      
         BNE LB12          
         DEY               
         BNE LB11          
LB13     LDA LBRUN,Y       
         STA TPUF,Y        
         INY               ;TASTATURPUFFER
         CPY #4            
         BNE LB13          
         STY PUFLEN        
LB12     LDY #3            ;M/C
LB14     LDA PUFFER+12,Y   ;AUTOSTART
         CMP LBCOM,Y       
         BNE LBEXIT        
         DEY               
         BNE LB14          
         JMP (PUFFER+17)   
LBVERI   CMP (POINTL),Y    
         BEQ LB5           
LBERR1   LDA ERRM,Y        ;FEHLERMELDUNG
         BEQ LBEND         
         JSR BSOUT         
         INY               
         BNE LBERR1        
LBEND    JSR ENDSR         ;MOTOR AUS
LBEXIT   LDX ENDL          
         LDY ENDH          
         LDA #0            
         CLC               
         RTS               
;
LBAUTO   .TX " AUT"        
LBRUN    .TX "RUN"         
         .BY $0D           
LBCOM    .TX " COM"        
;*****************************
;* LOAD ROUTINE
;*****************************
;
LOAD     SEI               ;INT.SPERREN
         STA TEMPA         
         LSR               
         ROR               ;VERIFY
         STA TEMPB         ;FLAG
         LDA #0            
         STA STATUS        
         LDA DN            ;DEVICE
         CMP #7            ;SUPERTAPE
         BEQ LO1           
         SEC               
         AND #$0E          
         BNE LO2           ;IEC OD. TAPE
         CLC               
LO2      JMP BETRL         
LO1      JSR LNAME         ;SYNTAXCHECK
         JSR PRESSP        ;PLAYTASTE?
         LDA CPORT         ;MOTOR
         AND #$1F          ;EIN
         STA CPORT         
         STA MFLAG         ;MOTOR ON-FLAG
         JSR DELAY         
         LDA #<(BRKSR)     ;BREAK-
         STA NMIV          ;ROUTINE
         LDA #>(BRKSR)     ;AUFSETZEN
         STA NMIV+1        
         LDA IRQV          ;IRQ-VEKT.
         STA IRQT          
         LDA IRQV+1        ;RETTEN
         STA IRQT+1        
         LDA #<(RDDATA)    ;LESEROUTINE
         STA IRQV          
         LDA #>(RDDATA)    ;AUFSETZEN
         STA IRQV+1        
         LDA #$7F          ;INT.MASK
         STA ICR           ;LOESCHEN
         LDA #$90          ;UND NEU
         STA ICR           ;SETZEN
         LDA VICRG         ;DISPLAY
         AND #$EF          
         STA VICRG         ;DISABLE
         LDA #LOSPDL       
         STA CNTAL         ;TIMER
         LDA #LOSPDH       ; 3600
         STA CNTAH         
         LDA #LOCMP        ;ZEITLIMIT F. T3
         STA TLIM          
         CLI               ;LOS GEHTS
LOSYNC   JSR INSYN         ;SYNCHRONISIEREN
         CMP #$2A          ;STARTZEICHEN?
         BNE LOSYNC        
         STY CHKSML        ;PRUEFSUMME
         STY CHKSMH        ;LOESCHEN
LOPARA   BIT WTFLAG        
         BPL LOPARA        ;WARTEN
         STY WTFLAG        
         LDA BUFF          
         STA PUFFER,Y      
         SEC               
         ROR               ;PRUEFSUMME
LO3      BCC LO4           ;BERECHNEN
         INC CHKSML        
         BNE LO4           
         INC CHKSMH        
LO4      LSR               
         BNE LO3           
         INY               
         CPY #$19          ;BLOCK ENDE
         BCC LOPARA        
         JSR RDBYTE        
         CMP CHKSML        
         BNE LOERR1        
         JSR RDBYTE        
         CMP CHKSMH        ;PRUEFSUMME
         BNE LOERR1        ;TESTEN
         LDY #0            
         CPY NLEN          ;KEIN NAME
         BEQ LOFOUND       ;OK
         LDY #0            ;***
         CPY NLEN          ;***
         BEQ LOFOUND       ;***
         STY TEMPY         
         STY PKTFL         
         LDX #0            
LO5      LDA (INBUF),Y     
         CMP #"."          ;EXTENSION?
         BEQ LO8           
         CMP #"*"          
         BEQ LO9           
         LDY TEMPY         
         CMP #"?"          
         BEQ LO6           ;NAECHSTES ZEICHEN
         CMP PUFFER,Y      
         BNE LOERR2        ;NAECHSTES FILE
LO6      INY               
LO7      INX               
         CPX NLEN          
         BCS LOFOUND       ;OK
         STY TEMPY         
         TXA               
         TAY               
         BCC LO5           ;WEITER
LO8      DEC PKTFL         ;PUNKT WAR DA
         LDY #13           ;PUNKTPOS.
         JMP LO7           
LO9      BIT PKTFL         ;PUNKT DOPPELT?
         BMI LOFOUND       ;OK
         LDY #12           
         JMP LO7           
LOERR2   JSR LBEND         
         LDY #$63          ;OFFSET
         JSR FOUND         ;MELDUNG
         LDA #$20          
         JSR OUTNAM        ;NAMEN AUSGEBEN
         LDA #3            
         STA TEMPA         
LO10     JSR DELAY         ;CA.2 SEK
         DEC TEMPA         
         BNE LO10          
         SEI               
         JMP LO1           
OUTNAM   LDY #0            
OUT1     LDA PUFFER,Y      
         JSR BSOUT         
         INY               
         CPY #16           
         BCC OUT1          
         RTS               
LOERR1   JMP LBERR1        
LOFOUND  JMP BFOUND        ;DATENBLOCK LADEN
;*****************************
;* WRITE BYTE
;*****************************
;
WRTA     STA CHAR          ;LOW NIBBLE
WRTB     LDY #4            ;HIGH NIBBLE
WBY1     LSR CHAR          
         BCC WBY2          ; '0' OD. '1'
         LDA TIM1          ;ZEIT F. '1'
         STA CNTBL         
WBY2     LDA #$01          
WBY3     BIT ICRB          ;TIME OUT
         BEQ WBY3          
         LDA #STW          
         STA TST           ;TIMER STARTEN
         LDA CPORT         
         EOR #$08          ;INV.
         STA CPORT         
         BCS WBY5          ;EV. PRUEFSUMME
         LDA #$01          
WBY4     BIT ICRB          
         BEQ WBY4          ;TIME OUT?
         LDA #STW          
         STA TST           ;TIMER STARTEN
         LDA CPORT         
         EOR #$08          ;INV.
         STA CPORT         
         DEY               
         BNE WBY1          ;NAECHSTES BIT
         RTS               
WBY5     LDA CHKSML        ;PRUEFSUMME
         ADC #00           
         STA CHKSML        ;BERECHNEN
         LDA CHKSMH        
         ADC #0            
         STA CHKSMH        
         LDA TIM0          ;ZEIT F. '0'
         STA CNTBL         
         DEY               
         BNE WBY1          
         RTS               
;*****************************
;* WRITE BLOCK
;*****************************
;
WRITE    PHA               
         LDX #64           ;ANZAHL D. SYNCS
WBL1     LDA #$16          ;SYNC
         JSR WRTA          
         JSR WRTB          
         DEX               
         BNE WBL1          
         PLA               
         JSR WRTA          ;BLOCKKENNUNG
         JSR WRTB          ;SCHREIBEN
         LDY #0            ;PRUEFSUMME
         STY CHKSML        
         STY CHKSMH        ;LOESCHEN
WBL2     LDA (POINTL),Y    
         JSR WRTA          
         INC POINTL        ;ZEIGER ERHOEHEN
         BNE WBL3          
         INC POINTH        
WBL3     JSR WRTB          
         LDA POINTL        ;BLOCKENDE?
         CMP POENL         
         BNE WBL2          
         LDA POINTH        
         CMP POENH         
         BNE WBL2          
         LDX CHKSMH        ;PRUEFSUMME
         LDA CHKSML        ;SPEICHERN
         JSR WRTA          
         JSR WRTB          
         TXA               
         JSR WRTA          
         JSR WRTB          
         JMP WRTA          ;SCHLUSSBIT
;*****************************
;* NAMENSPRUEFUNG SAVE
;*****************************
;
SNAME    LDY #0            
         LDA #" "          
SN1      STA PUFFER,Y      
         INY               
         CPY #16           
         BCC SN1           
         LDY #0            
         STY PKTFL         
         STY TEMPY         
         CPY NLEN          
         BEQ SN4           ;EXIT
         TYA               
         TAX               
SN2      LDA (INBUF),Y     
         CMP #"."          
         BNE SN5           
         CPY #13           ;PUNKTPOS.
         BCS SN6           ;FEHLER
         BIT PKTFL         
         BMI SN6           ;PUNKT DOPPELT
         DEC PKTFL         ;PUNKT MERKEN
         LDY #12           
SN3      STA PUFFER,Y      
         INY               
         STY TEMPY         
         CPY #16           
         BCS SN4           ;EXIT
         INX               
         CPX NLEN          ;FERTIG?
         BCS SN4           ;EXIT
         TXA               
         TAY               
         JMP SN2           
SN4      RTS               
SN5      LDY TEMPY         
         JMP SN3           
SN6      JMP SERROR        
;*****************************
;* SUPERTAPE SAVE
;*****************************
;
SAVE     LDA DN            ;DEVICE#
         CMP #7            ;SUPERTAPE?
         BEQ SV2           
         SEC               
         AND #$0E          
         BNE SV1           
         CLC               
SV1      JMP BETRSA        
SV2      JSR SNAME         ;NAMEN PRUEFEN
         SEI               
         JSR PRESSR        ;BANDTASTE WARTEN
         LDA CPORT         
         AND #$1F          ;MOT.EIN
         STA CPORT         
         STA MFLAG         ;MOTOR ON FLAG
         JSR DELAY         
         LDA #<(BRKSR)     ;BREAK ROUTINE
         STA NMIV          
         LDA #>(BRKSR)     ;AUFSETZEN
         STA NMIV+1        
         LDA VICRG         ;SCREEN
         AND #$EF          ;DISABLE
         STA VICRG         
         LDA SECA          ;SEK.ADR.
         STA PUFFER+16     ;BAUDRATE
         LDA ANFL          ;STARTADR.
         STA PUFFER+17     
         LDA ANFH          
         STA PUFFER+18     
         SEC               ;LAENGE
         LDA ENDL          
         SBC ANFL          
         STA PUFFER+19     
         LDA ENDH          
         SBC ANFH          
         STA PUFFER+20     
         LDA #0            
         STA PUFFER+21     
         STA PUFFER+22     
         STA PUFFER+23     
         STA PUFFER+24     
         STA CNTBH         ;TIMERWERT
         LDA #<(PUFFER)    
         STA POINTL        
         LDA #>(PUFFER)    
         STA POINTH        
         STA POENH         
         LDA #$55          ;PUFFERENDE
         STA POENL         
         LDA #LSL1         
         STA TIM1          
         LDA #LSL0         ;TIMERWERTE
         STA TIM0          ;3600 BAUD
         STA CNTBL         
         LDA #STW          ;TIMER
         STA TST           ;STARTEN
         LDA #$2A          ;PARA.BLOCK
         JSR WRITE         ;SCHREIBEN
         JSR DELAY         
         BIT SECA          ;BAUDRATE
         BPL SV3           
         LDA #HSL1         ;7200
         STA TIM1          
         LDA #HSL0         
         STA TIM0          
         STA CNTBL         ;TIMER SETZEN
SV3      LDA ANFL          ;BASIC START
         STA POINTL        
         LDA ANFH          
         STA POINTH        
         LDA ENDL          ;BASIC ENDE
         STA POENL         
         LDA ENDH          
         STA POENH         
         LDA #$C5          ;DATENBLOCK
         JSR WRITE         ;SCHREIBEN
         JMP ENDSR         
;*****************************
;* SUPERTAPE EINSPRUNG
;*****************************
;
BEGIN    LDA #<(LOAD)      ;LOAD
         STA $330          ;VEKTOR
         LDA #>(LOAD)      
         STA $331          
         LDA #<(SAVE)      ;SAVE
         STA $332          ;VEKTOR
         LDA #>(SAVE)      
         STA $333          
         LDY #0            
BE1      LDA BESTM,Y       
         INY               
         JSR BSOUT         
         CMP #0            
         BNE BE1           
         RTS               
BESTM    .BY $93,$0D       
         .TX "      *** SUPERTAPE"
         .TX "  AKTIVIERT ***"
         .BY $0D,$0D,0     
