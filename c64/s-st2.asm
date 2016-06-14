;********************************
;*                              *
;*   S U P E R T A P E   C 6 4  *
;*                              *
;*   AUS C'T 1984, HEFT 10      *
;*                              *
;*   ERSTELLT 27/5/1990, JK     *
;*                              *
;*   VERBESSERT 6/6/1990 JK     *
;*              10/6/90         *
;*   UEBERARBEITET 28/3/2012 JK *
;*                              *
;********************************
;
         .MA RMB(N)        
HERE     .BA HERE+N        
         .RT               
;
.OB "C-ST2-22,P,W"
.EQ CPORT   = $01    ; MP-PORT
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
TEMPA    ... RMB(1)        ;LOAD/VERIFY FLAG, BIT7
;
.EQ STATUS  = $90
.EQ DN      = $BA ;DEV.NUM.
.EQ INBUF   = $BB ;ZEIGER FILENAME
.EQ SECA    = $B9 ;SEK.ADR.
.EQ BASAL   = $2B ;BASICSTART
.EQ BASAH   = BASAL+1
.EQ ANFL    = $C1 ;ANF.ADR.
.EQ ANFH    = ANFL+1
.EQ LANF    = $C3
.EQ ENDL    = $AE ;END-ADR.
.EQ ENDH    = ENDL+1
.EQ MFLAG   = $C0  ;MOTORFLAG
.EQ STW     = $D9 ;TIMER START
.EQ CBUF    = $F9 ;POINTER AUF KASSETTENPUFFER
.EQ TIM0    = $FB ;TIMER WERT '0'
.EQ TIM1    = TIM0+1 ; -"-    '1'
.EQ TPUF    = $277 ;TAST.PUFFER
;
.EQ IRQV    = $314 ;IRQ VEKTOR
.EQ NMIV    = $318 ;NMI VEKTOR
.EQ PUFFER  = $33C ;KASSETTENPUFFER
.EQ CNTAL   = $DC04 ;TIMER LOW (LOAD)
.EQ CNTAH   = CNTAL+1;     HIGH
.EQ CNTBL   = $DD04 ;TIMER LOW (SAVE)
.EQ CNTBH   = CNTBL+1;     HIGH
.EQ ICR     = $DC0D ;INT.REG.
.EQ ICRB    = $DD0D ;INT.REG.
.EQ CRA     = ICR+1 ;TIMER START
.EQ TST     = ICRB+1 ;TIMER START
.EQ VICRG   = $D011
;
.EQ BREAK   = $A381 ;BREAK-TEXT
.EQ CIAINIT = $FDA3
.EQ ERRM    = $A369 ;MELDUNG ERROR
.EQ BSMSG   = $F12F ;MESSAGE OUT
.EQ LVM     = $F5D2 ;LOADING/VER.
.EQ BSOUT   = $FFD2 ;CHAR.OUT
.EQ STROUT  = $AB1E ;STR.OUT
;
;
.BA $CA25
NMIT     .WO 0             
IRQT     .WO 0             
STKT     .BY 0             
BSSAV    .WO 0             
BSLOD    .WO 0             
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
;* FEHLERMELDUNG AUSGEBEN
;*****************************
;
MSG      BIT $9D           ;DIREKTMODUS FLAG
         BPL MSGEXIT       
MSG1     LDA MSGTXT,Y      
         PHP               
         AND #$7F          ;BIT 7 WEG
         JSR BSOUT         ;AUSGEBEN
         INY               
         PLP               
         BPL MSG1          ;MESSAGEENDE?
MSGEXIT  CLC               
         RTS               
;
MSGTXT   .BY $0D,$0D       
         .TX " **** SUPERTAPE V2.0  AKTIVIERT ****"
         .BY $0D,$8D       
MSGPPT   .BY $0D           
         .TX "PRESS PLAY ON TAP"
         .BY $C5           ; 'E'+$80
MSGPPRT  .BY $0D           
         .TX "PRESS RECORD & PLAY ON TAP"
         .BY $C5           ; 'E'+$80
MSGIFN   .BY $0D           
         .TX "? INVALID FILENAME"
         .BY $8D           
MSGBPB   .BY $0D           
         .TX "? BAD PARA-BLOCK"
         .BY $8D           
MSGBCS   .BY $0D           
         .TX "? BAD CHECKSUM"
         .BY $8D           
MSGSE    .BY $0D           
         .TX "? BAD SYNC"  
         .BY $8D           
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
;
;*****************************
;* BYTE LESEN
;*****************************
;
; Y-REG BIT7 MUSS IMMER 0 SEIN!
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
         STY WTFLAG        ;FLAG LOESCHEN, BIT7
INS2     JSR RDBYTE        ;BYTE EINLESEN
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
ENDSR    PHA               
         PHP               
         SEI               
         LDA VICRG         
         ORA #$10          ;SCREEN ENABLE
         STA VICRG         
         LDA CPORT         
         ORA #$20          ;MOTOR AUS
         STA CPORT         
         LDA #$7F          ;INT.CONTR.
         STA ICR           ;REG.
         STA ICRB          ;LOESCHEN
         LDA #$08          ;ONE SHOT
         STA CRA           
         STA TST           
         LDA IRQT          
         STA IRQV          
         LDA IRQT+1        ;VEKTOREN
         STA IRQV+1        ;WIEDER
         LDA NMIT          
         STA NMIV          ;HERSTELLEN
         LDA NMIT+1        
         STA NMIV+1        
         PLP               
         PLA               
         RTS               
;
;*****************************
;* SUPERTAPE ABBRUCH
;*****************************
;
BRKSR    JSR ENDSR         
         LDX STKT          ;STACK AM BEGINN
         TXS               
         CLI               
         LDA #0            ;FEHLERCODE BREAK
         SEC               ;FEHLER
         RTS               
;
;*****************************
;* NAMENSPRUEFUNG LOAD
;*****************************
;
LNAME    LDA NLEN          
         CMP #17           
         BCS LN7           ;ZU LANG
         LDY #0            
         LDX #0            ;FIKT. ZIELPUFFER
         BEQ LNCHK         ;IMMER..
LN1      LDA (INBUF),Y     
         CMP #"."          
         BNE LN5           
         CPY #13           
         BCS LN7           ;NAMESTEIL ZU LANG - FEHLER!
         CPX #13           
         BCS LN7           ;2. PUNKT - FEHLER!
         LDX #12           ;PUNKTPOS., ZUM TYP
LN5      INX               
         CPX #16           ;FIKT.PUFFER ENDE
         BEQ LN6           ;ENDE, OK
         INY               
LNCHK    CPY NLEN          ;INPUT ENDE?
         BCC LN1           ;NAECHSTES
LN6      CLC               ;OK
LN7      RTS               
;
;*****************************
;* DATENBLOCK LADEN
;*****************************
;
STL      SEI               
         TSX               ;STACKPOINTER
         STX STKT          ;ABSPEICHERN
         JSR SETUP         ;LESEROUTINE AUFSETZEN
         LDY #17           ;STARTADR.
         LDA (CBUF),Y      ;VON BAND:
         INY               
         STA POINTL        ;ABSOLUT
         LDA (CBUF),Y      ;LOAD
         INY               
         STA POINTH        
         LDA SECA          ;SEK.ADR.?
         BNE STL1          
         LDA LANF          
         STA POINTL        ;AN GEGEBENE
         LDA LANF+1        ;ADR.
         STA POINTH        
STL1     CLC               
         LDA POINTL        ;LAENGE
         ADC (CBUF),Y      ;+START
         STA ENDL          
         INY               
         LDA POINTH        
         ADC (CBUF),Y      
         STA ENDH          ;=ENDADR
         LDY #16           
         LDA (CBUF),Y      ;BAUDRATE
         JSR SETTIMER      ;TIMERINTERRUPT
         CLI               
         JSR INSYN         
         CMP #$C5          ;START
         BNE STLERR3       
         STY CHKSML        ;PRUEFSUMME
         STY CHKSMH        ;LOESCHEN
;
STLREAD  JSR RDBYTE        
         BIT TEMPA         ;VERIFY?
         BPL STLNOVER      
         CMP (POINTL),Y    
         BNE STLERR4       ;ERROR
         .BY $2C           
STLNOVER STA (POINTL),Y    
         LDX CHKSML        ;CHECKSUM
;
STLPLOOP LSR               ;BERECHNEN
         BCC STLPNULL      ;1EN ZAEHLEN
         INX               
         BNE STLPLOOP      
         INC CHKSMH        
         BEQ STLPLOOP      ;UEBERLAUF - WEITER!
STLPNULL BNE STLPLOOP      
         STX CHKSML        
;
         INC POINTL        ;POINTER
         BNE STL6          ;ERHOEHEN
         INC POINTH        
;
STL6     LDA POINTL        
         CMP ENDL          ;ENDE?
         BNE STLREAD       ;WEITER LESEN
         LDA POINTH        
         CMP ENDH          
         BNE STLREAD       
;
         JSR RDBYTE        ;PRUEFSUMME
         CMP CHKSML        ;LESEN
         BNE STLERR2       
         JSR RDBYTE        ;UND TESTEN
         CMP CHKSMH        
         BEQ STLEXIT       
STLERR2  LDA #2            ;BAD CHECKSUM
         .BY $2C           
STLERR3  LDA #3            ;SYNC ERROR
         .BY $2C           
STLERR4  LDA #4            ;VERIFY ERROR
         SEC               
         .BY $24           
STLEXIT  CLC               ;BASIC
         LDX POINTL        ;ENDE
         STX ENDL          ;SETZEN!
         LDX POINTH        
         STX ENDH          
         CLI               
         JMP ENDSR         
;
;*****************************
;* TIMER SETZEN (BAUDRATE)
;*****************************
;
SETTIMER BPL SET1          
         LDX #HISPDL       ;7200
         LDY #HISPDH       ;BAUD
         LDA #HICMP        
         BNE SET2          
SET1     LDX #LOSPDL       ;3600
         LDY #LOSPDH       ;BAUD
         LDA #LOCMP        
SET2     STX CNTAL         ;TIMER
         STY CNTAH         ;WERT
         STA TLIM          ;ZEITLIMIT F.T3
         LDA #$7F          
         STA ICR           
         LDA #$90          
         STA ICR           ;TIMER INTERRUPT
         RTS               
;*****************************
;* LOAD ROUTINE
;*****************************
;
LOAD     LSR               ;BIT0->C
         ROR               ;C->BIT7
         STA TEMPA         ;VERIFY FLAG
         LDA #0            
         STA STATUS        
         LDA DN            ;DEVICE
         CMP #7            ;SUPERTAPE
         BEQ LO1           
         LDA TEMPA         ;BIT7->
         ASL               
         ROL               ;BIT0
         JMP (BSLOD)       ;ORIGINAL
LO1      LDY #(MSGPPT-MSGTXT)
         JSR MSG           ;PRESS PLAY...
         LDA #1            
         STA MFLAG         
         LDA #<(PUFFER)    ;KASSETTENPUFFER
         STA CBUF          ;SETZEN
         LDA #>(PUFFER)    
         STA CBUF+1        
LO3      JSR STP           ;FILE SUCHEN
         BCC LO4           
         TAX               ;BREAK?
         BEQ LOBRK         
         CMP #02           ;PARA.BLOCK
         BNE LO31          ;FEHLERHAFT?
         LDY #(MSGBPB-MSGTXT)
LO32     JSR MSG           
         BCC LO3           
LO31     CMP #01           ;FILENAMENSYNTAX FALSCH?
         BNE LO3           ;SONST WEITER SUCHEN
         LDY #(MSGIFN-MSGTXT)
         BNE LO61          
LO4      PHA               ;FEHLERCODE
         LDY #$63          ;'FOUND'
         JSR BSMSG         
         JSR OUTNAM        ;FILENAME
         LDA #4            
         STA BUFF          
LO5      JSR DELAY         
         DEC BUFF          ;2 SEK
         BNE LO5           
         PLA               
         TAX               
         BEQ LO3           ;FILE PASST NICHT
         JSR LVM           ;'LOADING/VERIFYING'
         JSR OUTNAM        ;FILENAME
         JSR STL           ;LOAD DATA
         BCC LOAUTO        
         TAX               ;BREAK?
         BNE LO6           
LOBRK    JMP SVBREAK       
LO6      CMP #2            ;BAD CHECKSUM?
         BNE LO7           
         LDY #(MSGBCS-MSGTXT)
LO61     JSR MSG           
         BCC LO81          
LO7      CMP #3            ;SYNC-ERROR?
         BNE LO8           
         LDY #(MSGSE-MSGTXT)
         BNE LO61          
LO8      CMP #4            ;VERIFY-ERROR?
         BNE LOEXIT        
         LDA STATUS        
         ORA #$10          
         STA STATUS        
         JMP SVEXIT        
LO81     JSR SVEXIT        
         LDA #$1D          
         SEC               
         .BY $24           
LOEXIT   CLC               
         LDX ENDL          
         LDY ENDH          
         RTS               
LOAUTO   JSR SVEXIT        
         BIT TEMPA         ;VERIFY?
         BMI LOEXIT        
         LDY #13           ;BASIC
LO10     LDA (CBUF),Y      ;AUTOSTART
         CMP LOAUT-13,Y    
         BNE LO111         
         INY               
         CPY #15           
         BNE LO10          
         LDY #5            
         STY PUFLEN        
LO11     LDA LORUN-1,Y     
         STA TPUF-1,Y      
         DEY               ;TASTATURPUFFER
         BNE LO11          
LO111    LDY #13           ;M/C
LO12     LDA (CBUF),Y      ;AUTOSTART
         CMP LOCOM-13,Y    
         BNE LOEXIT        
         INY               
         CPY #15           
         BNE LO12          
         LDY #17           
         LDA (CBUF),Y      ;ANF.ADR.
         STA POINTL        ;COM-FILE
         INY               ;STARTEN
         LDA (CBUF),Y      
         STA POINTH        
         JSR LOEXIT        
         JMP (POINTL)      
LOAUT    .TX "AUT"         
LORUN    .TX "RUN:"        
         .BY $0D           
LOCOM    .TX "COM"         
;*****************************
;* LOAD POSITIONIER-ROUTINE
;*****************************
;
STP      JSR LNAME         
         BCC STPNOK        
         LDA #01           ;SYNTAXFEHLER
         RTS               
STPNOK   TSX               ;STACKPOS.
         STX STKT          ;MERKEN
         JSR SETUP         
         JSR DELAY         
         LDA #0            ;N-FLAG=0=>3600 BAUD
         JSR SETTIMER      ;TIMER SETZEN
         CLI               ;LOS GEHTS
LOSYNC   JSR INSYN         ;SYNCHRONISIEREN
         CMP #$2A          ;STARTZEICHEN?
         BNE LOSYNC        
         STY CHKSML        ;PRUEFSUMME
         STY CHKSMH        ;LOESCHEN
STPREAD  JSR RDBYTE        ;BYTE VOM BAND
         STA (CBUF),Y      ;ABLEGEN
;
         LDX CHKSML        
STPPLOOP LSR               ;PRUEFSUM
         BCC STPPNULL      ;BERECHNEN
         INX               
         BNE STPPLOOP      
         INC CHKSMH        
         BEQ STPPLOOP      ;UEBERLAUF - WEITER!
STPPNULL BNE STPPLOOP      ;ALLE 1EN GEZAEHLT?
         STX CHKSML        
         INY               
         CPY #$19          ;BLOCK ENDE
         BCC STPREAD       
         JSR RDBYTE        
         CMP CHKSML        
         BNE STPCSERR      
         JSR RDBYTE        
         CMP CHKSMH        ;PRUEFSUMME
         BNE STPCSERR      ;TESTEN
         LDY #0            
         LDX #0            
STPLOOP  LDA (INBUF),Y     
         CMP #"."          
         BEQ STPDOT        
         CMP #"*"          
         BEQ STPEXT        ;2. STERN?
         CMP #"?"          
         BEQ STPNEXT       
STPCMP   CMP PUFFER,X      
         BNE STPCONT       ;VERSCHIEDEN!
STPNEXT  INX               
STPNXTIN INY               
         CPY NLEN          ;INPUTENDE?
         BCC STPLOOP       ;WEITER
STPFND   LDA #1            ;GEFUNDEN!
         .BY $2C           ;SKIP NAECHTEN BEFEHL
STPCONT  LDA #0            
         CLC               
         BCC STPEXIT       ;EXIT
STPCSERR LDA #2            ;CHECKSUM
         SEC               ;ERROR
STPEXIT  CLI               ;INTERRUPTS ERLAUBEN
         JMP ENDSR         ;ENDE!
STPEXT   CPX #13           ;IN TYP FELD (NACH PUNKT)
         BCS STPFND        ;JA, GEFUNDEN!
         LDX #12           ;PUNKTPOS.
         BNE STPNXTIN      ;WEITER
STPDOT   LDX #12           ;PUNKTPOS.
         BNE STPCMP        ;IMMER VERGLEICHEN!
;
OUTNAM   LDA #" "          
         JSR BSOUT         
         LDY #0            
OUT1     LDA (CBUF),Y      
         JSR BSOUT         
         INY               
         CPY #16           
         BCC OUT1          
         RTS               
;*****************************
;* WRITE BYTE
;*****************************
;
WRTAB    STA CHAR          ;GANZES BYTE
         LDY #8            ;ALLE 8 BIT
         BNE WBY1          ;IMMER
WRTA     STA CHAR          ;LOW NIBBLE
WRTB     LDY #4            ;HIGH NIBBLE
WBY1     LDA TIM0          
         LSR CHAR          
         BCC WBY2          ;'0' OD. '1'
         LDA TIM1          ;ZEIT F. '1'
WBY2     STA CNTBL         
         LDA #$01          
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
WBY5     INC CHKSML        ;PRUEFSUMME
         BNE WBY6          
         INC CHKSMH        ;BERECHNEN
WBY6     DEY               
         BNE WBY1          
         RTS               
;*****************************
;* WRITE BLOCK
;*****************************
;
WRITE    PHA               
         LDX #64           ;ANZAHL D. SYNCS
WBL1     LDA #$16          ;SYNC
         JSR WRTAB         
         DEX               
         BNE WBL1          
         PLA               ;BLOCKKENNUNG
         JSR WRTAB         ;SCHREIBEN
;Y-REG NACH WRTAB IST IMMER 0!
         STY CHKSML        ;PRUEFSUMME
         STY CHKSMH        ;LOESCHEN
         LDX POENL         
WBL2     LDA (POINTL),Y    
         JSR WRTA          
         INC POINTL        ;ZEIGER ERHOEHEN
         BNE WBL3          
         INC POINTH        
WBL3     JSR WRTB          
         CPX POINTL        ;BLOCKENDE
         BNE WBL2          
         LDA POINTH        
         CMP POENH         
         BNE WBL2          
         LDX CHKSMH        ;PRUEFSUMME
         LDA CHKSML        ;SPEICHERN
         JSR WRTAB         
         TXA               
         JSR WRTAB         
         JMP WRTA          ;SCHLUSSBIT
;*****************************
;* NAMENSPRUEFUNG SAVE
;*****************************
;
SNAME    LDY #16           
         LDA #" "          
SNCLEAR  STA PUFFER-1,Y    ;PUFFER
         DEY               ;LOESCHEN
         BNE SNCLEAR       
         LDX #0            ;VORHER Y=0
         BEQ SNCHECK       ;IMMER
SNLOOP   LDA (INBUF),Y     ;FILENAME
         CMP #"*"          ;PLATZHALTER
         BEQ SNERROR       ;ILLEGAL
         CMP #"?"          ;PLATZHALTER
         BEQ SNERROR       ;ILLEGAL
         CMP #"."          
         BNE SNSTORE       
         CPY #13           ;PUNKTPOS.
         BCS SNERROR       ;NAME ZU LANG - FEHLER!
         CPX #13           ;PUNKT DOPPELT - FEHLER!
         BCS SNERROR       
         LDX #12           ;PUNKTPOS.
SNSTORE  STA PUFFER,X      ;PUFFER
         INX               ;UEBERTRAGEN
         CPX #16           ;PUFFERENDE?
         BEQ SNOK          ;VOLL!
         INY               ;NAECHSTES
SNCHECK  CPY NLEN          ;EINGABE ENDE?
         BCC SNLOOP        ;WEITER
SNOK     CLC               ;C-FLAG
         RTS               
SNERROR  SEC               ;ERROR
         RTS               
;*****************************
;* SUPERTAPE BETRIEBSSYS.SAVE
;*****************************
;
SAVE     LDA DN            ;DEVICE#
         CMP #7            ;SUPERTAPE?
         BEQ SV2           
SV1      JMP (BSSAV)       ;ZUM ALTEN VEKTOR
SV2      LDY #(MSGPPRT-MSGTXT)
         JSR MSG           ;PRESS PLAY ...
         LDA #1            
         STA MFLAG         
         LDA #<(PUFFER)    
         STA CBUF          
         LDA #>(PUFFER)    ;KASSETTEN-PUFFER
         STA CBUF+1        ;BESTIMMEN
         JSR STS           ;ABSPEICHERN
         BCC SVEXIT        
         CMP #00           ;BREAK?
         BNE SVIFN         
SVBREAK  JSR SVEXIT        ;BREAK
         LDA #0            ;MELDUNG
         SEC               
         RTS               
SVIFN    LDY #(MSGIFN-MSGTXT)
         JSR MSG           ;INVALID FILENAME
SVEXIT   JSR CIAINIT       
         CLC               
         CLI               
         RTS               
;*****************************
;* SUPERTAPE SAVE
;*****************************
;
STS      JSR SNAME         
         BCC STS0          
         LDA #01           ;ERRORCODE
         RTS               
STS0     TSX               
         STX STKT          ;STACK MERKEN
         JSR SETUP         
         JSR DELAY         
         LDY #16           
         LDA SECA          ;SEK.ADR.
         STA (CBUF),Y      ;BAUDRATE
         INY               
         LDA ANFL          ;STARTADR.
         STA (CBUF),Y      
         INY               
         LDA ANFH          
         STA (CBUF),Y      
         INY               
         SEC               ;LAENGE
         LDA ENDL          
         SBC ANFL          
         STA (CBUF),Y      
         INY               
         LDA ENDH          
         SBC ANFH          
         STA (CBUF),Y      
         INY               
         LDA #0            
STS1     STA (CBUF),Y      
         INY               
         CPY #25           
         BCC STS1          
         STA CNTBH         ;TIMERWERT
         LDA CBUF          
         STA POINTL        
         LDX CBUF+1        
         STX POINTH        
         CLC               
         ADC #$19          ;PUFFERLAENGE
         STA POENL         
         BCC STS11         
         INX               
STS11    STX POENH         
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
         BPL STS2          
         LDA #HSL1         ;7200
         STA TIM1          
         LDA #HSL0         
         STA TIM0          
         STA CNTBL         ;TIMER SETZEN
STS2     LDA ANFL          ;START
         STA POINTL        
         LDA ANFH          
         STA POINTH        
         LDA ENDL          ;ENDE
         STA POENL         
         LDA ENDH          
         STA POENH         
         LDA #$C5          ;DATENBLOCK
         JSR WRITE         ;SCHREIBEN
         CLC               
         CLI               
         JMP ENDSR         
;*****************************
;* SETUP SAVE/LOAD VORBEREITEN
;*****************************
;
SETUP    SEI               
         LDA NMIV          
         STA NMIT          
         LDA NMIV+1        
         STA NMIT+1        
         LDA #<(BRKSR)     
         STA NMIV          
         LDA #>(BRKSR)     ;NMI-VEKTOR
         STA NMIV+1        ;SETZEN
         LDA IRQV          
         STA IRQT          
         LDA IRQV+1        
         STA IRQT+1        
         LDA #<(RDDATA)    ;BYTELESE
         STA IRQV          
         LDA #>(RDDATA)    ;ROUTINE
         STA IRQV+1        
         LDA #$10          ;AUF BANDTASTE
SU2      BIT CPORT         ;WARTEN
         BNE SU2           
         BIT CPORT         ;ENT-
         BNE SU2           ;PRELLEN
         LDA CPORT         
         AND #$1F          ;MOTOR
         STA CPORT         ;EIN
         LDA VICRG         ;SCREEN
         AND #$EF          ;DISABLE
         STA VICRG         
         RTS               
;*****************************
;* SUPERTAPE EINSPRUNG
;*****************************
;
BEGIN    LDA $330          
         STA BSLOD         
         LDA #<(LOAD)      ;LOAD
         STA $330          ;VEKTOR
         LDA $331          
         STA BSLOD+1       
         LDA #>(LOAD)      
         STA $331          
         LDA $332          
         STA BSSAV         
         LDA #<(SAVE)      ;SAVE
         STA $332          ;VEKTOR
         LDA $333          
         STA BSSAV+1       
         LDA #>(SAVE)      
         STA $333          
         LDY #0            
         JMP MSG           
.EN
