	org $4f00

****************************************
* supertape fuer 6809          *********
* autor roland wiese           *********
* kommentare von johann klasek *********
****************************************

* puffer fuer eingabe parameter
name	equ	$4f00	; name 12 zeichen
punkt	equ	$4f0c	; punkt 1 zeichen
typ	equ	$4f0d	; typ 3 zeichen
flag	equ	$4f10	; steuerbyte:
			; bit 0,1: modus
			;   00 load
			;   01 save
			;   10 verify
			; bit 2: startadresse vorgeben
			;    0 von parameterblock
			;    1 von eingabe
			; bit 7: baudrate
			;    0 3600 baud
			;    1 7200 baud
start	equ	$4f11	; startadresse (2 byte)
laeng	equ	$4f13	; laenge (2 byte)
res1	equ	$4f15	; reserviert (2 byte)
res2	equ	$4f17	; reserviert (2 byte)
ende	equ	$4f19	; endadresse + 1 (start+laenge, 2 byte)
pruef	equ	$4f1b	; pruefsumme (2 byte)

nr	equ	$4f1d	; index in ascii-format (2 byte)

* puffer fuer empfangsparameter
ldname	equ	$4f20	; name 12 zeichen
ldpunk	equ	$4f2c	; punkt 1 zeichen
ldtyp	equ	$4f2d	; typ 3 zeichen
ldflag	equ	$4f30	; steuerbyte (baudrate)
ldstar	equ	$4f31	; startadresse (2 byte)
ldlaen	equ	$4f33	; laenge (2 byte)
ldres1	equ	$4f35	; reserviert (2 byte)
ldres2	equ	$4f37	; reserviert (2 byte)
			; berechnet:
ldende	equ	$4f39	; endadresse + 1 (start+laenge, 2 byte)
			; 2 byte puffer fuer hex-ausgabe

byte	equ	$4f50	; zuletzt gelesener portstatus
zahl	equ	$4f51	; 16-bit zwischenwert (2 byte)
baud	equ	$4f53	; aktuelle baudrate

* eurocom-spezifisch:
piaac0	equ	$fcf1	; tast-port-kontrollregister
crb	equ	$fcf3	; port-kontrollregister
pb	equ	$fcf2	; port-darichtregister
hks	equ	$f02d	; monitorprogramm
outasc	equ	$f33d	; ausgabe ein ascii-zeichen
in	equ	$f321	; eingabe ein ascii-zeichen

	org	$5000
	lbra	start1

* ausgabe texte

tmode	fcc	"save=s : load=l : verify=v : "
	fcb	$04
tnam	fcc	"name : "
	fcb	$04
ttyp	fcc	"typ  : "
	fcb	$04
tstart	fcc	"start: "
	fcb	$04
tende	fcc	"ende : "
	fcb	$04
tbaud	fcc	"baudrate 3600 = 0, 7200 = 1 : "
	fcb	$04
tfehl	fcc	"eingabefehler, bitte neu eingeben!"
	fcb	$04
tok1	fcc	"record on?"
	fcb	$04
tok	fcc	"ok? (cr)"
	fcb	$04
tok2	fcc 	"ok"
	fcb	$04
tfpp	fcc	"fehler in parameterblock!      "
	fcb	$04
tfpd	fcc	"fehler im datenblock!        "
	fcb	$04
tnr	fcc	"nr name         typ start   ende"
	fcb	$04
tstop	fcc	"save:load:verify? (yes=y) "
	fcb	$04
tgo	fcc	"go"
	fcb	$04

***********************************************
* supertape-hauptprogramm start bei $5000 - - *
***********************************************
	setdp	$00

start1	clr	flag
	ldx	#$3030		; "00" index anfangswert
	stx	nr		; index bandverzeichnis
	ldb	#4		; 4 versuche
eingab	ldx	#tmode		; aktion abfragen
	lbsr	out
	lbsr	in
	cmpa	#$53		; "s"
	beq	sv0
	cmpa	#$4c		; "l"
	beq	ld0
	cmpa	#$56		; "v"
	beq	ve0
	decb			; versuchszaehler
	lbeq	aus		; exit
	ldx	#tfehl		; fehlermeldung
	lbsr	out		; ausgeben
	bra	eingab		; naechster versuch

sv0	lda	#$01		; save-modus
	sta	flag		; merken
	bra	ld0		; weiter bei load
ve0	lda	#$02		; verify-modus
	sta	flag
ld0	ldx	#tnam		; name erfragen
	lbsr	out
	ldx	#name
	stx	zahl		; startzeiger
	ldb	#12		; laenge
	stb	byte		; fuer prnaty
	abx
	leax	-1,x		; start+len-1
	stx	pruef		; pufferendposition
	ldx	#name		; zielpuffer
	lbsr	prnaty		; einlesen
	bcs	ld0		; nochmal bei fehleingabe
	lda	flag		; modus
	cmpa	#$00		; load?
	bne	ty0		; weiter mit typ bei save oder verify
	ldx	#name		; eingabepuffer
ld1	lda	,x+		; nach
	cmpa	#$2e		; "." absuchen
	lbeq	ok		; punkt gefunden, inhaltsverzeichnis!
	cmpx	#name+11	; pufferende?
	bls	ld1		; weiter suchen
				; sonst...
ty0	ldx	#ttyp		; typ abfragen
	lbsr	out
	ldx	#typ
	stx	zahl
	ldb	#3		; laenge
	stb	byte		; fuer prnaty
	abx
	leax	-1,x		; start+len-1
	stx	pruef		; pufferendposition
	ldx	#typ		; zielpuffer
	lbsr	prnaty		; einlesen
	bcs	ty0		; nochmal bei fehleingabe
	lda	flag		; modus
	cmpa	#$02		; verify?
	lbeq	ok		; dann nur einlesen

st0	ldx	#tstart		; startadresse abfragen
	lbsr	out
	lbsr	hex0		; hex-zahl einlesen
	bcs	st0		; wiederholen bei syntaxfehler
	ldy	zahl		; wert holen
	sty	start		; als startadresse
	lda	flag		; modus
	cmpa	#$01		; save?
	beq	st2		;
	cmpb	#4		; leereingabe?
	beq	st1		; zu ok-abfrage
	ora	#$04		; steuerbyte: startadr.
	sta	flag		; vorgeben bei load/verify
st1	lbra	ok		; ok-abfrage
st2	cmpb	#4		; nur save: leereingabe?
	bne	end		; startadr. -> endadr.
	ldx	#tfehl		; startadr. leer ->
	lbsr	out		; fehler
	bra	st0		; neueingabe startadr.

end	ldx	#tende		; endadresse+1 abfragen
	lbsr	out
	lbsr	hex0		; hex-zahl einlesen
	bcs	end		; wiederholen bei syntaxfehler
	ldy	zahl		; wert holen
	sty	ende		; als endadresse+1
	cmpb	#4		; leereingabe?
	beq	en1		; ja, dann fehler
	ldx	start
	cmpx	ende		; start < ende? (laenge=ende-start)
	bmi	ba0		; ja, weiter mit baudrate
en1	ldx	#tfehl		; fehler bei eingabe
	lbsr	out
	bra	st0		; wieder startadr. abfragen

ba0	ldx	#tbaud		; baudrate abfragen
	lbsr	out
	lbsr	in		; ziffer (0,1) erwartet
	anda	#$0f		; $30/$31 -> 0/1
	beq	ok1		; 3600 baud
	cmpa	#$01		; 1?
	beq	ba1		; ja, dann 7200 baud
	ldx	#tfehl		; fehler bei eingabe
	lbsr	out
	bra	ba0		; erneut abfragen
ba1	lda	flag		; bit 7 ist
	ora	#$80		; 7200-baud-marke
	sta	flag

ok1	ldx	#tok1		; "record on?"
	lbsr	out		; nur bei save
ok	ldx	#tok		; "ok?" bei load/verify
	lbsr	out		; und auf
	lbsr	in		; eingabe warten
	cmpa	#$0d		; nur wenn "cr"
	lbne	aus		; kein "cr", dann ende
	ldx	#tgo		; sonst "go"
	lbsr	out
	lbra	on		; zur eigentlichen aktion

************************************************
* up fuer ein-ausgabe - - - - - - - - - - - - -*
************************************************

* pruefe zulaessige eingabe- - - - - - - - - - -

; in: x		eingabepufferadresse
;     zahl	eingabepufferadresse
;     pruef	letzte pufferadresse
;     byte	anzahl der zeichen (via headin)
;     flag	modus
; out: carry-flag  0|1 ... ok | syntaxfehler
; veraendert: a,b,x

prnaty	lbsr	headin		; eingabe in x
	ldb	flag		; modus
	cmpb	#$00		; bei load alles ok
	beq	praus		; und exit
				; fuer verify+save:
	ldx	zahl		; x wieder herstellen
p1	lda	,x+		; enthaelt eingabe
	cmpa	#$2e		; einen punkt
	beq	fhead		; ja, fehler
	cmpx	pruef		; pufferende?
	bne	p1		; nein, weiterpruefen
	lda	flag		; modus
	cmpa	#$02		; verify?
	beq	praus		; ja, dann ok und exit
				; fuer save:
	ldx	zahl		; x wieder herstellen
p2	lda	,x+		; enthaelt eingabe joker?
	cmpa	#$3f		; fragezeichen "?"?
	beq	fhead		; fehler
	cmpa	#$2a		; stern "*"?
	beq	fhead		; fehler
	cmpx	pruef		; pufferende?
	bne	p2		; nein, weiterpruefen

praus	clc			; carry=0: eingabe ok
	rts

fhead	ldx	#tfehl		; fehlermeldung
	lbsr	out		; ausgeben und
	sec			; carry=1: eingabefehler
	rts

* lese ascii-zeichen - - - - - - - - - -  - - - -

; in: x		pufferadresse
;     zahl	pufferadresse
;     byte	anzahl der zeichen
;     pruef     letzte pufferadresse
; out: (x)	eingelesene zeichen
; veraendert: a,b,x,y

headin	lbsr	in		; zeichen einlesen
	cmpa	#$0d		; zeilenende (cr)?
	beq	heaaus		; fertig
	cmpb	byte		; laengen-counter
	bne	head11		; am start?
	ldy	#$2020		; 2 leerzeichen
head12	sty	,x+		; 2 bytes löschen
	cmpx	pruef		; letztes pufferbyte erreicht?
	bne	head12		; weiter mit padding
	ldx	zahl		; wieder zum pufferstart
head11	sta	,x+		; eingelesenes zeichen speichern
	decb			; anzahl herunterzaehlen
	bne	headin		; bei 0 fertig
heaaus	rts

* change hex in ascii - - - - - - - - - - - - - -

; in: x,(x)	adresse auf 16-bit-wert
; out: (x)	4 bytes mit hex-darstellung
; veraendert: a,b,u,x,y

hexasc	ldu	#2		; 2 hex-bytes
hexas	ldy	#4		; bit-zaehler
	lda	#0		; zu
	ldb	1,x		; low-byte
h1	aslb			; oberes
	rola			; nibble in a
	leay	-1,y		; bit-counter
	bne	h1		; alle 4 bits?

	ldy	#4		; 4 mal
h2	lsrb			; b wieder zurueck
	leay	-1,y		; schieben
	bne	h2

	cmpa	#$09		; a in hex-ziffer
	ble	h3		; 0-9?
	adda	#7		; "a"-"f"
h3	adda	#$30		; ascii-zeichen
	cmpb	#$09		; b in hex-ziffer
	ble	h4		; 0-9?
	addb	#7		; "a"-"f"
h4	addb	#$30		; ascii-zeichen
	leau	-1,u		; byte-zaehler
	cmpu	#0		; alle bytes?
	beq	h5		; fertig
	std	2,x		; 2 hex-ziffern ablegen
	leax	-1,x		; zum high-byte
	bra	hexas
h5	std	1,x		; x schon vermindert
				; korrigiert, 2 hex-ziffern
	rts			; statt wert ablegen

* dokumentiere auf bildschirm - - - - - - - - - -

; in: nr
;     (ldname)..(ldende+1)
; out: (ldstar)..(ldstar+3)	startadresse im hex-format 
;      (ldende)..(ldende+3)	endadresse im hex-format 
;      (ldende+4)		endemarke
; veraendert: a,b,u,x,y

direct	ldx	nr		; bandindex als ascii-nummer
	cmpx	#$3030		; "00"?
	bne	d0		; nein, dann tabellenzeile
	ldx	#tnr		; tabellenkopf ausgeben
	lbsr	out
d0	ldd	nr		; bandindex
	cmpd	#$3939		; = "99"
	bne	d1
	ldd	#$3030		; ueberrollen auf "00"
	std	nr
d1	cmpb	#$39		; einerstelle
	bne	d2		; ="9" (ueberlauf)?
	adda	#$01		; zehnerstelle +1
	ldb	#$2f		; "0"-1
d2	addb	#$01		; einerstelle +1
	std	nr		; neuen wert speichern
	ldx	#ldstar		; startadresse nach hex
	lbsr	hexasc		; ueberschreibt ldlaen
	ldx	#ldende		; endadresse nach hex
	lbsr	hexasc		; ueberschreibt 2 folgebytes
	lda	#$20		; statt punkt ein leerzeichen
	sta	ldpunk
	sta	ldflag		; steuerbyte auch als leerzeichen
	sta	nr+2
	ldx	#$2020		; reserviert sicherheitshalber
	stx	ldres1		; mit leerzeichen fuellen
	stx	ldres2
	lda	#$04		; ausgabeendemarkierung
	sta	ldende+4
	ldx	#nr		; ausgabestart 
	lbsr	out		; von nr bis ldende+3 ausgeben
	rts

* change ascii in hex - - - - - - - - - - - - -

; in: zahl	adresse auf 16-bit-wert
; out: (zahl)		16-bit-wert
;      b		(4 - anzahl der ziffern), 4 ... leereingabe
;      carry-flag	0|1 ... ok | syntaxfehler
; veraendert: a,b,u,y,x (via out)

hex0	ldu	#0		; ergebnis register
	stu	zahl		; mit 0 initialisieren
	ldb	#4		; max. 4 zeichen
hex	lbsr	in		; zeichen von tastatur
	cmpa	#$0d		; eingabeende mit return
	beq	hexaus		; ende
	cmpa	#$30		; < '0'
	bmi	hexf		; ja, dann fehler
	cmpa	#$39		; <= '9'
	ble	in1		; ja, dann gut ziffer
	cmpa	#$41		; < 'a'
	bmi	hexf		; ja, dann fehler
	cmpa	#$46		; > 'f'
	bgt	hexf		; ja, dann fehler
	suba	#7		; a-f-korrektur
in1	anda	#$0f		; ascii-bits ausblenden

	ldy	#4		; bit-zaehler
hex1	asl	zahl+1		; vorlaeufiges ergebnis
	rol	zahl		; insgesamt: * 16
	leay	-1,y		; alle bits?
	bne hex1		; weiter
	ora	zahl+1		; wert neu eingelesene
	sta	zahl+1		; ziffer dazu
	decb			; hex-ziffern-zaehler
	bne	hex		; maximalanzahl erreicht
hexaus	rts			; ok: carry implizit = 0

hexf	ldx	#tfehl		; fehler in hex-zahl
	lbsr	out
	sec			; fehler: carry = 1
	rts

* standard ausgabe cr+lf+text - - - - - -

; in: x		adresse auf zeichenkette
;     (x)	zeichenkette
; out: -
; veraendert: a,x

out	lda	#$0d		; "cr"
	lbsr	outasc		; ausgeben
	lda	#$0a		; "lf"
	lbsr	outasc		; ausgeben
out2	lda	,x+		; zeichen
	cmpa	#$04		; endemarkierung?
	beq	out1		; ja, fertig
	lbsr	outasc		; sonst ausgeben
	bra	out2		; weiter
out1	rts

* eingabe ende - - - - - - -- - - - - - -

; ab hier keine betriebssystemroutinen mehr, daher
; jetzt interrupts aus und directpage angepasst.

on	orcc	#$50		; firq+irq abschalten
	lda	#$4f		; directpage setzen auf
	tfr	a,dp		; $4f00-$4fff
	setdp	$4f

	clr	crb		; pia control register port b
				; bit 2 =0: data direction r.
	lda	#$7f		; pb7 auf eingabe
	sta	pb		; rest auf ausgabe
	lda	#$04		; zurueck zu port register
	sta	crb

	lda	flag		; lade steuerbyte
	anda	#$01		; isoliere modus
	cmpa	#$01		; save?
	lbeq	save		; andernfalls load oder verify

***********************************************
* load supertape - -hp 28 zyklen - - - - - - -*
***********************************************

load	lda	#$00		; initialisierung
	sta	baud		; 3600 baud
	lda	#$80		; bit 7 gesetzt
	sta	byte		; fuer letzten eingangsstatus
	ldb	#$ff
	ldx	#$00		; fuer pruefsumme ruecksetzen

sycpar	lda	piaac0		; tastatur: pia cr port a (int flag)
	lbmi	aus		; bit7=1 break
	nop			; warte 2+4
	ldy	#$01
	nop			; warte 2+4
	ldy	#$01		; bitzaehler = 1
	lbsr	rdbit		; empfange 1 bit (in b)
	cmpb	#$16		; sync erkannt?
	bne	sycpar		; weiter synchronisieren

sycp	cmpu	baud		; warte 14
	cmpu	baud
	lbsr	rdbyte		; empfange 1 byte
	cmpb	#$16		; noch sync-zeichen?
	beq	sycp		; ja, weiter alle lesen

	cmpb	#$2a		; startbyte parameterblock?
	bne	load		; nein, dann naechsten block
	stx	pruef		; pruefsumme zuruecksetzen

* empfange parameter - - - - - - - - - - - - -
* name, typ, flag    - - - - - - - - - - - - -

	ldx	#ldname		; zielpuffer
pantf	nop			; warte 2
	lbsr	rdbyte		; ein parameterbyte
	stb	,x+		; in den puffer
	cmpu	#$00		; warte 5
	cmpx	#ldflag		; name+flag fertig?
	bls	pantf		; nein, weiter einlesen
	bra	par		; sonst zweiten teil

* empfange restliche parameter - - - - - - -

parest	ldu	#$00		; warte 6
	ldu	#$00
				; zweiter teil mit 16-bit-werten
par	lbsr	rdbyte		; empfange low-byte
	stb	1,x		; big-endian an hoeherer adresse
	mul			; warte 15
	cmpx	#$00
	lbsr	rdbyte		; empfange high-byte
	stb	,x++		; big-endian an niedrige adresse
	cmpx	#ldres2		; parameterblockende?
	bls	parest		; nein, weiter einlesen

* empfange pruefsumme parameter - - - - - - -

	ldx	pruef		; bisherige pruefsumme
	nop			; warte 2
	lbsr	rdbyte		; low-byte pruefsumme
	stx	pruef		; blockprufsumme wieder herstellen
	cmpb	pruef+1		; pruefsumme-low vergleichen
	lbeq	re1		; wenn gleich weiter
	ldx	#tfpp		; nicht gleich! fehler
	lbsr	out		; fehler pruefsumme
	lbra	stop		; ende
re1	cmpx	#$00		; warte 6
	nop
	lbsr	rdbyte		; high-byte pruefsumme
	stx	pruef		; blockprufsumme wieder herstellen
	cmpb	pruef		; pruefsumme-high vergleichen
	beq	re2		; wenn gleich weiter
	ldx	#tfpp		; nicht gleich! fehler
	lbsr	out		; fehler pruefsuumme
	lbra	stop		; ende

* vergleiche name - - - - - - - - - - - - - - -

re2	ldy	#ldname		; name von band
	ldx	#name		; name angegeben
n0	ldb	,x+		; hole byte angegeben
	cmpb	#$2a		; stern (*)?
	beq	typ1		; ja, dann weiter mit typ
	cmpb	#$3f		; fragezeichen (?)?
	beq	n1		; ja, naechstes zeichen
	cmpb	,y		; gleich bandname?
	beq	n1		; ja, naechstes zeichen
	lbsr	direct		; nicht gleich, dann parameter zeigen
	lbra	load		; naechster versuch
n1	leay	1,y		; weiter im bandnamen
	cmpy	#ldname+11	; fertig?
	bls	n0		; nein, weiter vergleichen

* vergleiche typ - - - - - - - - - - - - - - - -

typ1	ldy	#ldtyp		; typ vom band
	ldx	#typ		; typ angegeben
t0	ldb	,x+		; hole byte angegeben
	cmpb	#$2a		; stern (*)?
	beq	t1		; ja, dann datei gefunden!
	cmpb	#$3f		; fragezeichen (?)?
	beq	t2		; ja, naechstes zeichen
	cmpb	,y		; gleich bandtyp?
	beq	t2		; falscher typ?
	lbsr	direct		; nicht gleich, dann parameter zeigen
	lbra	load		; naechster versuch
t2	leay	1,y		; weiter im bandtyp
	cmpy	#ldtyp+2	; fertig?
	bls	t0		; nein, weiter vergleichen

t1	lda	ldflag		; baudrate vom band einstellen
	sta	baud		; fuer den datenblock

	lda	#$80		; bit 7 gesetzt
	sta	byte		; fuer letzten eingangsstatus
	ldx	#$00		; fuer pruefsumme ruecksetzen

	lda	flag		; steuerbyte
	anda	#$04		; maskiere bit
	cmpa	#$04		; startadresse vorgegeben?
	bne	salt		; nein, ueberspringen
	lda	start		; abgefragte startadresse
	sta	ldstar		; uebernehmen
salt	ldd	ldstar		; berechne block-
	addd	ldlaen		; endadresse + 1
	std	ldende

	ldb	flag		; steuerbyte
	andb	#$02		; modus verify/load ausmaskieren
	cmpb	#$02		; verify?
	beq	ve		; ja
	lbsr	daten		; datenblock laden
	bra	chek		; pruefsumme kontrollieren
ve	lbsr	verify		; datenblock verifizieren

chek	bcc	right		; laden/verify ok
	ldx	#tfpd		; fehler im datenblock!
	lbsr	out
	lbra	stop		; ende
right	ldx	#tok2		; ok ausgeben
	lbsr	out
	ldb	flag		; steuerbyte
	andb	#$03		; modus ausmaskieren
	cmpb	#$00		; load?
	bne	stop		; ende bei save und verify
	lbsr	direct		; dateiinfo anzeigen
stop	ldx	#tstop		; weitere aktion abfragen
	lbsr	out
	lbsr	in
	cmpa	#$5b		; taste "y"?
	lbeq	start1		; wieder zum start

aus	andcc	#$af		; freigabe firq+irq
	jmp	hks		; ins monitorprogramm (muss dp
				; wieder festlegen!)
	nop			; nicht verwendet

***********************************************
* hp-load-ende - - - - - - - - - - - - - - - -*
***********************************************

* up daten - - - - - - - - - - - - - - - - - -

; in: baud			baudrate ($80 = 7200)
;     ldstar			startadresse
;     ldende			endadresse + 1
; out: carry-flag		status ( 0 | 1 ... ok | fehler)
;      (ldstar) ... (ldende)	datenbereich
; veraendert: a,b,u,x,y

daten	ldb	#$ff		; empfangen und einrasten:
sycdar	cmpu	baud		; warte 14
	cmpu	baud
	ldy	#$01		; warte 8
	ldy	#$01		; dabei bitzaehler = 1
	lbsr	rdbit		; empfange 1 bit (in b)
	cmpb	#$16		; sync erkannt?
	bne	sycdar		; weiter synchronisieren

sycd	cmpu	baud		; warte 14
	cmpu	baud
	lbsr	rdbyte		; empfange 1 byte
	cmpb	#$16		; noch sync-zeichen?
	beq	sycd		; ja, weiter alle lesen

	cmpb	#$c5		; startbyte datenblock?
	bne	dat1		; nein, dann fehlerstatus melden
	stx	pruef		; pruefsumme zuruecksetzen

	ldx	ldstar		; startadresse als datenziel
	bra	dat
datlop	cmpu	#$00		; warte 5
dat	lbsr	rdbyte		; ein datenbyte
	stb	,x+		; in den speicher
	cmpx	ldende		; datenendadresse erreicht?
	bls	datlop		; nein, weiterlesen

	ldx	pruef		; bisherige pruefsumme
	cmpx	#$00		; warte 4
	lbsr	rdbyte		; low-byte pruefsumme
	stx	pruef		; blockprufsumme wieder herstellen
	cmpb	pruef+1		; pruefsumme-low vergleichen
	bne	dat1		; fehlerstatus retour
	cmpx	#$00		; warte 6
	nop
	lbsr	rdbyte		; high-byte pruefsumme
	stx	pruef		; blockprufsumme wieder herstellen
	cmpb	pruef		; pruefsumme-high vergleichen
	bne	dat1		; fehlerstatus retour
	clc			; ok status
	rts			; zurueck
dat1	sec			; fehler status
	rts			; zurueck

* up verify - - - - - - - - - - - - - - - - - -

; in: baud			baudrate ($80 = 7200)
;     ldstar			startadresse
;     ldende			endadresse + 1
;     (ldstar) ... (ldende)	datenbereich
; out: carry-flag		status ( 0 | 1 ... ok | fehler)
; veraendert: a,b,u,x,y

verify	ldb	#$ff		; empfangen und einrasten:
sycver	cmpu	baud		; warte 14
	cmpu	baud
	ldy	#$01		; warte 8
	ldy	#$01		; dabei bitzaehler = 1
	lbsr	rdbit		; empfange 1 bit (in b)
	cmpb	#$16		; sync erkannt?
	bne	sycver		; weiter synchronisieren

sycv	cmpu	baud		; warte 14
	cmpu	baud
	lbsr	rdbyte		; empfange 1 byte
	cmpb	#$16		; noch sync-zeichen?
	beq	sycv		; ja, weiter alle lesen

	cmpb	#$c5		; startbyte datenblock?
	bne	ver1		; nein, dann fehlerstatus melden
	stx	pruef		; pruefsumme zuruecksetzen

	ldx	ldstar		; startadresse als datenziel
verlop	lbsr	rdbyte		; ein datenbyte
	cmpb	,x+		; vergleichen
	bne	ver1		; fehlerstatus, wenn abweichung
	cmpx	ldende		; datenendadresse erreicht?
	bls	verlop		; nein, weiter vergleichen

	ldx	pruef		; bisherige pruefsumme
	lbsr	rdbyte		; low-byte pruefsumme
	stx	pruef		; blockprufsumme wieder herstellen
	cmpb	pruef+1		; pruefsumme-low vergleichen
	bne	ver1		; fehlerstatus retour
	cmpx	#$00		; warte 6
	nop
	lbsr	rdbyte		; high-byte pruefsumme
	stx	pruef		; blockprufsumme wieder herstellen
	cmpb	pruef		; pruefsumme-high vergleichen
	bne	ver1		; fehlerstatus retour
	clc			; ok status
	rts			; zurueck
ver1	sec			; fehler status
	rts			; zurueck

* up rdbyte ,rdbit - - - - - - - - - - - - - - - - -

; in: y		bitzaehler (1 oder 8)
;     baud	baudrate ( $00 | $80 ... 3600 | 7200 )
;     byte	pegel beim bitanfang (vom letzten aufruf)
; out: b	eingelesene bits
; veraendert: a,b,u,y

rdbyte	ldy	#$08		; bitzaehler=8
	bra	rdbit		; byte-einstieg
read	ldu	#$01		; warte 41
	ldu	#$01		; bitwartezeit
	lbsr	wa8
rdbit	lbsr	euro1		; euro-wartezeit
	lda	baud		; baudrate
	bmi	rd		; verzweige bei 7200
	ldu	#$05		; warte 93
	ldu	#$05		; bei 3600
	lbsr	wa8
	lbsr	euro1		; euro-wartezeit
rd	lda	pb		; pegelabtastung (ta)
	anda	#$80		; maskiere port und
	cmpa	byte		; pegelvergleich mit (t0)
	beq	rdeins		; wenn gleich, dann eins
	sta	byte		; pegel merken
	clc			; 0-bit carry=0
	rorb			; ergebnis-byte
	bra	flanke		; warte auf flanke
rdeins	sta	byte		; 1-bit: pegel merken
	ldu	pruef		; pruefsumme
	leau	1,u		; erhoehen
	stu	pruef
	sec			; carry=1
	rorb			; ins ergebnis-byte
flanke	lda	pb		; eingang
	anda	#$80		; maskieren
	cmpa	byte		; flankenaenderung?
	beq	flanke		; nein, warten
	sta	byte		; als (t0) pegel merken
	leay	-1,y		; bitzaehler
	bne	read		; alle bits empfangen?
	rts

************************************************
* save supertape- - hp 21 zyklen - - - - - - - *
************************************************

save	clr	baud		; 3600 baud einstellen

	ldd	ende		; endadresse + 1
	subd	start		; minus startadresse
	std	laeng		; ergibt datenblocklaenge

	lda	flag		; steuerbyte
	anda	#$80		; baudrate maskieren
	sta	flag

* sync-zeichen fuer parameter - - - - - - - - -

	ldx	#$40		; zaehler fuer 64 sync
syncpa	ldb	#$16		; sync-zeichen
	lbsr	outch		; sende sync-zeichen
	nop
	leax	-1,x		; sync-zaehler -1
	bne	syncpa		; alle zeichen ausgegeben?

	ldb	#$2a		; startbyte parameterblock
	bsr	outch		; senden

	stx	pruef		; pruefsumme=0

* sende parameterblock - - - - - - - - - - - -

	ldx	#name		; parameterblock pufferstart
parout	ldb	,x+		; parameter-byte
	bsr	outch		; auf band
	cmpx	#flag		; name+flag ausgegeben?
	bls	parout		; nein, weiter

poutr	nop			; restliche 16-bit-parameter
	ldb	1,x		; in little-endian!
	bsr	outch		; low-byte
	cmpu	baud		; warte 7
	ldb	,x++		; high und zeiger+2
	bsr	outch		; high-byte
	cmpx	#res2		; parameterblock fertig?
	bls	poutr		; nein, dann weiter

* sende pruefsumme des parameterblocks - - - -

	ldx	pruef		; rette pruefsumme
	ldb	pruef+1		; pruefsumme low-byte
	bsr	outch		; auf band
	stx	pruef		; gerettete pruefsumme
	stx	pruef		; warte 5
	ldb	pruef		; pruefsumme high-byte
	bsr 	outch		; auf band
	nop			; warte 2
	lda	flag		; angegebene baudrate
	sta	baud		; einstellen fuer datenblock

* sende sync-zeichen des datenblocks - - - - - -

	ldx	#$40		; 64 sync-zeichen
syncda	ldb	#$16		; sync-zeichen
	lbsr	outch		; auf band
	nop			; warte 2
	leax	-1,x		; zeichenzaehler
	bne	syncda		; alle gesendet?

	ldb	#$c5		; startbyte datenblock
	bsr	outch		; auf band
	stx	pruef		; pruefsumme=0

* sende datenblock - - - - - - - - - - - - - -

	ldx	start		; startadresse datenblock
datout	ldb	,x+		; datenbyte
	bsr	outch		; auf band
	cmpx	ende		; ende + 1 erreicht?
	bls	datout		; nein, weiter

* sende pruefsumme datenblock - - - - - - - -

	ldx	pruef		; rette pruefsumme
	ldb	pruef+1		; pruefsumme low-byte
	bsr	outch		; auf band
	stx	pruef		; gerettete pruefsumme
	stx	pruef		; warte 5
	ldb	pruef		; pruefsumme high-byte
	bsr 	outch		; auf band

	clr	pb		; port löschen
	lda	#$01		; steuerbyte
	sta	flag		; auf save-modus

	lbra	right		; ok ausgeben und ende

**********************************************
* ende  hp-save - - - - - - - - - - - - - - -*
**********************************************

* up fuer save - - - - - - - - - - - - - - - -

; in: b		auszugebendes byte
;     baud	baudrate ( $00 | $80 ... 3600 | 7200)
; out: -
; veraendert: a,b,u,y

outch	ldy	#$08		; bit-zaehler
	lbsr	euro		; euro-wartezeit
	lda	baud		; baudrate
	bmi	l3		; 7200?
	ldu	#$03		; 3600: warte 59 zyklen
	ldu	#$03
	lbsr	wa0
	lbsr	euro		; euro-wartezeit
	bra	l3		; byte-einstieg
l1	lda	baud		; bit-einstieg: baudrate
	bmi	l2		; 7200?
	ldu	#$03		; 3600: warte 62 zyklen
	lbsr	wa6
	lbsr	euro		; euro-wartezeit
l2	ldu	#$01		; warte 30
	lbsr	wa0
	lbsr	euro		; euro-wartezeit
l3	lsrb			; bit aus datenbyte
	bcs	eins		; 1-bit?
	lda	pb		; 0-bit:
	eora	#$01		; flanke erzeugen
	sta	pb
	lda	baud		; baudrate
	bmi	l4		; 7200?
	ldu	#$03		; 3600: warte 62 zyklen
	lbsr	wa6
	lbsr	euro		; euro-wartezeit
l4	ldu	#$02		; warte 43 zyklen
	lbsr	wa0
	lbsr	euro		; euro-wartezeit
	lda	pb		; flanke erzeugen
	eora	#$01
	sta	pb
	leay	-1,y		; bitzaehler
	bne	l1		; alle bits ausgegeben?
	rts

eins	ldu	pruef		; pruefsumme
	leau	1,u		; erhoehen
	stu	pruef
	lda	baud		; baudrate
	bmi	l5		; 7200?
	ldu	#$03		; 3600: warte 62 zyklen
	lbsr	wa6
	lbsr	euro		; euro-wartezeit
l5	ldu	#$01		; warte 40 zyklen
	lbsr	wa10
	lbsr	euro		; euro-wartezeit
	lda	pb		; flanke erzeugen
	eora	#$01
	sta	pb
	leay	-1,y		; bitzaehler
	bne	l1		; alle bits ausgegeben?
	rts

* warteschleifen - - - - - - - - - - - - - - -

; in: u			wartezeit: 
;				wa0: u*13+14
;				wa6: u*13+20
;				wa8: u*13+22
;				wa10: u*13+24
; out: u		= 0
; veraendert: u

				; lbsr = 9 zyklen
wa10	nop			;      = 2 zyklen
wa8	nop			;      = 2 zyklen
wa6	nop			;      = 2 zyklen
	nop			;      = 2 zyklen
	nop			;      = 2 zyklen
wa0	leau	-1,u		;      = 5 zyklen
	cmpu	#$00		;      = 5 zyklen
	bne	wa0		;      = 3 zyklen
	rts			;      = 5 zyklen

* eurocom warteschleife  - - - - - - - - - - -
* euro=31 zyklen  euro1=46 zyklen

; in: -
; out: u	= 0
; veraendert: u

				; lbsr = 9 zyklen
euro1	cmpu	#$00		;      = 5 zyklen
	cmpu	#$00		;      = 5 zyklen
	cmpu	#$00		;      = 5 zyklen
euro	cmpu	baud		;      = 7 zyklen
	cmpu	baud		;      = 7 zyklen
	ldu	#$00		;      = 3 zyklen
	rts			;      = 5 zyklen


