	org $4f00

****************************************
*supertape fuer      6809      *********
*******roland wiese*********************
****************************************

name	equ	$4f00	;puffer fuer eingabe
punkt	equ	$4f0c	;parameter
typ	equ	$4f0d
flag	equ	$4f10	;"wichtig" steuerbyte
start	equ	$4f11
ende	equ	$4f19
res1	equ	$4f15
res2	equ	$4f17
laeng	equ	$4f13
pruef	equ	$4f1b

nr	equ	$4f1d
ldname	equ	$4f20	;puffer fuer empfangs
ldpunk	equ	$4f2c	;parameter
ldtyp	equ	$4f2d
ldflag	equ	$4f30
ldstar	equ	$4f31
ldlaen	equ	$4f33
ldres1	equ	$4f35
ldres2	equ	$4f37
ldende	equ	$4f39

byte	equ	$4f50
zahl	equ	$4f51
baud	equ	$4f53

piaac0	equ	$fcf1	;tast-port kontr.
crb	equ	$fcf3	;port-kontrollre.
pb	equ	$fcf2	;port-darichtre.
hks	equ	$f02d	;monitorprogramm
outasc	equ	$f33d	;ausgabe 1.ascii-z
in	equ	$f321	;eingabe 1.ascii-z

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
tbaud	fcc	"baydrate: 3600bd=0 : 7200bd=1 : "
	fcb	$04
tfehl	fcc	"eigabe-fehler.bitte neu eingeben"
	fcb	$04
tok1	fcc	"record on ??"
	fcb	$04
tok	fcc	"ok? ( cr ) :"
	fcb	$04
tok2	fcc 	"ok"
	fcb	$04
tfpp	fcc	"fehler in pruefsumme parameter"
	fcb	$04
tfpd	fcc	"fehler im daten-block    "
	fcb	$04
tnr	fcc	"nr name         typ start   ende"
	fcb	$04
tstop	fcc	"save:load:verify? yes=y :"
	fcb	$04
tgo	fcc	"go"
	fcb	$04

***********************************************
*supertape-hauptprogramm start bei $5000 - - -*
***********************************************
	setdp	$00

start1	clr	flag
	ldx	#$3030
	stx	nr
	ldb	#4
eingab	ldx	#tmode
	lbsr	out
	lbsr	in
	cmpa	#$53		; "s"
	beq	sv0
	cmpa	#$4c		; "l"
	beq	ld0
	cmpa	#$56		; "v"
	beq	ve0
	decb
	lbeq	aus
	ldx	#tfehl
	lbsr	out
	bra	eingab

sv0	lda	#$01
	sta	flag
	bra	ld0
ve0	lda	#$02
	sta	flag
ld0	ldx	#tnam
	lbsr	out
	ldx	#name
	stx	zahl
	ldb	#12
	stb	byte
	abx
	leax	-1,x
	stx	pruef
	ldx	#name
	lbsr	prnaty
	bcs	ld0
	lda	flag
	cmpa	#$00
	bne	ty0
	ldx	#name
ld1	lda	,x+
	cmpa	#$2e
	lbeq	ok
	cmpx	#name+11
	bls	ld1

ty0	ldx	#ttyp
	lbsr	out
	ldx	#typ
	stx	zahl
	ldb	#3
	stb	byte
	abx
	leax	-1,x
	stx	pruef
	ldx	#typ
	lbsr	prnaty
	bcs	ty0
	lda	flag
	cmpa	#$02
	lbeq	ok

st0	ldx	#tstart
	lbsr	out
	lbsr	hex0
	bcs	st0
	ldy	zahl
	sty	start
	lda	flag
	cmpa	#$01
	beq	st2
	cmpb	#4
	beq	st1
	ora	#$04
	sta	flag
st1	lbra	ok
st2	cmpb	#4
	bne	end
	ldx	#tfehl
	lbsr	out
	bra	st0

end	ldx	#tende
	lbsr	out
	lbsr	hex0
	bcs	end
	ldy	zahl
	sty	ende
	cmpb	#4
	beq	en1
	ldx	start
	cmpx	ende
	bmi	ba0
en1	ldx	#tfehl
	lbsr	out
	bra	st0

ba0	ldx	#tbaud
	lbsr	out
	lbsr	in
	anda	#$0f
	beq	ok1
	cmpa	#$01
	beq	ba1
	ldx	#tfehl
	lbsr	out
	bra	ba0
ba1	lda	flag
	ora	#$80
	sta	flag

ok1	ldx	#tok1
	lbsr	out
ok	ldx	#tok
	lbsr	out
	lbsr	in
	cmpa	#$0d
	lbne	aus
	ldx	#tgo
	lbsr	out
	lbra	on

***********************************************
* up"s fuer ein-ausgabe - - - - - - - - - - - -
************************************************

* pruefe zulaessige eingabe- - - - - - - - - - -
prnaty	lbsr	headin
	ldb	flag
	cmpb	#$00		;load ?
	beq	praus		;ja, praus
	ldx	zahl
p1	lda	,x+
	cmpa	#$2e		;punkt
	beq	fhead
	cmpx	pruef
	bne	p1
	lda	flag
	cmpa	#$02
	beq	praus
	ldx	zahl
p2	lda	,x+
	cmpa	#$3f		;fragezeichen
	beq	fhead
	cmpa	#$2a		;stern"*"?
	beq	fhead
	cmpx	pruef		;alles geprueft?
	bne	p2

praus	clc
	rts

fhead	ldx	#tfehl
	lbsr	out
	sec
	rts

* lese ascii-zeichen - - - - - - - - - -  - - - -

headin	lbsr	in
	cmpa	#$0d
	beq	heaaus
	cmpb	byte
	bne	head11
	ldy	#$2020
head12	sty	,x+
	cmpx	pruef
	bne	head12
	ldx	zahl
head11	sta	,x+
	decb
	bne	headin
heaaus	rts

* change hex in ascii - - - - - - - - - - - - - -

hexasc	ldu	#2
hexas	ldy	#4
	lda	#0
	ldb	1,x
h1	aslb
	rola
	leay	-1,y
	bne	h1

	ldy	#4
h2	lsrb
	leay	-1,y
	bne	h2
	cmpa	#$09
	ble	h3
	adda	#7
h3	adda	#$30
	cmpb	#$09
	ble	h4
	addb	#7
h4	addb	#$30
	leau	-1,u
	cmpu	#0
	beq	h5
	std	2,x
	leax	-1,x
	bra	hexas
h5	std	1,x
	rts

* dokumentiere auf bildschirm - - - - - - - - - -

direct	ldx	nr
	cmpx	#$3030
	bne	d0
	ldx	#tnr
	lbsr	out
d0	ldd	nr
	cmpd	#$3939
	bne	d1
	ldd	#$3030
	std	nr
d1	cmpb	#$39
	bne	d2
	adda	#$01
	ldb	#$2f
d2	addb	#$01
	std	nr
	ldx	#ldstar
	lbsr	hexasc
	ldx	#ldende
	lbsr	hexasc
	lda	#$20
	sta	ldpunk
	sta	ldflag
	sta	nr+2
	ldx	#$2020
	stx	ldres1
	stx	ldres2
	lda	#$04
	sta	ldende+4
	ldx	#nr
	lbsr	out
	rts

* change ascii in hex - - - - - - - - - - - - -

hex0	ldu	#0
	stu	zahl
	ldb	#4
hex	lbsr	in
	cmpa	#$0d
	beq	hexaus
	cmpa	#$30
	bmi	hexf
	cmpa	#$39
	ble	in1
	cmpa	#$41
	bmi	hexf
	cmpa	#$46
	bgt	hexf
	suba	#7
in1	anda	#$0f

	ldy	#4
hex1	asl	zahl+1
	rol	zahl
	leay	-1,y
	bne hex1
	ora	zahl+1
	sta	zahl+1
	decb
	bne	hex
hexaus	rts

hexf	ldx	#tfehl
	lbsr	out
	sec
	rts

* standard ausgabe cr+lf+text - - - - - -

out	lda	#$0d
	lbsr	outasc
	lda	#$0a
	lbsr	outasc
out2	lda	,x+
	cmpa	#$04
	beq	out1
	lbsr	outasc
	bra	out2
out1	rts

* eingabe ende - - - - - - -- - - - - - -

on	orcc	#$50		;firq-irq abschalten
	lda	#$4f		;directpage von
	tfr	a,dp		;$4f00-$4fff
	setdp	$4f

	clr	crb		;port setzen
	lda	#$7f		;pb7-eingabe
	sta	pb		;rest ausgabe
	lda	#$04
	sta	crb

	lda	flag		;lade steuerbyte
	anda	#$01		;maskiere und
	cmpa	#$01		;vergleiche db
	lbeq	save		;save ?

***********************************************
*load supertape - -hp 28 zyklen - - - - - - - *
***********************************************

load	lda	#$00		;setze bestimmte
	sta	baud		;anfangs-
	lda	#$80		;bedingungen
	sta	byte
	ldb	#$ff
	ldx	#$00		;x=0 fuer pruef

sycpar	lda	piaac0		;tastatur control
	lbmi	aus		;bit7=1 break
	nop			;zu break siehe
	ldy	#$01		;text !!
	nop
	ldy	#$01
	lbsr	rdbit		;empfange 1-bit
	cmpb	#$16		;sync erkannt?
	bne	sycpar		;nein sycpar

sycp	cmpu	baud		;warte 15
	cmpu	baud
	lbsr	rdbyte		;empfange 1-byte
	cmpb	#$16		;sync-zeichen?
	beq	sycp		;ja dann weiter

	cmpb	#$2a		;startbyte parblock?
	bne	load		;nein von vorne
	stx	pruef		;pruef=0 setzen

*empfange parameter - - - - - - - - - - - - -
* name, typ, flag   - - - - - - - - - - - - -

	ldx	#ldname		;lade zaehler pa.
pantf	nop			;warte 2
	lbsr	rdbyte		;empfange parameter
	stb	,x+		;und speichern
	cmpu	#$00		;warte 5
	cmpx	#ldflag		;parameterende?
	bls	pantf		;nein pantf
	bra	par		;springe nach par

* empfange restliche parameter - - - - - - -

parest	ldu	#$00		;warte 6
	ldu	#$00
par	lbsr	rdbyte		;empf. low-byte pa.
	stb	1,x		;und speicher
	mul			;warte 15
	cmpx	#$00
	lbsr	rdbyte		;empf. high-byte pa.
	stb	,x++		;und speicher
	cmpx	#ldres2		;alle pa. empfangen?
	bls	parest		;nein parest

* empfange pruefsumme parameter - - - - - - -

	ldx	pruef		;rette pruefsumme
	nop			;warte 2
	lbsr	rdbyte		;low-byte pruef empf.
	stx	pruef		;gerette pruef zuru.
	cmpb	pruef+1		;vergleiche pruef-low
	lbeq	re1		;nicht gleich! fehler
	ldx	#tfpp
	lbsr	out		;text fehler pruefsu.
	lbra	stop
re1	cmpx	#$00		;warte 6
	nop
	lbsr	rdbyte		;high-byte pruef emp.
	stx	pruef		;gerettete pruef zur.
	cmpb	pruef		;vergleiche pruef-h.
	beq	re2		;nicht gleich! fehler
	ldx	#tfpp
	lbsr	out		;text fehler pruefsu.
	lbra	stop

*vergleiche name - - - - - - - - - - - - - - -

re2	ldy	#ldname		;name von band
	ldx	#name		;name angegeben
n0	ldb	,x+		;lade "b" mit pa.
	cmpb	#$2a		;stern (*) ?
	beq	typ1		;ja dann typ1
	cmpb	#$3f		;fragezeichen (?) ?
	beq	n1		;ja dann n1
	cmpb	,y		;angegebener name ?
	beq	n1		;falscher name ?
	lbsr	direct		;ja, zeige
	lbra	load
n1	leay	1,y		;erhoehe y um 1
	cmpy	#ldname+11	;name vergleichen ?
	bls	n0		;nein n0

*vergleiche typ - - - - - - - - - - - - - - - -

typ1	ldy	#ldtyp		;typ vom band
	ldx	#typ		;typ angegeben
t0	ldb	,x+		;rest wie beim namen
	cmpb	#$2a	
	beq	t1
	cmpb	#$3f
	beq	t2
	cmpb	,y
	beq	t2		;falscher typ ?
	lbsr	direct		;ja: zeige
	lbra	load
t2	leay	1,y
	cmpy	#ldtyp+2
	bls	t0

t1	lda	ldflag		;baudrate einstellen
	sta	baud		;in baud

	lda	#$80		;setze bestimmte
	sta	byte		;anfangsbedingungen
	ldx	#$00		;fuer pruef

	lda	flag		;lade steuerbyte
	anda	#$04		;maskiere und ver-
	cmpa	#$04		;gleiche ob neue
	bne	salt		;startadresse n-salt
	lda	start		;ja startad. in
	sta	ldstar		;ldstar speichern
salt	ldd	ldstar		;berechne block-
	addd	ldlaen		;laenge
	std	ldende

	ldb	flag		;lade steuerbyte
	andb	#$02		;vergleiche ob
	cmpb	#$02		;verify oder load
	beq	ve
	lbsr	daten		;up-datenempfang
	bra	chek
ve	lbsr	verify		;up-verify

chek	bcc	right
	ldx	#tfpd
	lbsr	out
	lbra	stop
right	ldx	#tok2
	lbsr	out
	ldb	flag
	andb	#$03
	cmpb	#$00
	bne	stop
	lbsr	direct
stop	ldx	#tstop
	lbsr	out
	lbsr	in
	cmpa	#$5b
	lbeq	start1

aus	andcc	#$af		;freigabe firq-irq
	jmp	hks		;korrektes ende des
	nop			;ges. programms

***********************************************
* hp-load-ende - - - - - - - - - - - - - - - -*
***********************************************

* up daten - - - - - - - - - - - - - - - - - -

daten	ldb	#$ff		;empfangen und ein-
sycdar	cmpu	baud		;rasten mittels sync
	cmpu	baud		;siehe oben
	ldy	#$01
	ldy	#$01
	lbsr	rdbit
	cmpb	#$16
	bne	sycdar

sycd	cmpu	baud
	cmpu	baud
	lbsr	rdbyte
	cmpb	#$16
	beq	sycd

	cmpb	#$c5
	bne	dat1		;fehler
	stx	pruef

	ldx	ldstar		;lade startadresse
	bra	dat
datlop	cmpu	#$00		;warte 5
dat	lbsr	rdbyte		;empfange daten
	stb	,x+		;und speicher
	cmpx	ldende		;daten ende?
	bls	datlop		;nein datlop

	ldx	pruef		;pruefsumme
	cmpx	#$00		;siehe oben
	lbsr	rdbyte
	stx	pruef
	cmpb	pruef+1
	bne	dat1		;fehler
	cmpx	#$00
	nop
	lbsr	rdbyte
	stx	pruef
	cmpb	pruef
	bne	dat1		;fehler
	clc
	rts			;korrektes ende
dat1	sec
	rts			;ende bei fehler

* up verify - - - - - - - - - - - - - - - - - -

verify	ldb	#$ff		;siehe oben
sycver	cmpu	baud
	cmpu	baud
	ldy	#$01
	ldy	#$01
	lbsr	rdbit
	cmpb	#$16
	bne	sycver

sycv	cmpu	baud
	cmpu	baud
	lbsr	rdbyte
	cmpb	#$16
	beq	sycv

	cmpb	#$c5
	bne	ver1		;fehler
	stx	pruef

	ldx	ldstar		;lade startadresse
verlop	lbsr	rdbyte		;empfange daten
	cmpb	,x+		;und vergleichen
	bne	ver1		;ungleich! fehler
	cmpx	ldende		;alle verglichen ?
	bls	verlop		;nein weiter

	ldx	pruef		;pruefsumme
	lbsr	rdbyte		;siehe oben
	stx	pruef
	cmpb	pruef+1
	bne	ver1		;fehler
	cmpx	#$00
	nop
	lbsr	rdbyte
	stx	pruef
	cmpb	pruef
	bne	ver1		;fehler
	clc
	rts			;korrektes ende
ver1	sec
	rts			;ende bei fehler

*up rdbyte ,rdbit - - - - - - - - - - - - - - - - -

rdbyte	ldy	#$08		;bitzaehler =8
	bra	rdbit		;goto rdbit
read	ldu	#$01		;warte 41
	ldu	#$01
	lbsr	wa8
rdbit	lbsr	euro1		;- euro-wartezeit
	lda	baud		;7200 baud ?
	bmi	rd		;ja dann rd
	ldu	#$05		;warte 93
	ldu	#$05
	lbsr	wa8
	lbsr	euro1		;- euro-wartezeit
rd	lda	pb		;abtastung (ta)
	anda	#$80		;maskiere port und
	cmpa	byte		;vergleiche mit (t0)
	beq	rdeins		;wenn gleich= eins
	sta	byte
	clc			;carry=0
	rorb			;schiebe "b"
	bra	flanke		;goto flanke
rdeins	sta	byte
	ldu	pruef		;initialisiere
	leau	1,u		;pruefsumme
	stu	pruef
	sec			;carry=1
	rorb			;schiebe "b"
flanke	lda	pb		;lade port und
	anda	#$80		;maskiere
	cmpa	byte		;flankenaenderung ?
	beq	flanke		;nein goto flanke
	sta	byte		;speichere pb7
	leay	-1,y		;y=y-1
	bne	read		;8-bit empfangen ?
	rts

************************************************
*save supertape- - hp 21 zyklen - - - - - - - -*
************************************************

save	clr	baud		;3600 baud einstellen

	ldd	ende		;berechnung der
	subd	start		;datenblocklaenge
	std	laeng

	lda	flag
	anda	#$80
	sta	flag

*sync-zeicen fuer parameter - - - - - - - - -

	ldx	#$40		;zaehler fuer 64-syc
syncpa	ldb	#$16		;lade syc-zeichen
	lbsr	outch		;sende syc-zeichen
	nop
	leax	-1,x		;syc-zaehler -1
	bne	syncpa		;alle zeichen ausge.?

	ldb	#$2a		;lade startbyte para.
	bsr	outch		;sende startbyte

	stx	pruef		;pruefsumme=00

*sende parameterblock - - - - - - - - - - - -

	ldx	#name		;zaehler fur pa.block
parout	ldb	,x+		;lade parameter
	bsr	outch		;sende parameter
	cmpx	#flag		;parameter
	bls	parout		;ausgegeben ?

poutr	nop			;sende restliche
	ldb	1,x		;parameter
	bsr	outch		;low-byte
	cmpu	baud
	ldb	,x++
	bsr	outch		;high-byte
	cmpx	#res2		;pa-block
	bls	poutr		;ausgegeben ?

*sende pruefsumme des parameterblocks - - - -

	ldx	pruef		;rette pruefsumme
	ldb	pruef+1		;lade low-byte :pruef
	bsr	outch		;sende low-byte
	stx	pruef		;zurueck in pruef
	stx	pruef		;warte zeit
	ldb	pruef		;lade high-byte:pruef
	bsr 	outch		;sende high-byte
	nop
	lda	flag		;baudrate einstellen
	sta	baud		;in baud

*sende syc-zeichen des datenblocks - - - - - -

	ldx	#$40		;wie oben beschrieben
syncda	ldb	#$16
	lbsr	outch
	nop
	leax	-1,x
	bne	syncda

	ldb	#$c5		;lade startbyte d-block
	bsr	outch		;sende startbyte
	stx	pruef		;pruefsumme=00

*sende datenblock - - - - - - - - - - - - - -

	ldx	start		;zaehler fuer d-block
datout	ldb	,x+		;lade daten
	bsr	outch		;sende daten
	cmpx	ende
	bls	datout		;nein, weiter

* sende pruefsumme datenblock - - - - - - - -

	ldx	pruef		;wie oben beschrieben
	ldb	pruef+1
	bsr	outch
	stx	pruef
	stx	pruef
	ldb	pruef
	bsr	outch

	clr	pb
	lda	#$01
	sta	flag

	lbra	right

**********************************************
* ende  hp-save - - - - - - - - - - - - - - -*
**********************************************

* up's fuer save - - - - - - - - - - - - - - -

outch	ldy	#$08		;bit-zaehler
	lbsr	euro		;- euro-wartezeit
	lda	baud		;lade baudrate
	bmi	l3		;7200 ?
	ldu	#$03		;warte 59 zyklen
	ldu	#$03
	lbsr	wa0
	lbsr	euro		;- euro-wartezeit
	bra	l3		;springe zum label l3
l1	lda	baud		;lade baudrate
	bmi	l2		;7200 ?
	ldu	#$03		;warte 62 zyklen
	lbsr	wa6
	lbsr	euro		;- euro-wartezeit
l2	ldu	#$01		;warte 30 zyklen
	lbsr	wa0
	lbsr	euro		;- euro-wartezeit
l3	lsrb			;schiebe "b" rechts
	bcs	eins		;carry-bit=1 ?
	lda	pb		;lade port
	eora	#$01		;invertiere pb0
	sta	pb		;zurueck ins port
	lda	baud		;lade baudrate
	bmi	l4		;7200 ?
	ldu	#$03		;warte 62 zyklen
	lbsr	wa6
	lbsr	euro		;- euro-wartezeit
l4	ldu	#$02		;warte 43 zyklen
	lbsr	wa0
	lbsr	euro		;- euro-wartezeit
	lda	pb		;lade port
	eora	#$01		;invertiere pb0
	sta	pb		;zurueck ins port
	leay	-1,y		;bitzaehler -1
	bne	l1		;8-bit ausgegeben
	rts

eins	ldu	pruef		;initialisiere
	leau	1,u		;pruefsumme
	stu	pruef
	lda	baud		;lade baudrate
	bmi	l5		;7200 ?
	ldu	#$03		;warte 62 zyklen
	lbsr	wa6
	lbsr	euro		;- euro-wartezeit
l5	ldu	#$01		;warte 40 zyklen
	lbsr	wa10
	lbsr	euro		;- euro-wartezeit
	lda	pb		;lade port
	eora	#$01		;invertiere pb0
	sta	pb		;zurueck ins port
	leay	-1,y		;bitzaehler -1
	bne	l1		;8-bit ausgegeben
	rts

* warteschleifen - - - - - - - - - - - - - - -

wa10	nop			;lbsr = 8-zyklen
wa8	nop			;nop  = 2-zyklen
wa6	nop
	nop
	nop
wa0	leau	-1,u		;     = 5-zyklen
	cmpu	#$00		;     = 5-zyklen
	bne	wa0		;     = 3-zyklen
	rts			;     = 5-zyklen

*eurocom warteschleife  - - - - - - - - - - -
*euro=31-zyklen  euro1=46-zyklen

euro1	cmpu	#$00		;lbsr = 8-zyklen
	cmpu	#$00		;cmpu = 5-zyklen
	cmpu	#$00
euro	cmpu	baud		;     = 7-zyklen
	cmpu	baud
	ldu	#$00		;     = 3-zyklen
	rts			;     = 5-zyklen


