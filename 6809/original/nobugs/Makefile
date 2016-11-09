
FILE=st09

ASM=../a09

all: $(FILE)

# ------------------------------------

$(FILE): $(FILE).asm
	$(ASM) -l $@.lst  $<

# ------------------------------------

cleanall: clean

clean:
	rm -f core *.BAK $(FILE)


