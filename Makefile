PREFIX		:=	.
GHC			:=	ghc
GHCFLAGS	:=
SOURCES		:= $(wildcard *.hs)

verifynd: $(SOURCES)
	$(GHC) $(GHCFLAGS) --make -o $(@) Main.hs

trace: GHCFLAGS	+= -prof -fprof-auto -rtsopts
trace: verifynd
	./$(<) +RTC -xc

clean:
	rm -rf verifynd *.hi *.o

install:
	install -Dm755 verifynd $(PREFIX)/bin/verifynd

.PHONY: clean install trace test
