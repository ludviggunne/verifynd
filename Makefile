PREFIX		:=	.
GHC			:=	ghc
GHCFLAGS	:= -isrc
SOURCES		:= $(wildcard src/*.hs)

verifynd: $(SOURCES)
	$(GHC) $(GHCFLAGS) --make -o $(@) src/Main.hs

trace: GHCFLAGS	+= -prof -fprof-auto -rtsopts
trace: verifynd
	./$(<) +RTC -xc

clean:
	rm -rf verifynd src/*.hi src/*.o

install:
	install -Dm755 verifynd $(PREFIX)/bin/verifynd

.PHONY: clean install trace test
