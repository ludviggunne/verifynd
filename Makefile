PREFIX		:=	.
GHC			:=	ghc
GHCFLAGS	:=
SOURCES		:= $(wildcard *.hs)

nde: $(SOURCES)
	$(GHC) --make -o $(@) Main.hs

clean:
	rm -rf nde *.hi *.o

install:
	install -Dm755 nde $(PREFIX)/bin/nde

.PHONY: clean install
