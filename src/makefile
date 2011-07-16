VPATH = src
rmmbr: rmmbr.hs
	ghc rmmbr.hs
	rm rmmbr.hi rmmbr.o

.PHONY: release
release: 
	ghc -outputdir ../bin rmmbr.hs
	rm rmmbr.hi rmmbr.o