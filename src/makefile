VPATH = src
rmmbr: rmmbr.hs
	ghc rmmbr.hs -i/Users/spencergordon/Documents/experiments/haskell/utilities
	rm rmmbr.hi rmmbr.o

.PHONY: release
release: 
	ghc -outputdir ../bin rmmbr.hs -i/Users/spencergordon/Documents/experiments/haskell/utilities
	rm ../bin/Main.hi ../bin/Main.o