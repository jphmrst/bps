
all: JTMS-hs.ps
	#

JTMS-hs.ps: src/main/haskell/lib/Data/TMS/JTMS.hs
	a2ps -2r -o $@ $<
