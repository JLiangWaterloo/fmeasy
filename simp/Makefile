MROOT := $(shell pwd)/minisat
export MROOT

all: Simplify minisat_simp

Simplify: Sat.hs Simplify.hs
	ghc -O2 Simplify.hs

minisat_simp:
	@echo $(MROOT)
	make -C minisat/simp
	cp minisat/simp/minisat minisat_simp

clean:
	rm *.hi *.o Simplify minisat_simp
	make -C minisat/simp clean
