
OPT  ?= -O1
RTS_OPTS ?=
MINISAT = ./minisat/current-base
INST    = ./instantiate
OBJS    = $(MINISAT)/Solver.or $(MINISAT)/Prop.or $(INST)/MiniSatWrapper.or $(INST)/MiniSatInstantiateClause.or
HFLAGS  = -optl -static -lstdc++ -fglasgow-exts -optl -fexceptions $(OPT) -I$(INST) -I$(MINISAT) $(OBJS)
GHC     = ghc

# OPTIONS += -prof -auto-all
# RTS_OPTS += -p

TARGET = flexcomp

main: flexcomp

flexcomp: *.hs SAT.o
	$(GHC) $(HFLAGS) --make Main.hs -o $@

run: main
	./Main +RTS $(RTS_OPTS) -RTS

DOCFILES = Types.hs MicroOp.hs GPP.hs IDGen.hs MipsCFG.hs CFG.hs Mips.hs
docs: *.hs
	haddock $(DOCFILES) --html --o=./docs

.PHONY: clean minisat instantiate
clean:
	rm -f *.o *.hi $(TARGET)

test-sat: TestSAT.hs SAT.o
	$(GHC) $(HFLAGS) -main-is Main.main --make TestSAT -o test-sat 

test-sched: Schedule3.hs SAT.o
	$(GHC) $(HFLAGS) -main-is Schedule3.main --make Schedule3 -o test-sched

SAT.o: SAT.hs minisat instantiate
	$(GHC) -fglasgow-exts --make SAT.hs

minisat: 
	make Solver.or -C $(MINISAT)
	make Prop.or   -C $(MINISAT)

instantiate:
	make MiniSatWrapper.or           -C $(INST)
	make MiniSatInstantiateClause.or -C $(INST)

FFT_FILE = /Users/nominolo/code/flexsoc/library/benchmarks/EEMBC/work/fft00_data1/fft00.mips
compile-fft: flexcomp
	./flexcomp 

AUTOCOR_FILE = /Users/nominolo/code/flexsoc/library/benchmarks/EEMBC/work/autcor00data_1/autcor00.mips
compile-autocor: flexcomp
	./flexcomp $(AUTOCOR_FILE)

sim-autocor: compile-autocor
	../simulator/runmips $(AUTOCOR_FILE).flexcompiled