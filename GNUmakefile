
FC = gfortran
FCOPTS =

LN = $(FC)
LNOPTS =

include config.mk
BIBOPTS  = -llapack95 -llapack

NAME = schroedinger

FILFO = formatting.f90
FILIO = io.f90
FILEX = externs.f90
FILCA = calculations.f90
FILSC = schroedinger.f90

OBJFO = formatting.o
OBJIO = io.o
OBJEX = externs.o
OBJCA = calculations.o
OBJSC = schroedinger.o
ALOBJS = $(OBJSC) $(OBJFO) $(OBJIO) $(OBJEX) $(OBJCA)


$(NAME): $(ALOBJS)
	$(LN) $(LNOPTS) -o $@ $(EXLNOPTS) $^ $(BIBOPTS)

$(OBJFO): $(FILFO)
	$(FC) $(FCOPTS) -c $<

$(OBJIO): $(FILIO) $(OBJFO)
	$(FC) $(FCOPTS) -c $<

$(OBJEX): $(FILEX) $(OBJFO)
	$(FC) $(FCOPTS) -c $(EXFCOPTS) $<

$(OBJCA): $(FILCA) $(OBJFO) $(OBJEX)
	$(FC) $(FCOPTS) -c $<

$(OBJSC): $(FILSC) $(OBJFO) $(OBJIO) $(OBJCA)
	$(FC) $(FCOPTS) -c $<

.PHONY: clean realclean test

clean:
	rm -f *.o *.mod

realclean: clean
	rm -f schroedinger

test:
	cp test/schrodinger1.inp ./
	mv schrodinger1.inp schrodinger.inp
	./schroedinger

	cp discrpot.dat test/
	mv test/discrpot.dat test/discrpot1.dat
	cp energies.dat test/
	mv test/energies.dat test/energies1.dat
	cp wfuncs.dat test/
	mv test/wfuncs.dat test/wfuncs1.dat
	cp ewfuncs.dat test/
	mv test/ewfuncs.dat test/ewfuncs1.dat


	cp test/schrodinger2.inp ./
	mv schrodinger2.inp schrodinger.inp
	./schroedinger

	cp discrpot.dat test/
	mv test/discrpot.dat test/discrpot2.dat
	cp energies.dat test/
	mv test/energies.dat test/energies2.dat
	cp wfuncs.dat test/
	mv test/wfuncs.dat test/wfuncs2.dat
	cp ewfuncs.dat test/
	mv test/ewfuncs.dat test/ewfuncs2.dat


	cp test/schrodinger3.inp ./
	mv schrodinger3.inp schrodinger.inp
	./schroedinger

	cp discrpot.dat test/
	mv test/discrpot.dat test/discrpot3.dat
	cp energies.dat test/
	mv test/energies.dat test/energies3.dat
	cp wfuncs.dat test/
	mv test/wfuncs.dat test/wfuncs3.dat
	cp ewfuncs.dat test/
	mv test/ewfuncs.dat test/ewfuncs3.dat

	$(FC) $(FCOPTS) -c autotest.f90

	$(LN) $(LNOPTS) -o autotest autotest.o formatting.o io.o

	./autotest

	rm test/*.dat
	rm autotest.o
	rm autotest
