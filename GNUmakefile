
FC = gfortran
FCOPTS =

LN = $(FC)
LNOPTS =

EXLNOPTS = -L/usr/local/stow/lapack95/lib
EXFCOPTS = -I/usr/local/stow/lapack95/include/lapack95

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
	$(LN) $(LNOPTS) -o $@ $^

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

.PHONY: clean realclean

clean:
	rm -f *.o *.mod

realclean: clean
	rm -f schroedinger

