##########################################################################
#  This file is part of dx, a tool to derive C from monadic Gallina.     #
#                                                                        #
#  Copyright (C) 2021 Université de Lille & CNRS                         #
#                                                                        #
#  This program is free software; you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation; either version 2 of the License, or     #
#  (at your option) any later version.                                   #
#                                                                        #
#  This program is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU General Public License for more details.                          #
##########################################################################

# This Makefile runs the same tests as ../Makefile but it builds the
# tests against an _installed_ dx instead of the dx in the ..
# directory.
# This Makefile can thus be used:
# - to test that dx was properly built and installed,
# - as a template for other projects using dx.


# If Makefile.config exists (it could contain CPRINTERDIR and
# COMPCERTINI variables, in particular), include it
-include Makefile.config

ifndef CPRINTERDIR
$(error $$(CPRINTERDIR) should be set to the directory in which the OCaml code needed to print the derived C code is installed)
endif


COQC := coqc
COQDEP := coqdep
OCAMLOPT := ocamlopt

# Disable warnings on notations (that are coming from the standard
# library)
COQDIROPTS := -R . dx.tests
COQDEPOPTS := $(COQDIROPTS)
COQCOPTS := $(COQDIROPTS) -w all,-notation
COQEXTROPTS := $(COQDIROPTS) -w all,-extraction

OCAMLINCS := -args $(CPRINTERDIR)/cprinter-inc-args

ifndef COMPCERTINI
$(info $$(COMPCERTINI) is not set: defaulting to $$(CPRINTERDIR)/compcertcprinter/compcert.ini)
COMPCERTINI := $(CPRINTERDIR)/compcertcprinter/compcert.ini
endif

all: generated.c

.depend.coq: $(wildcard *.v)
	$(COQDEP) $(COQDEPOPTS) $^ > $@

-include .depend.coq

ifneq (,$(findstring grouped-target,$(.FEATURES)))

ExtrMain.vo TestMain.ml TestMain.mli &: ExtrMain.v TestMain.vo
	$(COQC) $(COQEXTROPTS) ExtrMain.v

generated.h generated.c &: main compcert.ini
	./main
	diff -u expected.c generated.c
	diff -u expected.h generated.h

else

ExtrMain.vo TestMain.ml TestMain.mli: ExtrMain.v TestMain.vo
	$(COQC) $(COQEXTROPTS) ExtrMain.v

# Forget about generated.h to avoid repeating the recipe
generated.c: main compcert.ini
	./main
	diff -u expected.c generated.c
	diff -u expected.h generated.h

endif

main: TestMain.cmx TestMain.o
	$(OCAMLOPT) $(OCAMLINCS) -args $(CPRINTERDIR)/cprinter-link-args $< -o $@

compcert.ini:
	ln -sf $(COMPCERTINI) $@


## Implicit rules

%.vo: %.v
	$(COQC) $(COQCOPTS) $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLINCS) $<

ifneq (,$(findstring grouped-target,$(.FEATURES)))
%.cmx %.o &: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLINCS) -c $<
else
%.cmx %.o: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLINCS) -c $<
endif

clean:
	rm -f *.glob *.vo *.vok *.vos .*.aux
	rm -f *.ml *.mli
	rm -f *.cmi *.cmx *.o
	rm -f compcert.ini main generated.c generated.h
	rm -f .depend.coq

.PHONY: all clean
