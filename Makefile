# All program source code
SRCFILES = search.ml\


# All targets for the program
LIBTARGETS = $(SRCFILES:.ml=.cmo)
OPTTARGETS = $(SRCFILES:.ml=.cmx)
MAINTARGETS= main.cmo
ALLTARGETS = $(LIBTARGETS) $(TESTTARGETS) $(MAINTARGETS)
LIBRARY=paclib.cma

#Compiler flags
COMPFLAGS = str.cma unix.cma

# Executable to be created
EXECUTABLE=ac

#OCAML PROGRAMS
SW_OCAMLDIR=/home/dmp4866/sw/bin
SW_OCAMLLIB=/home/dmp4866/sw/lib/ocaml
HOME_OCAMLDIR=/usr/bin
HOME_OCAMLLIB=/usr/lib/ocaml
MAC_OCAMLDIR=/opt/local/bin
MAC_OCAMLLIB=/opt/local/lib/ocaml

# OWLNET
#OCAMLDIR=$(OWL_OCAMLDIR)

# SW
#OCAMLLIB=$(SW_OCAMLLIB)
#OCAMLDIR=$(SW_OCAMLDIR)

# HOME
OCAMLLIB=$(HOME_OCAMLLIB)
OCAMLDIR=$(HOME_OCAMLDIR)

# MAC
#OCAMLDIR=$(MAC_OCAMLDIR)
#OCAMLLIB=$(MAC_OCAMLLIB)

#============================================================
#SHOULD NOT NEED TO CHANGE THESE ONCE SETUP CORRECTLY ABOVE
#============================================================
OCAMLFIND=$(OCAMLDIR)/ocamlfind
OCAML=$(OCAMLDIR)/ocaml
OCAMLOPT=$(OCAMLDIR)/ocamlopt
OCAMLDEBUG=$(OCAMLDIR)/ocamldebug
OCAMLC=$(OCAMLDIR)/ocamlc
CAMLP4=$(OCAMLDIR)/camlp4o
RLWRAP=rlwrap
PACKAGES=-package str -package unix -package sqlite3 -linkpkg

# (Default) Option to list targets
default: $(EXECUTABLE)

# Option to remove automatically generated files
# This target is required in all makefiles
clean:
	@ rm -f $(ALLTARGETS)
	@ rm -f $(ALLTARGETS:.cmo=.cmi)
	@ rm -f $(ALLTARGETS:.cmo=.cmxa)
	@ rm -f $(ALLTARGETS:.cmo=.cmx)
	@ rm -f $(ALLTARGETS:.cmo=.o)
	@ rm -f $(LIBRARY)
	@ rm -f $(EXECUTABLE)
	@ rm -f $(EXECUTABLE)-opt
	@ rm -f ttop
	@ echo " -- make clean (Done)"
   
# Option to load all targets into interactive loop
# This target is required in all makefiles
#toploop: src/library.cma
#	$(RLWRAP) $(OCAML) -I src/ src/library.cma

top: $(LIBTARGETS)
	@ $(OCAMLFIND) ocamlmktop -o ttop $(PACKAGES) $(LIBTARGETS)
	$(RLWRAP) ttop

debug: $(EXECUTABLE)
	$(RLWRAP) $(OCAMLDEBUG) -I src/ $(EXECUTABLE)


#$(EXECUTABLE): $(LIBTARGETS) src/main.ml
#	@ $(OCAMLC) $(COMPFLAGS) $(LIBTARGETS) -o $(EXECUTABLE) src/main.ml 
#	@ echo " -- make $(EXECUTABLE) (Done)"

$(EXECUTABLE): $(LIBTARGETS) $(PACWAR_O) main.cmo
	@ $(OCAMLFIND) ocamlc  $(LIBTARGETS) \
    $(PACKAGES) \
    -o $(EXECUTABLE) main.cmo
	@ echo " -- make $(EXECUTABLE) (Done)"

opt: $(OPTTARGETS) main.cmx
	@$(OCAMLFIND) ocamlopt -o $(EXECUTABLE)-opt \
   $(PACKAGES) \
   $(OPTTARGETS) main.cmx
	@ echo " -- make $(EXECUTABLE)-opt (Done)"

# Option to use ocamldep to dependecies automatically
depend: $(SRCFILES) 
	@ ocamldep $(ALLTARGETS:.cmo=.ml) > deps
	@ echo " -- make depend (Done)"

# Recognize the following suffixes in file names
.SUFFIXES: .ml .mli .cmo .cmi .cmx

# How to turn a ".ml" file into a ".cmo" file
#.ml.cmx: $(<:.ml=.cmo) 
.ml.cmx: 
	@ $(OCAMLFIND) ocamlopt $(PACKAGES) -c $<
	@ echo " -- make $< (Done)"
.ml.cmo: 
	@ $(OCAMLFIND) ocamlc $(PACKAGES) -c $<
	@ echo " -- make $< (Done)"
.mli.cmi:
	@ $(OCAMLC) $(COMPFLAGS) -c $<
	@ echo " -- make $< (Done)"

lib: $(LIBRARY)

$(LIBRARY): $(LIBTARGETS)
	@ $(OCAMLC) $(COMPFLAGS) -a $(LIBTARGETS) -o $(LIBRARY) 
	@ echo " -- make $(LIBRARY) (Done)"

-include deps 

