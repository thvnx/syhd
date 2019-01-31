OCAMLC=ocamlc
OCAMLOPT=ocamlfind ocamlopt

EXE=syhd

INCLUDES=
PACKAGES=-package str -linkpkg 

OCAMLFLAGS= $(INCLUDES) $(LIBS)

SRCS = config.cmx commandline.cmx syscalls.cmx gnuplot.cmx datamanager.cmx plot.cmx exploration.cmx synth0.cmx synthd.cmx

COHD = $(SRCS)
COHDO = unix.cmxa $(SRCS)


all: $(COHD)
	$(OCAMLOPT) $(PACKAGES) $(OCAMLFLAGS) -o $(EXE) $(COHDO)

# Common rules
.SUFFIXES: .ml .mli .cmx

.mli.ml:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(PACKAGES) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	rm -f $(EXE)
	rm -f *.cm[iox]
	rm -f *.o
	rm -f *~
	rm -f *~
