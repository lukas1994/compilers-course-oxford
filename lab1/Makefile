# lab1/Makefile

## Add your own test cases to this list
TEST = gcd repeat loop case

all: ppc

PPC = print.cmo source.cmo keiko.cmo lexer.cmo tree.cmo parser.cmo \
	peepopt.cmo kgen.cmo main.cmo
ppc: $(PPC)
	ocamlc -o ppc $(PPC)

parser.mli parser.ml: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

tree.cmo: MLFLAGS += -w u

KEIKO = ../keiko

test : force
	$(MAKE) $(TEST:%=test-%)

test-%: force
	@echo "*** Test $*.p"
	./ppc $*.p >a.k
	$(KEIKO)/pplink -custom -nostdlib $(KEIKO)/lib.k a.k -o a.x >/dev/null
	$(KEIKO)/ppx ./a.x >a.test
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p $*.p | diff - a.test
	@echo "*** Passed"; echo

realclean: clean

clean: force
	rm -f ppc *.cma *.cmo *.cmi
	rm -f parser.mli parser.ml lexer.ml parser.output
	rm -f a.k a.out a.x a.test

ML = keiko.ml keiko.mli kgen.ml kgen.mli lexer.mli \
	lexer.ml main.ml parser.mli parser.ml tree.ml tree.mli \
	print.mli print.ml source.mli source.ml peepopt.mli peepopt.ml

depend : $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

%.cmi : %.mli
	ocamlc $(MLFLAGS) -c $<

%.cmo : %.ml
	ocamlc $(MLFLAGS) -c $<

force:

###

keiko.cmo : tree.cmi source.cmi print.cmi keiko.cmi
keiko.cmx : tree.cmx source.cmx print.cmx keiko.cmi
keiko.cmi : print.cmi
kgen.cmo : tree.cmi keiko.cmi kgen.cmi
kgen.cmx : tree.cmx keiko.cmx kgen.cmi
kgen.cmi : tree.cmi
lexer.cmi : tree.cmi parser.cmi
lexer.cmo : tree.cmi source.cmi parser.cmi keiko.cmi lexer.cmi
lexer.cmx : tree.cmx source.cmx parser.cmx keiko.cmx lexer.cmi
main.cmo : tree.cmi source.cmi print.cmi parser.cmi lexer.cmi kgen.cmi
main.cmx : tree.cmx source.cmx print.cmx parser.cmx lexer.cmx kgen.cmx
parser.cmi : tree.cmi keiko.cmi
parser.cmo : tree.cmi lexer.cmi keiko.cmi parser.cmi
parser.cmx : tree.cmx lexer.cmx keiko.cmx parser.cmi
tree.cmo : print.cmi keiko.cmi tree.cmi
tree.cmx : print.cmx keiko.cmx tree.cmi
tree.cmi : keiko.cmi
print.cmi :
print.cmo : print.cmi
print.cmx : print.cmi
source.cmi : print.cmi
source.cmo : print.cmi source.cmi
source.cmx : print.cmx source.cmi
peepopt.cmi : keiko.cmi
peepopt.cmo : print.cmi keiko.cmi peepopt.cmi
peepopt.cmx : print.cmx keiko.cmx peepopt.cmi
