TARGETS=main
BYTES=$(TARGETS:=.byte)
NATIVE=$(TARGETS:=.native)

OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags '-w +A-4'
.PHONY:native byte clean tests clean-tests

all:native byte
	cp _build/$(NATIVE) petitghc
native:
	$(OCAMLBUILD) $(NATIVE)

byte:
	$(OCAMLBUILD) $(BYTES)

clean:
	rm -f petitghc ;
	$(OCAMLBUILD) -clean

tests:
	./tests.sh syntax ;
	./tests.sh typing ;
	./tests.sh execution

clean-tests:
	rm -f tests/exec*/*.s ;
	rm -f tests/exec*/*.out0 ;
	rm -f tests/exec*/*.out1
