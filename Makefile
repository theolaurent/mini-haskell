TARGETS=main
BYTES=$(TARGETS:=.byte)
NATIVE=$(TARGETS:=.native)

OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags '-w +A-4' 
.PHONY:native byte clean

all:native byte

native:
	$(OCAMLBUILD) $(NATIVE)

byte:
	$(OCAMLBUILD) $(BYTES)

clean:
	$(OCAMLBUILD) -clean
