
TARGETS=main
BYTES=$(TARGETS:=.byte)

OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags '-w +A-4' 
.PHONY: byte clean

byte:
	$(OCAMLBUILD) $(BYTES)

clean:
	$(OCAMLBUILD) -clean
