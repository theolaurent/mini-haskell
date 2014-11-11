
TARGETS=main
BYTES=$(TARGETS:=.byte)

OCAMLBUILD=ocamlbuild

.PHONY: byte clean

byte:
	$(OCAMLBUILD) $(BYTES)

clean:
	$(OCAMLBUILD) -clean
