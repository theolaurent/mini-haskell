TARGETS=main
BYTES=$(TARGETS:=.byte)
NATIVE=$(TARGETS:=.native)

OCAMLBUILD=ocamlbuild -use-ocamlfind -cflags '-w +A-4' 
.PHONY:native byte clean

all:native byte
	cp _build/$(NATIVE) petitghc
native:
	$(OCAMLBUILD) $(NATIVE)

byte:
	$(OCAMLBUILD) $(BYTES)

clean:
	rm petitghc ; \
	$(OCAMLBUILD) -clean
