SOURCES = \
	utils.ml \
	ast.ml \
	parser.mly \
	lexer.mll \
	main.ml

RESULT = petitghc

OCAMLYACC = menhir

-include OCamlMakefile
