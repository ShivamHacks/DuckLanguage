compiler: duckTypes.cmx lexer.cmx parser.cmx interpreter.cmx utils.cmx
	ocamlopt -o compiler str.cmxa unix.cmxa duckTypes.cmx lexer.cmx parser.cmx interpreter.cmx utils.cmx compiler.ml

lexer.cmx: lexer.ml duckTypes.cmx
	ocamlopt -o lexer str.cmxa duckTypes.cmx lexer.ml

parser.cmx: parser.ml duckTypes.cmx
	ocamlopt -o parser str.cmxa duckTypes.cmx parser.ml

interpreter.cmx: interpreter.ml duckTypes.cmx
	ocamlopt -o interpreter str.cmxa duckTypes.cmx interpreter.ml

utils.cmx: utils.ml duckTypes.cmx
	ocamlopt -o utils str.cmxa duckTypes.cmx utils.ml

duckTypes.cmx: duckTypes.ml
	ocamlopt -o duckTypes str.cmxa duckTypes.ml

clean:
	rm -f *.o *.cmx *.cmi compiler duckTypes utils lexer parser interpreter
	rm -f test_cmdargs
