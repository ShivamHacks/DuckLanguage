compiler: lexer.cmx parser.cmx interpreter.cmx
	ocamlopt -o compiler str.cmxa unix.cmxa duckTypes.cmx lexer.cmx parser.cmx interpreter.cmx compiler.ml

lexer.cmx: lexer.ml duckTypes.cmx
	ocamlopt -o lexer str.cmxa duckTypes.cmx lexer.ml

parser.cmx: parser.ml duckTypes.cmx
	ocamlopt -o parser str.cmxa duckTypes.cmx parser.ml

interpreter.cmx: interpreter.ml duckTypes.cmx
	ocamlopt -o interpreter str.cmxa duckTypes.cmx interpreter.ml

duckTypes.cmx: duckTypes.ml
	ocamlopt -o duckTypes str.cmxa duckTypes.ml

clean:
	rm -f *.cmx *.cmi compiler duckTypes lexer parser interpreter
