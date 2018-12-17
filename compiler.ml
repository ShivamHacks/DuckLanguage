open DuckTypes
open Lexer
open Parser
open Interpreter
open Printf
open Scanf

let () =
	if (Array.length Sys.argv) > 1 then
		(* Read the file *)
		let lines = ref "" in
		let read_file filename =
			let ch = open_in filename in
			try while true do lines := !lines ^ (input_line ch) done
			with End_of_file -> close_in ch
		in read_file Sys.argv.(1);
		(* Process the file *)
		let tokens = tokenize !lines in
		let tokens_string = Utils.tokens_to_string tokens in
		printf "Tokens: %s\n" tokens_string;
		let parsed = parse_Expr tokens in 
		let parsed_string = Utils.expr_to_string parsed in
		printf "Parsed: %s\n" parsed_string
	else printf "No file provided!\n"
;;

