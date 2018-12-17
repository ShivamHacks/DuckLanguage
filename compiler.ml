open DuckTypes
open Lexer
open Parser
open Interpreter
open Printf

(* just for testing *)

(* Print tokens in input string *)

let () =
	if (Array.length Sys.argv) > 1 then
		let tokens = tokenize Sys.argv.(1) in
		let tokens_string = Utils.tokens_to_string tokens in
		printf "Tokens: %s\n" tokens_string
	else printf "No input string provided!\n"
;;

(*
let () =
	if (Array.length Sys.argv) > 1 then
		printf "Given input: %s\n" Sys.argv.(1);
		let tokens = tokenize Sys.argv.(1) in
		let tokens_string = tokens_to_string tokens in
		(printf "%s\n" tokens_string)
	else (printf "No input string provided!\n")
;;
*)