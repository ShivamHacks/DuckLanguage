open DuckTypes

(* Converts list of tokens into abstract syntax tree for execution by interpreter *)

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an expected token.
 * Handles error cases and returns the tail of the list *)

let lookahead toks = match toks with
	[] -> raise (InvalidInputException "no tokens")
	| h :: t -> h
;;

let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException "no token matched")
  | h :: t -> if h = tok then t else raise (InvalidInputException "no token matched")
;;