open DuckTypes
open Printf

(* Converts list of tokens into abstract syntax tree for execution by interpreter *)

type stmt_result = token list * stmt
type expr_result = token list * expr

(* Provided helper function - takes a token list and an expected token.
 * Handles error cases and returns the tail of the list *)

let lookahead toks = match !toks with
	[] -> raise (InvalidInputException "no tokens")
	| h :: t -> h
;;

let match_token toks token =
	match !toks with
		| [] -> raise (InvalidInputException "no token matched")
		| h :: t ->
			if h = token then toks := t
			else raise (InvalidInputException "invalid expression")
;;

let rec parse_Expr toks =
	let toks = ref toks in
	parse_arrayExpr toks

and parse_arrayExpr toks =
	if lookahead toks = Tok_LBracket then
		(* This is the start of an array *)
		(
			match_token toks Tok_LBracket;
			(* Empty Array *)
			if lookahead toks = Tok_RBracket then
				(
					match_token toks Tok_RBracket;
					Array []
				)
			(* Non-Empty Array *)
			else
				(
					let rec get_elements elements =
						let element = parse_primitiveExpr toks in
						(* There are more elements *)
						if lookahead toks = Tok_Comma then
							(
								match_token toks Tok_Comma;
								get_elements (elements @ [element])
							)
						(* No more elements *)
						else if lookahead toks = Tok_RBracket then
							(
								match_token toks Tok_RBracket;
								Array (elements @ [element])
							)
						else raise (InvalidInputException "Array")
					in get_elements []
				)
		)
	else parse_primitiveExpr toks

and parse_primitiveExpr toks =
	let next = lookahead toks in match next with
		Tok_Int(i) -> match_token toks next; Int i
		| Tok_String(s) -> match_token toks next; String s
		| _ -> raise (InvalidInputException "Primitive")
;;

