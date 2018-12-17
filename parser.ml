open DuckTypes
open Printf

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

let rec parse_arrayExpr toks =
	if lookahead toks = Tok_LBracket then
		(* This is the start of an array *)
		(
			let toks = match_token toks Tok_LBracket in
			(* Empty Array *)
			if lookahead toks = Tok_RBracket then
				let toks = match_token toks Tok_RBracket in
				(toks, Array [])
			(* Non-Empty Array *)
			else
				let rec get_elements toks elements =
					let (toks, element) = parse_primitiveExpr toks in
					(* There are more elements *)
					if lookahead toks = Tok_Comma then
						let toks = match_token toks Tok_Comma in
						get_elements toks (elements @ [element])
					(* No more elements *)
					else if lookahead toks = Tok_RBracket then
						let toks = match_token toks Tok_RBracket in
						(toks, Array (elements @ [element]))
					else raise (InvalidInputException "Array")
				in get_elements toks []
		)
	else parse_primitiveExpr toks

and parse_primitiveExpr toks =
	let next = lookahead toks in match next with
		Tok_Int(i) -> let toks = match_token toks next in (toks, Int i)
		| Tok_String(s) ->  let toks = match_token toks next in (toks, String s)
		| _ -> raise (InvalidInputException "Primitive")
;;

