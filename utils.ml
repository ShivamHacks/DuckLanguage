(* contains to_string methods *)

open DuckTypes

let token_to_string token = match token with
	(* Empty token *)
	| Tok_EMPTY -> ""
	(* Instances *)
	| Tok_ID(str) -> "Tok_ID(" ^ str ^ ")"
	(* Types *)
  	| Tok_Int(i) -> "Tok_Int(" ^ (string_of_int i) ^ ")" 
  	| Tok_String(s) -> "Tok_String(" ^ s ^ ")"
  	(* Binary Ops *)
	| Tok_Add -> "Tok_Add"
	| Tok_Sub -> "Tok_Sub"
	| Tok_Mult -> "Tok_Mult"
	| Tok_Div -> "Tok_Div"
	(* Syntax *)
	| Tok_LParen -> "Tok_LParen"
	| Tok_RParen -> "Tok_RParen"
	| Tok_LBracket -> "Tok_LBracket"
	| Tok_RBracket -> "Tok_RBracket"
  	| Tok_LBrace -> "Tok_LBrace"
  	| Tok_RBrace -> "Tok_RBrace"
  	| Tok_Assign -> "Tok_Assign"
  	| Tok_Comma -> "Tok_Comma"
  	| Tok_Semi -> "Tok_Semi"
  	(* Functions *)
  	| Tok_Print -> "Tok_Print"
	(* Misc *)
	| EOF -> "EOF"
;;

let tokens_to_string tok_list = 
	let rec get_toks_str lst = (match lst with
		[] -> ""
		| token :: [] -> token_to_string token
		| token :: next_toks -> (token_to_string token) ^ "," ^ (get_toks_str next_toks))
	in get_toks_str tok_list
;;

let rec expr_to_string expr = match expr with
	Array(elements) ->
		let rec print_els elements = (
			match elements with
				[] -> ""
				| h :: [] -> expr_to_string h
				| h :: t -> (expr_to_string h) ^ "," ^ (print_els t)
		) in "[" ^ (print_els elements) ^ "]"
	| Int(i) -> string_of_int i
	| String(s) -> s
	| _ -> ""
;;

