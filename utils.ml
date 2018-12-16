(* contains to_string methods *)

open DuckTypes

let token_to_string token = match token with
	(* Empty token *)
	Tok_EMPTY -> ""
	(* Instances *)
	| Tok_ID(str) -> "Tok_ID(" ^ str ^ ")"
	(* Types *)
	| Tok_Float_Type -> "Tok_Float_Type"
  	| Tok_Bool_Type -> "Tok_Bool_Type"
  	| Tok_String_Type -> "Tok_String_Type"
  	| Tok_Float(f) -> "Tok_Float(" ^ (string_of_float f) ^ ")" 
  	| Tok_Bool(b) ->  "Tok_Bool(" ^ (string_of_bool b) ^ ")" 
  	| Tok_String(s) -> "Tok_String(" ^ s ^ ")"
  	(* Binary Ops *)
	| Tok_Add -> "Tok_Add"
	| Tok_Sub -> "Tok_Sub"
	| Tok_Mult -> "Tok_Mult"
	| Tok_Div -> "Tok_Div"
  	| Tok_Greater -> "Tok_Greater"
	| Tok_Less -> "Tok_Less"
	| Tok_GreaterEqual -> "Tok_GreaterEqual"
  	| Tok_LessEqual -> "Tok_LessEqual"
  	| Tok_Equal -> "Tok_Equal"
  	| Tok_NotEqual -> "Tok_NotEqual"
  	| Tok_Or -> "Tok_Or"
  	| Tok_And -> "Tok_And"
  	(* Unary Ops *)
	| Tok_Not -> "Tok_Not"
	(* Control Flow *)
	| Tok_If -> "Tok_If"
	| Tok_Else -> "Tok_Else"
	| Tok_While -> "Tok_While"
	(* Syntax *)
	| Tok_LParen -> "Tok_LParen"
	| Tok_RParen -> "Tok_RParen"
  	| Tok_RBrace -> "Tok_LBrace"
  	| Tok_LBrace -> "Tok_LBrace"
  	| Tok_Semi -> "Tok_Semi"
  	| Tok_Assign -> "Tok_Assign"
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
	in "[" ^ (get_toks_str tok_list) ^ "]"
;;