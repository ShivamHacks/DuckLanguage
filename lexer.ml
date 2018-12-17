open DuckTypes
open Printf

(* Simple language only handles expressions *)

let re_whitespace = Str.regexp "[ \n\t]+";;
(* note: identifier must come after keywords for lexer to work properly *)
(* otherwise, the lexer will recognize keywords as identifiers *)
let token_table = [
	(***** EXPRESSIONS *****)
	(* Variables *)
	(	Str.regexp "-?[0-9]+",				(fun str -> Tok_Int (int_of_string str))		);
	(	Str.regexp "\"[^\"\"]*\"",			(fun str -> Tok_String str)						);
	(* Identifier *)
	(	Str.regexp "[a-zA-Z][a-zA-Z0-9]*",	(fun str -> Tok_ID str)							);
	(* Math *)
	(	Str.regexp_string "+", 				(fun str -> Tok_Add)							);
	(	Str.regexp_string "-", 				(fun str -> Tok_Sub)							);
	(	Str.regexp_string "*", 				(fun str -> Tok_Mult)							);
	(	Str.regexp_string "/", 				(fun str -> Tok_Div)							);
	(* Syntax *)
	(	Str.regexp_string "(", 				(fun str -> Tok_LParen)							);
	(	Str.regexp_string ")", 				(fun str -> Tok_RParen)							);
	(	Str.regexp_string "[", 				(fun str -> Tok_LBracket)						);
	(	Str.regexp_string "]", 				(fun str -> Tok_RBracket)						);
	(	Str.regexp_string "{", 				(fun str -> Tok_LBrace)							);
	(	Str.regexp_string "}", 				(fun str -> Tok_RBrace)							);
	(	Str.regexp_string "<=", 			(fun str -> Tok_Assign)							);
	(	Str.regexp_string ",", 				(fun str -> Tok_Comma)							);
	(	Str.regexp_string ";", 				(fun str -> Tok_Semi)							)
];;

(* finds the longest matching token *)
(* returns matched token and positon offset for string *)
let rec find_longest_match str toks pos largest_match curr_match = match toks with
	(* iterated through entire token map *)
	[] -> ( match curr_match with
		Tok_EMPTY -> raise (InvalidInputException "tokenize")
		| _ -> (curr_match, largest_match) )
	(* there are remaining tokens in map that can be matched *)
	| (tok_re, tok_wrap) :: next_toks ->
		let matched = Str.string_match tok_re str pos in
		let token = if matched then Str.matched_string str else "" in
		let len = if matched then String.length token else 0 in
		(* Found a match longer than current match *)
		if matched && len > largest_match then
			find_longest_match str next_toks pos len (tok_wrap token)
		(* Did not find a match longer than current match *)
		else find_longest_match str next_toks pos largest_match curr_match
;;

(* converts code into list of tokens *)
let tokenize input =
	let rec tokenize_helper str pos = 
		if pos >= String.length str then [EOF]
		else if Str.string_match re_whitespace str pos then
			let len = String.length (Str.matched_string str) in
			tokenize_helper str (pos + len)
		else
			let (tok, offset) = find_longest_match str token_table pos 0 Tok_EMPTY in
			tok :: (tokenize_helper str (pos + offset))
	in tokenize_helper input 0
;;

