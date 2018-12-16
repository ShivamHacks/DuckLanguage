open DuckTypes

(* Simple language only handles expressions *)

let token_table = [
	(***** EXPRESSIONS *****)
	(	Str.regexp_string "(", 		(fun (str : string) : token -> Tok_LParen)				);
	(	Str.regexp_string ")", 		(fun (str : string) : token -> Tok_RParen)				);
	(* Math *)
	(	Str.regexp_string "+", 		(fun (str : string) : token -> Tok_Add)					);
	(	Str.regexp_string "-", 		(fun (str : string) : token -> Tok_Sub)					);
	(	Str.regexp_string "*", 		(fun (str : string) : token -> Tok_Mult)				);
	(	Str.regexp_string "/", 		(fun (str : string) : token -> Tok_Div)					);
	(* Logic *)
	(	Str.regexp_string "==", 	(fun (str : string) : token -> Tok_Equal)				);
	(	Str.regexp_string "!=", 	(fun (str : string) : token -> Tok_NotEqual)			);
	(	Str.regexp_string ">=", 	(fun (str : string) : token -> Tok_GreaterEqual)		);
	(	Str.regexp_string "<=", 	(fun (str : string) : token -> Tok_LessEqual)			);
	(	Str.regexp_string ">", 		(fun (str : string) : token -> Tok_Greater)				);
	(	Str.regexp_string "<", 		(fun (str : string) : token -> Tok_Less)				);
	(	Str.regexp_string "&&", 	(fun (str : string) : token -> Tok_Add)					);
	(	Str.regexp_string "||", 	(fun (str : string) : token -> Tok_Or)					);
	(	Str.regexp_string "!", 		(fun (str : string) : token -> Tok_Not)					)
];;

(* finds the longest matching token *)
(* returns matched token and positon offset for string *)
let rec find_longest_match str toks pos largest_match curr_match = match toks with
	(* iterated through entire token map *)
	[] -> ( match curr_match with
		Tok_EMPTY -> raise (InvalidInputException "tokenize")
		| _ -> (curr_match, pos + largest_match) )
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
		else
			let (tok, offset) = find_longest_match str token_table pos 0 Tok_EMPTY in
			tok :: (tokenize_helper str (pos + offset))
	in tokenize_helper input 0
;;

