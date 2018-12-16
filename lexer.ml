open DuckTypes

(*
1. Keywords must be read before identifier
*)

module L = List (*declarations for laziness/reuse*)
module S = String
module R = Str

let tokenize input =
let re = [ (* Regular expressions and the tokens they generate. *)
  (R.regexp_string("("), fun _ -> [Tok_LParen]) ;
  (R.regexp_string(")"), fun _ -> [Tok_RParen]) ;
  (R.regexp_string("{"), fun _ -> [Tok_LBrace]) ;
  (R.regexp_string("}"), fun _ -> [Tok_RBrace]) ;
  (R.regexp_string("=="), fun _ -> [Tok_Equal]) ;
  (R.regexp_string("!="), fun _ -> [Tok_NotEqual]) ;
  (R.regexp_string(">="), fun _ -> [Tok_GreaterEqual]) ;
  (R.regexp_string("<="), fun _ -> [Tok_LessEqual]) ;
  (R.regexp_string(">"), fun _ -> [Tok_Greater]) ;
  (R.regexp_string("<"), fun _ -> [Tok_Less]) ;
  (R.regexp_string("="), fun _ -> [Tok_Assign]) ;
  (R.regexp_string("||"), fun _ -> [Tok_Or]) ;
  (R.regexp_string("&&"), fun _ -> [Tok_And]) ;
  (R.regexp_string("!"), fun _ -> [Tok_Not]) ;
  (R.regexp_string(";"), fun _ -> [Tok_Semi]) ;
  (R.regexp("-?[0-9]+(.[0-9]+)?"), fun x -> [Tok_Float (float_of_string x)]) ;
  (R.regexp("true\\|false"), fun x -> [Tok_Bool (bool_of_string x)]) ;
  (R.regexp_string("float"), fun _ -> [Tok_Float_Type]) ;
  (R.regexp_string("bool"), fun _ -> [Tok_Bool_Type]) ;
  (R.regexp_string("quack"), fun _ -> [Tok_Print]) ;
  (R.regexp_string("if"), fun _ -> [Tok_If]) ;
  (R.regexp_string("else"), fun _ -> [Tok_Else]) ;
  (R.regexp_string("while"), fun _ -> [Tok_While]) ;
  (R.regexp_string("+"), fun _ -> [Tok_Add]) ;
  (R.regexp_string("-"), fun _ -> [Tok_Sub]) ;
  (R.regexp_string("*"), fun _ -> [Tok_Mult]) ;
  (R.regexp_string("/"), fun _ -> [Tok_Div]) ;
  (R.regexp("[a-zA-Z][a-zA-Z0-9]*"), fun s -> [Tok_ID s]) ;
  (R.regexp "[ \t\n]" , fun _ -> []) (*to eat spaces rather than splitting on them*)
] in 
let rec tokenize' (s : string) (pos : int) : token list = 
  if pos >= S.length s then [EOF]
  else
    match L.find_all (fun (matched, _) -> S.length matched > 0) @@ L.map (fun (reg, f) -> ((if R.string_match reg s pos then R.matched_string s else ""), f)) re with
    | [] -> raise (InvalidInputException "lexer didn't match anything")
    | h::t -> let (s', f) = L.fold_left (fun (s'_acc, f_acc) (s', f) -> if S.length s' > S.length s'_acc then (s', f) else (s'_acc, f_acc)) h t in
      (f s') @ (tokenize' s (pos + (S.length s')))
in tokenize' input 0
