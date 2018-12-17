(* Contains all the types & errors *)

(* Contains all the types & errors *)

type token = 
	(* Empty token *)
	| Tok_EMPTY
	(* Instances *)
	| Tok_ID of string
	(* Types *)
  	| Tok_Int of int
  	| Tok_String of string
  	(* Binary Ops *)
	| Tok_Add
	| Tok_Sub
	| Tok_Mult
	| Tok_Div
	(* Syntax *)
	| Tok_LParen
	| Tok_RParen
	| Tok_LBracket
	| Tok_RBracket
  	| Tok_LBrace
  	| Tok_RBrace
  	| Tok_Assign
  	| Tok_Comma
  	| Tok_Semi
  	(* Functions *)
  	| Tok_Print
	(* Misc *)
	| EOF

type expr =
	(* Identifiers *)
  	| ID of string
  	(* Values *)
  	| Int of int
  	| String of string
  	(* Binary Ops *)
  	| Add of expr * expr
  	| Sub of expr * expr
  	| Mult of expr * expr
  	| Div of expr * expr
  	(* Arrays *)
  	| Array of expr list

type stmt =
  | NoOp
  | Seq of stmt * stmt
  | Declare of string * expr
  | Print of expr

(* Errors *)
exception InvalidInputException of string