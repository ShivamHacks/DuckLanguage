(* Contains all the types & errors *)

(* Contains all the types & errors *)

type token = 
	(* Empty token *)
	Tok_EMPTY
	(* Instances *)
	| Tok_ID of string
	(* Types *)
	| Tok_Float_Type
  	| Tok_Bool_Type
  	| Tok_String_Type
  	| Tok_Float of float
  	| Tok_Bool of bool
  	| Tok_String of string
  	(* Binary Ops *)
	| Tok_Add
	| Tok_Sub
	| Tok_Mult
	| Tok_Div
  	| Tok_Greater
	| Tok_Less
	| Tok_GreaterEqual
  	| Tok_LessEqual
  	| Tok_Equal
  	| Tok_NotEqual
  	| Tok_Or
  	| Tok_And
  	(* Unary Ops *)
	| Tok_Not
	(* Control Flow *)
	| Tok_If
	| Tok_Else
	| Tok_While
	(* Syntax *)
	| Tok_LParen
	| Tok_RParen
  	| Tok_RBrace
  	| Tok_LBrace
  	| Tok_Semi
  	| Tok_Assign
  	(* Functions *)
  	| Tok_Print
	(* Misc *)
	| EOF

type data_type =
	| Float_Type
	| Bool_Type
	| String_Type

type data_value =
	| Float_Val of float
	| Bool_Val of bool
	| String_Val of string

type expr =
	(* Identifiers *)
  	| ID of string
  	(* Values *)
  	| Float of float
  	| Bool of bool
  	| String of string
  	(* Binary Ops *)
  	| Add of expr * expr
  	| Sub of expr * expr
  	| Mult of expr * expr
  	| Div of expr * expr
  	| Greater of expr * expr
  	| Less of expr * expr
  	| GreaterEqual of expr * expr
  	| LessEqual of expr * expr
  	| Equal of expr * expr
  	| NotEqual of expr * expr
  	| Or of expr * expr
  	| And of expr * expr
  	(* Unary Ops *)
  	| Not of expr

type stmt =
  | NoOp
  | Seq of stmt * stmt
  | Declare of data_type * string
  | Assign of string * expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Print of expr

type environment = (string * data_value) list
(* Errors : TBD *)
exception InvalidInputException of string

let new_token_to_string token = match token with
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
