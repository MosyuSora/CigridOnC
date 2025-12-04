open Printf

type uop = 
	| UopNegate
	| UopBitNot
	| UopMinus

type bop = 
	| BopADD 
	| BopSUB 
	| BopMUL 
	| BopDIV
	| BopMOD
	| BopLT
	| BopGE
	| BopLTE
	| BopGTE
	| BopSLT
	| BopSRT
	| BopEQ
	| BopNEQ
	| BopCOMBAND
	| BopCOMBOR
	| BopBITAND
	| BopBITOR

type ty =
	| TVoid
	| TInt
	| TChar
	| TIdent of string  (* for self defined struct ty *)
	| TPoint of ty

type expr =
	| EVar 		of string * Lexing.position
	| EInt 		of int * Lexing.position
	| EChar		of char * Lexing.position
	| EString	of string * Lexing.position

	| EBinOp 	of bop 	* expr * expr * Lexing.position
	| EUnOp 	of uop 	* expr * Lexing.position
	| ECall		of string * expr list * Lexing.position
	| ENew		of ty * expr * Lexing.position
	| EArrayAccess	of string * expr * string option * Lexing.position

type stmt =
	| SExpr		of expr
	| SVarDef	of ty * string * expr * Lexing.position
	| SVarAssign	of string * expr * Lexing.position
	| SArrayAssign 	of string * expr * string option * expr * Lexing.position
	| SScope	of stmt list
	| SIf		of expr * stmt * stmt option * Lexing.position
	| SWhile	of expr * stmt
	| SBreak
	| SReturn	of expr option
	| SDelete	of string * Lexing.position

type params =
	| Param 	of ty * string

type global =
	| GFuncDef	of ty * string * params list * stmt * Lexing.position
	| GFuncDecl	of ty * string * params list * Lexing.position
	| GVarDef	of ty * string * expr * Lexing.position
	| GVarDecl	of ty * string * Lexing.position
	| GStruct	of string * params list * Lexing.position


type program =
	| Prog		of global list

let pprint_bop = function
	| BopADD -> "+"
	| BopSUB -> "-"
	| BopMUL -> "*"
	| BopDIV -> "/"
	| BopMOD -> "%"
	| BopLT  -> "<"
	| BopGE	 -> ">"
	| BopLTE -> "<="
	| BopGTE -> ">="
	| BopSLT -> "<<"
	| BopSRT -> ">>"
	| BopEQ  -> "=="
	| BopNEQ -> "!="
	| BopCOMBAND -> "&&"
	| BopCOMBOR  -> "||"
	| BopBITAND  -> "&"
	| BopBITOR   -> "|"

let pprint_uop = function
	| UopMinus -> "-"
	| UopNegate -> "!"
	| UopBitNot -> "~"

let pprint_ident = function
	| r -> "\"" ^ r ^ "\""

let rec pprint_ident_option = function
	| None -> ""
	| Some(r) -> pprint_ident r

let rec pprint_ident_list = function
	| [] -> ""
	| hd :: rs -> hd ^ ", " ^ pprint_ident_list rs

(* pprint_ty(): *)
(* ========================================= *)
let rec pprint_ty = function
	| TVoid -> "TVoid"
	| TInt -> "TInt"
	| TChar -> "TChar"
	| TIdent(r) -> "TIdent(" ^ pprint_ident r ^ ")"
	| TPoint(t) -> "TPoint(" ^ pprint_ty t ^ ")"


(* pprint_expr(): *)
(* ========================================= *)
let rec pprint_expr = function
	| EVar(r, _) 	-> "EVar(" ^ pprint_ident r ^ ")"
	| EInt(i, _) 	-> sprintf "EInt(%d)" i
	| EChar(c, _) -> (
		match c with
		| '\n' -> sprintf "EChar('\\n')" 
		| '\t' -> sprintf "EChar('\\t')" 
		| '\\' -> sprintf "EChar('\\\\')" 
		| '\'' -> sprintf "EChar('\\'')" 
		| '\"' -> sprintf "EChar('\\\"')" 
		| _ -> sprintf "EChar('%c')" c 
	)
	| EString(r, _) 	-> "EString(" ^ r ^ ")"
	| EBinOp(bop, e1, e2, _) -> 
		"EBinOp(" ^ pprint_bop bop ^ ", " ^ pprint_expr e1 ^ ", " ^ pprint_expr e2 ^ ")"
	| EUnOp(uop, e, _) -> 
		"EUnOp(" ^ pprint_uop uop ^ ", " ^ pprint_expr e ^ ")"
	| ECall(r, e_lst, _) ->
		"ECall(" ^ pprint_ident r ^ ",{" ^ pprint_expr_list e_lst ^ "})"
	| ENew(t, e, _) ->
		"ENew(" ^ pprint_ty t ^ ", " ^ pprint_expr e ^ ")"
	| EArrayAccess(r, e, r_opt, _) -> 
		"EArrayAccess(" ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ", " ^ pprint_ident_option r_opt ^ ")"
and pprint_expr_list = function
	| [] -> ""
	| [hd] -> pprint_expr hd
	| hd :: rs -> pprint_expr hd ^ " " ^ pprint_expr_list rs
and pprint_expr_option = function
	| None -> ""
	| Some(e) -> pprint_expr e

(* pprint_stmt(): *)
(* ========================================= *)
let rec pprint_stmt indent = function
	| SExpr(e) 		-> "SExpr(" ^ pprint_expr e ^ ")"
	| SVarDef(t,r,e, _)  	-> "SVarDef(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ")"
	| SVarAssign(r,e, _) 	-> "SVarAssign(" ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ")"
	| SArrayAssign(r,e1,r_opt,e2, _) -> 
		"SArrayAssign(" ^ pprint_ident r ^ ", " ^pprint_expr e1 ^
		", " ^ pprint_ident_option r_opt ^ ", " ^ pprint_expr e2 ^ ")"
	| SScope(s_lst) 	-> "SScope({" ^ (pprint_stmt_list (indent ^ "  ") s_lst) ^ "\n" ^ indent ^ "})"
	| SIf(e, s, else_opt, _) -> 
		"SIf(" ^ pprint_expr e ^ ",\n" ^ (indent ^ "  ") ^ (pprint_stmt (indent ^ "  ") s) ^ ", " ^ 
		(pprint_else_option (indent ^ "  ") else_opt) ^ ")"
	| SWhile(e, s)		-> "SWhile(" ^ pprint_expr e ^ ",\n" ^ indent ^ (pprint_stmt (indent ^ "  ") s) ^ ")"
	| SBreak		-> "SBreak"
	| SReturn(e_opt) 	-> "SReturn(" ^ pprint_expr_option e_opt ^ ")"
	| SDelete(r, _) 	-> "SDelete(" ^ pprint_ident r ^ ")"
and pprint_stmt_list indent = function
	| [] -> ""
	| hd :: rs -> "\n" ^ indent ^ (pprint_stmt indent hd) ^ (pprint_stmt_list indent rs)
and pprint_else_option indent = function
	| None -> ""
	| Some(s) -> "\n" ^ indent ^ (pprint_stmt indent s)

(* pprint_params(): *)
(* ========================================= *)
let pprint_params = function
	| Param(t, r) -> "(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ")"
let rec pprint_params_list = function
	| [] -> ""
	| [hd] -> pprint_params hd
	| hd :: rs -> pprint_params hd ^ " " ^ pprint_params_list rs
let rec pprint_construct_list indent = function
	| [] -> ""
	| [hd] -> "\n" ^ indent ^ pprint_params hd
	| hd :: rs -> "\n" ^ indent ^ pprint_params hd ^ pprint_construct_list indent rs

(* pprint_global(): *)
(* ========================================= *)
let rec pprint_global = function
	| GFuncDef(t,r,p_lst,s,_) -> 
		"GFuncDef(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", {" ^ pprint_params_list p_lst ^ "}," ^
		"\n" ^ "  " ^ (pprint_stmt "  " s) ^ ")\n"
	| GFuncDecl(t,r,p_lst,_) -> 
		"GFuncDecl(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", {" ^ pprint_params_list p_lst ^ "})\n"
	| GVarDef(t,r,e,_) -> "GVarDef(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ")\n"
	| GVarDecl(t,r,_) -> "GVarDecl(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ")\n"
	| GStruct(r, p_lst,_) -> "GStruct(" ^ pprint_ident r ^ ",{" ^ (pprint_construct_list "  " p_lst)  ^ "\n})\n"

let rec pprint_global_list = function
	| [] -> ""
	| [hd] -> pprint_global hd
	| hd :: rs -> pprint_global hd ^ "\n" ^ pprint_global_list rs
	
(* pprint_program(): *)
(* ========================================= *)
let pprint_program = function
	| Prog(g_lst) -> pprint_global_list g_lst
