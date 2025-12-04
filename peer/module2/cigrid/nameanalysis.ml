open Printf
open Ast

exception NameAnalysisError of string * Lexing.position
exception PassAlongError	of string
exception ProgramError		of string

type symbol =
	| Symbol of string * typ

and typ =
	| Int
	| Char 
	| String
	| Void
	| Ident of string
	| Pointer of typ
	| Arrow of typ * typ
	| Struct of string * symbol list



let rec pprint_typ = function
	| Int -> "Int"
	| Char -> "Char"
	| String -> "String"
	| Void -> "Void"
	| Ident(r) -> "Ident(" ^ r ^ ")"
	| Pointer(t) -> "Pointer(" ^ pprint_typ t ^ ")"
	| Arrow(t1, t2) -> "Arrow(" ^ pprint_typ t1 ^ ", " ^ pprint_typ t2 ^ ")"
	| Struct(r, sym_lst) -> "Struct(" ^ r ^ ", {" ^ pprint_symbol_list sym_lst ^ "}"

and pprint_symbol = function
	| Symbol(r,t) -> "(" ^ r ^ ":" ^ pprint_typ t ^ ")"
and pprint_symbol_list = function
	| [] -> ""
	| hd :: rs -> 
		"(" ^ pprint_symbol hd ^ "), " ^ pprint_symbol_list rs
and pprint_env = function
	| [] -> ""
	| hd :: rs -> pprint_symbol hd ^ ", " ^ pprint_env rs



(* env_contains():
 *	Checks if symbol identifier exists in environment list.
 *)
let rec env_contains lst ident =
		match lst with
		| [] -> false
		| Symbol(r, t) :: rs -> 
			if r = ident then true
			else env_contains rs ident

(* env_get_type():
 *	Iterates through all env symbols and returns the corresponding type
 *	when ident is found.
 *)
let rec env_get_type lst ident =
		match lst with
		| [] -> raise (PassAlongError "Type not found in env.")
		| Symbol(r, t) :: rs ->
			if r = ident then t
			else env_get_type rs ident


(* get_type():
 * 	Converts AST type into type checker type.
 *	If not regular type then check if it is a declared struct
 *	in type_env.
 *)
let rec get_type t type_env =
	match t with
	| TInt -> Int
	| TChar -> Char
	| TVoid -> Void
	| TIdent(r) -> 
		if env_contains type_env r then
			env_get_type type_env r
		else raise (PassAlongError "Undeclared type used.")

	| TPoint(p) -> Pointer(get_type p type_env) 

(* traverse_expr():
 * 	travels through expressions. Only cares about VAR_ENV
 *
 *	Evar(r): check that r exists in env then return type of r.
 *		 if not then variable is undeclared.
 *
 *	ECall(r): check that function called is defined (type_env)
 *)
let rec traverse_expr e type_env var_env =
	match e with
	| EVar(r, pos) -> 
		if env_contains var_env r then ()
		else raise (NameAnalysisError ("EVar undeclared.", pos))
	| EInt(i, _) 	-> ()
	| EChar(c, _) 	-> ()
	| EString(s, _) -> ()
	| EBinOp(b, e1, e2, _) -> 
		let (_) = traverse_expr e1 type_env var_env in 
		let (_) = traverse_expr e2 type_env var_env in
		(* Printf.printf "%s  |  %s\n" (pprint_env type_env) (pprint_env var_env); *)
		()

	| EUnOp(uop, e1, _) ->
		let (_) = traverse_expr e1 type_env var_env in 
		()

	| ECall(r, e_lst, pos) ->
		if env_contains type_env r then 
			let (_) = traverse_expr_list e_lst type_env var_env in 
 			()
		else 
			if env_contains var_env r then
				let (_) = traverse_expr_list e_lst type_env var_env in 
				()
			else raise (NameAnalysisError ("ECall undefined and undeclared.", pos))

	| ENew(t, e1, _) -> ()

	| EArrayAccess(r, e1, r_opt, _) -> 
		let (_) = traverse_expr e1 type_env var_env in
		()
		(* make it same as EVar() case? *)
		
			
and traverse_expr_list e_lst type_env var_env =
	match e_lst with
	| [] -> ()
	| e :: es -> 
		let (_) = traverse_expr e type_env var_env in
		traverse_expr_list es type_env var_env
			
let traverse_expr_opt e_opt type_env var_env =
	match e_opt with
	| Some(e) -> traverse_expr e type_env var_env
	| None -> ()


(* iterate_types():
 *	iterate through pointers until real type is
 *	reached, and return real type.
 *)	
let rec iterate_types t type_env =
	match t with
	| Pointer(p) -> iterate_types p type_env
	| Ident(r) -> 
		if env_contains type_env r then
			env_get_type type_env r
		else raise (PassAlongError "Undeclared type used.")
	| _ as bt    -> bt



(* traverse_stmt():
 *	SExpr() continue traverse expr.
 *
 *	SVarDef() similar to param. Check if var exists in env,
 *	if not then add to env and traverse expr.
 *
 *	SVarAssign() can only assign to existing variables.
 *
 *	SArrayAssign():
 *	SArrayAssing("a1", EInt(2), "x", EInt(3))
 *	first check if a1 exists. 
 *	if r_opt ("x") exists then it has to be a struct
 *	and we get_type = pointer(struct("a2", [x,y]) and then 
 *	go through all pointers until we get real symbol (struct).
 *	Then check that member ("x") exists in struct parameters.
 *
 *	SScope() when we enter scope and return back up we get a new
 *	updated env containing local vars from inside scope. 
 *	Thus we do NOT want this env, we want the old env (env).
 *	This is the equivalent of popping of the stack.
 *
 *	SIf() traverse expr, then traverse statement. 
 *	ELSE should use old env.
 *
 *	SReturn() continue traverse optional expr
 *
 *	SDelete() check that var exists in env. 
 *	Do NOT remove var from env. Analysis does not concern
 *	freed memory at runtime.
 *
 *)
let rec traverse_stmt s type_env var_env =
	match s with
	| SExpr(e) -> 
		let (_) = traverse_expr e type_env var_env in
		(type_env, var_env)
	
	| SVarDef(t, r, e, pos) -> 
		(* Printf.printf "SVarDef()\n"; *)
		if (env_contains var_env r) = false then 
			let var_env2 = Symbol(r, (get_type t type_env)) :: var_env in
			(* Printf.printf "Added var %s to var_env. env: %s\n" r (pprint_env var_env2); *)
			let (_) = traverse_expr e type_env var_env2 in
			(type_env, var_env2)
		else raise (NameAnalysisError ("SVarDef already exists.", pos))

	| SVarAssign(r, e, pos) -> 
		if (env_contains var_env r) = true then
			let (_) = traverse_expr e type_env var_env in
			(type_env, var_env)
		else raise (NameAnalysisError ("SVarAssign to undeclared variable.", pos))

	| SArrayAssign(r, e1, r_opt, e2, pos) -> 
		(* Printf.printf "SArrayAssign.\n"; *)
		if (env_contains var_env r) = true then 
			let (_) = traverse_expr e1 type_env var_env in (
			match r_opt with
			| Some(member) ->
				let r_type = env_get_type var_env r in
				let bt = iterate_types r_type type_env in (
					match bt with 
					| Struct(_, sym_lst) -> 
						if env_contains sym_lst member then 
							let (_) = traverse_expr e2 type_env var_env in
							(type_env, var_env)
						else raise (NameAnalysisError ("Struct member does not exist.", pos))
					| _ -> raise (NameAnalysisError ("base type is not a struct.", pos))
				)
			| None -> 
				let (_) = traverse_expr e2 type_env var_env in 
				(type_env, var_env)
			)	
		else raise (NameAnalysisError ("SArrayAssign to undeclared variable.", pos))

	| SScope(s_lst) -> 
		(* Printf.printf "SScope()\n"; *)
		let (type_env2, var_env2) = traverse_stmt_list s_lst type_env var_env in 
		(* Printf.printf "subset var: %s\n" (pprint_env var_env2); *)
		(* Printf.printf "subset type: %s\n" (pprint_env type_env2); *)
		(type_env, var_env)

	| SIf(e, s, else_opt, _) ->
		let (_) = traverse_expr e type_env var_env in
		let (type_env2, var_env2) = traverse_stmt s type_env var_env in
		traverse_else_opt else_opt type_env2 var_env2

	| SWhile(e, s) -> 
		let (_) = traverse_expr e type_env var_env in
		traverse_stmt s type_env var_env

	| SBreak -> (type_env, var_env)

	| SReturn(e_opt) -> 
		let (_) = traverse_expr_opt e_opt type_env var_env in
		(type_env, var_env)

	| SDelete(r, pos) ->
		if env_contains var_env r then (type_env, var_env)
		else raise (NameAnalysisError ("SDelete undeclared variable.", pos))


and traverse_stmt_list s_lst type_env var_env = 
	match s_lst with
	| [] -> (type_env, var_env)
	| s :: rs -> 
		let (type_env2, var_env2) = traverse_stmt s type_env var_env in
		traverse_stmt_list rs type_env2 var_env2

and traverse_else_opt else_opt type_env var_env =
	match else_opt with
	| Some(s) -> traverse_stmt s type_env var_env
	| None -> (type_env, var_env)

(* traverse_param():
 *	Adds valid param to env. Otherwise throw error.
 *	If param already exists in env then also throw error.
 *)
let rec traverse_param p type_env var_env =
	match p with
	| Param(t,r) ->
		if (env_contains var_env r) = false then
			((get_type t type_env), Symbol(r, (get_type t type_env)) :: var_env)
		else raise (PassAlongError "param already exists.")
	| _ -> 
		raise (PassAlongError "Invalid param.")

(* traverse_params_list():
 * 	travels through all params and sends updated env along.
 *	Also thorugh Arrow -> finds the expected type for original function.
 *	int foo(int x, int y) : (Int -> (Int -> Int))
 *)
let rec traverse_params_list p_lst pt type_env var_env =
	match p_lst with	
	| [] -> (pt, var_env)
	| p :: ps -> 
		let (npt, var_env2) = traverse_param p type_env var_env in
		traverse_params_list ps (Arrow(npt, pt)) type_env var_env2

(* traverse_global():
 *	Trickles down case for each global grammar rule.
 *
 *	GFuncDef: go through param list and validate.
 *		  go through all statements and validate.
 *		  Definition creates a type in type_env.
 *		  Need to check that type is not already defined, 
 *		  and if function is already declared, that types match exactly.
 *		
 *	GStruct: returns TYPE of new struct and embeds
 *		 param list into struct type.
 *)
let rec traverse_global g type_env var_env =
	match g with
	| GFuncDef(t, r, p_lst, s, pos) -> (
		try 
			(* Printf.printf "GFuncDef()\n"; *)
			if (env_contains var_env r) = false then
				let (pt, param_env) = traverse_params_list p_lst (get_type t type_env) type_env var_env in
				let (_, _) = traverse_stmt s type_env (Symbol(r, pt) :: param_env) in
				let var_env2 = (Symbol(r, pt) :: var_env) in
				if (env_contains type_env r) then
					let ot = env_get_type type_env r in
					if ot = pt then (type_env, var_env2)
					else raise (NameAnalysisError ("Function definition does not match declaration.", pos))
				else (type_env, var_env2)
			else raise (NameAnalysisError ("Function defined already exist.", pos))
		with
		| PassAlongError(msg) -> raise (NameAnalysisError(msg, pos))
		)
		

	(* Currently only allows 1 time declaration of function. *)
	| GFuncDecl(t, r, p_lst, pos) -> (
		try
			(* Printf.printf "GFuncDecl()\n"; *)
			if (env_contains type_env r) = false then
				let (pt, _) = traverse_params_list p_lst (get_type t type_env) type_env var_env in
				let type_env2 = (Symbol(r, pt) :: type_env) in
				if (env_contains var_env r) then 
					let ot = env_get_type var_env r in
					if ot = pt then (type_env2, var_env)
					else raise (NameAnalysisError ("Function declaration does not match definition.", pos))
				else (type_env2, var_env)
			else raise (NameAnalysisError ("Function already declared exist.", pos))
		with
		| PassAlongError(msg) -> raise (NameAnalysisError(msg, pos))
		)


	| GVarDef(t,r,e, pos) -> 
		(* Printf.printf "GVarDef()\n"; *)
		if (env_contains var_env r) = false then
			let pt = (get_type t type_env) in
			let var_env2 = (Symbol(r, pt) :: var_env) in
			let (_) = traverse_expr e type_env var_env2 in
			if (env_contains type_env r) then
				let ot = env_get_type type_env r in
				if ot = pt then (type_env, var_env2)
				else raise (NameAnalysisError ("Global Variable definition does not match declaration.", pos))
			else 
			(type_env, var_env2)
		else raise (NameAnalysisError ("Global Variable defined already exist.", pos))
			

	| GVarDecl(t,r, pos) -> 
		(* Printf.printf "GVarDecl()\n"; *)
		if (env_contains type_env r) = false then
			let pt = (get_type t type_env) in
			let type_env2 = (Symbol(r, pt) :: type_env) in
			if (env_contains var_env r) then 
				let ot = env_get_type var_env r in
				if ot = pt then (type_env2, var_env)
				else raise (NameAnalysisError ("Global Variable declaration does not match definition.", pos))
			else (type_env2, var_env)
		else raise (NameAnalysisError ("Global Variable already declared exist.", pos))
		

	| GStruct(r, p_lst, pos) -> (
		try
			(* Printf.printf "GStruct()\n"; *)
			if (env_contains type_env r) = false then 
				let type_env2 = Symbol(r, Ident(r)) :: type_env in
				let (_, param_env) = traverse_params_list p_lst Void type_env2 [] in
				let type_env3 = Symbol(r, Struct(r, param_env)) :: type_env in
				(type_env3, var_env)
			else raise (NameAnalysisError ("Struct defined already exist.", pos))
		with
		| PassAlongError(msg) -> raise (NameAnalysisError(msg, pos))
		)
	
		

let rec traverse_global_list g_lst type_env var_env =
	match g_lst with
	| [] -> 
		(* Printf.printf "exit []: var: %s\n" (pprint_env var_env); *)
		(* Printf.printf "exit [] type: %s\n" (pprint_env type_env); *)
		[]
	| g :: gs -> 
		(* Printf.printf "traverse global.\n" ; *)
		let (type_env2, var_env2) = traverse_global g type_env var_env in
		(* Printf.printf "glist: var: %s\n" (pprint_env var_env2); *)
		(* Printf.printf "glist type: %s\n" (pprint_env type_env2); *)
		traverse_global_list gs type_env2 var_env2

let nameAnalysis tree line_error =
	try 
		(* Printf.printf "name analysis.\n" ; *)
		match tree with
		| Prog(g_lst) ->
			traverse_global_list g_lst [] []
		| _ -> 
			raise (ProgramError "No program given.")
	with
	| NameAnalysisError(msg, pos) 	-> 
		if line_error = true then (
			Printf.fprintf stderr "%d\n" pos.pos_lnum; 
			exit 2 
		)
		else (
			Printf.fprintf stderr "%s\n" msg; 
			exit 2 
		)
