open Printf
open Ast

exception NameAnalysisError 	of string * Lexing.position
exception TypeCheckError	of string * Lexing.position
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

(* ============================================= *)
(* TYPE TABLES FOR UNARY OPERATIONS		 *)
(* ============================================= *)
let uop_negate = 
	[
	 ((Int), (Int)) 	;
	]
let uop_bitnot = 
	[
	 ((Int), (Int)) 	;
	]
let uop_minus = 
	[
	 ((Int), (Int)) 	;
	]

(* ============================================= *)
(* TYPE TABLES FOR BINARY OPERATIONS		 *)
(* ============================================= *)
let binop_add = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
let binop_sub = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
let binop_mul = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
let binop_div = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
let binop_mod = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
let binop_slt = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
let binop_srt = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Char)) ;
	]
(* Boolean operations but there are no booleans. *)
let binop_lt = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) ;
	]
let binop_ge = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) ;
	]
let binop_lte = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) ;
	]
let binop_gte = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) ;
	]

(* EQ "==" and NEQ "!=" can be used on pointers. *)
let binop_eq = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) 	;
	]
let binop_neq = 
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) 	;
	]
let binop_comband =
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) 	;
	]
let binop_combor =
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) 	;
	]
let binop_bitand =
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) 	;
	]
let binop_bitor =
	[
	 ((Int, Int), (Int)) 	;
	 ((Char, Int), (Int)) 	;
	 ((Int, Char), (Int)) 	;
	 ((Char, Char), (Int)) 	;
	]






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
	| hd :: rs -> pprint_symbol hd ^ "\n --- " ^ pprint_env rs


(* lookup_type():
 * 	Tries to find matching type and return corresponding return type.
 *	If there is no match then type error has occurred.
 *)
let rec lookup_type lst t =
	match lst with
	| [] -> raise (PassAlongError "Cannot find matching type.")
	| (it, ot) :: rs -> 
		if it = t then ot
		else lookup_type rs t

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


let rec get_return_type t = 
	match t with
	| Arrow(t1, t2) -> get_return_type t2
	| _ as rt -> rt


let rec compare_types t1 t2 =
	(* Printf.printf "COMPARE TYPES t1: %s   t2: %s\n" (pprint_typ t1) (pprint_typ t2); *)
	if t1 = t2 then true
	else 
		match t1, t2 with
		| (Char, Int) -> true
		| (Int, Char) -> true
		| (Arrow(a1, a2), Arrow(b1, b2)) ->
			if (compare_types a1 b1) && (compare_types a2 b2) then true
			else false
			
		| (Pointer(p1), Pointer(p2)) -> 
			(* Printf.printf "COMPARE TYPE POINTER p1: %s   p2: %s\n" (pprint_typ p1) (pprint_typ p2); *)
			compare_types p1 p2

		| (Ident(r1), Struct(r2, _)) -> 
			if r1 = r2 then true
			else false
		| (Struct(r1, _), Ident(r2)) -> 
			if r1 = r2 then true
			else false
		| (Struct(r1, _), Struct(r2, _)) -> 
			if r1 = r2 then true
			else false

		| _ -> false

(* traverse_expr():
 * 	travels through expressions. Only cares about VAR_ENV
 *
 *	Evar(r): check that r exists in env then return type of r.
 *		 if not then variable is undeclared.
 *
 *	ECall(r, e_lst): check that function called is defined (type_env)
 *		  or declared in (var_env).
 *		  Check that function args align with function definition
 *		  get return type then recursive descent, on way back up
 *		  combine types, starting form ret in Arrow(t, ret). 
 *		  On finish we have entire function type and can compare.
 *		  The return type itself we can assume as that is compared
 *		  to variable we are assigning to anyways.
 *		  string z = sqrt(5) -> string != int.
 *		  Only compare function args in Ecall.
 *
 *	ENew(t, e1): Check that t exists (in type_var for structs) or regular 
 *		  get_type for int, char, etc. 
 *		  check that e1 is int since it is index in array.
 *		  Wrap rt in pointer(rt) since pointer into struct or array.
 *)
let rec traverse_expr e type_env var_env =
	match e with
	| EVar(r, pos) -> 
		if env_contains var_env r then 
			let rt = env_get_type var_env r in
			(rt)
		else raise (NameAnalysisError ("EVar undeclared.", pos))
	| EInt(i, _) 	-> Int
	| EChar(c, _) 	-> Char
	| EString(s, _) -> Pointer(Char)
	| EBinOp(b, e1, e2, pos) -> 
		let t1 = traverse_expr e1 type_env var_env in 
		let t2 = traverse_expr e2 type_env var_env in (
		try 
			match b with
			| BopADD 	-> lookup_type binop_add (t1, t2) 
			| BopSUB 	-> lookup_type binop_sub (t1, t2)
			| BopMUL 	-> lookup_type binop_mul (t1, t2)
			| BopDIV 	-> lookup_type binop_div (t1, t2)
			| BopMOD 	-> lookup_type binop_mod (t1, t2)
			| BopLT	 	-> lookup_type binop_lt (t1, t2)
			| BopGE 	-> lookup_type binop_ge (t1, t2)
			| BopLTE 	-> lookup_type binop_lte (t1, t2)
			| BopGTE 	-> lookup_type binop_gte (t1, t2)
			| BopSLT 	-> lookup_type binop_slt (t1, t2)
			| BopSRT 	-> lookup_type binop_srt (t1, t2)
			
			| BopEQ -> (
				(* Printf.printf "t1: %s   t2: %s\n" (pprint_typ t1) (pprint_typ t2); *)
				match (t1, t2) with
				| (Pointer(_), Pointer(_)) -> Int
				| (Pointer(_), Int) -> Int
				| (Int, Pointer(_)) -> Int
				| _ -> lookup_type binop_eq (t1, t2)
			)
			| BopNEQ -> (
				match (t1, t2) with
				| (Pointer(_), Pointer(_)) -> Int
				| (Pointer(_), Int) -> Int
				| (Int, Pointer(_)) -> Int
				| _ -> lookup_type binop_eq (t1, t2)
			)

			| BopCOMBAND 	-> lookup_type binop_comband (t1, t2)
			| BopCOMBOR 	-> lookup_type binop_combor (t1, t2)
			| BopBITAND 	-> lookup_type binop_bitand (t1, t2)
			| BopBITOR 	-> lookup_type binop_bitor (t1, t2)
		with 
		| PassAlongError(msg) -> raise (TypeCheckError (msg, pos))
		)

	| EUnOp(uop, e1, pos) ->
		let t1 = traverse_expr e1 type_env var_env in (
		try 
			match uop with
			| UopNegate -> lookup_type uop_negate t1
			| UopBitNot -> lookup_type uop_bitnot t1
			| UopMinus -> lookup_type uop_minus t1
		with 
		| PassAlongError(msg) -> raise (TypeCheckError (msg, pos))
		)

	| ECall(r, e_lst, pos) ->
		if env_contains type_env r then 
			let ft = env_get_type type_env r in
			let ret = get_return_type ft in 
			let t1 = traverse_expr_args e_lst ret type_env var_env in
			(* Printf.printf "ECALL expected type ft: %s   ret t1 type: %s\n" (pprint_typ ft) (pprint_typ t1); *)
			if compare_types t1 ft then (
				(* Printf.printf "ECALL return: %s\n" (pprint_typ ret); *)
				(ret)
			)
			else raise (TypeCheckError ("ECall function types not matching.", pos))
		else 
			if env_contains var_env r then
				let ft = env_get_type var_env r in
				let ret = get_return_type ft in 
				let t1 = traverse_expr_args e_lst ret type_env var_env in
				(* Printf.printf "ECALL expected type ft: \n%s   \nret t1 type: \n%s\n" (pprint_typ ft) (pprint_typ t1); *)
				if compare_types t1 ft then (
					(* Printf.printf "ECALL return: %s\n" (pprint_typ ret); *)
					(ret)
				)
				else raise (TypeCheckError ("ECall function types not matching.", pos))
			else raise (NameAnalysisError ("ECall undefined and undeclared.", pos))

	| ENew(t, e1, pos) -> 
		let rt = get_type t type_env in
		let rt1 = traverse_expr e1 type_env var_env in
		(* Printf.printf "ENEW rt: %s   rt1: %s\n" (pprint_typ rt) (pprint_typ rt1); *)
		if rt1 = Int then Pointer(rt)
		else raise (TypeCheckError ("Index in new construct is not an Integer.", pos))

	| EArrayAccess(r, e1, r_opt, pos) -> 
		(* Printf.printf "EARRAYACCESS \ntype_env: %s\n   var_env: %s\n" (pprint_env type_env) (pprint_env var_env); *)
		if env_contains var_env r then
			let rt = (env_get_type var_env r) in 
			let rt1 = traverse_expr e1 type_env var_env in
			if rt1 = Int then
				( match r_opt with
				| Some(member) -> 
					let bt = iterate_types rt type_env in
					(* Printf.printf "EARRAYACCCESS R_OPT(): \nbt: %s   \nrt: %s\n" (pprint_typ bt) (pprint_typ rt); *)
						( match bt with 
						| Struct(_, sym_lst) -> 
							if env_contains sym_lst member then 
								let memt = env_get_type sym_lst member in 
								(* Printf.printf "EARRAYACCCESS R_OPT(): \nmemt: %s\n" (pprint_typ memt); *)
								(memt)
							else raise (NameAnalysisError ("Struct member does not exist.", pos))
						| _ -> raise (NameAnalysisError ("base type is not a struct.", pos))
						)
				| None -> 
					match rt with
					| Pointer(p) -> 
						(* Printf.printf "EArrayAccess NONE: ret: %s  p: %s\n" (pprint_typ rt) (pprint_typ p); *)
						(p)
					| _ -> raise (TypeCheckError ("EArrayAccess tried to dereference a non-pointer type.", pos))
				)
			else raise (TypeCheckError ("Index in array is not an Integer.", pos))
		else raise (NameAnalysisError ("EArrayAccess to undeclared variable.", pos))
		
(* traverse_expr_args():
 *	Recursive descent to bottom then start building line of arrows
 *	Arrow(Int, Arrow(Int, Int) on way up.
 *)
and traverse_expr_args e_lst ret type_env var_env =
	match e_lst with	
	| [] -> ret
	| p :: ps -> 
		let tn = traverse_expr_args ps ret type_env var_env in
		let tp = traverse_expr p type_env var_env in
		(* Printf.printf "expr_args: tp: %s   tn: %s\n" (pprint_typ tp) (pprint_typ tn); *)
		(Arrow(tp, tn))

			
let traverse_expr_opt e_opt type_env var_env =
	match e_opt with
	| Some(e) -> traverse_expr e type_env var_env
	| None -> Void



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
		let rt = traverse_expr e type_env var_env in
		(type_env, var_env, rt, false)
	
	| SVarDef(t, r, e, pos) -> 
		(* Printf.printf "SVarDef()\n"; *)
		if (env_contains var_env r) = false then 
			let pt = get_type t type_env in
			let var_env2 = Symbol(r, pt) :: var_env in
			let rt = traverse_expr e type_env var_env2 in
			(* Printf.printf "\npt: %s \nrt: %s\n" (pprint_typ pt) (pprint_typ rt); *)
			if pt = rt then 
				(type_env, var_env2, rt, false)
			else raise (TypeCheckError ("Type of variable defined does not match.", pos))
		else raise (NameAnalysisError ("SVarDef already exists.", pos))

	| SVarAssign(r, e, pos) -> 
		(* Printf.printf "SVarAssign hello: \n"; *)
		if (env_contains var_env r) = true then
			let rt = traverse_expr e type_env var_env in
			(* Printf.printf "SVarAssign(): \nrt: %s\n" (pprint_typ rt); *)
			(type_env, var_env, rt, false)
		else raise (NameAnalysisError ("SVarAssign to undeclared variable.", pos))

	| SArrayAssign(r, e1, r_opt, e2, pos) -> 
		(* Printf.printf "SArrayAssign.\n"; *)
		if (env_contains var_env r) = true then 
			let rt1 = traverse_expr e1 type_env var_env in 
			if rt1 = Int then
				(
				match r_opt with
				| Some(member) ->
					let r_type = env_get_type var_env r in
					let bt = iterate_types r_type type_env in (
					(* Printf.printf "SArrayAssign(): \nbt: %s   \nr_type: %s\n" (pprint_typ bt) (pprint_typ r_type); *)
						match bt with 
						| Struct(_, sym_lst) -> 
							if env_contains sym_lst member then 
								let memt = env_get_type sym_lst member in
								let rt2 = traverse_expr e2 type_env var_env in
								(* Printf.printf "SArrayAssign(): \nmemt: %s   \nrt2: %s\n" (pprint_typ memt) (pprint_typ rt2); *)
								match memt with
								| Pointer(_) -> 
									if (memt = rt2) || (rt2 = Int) then
										(type_env, var_env, rt2, false)
									else 
									raise (TypeCheckError ("Type of struct member does not match.", pos))
								| _ -> 	
									if memt = rt2 then
										(type_env, var_env, rt2, false)
									else 
									raise (TypeCheckError ("Type of struct member does not match.", pos))
							else raise (NameAnalysisError ("Struct member does not exist.", pos))
						| _ -> raise (NameAnalysisError ("base type is not a struct.", pos))
					)
				| None -> 
					let rt2 = traverse_expr e2 type_env var_env in 
					(type_env, var_env, rt2, false)
				)	
			else raise (TypeCheckError ("Array Index is not an Integer.", pos))
		else raise (NameAnalysisError ("SArrayAssign to undeclared variable.", pos))

	| SScope(s_lst) -> 
		(* Printf.printf "SScope() FIRST enter.\n"; *)
		let (type_env2, var_env2, rt, found) = traverse_stmt_list s_lst type_env var_env false in 
		(* Printf.printf "SScop() FIRST exit: \nrt: %s\n" (pprint_typ rt); *)
			(type_env, var_env, rt, found)

	| SIf(e, s, else_opt, pos) ->
		(* Printf.printf "SIF enter:\n"; *)
		let rt1 = traverse_expr e type_env var_env in
		let (type_env2, var_env2, rt2, return_exists) = traverse_stmt s type_env var_env in (
		(* Printf.printf "SIF mid: \nrt1: %s   \nrt2: %s\n" (pprint_typ rt1) (pprint_typ rt2); *)
		match else_opt with
		| Some(s) -> 
			let (type_env3, var_env3, rt3, return_exists_else) = traverse_stmt s type_env var_env in
			(* Printf.printf "SIF ELSE exit(): \nrt2: %s   \nrt3: %s\n" (pprint_typ rt2) (pprint_typ rt3); *)
			if return_exists && return_exists_else then
					if rt2 = rt3 then (type_env3, var_env3, rt3, true)
					else raise (TypeCheckError ("If and else cases does not have matching return types.", pos))
			else (type_env2, var_env2, Void, false)
			
		| None -> 
			(* Printf.printf "SIF exit(): \nrt2: %s\n" (pprint_typ rt2); *)
			if return_exists then (type_env2, var_env2, rt2, true)
			else (type_env2, var_env2, Void, false)
		)

	| SWhile(e, s) -> 
		let rt = traverse_expr e type_env var_env in
		let (type_env2, var_env2, rt2, return_exists) = traverse_stmt s type_env var_env in
		(type_env2, var_env2, rt2, return_exists)

	| SBreak -> 
		(* Printf.printf "BREAK: \n";  *)
		(type_env, var_env, Void, false)

	| SReturn(e_opt) -> 
		(* Printf.printf "SReturn enter: \n";  *)
		let rt = traverse_expr_opt e_opt type_env var_env in
		(* Printf.printf "SReturn exit: rt = %s\n" (pprint_typ rt);  *)
		(type_env, var_env, rt, true)

	| SDelete(r, pos) ->
		if env_contains var_env r then (type_env, var_env, Void, false)
		else raise (NameAnalysisError ("SDelete undeclared variable.", pos))

and traverse_stmt_list s_lst type_env var_env found = 
	match s_lst with
	| [] -> (type_env, var_env, Void, false)
	| s :: rs -> 
		(* Printf.printf "STMT LIST NORMAL CASE: \n" ;  *)
		let (type_env2, var_env2, rt1, found1) = traverse_stmt s type_env var_env in
		let (type_env3, var_env3, rt2, found2) = traverse_stmt_list rs type_env2 var_env2 found in
		(* Printf.printf "STMT LIST rt1: %s found1: %b\n" (pprint_typ rt1) (found1);  *)
		(* Printf.printf "STMT LIST rt2: %s found2: %b\n" (pprint_typ rt2) (found2);  *)
		if found1 = true then
			(type_env3, var_env3, rt1, true)
		else 
			if found2 = true then (type_env3, var_env3, rt2, true)
			else (type_env3, var_env3, Void, false)
			


let rec add_params_to_env var_env param_env =
	match param_env with
	| [] -> var_env
	| hd :: rs -> 
		add_params_to_env (hd :: var_env) rs


let rec get_type_param t type_env =
	match t with
	| TInt -> Int
	| TChar -> Char
	| TVoid -> Void
	| TIdent(r) ->  
			if (env_contains type_env r) then Ident(r)
			else raise (PassAlongError "Param type does not exist.")
	| TPoint(p) -> Pointer(get_type_param p type_env) 

(* traverse_param():
 *	Adds valid param to env. Otherwise throw error.
 *	If param already exists in env then also throw error.
 *)
let rec traverse_param p type_env var_env =
	match p with
	| Param(t,r) ->
		if ((env_contains var_env r) = false) then
			let nt = get_type_param t type_env in
			(nt, Symbol(r, (get_type_param t type_env)) :: var_env)
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
				let pt = get_type t type_env in 
				let (fpt, param_env) = traverse_params_list p_lst pt type_env [] in
				let var_env2 = add_params_to_env var_env param_env in
				let (_, _, rt, _) = traverse_stmt s type_env (Symbol(r, fpt) :: var_env2) in
				(* Printf.printf "param_env: %s\n  var_env %s\n" (pprint_env param_env) (pprint_env var_env2); *)
				(* Printf.printf "expected type: %s   ret type: %s\n" (pprint_typ pt) (pprint_typ rt); *)
				if pt = rt then
					let var_env3 = (Symbol(r, fpt) :: var_env) in
					if (env_contains type_env r) then
						let ot = env_get_type type_env r in
						if ot = fpt then (type_env, var_env3)
						else raise (NameAnalysisError ("Function definition does not match declaration.", pos))
					else (type_env, var_env3)
				else raise (TypeCheckError ("Variable does not match type of expression.", pos))
			else raise (NameAnalysisError ("Function defined already exist.", pos))
		with
		| PassAlongError(msg) -> raise (NameAnalysisError(msg, pos))
		)
		

	(* Currently only allows 1 time declaration of function. *)
	| GFuncDecl(t, r, p_lst, pos) -> (
		try
			(* Printf.printf "GFuncDecl()\n"; *)
			if (env_contains type_env r) = false then
				let (pt, _) = traverse_params_list p_lst (get_type t type_env) type_env [] in
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


	| GVarDef(t, r, e, pos) -> 
		(* Printf.printf "GVarDef()\n"; *)
		if (env_contains var_env r) = false then
			let pt = (get_type t type_env) in
			let var_env2 = (Symbol(r, pt) :: var_env) in
			let rt = traverse_expr e type_env var_env2 in
			(* Printf.printf "expected type: %s   ret type: %s\n" (pprint_typ pt) (pprint_typ rt); *)
			if pt = rt then
				if (env_contains type_env r) then
					let ot = env_get_type type_env r in
					if ot = pt then (type_env, var_env2)
					else raise (NameAnalysisError ("Global Variable definition does not match declaration.", pos))
				else 
				(type_env, var_env2)
			else raise (TypeCheckError ("Variable does not match type of expression.", pos))
		else raise (NameAnalysisError ("Global Variable defined already exist.", pos))
			

	| GVarDecl(t,r, pos) -> 
		(* Printf.printf "GVarDecl()\n"; *)
		if (env_contains type_env r) = false then
			let pt = (get_type t type_env) in
			let type_env2 = (Symbol(r, pt) :: type_env) in
			let var_env2 = (Symbol(r, pt) :: var_env) in
			if (env_contains var_env r) then 
				let ot = env_get_type var_env r in
				if ot = pt then (type_env2, var_env)
				else raise (NameAnalysisError ("Global Variable declaration does not match definition.", pos))
			else (type_env2, var_env2)
		else raise (NameAnalysisError ("Global Variable already declared exist.", pos))
		

	| GStruct(r, p_lst, pos) -> (
		try
			(* Printf.printf "GStruct()\n"; *)
			if ((env_contains type_env r) = false) && ((env_contains var_env r) = false) then 
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

let typeCheck tree line_error =
	try 
		(* Printf.printf "name analysis.\n" ; *)
		match tree with
		| Prog(g_lst) ->
			traverse_global_list g_lst [] []
		| _ -> 
			raise (ProgramError "No program given.")
	with
	| ProgramError(msg)		-> Printf.fprintf stderr "%s\n" msg; exit 2 
	| NameAnalysisError(msg, pos) 	-> 
		if line_error = true then (
			Printf.fprintf stderr "%d\n" pos.pos_lnum; 
			exit 2 
		)
		else (
			Printf.fprintf stderr "%s\n" msg; 
			exit 2 
		)
	| TypeCheckError(msg, pos)		-> 
		if line_error = true then (
			Printf.fprintf stderr "%d\n" pos.pos_lnum; 
			exit 2 
		)
		else (
			Printf.fprintf stderr "%s\n" msg; 
			exit 2 
		)
