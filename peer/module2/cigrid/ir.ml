open Printf
open Ast

(* type sym = string (applied to EVar())	*)

type ir_stmt =
	| IRSExpr	of expr
	| IRSVarAssign	of string * expr
	| IRSVarDecl	of string * ty


type ir_blockend =
	| IRSReturn	of expr option
	| IRSBranch	of expr * string * string
	| IRSJump	of string
	
type ir_block = | IRBlock of string * (ir_stmt list * ir_blockend)

type ir_global =
	| IRFunc of string * (ty * params list * ir_block list)



let block_count = ref 0
let unique () =
	let n = !block_count in
	incr block_count;
	Printf.sprintf "v%d" n



let rec cfg_stmts cur_block next_block breakto_block acc_stmts acc_blocks ast_lst =
	match ast_lst with
	| SExpr(e)::xs ->
		cfg_stmts 
			cur_block 
			next_block 
			breakto_block
			(IRSExpr(e)::acc_stmts) 
			acc_blocks 
			xs
	
	| SVarDef(t,r,e,_)::xs ->
		cfg_stmts 
			cur_block 
			next_block 
			breakto_block
			(IRSVarAssign(r,e)::IRSVarDecl(r,t)::acc_stmts) 
			acc_blocks
			xs

	 | SVarAssign(r,e,_)::xs ->
		cfg_stmts
			cur_block
			next_block
			breakto_block
			(IRSVarAssign(r,e)::acc_stmts)
			acc_blocks
			xs

	(* SArrayAssign(r, e1, r_opt, e2, _) *)
		
	| SScope(s_lst)::xs -> 
		cfg_stmts
			cur_block
			next_block
			breakto_block
			acc_stmts
			acc_blocks
			(List.append s_lst xs)

	| SIf(e, s, None, _)::xs -> 
		let b_true 	= (unique ()) in
		let b_after 	= (unique ()) in
		let block_before = 
			IRBlock(cur_block, 
				((List.rev acc_stmts), IRSBranch(e, b_true, b_after))) in
		let block_lst2 = 
			cfg_stmts b_true (Some(b_after)) breakto_block
					[] (block_before::acc_blocks) [s] in
		cfg_stmts
			b_after 
			next_block 
			breakto_block
			[] 
			block_lst2 
			xs

	| SIf(e, s, Some(e_else), _)::xs -> 
		let b_true = (unique ()) in
		let b_else = (unique ()) in
		let b_after = (unique ()) in
		let block_before = 
			IRBlock(cur_block, ((List.rev acc_stmts), IRSBranch(e, b_true, b_else))) in
		let block_lst2 =
			cfg_stmts b_true (Some(b_after)) breakto_block [] 
				(block_before::acc_blocks) [s] in
		let block_lst3 = 
			cfg_stmts b_else (Some(b_after)) breakto_block [] 
				(block_before::acc_blocks) [SExpr(e)] in
		cfg_stmts
			b_after
			next_block
			None
			[]
			(List.append block_lst3 block_lst2)
			xs
			
			
	| SWhile(e, s)::xs -> 
		let b_cond = (unique ()) in
		let b_body = (unique ()) in
		let b_after = (unique ()) in
		let block_before = 
			IRBlock(cur_block, 
				((List.rev acc_stmts), IRSJump(b_cond))) in
		let block_condition = 
			IRBlock(b_cond, 
				([], IRSBranch(e, b_body, b_after))) in
		let block_lst2 = 		
			cfg_stmts b_body (Some(b_cond)) (Some(b_after)) [] 
				(block_condition::block_before::acc_blocks) [s] in
		cfg_stmts
			b_after 
			next_block 
			breakto_block
			[] 
			block_lst2 
			xs


	| SBreak::xs -> 
		(match breakto_block with
		| Some(b) -> 
			let block_before = 
				IRBlock(cur_block, 
				((List.rev acc_stmts), IRSJump(b))) in
			(block_before::acc_blocks)
		)
		

	| SReturn(e_opt)::xs -> 
		IRBlock(cur_block, (List.rev acc_stmts, IRSReturn(e_opt)))::acc_blocks

	(* | SDelete(r, _) ->  *)
	
	| [] -> 
		let blockend = 
			(match next_block with
			| None -> IRSReturn(None)
			| Some(b2) -> 	
				IRSJump(b2)
			) 
		in
		IRBlock(cur_block, (List.rev acc_stmts, blockend))::acc_blocks
	

let ir_global ast =
	match ast with
	| GFuncDef(t,r,p_lst,s,_) -> 
		let block_lst = cfg_stmts (unique ()) (Some(unique ())) None [] [] [s] in
		IRFunc(r, (t, p_lst, List.rev(block_lst)))

	(*
	| GFuncDecl(t,r,p_lst,_) -> 
	| GVarDef(t,r,e,_) -> 
	| GVarDecl(t,r,_) -> 
	| GStruct(r, p_lst,_) -> 
	*)

let rec ir_global_list ast acc_ir =
	match ast with
	| [] -> List.rev(acc_ir)
	| hd :: rs -> 
		let g = ir_global hd in
		ir_global_list rs (g::acc_ir)

let ir_program ast = 
	match ast with
	| Prog(g_lst) -> ir_global_list g_lst []


(* ========================================================== *)
(*	PRETTY PRINTER					      *)
(* ========================================================== *)
let pprint_ir_stmt stmt =
	match stmt with
	| IRSExpr(e) -> 
		"IRSExpr(" ^ pprint_expr e ^ ")"
	| IRSVarAssign(r, e) -> 
		"IRSVarAssign(" ^ r ^ ", " ^ pprint_expr e ^ ")"
	| IRSVarDecl(r,t) -> 
		"IRSVarDecl(" ^ r ^ ", " ^ pprint_ty t ^ ")"

let rec pprint_ir_stmt_list acc_stmts indent =
	match acc_stmts with
	| [] -> ""
	| hd :: rs -> 
		"\n" ^ indent ^ (pprint_ir_stmt hd) ^ (pprint_ir_stmt_list rs indent)


let pprint_ir_blockend blockend indent = 
	match blockend with
	| IRSReturn(e_opt) -> 
		"IRSReturn(" ^ (pprint_expr_option e_opt) ^ ")"
	| IRSBranch(e, r1, r2) -> 
		"IRSBranch(" ^ pprint_expr e ^ ", " ^ r1 ^ ", " ^ r2 ^ ")"
	| IRSJump(r) -> 
		"IRSJump(" ^ r ^ ")"


let rec pprint_ir_block block indent = 
	match block with
	| IRBlock(cur_block, (acc_stmts, blockend)) -> 
		"IRBlock({" ^ cur_block ^ "," ^ indent ^ 
		(pprint_ir_stmt_list acc_stmts (indent ^ "  ")) ^ "\n" ^ indent ^
		(pprint_ir_blockend blockend (indent ^ "  ")) ^ "\n" ^ indent ^ "})\n"


let rec pprint_ir_block_list block_lst indent = 
	match block_lst with
	| [] -> ""
	| hd :: rs -> "\n" ^ indent ^ (pprint_ir_block hd (indent ^ "  ")) 
			^ (pprint_ir_block_list rs indent)

let pprint_ir_global ir_g =
	match ir_g with
	| IRFunc(r, (t, p_lst, block_lst)) ->
		"IRFunc(" ^ (pprint_ty t) ^ ", " ^ r ^ ", {" ^ pprint_params_list p_lst ^ "}, {" ^
		(pprint_ir_block_list block_lst "  ") ^ "})\n"


let rec pprint_ir ir =
	match ir with
	| [] -> ""
	| [hd] -> pprint_ir_global hd
	| hd :: rs -> pprint_ir_global hd ^ "\n" ^ pprint_ir rs



let ir_main ast =
	let ir = ir_program ast in
	let str = pprint_ir ir in
	(ir, str)



