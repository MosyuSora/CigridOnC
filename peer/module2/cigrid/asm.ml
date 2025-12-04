open Printf
open Ir
open Ast

exception AsmException of string

type bitsize =
	| Byte
	| Word
	| DWord
	| QWord

type displacement = int
type scale = int
type reg = int * bitsize

type unop = 
	| Inc
	| Dec
	| Push
	| Pop
	| IMul
	| IDiv
	| Not
	| Neg
	| Setg
	| Setl
	| Setge
	| Setle
	| Sete
	| Setne

type binop =
	| Add
	| Sub
	| Cmp
	| Mov
	| And
	| Or
	| Xor

type op = 
	| Imm of int
	| Reg of reg
	| TReg of reg * string * int (* temporary register *)
	| Mem of bitsize * reg * reg option * scale * displacement
	| NoOp

type jbinop =
	| Jl
	| Jg
	| Jle
	| Jge
	| Je
	| Jne

type inst =
	| UnOp of unop * op
	| BinOp of binop * op * op
	| Call of string
	| Cqo	(* sign extension *)

type blockend =
	| Ret
	| Jmp of string
	| JBinOp of jbinop * string * string

type block = 
	Block of string * (inst list * blockend)

type func = 
	Func of string * block list

(* ========================================================== *)
(*	PRETTY PRINTER					      *)
(* ========================================================== *)
let pprint_asm_register (i, size) = 
	match i with
	| 0 -> "rax"
	| 1 -> "rcx"
	| 2 -> "rdx"
	| 3 -> "rbx"
	| 4 -> "rsp"
	| 5 -> "rbp"
	| 6 -> "rsi"
	| 7 -> "rdi"
	| 8 -> "r8"
	| 9 -> "r9"
	| 10 -> "r10"
	| 11 -> "r11"
	| 12 -> "r12"
	| 13 -> "r13"
	| 14 -> "r14"
	| 15 -> "r15"

let pprint_asm_bitsize = function
	| QWord -> "qword"
	| DWord -> "dword"
	| Word -> "word"
	| Byte -> "byte"

let pprint_asm_binop = function
	| Add -> "add"
	| Sub -> "sub"
	| Cmp -> "cmp"
	| Mov -> "mov"
	| And -> "and"
	| Or  -> "or"
	| Xor -> "xor"

let rec pprint_asm_op = function
	| Imm(i) -> (string_of_int i) 
	| TReg(reg, r, n) -> r 
	| Reg(i, size) -> pprint_asm_register (i, size)
	| Mem(size, r1, None, sc, disp) -> 
			pprint_asm_bitsize size ^ 
			" [" ^ (pprint_asm_register r1) ^ " + " ^ (string_of_int (sc * disp)) ^ "]"

let rec pprint_asm_instr instr indent =
	match instr with
	| BinOp(bop, op1, op2) -> 
		pprint_asm_binop bop ^ " " ^ pprint_asm_op op1 ^ ", " ^ pprint_asm_op op2


let rec pprint_asm_instr_list instr_lst indent =
	match instr_lst with
	| [] -> ""
	| hd :: rs -> 
		"\n" ^ indent ^ (pprint_asm_instr hd indent) ^ (pprint_asm_instr_list rs indent)

let rec pprint_asm_b_end b_end indent = 
	match b_end with
	| Ret -> "ret"
	| Jmp(r) -> "jmp " ^ r

let rec pprint_asm_block b indent =
	match b with
	| Block(r, (instr_lst, b_end)) -> 
		"\n" ^ (pprint_asm_instr_list instr_lst indent) ^ 
		"\n" ^ indent ^ (pprint_asm_b_end b_end indent)

let rec pprint_asm_block_list b_lst indent = 
	match b_lst with
	| [] -> ""
	| hd :: rs -> 
		(pprint_asm_block hd indent) ^ "\n" ^ 
		indent ^ (pprint_asm_block_list rs indent)

let rec pprint_asm_func func = 
	match func with
	| Func(r, b_lst) -> 
		"global main\n" ^ "section .text\n" ^ r ^ ":" ^ (pprint_asm_block_list b_lst "  ")

let rec pprint_asm asm =
	match asm with
	| [] -> ""
	| [hd] -> pprint_asm_func hd
	| hd :: rs -> 
		pprint_asm_func hd ^ ", " ^ pprint_asm rs

(* ========================================================== *)
(*	ASM IR GENERATION				      *)
(* ========================================================== *)

let temp_reg_count = ref 0

let create_temp_reg () =
	let n = !temp_reg_count in
	incr temp_reg_count;
	Printf.sprintf "t%d" n

let get_count () =
	!temp_reg_count

let incr_count () =
	let n = !temp_reg_count in
	incr temp_reg_count;
	(n)


let match_bitsize = function
	| TInt -> QWord
	| TChar -> Byte

let match_binop = function
	| BopADD -> Add
	| BopSUB -> Sub

(* env_get():
 *	iterates list until it finds match.
 *	Location can be Register or Memory location.
 *)
let rec env_get r env =
	match env with
	| [] -> raise (AsmException "could not find variable in env.")
	| (r2, location)::rs -> 
		if r = r2 then location
		else env_get r rs
	
(* maximal_munch():
 *	Algorithm 
 *)
let rec maximal_munch e acc env =
	match e with
	| EInt(i, _) -> 
		let tr = TReg((4, QWord), (create_temp_reg ()), (get_count ())) in
		let instr = BinOp(Mov, tr, Imm(i)) in
		(tr, instr::acc) 
	
	| EVar(r, _) -> 
		let tr = env_get r env in
		(tr, acc)

	| EBinOp(bop, e1, e2, _) -> 
		let (op1, acc_instr1) = maximal_munch e1 acc env in
		let (op2, acc_instr2) = maximal_munch e2 acc env in
		let tr = TReg((4, QWord), (create_temp_reg ()), (get_count ())) in
		let acc_instr3 = List.append acc_instr2 acc_instr1 in
		let acc_instr4 = BinOp(match_binop bop, tr, op2)::BinOp(Mov, tr, op1)::acc_instr3 in
		(tr, acc_instr4)
		

(* ir_stmt():
 *	Recursively convert statements.
 *	IRSVarAssign() adds new list of instructions.
 *	IRSVarDecl() adds new variables in env.
 *)
let ir_stmt stmt curt acc_instr env = 
	match stmt with
	(* 
	| IRSExpr(e) 
	*)
	| IRSVarAssign(r, e) -> 
		let (tr, instr) = maximal_munch e [] env in
		let tv = env_get r env in
		let instr2 = (BinOp(Mov, tv, tr)::instr) in
		(tr, (List.append instr2 acc_instr), env)
		
	| IRSVarDecl(r,t) -> 
		let env2 = (r, TReg((4, QWord), r, incr_count ()))::env in
		(curt, acc_instr, env2)

(* ir_stmts_list():
 *	Iterates through list of statements and passes updated values.
 *)
let rec ir_stmts_list stmts curt acc_instr env =
	match stmts with
	| [] -> 
		(curt, acc_instr, env)
	| hd :: rs -> 
		let (curt2, acc_instr2, env2) = ir_stmt hd curt acc_instr env in
		ir_stmts_list rs curt2 acc_instr2 env2


(* ir_blockend():
 *	Dissects the IR blockend and converts it into ASM IR blockend.
 *	IRSReturn(e) move variable to RAX register and return RET and updated list.
 *	IRSREturn() return RET keyword
 *)
let ir_blockend blockend curt acc_instr env =
	match blockend with
	| IRSReturn(Some(e)) -> 
		let (tr, acc_instr2) = maximal_munch e acc_instr env in
		let acc_instr3 = BinOp(Mov, Reg(0, QWord), tr)::acc_instr2 in
		(Ret, acc_instr3)

	| IRSReturn(None) -> 
		(Ret, acc_instr)

	(*
	| IRSBranch(e, r1, r2) -> 
	
	| IRSJump(r) -> 
	*)
	

(* ir_block():
 *	Dissects the IR block and recursively calls on each part of block.
 *	Reconstruct new ASM IR block.
 *)
let rec ir_block block acc =
	match block with
	| IRBlock(cur_block, (stmts, blockend)) -> 
		let (curt, acc_instr, env) = ir_stmts_list stmts NoOp [] [] in
		let (blockend_instr, acc_instr2) = ir_blockend blockend curt acc_instr env in
		(* Printf.printf "BLOCK: acc_instr2: %s\n" (pprint_asm_instr_list acc_instr2 "") ; *)
		Block(cur_block, (List.rev acc_instr2, blockend_instr))


let rec ir_block_list block_lst acc =
	match block_lst with
	| [] -> acc
	| hd :: rs -> 
		let b = ir_block hd acc in
		ir_block_list rs (b::acc)


let rec ir_global ir acc =
	match ir with
	| IRFunc(r, (t, p_lst, block_lst)) ->
		let b_lst = ir_block_list block_lst [] in
		Func(r, List.rev b_lst) :: acc
		

let rec ir_global_list ir acc =
	match ir with
	| [] -> List.rev acc
	| hd :: rs -> 
		let acc2 = ir_global hd acc in
		ir_global_list rs acc2

let generateAsm ir acc =
	ir_global_list ir acc




(* ========================================================== *)
(*	REGISTER SPILLING 				      *)
(* ========================================================== *)
let convertToStack (i, size) n =
		Mem(size, (4, size), None, 8, n)

let rec traverse_op op =
	match op with
	| Imm(i) -> Imm(i)
	| TReg(reg, r, n) -> convertToStack reg n
	| Reg(i, size) -> Reg(i, size)

(* traverse_instr():
 *	Convert temp registers to stack memory locations.
 *	If binop with 2 temp registers then introduce r10 register
 *	so that we do not have 2 memory locations.
 *)
let rec traverse_instr instr =
	match instr with
	| BinOp(bop, op1, op2) -> 
		let x1 = traverse_op op1 in
		let x2 = traverse_op op2 in
		match (x1, x2) with
		| (Mem(size1, reg1, None, sc1, n1), 
		   Mem(size2, reg2, None, sc2, n2)) -> 
			[
				BinOp(bop, x1, Reg(10, QWord)) ;
				BinOp(Mov, Reg(10, QWord), x2) 
			]
		| _ -> 
			[ BinOp(bop, x1, x2) ]

let rec traverse_instr_list instr_lst acc =
	match instr_lst with
	| [] -> acc
	| hd :: rs -> 
		let lst = traverse_instr hd in
		traverse_instr_list rs (List.append lst acc)
		

let rec traverse_block b acc =
	match b with
	| Block(r, (instr_lst, b_end)) -> 
		let acc2 = traverse_instr_list instr_lst [] in
		let acc3 = List.rev (BinOp(Add, Reg(4, QWord), 
				Imm((get_count ()) * 8))::acc2) in
		let acc4 = BinOp(Sub, Reg(4, QWord), Imm((get_count ()) * 8))::acc3 in
		Block(r, (acc4, b_end))

let rec traverse_block_list b_lst bacc = 
	match b_lst with
	| [] -> bacc
	| hd :: rs -> 
		let b = traverse_block hd bacc in
		traverse_block_list rs (b::bacc)
		

let rec traverse_func func = 
	match func with
	| Func(r, b_lst) -> 
		let bacc = traverse_block_list b_lst [] in
		Func(r, List.rev bacc)

let rec traverse_func_list asm facc =
	match asm with
	| [] -> facc
	| hd :: rs -> 
		let f1 = traverse_func hd in
		traverse_func_list rs (f1::facc)

let register_spill asm = 
	traverse_func_list asm []
	

let asmMain ir =
		let asm = generateAsm ir [] in
		let asm_sp = register_spill asm in
		(* Printf.printf "%s\n" (pprint_asm asm) ; *)
		let str = pprint_asm asm_sp in
		(asm_sp, str)
