%{
	open Ast
%}


%token <int> 	INT
%token <char> 	CHAR
%token <string> IDENT
%token <string> STRING

%token NEGATE "!"
%token TILDE "~"

%token ADD "+"
%token SUB "-"
%token DIV "/"
%token MOD "%"
%token LT  "<"
%token GE  ">"
%token LTE "<="
%token GTE ">="
%token SLT "<<"
%token SRT ">>"
%token EQ  "=="
%token NEQ "!="
%token COMBAND "&&"
%token COMBOR  "||"
%token BITAND  "&"
%token BITOR   "|"	

%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token LCURLY	"{"
%token RCURLY	"}"
%token SEMICOLON ";"
%token COMMA ","

%token BREAK
%token EXTERN
%token NEW
%token WHILE
%token FOR
%token RETURN
%token DELETE
%token IF
%token STRUCT
%token ELSE

%token TCHAR
%token TINT 
%token TVOID

%token STAR "*"
%token DOT "."
%token INC "++"
%token DEC "--"

%token ASSIGN "="
%token EOF

%start program
%type <program> program

%left "||"
%left "&&"
%left "|"
%left "&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%right "!" "~" UMINUS

%%

global_lst:
	| { [] }
	| g=global rs=global_lst { g :: rs }

program:
	| g_lst = global_lst EOF
		{ Prog(g_lst) }

ty:	
	| TVOID 	{ TVoid }
	| TINT  	{ TInt }
	| TCHAR 	{ TChar }
	| r=IDENT	{ TIdent(r) }
	| t=ty "*" { TPoint(t) }

(* Represents the following grammar: 	*)
(* ["." IDENT ] 			*)
ident_option:
	| 	{ None }
	| "." r=IDENT { Some(r) }

(* Represents the following grammar: 	*)
(* ["else" stmt] 			*)
else_option:
	| 	{ None }
	| ELSE s=stmt { Some(s) }

(* This kind of structure represents the grammar: *)
(* 	[ expr { "," expr }]			  *)
expr_lst:
	| 	 { [] }
	| e=expr { [e] }
	| e=expr "," rs=expr_lst { e :: rs }

expr_option:
	| { None }
	| e=expr { Some(e) }

expr:
	| r = IDENT 	{ EVar(r, $symbolstartpos) }
	| i = INT 	{ EInt(i, $symbolstartpos) }
	| c = CHAR	{ EChar(c, $symbolstartpos) }
	| r = STRING	{ EString(r, $symbolstartpos) }

	| e1=expr "+" e2=expr { EBinOp(BopADD, e1, e2, $symbolstartpos) }
	| e1=expr "-" e2=expr { EBinOp(BopSUB, e1, e2, $symbolstartpos) }
	| e1=expr "*" e2=expr { EBinOp(BopMUL, e1, e2, $symbolstartpos) }
	| e1=expr "/" e2=expr { EBinOp(BopDIV, e1, e2, $symbolstartpos) }
	| e1=expr "%" e2=expr { EBinOp(BopMOD, e1, e2, $symbolstartpos) }
	| e1=expr "<" e2=expr { EBinOp(BopLT, e1, e2, $symbolstartpos) }
	| e1=expr ">" e2=expr { EBinOp(BopGE, e1, e2, $symbolstartpos) }
	| e1=expr "<=" e2=expr { EBinOp(BopLTE, e1, e2, $symbolstartpos) }
	| e1=expr ">=" e2=expr { EBinOp(BopGTE, e1, e2, $symbolstartpos) }
	| e1=expr "<<" e2=expr { EBinOp(BopSLT, e1, e2, $symbolstartpos) }
		| e1=expr ">>" e2=expr { EBinOp(BopSRT, e1, e2, $symbolstartpos) }
		| e1=expr "==" e2=expr { EBinOp(BopEQ, e1, e2, $symbolstartpos) }
	| e1=expr "!=" e2=expr { EBinOp(BopNEQ, e1, e2, $symbolstartpos) }
	| e1=expr "&&" e2=expr { EBinOp(BopCOMBAND, e1, e2, $symbolstartpos) }
	| e1=expr "||" e2=expr { EBinOp(BopCOMBOR, e1, e2, $symbolstartpos) }
	| e1=expr "&" e2=expr { EBinOp(BopBITAND, e1, e2, $symbolstartpos) }
	| e1=expr "|" e2=expr { EBinOp(BopBITOR, e1, e2, $symbolstartpos) }

	| "!" e=expr 		  { EUnOp(UopNegate, e, $symbolstartpos) }
	| "~" e=expr 		  { EUnOp(UopBitNot, e, $symbolstartpos) }
	| "-" e=expr %prec UMINUS { EUnOp(UopMinus, e, $symbolstartpos) }

	| r=IDENT "(" e_lst=expr_lst ")" { ECall(r, e_lst, $symbolstartpos) }
	| NEW t=ty "[" e=expr "]" 	 { ENew(t, e, $symbolstartpos) }
	| r=IDENT  "[" e=expr "]" r_opt=ident_option { EArrayAccess(r, e, r_opt, $symbolstartpos) }
	| "(" e=expr ")" { e } 



(* lvalue is baked into assign to more easily include the SArrayAssign() case. *)
assign:
	| r=IDENT "(" e_lst=expr_lst ")" { SExpr(ECall(r, e_lst, $symbolstartpos)) }
	| r=IDENT "=" e=expr		 { SVarAssign(r,e, $symbolstartpos) }
	| r=IDENT "[" e1=expr "]" r_opt=ident_option "=" e2=expr 
		{ SArrayAssign(r, e1, r_opt, e2, $symbolstartpos) }
	| r=IDENT "++"			 { SVarAssign(r, EBinOp(BopADD, EVar(r, $symbolstartpos), EInt(1, $symbolstartpos), $symbolstartpos), $symbolstartpos) }
	| r=IDENT "--"			 { SVarAssign(r, EBinOp(BopSUB, EVar(r, $symbolstartpos), EInt(1, $symbolstartpos), $symbolstartpos), $symbolstartpos) }
	| r=IDENT "[" e=expr "]" r_opt=ident_option "++" 
		{ SArrayAssign(r, e, r_opt, EBinOp(BopADD, EArrayAccess(r, e, r_opt, $symbolstartpos), EInt(1, $symbolstartpos), $symbolstartpos), $symbolstartpos) }
	| r=IDENT "[" e=expr "]" r_opt=ident_option "--" 
		{ SArrayAssign(r, e, r_opt, EBinOp(BopSUB, EArrayAccess(r, e, r_opt, $symbolstartpos), EInt(1, $symbolstartpos), $symbolstartpos), $symbolstartpos) }

varassign:
	| t=ty r=IDENT "=" e=expr { SVarDef(t, r, e, $symbolstartpos) }
	| a=assign { a }

stmt_lst:
	| { [] }
	| s=stmt rs=stmt_lst { s :: rs }

stmt:
	| v=varassign ";" 		{ v }
	| IF "(" e=expr ")" s=stmt else_opt=else_option { SIf(e, s, else_opt, $symbolstartpos) }
	| "{" s_lst=stmt_lst "}"	{ SScope(s_lst) }
	| WHILE "(" e=expr ")" s=stmt 	{ SWhile(e, s) }
	| BREAK	";" 			{ SBreak }
	| RETURN e_opt=expr_option ";" 	{ SReturn(e_opt) }
	| DELETE "[" "]" r=IDENT ";"	{ SDelete(r, $symbolstartpos) }
	| FOR "(" v=varassign ";" e=expr ";" a=assign ")" s=stmt { SScope( [v ; SWhile(e, SScope([s ; a]) )]) }

(* This kind of structure represents the grammar:
 * 	[ ty Ident { "," ty IDENT }]		*)
params_lst:
	| { [] }
	| p=params { [p] } 
	| p=params "," rs=params_lst 	{ p :: rs }
params:
	| t=ty r=IDENT 	   { Param(t, r) }	

(* Almost same as params_lst but should not be intertwined. *)
(* Represents the following: { ty IDENT ";" }		    *)
constructor_lst:
	| { [] }
	| p=constructor rs=constructor_lst { p :: rs }
constructor:
	| t=ty r=IDENT ";" { Param(t, r) }
global:
	| t=ty r=IDENT "(" p_lst=params_lst ")" "{" s_lst=stmt_lst "}" { GFuncDef(t, r, p_lst, SScope(s_lst), $symbolstartpos) }
	| EXTERN t=ty r=IDENT "(" p_lst=params_lst ")" ";" 	{ GFuncDecl(t, r, p_lst, $symbolstartpos) }
	| t=ty r=IDENT "=" e=expr ";" 				{ GVarDef(t, r, e, $symbolstartpos) }
	| EXTERN t=ty r=IDENT ";"				{ GVarDecl(t, r, $symbolstartpos) }
	| STRUCT r=IDENT "{" p_lst=constructor_lst "}" ";" 	{ GStruct(r, p_lst, $symbolstartpos) }

