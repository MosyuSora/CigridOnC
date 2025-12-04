{
	open Parser
	exception Error of char option
}

(* LEXICAL CONVENTIONS 
 * ================================================
 * tokens = IDENTIFIER	TIdent		[_a-zA-Z][_a-zA-Z0-9]*
 *	  | KEYWORD 	S (stmt)	break, extern, new, while, char, 
 *					for, return, delete, if, struct,
 *					else, int, void
 *	  | CONSTANTS			decimal: 0 | [1-9][0-9]*
 *					string: "<string>"
 * 	  | OPERATORS			+ - * / % < > <= >= == !=
 *					& | && || << >>
 *	  | SEPARATORS			() {} [] , ; : 
 *)

let line_comment = "//" [^ '\n']*
(* let multi_line_comment = "/*" [^ '*']* '*'* '/' *)

let includes = "#" [^ '\n']*

let letter 	= ['A'-'Z'] | ['a'-'z']
let digit  	= ['0'-'9']
let non_digit 	= '_' | letter
let ident 	= non_digit (digit | non_digit)*

let integer_constant_dec = ['0'] | ['1'-'9']['0'-'9']*
let integer_constant_hex = '0' ['X' 'x'] ['0'-'9' 'a'-'f' 'A'-'F']+

let character = ['\x20'-'\x7e']
let char_exclude = ['\\' '\'' '\"']
let char_escape = ['\\'] ['n' 't' '\\' '\'' '\"']
let valid_character = (character # char_exclude) | char_escape
let char_constant = '\'' valid_character '\''

let string_constant = '"' (valid_character)* '"'

rule token = parse
	(* COMMENTS and NEWLINE *)
	| [' '] { token lexbuf }
	| ['\t'] {token lexbuf }
	| line_comment { token lexbuf }
	| includes { token lexbuf }

	| "/*" { multiline_comment lexbuf }

	| ['\n'] { Lexing.new_line lexbuf; token lexbuf }


	(* KEYWORDS *) 
	| "break"	{ BREAK }
	| "extern"	{ EXTERN }
	| "new"		{ NEW }
	| "while"	{ WHILE }
	| "char"	{ TCHAR }
	| "for"		{ FOR }
	| "return"	{ RETURN }
	| "delete"	{ DELETE }
	| "if"		{ IF }
	| "struct"	{ STRUCT }
	| "else"	{ ELSE }
	| "int"		{ TINT }
	| "void"	{ TVOID }

	| ['='] { ASSIGN }
	
 	(* CONSTANTS *)
	| integer_constant_dec as n 	{ INT(int_of_string n) }
	| integer_constant_hex as n	{ INT(int_of_string n) }
	| '\'' '\\' 'n'  '\''		{ CHAR('\n') } 
	| '\'' '\\' 't'  '\''		{ CHAR('\t') } 
	| '\'' '\\' '\\' '\''		{ CHAR('\\') } 
	| '\'' '\\' '\'' '\''		{ CHAR('\'') } 
	| '\'' '\\' '\"' '\''		{ CHAR('\"') } 
	| char_constant 		{ CHAR(Lexing.lexeme_char lexbuf 1) }
	| string_constant as str	{ STRING(str) }

	(* IDENTIFIERS *)
	| ident as r { IDENT(r) }

	(* OPERATORS *)
	| ['!'] { NEGATE }
	| ['~'] { TILDE } 
	
	| "++"  { INC }
	| "--"  { DEC }
	| ['.'] { DOT }
	| ['*'] { STAR } (* Can be both pointer or mul *)

	| ['+'] { ADD }
	| ['-'] { SUB }

	| ['/'] { DIV }
	| ['%'] { MOD }
	| "<=" { LTE }
	| ">=" { GTE }
	| "<<" { SLT }
	| ">>" { SRT }
	| ['<'] { LT }
	| ['>'] { GE }
	| "=="	{ EQ }
	| "!="  { NEQ }
	| "&&"  { COMBAND }
	| "||"	{ COMBOR }
	| "&"	{ BITAND }
	| "|"	{ BITOR }

	(* SEPARATORS *)
	| ['('] { LPAREN }
	| [')']	{ RPAREN }
	| ['['] { LBRACKET }
	| [']'] { RBRACKET }
	| ['{'] { LCURLY }
	| ['}']	{ RCURLY }
	| [';'] { SEMICOLON }
	| [','] { COMMA }

	(* END OF FILE *)	
	| eof	{ EOF }
	| _ as ce { raise (Error (Some ce)) }

(* Mutually recursive extra function to handle multi-line comments *)
(* Counts newlines inside comment and if comment ends go back to token *)
(* EOF in unfinished comment, raise error *)
and multiline_comment = parse
	| "*/" { token lexbuf }
	| ['\n'] { Lexing.new_line lexbuf; multiline_comment lexbuf }
	| _	 { multiline_comment lexbuf }
	| eof 	{ raise (Error None) } 
	
