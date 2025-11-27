# Cigrid Front‑End (Good Level)

This directory contains an implementation of the **Cigrid** programming language front‑end for the Good (G) level of Module 1.  The Good level requires a complete scanner/lexer, parser and pretty printer for the entire Cigrid specification and mandates syntactic sugar for `++`/`--` as well as a `--line-error` flag for concise error reporting【413910581251483†L210-L233】【413910581251483†L310-L329】.  The implementation provided here meets these requirements.

## Project architecture

The Cigrid front‑end is organised into several components, each in its own file:

| File | Purpose |
| --- | --- |
| **`Makefile`** | Builds the `cigrid` executable from the source files using `g++` with C++17.  The `SRC` variable lists `main.cpp`, `lexer.cpp`, `parser.cpp` and `pprint.cpp`; `make` produces the `cigrid` binary. |
| **`main.cpp`** | Program entry point.  Parses command line flags (`--pretty-print` and `--line-error`), reads a Cigrid source file, invokes the parser and optionally pretty‑prints the resulting AST.  If `--line-error` is present, it reports only the offending line number on lexical or syntax errors, otherwise it prints a descriptive message.  Exit code is `0` on success and `1` on error. |
| **`ast.hpp`** | Definitions of the abstract syntax tree (AST).  It declares structures for types (`Type`, including base types, pointer types and user‑defined identifiers), expressions (integer/char/string literals, variables, binary/unary operations, function calls, new expressions, array access), statements (assignments, array assignments, returns, breaks, expression statements, if/while/for loops, blocks, variable definitions, delete), and top‑level entities (extern declarations, global variable declarations and definitions, struct definitions, function definitions and the overall program).  Shared pointers (`Ptr<T>`) are used for memory‑safe ownership. |
| **`lexer.hpp` & `lexer.cpp`** | The lexical analyser (lexer) responsible for tokenising the input source.  The lexer is hand‑written as a deterministic finite–state automaton.  It reads the entire file into a string, maintains a position index and a line counter, skips whitespace and three forms of comments (`#`, `//`, `/* … */`), and returns tokens representing keywords, identifiers, numbers (decimal and hexadecimal), character and string literals, operators and punctuation.  Multi‑character operators (`<<`, `>>`, `==`, `!=`, `<=`, `>=`, `&&`, `||`, `++`, `--`, `->`) are recognised using a look‑ahead on the underlying character buffer.  Escape sequences in character and string literals are decoded (e.g. `\n`, `\t`, `\"`).  The lexer follows a **maximal munch** strategy—choosing the longest matching lexeme—and throws a `ParseError` (with line number) on invalid characters.  A single‑token look‑ahead buffer supports the `peek()` function used by the parser for disambiguation. |
| **`parser.hpp` & `parser.cpp`** | The syntax analyser.  It implements a **recursive descent parser** with a top‑level `parse_file` function that wraps a `Lexer` and a `Parser` object.  The parser consumes tokens from the lexer and constructs the AST defined in `ast.hpp`.  Parsing uses two broad techniques: (1) for high‑level constructs (functions, structs, global variables) it uses straightforward top‑down recursive functions (`parseFuncDef`, `parseStruct`, etc.); (2) for expressions it employs **precedence climbing** (a variant of recursive descent) to implement C‑style operator precedence and associativity.  The parser recognises all language features required by the specification【413910581251483†L210-L233】, including pointer types (`TPoint`), user‑defined types (`TIdent`), unary operators (`!`, `~`, unary `-`), shift operators (`<<`, `>>`), arrays and array element assignments, struct definitions and field accesses, `new` and `delete` expressions, and `for` loops (desugared internally into a block containing an initial statement and a `while` condition with an increment statement appended to the loop body).  Prefix and postfix `++`/`--` are converted into assignments (`x++` becomes `x = x + 1`, and similarly for `--`) per the assignment requirement【413910581251483†L310-L329】.  On encountering invalid syntax, the parser throws a `ParseError` containing a message and line number. |
| **`pprint.hpp` & `pprint.cpp`** | The pretty printer.  It traverses the AST and prints it in a canonical textual representation used by the assignment’s testing framework (see `examples.txt`).  Each AST node type has a distinct printed form (e.g. `EBinOp("+", lhs, rhs)` for binary addition, `SIf(cond, thenBranch, elseBranch)` for an if‑statement).  Blocks are printed as `SScope({ … })`.  Indentation is minimal and purely cosmetic—the testing harness ignores whitespace differences—but the printer inserts newlines and indentation to improve readability. |

## Solution overview

The implementation follows the typical compiler front‑end pipeline: **lexical analysis**, **syntactic analysis** and **AST pretty printing**.  The design deliberately avoids using automatic parser generators; instead both the lexer and parser are hand‑crafted to provide full control over corner cases and to meet the requirements of the assignment.

### Lexical analysis

Lexical analysis is performed by `lexer.cpp/lexer.hpp`.  The lexer reads the entire input source into a single string and maintains two pieces of state: a **cursor** that points at the current character and a **line counter** to track line numbers for error reporting.  Tokenisation proceeds by repeatedly skipping whitespace and comments, then examining the next character to decide which token to produce.

* **Deterministic finite–state machine**: Rather than building an explicit table, the lexer implements a deterministic finite–state machine via nested `if`/`switch` statements.  For each token kind the lexer consumes as many characters as possible (the *maximal munch* rule) before deciding that a complete lexeme has been recognised.
* **Comments**: Three comment styles are recognised: lines beginning with `#` (used in the assignment’s examples), C++‐style `//…` comments and C‑style `/*…*/` block comments.  Comments are skipped and line numbers are updated appropriately.
* **Literals**: The lexer recognises **decimal and hexadecimal integer literals**, **character literals** (supporting escape sequences like `\n`, `\t`, `\'` and escaped Unicode code units), and **string literals** with escape sequences decoded into their actual characters.  Invalid escape sequences result in a `ParseError` with the offending line number.
* **Identifiers and keywords**: Sequences of letters, digits and underscores beginning with a letter or underscore are classified as identifiers.  Reserved words such as `int`, `char`, `void`, `struct`, `extern`, `new`, `delete`, `if`, `else`, `while`, `for`, `return` and `break` are recognised as keywords.
* **Operators and punctuation**: All Cigrid operators and separators are handled, including multi‑character operators (`++`, `--`, `==`, `!=`, `<=`, `>=`, `&&`, `||`, `<<`, `>>`, `->`) and single‑character operators (`+`, `-`, `*`, `/`, `%`, `!`, `~`, `&`, `|`, `<`, `>`, `=`, `,`, `;`, `.`, `(`, `)`, `{`, `}`, `[`, `]`).  Look‑ahead is used to disambiguate multi‑character tokens without consuming extra characters.
* **Error handling**: On encountering an unrecognised character sequence, the lexer throws a `ParseError` containing both a message and the current line number.  This exception is caught in `main.cpp` and, depending on the `--line-error` flag, either prints just the line number or a human‑readable message.

### Parsing

Parsing is implemented in `parser.cpp/parser.hpp` as a **recursive descent parser**.  The parser consumes tokens from the lexer and constructs instances of the AST classes defined in `ast.hpp`.  Parsing functions mirror the high‑level grammar of Cigrid:

* **Top‑level constructs**: `parseProgram` reads a sequence of extern declarations, struct definitions, global variable declarations/definitions and function definitions until end of file.  Each top‑level element is stored in the `ASTProgram` vector of externs, structs, global declarations or functions.
* **Type grammar**: `parseType` recognises base types (`int`, `char`, `void`), user‑defined types (struct names) and pointer types.  Pointer types are parsed by counting `*` following a type and wrapping the underlying type in one or more `TPoint` nodes.
* **Function signatures**: `parseFuncDef` parses a return type, an identifier for the function name and a comma‑separated parameter list.  Parameter lists may be empty.  Each parameter is represented by a `FuncParam` containing its type and name.
* **Statements**: The main dispatcher `parseStmt` examines the current token and dispatches to specific functions for `if`/`while`/`for`/`return`/`break`/`delete` statements, block statements `{…}`, local variable declarations (`int x = 0;`), assignments and expression statements.  `for` loops are not represented directly in the AST; instead `parseFor` desugars them into a block containing the initialisation, a `while` loop for the condition and an increment expression at the end of the loop body, matching the semantics described in the assignment.【413910581251483†L310-L329】
* **Expressions**: Expressions are parsed using **precedence climbing** (also known as Pratt parsing).  The grammar distinguishes levels for logical OR (`||`), logical AND (`&&`), bitwise OR (`|`), bitwise XOR (`^`), bitwise AND (`&`), equality/inequality (`==`, `!=`), relational (`<`, `<=`, `>`, `>=`), shift (`<<`, `>>`), additive (`+`, `-`), multiplicative (`*`, `/`, `%`), unary (prefix `!`, `~`, unary `-` and prefix `++`/`--`) and postfix (function calls, array accesses, postfix `++`/`--`).  The parser constructs `EBinOp`, `EUnOp`, `ECall`, `ENew` and `EArrayAccess` nodes accordingly.
* **Desugaring**: The parser implements syntactic sugar required by the Good level:
  * **Pre/post increment and decrement**: Occurrences of `x++`, `++x`, `x--` or `--x` are translated into `SVarAssign` or `EBinOp` nodes equivalent to `x = x + 1` or `x = x - 1`.
  * **For loops**: `for(init; cond; inc) body` is lowered into a block containing `init`, a `while` loop with condition `cond` and body containing the original `body` followed by `inc`.  This simplifies semantic analysis and pretty printing.
* **Error propagation**: Any unexpected token or grammar violation results in a `ParseError` with line number.  The parser uses look‑ahead via `Lexer::peek()` to disambiguate constructs such as distinguishing between an assignment (`x = expr;`) and an expression statement beginning with an identifier (`f(x);`).

### Pretty printing

The pretty printer in `pprint.cpp` traverses the AST and prints a canonical representation used by the assignment’s tests.  Each node type has a corresponding textual form.  For example:

* `Type`: printed as `TInt`, `TChar`, `TVoid`, `TPoint(subtype)` for pointer types and `TIdent("Name")` for user‑defined types.
* `Expr`: `EInt(n)`, `EChar('c')`, `EString("text")`, `EVar("x")`, `EBinOp("+", lhs, rhs)`, `EUnOp("~", expr)`, `ECall("f", {arg1, arg2})`, `ENew(type, size)`, `EArrayAccess("a", idx, field)`.
* `Stmt`: `SVarAssign("x", expr)`, `SArrayAssign("a", idx, field, expr)`, `SVarDef(type, "x", init)`, `SReturn(expr)`, `SBreak`, `SDelete("p")`, `SExpr(expr)`, `SIf(cond, then, else)`, `SWhile(cond, body)`, and `SScope({ stmts… })` for blocks.
* `FuncDef` and other top‑level nodes are printed as `GFuncDef(retType, "name", {params}, body)`, `GVarDef`, `GVarDecl` or `GStruct`.

The printer inserts minimal indentation and newlines to enhance readability while ensuring that the semantics are unambiguous.  The testing harness used by the course ignores whitespace differences【413910581251483†L210-L233】, so formatting is largely cosmetic.

## Building and running

To build the Cigrid front‑end, navigate to the `solutions/cigrid` directory and run:

```sh
make
```

This compiles all `.cpp` files into a single executable named `cigrid` using `g++` and C++17 settings.  No external libraries are required.

To run the compiler on a source file:

```sh
./cigrid [--pretty-print] [--line-error] [--asm] [--compile] source.c
```

* `--pretty-print` causes the program to print the constructed AST to standard output in the canonical format described above.  If omitted, the program performs only syntax checking and exits quietly on success.
* `--line-error` changes the error reporting mode: on the first lexical or syntax error the program writes only the line number to standard error and exits with code 1.  Without this flag, a descriptive error message (including line number) is printed.  This behaviour satisfies the Good‑level specification【413910581251483†L310-L329】.
* `--asm` lowers straight‑line `int main()` programs into a simple linear IR and emits NASM‑compatible x86‑64 assembly, always declaring `global main` and using a stack frame built with `sub rsp, N`/`add rsp, N`.
* `--compile` is a convenience flag that requires `--asm`; after printing the assembly it invokes `nasm -felf64` followed by `gcc -no-pie` to produce an executable named after the input stem.
* Providing an unknown flag results in exit code 1 and a short usage error.

The return codes are:

| Code | Meaning |
| --- | --- |
| `0` | Successfully parsed the entire input (and printed the AST if requested). |
| `1` | Lexical or syntax error occurred.  If `--line-error` is used, only the line number is printed to standard error. |

## Notes and further work

* **Good level vs. Satisfactory level**: The Satisfactory (S) level requires only a subset of Cigrid—hex literals, shift and unary operators, arrays, structs, pointers, `new`/`delete`, `for` loops and global variables can be omitted.  The Good (G) level implemented here includes all optional features and the `--line-error` flag, and implements syntactic sugar for `++`/`--` as required【413910581251483†L210-L233】【413910581251483†L310-L329】.
* **Memory management**: AST nodes are allocated via `std::shared_ptr` to simplify ownership.  There is no garbage collector; once parsing is complete the entire AST remains in memory until program termination.
* **Future extensions**: For the Very Good (VG) level, one would implement name analysis (symbol table construction and checking), type checking, and more precise error reporting (e.g. multiple errors).  The existing architecture—with a lexer, recursive descent parser and rich AST—provides a solid foundation for these additional phases.
