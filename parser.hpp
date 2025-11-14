#pragma once

#include "lexer.hpp"
#include "ast.hpp"

namespace cigrid {

// Parse the entire file at the given path into an ASTProgram.  Throws
// ParseError on lexing or parsing errors.  This is the main entry
// point used by main.cpp.
ASTProgram parse_file(const std::string& filename);

// Parser encapsulates the logic of turning a token stream produced by
// the Lexer into an AST.  It maintains the current token and provides
// a number of helper methods corresponding to grammar productions.
class Parser {
public:
    explicit Parser(Lexer &s);

    // Parse the entire program.  Returns an ASTProgram containing
    // extern declarations, global variables, struct definitions and
    // function definitions.
    ASTProgram parseProgram();

private:
    Lexer &sc;
    Token tok;

    // Advance to the next token.
    void advance();
    // Require that the current token is of type t and consume it.  If
    // the current token does not match, a ParseError is thrown.
    void expect(TokenType t);
    // Optionally consume the current token if it matches t.  Returns
    // true if the token was consumed.
    bool accept(TokenType t);

    // Parsing helpers
    Type parseType();                 // parse a type with optional pointer stars
    FuncParam parseParam();           // parse one function parameter
    std::vector<FuncParam> parseParamList(); // parse a parenthesised parameter list
    FuncDef parseFuncDef(const Type& retType, const std::string& name); // parse a function definition given its return type and name
    ExternDecl parseExtern(const Type& retType, const std::string& name); // parse an extern declaration after reading return type/name
    GStruct parseStruct();            // parse a struct definition
    // parse a global variable declaration or definition after reading type and (optionally) pointer stars and name
    // returns true if it was a definition (with initializer) and adds to globalDefs; otherwise adds to globals
    void parseGlobalVar(Type type, const std::string& name, ASTProgram& prog);

    // parse a block { ... }
    Ptr<SBlock> parseBlock();
    // parse a statement
    Ptr<Stmt> parseStmt();
    Ptr<Stmt> parseIf();
    Ptr<Stmt> parseWhile();
    Ptr<Stmt> parseFor();
    Ptr<Stmt> parseReturn();
    Ptr<Stmt> parseBreak();
    Ptr<Stmt> parseDelete();
    Ptr<Stmt> parseVarDefStmt();
    Ptr<Stmt> parseExprOrAssignStmt();

    // Expression parsing with precedence climbing
    Ptr<Expr> parseExpr();
    Ptr<Expr> parseOrOr();
    Ptr<Expr> parseAndAnd();
    Ptr<Expr> parseBitOr();
    Ptr<Expr> parseBitXor();
    Ptr<Expr> parseBitAnd();
    Ptr<Expr> parseEquality();
    Ptr<Expr> parseRelational();
    Ptr<Expr> parseShift();
    Ptr<Expr> parseAddSub();
    Ptr<Expr> parseMulDivMod();
    Ptr<Expr> parseUnary();
    Ptr<Expr> parsePostfix();
    Ptr<Expr> parsePrimary();
};

} // namespace cigrid