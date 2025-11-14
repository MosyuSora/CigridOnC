#include "parser.hpp"

// Include standard library headers explicitly.  The parser makes use
// of std::make_shared, std::vector and std::string, which are
// transitively available via ast.hpp and scanner.hpp.  Adding the
// headers here clarifies the dependencies and prevents compilation
// errors on toolchains that require direct inclusion.
#include <iostream>
#include <stdexcept>
#include <memory>
#include <string>
#include <vector>

namespace cigrid {

// Entry point: parse the file into an ASTProgram.  This wraps the
// Scanner and Parser together.
ASTProgram parse_file(const std::string &filename) {
    Scanner sc(filename);
    Parser parser(sc);
    return parser.parseProgram();
}

// Parser constructor: initialise the scanner and read the first token.
Parser::Parser(Scanner &s) : sc(s) {
    tok = sc.next();
}

// Advance to the next token.
void Parser::advance() {
    tok = sc.next();
}

// Expect a specific token type; throw on mismatch.
void Parser::expect(TokenType t) {
    if (tok.type != t) {
        throw ParseError("unexpected token", tok.line);
    }
    advance();
}

// If the current token matches t, consume it and return true; else return false.
bool Parser::accept(TokenType t) {
    if (tok.type == t) {
        advance();
        return true;
    }
    return false;
}

// Parse a type.  A type can be a base type (int/char/void) or an
// identifier referring to a struct.  After the base type zero or more
// asterisks may follow to form pointer types.
Type Parser::parseType() {
    Type base;
    // base type or user defined ident
    if (tok.type == TokenType::Kw_int) {
        base = Type(BaseType::Int);
        advance();
    } else if (tok.type == TokenType::Kw_char) {
        base = Type(BaseType::Char);
        advance();
    } else if (tok.type == TokenType::Kw_void) {
        base = Type(BaseType::Void);
        advance();
    } else if (tok.type == TokenType::Ident) {
        // user defined type
        base = Type(tok.text);
        advance();
    } else {
        throw ParseError("expected type", tok.line);
    }
    // pointer stars
    while (tok.type == TokenType::Star) {
        advance();
        base = Type(std::make_shared<Type>(base));
    }
    return base;
}

// Parse a single function parameter: type and identifier.
FuncParam Parser::parseParam() {
    Type t = parseType();
    if (tok.type != TokenType::Ident) {
        throw ParseError("expected parameter name", tok.line);
    }
    std::string name = tok.text;
    advance();
    return FuncParam(t, name);
}

// Parse a parameter list enclosed in parentheses.  Handles zero
// parameters correctly.
std::vector<FuncParam> Parser::parseParamList() {
    std::vector<FuncParam> params;
    expect(TokenType::LParen);
    if (tok.type != TokenType::RParen) {
        // one or more parameters
        params.push_back(parseParam());
        while (tok.type == TokenType::Comma) {
            advance();
            params.push_back(parseParam());
        }
    }
    expect(TokenType::RParen);
    return params;
}

// Parse a function definition given its already parsed return type and name.
FuncDef Parser::parseFuncDef(const Type& retType, const std::string& name) {
    // parse parameter list
    std::vector<FuncParam> params = parseParamList();
    // parse body block
    Ptr<SBlock> body = parseBlock();
    FuncDef fd;
    fd.retType = retType;
    fd.name = name;
    fd.params = params;
    fd.body = body;
    return fd;
}

// Parse an extern declaration given the return type and name.  For extern
// declarations we only record the parameter types (names are unused).
ExternDecl Parser::parseExtern(const Type& retType, const std::string& name) {
    // parse parameter list but only record types
    std::vector<FuncParam> params;
    expect(TokenType::LParen);
    std::vector<Type> paramTypes;
    if (tok.type != TokenType::RParen) {
        Type t = parseType();
        paramTypes.push_back(t);
        if (tok.type == TokenType::Ident) {
            // Parameter name present in prototype; ignore it.
            advance();
        }
        while (tok.type == TokenType::Comma) {
            advance();
            t = parseType();
            paramTypes.push_back(t);
            if (tok.type == TokenType::Ident) {
                advance();
            }
        }
    }
    expect(TokenType::RParen);
    expect(TokenType::Semicolon);
    ExternDecl ex;
    ex.type = retType;
    ex.name = name;
    ex.paramTypes = paramTypes;
    return ex;
}

// Parse a struct definition: struct Name { fields } ;  The semicolon
// after the closing brace is optional in examples but we treat it as
// required.  Fields are a list of (type name;) pairs.
GStruct Parser::parseStruct() {
    expect(TokenType::Kw_struct);
    if (tok.type != TokenType::Ident) {
        throw ParseError("expected struct name", tok.line);
    }
    std::string name = tok.text;
    advance();
    expect(TokenType::LBrace);
    std::vector<StructField> fields;
    while (tok.type != TokenType::RBrace) {
        // parse field type
        Type t = parseType();
        if (tok.type != TokenType::Ident) {
            throw ParseError("expected field name", tok.line);
        }
        std::string fname = tok.text;
        advance();
        expect(TokenType::Semicolon);
        fields.emplace_back(t, fname);
    }
    expect(TokenType::RBrace);
    // optional semicolon after struct
    if (tok.type == TokenType::Semicolon) {
        advance();
    }
    GStruct st;
    st.name = name;
    st.fields = fields;
    return st;
}

// Parse a global variable declaration or definition.  The caller has
// already read the type and identifier.  This function will either add
// a GVarDecl to prog.globals or a GVarDef to prog.globalDefs.  A
// definition has an initializer starting with '='.
void Parser::parseGlobalVar(Type type, const std::string& name, ASTProgram& prog) {
    // Check if this is a definition with initializer
    if (tok.type == TokenType::Assign) {
        // definition
        advance();
        Ptr<Expr> init = parseExpr();
        expect(TokenType::Semicolon);
        GVarDef d;
        d.type = type;
        d.name = name;
        d.init = init;
        prog.globalDefs.push_back(d);
    } else if (tok.type == TokenType::Semicolon) {
        // declaration
        advance();
        GVarDecl d;
        d.type = type;
        d.name = name;
        prog.globals.push_back(d);
    } else {
        throw ParseError("unexpected token in global declaration", tok.line);
    }
}

// Parse a block of statements surrounded by braces.  A block may contain
// zero or more statements.  This function consumes the opening brace
// and stops when it encounters the matching closing brace.
Ptr<SBlock> Parser::parseBlock() {
    expect(TokenType::LBrace);
    auto block = std::make_shared<SBlock>();
    while (tok.type != TokenType::RBrace) {
        block->stmts.push_back(parseStmt());
    }
    expect(TokenType::RBrace);
    return block;
}

// Parse any statement.  Dispatches to specific statement parsers based
// on the current token.  Note that declarations inside blocks begin
// with a type keyword.
Ptr<Stmt> Parser::parseStmt() {
    // block
    if (tok.type == TokenType::LBrace) {
        return parseBlock();
    }
    // if
    if (tok.type == TokenType::Kw_if) {
        return parseIf();
    }
    // while
    if (tok.type == TokenType::Kw_while) {
        return parseWhile();
    }
    // for
    if (tok.type == TokenType::Kw_for) {
        return parseFor();
    }
    // return
    if (tok.type == TokenType::Kw_return) {
        return parseReturn();
    }
    // break
    if (tok.type == TokenType::Kw_break) {
        return parseBreak();
    }
    // delete
    if (tok.type == TokenType::Kw_delete) {
        return parseDelete();
    }
    // local variable definition: starts with type keyword or identifier type
    if (tok.type == TokenType::Kw_int || tok.type == TokenType::Kw_char || tok.type == TokenType::Kw_void || tok.type == TokenType::Ident) {
        // Save current position to decide if this is a declaration or an expression starting with an identifier.
        // We only treat it as a declaration if it's a base type or ident followed by '*' or another ident and then a variable name and optionally '='.
        // To decide, we need to parse type and then peek if next token is ident.
        // We'll clone scanner state by using peek and not consuming tokens before committing.
        // However since parseType consumes base and star(s), we can't easily peek.  Instead we will temporarily parse type and then check next token.
        // To avoid complexity, we'll treat any type keyword (int/char/void) as the start of a declaration.  For ident we must check if ident is a type name.
        // For simplicity we treat ident followed by pointer or another ident as declaration.  If ident followed by '(' then it's a function call; if ident followed by number or operator then it's expression.
        // We check: if current token is not type keyword but ident, we peek next token: if next is Ident or Star, treat as declaration; if next is '=' or '(' or '[' or '.' or other operator, treat as expression or assignment.
        if (tok.type != TokenType::Ident || (tok.type == TokenType::Ident && (sc.peek().type == TokenType::Ident || sc.peek().type == TokenType::Star))) {
            return parseVarDefStmt();
        }
    }
    // default: assignment, array assignment or expression statement
    return parseExprOrAssignStmt();
}

// Parse an if statement: if(expr) stmt [else stmt]
Ptr<Stmt> Parser::parseIf() {
    expect(TokenType::Kw_if);
    expect(TokenType::LParen);
    Ptr<Expr> cond = parseExpr();
    expect(TokenType::RParen);
    Ptr<Stmt> thenStmt = parseStmt();
    Ptr<Stmt> elseStmt = nullptr;
    if (tok.type == TokenType::Kw_else) {
        advance();
        elseStmt = parseStmt();
    }
    return std::make_shared<SIf>(cond, thenStmt, elseStmt);
}

// Parse a while loop: while(expr) stmt
Ptr<Stmt> Parser::parseWhile() {
    expect(TokenType::Kw_while);
    expect(TokenType::LParen);
    Ptr<Expr> cond = parseExpr();
    expect(TokenType::RParen);
    Ptr<Stmt> body = parseStmt();
    return std::make_shared<SWhile>(cond, body);
}

// Parse a return statement: return [expr] ;
Ptr<Stmt> Parser::parseReturn() {
    expect(TokenType::Kw_return);
    if (tok.type == TokenType::Semicolon) {
        advance();
        return std::make_shared<SReturn>(nullptr);
    } else {
        Ptr<Expr> val = parseExpr();
        expect(TokenType::Semicolon);
        return std::make_shared<SReturn>(val);
    }
}

// Parse a break statement: break ;
Ptr<Stmt> Parser::parseBreak() {
    expect(TokenType::Kw_break);
    expect(TokenType::Semicolon);
    return std::make_shared<SBreak>();
}

// Parse a delete statement: delete expr ; or delete[] expr ;
Ptr<Stmt> Parser::parseDelete() {
    expect(TokenType::Kw_delete);
    bool isArray = false;
    if (tok.type == TokenType::LBracket) {
        // delete[]
        advance();
        expect(TokenType::RBracket);
        isArray = true;
    }
    // parse expression to delete; in examples it's always a variable or array
    Ptr<Expr> expr = parseExpr();
    expect(TokenType::Semicolon);
    return std::make_shared<SDelete>(isArray, expr);
}

// Parse a local variable definition statement: type name [= expr] ;
Ptr<Stmt> Parser::parseVarDefStmt() {
    Type t = parseType();
    if (tok.type != TokenType::Ident) {
        throw ParseError("expected variable name", tok.line);
    }
    std::string name = tok.text;
    advance();
    Ptr<Expr> init = nullptr;
    if (tok.type == TokenType::Assign) {
        advance();
        init = parseExpr();
    }
    expect(TokenType::Semicolon);
    return std::make_shared<SVarDef>(t, name, init);
}

// Parse assignment, array assignment or expression statement.  Handles
// postfix ++ and -- as syntactic sugar.  If the statement begins with
// '++' or '--' we treat it as prefix increment/decrement and convert
// into an assignment.  Otherwise, if it begins with an identifier we
// check for assignment '=' or array indexing; if none is found we
// parse as an expression statement.  Finally we support postfix
// increment/decrement on variables and array elements.
Ptr<Stmt> Parser::parseExprOrAssignStmt() {
    // prefix ++ or -- as statement
    if (tok.type == TokenType::PlusPlus || tok.type == TokenType::MinusMinus) {
        bool inc = (tok.type == TokenType::PlusPlus);
        advance();
        if (tok.type != TokenType::Ident) {
            throw ParseError("expected identifier after prefix ++/--", tok.line);
        }
        std::string name = tok.text;
        advance();
        expect(TokenType::Semicolon);
        // translate ++x to x = x + 1 and --x to x = x - 1
        Ptr<Expr> rhs = std::make_shared<EBinOp>(inc ? "+" : "-", std::make_shared<EVar>(name), std::make_shared<EInt>(1));
        return std::make_shared<SVarAssign>(name, rhs);
    }
    // We need to parse a potential assignment or array/field assignment or a
    // generic expression statement starting with an identifier.
    if (tok.type == TokenType::Ident) {
        std::string baseName = tok.text;
        advance();

        // Function call used as a statement.
        if (tok.type == TokenType::LParen) {
            advance();
            std::vector<Ptr<Expr>> args;
            if (tok.type != TokenType::RParen) {
                args.push_back(parseExpr());
                while (tok.type == TokenType::Comma) {
                    advance();
                    args.push_back(parseExpr());
                }
            }
            expect(TokenType::RParen);
            expect(TokenType::Semicolon);
            return std::make_shared<SExpr>(std::make_shared<ECall>(baseName, args));
        }

        // Parse optional indexing and/or field access.
        Ptr<Expr> indexExpr;
        std::string field;
        bool hasIndex = false;
        bool hasField = false;

        auto parseFieldName = [this]() -> std::string {
            if (tok.type != TokenType::Ident) {
                throw ParseError("expected field name after '.'", tok.line);
            }
            std::string fname = tok.text;
            advance();
            return fname;
        };

        if (tok.type == TokenType::LBracket) {
            hasIndex = true;
            advance();
            indexExpr = parseExpr();
            expect(TokenType::RBracket);
        }

        if (tok.type == TokenType::Dot || tok.type == TokenType::Arrow) {
            advance();
            field = parseFieldName();
            hasField = true;
        }

        auto buildLHSExpr = [&]() -> Ptr<Expr> {
            if (hasIndex || hasField) {
                return std::make_shared<EArrayAccess>(baseName, indexExpr, field);
            }
            return std::make_shared<EVar>(baseName);
        };

        bool useArrayAssign = hasIndex || hasField;

        if (tok.type == TokenType::PlusPlus || tok.type == TokenType::MinusMinus) {
            bool inc = (tok.type == TokenType::PlusPlus);
            advance();
            expect(TokenType::Semicolon);
            Ptr<Expr> lhsExpr = buildLHSExpr();
            Ptr<Expr> rhs = std::make_shared<EBinOp>(inc ? "+" : "-", lhsExpr, std::make_shared<EInt>(1));
            if (useArrayAssign) {
                return std::make_shared<SArrayAssign>(baseName, indexExpr, field, rhs);
            }
            return std::make_shared<SVarAssign>(baseName, rhs);
        }

        if (tok.type == TokenType::Assign) {
            advance();
            Ptr<Expr> rhs = parseExpr();
            expect(TokenType::Semicolon);
            if (useArrayAssign) {
                return std::make_shared<SArrayAssign>(baseName, indexExpr, field, rhs);
            }
            return std::make_shared<SVarAssign>(baseName, rhs);
        }

        Ptr<Expr> expr = buildLHSExpr();
        expect(TokenType::Semicolon);
        return std::make_shared<SExpr>(expr);
    }

    // Fallback: parse a generic expression statement.
    Ptr<Expr> e = parseExpr();
    expect(TokenType::Semicolon);
    return std::make_shared<SExpr>(e);
}
// Parse a for loop: for(init; cond; incr) stmt.  We desugar it into
// a scope containing the initialiser and a while loop whose body
// contains the original body followed by the increment.
Ptr<Stmt> Parser::parseFor() {
    expect(TokenType::Kw_for);
    expect(TokenType::LParen);
    // init statement: can be variable definition or expression or empty
    Ptr<Stmt> initStmt;
    if (tok.type == TokenType::Semicolon) {
        // empty init
        initStmt = nullptr;
        advance();
    } else {
        // parse either declaration or assignment/expr
        if (tok.type == TokenType::Kw_int || tok.type == TokenType::Kw_char || tok.type == TokenType::Kw_void || tok.type == TokenType::Ident) {
            // for simplicity we treat ident followed by ident/star as decl
            if (tok.type != TokenType::Ident || (tok.type == TokenType::Ident && (sc.peek().type == TokenType::Ident || sc.peek().type == TokenType::Star))) {
                initStmt = parseVarDefStmt();
            } else {
                initStmt = parseExprOrAssignStmt();
            }
        } else {
            initStmt = parseExprOrAssignStmt();
        }
    }
    // condition expression
    Ptr<Expr> cond = nullptr;
    if (tok.type == TokenType::Semicolon) {
        // empty condition = true
        cond = std::make_shared<EInt>(1);
        advance();
    } else {
        cond = parseExpr();
        expect(TokenType::Semicolon);
    }
    // increment statement
    Ptr<Stmt> incrStmt = nullptr;
    if (tok.type == TokenType::RParen) {
        // empty increment
        incrStmt = nullptr;
    } else {
        // parse assignment or expr (we can reuse parseExprOrAssignStmt but must not consume semicolon)
        // We'll parse similar to parseExprOrAssignStmt but expecting no trailing semicolon
        // We'll handle prefix and postfix ++/--, assignments, or expression statements without semicolon.
        if (tok.type == TokenType::PlusPlus || tok.type == TokenType::MinusMinus) {
            bool inc = (tok.type == TokenType::PlusPlus);
            advance();
            if (tok.type != TokenType::Ident) {
                throw ParseError("expected identifier after prefix ++/--", tok.line);
            }
            std::string name = tok.text;
            advance();
            // no semicolon here
            Ptr<Expr> rhs = std::make_shared<EBinOp>(inc ? "+" : "-", std::make_shared<EVar>(name), std::make_shared<EInt>(1));
            incrStmt = std::make_shared<SVarAssign>(name, rhs);
        } else if (tok.type == TokenType::Ident) {
            std::string baseName = tok.text;
            advance();
            bool isArray = false;
            std::string arrayName;
            Ptr<Expr> indexExpr;
            std::string field;
            if (tok.type == TokenType::LBracket) {
                isArray = true;
                arrayName = baseName;
                advance();
                indexExpr = parseExpr();
                expect(TokenType::RBracket);
                if (tok.type == TokenType::Dot) {
                    advance();
                    if (tok.type != TokenType::Ident) {
                        throw ParseError("expected field name", tok.line);
                    }
                    field = tok.text;
                    advance();
                }
            }
            if (tok.type == TokenType::PlusPlus || tok.type == TokenType::MinusMinus) {
                bool inc = (tok.type == TokenType::PlusPlus);
                advance();
                Ptr<Expr> rhs;
                if (isArray) {
                    auto lhs = std::make_shared<EArrayAccess>(arrayName, indexExpr, field);
                    rhs = std::make_shared<EBinOp>(inc ? "+" : "-", lhs, std::make_shared<EInt>(1));
                    incrStmt = std::make_shared<SArrayAssign>(arrayName, indexExpr, field, rhs);
                } else {
                    rhs = std::make_shared<EBinOp>(inc ? "+" : "-", std::make_shared<EVar>(baseName), std::make_shared<EInt>(1));
                    incrStmt = std::make_shared<SVarAssign>(baseName, rhs);
                }
            } else if (tok.type == TokenType::Assign) {
                advance();
                Ptr<Expr> rhs = parseExpr();
                if (isArray) {
                    incrStmt = std::make_shared<SArrayAssign>(arrayName, indexExpr, field, rhs);
                } else {
                    incrStmt = std::make_shared<SVarAssign>(baseName, rhs);
                }
            } else {
                // treat as expression statement (without semicolon) but can't happen in for loop increment
                incrStmt = std::make_shared<SExpr>(std::make_shared<EVar>(baseName));
            }
        } else {
            // handle other possible increment expressions but not needed
            incrStmt = nullptr;
        }
    }
    expect(TokenType::RParen);
    // body
    Ptr<Stmt> bodyStmt = parseStmt();
    // desugar
    auto outerBlock = std::make_shared<SBlock>();
    if (initStmt) outerBlock->stmts.push_back(initStmt);
    // while loop block
    auto innerBlock = std::make_shared<SBlock>();
    // if body is a block we can insert its stmts then incr; else just insert body then incr
    if (auto blk = std::dynamic_pointer_cast<SBlock>(bodyStmt)) {
        // copy statements
        for (auto& s : blk->stmts) {
            innerBlock->stmts.push_back(s);
        }
    } else {
        innerBlock->stmts.push_back(bodyStmt);
    }
    // append increment if present
    if (incrStmt) {
        innerBlock->stmts.push_back(incrStmt);
    }
    auto whileStmt = std::make_shared<SWhile>(cond, innerBlock);
    outerBlock->stmts.push_back(whileStmt);
    return outerBlock;
}

// Parse expression with lowest precedence
Ptr<Expr> Parser::parseExpr() {
    return parseOrOr();
}

Ptr<Expr> Parser::parseOrOr() {
    Ptr<Expr> lhs = parseAndAnd();
    while (tok.type == TokenType::OrOr) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseAndAnd();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseAndAnd() {
    Ptr<Expr> lhs = parseBitOr();
    while (tok.type == TokenType::AndAnd) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseBitOr();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseBitOr() {
    Ptr<Expr> lhs = parseBitXor();
    while (tok.type == TokenType::Or) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseBitXor();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseBitXor() {
    Ptr<Expr> lhs = parseBitAnd();
    while (tok.type == TokenType::Xor) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseBitAnd();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseBitAnd() {
    Ptr<Expr> lhs = parseEquality();
    while (tok.type == TokenType::And) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseEquality();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseEquality() {
    Ptr<Expr> lhs = parseRelational();
    while (tok.type == TokenType::Eq || tok.type == TokenType::Neq) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseRelational();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseRelational() {
    Ptr<Expr> lhs = parseShift();
    while (tok.type == TokenType::Lt || tok.type == TokenType::Le || tok.type == TokenType::Gt || tok.type == TokenType::Ge) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseShift();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseShift() {
    Ptr<Expr> lhs = parseAddSub();
    while (tok.type == TokenType::ShiftL || tok.type == TokenType::ShiftR) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseAddSub();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseAddSub() {
    Ptr<Expr> lhs = parseMulDivMod();
    while (tok.type == TokenType::Plus || tok.type == TokenType::Minus) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseMulDivMod();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseMulDivMod() {
    Ptr<Expr> lhs = parseUnary();
    while (tok.type == TokenType::Star || tok.type == TokenType::Slash || tok.type == TokenType::Percent) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> rhs = parseUnary();
        lhs = std::make_shared<EBinOp>(op, lhs, rhs);
    }
    return lhs;
}

Ptr<Expr> Parser::parseUnary() {
    // unary operators: ! ~ -
    if (tok.type == TokenType::Not || tok.type == TokenType::Tilde || tok.type == TokenType::Minus) {
        std::string op = tok.text;
        advance();
        Ptr<Expr> operand = parseUnary();
        return std::make_shared<EUnOp>(op, operand);
    }
    return parsePostfix();
}

Ptr<Expr> Parser::parsePostfix() {
    Ptr<Expr> expr = parsePrimary();
    while (true) {
        if (tok.type == TokenType::LBracket) {
            // array access
            advance();
            Ptr<Expr> index = parseExpr();
            expect(TokenType::RBracket);
            std::string fieldName;
            if (tok.type == TokenType::Dot || tok.type == TokenType::Arrow) {
                advance();
                if (tok.type != TokenType::Ident) {
                    throw ParseError("expected field name", tok.line);
                }
                fieldName = tok.text;
                advance();
            }
            if (auto v = std::dynamic_pointer_cast<EVar>(expr)) {
                expr = std::make_shared<EArrayAccess>(v->name, index, fieldName);
            } else if (auto a = std::dynamic_pointer_cast<EArrayAccess>(expr)) {
                if (!a->field.empty()) {
                    throw ParseError("nested array access not supported", tok.line);
                }
                expr = std::make_shared<EArrayAccess>(a->name, a->index, fieldName);
            } else {
                throw ParseError("invalid array base", tok.line);
            }
        } else if (tok.type == TokenType::Dot || tok.type == TokenType::Arrow) {
            advance();
            if (tok.type != TokenType::Ident) {
                throw ParseError("expected field name", tok.line);
            }
            std::string fieldName = tok.text;
            advance();
            if (auto v = std::dynamic_pointer_cast<EVar>(expr)) {
                expr = std::make_shared<EArrayAccess>(v->name, Ptr<Expr>(), fieldName);
            } else if (auto a = std::dynamic_pointer_cast<EArrayAccess>(expr)) {
                if (!a->field.empty()) {
                    throw ParseError("nested field access not supported", tok.line);
                }
                expr = std::make_shared<EArrayAccess>(a->name, a->index, fieldName);
            } else {
                throw ParseError("invalid field access", tok.line);
            }
        } else {
            break;
        }
    }
    return expr;
}

Ptr<Expr> Parser::parsePrimary() {
    // literals, identifiers, parentheses, function calls, new expressions
    if (tok.type == TokenType::Number) {
        std::string text = tok.text;
        // convert to integer; handle hex prefix
        long long value = 0;
        if (text.size() > 2 && (text[0] == '0' && (text[1] == 'x' || text[1] == 'X'))) {
            // hex
            value = std::stoll(text.substr(2), nullptr, 16);
        } else {
            value = std::stoll(text);
        }
        advance();
        return std::make_shared<EInt>(value);
    }
    if (tok.type == TokenType::CharLit) {
        // tok.text contains single char
        char c = tok.text[0];
        advance();
        return std::make_shared<EChar>(c);
    }
    if (tok.type == TokenType::StringLit) {
        std::string s = tok.text;
        advance();
        return std::make_shared<EString>(s);
    }
    if (tok.type == TokenType::LParen) {
        advance();
        Ptr<Expr> e = parseExpr();
        expect(TokenType::RParen);
        return e;
    }
    if (tok.type == TokenType::Kw_new) {
        // new T[n]
        advance();
        // parse base type (not pointer type here)
        Type t = parseType();
        // we expect '[' count ']' to allocate array
        expect(TokenType::LBracket);
        Ptr<Expr> count = parseExpr();
        expect(TokenType::RBracket);
        // new returns pointer type
        Type ptrType = Type(std::make_shared<Type>(t));
        return std::make_shared<ENew>(ptrType, count);
    }
    if (tok.type == TokenType::Ident) {
        std::string name = tok.text;
        advance();
        // function call or variable
        if (tok.type == TokenType::LParen) {
            // function call
            advance();
            std::vector<Ptr<Expr>> args;
            if (tok.type != TokenType::RParen) {
                args.push_back(parseExpr());
                while (tok.type == TokenType::Comma) {
                    advance();
                    args.push_back(parseExpr());
                }
            }
            expect(TokenType::RParen);
            return std::make_shared<ECall>(name, args);
        }
        // variable
        return std::make_shared<EVar>(name);
    }
    throw ParseError("unexpected token in expression", tok.line);
}

// Parse the entire program.  Iterate through top level constructs until
// reaching end of input.  Recognises extern declarations, struct
// definitions, global variables and function definitions.
ASTProgram Parser::parseProgram() {
    ASTProgram prog;
    while (tok.type != TokenType::End) {
        if (tok.type == TokenType::Kw_extern) {
            // parse extern declaration: extern type name(paramTypes) ;
            advance();
            Type retType = parseType();
            if (tok.type != TokenType::Ident) {
                throw ParseError("expected function name in extern", tok.line);
            }
            std::string name = tok.text;
            advance();
            if (tok.type == TokenType::LParen) {
                ExternDecl ex = parseExtern(retType, name);
                prog.externs.push_back(ex);
            } else {
                expect(TokenType::Semicolon);
                GVarDecl decl;
                decl.type = retType;
                decl.name = name;
                prog.globals.push_back(decl);
            }
            continue;
        }
        if (tok.type == TokenType::Kw_struct) {
            GStruct st = parseStruct();
            prog.structs.push_back(st);
            continue;
        }
        // parse type at top level
        Type retType = parseType();
        if (tok.type != TokenType::Ident) {
            throw ParseError("expected identifier after type", tok.line);
        }
        std::string name = tok.text;
        advance();
        if (tok.type == TokenType::LParen) {
            // function definition
            FuncDef f = parseFuncDef(retType, name);
            prog.funcs.push_back(f);
        } else {
            // global var
            parseGlobalVar(retType, name, prog);
        }
    }
    return prog;
}

} // namespace cigrid