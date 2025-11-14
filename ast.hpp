#pragma once

#include <string>
#include <vector>
#include <memory>

// This file defines the abstract syntax tree (AST) for the Cigrid language.
// The AST here supports both the S‑level and G‑level constructs as described
// in the assignment specification.  Many of the structures added here are
// needed for pointer types, structs, arrays, new/delete expressions, unary
// operators and other extensions present at the G level.

namespace cigrid {

// Shared pointer alias used throughout the AST.  Using shared_ptr makes it
// easier to build and manage trees without worrying about ownership.
template <typename T>
using Ptr = std::shared_ptr<T>;

// Enumeration of the built‑in base types.  These correspond to the simple
// built‑in types allowed in Cigrid: int, char and void.
enum class BaseType {
    Int,
    Char,
    Void,
};

// Type represents any kind of type in the language.  It can be either a
// built‑in base type, a pointer to another type or a user defined type (e.g.
// struct names).  The `kind` field distinguishes between these variants.  For
// pointers the `pointee` field holds the type being pointed to.  For user
// defined types the `ident` field holds the name of the type.  A Type
// instance stores only one of these depending on its kind.
struct Type {
    enum class Kind { Base, Pointer, Ident };
    Kind kind;
    BaseType base{};                 // valid when kind==Base
    Ptr<Type> pointee;               // valid when kind==Pointer
    std::string ident;               // valid when kind==Ident

    // Constructors for convenience
    Type() : kind(Kind::Base), base(BaseType::Int) {}
    Type(BaseType b) : kind(Kind::Base), base(b) {}
    Type(const Ptr<Type>& p) : kind(Kind::Pointer), pointee(p) {}
    Type(const std::string& name) : kind(Kind::Ident), ident(name) {}
};

// Forward declarations for expression and statement types.
struct Expr;
struct Stmt;

// Expression base class.  All expressions derive from this.  The AST uses
// dynamic_pointer_cast to downcast expressions when pretty printing or
// analysing.
struct Expr {
    virtual ~Expr() = default;
};

// Numeric literal
struct EInt : Expr {
    long long value;
    explicit EInt(long long v) : value(v) {}
};

// Character literal
struct EChar : Expr {
    char value;
    explicit EChar(char v) : value(v) {}
};

// String literal
struct EString : Expr {
    std::string value;
    explicit EString(const std::string& s) : value(s) {}
};

// Variable reference
struct EVar : Expr {
    std::string name;
    explicit EVar(const std::string& n) : name(n) {}
};

// Binary operator expression.  The `op` string holds the operator symbol
// exactly as it appears in the source (e.g. "+", "-", "<<", ">>", "||").
struct EBinOp : Expr {
    std::string op;
    Ptr<Expr> lhs;
    Ptr<Expr> rhs;
    EBinOp(const std::string& o, const Ptr<Expr>& a, const Ptr<Expr>& b)
        : op(o), lhs(a), rhs(b) {}
};

// Unary operator expression (prefix).  Unary operators include logical not
// ("!"), bitwise not ("~") and unary minus ("-").
struct EUnOp : Expr {
    std::string op;
    Ptr<Expr> expr;
    EUnOp(const std::string& o, const Ptr<Expr>& e) : op(o), expr(e) {}
};

// Function call expression.  Contains the function name and a list of
// argument expressions.
struct ECall : Expr {
    std::string func;
    std::vector<Ptr<Expr>> args;
    ECall(const std::string& f, const std::vector<Ptr<Expr>>& as)
        : func(f), args(as) {}
};

// New expression.  Represents `new T[n]` where T is a type and n is the
// number of elements.  In the AST we represent `new T[n]` as a pointer type
// to T with count expression `n`.  For example `new int[x]` becomes
// ENew(TypePointer(TInt), EVar("x")).
struct ENew : Expr {
    Type type;
    Ptr<Expr> count;
    ENew(const Type& t, const Ptr<Expr>& c) : type(t), count(c) {}
};

// Array access expression.  Represents `name[index]` optionally followed by
// a struct field selection.  The `field` member is empty when no field is
// selected.  In examples this is printed as EArrayAccess("a", EInt(2), "x").
struct EArrayAccess : Expr {
    std::string name;
    Ptr<Expr> index;
    std::string field; // empty if none
    EArrayAccess(const std::string& n, const Ptr<Expr>& idx, const std::string& fld)
        : name(n), index(idx), field(fld) {}
};

// Base class for all statements
struct Stmt {
    virtual ~Stmt() = default;
};

// Variable assignment: x = expr;
struct SVarAssign : Stmt {
    std::string name;
    Ptr<Expr> rhs;
    SVarAssign(const std::string& n, const Ptr<Expr>& r) : name(n), rhs(r) {}
};

// Assignment to an array element (with optional struct field)
struct SArrayAssign : Stmt {
    std::string name;
    Ptr<Expr> index;
    std::string field;
    Ptr<Expr> rhs;
    SArrayAssign(const std::string& n, const Ptr<Expr>& idx, const std::string& fld, const Ptr<Expr>& r)
        : name(n), index(idx), field(fld), rhs(r) {}
};

// Return statement.  The `value` may be nullptr for a bare return;
// otherwise it points to the return expression.
struct SReturn : Stmt {
    Ptr<Expr> value;
    explicit SReturn(const Ptr<Expr>& v) : value(v) {}
};

// Standalone expression statement, e.g. a function call or an expression on
// its own line.
struct SExpr : Stmt {
    Ptr<Expr> expr;
    explicit SExpr(const Ptr<Expr>& e) : expr(e) {}
};

// If statement.  The `elseBranch` pointer may be null for an if without
// else.
struct SIf : Stmt {
    Ptr<Expr> cond;
    Ptr<Stmt> thenBranch;
    Ptr<Stmt> elseBranch;
    SIf(const Ptr<Expr>& c, const Ptr<Stmt>& t, const Ptr<Stmt>& e)
        : cond(c), thenBranch(t), elseBranch(e) {}
};

// While loop
struct SWhile : Stmt {
    Ptr<Expr> cond;
    Ptr<Stmt> body;
    SWhile(const Ptr<Expr>& c, const Ptr<Stmt>& b) : cond(c), body(b) {}
};

// Block of statements surrounded by braces { ... }.  This is the core
// structure used for scopes and function bodies.
struct SBlock : Stmt {
    std::vector<Ptr<Stmt>> stmts;
    SBlock() = default;
};

// Break statement
struct SBreak : Stmt {};

// Local variable definition.  The `init` pointer may be null for an
// uninitialised local variable.
struct SVarDef : Stmt {
    Type type;
    std::string name;
    Ptr<Expr> init;
    SVarDef(const Type& t, const std::string& n, const Ptr<Expr>& i)
        : type(t), name(n), init(i) {}
};

// Delete statement.  Represents `delete[] expr;` or `delete expr;`.  The
// `isArray` flag indicates whether this was `delete[]` (true) or just
// `delete` (false).
struct SDelete : Stmt {
    bool isArray;
    Ptr<Expr> expr;
    SDelete(bool arr, const Ptr<Expr>& e) : isArray(arr), expr(e) {}
};

// Function parameter
struct FuncParam {
    Type type;
    std::string name;
    FuncParam(const Type& t, const std::string& n) : type(t), name(n) {}
};

// Function definition
struct FuncDef {
    Type retType;
    std::string name;
    std::vector<FuncParam> params;
    Ptr<SBlock> body;
};

// External function declaration
struct ExternDecl {
    Type type;
    std::string name;
    std::vector<FuncParam> params;
};

// Global variable declaration (no initializer)
struct GVarDecl {
    Type type;
    std::string name;
};

// Global variable definition (with initializer)
struct GVarDef {
    Type type;
    std::string name;
    Ptr<Expr> init;
};

// Struct definition.  Contains the name of the struct and a list of fields.
struct StructField {
    Type type;
    std::string name;
    StructField(const Type& t, const std::string& n) : type(t), name(n) {}
};

struct GStruct {
    std::string name;
    std::vector<StructField> fields;
};

// AST root node representing a whole program.  Contains lists of
// extern declarations, global variable declarations/definitions, struct
// definitions and functions.
struct ASTProgram {
    std::vector<ExternDecl> externs;
    std::vector<GVarDecl> globals;
    std::vector<GVarDef> globalDefs;
    std::vector<GStruct> structs;
    std::vector<FuncDef> funcs;
};

} // namespace cigrid