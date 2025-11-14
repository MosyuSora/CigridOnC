#include "pprint.hpp"

#include <iostream>
#include <sstream>

using namespace std;

namespace cigrid {

// Helper forward declarations
static void printType(const Type &t, ostream &out);
static void printExpr(const Ptr<Expr> &e, ostream &out);
static void printStmt(const Ptr<Stmt> &s, int indent, ostream &out);
static string indentStr(int indent);

// Entry point
void pprint(const ASTProgram &prog) {
    // Print extern declarations
    for (const auto &ex : prog.externs) {
        cout << "GFuncDecl(";
        printType(ex.type, cout);
        cout << ", \"" << ex.name << "\", {";
        for (size_t i = 0; i < ex.paramTypes.size(); i++) {
            cout << "(";
            printType(ex.paramTypes[i], cout);
            cout << ")";
            if (i + 1 < ex.paramTypes.size())
                cout << " ";
        }
        cout << "})" << endl << endl;
    }
    // Print global variable declarations
    for (const auto &gv : prog.globals) {
        cout << "GVarDecl(";
        printType(gv.type, cout);
        cout << ", \"" << gv.name << "\")" << endl << endl;
    }
    // Print global variable definitions
    for (const auto &gv : prog.globalDefs) {
        cout << "GVarDef(";
        printType(gv.type, cout);
        cout << ", \"" << gv.name << "\", ";
        printExpr(gv.init, cout);
        cout << ")" << endl << endl;
    }
    // Print struct definitions
    for (const auto &st : prog.structs) {
        cout << "GStruct(\"" << st.name << "\",{" << endl;
        for (const auto &field : st.fields) {
            cout << "  (";
            printType(field.type, cout);
            cout << ", \"" << field.name << "\")" << endl;
        }
        cout << "})" << endl << endl;
    }
    // Print function definitions
    for (const auto &fn : prog.funcs) {
        cout << "GFuncDef(";
        printType(fn.retType, cout);
        cout << ", \"" << fn.name << "\", {";
        // parameters
        for (size_t i = 0; i < fn.params.size(); i++) {
            const auto &p = fn.params[i];
            cout << "(";
            printType(p.type, cout);
            cout << ", \"" << p.name << "\")";
            if (i + 1 < fn.params.size())
                cout << " ";
        }
        cout << "}," << endl;
        // body
        printStmt(fn.body, 1, cout);
        cout << ")" << endl << endl;
    }
}

// Generate indentation string
static string indentStr(int indent) {
    return string(indent * 2, ' ');
}

// Print a Type to out stream
static void printType(const Type &t, ostream &out) {
    switch (t.kind) {
    case Type::Kind::Base:
        switch (t.base) {
        case BaseType::Int: out << "TInt"; break;
        case BaseType::Char: out << "TChar"; break;
        case BaseType::Void: out << "TVoid"; break;
        }
        break;
    case Type::Kind::Pointer:
        out << "TPoint(";
        printType(*t.pointee, out);
        out << ")";
        break;
    case Type::Kind::Ident:
        out << "TIdent(\"" << t.ident << "\")";
        break;
    }
}

// Print an expression.  This prints expressions without introducing
// additional indentation or line breaks (all on one line).  Sub
// expressions are nested inside parentheses as needed.
static void printExpr(const Ptr<Expr> &e, ostream &out) {
    if (!e) {
        out << "";
        return;
    }
    if (auto ie = dynamic_pointer_cast<EInt>(e)) {
        out << "EInt(" << ie->value << ")";
    } else if (auto ce = dynamic_pointer_cast<EChar>(e)) {
        // Print character with escape if needed
        out << "EChar('";
        char c = ce->value;
        switch (c) {
        case '\\': out << "\\\\"; break;
        case '\n': out << "\\n"; break;
        case '\t': out << "\\t"; break;
        case '\r': out << "\\r"; break;
        case '\'': out << "\\\'"; break;
        case '"': out << "\\\""; break;
        default: out << c; break;
        }
        out << "')";
    } else if (auto se = dynamic_pointer_cast<EString>(e)) {
        out << "EString(\"";
        for (char c : se->value) {
            switch (c) {
            case '\\': out << "\\\\"; break;
            case '\n': out << "\\n"; break;
            case '\t': out << "\\t"; break;
            case '\r': out << "\\r"; break;
            case '\'': out << "\\\'"; break;
            case '"': out << "\\\""; break;
            default: out << c; break;
            }
        }
        out << "\")";
    } else if (auto ve = dynamic_pointer_cast<EVar>(e)) {
        out << "EVar(\"" << ve->name << "\")";
    } else if (auto be = dynamic_pointer_cast<EBinOp>(e)) {
        out << "EBinOp(" << be->op << ", ";
        printExpr(be->lhs, out);
        out << ", ";
        printExpr(be->rhs, out);
        out << ")";
    } else if (auto ue = dynamic_pointer_cast<EUnOp>(e)) {
        out << "EUnOp(" << ue->op << ", ";
        printExpr(ue->expr, out);
        out << ")";
    } else if (auto ce = dynamic_pointer_cast<ECall>(e)) {
        out << "ECall(\"" << ce->func << "\",{";
        for (size_t i = 0; i < ce->args.size(); i++) {
            printExpr(ce->args[i], out);
            if (i + 1 < ce->args.size()) out << " ";
        }
        out << "})";
    } else if (auto ne = dynamic_pointer_cast<ENew>(e)) {
        out << "ENew(";
        printType(ne->type, out);
        out << ", ";
        printExpr(ne->count, out);
        out << ")";
    } else if (auto ae = dynamic_pointer_cast<EArrayAccess>(e)) {
        out << "EArrayAccess(\"" << ae->name << "\", ";
        printExpr(ae->index, out);
        out << ", ";
        if (!ae->field.empty()) {
            out << "\"" << ae->field << "\"";
        } else {
            // print nothing for field but leave blank
        }
        out << ")";
    } else {
        out << "/* unknown expr */";
    }
}

// Print a statement with indentation.  Each statement prints on its own
// line and may recurse for nested blocks.
static void printStmt(const Ptr<Stmt> &s, int indent, ostream &out) {
    string ind = indentStr(indent);
    if (!s) return;
    if (auto blk = dynamic_pointer_cast<SBlock>(s)) {
        out << ind << "SScope({" << endl;
        // print each statement inside
        for (const auto &st : blk->stmts) {
            printStmt(st, indent + 1, out);
        }
        out << ind << "})" << endl;
    } else if (auto va = dynamic_pointer_cast<SVarAssign>(s)) {
        out << ind << "SVarAssign(\"" << va->name << "\", ";
        printExpr(va->rhs, out);
        out << ")" << endl;
    } else if (auto aa = dynamic_pointer_cast<SArrayAssign>(s)) {
        out << ind << "SArrayAssign(\"" << aa->name << "\", ";
        printExpr(aa->index, out);
        out << ", ";
        if (!aa->field.empty()) {
            out << "\"" << aa->field << "\"";
        } else {
            // two spaces to match examples for blank field
            out << " ";
        }
        out << ", ";
        printExpr(aa->rhs, out);
        out << ")" << endl;
    } else if (auto rd = dynamic_pointer_cast<SReturn>(s)) {
        out << ind << "SReturn(";
        if (rd->value) {
            printExpr(rd->value, out);
        }
        out << ")" << endl;
    } else if (auto ex = dynamic_pointer_cast<SExpr>(s)) {
        out << ind << "SExpr(";
        printExpr(ex->expr, out);
        out << ")" << endl;
    } else if (auto vd = dynamic_pointer_cast<SVarDef>(s)) {
        out << ind << "SVarDef(";
        printType(vd->type, out);
        out << ", \"" << vd->name << "\", ";
        if (vd->init) {
            printExpr(vd->init, out);
        }
        out << ")" << endl;
    } else if (auto del = dynamic_pointer_cast<SDelete>(s)) {
        out << ind << "SDelete(";
        // We only print the variable name when possible.  If expr is EVar, extract name; otherwise print expr.
        if (auto ve = dynamic_pointer_cast<EVar>(del->expr)) {
            out << "\"" << ve->name << "\"";
        } else {
            printExpr(del->expr, out);
        }
        out << ")" << endl;
    } else if (auto br = dynamic_pointer_cast<SBreak>(s)) {
        out << ind << "SBreak" << endl;
    } else if (auto iff = dynamic_pointer_cast<SIf>(s)) {
        out << ind << "SIf(";
        printExpr(iff->cond, out);
        out << "," << endl;
        // then branch
        printStmt(iff->thenBranch, indent + 1, out);
        out << ind;
        out << ",";
        if (iff->elseBranch) {
            out << endl;
            printStmt(iff->elseBranch, indent + 1, out);
            out << ind;
        }
        out << ")" << endl;
    } else if (auto wh = dynamic_pointer_cast<SWhile>(s)) {
        out << ind << "SWhile(";
        printExpr(wh->cond, out);
        out << "," << endl;
        printStmt(wh->body, indent + 1, out);
        out << ind << ")" << endl;
    } else {
        out << ind << "/* unknown stmt */" << endl;
    }
}

} // namespace cigrid