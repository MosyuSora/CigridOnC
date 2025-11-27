#include "ir.hpp"

#include <stdexcept>
#include <unordered_map>

namespace cigrid {
namespace {

bool is_int_type(const Type &t) {
    return t.kind == Type::Kind::Base && t.base == BaseType::Int;
}

class IrBuilder {
public:
    explicit IrBuilder(const FuncDef &fn) : func(fn) {}

    IrFunction build() {
        if (!func.body) {
            throw std::runtime_error("main is missing a body");
        }
        for (const auto &stmt : func.body->stmts) {
            lower_stmt(stmt);
        }
        return IrFunction{locals, instrs};
    }

private:
    const FuncDef &func;
    std::vector<std::string> locals;
    std::unordered_map<std::string, size_t> localIndex;
    std::vector<IrInstr> instrs;
    int tempCounter = 0;

    void add_local(const std::string &name) {
        if (localIndex.count(name)) {
            throw std::runtime_error("Duplicate local variable: " + name);
        }
        localIndex[name] = locals.size();
        locals.push_back(name);
    }

    IrValue lower_expr(const Ptr<Expr> &expr, const std::string *preferredDest = nullptr) {
        if (!expr) {
            throw std::runtime_error("Missing expression");
        }

        if (auto eint = std::dynamic_pointer_cast<EInt>(expr)) {
            return IrValue::constant(eint->value);
        }
        if (auto evar = std::dynamic_pointer_cast<EVar>(expr)) {
            if (!localIndex.count(evar->name)) {
                throw std::runtime_error("Unknown variable: " + evar->name);
            }
            return IrValue::variable(evar->name);
        }
        if (auto bin = std::dynamic_pointer_cast<EBinOp>(expr)) {
            IrBinOpKind opKind;
            if (bin->op == "+") {
                opKind = IrBinOpKind::Add;
            } else if (bin->op == "-") {
                opKind = IrBinOpKind::Sub;
            } else {
                throw std::runtime_error("Unsupported binary operator: " + bin->op);
            }

            IrValue lhs = lower_expr(bin->lhs);
            IrValue rhs = lower_expr(bin->rhs);

            std::string dest;
            if (preferredDest) {
                dest = *preferredDest;
            } else {
                dest = fresh_temp();
            }
            instrs.push_back(IrAssignBinOp{dest, lhs, rhs, opKind});
            ensure_local_exists(dest);
            return IrValue::variable(dest);
        }

        throw std::runtime_error("Unsupported expression type in --asm mode");
    }

    void ensure_local_exists(const std::string &name) {
        if (!localIndex.count(name)) {
            add_local(name);
        }
    }

    std::string fresh_temp() {
        std::string name = "__tmp" + std::to_string(tempCounter++);
        add_local(name);
        return name;
    }

    void lower_stmt(const Ptr<Stmt> &stmt) {
        if (auto def = std::dynamic_pointer_cast<SVarDef>(stmt)) {
            if (!is_int_type(def->type)) {
                throw std::runtime_error("Only int locals are supported in --asm mode");
            }
            add_local(def->name);
            IrValue val;
            if (def->init) {
                val = lower_expr(def->init, &def->name);
            } else {
                val = IrValue::constant(0);
            }
            emit_assignment(def->name, val);
            return;
        }
        if (auto asg = std::dynamic_pointer_cast<SVarAssign>(stmt)) {
            if (!localIndex.count(asg->name)) {
                throw std::runtime_error("Assignment to undeclared variable: " + asg->name);
            }
            IrValue rhs = lower_expr(asg->rhs, &asg->name);
            emit_assignment(asg->name, rhs);
            return;
        }
        if (auto ret = std::dynamic_pointer_cast<SReturn>(stmt)) {
            if (!ret->value) {
                throw std::runtime_error("return without value is unsupported in --asm mode");
            }
            IrValue val = lower_expr(ret->value);
            instrs.push_back(IrReturn{val});
            return;
        }

        throw std::runtime_error("Unsupported statement in --asm mode");
    }

    void emit_assignment(const std::string &name, const IrValue &val) {
        if (val.isConst) {
            instrs.push_back(IrAssignConst{name, val.imm});
        } else if (val.var == name && !instrs.empty()) {
            // Already computed directly into destination; nothing more to emit.
        } else if (!val.var.empty()) {
            instrs.push_back(IrAssignVar{name, val.var});
        } else {
            throw std::runtime_error("Invalid assignment value");
        }
    }
};

const FuncDef *find_main(const ASTProgram &prog) {
    const FuncDef *mainFn = nullptr;
    for (const auto &fn : prog.funcs) {
        if (fn.name == "main") {
            if (mainFn) {
                throw std::runtime_error("Multiple definitions of main are not supported");
            }
            mainFn = &fn;
        }
    }
    if (!mainFn) {
        throw std::runtime_error("No main function found");
    }
    if (!is_int_type(mainFn->retType)) {
        throw std::runtime_error("main must return int in --asm mode");
    }
    if (!mainFn->params.empty()) {
        throw std::runtime_error("main with parameters is unsupported in --asm mode");
    }
    return mainFn;
}

} // namespace

IrFunction build_linear_ir(const ASTProgram &prog) {
    const FuncDef *mainFn = find_main(prog);
    IrBuilder builder(*mainFn);
    return builder.build();
}

} // namespace cigrid

