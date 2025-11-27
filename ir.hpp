#pragma once

#include "ast.hpp"
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cigrid {

struct IrValue {
    bool isConst;
    long long imm;
    std::string var;

    static IrValue constant(long long v) { return {true, v, ""}; }
    static IrValue variable(const std::string &v) { return {false, 0, v}; }
};

enum class IrBinOpKind { Add, Sub };

struct IrAssignConst {
    std::string dest;
    long long value;
};

struct IrAssignVar {
    std::string dest;
    std::string src;
};

struct IrAssignBinOp {
    std::string dest;
    IrValue lhs;
    IrValue rhs;
    IrBinOpKind op;
};

struct IrReturn {
    IrValue value;
};

using IrInstr = std::variant<IrAssignConst, IrAssignVar, IrAssignBinOp, IrReturn>;

struct IrFunction {
    std::vector<std::string> locals;
    std::vector<IrInstr> instrs;
};

// Build a linear IR for a restricted int main() consisting of straight-line
// statements. Throws std::runtime_error on unsupported constructs.
IrFunction build_linear_ir(const ASTProgram &prog);

} // namespace cigrid

