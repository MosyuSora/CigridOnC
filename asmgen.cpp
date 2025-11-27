#include "asmgen.hpp"

#include <sstream>
#include <stdexcept>
#include <unordered_map>

namespace cigrid {
namespace {

std::string format_register(AsmRegister reg) {
    switch (reg) {
    case AsmRegister::RAX:
        return "rax";
    case AsmRegister::RSP:
        return "rsp";
    }
    throw std::runtime_error("unknown register");
}

std::string format_operand(const AsmOperand &op) {
    switch (op.kind) {
    case AsmOperandKind::Reg:
        return format_register(op.reg);
    case AsmOperandKind::Imm:
        return std::to_string(op.imm);
    case AsmOperandKind::StackSlot: {
        std::ostringstream oss;
        oss << "qword [rsp";
        if (op.stackOffset != 0) {
            oss << " + " << op.stackOffset;
        }
        oss << "]";
        return oss.str();
    }
    }
    throw std::runtime_error("unknown operand kind");
}

AsmInstruction make_mov(const AsmOperand &dst, const AsmOperand &src) {
    return {"mov", {dst, src}};
}

AsmInstruction make_binop(const std::string &mnemonic, const AsmOperand &dst, const AsmOperand &src) {
    return {mnemonic, {dst, src}};
}

size_t stack_bytes(const IrFunction &func) { return func.locals.size() * 8; }

AsmOperand stack_for(const std::unordered_map<std::string, size_t> &offsets, const std::string &name) {
    auto it = offsets.find(name);
    if (it == offsets.end()) {
        throw std::runtime_error("Unknown stack slot for variable " + name);
    }
    return AsmOperand::stackOp(it->second * 8);
}

} // namespace

AsmProgram generate_asm(const IrFunction &func) {
    std::unordered_map<std::string, size_t> offsets;
    for (size_t i = 0; i < func.locals.size(); ++i) {
        offsets[func.locals[i]] = i;
    }

    AsmProgram prog;
    prog.directives = {"global main", "section .text"};
    prog.entryLabel = "main";

    size_t localsSize = stack_bytes(func);
    if (localsSize > 0) {
        prog.instructions.push_back({"sub", {AsmOperand::regOp(AsmRegister::RSP), AsmOperand::immOp(static_cast<long long>(localsSize))}});
    }

    for (const auto &instr : func.instrs) {
        if (std::holds_alternative<IrAssignConst>(instr)) {
            const auto &as = std::get<IrAssignConst>(instr);
            prog.instructions.push_back(make_mov(stack_for(offsets, as.dest), AsmOperand::immOp(as.value)));
        } else if (std::holds_alternative<IrAssignVar>(instr)) {
            const auto &as = std::get<IrAssignVar>(instr);
            prog.instructions.push_back(make_mov(AsmOperand::regOp(AsmRegister::RAX), stack_for(offsets, as.src)));
            prog.instructions.push_back(make_mov(stack_for(offsets, as.dest), AsmOperand::regOp(AsmRegister::RAX)));
        } else if (std::holds_alternative<IrAssignBinOp>(instr)) {
            const auto &as = std::get<IrAssignBinOp>(instr);
            // Move lhs into rax
            if (as.lhs.isConst) {
                prog.instructions.push_back(make_mov(AsmOperand::regOp(AsmRegister::RAX), AsmOperand::immOp(as.lhs.imm)));
            } else {
                prog.instructions.push_back(make_mov(AsmOperand::regOp(AsmRegister::RAX), stack_for(offsets, as.lhs.var)));
            }

            AsmOperand rhsOp = as.rhs.isConst ? AsmOperand::immOp(as.rhs.imm) : stack_for(offsets, as.rhs.var);
            if (as.op == IrBinOpKind::Add) {
                prog.instructions.push_back(make_binop("add", AsmOperand::regOp(AsmRegister::RAX), rhsOp));
            } else {
                prog.instructions.push_back(make_binop("sub", AsmOperand::regOp(AsmRegister::RAX), rhsOp));
            }
            prog.instructions.push_back(make_mov(stack_for(offsets, as.dest), AsmOperand::regOp(AsmRegister::RAX)));
        } else if (std::holds_alternative<IrReturn>(instr)) {
            const auto &ret = std::get<IrReturn>(instr);
            if (ret.value.isConst) {
                prog.instructions.push_back(make_mov(AsmOperand::regOp(AsmRegister::RAX), AsmOperand::immOp(ret.value.imm)));
            } else {
                prog.instructions.push_back(make_mov(AsmOperand::regOp(AsmRegister::RAX), stack_for(offsets, ret.value.var)));
            }
            if (localsSize > 0) {
                prog.instructions.push_back({"add", {AsmOperand::regOp(AsmRegister::RSP), AsmOperand::immOp(static_cast<long long>(localsSize))}});
            }
            prog.instructions.push_back({"ret", {}});
            break;
        }
    }

    return prog;
}

std::string format_asm(const AsmProgram &prog) {
    std::ostringstream oss;
    for (const auto &dir : prog.directives) {
        oss << dir << "\n";
    }
    oss << prog.entryLabel << ":\n";
    for (const auto &instr : prog.instructions) {
        oss << "\t" << instr.mnemonic;
        if (!instr.operands.empty()) {
            oss << " ";
            for (size_t i = 0; i < instr.operands.size(); ++i) {
                if (i > 0) {
                    oss << ", ";
                }
                oss << format_operand(instr.operands[i]);
            }
        }
        oss << "\n";
    }
    return oss.str();
}

} // namespace cigrid

