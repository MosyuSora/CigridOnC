#pragma once

#include "ir.hpp"
#include <string>
#include <vector>

namespace cigrid {

enum class AsmOperandKind { Reg, Imm, StackSlot };

enum class AsmRegister { RAX, RSP };

struct AsmOperand {
    AsmOperandKind kind;
    AsmRegister reg;
    long long imm{};
    size_t stackOffset{}; // bytes from rsp

    static AsmOperand regOp(AsmRegister r) { return {AsmOperandKind::Reg, r, 0, 0}; }
    static AsmOperand immOp(long long v) { return {AsmOperandKind::Imm, AsmRegister::RAX, v, 0}; }
    static AsmOperand stackOp(size_t offset) { return {AsmOperandKind::StackSlot, AsmRegister::RAX, 0, offset}; }
};

struct AsmInstruction {
    std::string mnemonic;
    std::vector<AsmOperand> operands;
};

struct AsmProgram {
    std::vector<std::string> directives;
    std::string entryLabel;
    std::vector<AsmInstruction> instructions;
};

AsmProgram generate_asm(const IrFunction &func);
std::string format_asm(const AsmProgram &prog);

} // namespace cigrid

