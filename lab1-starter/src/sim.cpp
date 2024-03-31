/**
 * Name: Pusanisa Jarusiripat
 * ID: 6580564
 * 
 * 
 * To implement:
 *    - J-type (Jump) Instructions:
 *       J    - Jump
 *       JAL  - Jump and Link
 * 
 *    - R-type (Register) Instructions:
 *       These operate on registers and have an opcode of 0x00. They use the instr field to determine the specific operation.
 *       ADD   - Add
 *       ADDU  - Add Unsigned
 *       SUB   - Subtract
 *       SUBU  - Subtract Unsigned
 *       AND   - Bitwise AND
 *       OR    - Bitwise OR
 *       XOR   - Bitwise XOR
 *       NOR   - Bitwise NOR
 *       SLT   - Set on Less Than
 *       SLTU  - Set on Less Than Unsigned
 *       SLL   - Shift Left Logical
 *       SRL   - Shift Right Logical
 *       SRA   - Shift Right Arithmetic
 *       SLLV  - Shift Left Logical Variable
 *       SRLV  - Shift Right Logical Variable
 *       SRAV  - Shift Right Arithmetic Variable
 *       JR    - Jump Register
 *       JALR  - Jump and Link Register
 *       MULT  - Multiply
 *       MULTU - Multiply Unsigned
 *       DIV   - Divide
 *       DIVU  - Divide Unsigned
 *       MFHI  - Move From HI Register
 *       MFLO  - Move From LO Register
 *       MTHI  - Move To HI Register
 *       MTLO  - Move To LO Register
 *       SYSCALL - System Call
 *
 *    - I-type (Immediate) Instructions:
 *       These instructions use immediate values and include arithmetic operations, memory access, and branches.
 *       ADDI  - Add Immediate
 *       ADDIU - Add Immediate Unsigned
 *       SLTI  - Set Less Than Immediate
 *       SLTIU - Set Less Than Immediate Unsigned
 *       ANDI  - AND Immediate
 *       ORI   - OR Immediate
 *       XORI  - XOR Immediate
 *       LUI   - Load Upper Immediate
 *       LB    - Load Byte
 *       LH    - Load Halfword
 *       LW    - Load Word
 *       LBU   - Load Byte Unsigned
 *       LHU   - Load Halfword Unsigned
 *       SB    - Store Byte
 *       SH    - Store Halfword
 *       SW    - Store Word
 *       BEQ   - Branch on Equal
 *       BNE   - Branch on Not Equal
 *       BLEZ  - Branch on Less Than or Equal to Zero
 *       BGTZ  - Branch on Greater Than Zero
 *       BLTZ  - Branch on Less Than Zero
 *       BGEZ  - Branch on Greater Than or Equal to Zero
 *       BLTZAL - Branch on Less Than Zero And Link
 *       BGEZAL - Branch on Greater Than or Equal to Zero And Link
 * 
 */



#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include "shell.h"

void process_instruction();
void check_syscall();
void R_type(uint32_t instruction);
void I_type(uint32_t instruction);
void J_type(uint32_t instruction);

void process_instruction() {

    // Fetch the instruction
    uint32_t instruction = mem_read_32(CURRENT_STATE.PC);

    // handle the type and execute the instruction
    // check the opcode
    uint32_t opcode = instruction >> 26;

    // if the register $v0  has value 0x0A (decimal 10) when SYSCALL is executed, 
    // then the go command should stop its simulation loop and return to the simulator shell’s prompt
    // other than that just keep running
    // after execute the instruction we move the pc to the next instruction
    switch (opcode) {
        case 0x00:
            printf("R-type\n");
            R_type(instruction);
            break;
        case 0x02:
        case 0x03:
            printf("J-type\n");
            J_type(instruction);
            break;
        default:
            printf("I-type\n");
            I_type(instruction);
            break;
    }

    // move the pc to the next instruction
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}


void check_syscall() {
    uint32_t syscall_number = CURRENT_STATE.REGS[2];
    printf("Syscall invoked with code: %u\n", syscall_number);
    
    switch(syscall_number) {
        case 0xA: // exit syscall
            printf("Exit syscall. Halting simulator.\n");
            RUN_BIT = 0;
            break;
        // Example: Add more cases here for other syscalls
        // case 0x5: // read integer syscall
        //     // Implementation to read an integer from input
        //     break;
        default:
            printf("Unknown syscall: %u\n or nothing", syscall_number);
            // Decide on behavior: halt, ignore, or default action
            break;
    }
}

int addition_overflows(int32_t a, int32_t b) {
    int64_t result = (int64_t)a + (int64_t)b;
    return result > INT_MAX || result < INT_MIN;
}

void R_type(uint32_t instruction) {
    uint32_t instr = instruction & 0x3F;
    uint32_t rs = (instruction >> 21) & 0x1F;
    uint32_t rt = (instruction >> 16) & 0x1F;
    uint32_t rd = (instruction >> 11) & 0x1F;
    uint32_t shamt = (instruction >> 6) & 0x1F;

    switch (instr) {
        case 0x20:
            // ADD
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
            break;
        case 0x21:
            // ADDU
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
            break;
        case 0x22:
            // SUB
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
            break;
        case 0x23:
            // SUBU
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
            break;
        case 0x24:
            // AND
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
            break;
        case 0x25:
            // OR
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt];
            break;
        case 0x26:
            // XOR
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
            break;
        case 0x27:
            // NOR
            NEXT_STATE.REGS[rd] = ~(CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]);
            break;
        case 0x2A:
            // SLT
            NEXT_STATE.REGS[rd] = (CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt]) ? 1 : 0;
            break;
        case 0x2B:
            // SLTU
            NEXT_STATE.REGS[rd] = (CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt]) ? 1 : 0;
            break;
        case 0x00:
            // SLL
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
            break;
        case 0x02:
            // SRL
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
            break;
        case 0x03:
            // SRA
            NEXT_STATE.REGS[rd] = (int32_t) CURRENT_STATE.REGS[rt] >> shamt;
            break;
        case 0x04:
            // SLLV
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << CURRENT_STATE.REGS[rs];
            break;
        case 0x06:
            // SRLV
            NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> CURRENT_STATE.REGS[rs];
            break;
        case 0x07:
            // SRAV
            NEXT_STATE.REGS[rd] = (int32_t) CURRENT_STATE.REGS[rt] >> CURRENT_STATE.REGS[rs];
            break;
        case 0x08:
            // JR
            NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
            break;
        case 0x09:
            // JALR
            NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
            break;
        case 0x18:
            // MULT
            {
                int64_t result = (int64_t) CURRENT_STATE.REGS[rs] * (int64_t) CURRENT_STATE.REGS[rt];
                NEXT_STATE.HI = (result >> 32) & 0xFFFFFFFF;
                NEXT_STATE.LO = result & 0xFFFFFFFF;
            }
            break;
        case 0x19:
            // MULTU
            {
                uint64_t result = (uint64_t) CURRENT_STATE.REGS[rs] * (uint64_t) CURRENT_STATE.REGS[rt];
                NEXT_STATE.HI = (result >> 32) & 0xFFFFFFFF;
                NEXT_STATE.LO = result & 0xFFFFFFFF;
            }
            break;
        case 0x1A:
            // DIV
            NEXT_STATE.LO = (int32_t) CURRENT_STATE.REGS[rs] / (int32_t) CURRENT_STATE.REGS[rt];
            NEXT_STATE.HI = (int32_t) CURRENT_STATE.REGS[rs] % (int32_t) CURRENT_STATE.REGS[rt];
            break;
        case 0x1B:
            // DIVU
            NEXT_STATE.LO = CURRENT_STATE.REGS[rs] / CURRENT_STATE.REGS[rt];
            NEXT_STATE.HI = CURRENT_STATE.REGS[rs] % CURRENT_STATE.REGS[rt];
            break;
        case 0x10:
            // MFHI
            NEXT_STATE.REGS[rd] = CURRENT_STATE.HI;
            break;
        case 0x12:
            // MFLO
            NEXT_STATE.REGS[rd] = CURRENT_STATE.LO;
            break;
        case 0x11:
            // MTHI
            NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
            break;
        case 0x13:
            // MTLO
            NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
            break;
        case 0x0C:
            // SYSCALL
            check_syscall();
            break;
        default:
            printf("Unknown R-type instruction: 0x%08x\n", instruction);
            break;
    }   
}

void I_type(uint32_t instruction) {
    uint32_t opcode = instruction >> 26;
    uint32_t rs = (instruction >> 21) & 0x1F; // Source register
    uint32_t rt = (instruction >> 16) & 0x1F; // Target register
    int32_t signed_imm = (int32_t)(int16_t)(instruction & 0xFFFF);
    printf("rs: %d rt: %d signed_imm: %d\n", rs, rt, signed_imm);
    switch (opcode) {
        case 0x08: // ADDI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + signed_imm;
            break;
        case 0x09: // ADDIU
            printf("next stage rt: %d curr rs: %d signed imm: %d\n", NEXT_STATE.REGS[rt], CURRENT_STATE.REGS[rs], signed_imm);
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + signed_imm;
            break;
        case 0x0A: // SLTI
            NEXT_STATE.REGS[rt] = ((int32_t)CURRENT_STATE.REGS[rs] < signed_imm) ? 1 : 0;
            break;
        case 0x0B: // SLTIU
            NEXT_STATE.REGS[rt] = (CURRENT_STATE.REGS[rs] < (uint32_t)signed_imm) ? 1 : 0;
            break;
        case 0x0C: // ANDI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & signed_imm; // ANDI uses zero-extension
            break;
        case 0x0D: // ORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | signed_imm; // ORI uses zero-extension
            break;
        case 0x0E: // XORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ signed_imm; // XORI uses zero-extension
            break;
        case 0x0F: // LUI
            NEXT_STATE.REGS[rt] = signed_imm << 16;
            break;
        // Memory access and branch instructions omitted for brevity...
        case 0x04: // BEQ
            if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4 + (signed_imm << 2);
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Only update if not branching
            }
            return; // Prevent further PC modification at the end of the function
        case 0x05: // BNE
            if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4 + (signed_imm << 2);
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4; // Only update if not branching
            }
            return; // Prevent further PC modification at the end of the function
        // Add additional cases for other I-type instructions...
        default:
            printf("Unknown I-type instruction: 0x%08x\n", instruction);
            break;
    }
}

void J_type(uint32_t instruction) {
    uint32_t opcode = instruction >> 26;
    uint32_t target = instruction & 0x3FFFFFF;

    switch (opcode) {
        case 0x02:
            // J
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xF0000000) | (target << 2);
            break;
        case 0x03:
            // JAL
            NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xF0000000) | (target << 2);
            break;
        default:
            printf("Unknown J-type instruction: 0x%08x\n", instruction);
            break;
    }
}


