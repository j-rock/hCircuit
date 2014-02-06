
module Alu where

mux2 a b control =
  
// output = A (when control == 0) or B (when control == 1)
module mux2(out, A, B, control);
    output out;
    input  A, B;
    input  control;
    wire   wA, wB, not_control;
         
    not n1(not_control, control);
    and a1(wA, A, not_control);
    and a2(wB, B, control);
    or  o1(out, wA, wB);
endmodule // mux2

// output = A (when control == 00) or B (when control == 01) or
//          C (when control == 10) or D (when control == 11)
module mux4(out, A, B, C, D, control);
    output      out;
    input       A, B, C, D;
    input [1:0] control;
    wire        w1, w2;

    mux2 m1(w1, A, B, control[0]);
    mux2 m2(w2, C, D, control[0]);
    mux2 m3(out, w1, w2, control[1]);

endmodule // mux4


`include "logicunit.v"

module full_adder(sum, cout, a, b, cin);
    output sum, cout;
    input  a, b, cin;
    wire   partial_s, partial_c1, partial_c2;

    xor x0(partial_s, a, b);
    xor x1(sum, partial_s, cin);
    and a0(partial_c1, a, b);
    and a0(partial_c2, partial_s, cin);
    or  o1(cout, partial_c1, partial_c2);
endmodule // full_adder

`define ALU_ADD    3'h2
`define ALU_SUB    3'h3
`define ALU_AND    3'h4
`define ALU_OR     3'h5
`define ALU_NOR    3'h6
`define ALU_XOR    3'h7

// 01x - arithmetic, 1xx - logic
module alu1(out, carryout, A, B, carryin, control);
    output      out, carryout;
    input       A, B, carryin;
    input [2:0] control;
    wire        l1, a1, b1;

    xor x2(b1, B, control[0]);
    full_adder f1(a1, carryout, A, b1, carryin); 

    logicunit l2(l1, A, B, control[1:0]);
    mux2 m1(out, a1, l1, control[2]);

endmodule // alu1

module alu32(out, overflow, zero, negative, A, B, control);
    output [31:0] out;
    output        overflow, zero, negative;
    input  [31:0] A, B;
    input   [2:0] control;
    wire   [31:0] cout;

    alu1 a0(out[0], cout[0], A[0], B[0], control[0], control);
    alu1 a1(out[1], cout[1], A[1], B[1], cout[0], control);
    alu1 a2(out[2], cout[2], A[2], B[2], cout[1], control);
    alu1 a3(out[3], cout[3], A[3], B[3], cout[2], control);
    alu1 a4(out[4], cout[4], A[4], B[4], cout[3], control);
    alu1 a5(out[5], cout[5], A[5], B[5], cout[4], control);
    alu1 a6(out[6], cout[6], A[6], B[6], cout[5], control);
    alu1 a7(out[7], cout[7], A[7], B[7], cout[6], control);
    alu1 a8(out[8], cout[8], A[8], B[8], cout[7], control);
    alu1 a9(out[9], cout[9], A[9], B[9], cout[8], control);
    alu1 a10(out[10], cout[10], A[10], B[10], cout[9], control);
    alu1 a11(out[11], cout[11], A[11], B[11], cout[10], control);
    alu1 a12(out[12], cout[12], A[12], B[12], cout[11], control);
    alu1 a13(out[13], cout[13], A[13], B[13], cout[12], control);
    alu1 a14(out[14], cout[14], A[14], B[14], cout[13], control);
    alu1 a15(out[15], cout[15], A[15], B[15], cout[14], control);
    alu1 a16(out[16], cout[16], A[16], B[16], cout[15], control);
    alu1 a17(out[17], cout[17], A[17], B[17], cout[16], control);
    alu1 a18(out[18], cout[18], A[18], B[18], cout[17], control);
    alu1 a19(out[19], cout[19], A[19], B[19], cout[18], control);
    alu1 a20(out[20], cout[20], A[20], B[20], cout[19], control);
    alu1 a21(out[21], cout[21], A[21], B[21], cout[20], control);
    alu1 a22(out[22], cout[22], A[22], B[22], cout[21], control);
    alu1 a23(out[23], cout[23], A[23], B[23], cout[22], control);
    alu1 a24(out[24], cout[24], A[24], B[24], cout[23], control);
    alu1 a25(out[25], cout[25], A[25], B[25], cout[24], control);
    alu1 a26(out[26], cout[26], A[26], B[26], cout[25], control);
    alu1 a27(out[27], cout[27], A[27], B[27], cout[26], control);
    alu1 a28(out[28], cout[28], A[28], B[28], cout[27], control);
    alu1 a29(out[29], cout[29], A[29], B[29], cout[28], control);
    alu1 a30(out[30], cout[30], A[30], B[30], cout[29], control);
    alu1 a31(out[31], cout[31], A[31], B[31], cout[30], control);

    xor x1(overflow, cout[30], cout[31]);  
    assign zero = (out == 0);
    assign negative = out[31];

endmodule // alu32
