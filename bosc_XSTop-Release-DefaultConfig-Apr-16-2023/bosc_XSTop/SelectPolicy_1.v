module SelectPolicy_1(
  input  [7:0] io_validVec,
  output [7:0] io_allocate_0_bits,
  output [7:0] io_allocate_1_bits,
  input  [7:0] io_request,
  output       io_grant_0_valid,
  output [7:0] io_grant_0_bits
);
  wire  emptyVec_0 = ~io_validVec[0]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_1 = ~io_validVec[1]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_2 = ~io_validVec[2]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_3 = ~io_validVec[3]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_4 = ~io_validVec[4]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_5 = ~io_validVec[5]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_6 = ~io_validVec[6]; // @[SelectPolicy.scala 44:55]
  wire  emptyVec_7 = ~io_validVec[7]; // @[SelectPolicy.scala 44:55]
  wire  matrix__1_0 = ~(|emptyVec_3); // @[BitUtils.scala 257:50]
  wire [1:0] _matrix_2_0_T = {emptyVec_3,emptyVec_2}; // @[Cat.scala 31:58]
  wire  matrix__2_0 = ~(|_matrix_2_0_T); // @[BitUtils.scala 257:50]
  wire [2:0] _matrix_3_0_T = {emptyVec_3,emptyVec_2,emptyVec_1}; // @[Cat.scala 31:58]
  wire  matrix__3_0 = ~(|_matrix_3_0_T); // @[BitUtils.scala 257:50]
  wire [3:0] _matrix_4_0_T = {emptyVec_3,emptyVec_2,emptyVec_1,emptyVec_0}; // @[Cat.scala 31:58]
  wire  matrix__4_0 = ~(|_matrix_4_0_T); // @[BitUtils.scala 257:50]
  wire [4:0] _matrix_5_0_T = {emptyVec_3,emptyVec_2,emptyVec_1,emptyVec_0,emptyVec_7}; // @[Cat.scala 31:58]
  wire  matrix__5_0 = ~(|_matrix_5_0_T); // @[BitUtils.scala 257:50]
  wire [5:0] _matrix_6_0_T = {emptyVec_3,emptyVec_2,emptyVec_1,emptyVec_0,emptyVec_7,emptyVec_6}; // @[Cat.scala 31:58]
  wire  matrix__6_0 = ~(|_matrix_6_0_T); // @[BitUtils.scala 257:50]
  wire [6:0] _matrix_7_0_T = {emptyVec_3,emptyVec_2,emptyVec_1,emptyVec_0,emptyVec_7,emptyVec_6,emptyVec_5}; // @[Cat.scala 31:58]
  wire  matrix__7_0 = ~(|_matrix_7_0_T); // @[BitUtils.scala 257:50]
  wire  matrix_1_1_0 = ~(|emptyVec_4); // @[BitUtils.scala 257:50]
  wire [1:0] _matrix_2_0_T_3 = {emptyVec_4,emptyVec_5}; // @[Cat.scala 31:58]
  wire  matrix_1_2_0 = ~(|_matrix_2_0_T_3); // @[BitUtils.scala 257:50]
  wire [2:0] _matrix_3_0_T_3 = {emptyVec_4,emptyVec_5,emptyVec_6}; // @[Cat.scala 31:58]
  wire  matrix_1_3_0 = ~(|_matrix_3_0_T_3); // @[BitUtils.scala 257:50]
  wire [3:0] _matrix_4_0_T_3 = {emptyVec_4,emptyVec_5,emptyVec_6,emptyVec_7}; // @[Cat.scala 31:58]
  wire  matrix_1_4_0 = ~(|_matrix_4_0_T_3); // @[BitUtils.scala 257:50]
  wire [4:0] _matrix_5_0_T_3 = {emptyVec_4,emptyVec_5,emptyVec_6,emptyVec_7,emptyVec_0}; // @[Cat.scala 31:58]
  wire  matrix_1_5_0 = ~(|_matrix_5_0_T_3); // @[BitUtils.scala 257:50]
  wire [5:0] _matrix_6_0_T_3 = {emptyVec_4,emptyVec_5,emptyVec_6,emptyVec_7,emptyVec_0,emptyVec_1}; // @[Cat.scala 31:58]
  wire  matrix_1_6_0 = ~(|_matrix_6_0_T_3); // @[BitUtils.scala 257:50]
  wire [6:0] _matrix_7_0_T_3 = {emptyVec_4,emptyVec_5,emptyVec_6,emptyVec_7,emptyVec_0,emptyVec_1,emptyVec_2}; // @[Cat.scala 31:58]
  wire  matrix_1_7_0 = ~(|_matrix_7_0_T_3); // @[BitUtils.scala 257:50]
  wire  sel__1 = emptyVec_2 & matrix__1_0; // @[BitUtils.scala 274:62]
  wire  sel__2 = emptyVec_1 & matrix__2_0; // @[BitUtils.scala 274:62]
  wire  sel__3 = emptyVec_0 & matrix__3_0; // @[BitUtils.scala 274:62]
  wire  sel__4 = emptyVec_7 & matrix__4_0; // @[BitUtils.scala 274:62]
  wire  sel__5 = emptyVec_6 & matrix__5_0; // @[BitUtils.scala 274:62]
  wire  sel__6 = emptyVec_5 & matrix__6_0; // @[BitUtils.scala 274:62]
  wire  sel__7 = emptyVec_4 & matrix__7_0; // @[BitUtils.scala 274:62]
  wire [3:0] io_allocate_0_bits_lo = {emptyVec_3,sel__1,sel__2,sel__3}; // @[SelectPolicy.scala 49:35]
  wire [3:0] io_allocate_0_bits_hi = {sel__4,sel__5,sel__6,sel__7}; // @[SelectPolicy.scala 49:35]
  wire  sel_1_1 = emptyVec_5 & matrix_1_1_0; // @[BitUtils.scala 274:62]
  wire  sel_1_2 = emptyVec_6 & matrix_1_2_0; // @[BitUtils.scala 274:62]
  wire  sel_1_3 = emptyVec_7 & matrix_1_3_0; // @[BitUtils.scala 274:62]
  wire  sel_1_4 = emptyVec_0 & matrix_1_4_0; // @[BitUtils.scala 274:62]
  wire  sel_1_5 = emptyVec_1 & matrix_1_5_0; // @[BitUtils.scala 274:62]
  wire  sel_1_6 = emptyVec_2 & matrix_1_6_0; // @[BitUtils.scala 274:62]
  wire  sel_1_7 = emptyVec_3 & matrix_1_7_0; // @[BitUtils.scala 274:62]
  wire [3:0] io_allocate_1_bits_lo = {sel_1_7,sel_1_6,sel_1_5,sel_1_4}; // @[SelectPolicy.scala 49:35]
  wire [3:0] io_allocate_1_bits_hi = {sel_1_3,sel_1_2,sel_1_1,emptyVec_4}; // @[SelectPolicy.scala 49:35]
  wire  request_0 = io_request[0]; // @[SelectPolicy.scala 57:28]
  wire  request_1 = io_request[1]; // @[SelectPolicy.scala 57:28]
  wire  request_2 = io_request[2]; // @[SelectPolicy.scala 57:28]
  wire  request_3 = io_request[3]; // @[SelectPolicy.scala 57:28]
  wire  request_4 = io_request[4]; // @[SelectPolicy.scala 57:28]
  wire  request_5 = io_request[5]; // @[SelectPolicy.scala 57:28]
  wire  request_6 = io_request[6]; // @[SelectPolicy.scala 57:28]
  wire  request_7 = io_request[7]; // @[SelectPolicy.scala 57:28]
  wire  matrix_2_1_0 = ~(|request_0); // @[BitUtils.scala 257:50]
  wire [1:0] _matrix_2_0_T_6 = {request_0,request_1}; // @[Cat.scala 31:58]
  wire  matrix_2_2_0 = ~(|_matrix_2_0_T_6); // @[BitUtils.scala 257:50]
  wire [2:0] _matrix_3_0_T_6 = {request_0,request_1,request_2}; // @[Cat.scala 31:58]
  wire  matrix_2_3_0 = ~(|_matrix_3_0_T_6); // @[BitUtils.scala 257:50]
  wire [3:0] _matrix_4_0_T_6 = {request_0,request_1,request_2,request_3}; // @[Cat.scala 31:58]
  wire  matrix_2_4_0 = ~(|_matrix_4_0_T_6); // @[BitUtils.scala 257:50]
  wire [4:0] _matrix_5_0_T_6 = {request_0,request_1,request_2,request_3,request_4}; // @[Cat.scala 31:58]
  wire  matrix_2_5_0 = ~(|_matrix_5_0_T_6); // @[BitUtils.scala 257:50]
  wire [5:0] _matrix_6_0_T_6 = {request_0,request_1,request_2,request_3,request_4,request_5}; // @[Cat.scala 31:58]
  wire  matrix_2_6_0 = ~(|_matrix_6_0_T_6); // @[BitUtils.scala 257:50]
  wire [6:0] _matrix_7_0_T_6 = {request_0,request_1,request_2,request_3,request_4,request_5,request_6}; // @[Cat.scala 31:58]
  wire  matrix_2_7_0 = ~(|_matrix_7_0_T_6); // @[BitUtils.scala 257:50]
  wire [7:0] _selValid_T_29 = {request_7,request_6,request_5,request_4,request_3,request_2,request_1,request_0}; // @[BitUtils.scala 235:22]
  wire  sel_2_1 = request_1 & matrix_2_1_0; // @[BitUtils.scala 274:62]
  wire  sel_2_2 = request_2 & matrix_2_2_0; // @[BitUtils.scala 274:62]
  wire  sel_2_3 = request_3 & matrix_2_3_0; // @[BitUtils.scala 274:62]
  wire  sel_2_4 = request_4 & matrix_2_4_0; // @[BitUtils.scala 274:62]
  wire  sel_2_5 = request_5 & matrix_2_5_0; // @[BitUtils.scala 274:62]
  wire  sel_2_6 = request_6 & matrix_2_6_0; // @[BitUtils.scala 274:62]
  wire  sel_2_7 = request_7 & matrix_2_7_0; // @[BitUtils.scala 274:62]
  wire [3:0] io_grant_0_bits_lo = {sel_2_3,sel_2_2,sel_2_1,request_0}; // @[SelectPolicy.scala 62:32]
  wire [3:0] io_grant_0_bits_hi = {sel_2_7,sel_2_6,sel_2_5,sel_2_4}; // @[SelectPolicy.scala 62:32]
  assign io_allocate_0_bits = {io_allocate_0_bits_hi,io_allocate_0_bits_lo}; // @[SelectPolicy.scala 49:35]
  assign io_allocate_1_bits = {io_allocate_1_bits_hi,io_allocate_1_bits_lo}; // @[SelectPolicy.scala 49:35]
  assign io_grant_0_valid = |_selValid_T_29; // @[BitUtils.scala 235:29]
  assign io_grant_0_bits = {io_grant_0_bits_hi,io_grant_0_bits_lo}; // @[SelectPolicy.scala 62:32]
endmodule
