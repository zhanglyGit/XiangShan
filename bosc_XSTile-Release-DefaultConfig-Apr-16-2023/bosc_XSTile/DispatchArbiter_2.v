module DispatchArbiter_2(
  output        io_in_ready,
  input         io_in_valid,
  input         io_in_bits_cf_pd_isRVC,
  input  [1:0]  io_in_bits_cf_pd_brType,
  input         io_in_bits_cf_pd_isCall,
  input         io_in_bits_cf_pd_isRet,
  input         io_in_bits_cf_pred_taken,
  input         io_in_bits_cf_ftqPtr_flag,
  input  [2:0]  io_in_bits_cf_ftqPtr_value,
  input  [2:0]  io_in_bits_cf_ftqOffset,
  input  [1:0]  io_in_bits_ctrl_srcType_0,
  input  [1:0]  io_in_bits_ctrl_srcType_1,
  input  [1:0]  io_in_bits_ctrl_srcType_2,
  input  [3:0]  io_in_bits_ctrl_fuType,
  input  [6:0]  io_in_bits_ctrl_fuOpType,
  input         io_in_bits_ctrl_rfWen,
  input         io_in_bits_ctrl_fpWen,
  input  [19:0] io_in_bits_ctrl_imm,
  input         io_in_bits_ctrl_fpu_isAddSub,
  input         io_in_bits_ctrl_fpu_typeTagIn,
  input         io_in_bits_ctrl_fpu_typeTagOut,
  input         io_in_bits_ctrl_fpu_fromInt,
  input         io_in_bits_ctrl_fpu_wflags,
  input         io_in_bits_ctrl_fpu_fpWen,
  input  [1:0]  io_in_bits_ctrl_fpu_fmaCmd,
  input         io_in_bits_ctrl_fpu_div,
  input         io_in_bits_ctrl_fpu_sqrt,
  input         io_in_bits_ctrl_fpu_fcvt,
  input  [1:0]  io_in_bits_ctrl_fpu_typ,
  input  [1:0]  io_in_bits_ctrl_fpu_fmt,
  input         io_in_bits_ctrl_fpu_ren3,
  input  [2:0]  io_in_bits_ctrl_fpu_rm,
  input         io_in_bits_srcState_0,
  input         io_in_bits_srcState_1,
  input         io_in_bits_srcState_2,
  input  [5:0]  io_in_bits_psrc_0,
  input  [5:0]  io_in_bits_psrc_1,
  input  [5:0]  io_in_bits_psrc_2,
  input  [5:0]  io_in_bits_pdest,
  input         io_in_bits_robIdx_flag,
  input  [4:0]  io_in_bits_robIdx_value,
  input         io_out_0_ready,
  output        io_out_0_valid,
  output        io_out_0_bits_cf_pd_isRVC,
  output [1:0]  io_out_0_bits_cf_pd_brType,
  output        io_out_0_bits_cf_pd_isCall,
  output        io_out_0_bits_cf_pd_isRet,
  output        io_out_0_bits_cf_pred_taken,
  output        io_out_0_bits_cf_ftqPtr_flag,
  output [2:0]  io_out_0_bits_cf_ftqPtr_value,
  output [2:0]  io_out_0_bits_cf_ftqOffset,
  output [1:0]  io_out_0_bits_ctrl_srcType_0,
  output [1:0]  io_out_0_bits_ctrl_srcType_1,
  output [1:0]  io_out_0_bits_ctrl_srcType_2,
  output [3:0]  io_out_0_bits_ctrl_fuType,
  output [6:0]  io_out_0_bits_ctrl_fuOpType,
  output        io_out_0_bits_ctrl_rfWen,
  output        io_out_0_bits_ctrl_fpWen,
  output [19:0] io_out_0_bits_ctrl_imm,
  output        io_out_0_bits_ctrl_fpu_isAddSub,
  output        io_out_0_bits_ctrl_fpu_typeTagIn,
  output        io_out_0_bits_ctrl_fpu_typeTagOut,
  output        io_out_0_bits_ctrl_fpu_fromInt,
  output        io_out_0_bits_ctrl_fpu_wflags,
  output        io_out_0_bits_ctrl_fpu_fpWen,
  output [1:0]  io_out_0_bits_ctrl_fpu_fmaCmd,
  output        io_out_0_bits_ctrl_fpu_div,
  output        io_out_0_bits_ctrl_fpu_sqrt,
  output        io_out_0_bits_ctrl_fpu_fcvt,
  output [1:0]  io_out_0_bits_ctrl_fpu_typ,
  output [1:0]  io_out_0_bits_ctrl_fpu_fmt,
  output        io_out_0_bits_ctrl_fpu_ren3,
  output [2:0]  io_out_0_bits_ctrl_fpu_rm,
  output        io_out_0_bits_srcState_0,
  output        io_out_0_bits_srcState_1,
  output        io_out_0_bits_srcState_2,
  output [5:0]  io_out_0_bits_psrc_0,
  output [5:0]  io_out_0_bits_psrc_1,
  output [5:0]  io_out_0_bits_psrc_2,
  output [5:0]  io_out_0_bits_pdest,
  output        io_out_0_bits_robIdx_flag,
  output [4:0]  io_out_0_bits_robIdx_value,
  input         io_out_1_ready,
  output        io_out_1_valid,
  output        io_out_1_bits_cf_pd_isRVC,
  output [1:0]  io_out_1_bits_cf_pd_brType,
  output        io_out_1_bits_cf_pd_isCall,
  output        io_out_1_bits_cf_pd_isRet,
  output        io_out_1_bits_cf_pred_taken,
  output        io_out_1_bits_cf_ftqPtr_flag,
  output [2:0]  io_out_1_bits_cf_ftqPtr_value,
  output [2:0]  io_out_1_bits_cf_ftqOffset,
  output [1:0]  io_out_1_bits_ctrl_srcType_0,
  output [1:0]  io_out_1_bits_ctrl_srcType_1,
  output [3:0]  io_out_1_bits_ctrl_fuType,
  output [6:0]  io_out_1_bits_ctrl_fuOpType,
  output        io_out_1_bits_ctrl_rfWen,
  output        io_out_1_bits_ctrl_fpWen,
  output [19:0] io_out_1_bits_ctrl_imm,
  output        io_out_1_bits_ctrl_fpu_isAddSub,
  output        io_out_1_bits_ctrl_fpu_typeTagIn,
  output        io_out_1_bits_ctrl_fpu_typeTagOut,
  output        io_out_1_bits_ctrl_fpu_fromInt,
  output        io_out_1_bits_ctrl_fpu_wflags,
  output        io_out_1_bits_ctrl_fpu_fpWen,
  output [1:0]  io_out_1_bits_ctrl_fpu_fmaCmd,
  output        io_out_1_bits_ctrl_fpu_div,
  output        io_out_1_bits_ctrl_fpu_sqrt,
  output        io_out_1_bits_ctrl_fpu_fcvt,
  output [1:0]  io_out_1_bits_ctrl_fpu_typ,
  output [1:0]  io_out_1_bits_ctrl_fpu_fmt,
  output        io_out_1_bits_ctrl_fpu_ren3,
  output [2:0]  io_out_1_bits_ctrl_fpu_rm,
  output        io_out_1_bits_srcState_0,
  output        io_out_1_bits_srcState_1,
  output [5:0]  io_out_1_bits_psrc_0,
  output [5:0]  io_out_1_bits_psrc_1,
  output [5:0]  io_out_1_bits_pdest,
  output        io_out_1_bits_robIdx_flag,
  output [4:0]  io_out_1_bits_robIdx_value
);
  wire  _io_out_0_valid_T_1 = |(4'h8 == io_in_bits_ctrl_fuType); // @[Exu.scala 108:45]
  wire  _io_out_1_valid_T = 4'hb == io_in_bits_ctrl_fuType; // @[Exu.scala 108:32]
  wire  _io_out_1_valid_T_2 = 4'ha == io_in_bits_ctrl_fuType; // @[Exu.scala 108:32]
  wire [2:0] _io_out_1_valid_T_3 = {_io_out_1_valid_T,_io_out_1_valid_T,_io_out_1_valid_T_2}; // @[Cat.scala 31:58]
  wire  _io_out_1_valid_T_4 = |_io_out_1_valid_T_3; // @[Exu.scala 108:45]
  wire  _io_in_ready_T = io_out_0_ready & io_out_0_valid; // @[Decoupled.scala 50:35]
  wire  _io_in_ready_T_1 = io_out_1_ready & io_out_1_valid; // @[Decoupled.scala 50:35]
  wire [1:0] _io_in_ready_T_2 = {_io_in_ready_T_1,_io_in_ready_T}; // @[Scheduler.scala 49:48]
  assign io_in_ready = |_io_in_ready_T_2; // @[Scheduler.scala 49:55]
  assign io_out_0_valid = io_in_valid & _io_out_0_valid_T_1; // @[Scheduler.scala 45:28]
  assign io_out_0_bits_cf_pd_isRVC = io_in_bits_cf_pd_isRVC; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_pd_brType = io_in_bits_cf_pd_brType; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_pd_isCall = io_in_bits_cf_pd_isCall; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_pd_isRet = io_in_bits_cf_pd_isRet; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_pred_taken = io_in_bits_cf_pred_taken; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_ftqPtr_flag = io_in_bits_cf_ftqPtr_flag; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_ftqPtr_value = io_in_bits_cf_ftqPtr_value; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_cf_ftqOffset = io_in_bits_cf_ftqOffset; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_srcType_0 = io_in_bits_ctrl_srcType_0; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_srcType_1 = io_in_bits_ctrl_srcType_1; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_srcType_2 = io_in_bits_ctrl_srcType_2; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fuType = io_in_bits_ctrl_fuType; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fuOpType = io_in_bits_ctrl_fuOpType; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_rfWen = io_in_bits_ctrl_rfWen; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpWen = io_in_bits_ctrl_fpWen; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_imm = io_in_bits_ctrl_imm; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_isAddSub = io_in_bits_ctrl_fpu_isAddSub; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_typeTagIn = io_in_bits_ctrl_fpu_typeTagIn; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_typeTagOut = io_in_bits_ctrl_fpu_typeTagOut; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_fromInt = io_in_bits_ctrl_fpu_fromInt; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_wflags = io_in_bits_ctrl_fpu_wflags; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_fpWen = io_in_bits_ctrl_fpu_fpWen; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_fmaCmd = io_in_bits_ctrl_fpu_fmaCmd; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_div = io_in_bits_ctrl_fpu_div; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_sqrt = io_in_bits_ctrl_fpu_sqrt; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_fcvt = io_in_bits_ctrl_fpu_fcvt; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_typ = io_in_bits_ctrl_fpu_typ; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_fmt = io_in_bits_ctrl_fpu_fmt; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_ren3 = io_in_bits_ctrl_fpu_ren3; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_ctrl_fpu_rm = io_in_bits_ctrl_fpu_rm; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_srcState_0 = io_in_bits_srcState_0; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_srcState_1 = io_in_bits_srcState_1; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_srcState_2 = io_in_bits_srcState_2; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_psrc_0 = io_in_bits_psrc_0; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_psrc_1 = io_in_bits_psrc_1; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_psrc_2 = io_in_bits_psrc_2; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_pdest = io_in_bits_pdest; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_robIdx_flag = io_in_bits_robIdx_flag; // @[Scheduler.scala 46:12]
  assign io_out_0_bits_robIdx_value = io_in_bits_robIdx_value; // @[Scheduler.scala 46:12]
  assign io_out_1_valid = io_in_valid & _io_out_1_valid_T_4; // @[Scheduler.scala 45:28]
  assign io_out_1_bits_cf_pd_isRVC = io_in_bits_cf_pd_isRVC; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_pd_brType = io_in_bits_cf_pd_brType; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_pd_isCall = io_in_bits_cf_pd_isCall; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_pd_isRet = io_in_bits_cf_pd_isRet; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_pred_taken = io_in_bits_cf_pred_taken; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_ftqPtr_flag = io_in_bits_cf_ftqPtr_flag; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_ftqPtr_value = io_in_bits_cf_ftqPtr_value; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_cf_ftqOffset = io_in_bits_cf_ftqOffset; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_srcType_0 = io_in_bits_ctrl_srcType_0; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_srcType_1 = io_in_bits_ctrl_srcType_1; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fuType = io_in_bits_ctrl_fuType; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fuOpType = io_in_bits_ctrl_fuOpType; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_rfWen = io_in_bits_ctrl_rfWen; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpWen = io_in_bits_ctrl_fpWen; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_imm = io_in_bits_ctrl_imm; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_isAddSub = io_in_bits_ctrl_fpu_isAddSub; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_typeTagIn = io_in_bits_ctrl_fpu_typeTagIn; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_typeTagOut = io_in_bits_ctrl_fpu_typeTagOut; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_fromInt = io_in_bits_ctrl_fpu_fromInt; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_wflags = io_in_bits_ctrl_fpu_wflags; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_fpWen = io_in_bits_ctrl_fpu_fpWen; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_fmaCmd = io_in_bits_ctrl_fpu_fmaCmd; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_div = io_in_bits_ctrl_fpu_div; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_sqrt = io_in_bits_ctrl_fpu_sqrt; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_fcvt = io_in_bits_ctrl_fpu_fcvt; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_typ = io_in_bits_ctrl_fpu_typ; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_fmt = io_in_bits_ctrl_fpu_fmt; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_ren3 = io_in_bits_ctrl_fpu_ren3; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_ctrl_fpu_rm = io_in_bits_ctrl_fpu_rm; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_srcState_0 = io_in_bits_srcState_0; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_srcState_1 = io_in_bits_srcState_1; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_psrc_0 = io_in_bits_psrc_0; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_psrc_1 = io_in_bits_psrc_1; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_pdest = io_in_bits_pdest; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_robIdx_flag = io_in_bits_robIdx_flag; // @[Scheduler.scala 46:12]
  assign io_out_1_bits_robIdx_value = io_in_bits_robIdx_value; // @[Scheduler.scala 46:12]
endmodule
