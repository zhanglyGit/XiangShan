/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.cache.dcache.ReplayCarry
import xiangshan.mem.mdp._
import xiangshan.backend.rob.RobPtr

class LqPtr(implicit p: Parameters) extends CircularQueuePtr[LqPtr](
  p => p(XSCoreParamsKey).LoadQueueFlagSize
){
}

object LqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): LqPtr = {
    val ptr = Wire(new LqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

trait HasLoadHelper { this: XSModule =>
  def rdataHelper(uop: MicroOp, rdata: UInt): UInt = {
    val fpWen = uop.ctrl.fpWen
    LookupTree(uop.ctrl.fuOpType, List(
      LSUOpType.lb   -> SignExt(rdata(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdata(15, 0), XLEN),
      /*
          riscv-spec-20191213: 12.2 NaN Boxing of Narrower Values
          Any operation that writes a narrower result to an f register must write
          all 1s to the uppermost FLEN−n bits to yield a legal NaN-boxed value.
      */
      LSUOpType.lw   -> Mux(fpWen, FPU.box(rdata, FPU.S), SignExt(rdata(31, 0), XLEN)),
      LSUOpType.ld   -> Mux(fpWen, FPU.box(rdata, FPU.D), SignExt(rdata(63, 0), XLEN)),
      LSUOpType.lbu  -> ZeroExt(rdata(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdata(31, 0), XLEN),
    ))
  }
}

class LqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(exuParameters.LsExuCnt, Input(Bool()))
  val req = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(exuParameters.LsExuCnt, Output(new LqPtr))
}

class LqTriggerIO(implicit p: Parameters) extends XSBundle {
  val hitLoadAddrTriggerHitVec = Input(Vec(3, Bool()))
  val lqLoadAddrTriggerHitVec = Output(Vec(3, Bool()))
}


class LoadQueue(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect)) 
    val enq = new LqEnqIO
    val ldu = new Bundle() {
      val s2 = new Bundle() {
        val storeLoadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
        val loadLoadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
      }
      val s3 = new Bundle() {
        val loadIn = Vec(StorePipelineWidth, Flipped(Valid(new LqWriteBundle)))
      }
    }
    val sta = new Bundle() {
      val s1 = new Bundle() {
        val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
      }
    }
    val std = new Bundle() {
      val s0 = new Bundle() {
        val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput))) // store data, send to sq from rs
      }
    }
    val sq = new Bundle() {
      val stAddrReadySqPtr = Input(new SqPtr)      
      val stDataReadySqPtr = Input(new SqPtr)
      val sqEmpty = Input(Bool())
    }
    val loadOut = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val ldRawDataOut = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val refill = Flipped(ValidIO(new Refill)) 
    val release = Flipped(Valid(new Release))
    val rollback = Output(Valid(new Redirect)) 
    val correctTableUpdate = Valid(new CorrectTableUpdate) 
    val rob = Flipped(new RobLsqIO)
    val uncache = new UncacheWordIO
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)
    val exceptionAddr = new ExceptionAddrIO
    val lqFlagFull = Output(Bool())
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueFlagSize+1).W))
    val lqReplayFull = Output(Bool())
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W))) 
  })

  val loadQueueRAR = Module(new LoadQueueRAR)  //  ld-ld violation
  val loadQueueRAW = Module(new LoadQueueRAW)  //  st-ld violation
  val loadQueueReplay = Module(new LoadQueueReplay)  //  enqueue if need replay
  val loadQueueFlag = Module(new LoadQueueFlag)  //  control state 

  /**
   * LoadQueueRAR
   */  
  loadQueueRAR.io.redirect <> io.redirect
  loadQueueRAR.io.release <> io.release
  loadQueueRAR.io.ldIssuePtr <> loadQueueFlag.io.ldIssuePtr
  loadQueueRAR.io.query <> io.ldu.s2.loadLoadViolationQuery //  enqueue & query

  /**
   * LoadQueueRAW
   */  
  loadQueueRAW.io.redirect <> io.redirect 
  loadQueueRAW.io.rollback <> io.rollback
  loadQueueRAW.io.storeIn <> io.sta.s1.storeAddrIn
  loadQueueRAW.io.correctTableUpdate <> io.correctTableUpdate
  loadQueueRAW.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueRAW.io.ldIssuePtr := loadQueueFlag.io.ldIssuePtr
  loadQueueRAW.io.lqEmpty := loadQueueFlag.io.lqEmpty
  loadQueueRAW.io.sqEmpty <> io.sq.sqEmpty
  loadQueueRAW.io.query <> io.ldu.s2.storeLoadViolationQuery  // enqueue

  /**
   * LoadQueueFlag
   */  
  loadQueueFlag.io.redirect <> io.redirect
  loadQueueFlag.io.enq <> io.enq 
  loadQueueFlag.io.loadIn <> io.ldu.s3.loadIn
  loadQueueFlag.io.loadOut <> io.loadOut
  loadQueueFlag.io.ldRawDataOut <> io.ldRawDataOut
  loadQueueFlag.io.rob <> io.rob 
  loadQueueFlag.io.uncache <> io.uncache
  loadQueueFlag.io.trigger <> io.trigger
  loadQueueFlag.io.exceptionAddr <> io.exceptionAddr
  loadQueueFlag.io.lqFull <> io.lqFlagFull
  loadQueueFlag.io.lqDeq <> io.lqDeq
  loadQueueFlag.io.lqCancelCnt <> io.lqCancelCnt

  /**
   * LoadQueueReplay
   */  
  loadQueueReplay.io.redirect <> io.redirect
  loadQueueReplay.io.enq <> io.ldu.s3.loadIn
  loadQueueReplay.io.storeAddrIn <> io.sta.s1.storeAddrIn
  loadQueueReplay.io.storeDataIn <> io.std.s0.storeDataIn
  loadQueueReplay.io.replay <> io.replay
  loadQueueReplay.io.refill <> io.refill 
  loadQueueReplay.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueReplay.io.stDataReadySqPtr <> io.sq.stDataReadySqPtr
  loadQueueReplay.io.sqEmpty <> io.sq.sqEmpty
  loadQueueReplay.io.lqFull <> io.lqReplayFull
  loadQueueReplay.io.tlbReplayDelayCycleCtrl <> io.tlbReplayDelayCycleCtrl


  def toVec(a: UInt): Vec[Bool] = {
    VecInit(a.asBools)
  }

  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until LoadQueueSize / LoadPipelineWidth).map(i => { input(LoadPipelineWidth * i + rem) })).asUInt
  }

  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
  }

  def getOldest[T <: XSBundleWithMicroOp](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    assert(isPow2(valid.length))
    if (valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1), Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx), res(1), res(0)), Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = getOldest(valid.take(valid.length / 2), bits.take(valid.length / 2))
      val right = getOldest(valid.takeRight(valid.length / 2), bits.takeRight(valid.length / 2))
      getOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  def getAfterMask(valid: Seq[Bool], uop: Seq[MicroOp]) = {
    assert(valid.length == uop.length)
    val length = valid.length
    (0 until length).map(i => {
      (0 until length).map(j => {
        Mux(valid(i) && valid(j),
          isAfter(uop(i).robIdx, uop(j).robIdx),
          Mux(!valid(i), true.B, false.B))
      })
    })
  }


  /**
    * Store-Load Memory violation detection
    *
    * When store writes back, it searches LoadQueue for younger load instructions
    * with the same load physical address. They loaded wrong data and need re-execution.
    *
    * Cycle 0: Store Writeback
    *   Generate match vector for store address with rangeMask(stPtr, enqPtr).
    * Cycle 1: Redirect Generation
    *   There're up to 2 possible redirect requests.
    *   Choose the oldest load (part 1). 
    * Cycle 2: Redirect Fire
    *   Choose the oldest load (part 2).
    *   Prepare redirect request according to the detected violation.
    *   Fire redirect request (if valid)
    */

  // stage 0:        lq                 lq
  //                 |                  |  (paddr match)
  // stage 1:        lq                 lq
  //                 |                  |
  //                 |                  |
  //                 |                  |
  // stage 2:        lq                 lq
  //                 |                  |
  //                 --------------------
  //                          |
  //                      rollback req
  io.load_s1 := DontCare
def detectRollback(i: Int) = {
    val startIndex = io.storeIn(i).bits.uop.lqIdx.value
    val lqIdxMask = UIntToMask(startIndex, LoadQueueSize)
    val xorMask = lqIdxMask ^ enqMask
    val sameFlag = io.storeIn(i).bits.uop.lqIdx.flag === enqPtrExt(0).flag
    val stToEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)

    // check if load already in lq needs to be rolledback
    dataModule.io.violation(i).paddr := io.storeIn(i).bits.paddr
    dataModule.io.violation(i).mask := io.storeIn(i).bits.mask
    val addrMaskMatch = RegNext(dataModule.io.violation(i).violationMask)
    val entryNeedCheck = RegNext(VecInit((0 until LoadQueueSize).map(j => {
      allocated(j) && stToEnqPtrMask(j) && datavalid(j)
    })))
    val lqViolationVec = VecInit((0 until LoadQueueSize).map(j => {
      addrMaskMatch(j) && entryNeedCheck(j)
    }))
    val lqViolation = lqViolationVec.asUInt().orR() && RegNext(!io.storeIn(i).bits.miss)
    val lqViolationIndex = getFirstOne(lqViolationVec, RegNext(lqIdxMask))
    val lqViolationUop = uop(lqViolationIndex)
    // lqViolationUop.lqIdx.flag := deqMask(lqViolationIndex) ^ deqPtrExt.flag
    // lqViolationUop.lqIdx.value := lqViolationIndex
    XSDebug(lqViolation, p"${Binary(Cat(lqViolationVec))}, $startIndex, $lqViolationIndex\n")

    XSDebug(
      lqViolation,
      "need rollback (ld wb before store) pc %x robidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.robIdx.asUInt, lqViolationUop.robIdx.asUInt
    )

    (lqViolation, lqViolationUop)
  }

  def rollbackSel(a: Valid[MicroOpRbExt], b: Valid[MicroOpRbExt]): ValidIO[MicroOpRbExt] = {
    Mux(
      a.valid,
      Mux(
        b.valid,
        Mux(isAfter(a.bits.uop.robIdx, b.bits.uop.robIdx), b, a), // a,b both valid, sel oldest
        a // sel a
      ),
      b // sel b
    )
  }

  // S2: select rollback (part1) and generate rollback request
  // rollback check
  // Lq rollback seq check is done in s3 (next stage), as getting rollbackLq MicroOp is slow
  val rollbackLq = Wire(Vec(StorePipelineWidth, Valid(new MicroOpRbExt)))
  // store ftq index for store set update
  val stFtqIdxS2 = Wire(Vec(StorePipelineWidth, new FtqPtr))
  val stFtqOffsetS2 = Wire(Vec(StorePipelineWidth, UInt(log2Up(PredictWidth).W)))
  for (i <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(i)
    rollbackLq(i).valid := detectedRollback._1 && RegNext(io.storeIn(i).valid)
    rollbackLq(i).bits.uop := detectedRollback._2
    rollbackLq(i).bits.flag := i.U
    stFtqIdxS2(i) := RegNext(io.storeIn(i).bits.uop.cf.ftqPtr)
    stFtqOffsetS2(i) := RegNext(io.storeIn(i).bits.uop.cf.ftqOffset)
  }

  val rollbackLqVReg = rollbackLq.map(x => RegNext(x.valid))
  val rollbackLqReg = rollbackLq.map(x => RegEnable(x.bits, x.valid))

  // S3: select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use robIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's robIdx equals to this cycle's robIdx, it still triggers the redirect.

  // select uop in parallel
  val lqs = getOldest(rollbackLqVReg, rollbackLqReg)
  val rollbackUopExt = lqs._2(0) 
  val stFtqIdxS3 = RegNext(stFtqIdxS2)
  val stFtqOffsetS3 = RegNext(stFtqOffsetS2)
  val rollbackUop = rollbackUopExt.uop
  val rollbackStFtqIdx = stFtqIdxS3(rollbackUopExt.flag)
  val rollbackStFtqOffset = stFtqOffsetS3(rollbackUopExt.flag)

  // check if rollback request is still valid in parallel
  io.rollback.bits.robIdx := rollbackUop.robIdx
  io.rollback.bits.ftqIdx := rollbackUop.cf.ftqPtr
  io.rollback.bits.stFtqIdx := rollbackStFtqIdx
  io.rollback.bits.ftqOffset := rollbackUop.cf.ftqOffset
  io.rollback.bits.stFtqOffset := rollbackStFtqOffset
  io.rollback.bits.level := RedirectLevel.flush
  io.rollback.bits.interrupt := DontCare
  io.rollback.bits.cfiUpdate := DontCare
  io.rollback.bits.cfiUpdate.target := rollbackUop.cf.pc
  io.rollback.bits.debug_runahead_checkpoint_id := rollbackUop.debugInfo.runahead_checkpoint_id
  // io.rollback.bits.pc := DontCare

  io.rollback.valid := rollbackLqVReg.reduce(_|_) &&
                        (!lastCycleRedirect.valid || isBefore(rollbackUop.robIdx, lastCycleRedirect.bits.robIdx)) && 
                        (!lastlastCycleRedirect.valid || isBefore(rollbackUop.robIdx, lastlastCycleRedirect.bits.robIdx))

  when(io.rollback.valid) {
    // XSDebug("Mem rollback: pc %x robidx %d\n", io.rollback.bits.cfi, io.rollback.bits.robIdx.asUInt)
  }

  /**
  * Load-Load Memory violation detection
  *
  * When load arrives load_s1, it searches LoadQueue for younger load instructions
  * with the same load physical address. If younger load has been released (or observed),
  * the younger load needs to be re-execed.
  * 
  * For now, if re-exec it found to be needed in load_s1, we mark the older load as replayInst,
  * the two loads will be replayed if the older load becomes the head of rob.
  *
  * When dcache releases a line, mark all writebacked entrys in load queue with
  * the same line paddr as released.
  */

  // Load-Load Memory violation query
  val deqRightMask = UIntToMask.rightmask(deqPtr, LoadQueueSize)
  (0 until LoadPipelineWidth).map(i => {
    dataModule.io.release_violation(i).paddr := io.loadViolationQuery(i).req.bits.paddr
    io.loadViolationQuery(i).req.ready := true.B
    io.loadViolationQuery(i).resp.valid := RegNext(io.loadViolationQuery(i).req.fire())
    // Generate real violation mask
    // Note that we use UIntToMask.rightmask here
    val startIndex = io.loadViolationQuery(i).req.bits.uop.lqIdx.value
    val lqIdxMask = UIntToMask(startIndex, LoadQueueSize)
    val xorMask = lqIdxMask ^ enqMask
    val sameFlag = io.loadViolationQuery(i).req.bits.uop.lqIdx.flag === enqPtrExt(0).flag
    val ldToEnqPtrMask = Mux(sameFlag, xorMask, ~xorMask)
    val ldld_violation_mask_gen_1 = WireInit(VecInit((0 until LoadQueueSize).map(j => {
      ldToEnqPtrMask(j) && // the load is younger than current load
      allocated(j) && // entry is valid
      released(j) && // cacheline is released
      (datavalid(j) || miss(j)) // paddr is valid
    })))
    val ldld_violation_mask_gen_2 = WireInit(VecInit((0 until LoadQueueSize).map(j => {
      dataModule.io.release_violation(i).match_mask(j)// addr match
      // addr match result is slow to generate, we RegNext() it
    })))
    val ldld_violation_mask = RegNext(ldld_violation_mask_gen_1).asUInt & RegNext(ldld_violation_mask_gen_2).asUInt
    dontTouch(ldld_violation_mask)
    ldld_violation_mask.suggestName("ldldViolationMask_" + i)
    io.loadViolationQuery(i).resp.bits.have_violation := ldld_violation_mask.orR
  })

  // "released" flag update
  // 
  // When io.release.valid (release1cycle.valid), it uses the last ld-ld paddr cam port to
  // update release flag in 1 cycle

  when(release1cycle.valid){
    // Take over ld-ld paddr cam port
    dataModule.io.release_violation.takeRight(1)(0).paddr := release1cycle.bits.paddr
    io.loadViolationQuery.takeRight(1)(0).req.ready := false.B
  }

  when(release2cycle.valid){
    // If a load comes in that cycle, we can not judge if it has ld-ld violation
    // We replay that load inst from RS
    io.loadViolationQuery.map(i => i.req.ready :=
      // use lsu side release2cycle_dup_lsu paddr for better timing
      !i.req.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2cycle_dup_lsu.bits.paddr(PAddrBits-1, DCacheLineOffset)
    )
    // io.loadViolationQuery.map(i => i.req.ready := false.B) // For better timing
  }

  (0 until LoadQueueSize).map(i => {
    when(RegNext(dataModule.io.release_violation.takeRight(1)(0).match_mask(i) && 
      allocated(i) &&
      datavalid(i) &&
      release1cycle.valid
    )){
      // Note: if a load has missed in dcache and is waiting for refill in load queue,
      // its released flag still needs to be set as true if addr matches. 
      released(i) := true.B
    }
  })

  /**
    * Memory mapped IO / other uncached operations
    *
    * States:
    * (1) writeback from store units: mark as pending
    * (2) when they reach ROB's head, they can be sent to uncache channel
    * (3) response from uncache channel: mark as datavalid
    * (4) writeback to ROB (and other units): mark as writebacked
    * (5) ROB commits the instruction: same as normal instructions
    */
  //(2) when they reach ROB's head, they can be sent to uncache channel
  val lqTailMmioPending = WireInit(pending(deqPtr))
  val lqTailAllocated = WireInit(allocated(deqPtr))
  val s_idle :: s_req :: s_resp :: s_wait :: Nil = Enum(4)
  val uncacheState = RegInit(s_idle)
  switch(uncacheState) {
    is(s_idle) {
      when(RegNext(io.rob.pendingld && lqTailMmioPending && lqTailAllocated)) {
        uncacheState := s_req
      }
    }
    is(s_req) {
      when(io.uncache.req.fire()) {
        uncacheState := s_resp
      }
    }
    is(s_resp) {
      when(io.uncache.resp.fire()) {
        uncacheState := s_wait
      }
    }
    is(s_wait) {
      when(RegNext(io.rob.commit)) {
        uncacheState := s_idle // ready for next mmio
      }
    }
  }

  // used for uncache commit
  val uncacheData = RegInit(0.U(XLEN.W))
  val uncacheCommitFired = RegInit(false.B)

  when(uncacheState === s_req) {
    uncacheCommitFired := false.B
  }

  io.uncache.req.valid := uncacheState === s_req

  dataModule.io.uncache.raddr := deqPtrExtNext.value

  io.uncache.req.bits.cmd  := MemoryOpConstants.M_XRD
  io.uncache.req.bits.addr := dataModule.io.uncache.rdata.paddr
  io.uncache.req.bits.data := DontCare
  io.uncache.req.bits.mask := dataModule.io.uncache.rdata.mask
  io.uncache.req.bits.id   := RegNext(deqPtrExtNext.value)
  io.uncache.req.bits.instrtype := DontCare
  io.uncache.req.bits.replayCarry := DontCare
  io.uncache.req.bits.atomic := true.B

  io.uncache.resp.ready := true.B

  when (io.uncache.req.fire()) {
    pending(deqPtr) := false.B

    XSDebug("uncache req: pc %x addr %x data %x op %x mask %x\n",
      uop(deqPtr).cf.pc,
      io.uncache.req.bits.addr,
      io.uncache.req.bits.data,
      io.uncache.req.bits.cmd,
      io.uncache.req.bits.mask
    )
  }

  // (3) response from uncache channel: mark as datavalid
  when(io.uncache.resp.fire()){
    datavalid(deqPtr) := true.B
    uncacheData := io.uncache.resp.bits.data(XLEN-1, 0)

    XSDebug("uncache resp: data %x\n", io.refill.bits.data)
  }

  // writeback mmio load, Note: only use ldout(0) to write back
  //
  // Int load writeback will finish (if not blocked) in one cycle
  io.ldout(0).bits.uop := uop(deqPtr)
  io.ldout(0).bits.uop.lqIdx := deqPtr.asTypeOf(new LqPtr)
  io.ldout(0).bits.data := DontCare // not used
  io.ldout(0).bits.redirectValid := false.B
  io.ldout(0).bits.redirect := DontCare
  io.ldout(0).bits.debug.isMMIO := true.B
  io.ldout(0).bits.debug.isPerfCnt := false.B
  io.ldout(0).bits.debug.paddr := debug_paddr(deqPtr)
  io.ldout(0).bits.debug.vaddr := vaddrModule.io.rdata(1)
  io.ldout(0).bits.fflags := DontCare
  io.ldout(0).bits.vxsat := DontCare

  io.ldout(0).valid := (uncacheState === s_wait) && !uncacheCommitFired

  io.ldout(1).bits := DontCare
  io.ldout(1).valid := false.B

  // merged data, uop and offset for data sel in load_s3
  io.ldRawDataOut(0).lqData := uncacheData
  io.ldRawDataOut(0).uop := io.ldout(0).bits.uop
  io.ldRawDataOut(0).addrOffset := dataModule.io.uncache.rdata.paddr

  io.ldRawDataOut(1) := DontCare

  when(io.ldout(0).fire()){
    uncacheCommitFired := true.B
  }

  XSPerfAccumulate("uncache_load_write_back", io.ldout(0).fire())

  // Read vaddr for mem exception
  // no inst will be commited 1 cycle before tval update
  vaddrModule.io.raddr(0) := (deqPtrExt + commitCount).value
  io.exceptionAddr.vaddr := vaddrModule.io.rdata(0)

  // read vaddr for mmio, and only port {1} is used
  vaddrModule.io.raddr(1) := deqPtr

  (0 until LoadPipelineWidth).map(i => {
    if(i == 0) {
      vaddrTriggerResultModule.io.raddr(i) := deqPtr
      io.trigger(i).lqLoadAddrTriggerHitVec := Mux(
        io.ldout(i).valid,
        vaddrTriggerResultModule.io.rdata(i),
        VecInit(Seq.fill(3)(false.B))
      )
    }else {
      vaddrTriggerResultModule.io.raddr(i) := DontCare
      io.trigger(i).lqLoadAddrTriggerHitVec := VecInit(Seq.fill(3)(false.B))
    }
    // vaddrTriggerResultModule.io.raddr(i) := loadWbSelGen(i)
    // io.trigger(i).lqLoadAddrTriggerHitVec := Mux(
    //   loadWbSelV(i),
    //   vaddrTriggerResultModule.io.rdata(i),
    //   VecInit(Seq.fill(3)(false.B))
    // )
  })

  // misprediction recovery / exception redirect
  // invalidate lq term using robIdx
  val needCancel = Wire(Vec(LoadQueueSize, Bool()))
  for (i <- 0 until LoadQueueSize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.brqRedirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }

  /**
    * update pointers
    */
  val lastEnqCancel = PopCount(RegNext(VecInit(canEnqueue.zip(enqCancel).map(x => x._1 && x._2))))
  val lastCycleCancelCount = PopCount(RegNext(needCancel))
  val enqNumber = Mux(io.enq.canAccept && io.enq.sqCanAccept, PopCount(io.enq.req.map(_.valid)), 0.U)
  when (lastCycleRedirect.valid) {
    // we recover the pointers in the next cycle after redirect
    enqPtrExt := VecInit(enqPtrExt.map(_ - (lastCycleCancelCount + lastEnqCancel)))
  }.otherwise {
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }

  deqPtrExtNext := deqPtrExt + commitCount
  deqPtrExt := deqPtrExtNext

  io.lqCancelCnt := RegNext(lastCycleCancelCount + lastEnqCancel)

  /**
    * misc
    */
  // perf counter
  QueuePerf(LoadQueueSize, validCount, !allowEnqueue)
  io.lqFull := !allowEnqueue
  XSPerfAccumulate("rollback", io.rollback.valid) // rollback redirect generated
  XSPerfAccumulate("mmioCycle", uncacheState =/= s_idle) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.uncache.req.fire())
  XSPerfAccumulate("refill", io.refill.valid)
  XSPerfAccumulate("writeback_success", PopCount(VecInit(io.ldout.map(i => i.fire()))))
  XSPerfAccumulate("writeback_blocked", PopCount(VecInit(io.ldout.map(i => i.valid && !i.ready))))
  XSPerfAccumulate("utilization_miss", PopCount((0 until LoadQueueSize).map(i => allocated(i) && miss(i))))

  if (env.EnableTopDown) {
    val stall_loads_bound = WireDefault(0.B)
    ExcitingUtils.addSink(stall_loads_bound, "stall_loads_bound", ExcitingUtils.Perf)
    val have_miss_entry = (allocated zip miss).map(x => x._1 && x._2).reduce(_ || _)
    val l1d_loads_bound = stall_loads_bound && !have_miss_entry
    ExcitingUtils.addSource(l1d_loads_bound, "l1d_loads_bound", ExcitingUtils.Perf)
    XSPerfAccumulate("l1d_loads_bound", l1d_loads_bound)
    val stall_l1d_load_miss = stall_loads_bound && have_miss_entry
    ExcitingUtils.addSource(stall_l1d_load_miss, "stall_l1d_load_miss", ExcitingUtils.Perf)
    ExcitingUtils.addSink(WireInit(0.U), "stall_l1d_load_miss", ExcitingUtils.Perf)
  }

  val perfValidCount = RegNext(validCount)

  val perfEvents = Seq(
    ("rollback         ", io.rollback.valid),
    ("mmioCycle        ", uncacheState =/= s_idle),
    ("mmio_Cnt         ", io.uncache.req.fire()),
    ("refill           ", io.refill.valid),
    ("writeback_success", PopCount(VecInit(io.ldout.map(i => i.fire())))),
    ("writeback_blocked", PopCount(VecInit(io.ldout.map(i => i.valid && !i.ready)))),
    ("ltq_1_4_valid    ", (perfValidCount < (LoadQueueSize.U/4.U))),
    ("ltq_2_4_valid    ", (perfValidCount > (LoadQueueSize.U/4.U)) & (perfValidCount <= (LoadQueueSize.U/2.U))),
    ("ltq_3_4_valid    ", (perfValidCount > (LoadQueueSize.U/2.U)) & (perfValidCount <= (LoadQueueSize.U*3.U/4.U))),
    ("ltq_4_4_valid    ", (perfValidCount > (LoadQueueSize.U*3.U/4.U)))
  )
  generatePerfEvent()
  // end
}