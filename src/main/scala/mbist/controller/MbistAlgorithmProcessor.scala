package xs.utils.mbist.controller

import chisel3._
import chisel3.util._
import xs.utils.mbist.algorithm.AlgoServiceIntf

class MbistAlgorithmProcessor(blockSize: Int, param: MbistControllerParam) extends Module {
  val io = IO(new Bundle {
    val instrReq = Flipped(new AlgoServiceIntf(blockSize, param))
    val sram = Input(Valid(new SramInfo(param)))
    val wreq = Output(Valid(new MbistReqBundle(param)))
    val rreq = Output(Valid(new MbistReqBundle(param)))
    val start = Input(Bool())
    val kill = Input(Bool())
    val singlePort = Output(Bool())
  })
  private val restart = io.start || io.kill
  private val instrPtr = RegInit(1.U(blockSize.W))
  private val algoBlock = RegEnable(io.instrReq.ackD, io.instrReq.req & io.instrReq.ack)
  private val instrSel = Mux1H(instrPtr, algoBlock.ops)
  private val patGen = Module(new MbistPatternGenerator(param))
  patGen.io.sram := io.sram
  io.wreq := patGen.io.wreq
  io.rreq := patGen.io.rreq
  private val lastInstr = instrSel.last && instrSel.valid
  private val lastFire = lastInstr & patGen.io.instr.fire & patGen.io.lastLoop

  private val s_idle :: s_ram_set :: s_instr_get :: s_loop :: Nil = Enum(4)
  private val state = RegInit(s_idle)
  switch(state) {
    is(s_idle) {
      state := Mux(io.sram.valid, s_ram_set, state)
    }
    is(s_ram_set) {
      state := Mux(restart, s_idle, Mux(io.instrReq.ack, s_instr_get, state))
    }
    is(s_instr_get) {
      state := Mux(restart, s_idle, s_loop)
    }
    is(s_loop) {
      state := Mux(restart, s_idle, Mux(lastFire, s_ram_set, state))
    }
  }

  io.instrReq.req := state === s_ram_set

  patGen.io.loop.valid := state === s_instr_get
  patGen.io.loop.bits := algoBlock.loop
  patGen.io.instr.valid := state === s_loop
  patGen.io.instr.bits := instrSel
  when(patGen.io.instr.fire) {
    instrPtr := Mux(lastInstr, 1.U(blockSize.W), Cat(instrPtr(blockSize - 2, 0), false.B))
  }

  io.singlePort := patGen.io.singlePort
}
