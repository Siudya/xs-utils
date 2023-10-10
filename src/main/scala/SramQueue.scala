/** *************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

// See LICENSE.SiFive for license details.

package xs.utils

import xs.utils.sram._
import chisel3._
import chisel3.util._
import chisel3.experimental.{Direction, requireIsChiselType}
import org.chipsalliance.cde.config.Parameters
import xs.utils.mbist.MBISTPipeline

/** A hardware module implementing a Queue
 * @param gen The type of data to queue
 * @param entries The max number of entries in the queue
 * @param pipe True if a single entry queue can run at full throughput (like a pipeline). The ''ready'' signals are
 * combinationally coupled.
 * @param flow True if the inputs can be consumed on the same cycle (the inputs "flow" through the queue immediately).
 * The ''valid'' signals are coupled.
 * @param hasFlush True if generated queue requires a flush feature
 * @example {{{
 * val q = Module(new Queue(UInt(), 16))
 * q.io.enq <> producer.io.out
 * consumer.io.in <> q.io.deq
 * }}}
 */
class SRAMQueue[T <: Data](
                        val gen:            T,
                        val entries:        Int,
                        val pipe:           Boolean = false,
                        val flow:           Boolean = false,
                        val hasFlush:       Boolean = false,
                        // use in sram
                        val hasMbist:       Boolean = false,
                        val hasShareBus:    Boolean = false,
                        val hasClkGate:     Boolean = false,
                        val parentName:     String = "Unknown")
  (implicit p:Parameters) extends Module() {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  requireIsChiselType(gen)

  val io = IO(new QueueIO(gen, entries, hasFlush))
  val sram = Module(new SRAMTemplate(gen, entries, singlePort = false, bypassWrite = true, shouldReset = true,
    hasMbist = hasMbist, hasShareBus = hasShareBus,
    hasClkGate = hasClkGate, parentName = parentName + "queue_"))
  val mbistPl = MBISTPipeline.PlaceMbistPipeline(1,
    s"${parentName}_mbistPipe",
    hasMbist && hasShareBus
  )
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)
  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.enq.fire)
  val do_deq = WireDefault(io.deq.fire)
  val flush = io.flush.getOrElse(false.B)

  dontTouch(do_enq)
  dontTouch(do_deq)

  // ctrl logic
  when(do_enq) {
    enq_ptr.inc()
  }
  when(do_deq) {
    deq_ptr.inc()
  }
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }
  // when flush is high, empty the queue
  // Semantically, any enqueues happen before the flush.
  when(flush) {
    enq_ptr.reset()
    deq_ptr.reset()
    maybe_full := false.B
  }

  io.deq.valid := !empty
  io.enq.ready := !full

  sram.io.w(
      do_enq,
      io.enq.bits,
      enq_ptr.value,
      1.U
    )
  val deq_ptr_next = Mux(deq_ptr.value === (entries.U - 1.U), 0.U, deq_ptr.value + 1.U)
  val r_addr = WireDefault(Mux(do_deq, deq_ptr_next, deq_ptr.value))
  io.deq.bits := sram.io.r(true.B, r_addr).resp.data(0)

  if (flow) {
    when(io.enq.valid) { io.deq.valid := true.B }
    when(empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when(io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }

  val ptr_diff = enq_ptr.value - deq_ptr.value

  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(
      ptr_match,
      Mux(maybe_full, entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value, entries.asUInt + ptr_diff, ptr_diff)
    )
  }
}