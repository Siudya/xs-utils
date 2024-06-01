package xs.utils.mbist.algorithm

import chisel3._
import PatternCode._
import xs.utils.mbist.controller.MbistControllerParam

trait SMarchCHKBvcdDesc extends AlgoUtil {

  def for_mask_firstrow(block: => Any): Unit = {
    val res = new LoopContext
    res.ma = true
    res.ra = false
    res.ri = 0
    ctxs += res
    block
  }

  s2p_start()
  //phase 2
  for_col(0) {
    wr(patA)
  }
  for_col(0) {
    wr(pat5)
  }

  //phase 3
  for_mask_firstrow {
    wr(pat0)
    wrs(pat1)
  }
  for_col(0) {
    wr(pat0)
    wr(pat1)
  }
  for_col(0) {
    wr(pat1)
    wrm0(pat0, pat1)
  }

  //Phase 3.6
  for_col(0) {
    wr(pat1)
  }
  for_col(1) {
    wr(pat0)
  }
  for_col(0) {
    wr(pat1)
  }
  for_col(1) {
    wr(pat0)
  }

  //Phase 5
  for_elm {
    rnc()
    w(patC)
  }
  s2p_end()

  //Phase 5.1
  for_elm(rowFirst = true) {
    rc(patC)
    w(patIC)
    wr(patC)
  }

  //Phase 5.2
  for_elm(rowFirst = true) {
    rc(patC)
    w(patIC)
  }

  //Phase 5.3
  for_elm(rowFirst = true) {
    wr(patC)
    rc(patC)
    w(patC)
  }

  //Phase 5.5
  for_elm {
    rc(patC, true, false)
    w(patC, false, true)
  }

  p2p_start()
  //Phase 6
  for_elm {
    rc(patC)
    rnc()
  }

  //Phase 6.1
  for_elm {
    rc(patC, false, false)
    w(patIC, false, true)
  }
  p2p_end()

  //Phase 6.2
  for_elm(rowFirst = true) {
    rc(patIC)
    w(patC)
    wr(patIC)
  }

  //Phase 6.3
  for_elm(rowFirst = true) {
    rc(patIC)
    w(patC)
  }

  //Phase 6.4
  for_elm(rowFirst = true) {
    wr(patIC)
    rc(patIC)
    w(patIC)
  }

  //Phase 6.5
  for_elm {
    rc(patIC, true, false)
    w(patIC, false, true)
  }

  p2e_start()
  //Phase 7
  for_elm {
    rc(patIC)
    rnc()
  }
  p2e_end()

  //Phase 7.1
  for_elm {
    rc(patIC, true, false)
    w(patC)
  }

  //Phase 10
  for_elm(rowFirst = true) {
    wr(pat0)
  }

  //Pahse 11
  for_elm(rowFirst = true) {
    rc(pat0)
    w(pat1)
    rc(pat1)
    w(pat1)
  }

  //Phase 12
  for_elm(rowFirst = true) {
    rc(pat1)
    w(pat0)
    rc(pat0)
    w(pat0)
  }

  //Phase 13
  for_elm(true, true, false) {
    rc(pat0)
    w(pat1)
    rc(pat1)
    w(pat1)
  }

  //Phase 14
  for_elm(true, true, false) {
    rc(pat1)
    w(pat0)
    rc(pat0)
    w(pat0)
  }

  //Phase 15
  for_elm(rowFirst = true) {
    rc(pat0)
    w(pat1)
    rc_adj(pat0)
    w_adj(pat0)
    rc(pat1)
    w(pat0)
  }

  //Phase 17
  for_elm(rowFirst = true) {
    wr(pat1)
  }

  //Phase 18
  for_elm(rowFirst = true) {
    rc(pat1)
    w(pat0)
    rc_adj(pat1)
    w_adj(pat1)
    rc(pat0)
    w(pat1)
  }

  //Phase 20
  for_elm(rowFirst = true) {
    wr(pat0)
  }
}

class SMarchCHKBvcd(val param: MbistControllerParam) extends Module with SMarchCHKBvcdDesc with AlgoInstantiation