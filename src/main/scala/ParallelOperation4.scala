package xs.utils

import chisel3._
import chisel3.util._
object ParallelOperation4 {
  def apply[T](xs: Seq[T], func: (T, T, T, T) => T, padding:T): T = {
    require(xs.nonEmpty)
    xs match {
      case Seq()            => padding
      case Seq(a)           => a
      case Seq(a, b)        => func(a, b, padding, padding)
      case Seq(a, b, c)     => func(a, b, c, padding)
      case Seq(a, b, c, d)  => func(a, b, c, d)
      case _ =>
        val results = xs.grouped(4).map(apply(_, func, padding)).toSeq
        apply(results, func, padding)
    }
  }
}