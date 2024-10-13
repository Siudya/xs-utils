package xs

import chisel3.Data

package object utils {
  type SRAMQueue[T <: Data] = _root_.xs.utils.sram.SramQueue[T]
}
