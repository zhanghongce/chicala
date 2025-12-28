package basic.ast

import chisel3._
import chisel3.util._

class MuxCatWidth(width: Int) extends Module {
  val io = IO(new Bundle {
    val sel = Input(Bool())
    val a   = Input(UInt(width.W))
    val b   = Input(UInt(width.W))
    val out = Output(UInt((width * 2).W))
  })

  def widen(a: UInt, b: UInt, s: Bool): UInt = {
    val cat1 = Cat(a, b)
    val cat2 = Cat(0.U(width.W), a)
    Mux(s, cat1, cat2)
  }

  io.out := widen(io.a, io.b, io.sel)
}
