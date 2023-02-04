package accelerators.bus

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._

case object BusWidth extends Field[Int]

trait BusParams {
  implicit val p: Parameters
  val busWidth = p(BusWidth)
}

class WishboneBundle(val N: Int) extends Bundle {
  assert(N % 8 == 0)
  val NUM_BYTES = N / 8

  val cyc = Input(Bool())
  val stb = Input(Bool())
  val we = Input(Bool())
  val sel = Input(UInt(NUM_BYTES.W))
  val addr = Input(UInt(N.W))
  val data_wr = Input(UInt(N.W))

  val ack = Output(Bool())
  val stall = Output(Bool())
  val err = Output(Bool())
  val data_rd = Output(UInt(N.W))
}

class WishboneSlave(val busWidth: Int) extends Bundle {
  val bus = new WishboneBundle(busWidth)
}

class WishboneMaster(val busWidth: Int) extends Bundle {
  val bus = Flipped(new WishboneBundle(busWidth))
}

