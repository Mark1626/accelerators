package accelerators.ram

import accelerators.bus.{BusParams, WishboneSlave}
import freechips.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._

case object RAMBlockSize extends Field[Int]
case object DataWidth extends Field[Int]
case object ReadBuffer extends Field[Int]

trait RAMBankParams {
  implicit val p: Parameters
  val ramSize = p(RAMBlockSize)
  val dataWidth = p(DataWidth)
  val readBankSize = p(ReadBuffer)

  val addrWidth = log2Ceil(ramSize)
}

class WishboneRAM()(implicit val p: Parameters) extends Module
  with BusParams
  with RAMBankParams {
  val io = IO(new WishboneSlave(busWidth))

  val (read, write) = {
    val mem = SyncReadMem(ramSize, UInt(busWidth.W))
    def read(addr: UInt, ren: Bool): Data = mem.read(addr, ren)
    def write(addr: UInt, wdata: UInt): Unit = mem.write(addr, wdata)
    (read _, write _)
  }

  private val ack = RegInit(false.B)
  private val data = RegInit(0.U(busWidth.W))

  io.bus.err := false.B
  io.bus.sel := DontCare
  io.bus.stall := false.B

  io.bus.ack := ack

  io.bus.data_rd := Mux(io.bus.we, data, read(io.bus.addr, !io.bus.we))

  ack := false.B
  data := 0.U
  when (io.bus.stb && io.bus.cyc && !io.bus.ack) {
    // Write Request
    when(io.bus.we) {
      write(io.bus.addr, io.bus.data_wr)
      data := io.bus.data_wr
    }

    ack := true.B
  }
}
