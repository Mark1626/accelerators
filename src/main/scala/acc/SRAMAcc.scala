package accelerators.acc

import accelerators.bus.BusParams
import accelerators.dsp.{TLReadQueue, TLWriteQueue}
import accelerators.ram.WishboneRAM
import freechips.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util._
import dspblocks.{DspBlock, TLChain, TLDspBlock}
import dsptools.numbers.Ring
import freechips.rocketchip.amba.axi4stream.AXI4StreamIdentityNode
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.regmapper.HasRegMap
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLFIFOFixer, TLManagerPortParameters}

case class SRAMAccParams(
  writeAddress: BigInt = 0x2200,
  readAddress: BigInt = 0x2300,
  depth: Int,
  p: Parameters
) extends BusParams

case object SRAMKey extends Field[Option[SRAMAccParams]](None)

//class SRAMBundle[T<:Data:Ring](proto: T) extends Bundle {
//  val data: T = proto.cloneType
//}
//
//object SRamBundle {
//  def apply[T<:Data:Ring](proto: T): SRAMBundle[T] = new SRAMBundle(proto)
//}

abstract class SRAMAccBlock[D, U, EO, EI, B<:Data, T<:Data:Ring]()(implicit p: Parameters)
  extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None
  def params: SRAMAccParams

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    val mod = Module(new WishboneRAM()(params.p))

    val idle :: busy :: Nil = Enum(2)
    val state = RegInit(idle)

    val data = RegInit(0.U(params.busWidth.W))
    val addr = RegInit(0.U(params.busWidth.W))
    val cyc = RegInit(false.B)
    val stb = RegInit(false.B)
    val sel = Reg(UInt(params.busWidth.W))
    val data_rd = RegInit(0.U(params.busWidth.W))

    when (in.valid) {
      data := in.bits.data.asUInt
      addr := data
    }

    in.ready := state === idle

    out.bits.data := DontCare

    // Start of request
    stb := in.valid && in.bits.last
    val we = addr(19, 16) === 0.U
    sel := addr(19, 16)

    mod.io.bus.cyc := cyc
    mod.io.bus.stb := false.B

    when (state === idle && stb && out.ready) {
      mod.io.bus.stb := stb
      mod.io.bus.cyc := true.B
      cyc := true.B
      state := busy
    } .elsewhen(mod.io.bus.ack) {
      cyc := false.B
      data_rd := mod.io.bus.data_rd.asUInt
      out.bits.data := mod.io.bus.data_rd.asUInt
      state := idle
    }

    mod.io.bus.sel := DontCare
    mod.io.bus.addr := addr(15, 0)
    mod.io.bus.data_wr := data
    mod.io.bus.we := we

    out.valid := mod.io.bus.ack
  }
}

class TLSRAMAccBlock[T<:Data:Ring](val params: SRAMAccParams)(implicit p: Parameters)
  extends SRAMAccBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T]()
  with TLDspBlock

class TLSRAMChain[T<:Data:Ring](proto: T, params: SRAMAccParams)(implicit p: Parameters)
  extends TLChain(Seq(
    TLWriteQueue(params.depth, AddressSet(params.writeAddress, 0xff), considerLast = true)(_),
    { implicit p: Parameters => {
      val mod = LazyModule(new TLSRAMAccBlock(params))
      mod
    }},
    TLReadQueue(params.depth, AddressSet(params.readAddress, 0xff))(_)
  ))

trait CanHavePeripherySRAM { this: BaseSubsystem =>
  val peripheral = p(SRAMKey) match {
    case Some(params) => {
      val chain = LazyModule(new TLSRAMChain(proto=UInt(params.busWidth.W), params))
      pbus.toVariableWidthSlave(Some("strSRAM")) { chain.mem.get := TLFIFOFixer() }
      Some(chain)
    }
    case None => None
  }
}

class WithPeripherySRAM(p: Parameters) extends Config((site, here, up) => {
  case SRAMKey => Some(SRAMAccParams(depth = 16, p=p))
})
