package accelerators.acc

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dspblocks.{DspBlock, HasCSR, TLChain, TLDspBlock, TLHasCSR}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamIdentityNode, AXI4StreamMasterNode, AXI4StreamMasterParameters, AXI4StreamSlaveNode, AXI4StreamSlaveParameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegReadFn}
import freechips.rocketchip.tilelink.{TLBundle, TLChannelBeatBytes, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLFIFOFixer, TLManagerPortParameters, TLRegisterNode}
import dsptools.numbers._
import freechips.rocketchip.subsystem.BaseSubsystem

case class StreamingPositivesParams(
  w: Int,
  bp: Int,
  threshold: FixedPoint,
  writeAddress: BigInt = 0x2200,
  readAddress: BigInt = 0x2300,
  depth: Int
)

case object StreamingPositivesKey extends Field[Option[StreamingPositivesParams]](None)

class StreamingPositivesBundle(w: Int, bp: Int) extends Bundle {
  val data = FixedPoint(w.W, bp.BP)
}

object StreamingPositivesBundle {
  def apply(w: Int, bp: Int): StreamingPositivesBundle = new StreamingPositivesBundle(w, bp)
}

class StreamingPositivesIO(w: Int, bp: Int) extends Bundle {
  val in = Flipped(Decoupled(FixedPoint(w.W, bp.BP)))
  val out = Decoupled(FixedPoint(w.W, bp.BP))
  val threshold = Input(FixedPoint(w.W, bp.BP))
}

class StreamingPositivesChiselModule(w: Int, bp: Int) extends Module {
  val io = IO(new StreamingPositivesIO(w, bp))

  val s_idle :: s_busy :: s_done :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val v   = Reg(FixedPoint(w.W, bp.BP))
  val res = Reg(FixedPoint(w.W, 0.BP))
  val done = RegInit(false.B)

  io.in.ready := state === s_idle
  io.out.valid := state === s_done
  io.out.bits := res

  when(state === s_idle && io.in.valid) {
    state := s_busy
    res := 0.F(0.BP)
    done := false.B
  }.elsewhen(state === s_busy && done) {
    state := s_done
  }.elsewhen(state === s_done && io.out.ready) {
    state := s_idle
  }

  when (state === s_idle && io.in.valid) {
    v := io.in.bits
  }.elsewhen(state === s_busy) {
    when (io.threshold > v) {
      res := 1.F(0.BP)
    }.otherwise {
      res := 0.F(0.BP)
    }
    done := true.B
  }
}

abstract class StreamingPositivesBlock[D, U, EO, EI, B<:Data, T<:Data:Ring]
(
  w: Int,
  bp: Int,
  threshold: FixedPoint
)(implicit p: Parameters) extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    val mod = Module(new StreamingPositivesChiselModule(w, bp))
    val thresholdReg = RegInit(threshold)

    in.ready := mod.io.in.ready
    mod.io.in.valid := in.valid
    mod.io.in.bits := in.bits.data.asTypeOf(StreamingPositivesBundle(w, bp).data)

    mod.io.threshold := thresholdReg

    mod.io.out.ready := out.ready
    out.valid := mod.io.out.valid
    out.bits.data := mod.io.out.bits.asUInt
  }
}

class TLStreamingPositivesBlock
(
  val w: Int,
  val bp: Int,
  val threshold: FixedPoint
)(implicit p: Parameters) extends
StreamingPositivesBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, FixedPoint](
  w, bp, threshold
) with TLDspBlock

class TLStreamingPositivesChain(params: StreamingPositivesParams)(implicit p: Parameters)
  extends TLChain(Seq(
    TLWriteQueue(params.depth, AddressSet(params.writeAddress, 0xff))(_),
    { implicit p: Parameters =>
      val mod = LazyModule(new TLStreamingPositivesBlock(params.w, params.bp, params.threshold))
      mod
    },
    TLReadQueue(params.depth, AddressSet(params.readAddress, 0xff))(_)
  ))

trait CanHavePeripheryStreamingPositives { this: BaseSubsystem =>
  val mod = p(StreamingPositivesKey) match {
    case Some(params) => {
      val chain = LazyModule(new TLStreamingPositivesChain(
        params = params))
      pbus.toVariableWidthSlave(Some("streamingPositives")) { chain.mem.get := TLFIFOFixer() }
      Some(chain)
    }
    case None => None
  }
}

class WithStreamingPositives extends Config((site, here, up) => {
  case StreamingPositivesKey => Some(StreamingPositivesParams(w=32, bp=5, threshold=0.F(0.BP),depth = 16))
})
