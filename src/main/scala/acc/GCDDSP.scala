package accelerators.acc

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util._
import accelerators.dsp.{TLReadQueue, TLWriteQueue}
import dspblocks.{DspBlock, HasCSR, TLChain, TLDspBlock, TLHasCSR}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamIdentityNode, AXI4StreamMasterNode, AXI4StreamMasterParameters, AXI4StreamSlaveNode, AXI4StreamSlaveParameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegReadFn}
import freechips.rocketchip.tilelink.{TLBundle, TLChannelBeatBytes, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLFIFOFixer, TLManagerPortParameters, TLRegisterNode}
import dsptools.numbers._
import freechips.rocketchip.subsystem.BaseSubsystem

case class StreamingGCDParams(
  writeAddress: BigInt = 0x2200,
  readAddress: BigInt = 0x2300,
  depth: Int
)

case object StreamingGCDKey extends Field[Option[StreamingGCDParams]](None)

class StreamingGCDBundle[T<:Data:Ring](proto: T) extends Bundle {
  val data: T = proto.cloneType
}

object StreamingGCDBundle {
  def apply[T<:Data:Ring](proto:T): StreamingGCDBundle[T] = new StreamingGCDBundle(proto)
}

class StreamingGCDIO[T<:Data:Ring](proto: T) extends Bundle {
  val in = Flipped(Decoupled(Vec(2, StreamingGCDBundle(proto))))
  val out = Decoupled(StreamingGCDBundle(proto))
  val busy = Output(Bool())
}

object StreamingGCDIO {
  def apply[T<:Data:Ring](proto: T): StreamingGCDIO[T] = new StreamingGCDIO(proto)
}

class StreamingGCD[T<:Data:Ring](proto: T) extends Module {
  val io = IO(StreamingGCDIO(proto))

  val s_idle :: s_run :: s_done :: Nil = Enum(3)

  val state = RegInit(s_idle)
  val tmp = Reg(UInt(proto.getWidth.W))
  val gcd = Reg(UInt(proto.getWidth.W))

  io.in.ready := state === s_idle
  io.out.valid := state === s_done
  io.out.bits.data := gcd

  when(state === s_idle && io.in.valid) {
    state := s_run
  }.elsewhen(state === s_run && tmp === 0.U) {
    state := s_done
  }.elsewhen(state === s_done && io.out.ready) {
    state := s_idle
  }

  when(state === s_idle && io.in.valid) {
    gcd := io.in.bits(0).data
    tmp := io.in.bits(1).data
  }.elsewhen(state === s_run) {
    when(gcd > tmp) {
      gcd := gcd - tmp
    }.otherwise {
      tmp := tmp - gcd
    }
  }

  io.busy := state =/= s_idle
}

abstract class StreamingGCDBlock[D, U, EO, EI, B<:Data, T<:Data:Ring]
(
  proto: T
)(implicit p: Parameters) extends DspBlock[D, U, EO, EI, B] {
  val streamNode = AXI4StreamIdentityNode()
  val mem = None

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    require(streamNode.out.length == 1)

    val in = streamNode.in.head._1
    val out = streamNode.out.head._1

    val gcd = Module(new StreamingGCD(proto))

    val x = Reg(proto.cloneType)

    when (in.valid) {
      x := in.bits.data
    }

    in.ready := gcd.io.in.ready
//    gcd.io.in.valid := in.valid
    gcd.io.in.valid := in.bits.last && in.valid

    gcd.io.in.bits(0) := x.asTypeOf(StreamingGCDBundle(proto))
    gcd.io.in.bits(1) := in.bits.data.asTypeOf(StreamingGCDBundle(proto))

    gcd.io.out.ready := out.ready
    out.valid := gcd.io.out.valid

    out.bits.data := gcd.io.out.bits.asUInt
  }
}

class TLStreamingGCDBlock[T<:Data:Ring]
(
  val proto: T
)(implicit p: Parameters) extends
StreamingGCDBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T](proto)
with TLDspBlock

class TLStreamingGCDChain[T<:Data:Ring](params: StreamingGCDParams, proto: T)(implicit p: Parameters)
  extends TLChain(Seq(
    TLWriteQueue(params.depth, AddressSet(params.writeAddress, 0xff), considerLast=true)(_),
    { implicit p: Parameters => {
      val streamingGCD = LazyModule(new TLStreamingGCDBlock(proto))
      streamingGCD
    }},
    TLReadQueue(params.depth, AddressSet(params.readAddress, 0xff))(_)
  ))

trait CanHavePeripheryStreamingGCD { this: BaseSubsystem =>
  val passthrough = p(StreamingGCDKey) match {
    case Some(params) => {
      val gcdChain = LazyModule(new TLStreamingGCDChain(params, UInt(32.W)))
      pbus.toVariableWidthSlave(Some("streaminggcd")) { gcdChain.mem.get := TLFIFOFixer() }
      Some(gcdChain)
    }
    case None => None
  }
}

class WithStreamingGCD extends Config((site, here, up) => {
  case StreamingGCDKey => Some(StreamingGCDParams(depth = 16))
})

