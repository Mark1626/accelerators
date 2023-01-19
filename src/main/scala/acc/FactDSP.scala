package accelerators.acc

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util._
import accelerators.dsp.{TLReadQueue, TLWriteQueueWithLast}
import dspblocks.{DspBlock, HasCSR, TLChain, TLDspBlock, TLHasCSR}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamIdentityNode, AXI4StreamMasterNode, AXI4StreamMasterParameters, AXI4StreamSlaveNode, AXI4StreamSlaveParameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegReadFn}
import freechips.rocketchip.tilelink.{TLBundle, TLChannelBeatBytes, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLFIFOFixer, TLManagerPortParameters, TLRegisterNode}
import dsptools.numbers._
import freechips.rocketchip.subsystem.BaseSubsystem

case class StreamingFactParams(
  writeAddress: BigInt = 0x2200,
  readAddress: BigInt = 0x2300,
  depth: Int
)

case object StreamingFactKey extends Field[Option[StreamingFactParams]](None)

class StreamingFactBundle[T<:Data:Ring](proto: T) extends Bundle {
  val data: T = proto.cloneType
}

object StreamingFactBundle {
  def apply[T<:Data:Ring](proto:T): StreamingFactBundle[T] = new StreamingFactBundle(proto)
}

class StreamingFactIO[T<:Data:Ring](proto: T) extends Bundle {
  val in = Flipped(Decoupled(StreamingFactBundle(proto)))
  val out = Decoupled(StreamingFactBundle(proto))
  val busy = Output(Bool())
}

object StreamingFactIO {
  def apply[T<:Data:Ring](proto: T): StreamingFactIO[T] = new StreamingFactIO(proto)
}

class StreamingFact[T<:Data:Ring](proto: T) extends Module {
  val io = IO(StreamingFactIO(proto))

  val idle :: run :: done :: Nil = Enum(3)
  val state: UInt = RegInit(idle)
  private val res = RegInit(1.U(proto.getWidth.W))
  val tmp: UInt = RegInit(1.U(proto.getWidth.W))

  io.in.ready       := state === idle
  io.out.valid      := state === done
  io.out.bits.data       := res

  // State Transition
  when(state === idle && io.in.valid) {
    state := run
  }.elsewhen(state === run && tmp === 1.U) {
    state := done
  }.elsewhen(state === done && io.out.ready) {
    state := idle
  }

  // State behaviour
  when(state === idle && io.in.valid) {
    tmp := io.in.bits.data
    res := 1.U
  }.elsewhen(state === run && tmp > 1.U) {
    res := res * tmp
    tmp := tmp - 1.U
  }

  io.busy := state =/= idle
}

abstract class StreamingFactBlock[D, U, EO, EI, B<:Data, T<:Data:Ring]
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

    val fact = Module(new StreamingFact(proto))

    in.ready := fact.io.in.ready
    fact.io.in.valid := in.valid && in.bits.last

    fact.io.in.bits := in.bits.data.asTypeOf(StreamingFactBundle(proto))

    fact.io.out.ready := out.ready
    out.valid := fact.io.out.valid

    out.bits.data := fact.io.out.bits.asUInt
  }
}

class TLStreamingFactBlock[T<:Data:Ring]
(
  val proto: T
)(implicit p: Parameters) extends
StreamingFactBlock[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle, T](proto)
with TLDspBlock

class TLStreamingFactChain[T<:Data:Ring](params: StreamingFactParams, proto: T)(implicit p: Parameters)
  extends TLChain(Seq(
    TLWriteQueueWithLast(params.depth, AddressSet(params.writeAddress, 0xff))(_),
    { implicit p: Parameters => {
      val streamingFact = LazyModule(new TLStreamingFactBlock(proto))
      streamingFact
    }},
    TLReadQueue(params.depth, AddressSet(params.readAddress, 0xff))(_)
  ))

trait CanHavePeripheryStreamingFact { this: BaseSubsystem =>
  val passthrough = p(StreamingFactKey) match {
    case Some(params) => {
      val chain = LazyModule(new TLStreamingFactChain(params, UInt(32.W)))
      pbus.toVariableWidthSlave(Some("streamingfact")) { chain.mem.get := TLFIFOFixer() }
      Some(chain)
    }
    case None => None
  }
}

class WithStreamingFact extends Config((site, here, up) => {
  case StreamingFactKey => Some(StreamingFactParams(depth = 16))
})

