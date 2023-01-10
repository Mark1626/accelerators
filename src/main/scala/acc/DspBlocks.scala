package accelerators.acc

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{Decoupled, Queue}
import dspblocks.{DspBlock, HasCSR, TLHasCSR}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamMasterNode, AXI4StreamMasterParameters, AXI4StreamSlaveNode, AXI4StreamSlaveParameters}
import freechips.rocketchip.diplomacy.{AddressSet, Description, LazyModule, LazyModuleImp, ResourceBindings, SimpleDevice}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn}
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLManagerPortParameters, TLRegisterNode}

abstract class WriteQueue[D, U, E, O, B <: Data]
(
  val depth: Int,
  val streamParams: AXI4StreamMasterParameters = AXI4StreamMasterParameters()
)(implicit p: Parameters) extends DspBlock[D, U, E, O, B] with HasCSR {
  val streamNode = AXI4StreamMasterNode(streamParams)
  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1)
    val out = streamNode.out.head._1
    val width = out.params.n * 8
    val queue = Module(new Queue(UInt(out.params.dataBits.W), depth))

    out.valid             := queue.io.deq.valid
    out.bits.data         := queue.io.deq.bits
    // Do I want to use this?
    out.bits.last         := false.B
    queue.io.deq.ready    := out.ready

    regmap(
      0x0 -> Seq(RegField.w(width, queue.io.enq)),
      (width+7)/8 -> Seq(RegField.r(width, queue.io.count)),
    )
  }
}

class TLWriteQueue(depth: Int, csrAddress: AddressSet, beatBytes: Int)
                  (implicit p: Parameters) extends WriteQueue[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle
](depth) with TLHasCSR {
  val devname = "tlqueuein"
  val devcompat = Seq("mark", "dspgcd")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  override val mem = Some(TLRegisterNode(address=Seq(csrAddress), device = device, beatBytes = beatBytes))
}

object TLWriteQueue {
  def apply(
             depth: Int = 8,
             csrAddress: AddressSet = AddressSet(0x2000, 0xff),
             beatBytes: Int = 8,
           )(implicit p: Parameters) = {
    val writeQueue = LazyModule(new TLWriteQueue(depth, csrAddress, beatBytes))
    writeQueue
  }
}

abstract class WriteQueueWithLast[D, U, E, O, B <: Data]
(
  val depth: Int,
  val streamParams: AXI4StreamMasterParameters = AXI4StreamMasterParameters()
)(implicit p: Parameters) extends DspBlock[D, U, E, O, B] with HasCSR {
  val streamNode = AXI4StreamMasterNode(streamParams)
  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1)
    val out = streamNode.out.head._1
    val width = out.params.n * 8
    val queue = Module(new Queue(UInt(out.params.dataBits.W), depth))
    val last = RegInit(true.B)

    out.valid             := queue.io.deq.valid
    out.bits.data         := queue.io.deq.bits
    // Do I want to use this?
    out.bits.last         := last
    queue.io.deq.ready    := out.ready

    regmap(
      0x00 -> Seq(RegField.w(width, queue.io.enq)),
      0x08 -> Seq(RegField.w(1, last, RegFieldDesc("last", "lastbit", reset=Some(0)))),
      0x10 -> Seq(RegField.r(width, queue.io.count)),
    )
  }
}

class TLWriteQueueWithLast(depth: Int, csrAddress: AddressSet, beatBytes: Int)
                  (implicit p: Parameters) extends WriteQueueWithLast[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle
](depth) with TLHasCSR {
  val devname = "tlqueuein"
  val devcompat = Seq("mark", "dspgcdlast")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  override val mem = Some(TLRegisterNode(address=Seq(csrAddress), device = device, beatBytes = beatBytes))
}

object TLWriteQueueWithLast {
  def apply(
             depth: Int = 8,
             csrAddress: AddressSet = AddressSet(0x2000, 0xff),
             beatBytes: Int = 8,
           )(implicit p: Parameters) = {
    val writeQueue = LazyModule(new TLWriteQueueWithLast(depth, csrAddress, beatBytes))
    writeQueue
  }
}

abstract class ReadQueue[D, U, E, O, B <: Data]
(
  val depth: Int,
  val streamParams: AXI4StreamSlaveParameters = AXI4StreamSlaveParameters()
)(implicit p: Parameters) extends DspBlock[D, U, E, O, B] with HasCSR {
  val streamNode = AXI4StreamSlaveNode(streamParams)
  lazy val module = new LazyModuleImp(this) {
    require(streamNode.in.length == 1)
    val in = streamNode.in.head._1
    val out = Wire(Decoupled(UInt()))
    val width = in.params.n * 8
    val queue = Module(new Queue(UInt(in.params.dataBits.W), depth))

    queue.io.enq.valid        := in.valid
    queue.io.enq.bits         := in.bits.data
    in.ready                  := queue.io.enq.ready

    out.valid                 := queue.io.deq.valid
    out.bits                  := queue.io.deq.bits
    queue.io.deq.ready        := out.ready

    regmap(
      0x0   -> Seq(RegField.r(width, RegReadFn(out))),
      (width+7)/8 -> Seq(RegField.r(width, queue.io.count)),
    )
  }
}

class TLReadQueue( depth: Int, csrAddress: AddressSet, beatBytes: Int)
                 (implicit p: Parameters) extends ReadQueue[
  TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle
](depth) with TLHasCSR {
  val devname = "tlqueueout"
  val devcompat = Seq("mark", "dspgcd")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = device, beatBytes = beatBytes))
}

object TLReadQueue {
  def apply(
             depth: Int = 8,
             csrAddress: AddressSet = AddressSet(0x2100, 0xff),
             beatBytes: Int = 8)(implicit p: Parameters) = {
    val readQueue = LazyModule(new TLReadQueue(depth = depth, csrAddress = csrAddress, beatBytes = beatBytes))
    readQueue
  }
}