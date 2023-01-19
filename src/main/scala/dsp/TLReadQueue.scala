package accelerators.dsp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{Decoupled, Queue}
import dspblocks.{DspBlock, HasCSR, TLHasCSR}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamSlaveNode, AXI4StreamSlaveParameters}
import freechips.rocketchip.diplomacy.{AddressSet, Description, LazyModule, LazyModuleImp, ResourceBindings, SimpleDevice}
import freechips.rocketchip.regmapper.{RegField, RegReadFn}
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLManagerPortParameters, TLRegisterNode}

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