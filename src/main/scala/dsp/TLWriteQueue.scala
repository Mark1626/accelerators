package accelerators.dsp

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.Queue
import dspblocks.{DspBlock, HasCSR, TLHasCSR}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamMasterNode, AXI4StreamMasterParameters}
import freechips.rocketchip.diplomacy.{AddressSet, Description, LazyModule, LazyModuleImp, ResourceBindings, SimpleDevice}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLManagerPortParameters, TLRegisterNode}


abstract class WriteQueue[D, U, E, O, B <: Data]
(
  val depth: Int,
  val considerLast: Boolean,
  val streamParams: AXI4StreamMasterParameters = AXI4StreamMasterParameters(),
)(implicit p: Parameters) extends DspBlock[D, U, E, O, B] with HasCSR {
  val streamNode = AXI4StreamMasterNode(streamParams)
  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1)
    val out = streamNode.out.head._1
    val width = out.params.n * 8
    val queue = Module(new Queue(UInt(out.params.dataBits.W), depth))

    out.valid             := queue.io.deq.valid
    out.bits.data         := queue.io.deq.bits

    queue.io.deq.ready    := out.ready

    if (considerLast) {
      val last = RegInit(true.B)
      out.bits.last := last
      regmap(
        0x0 -> Seq(RegField.w(width, queue.io.enq)),
        (width + 7) / 8 -> Seq(RegField.r(width, queue.io.count)),
        (2*width + 7) / 8 -> Seq(RegField.w(1, last)),
      )
    } else {
      out.bits.last := false.B
      regmap(
        0x0 -> Seq(RegField.w(width, queue.io.enq)),
        (width + 7) / 8 -> Seq(RegField.r(width, queue.io.count)),
      )
    }


  }
}

class TLWriteQueue(depth: Int, csrAddress: AddressSet, beatBytes: Int, considerLast: Boolean = false)
                  (implicit p: Parameters) extends WriteQueue[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle
](depth, considerLast) with TLHasCSR {
  val devname = "tlqueuein"
  val devcompat = Seq("mark", "dspwrite")
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
             considerLast: Boolean = false
           )(implicit p: Parameters) = {
    val writeQueue = LazyModule(new TLWriteQueue(depth, csrAddress, beatBytes, considerLast))
    writeQueue
  }
}