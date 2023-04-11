package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._

case class AXIRAMXbarCSRConfig(
  val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
 )

case object AXIRAMXbarCSRKey extends Field[Option[AXIRAMXbarCSRConfig]](None)

trait CanHaveAXIRAMXbarCSR { this: BaseSubsystem =>
  private val portName = "axiplus"

  val axiNode = p(AXIRAMXbarCSRKey) match {
    case Some(params) => {
      val axi = LazyModule(new AXIRAMXbarCSR(params.csrAddress, params.scratchpadAddress, pbus.beatBytes)(p))
      pbus.coupleTo(portName) {
        axi.node :=
          AXI4Buffer() :=
          TLToAXI4() :=
          TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true) :=
          _
      }
      Some(axi)
    }
    case None => None
  }
}

class WithAXIRAMXbarCSR(csrAddress: AddressSet, scratchpadAddress: AddressSet) extends Config((site, here, up) => {
  case AXIRAMXbarCSRKey => Some(AXIRAMXbarCSRConfig(csrAddress=csrAddress, scratchpadAddress=scratchpadAddress))
})

class AXIRAMXbarCSR(val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
  val beatBytes: Int = 4,
  val id: IdRange = IdRange(0, 1),
  val aligned: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()
  val csr = LazyModule(new AXIMMIO(csrAddress, beatBytes))
  val ram = AXI4RAM(
    address = scratchpadAddress,
    executable = false,
    beatBytes = beatBytes,
    devName = Some("com,axiramxbar"),
    errors = Nil,
  )
  val ramXbar = AXI4Xbar()

  ram := ramXbar
  csr.node := ramXbar

  ramXbar := node

  lazy val module = new LazyModuleImp(this) {
    node.out.foreach { o => o._2.slave.slaves.foreach(s => println(s"${s.name} is ${s.interleavedId}")) }
  }
}
