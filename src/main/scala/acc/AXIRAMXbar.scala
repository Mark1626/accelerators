package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._

case class AXIRAMXbarConfig(
  val scratchpadAddress: AddressSet,
)

case object AXIRAMXbarKey extends Field[Option[AXIRAMXbarConfig]](None)

trait CanHaveAXIRAMXbar { this: BaseSubsystem =>
  private val portName = "axiplus"

  val axiNode = p(AXIRAMXbarKey) match {
    case Some(params) => {
      val axi = LazyModule(new AXIRAMXbar(params.scratchpadAddress, pbus.beatBytes)(p))
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

class WithAXIRAMXbar(scratchpadAddress: AddressSet) extends Config((site, here, up) => {
  case AXIRAMXbarKey => Some(AXIRAMXbarConfig(scratchpadAddress=scratchpadAddress))
})

class AXIRAMXbar(val scratchpadAddress: AddressSet,
                    val beatBytes: Int = 4,
                    val id: IdRange = IdRange(0, 1),
                    val aligned: Boolean = false
                   )(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()
  val ram = AXI4RAM(
    address = scratchpadAddress,
    executable = false,
    beatBytes = beatBytes,
    devName = Some("com,axiramxbar"),
    errors = Nil,
  )
  val ramXbar = AXI4Xbar()

  ram := ramXbar
  ramXbar := node

  lazy val module = new LazyModuleImp(this) {
    node.out.foreach { o => o._2.slave.slaves.foreach(s => println(s"${s.name} is ${s.interleavedId}")) }
  }
}
