package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{IORegisterRouter, RegField, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._

case class AXIMMIOConfig(
  val csrAddress: AddressSet
)

case object AXIMMIOKey extends Field[Option[AXIMMIOConfig]](None)

trait CanHaveAXIMMIO { this: BaseSubsystem =>
  private val portName = "aximmio"

  val axiNode = p(AXIMMIOKey) match {
    case Some(params) => {
      val axi = LazyModule(new AXIMMIO(params.csrAddress, pbus.beatBytes)(p))

      pbus.coupleTo (portName) {
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

class WithAXIMMIO(csrAddress: AddressSet) extends Config((site, here, up) => {
  case AXIMMIOKey => Some(AXIMMIOConfig(csrAddress=csrAddress))
})

abstract class MMIO(
  csrAddress: AddressSet,
  beatBytes: Int = 4
)(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "aximmio",
    compat = Seq("com", "aximmio"),
    base = csrAddress.base,
    size = csrAddress.mask+1,
    beatBytes = beatBytes))
{
  lazy val module = new LazyModuleImp(this) { outer =>
    regmap(
      beatBytes * 0 -> Seq(RegField.r(64, 12.U)),
      beatBytes * 1 -> Seq(RegField.r(64, 24.U)),
      beatBytes * 2 -> Seq(RegField.r(64, 36.U))
    )
  }
}

class AXIMMIO(
  csrAddress: AddressSet,
  beatBytes: Int = 4
)(implicit p: Parameters) extends MMIO(csrAddress, beatBytes) with HasAXI4ControlRegMap
