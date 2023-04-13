package accelerators.acc

import chipsalliance.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink.{HasTLControlRegMap, TLFragmenter}

case class TLMMIOConfig(
  val csrAddress: AddressSet
)

case object TLMMIOKey extends Field[Option[TLMMIOConfig]](None)

trait CanHaveTLMMIO { this: BaseSubsystem =>
  private val portName = "aximmio"

  val axiNode = p(TLMMIOKey) match {
    case Some(params) => {
      val tl = LazyModule(new TLMMIO(params.csrAddress, pbus.beatBytes)(p))

      pbus.coupleTo (portName) {
        tl.node :=
          TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true) :=
          _
      }
      Some(tl)
    }
    case None => None
  }
}

class WithTLMMIO(csrAddress: AddressSet) extends Config((site, here, up) => {
  case TLMMIOKey => Some(TLMMIOConfig(csrAddress=csrAddress))
})

class TLMMIO(
   csrAddress: AddressSet,
   beatBytes: Int = 4,
 )(implicit p: Parameters) extends MMIO(csrAddress, beatBytes) with HasTLControlRegMap

