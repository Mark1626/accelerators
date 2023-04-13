package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink.{TLBuffer, TLFragmenter, _}

case class TLRAMXbarCSRConfig(
  val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
 )

case object TLRAMXbarCSRKey extends Field[Option[TLRAMXbarCSRConfig]](None)

trait CanHaveTLRAMXbarCSR { this: BaseSubsystem =>
  private val portName = "tlplus"

  val tlNode = p(TLRAMXbarCSRKey) match {
    case Some(params) => {
      val tl = LazyModule(new TLRAMXbarCSR(params.csrAddress, params.scratchpadAddress, pbus.beatBytes)(p))
      pbus.coupleTo(portName) {
        tl.node :*=
        _
      }
      Some(tl)
    }
    case None => None
  }
}

class WithTLRAMXbarCSR(csrAddress: AddressSet, scratchpadAddress: AddressSet) extends Config((site, here, up) => {
  case TLRAMXbarCSRKey => Some(TLRAMXbarCSRConfig(csrAddress=csrAddress, scratchpadAddress=scratchpadAddress))
})

class TLRAMXbarCSR(val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
  val beatBytes: Int = 4,
  val id: IdRange = IdRange(0, 1),
  val aligned: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val node = TLIdentityNode()
  val csr = LazyModule(new TLMMIO(csrAddress, beatBytes))
  val ram = TLRAM(
    address = scratchpadAddress,
    executable = false,
    beatBytes = beatBytes,
    devName = Some("com,TLramxbar"),
  )
  val ramXbar = TLXbar()

  ram := TLFragmenter(beatBytes, beatBytes * beatBytes) := TLBuffer() := ramXbar
  csr.node := TLFragmenter(beatBytes, beatBytes * beatBytes) := TLBuffer() := ramXbar

  ramXbar := node

  lazy val module = new LazyModuleImp(this) {}
}
