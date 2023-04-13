package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink.{TLFragmenter, _}
import testchipip.TLHelper

case class TLRAMXbarCSRMasterConfig(
  val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
 )

case object TLRAMXbarCSRMasterKey extends Field[Option[TLRAMXbarCSRMasterConfig]](None)

trait CanHaveTLRAMXbarCSRMaster { this: BaseSubsystem =>
  private val portName = "TLplus"

  val tlNode = p(TLRAMXbarCSRMasterKey) match {
    case Some(params) => {
      val tl = LazyModule(new TLRAMXbarCSRMaster(params.csrAddress, params.scratchpadAddress, pbus.beatBytes, pbus.blockBytes)(p))
      pbus.coupleTo(portName){
        tl.mem :=
        _
      }

      pbus.fromPort(Some("tlmaster"))() := tl.tlSlave.tlMasterNode.tlNode

      Some(tl)
    }
    case None => None
  }
}

class WithTLRAMXbarCSRMaster(csrAddress: AddressSet, scratchpadAddress: AddressSet) extends Config((site, here, up) => {
  case TLRAMXbarCSRMasterKey => Some(TLRAMXbarCSRMasterConfig(csrAddress=csrAddress, scratchpadAddress=scratchpadAddress))
})

class TLReadMaster(
                     val address: AddressSet,
                     id: IdRange = IdRange(0, 1),
                     beatBytes: Int = 4,
                     aligned: Boolean = false,
                   )(implicit p: Parameters) extends LazyModule {
  val tlNode = TLHelper.makeClientNode(
    name = name,
    sourceId = id
  )

  lazy val module = new LazyModuleImp(this) {
    val (mem, edge) = tlNode.out.head
    val addr = address

    val reading = RegInit(false.B)

    // Address of ram to read
    val req = IO(Flipped(Decoupled(UInt(64.W))))
    val resp = IO(Decoupled(UInt(64.W)))
    val idle = IO(Output(Bool()))

    val rdata = RegInit(0.U)
    val respWait = RegInit(false.B)

    req.ready := mem.a.ready && !respWait
    resp.valid := false.B

    // Handle incoming IO req
//    when (req.fire) {
//      reading := true.B
//    }

    mem.a.valid := !reading && req.valid
    mem.a.bits := edge.Get(
      fromSource = 0.U,
      toAddress = req.bits,
      lgSize = log2Ceil(beatBytes).U)._2

    when (edge.done(mem.a)) {
      reading := true.B
    }

    mem.d.ready := true.B

    when (mem.d.fire) {
      rdata := mem.d.bits.data
      respWait := true.B
      reading := false.B
    }

    when (resp.ready && respWait) {
      resp.valid := true.B
      resp.bits := rdata
      respWait := false.B
    }

    idle := !reading && !respWait
  }
}

abstract class CSRMMIOSlave(
                          csrAddress: AddressSet,
                          scratchpadAddress: AddressSet,
                          beatBytes: Int = 4
                        )(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "mmio",
    compat = Seq("com", "mmio"),
    base = csrAddress.base,
    size = csrAddress.mask+1,
    beatBytes = beatBytes))
{

  val tlMasterNode = LazyModule(new TLReadMaster(scratchpadAddress)(p))

  lazy val module = new LazyModuleImp(this) { outer =>

    val impl = tlMasterNode.module

    val idle = Wire(Bool())
    val req = Wire(DecoupledIO(UInt(64.W)))
    val resp = Wire(DecoupledIO(UInt(64.W)))

    impl.req <> req
    impl.resp <> resp
    idle := impl.idle

//    val idle = IO(Input(Bool()))
//    val req = IO(Flipped(Decoupled(UInt(64.W))))
//    val resp = IO(Decoupled(UInt(64.W)))

    regmap(
      beatBytes * 0 -> Seq(RegField.r(64, idle)),
      beatBytes * 1 -> Seq(RegField.w(64, req)),
      beatBytes * 2 -> Seq(RegField.r(64, resp))
    )
  }
}

class TLMMIOSlave(
   csrAddress: AddressSet,
   scratchpadAdderss: AddressSet,
   beatBytes: Int = 4
 )(implicit p: Parameters) extends CSRMMIOSlave(csrAddress, scratchpadAdderss, beatBytes) with HasTLControlRegMap

class TLRAMXbarCSRMaster(val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
  val beatBytes: Int = 4,
  val blockBytes: Int = 4,
  val id: IdRange = IdRange(0, 1),
  val aligned: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val mem = TLIdentityNode()

  val tlSlave = LazyModule(new TLMMIOSlave(csrAddress, scratchpadAddress, beatBytes))
//  val tlMaster = LazyModule(new TLReadMaster(scratchpadAddress))

  val ram = TLRAM(
    address = scratchpadAddress,
    executable = false,
    beatBytes = beatBytes,
    devName = Some("com,TLramxbar"),
  )
  val ramXbar = TLXbar()
  val topXbar = TLXbar()

  ram := TLFragmenter(beatBytes, beatBytes * beatBytes) := TLBuffer() := topXbar
  tlSlave.node := TLFragmenter(beatBytes, beatBytes * beatBytes) := TLBuffer() := topXbar

  topXbar := mem

  lazy val module = new LazyModuleImp(this) {
  }
}
