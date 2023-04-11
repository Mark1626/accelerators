package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._
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
        //TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true) :=
        _
      }

      pbus.coupleFrom("tlmaster") {
        _ :=
        tl.tlMaster.tlNode
      }

//      pbus.coupleFrom("TLmaster") {
//        _ :=
//        TL4ToTL() :=
//        TL4UserYanker() :=
//        TL4Fragmenter() :=
//        TL4IdIndexer(idBits=2) :=
//        TL.TLMaster.TLNode
//      }

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
    val busy = IO(Output(Bool()))

    val rdata = RegInit(0.U)
    val respWait = RegInit(false.B)

    req.ready := mem.a.ready && !respWait

    // Handle incoming IO req
    when (req.fire) {
      reading := true.B
    }

    mem.a.valid := !reading && req.valid
    mem.a.bits := edge.Get(
      fromSource = 1.U,
      toAddress = req.bits,
      lgSize = log2Ceil(beatBytes).U)._2

    when (edge.done(mem.a)) {
      reading := true.B
      respWait := true.B
    }

    mem.d.ready := resp.ready

    when (mem.d.fire) {
      resp.bits := mem.d.bits.data
      resp.valid := true.B
      respWait := false.B
      reading := false.B
    }

    busy := !reading && !respWait
  }
}

//abstract class MMIOSlave(
//                     csrAddress: AddressSet,
//                     scratchpadAddress: AddressSet,
//                     beatBytes: Int = 4
//                   )(implicit p: Parameters) extends RegisterRouter(
//  RegisterRouterParams(
//    name = "tlmmio",
//    compat = Seq("com", "tlmmio"),
//    base = csrAddress.base,
//    size = csrAddress.mask+1,
//    beatBytes = beatBytes))
//{
//
////  val TLMasterNode = LazyModule(new TLReadMaster(scratchpadAddress)(p))
//
//  lazy val module = new LazyModuleImp(this) { outer =>
//
////    val impl = TLMasterNode.module
////
////    val busy = Wire(Bool())
////    val req = Wire(DecoupledIO(UInt(64.W)))
////    val resp = Wire(DecoupledIO(UInt(64.W)))
//
//    val busy = IO(Input(Bool()))
//    val req = IO(Flipped(Decoupled(UInt(64.W))))
//    val resp = IO(Decoupled(UInt(64.W)))
//
//    regmap(
//      beatBytes * 0 -> Seq(RegField.r(64, busy)),
//      beatBytes * 1 -> Seq(RegField.w(64, req)),
//      beatBytes * 2 -> Seq(RegField.r(64, resp))
//    )
//  }
//}

class TLMMIOSlave(
   csrAddress: AddressSet,
   scratchpadAdderss: AddressSet,
   beatBytes: Int = 4
 )(implicit p: Parameters) extends MMIOSlave(csrAddress, scratchpadAdderss, beatBytes) with HasTLControlRegMap

class TLMMIONode(
 csrAddress: AddressSet,
 scratchpadAdderss: AddressSet,
 beatBytes: Int = 4
)(implicit p: Parameters) extends LazyModule {

  val node = TLIdentityNode()

  val TLMasterNode = LazyModule(new TLReadMaster(scratchpadAdderss, beatBytes=beatBytes)(p))
  val TLSlaveNode = LazyModule(new TLMMIOSlave(csrAddress, scratchpadAdderss, beatBytes)(p))

//  node := TLMasterNode.TLNode
  TLSlaveNode.node := node

  lazy val module = new LazyModuleImp(this) { outer =>
    TLMasterNode.module.req <> TLSlaveNode.module.req
    TLMasterNode.module.resp <> TLSlaveNode.module.resp
  }
}

class TLRAMXbarCSRMaster(val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
  val beatBytes: Int = 4,
  val blockBytes: Int = 4,
  val id: IdRange = IdRange(0, 1),
  val aligned: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val mem = TLIdentityNode()

  val tlSlave = LazyModule(new TLMMIOSlave(csrAddress, scratchpadAddress, beatBytes))
  val tlMaster = LazyModule(new TLReadMaster(scratchpadAddress))

  val ram = TLRAM(
    address = scratchpadAddress,
    executable = false,
    beatBytes = beatBytes,
    devName = Some("com,TLramxbar"),
  )
  val ramXbar = TLXbar()
  val topXbar = TLXbar()

  ram := topXbar

  tlSlave.node := topXbar
  topXbar := TLFragmenter(beatBytes, blockBytes, holdFirstDeny = true) := mem
//  topXbar := tlMaster.tlNode

  lazy val module = new LazyModuleImp(this) {
    tlMaster.module.req <> tlSlave.module.req
    tlMaster.module.resp <> tlSlave.module.resp
    tlSlave.module.busy := tlMaster.module.busy
//    TLMaster.module.req <> DontCare
//    TLMaster.module.resp <> DontCare
  }
}
