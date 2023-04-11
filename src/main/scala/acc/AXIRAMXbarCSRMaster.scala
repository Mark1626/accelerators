package accelerators.acc

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, Queue, log2Ceil}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._

case class AXIRAMXbarCSRMasterConfig(
  val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
 )

case object AXIRAMXbarCSRMasterKey extends Field[Option[AXIRAMXbarCSRMasterConfig]](None)

trait CanHaveAXIRAMXbarCSRMaster { this: BaseSubsystem =>
  private val portName = "axiplus"

  val axiNode = p(AXIRAMXbarCSRMasterKey) match {
    case Some(params) => {
      val axi = LazyModule(new AXIRAMXbarCSRMaster(params.csrAddress, params.scratchpadAddress, pbus.beatBytes)(p))
      pbus.coupleTo(portName){
        axi.mem :=
          AXI4Buffer() :=
          TLToAXI4() :=
          TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true) :=
          _
      }

//      pbus.coupleFrom("aximaster") {
//        _ :=
//        AXI4ToTL() :=
//        AXI4UserYanker() :=
//        AXI4Fragmenter() :=
//        AXI4IdIndexer(idBits=2) :=
//        axi.axiMaster.axiNode
//      }

      Some(axi)
    }
    case None => None
  }
}

class WithAXIRAMXbarCSRMaster(csrAddress: AddressSet, scratchpadAddress: AddressSet) extends Config((site, here, up) => {
  case AXIRAMXbarCSRMasterKey => Some(AXIRAMXbarCSRMasterConfig(csrAddress=csrAddress, scratchpadAddress=scratchpadAddress))
})

class AXIReadMaster(
                     val address: AddressSet,
                     id: IdRange = IdRange(0, 1),
                     aligned: Boolean = false,
                   )(implicit p: Parameters) extends LazyModule {
  val axiNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = name,
      id = id,
      aligned = aligned,
      maxFlight = Some(2)
    ))
  )))

  lazy val module = new LazyModuleImp(this) {
    val (axi, axiP) = axiNode.out.head
    val addr = address

    val addrWidth: Int = axiP.bundle.addrBits
    val lenWidth: Int = axiP.bundle.lenBits
    val beatLength: Int = 1 << lenWidth
    val dataWidth: Int = axiP.bundle.dataBits
    val beatBytes = dataWidth >> 3

    val reading = RegInit(false.B)
    val writing = RegInit(false.B)

    // Address of ram to read
    val req = IO(Flipped(Decoupled(UInt(64.W))))
    val resp = IO(Decoupled(UInt(64.W)))
    val busy = IO(Output(Bool()))

    val rdata = RegInit(0.U)
    val respWait = RegInit(false.B)

    req.ready := axi.ar.ready && !respWait

    // Handle incoming IO req
    when (req.fire) {
      reading := true.B
    }

    // Make AXI Request to RAM
    axi.ar.valid := !reading && req.valid
    axi.ar.bits.addr := req.bits // TODO: Check if we have to restrict to address space of RAM
    axi.ar.bits.burst := false.B
    axi.ar.bits.cache := AXI4Parameters.CACHE_MODIFIABLE | AXI4Parameters.CACHE_BUFFERABLE
    axi.ar.bits.id := id.start.U
    axi.ar.bits.len := dataWidth.U
    axi.ar.bits.lock := 0.U // normal access
    axi.ar.bits.prot := 0.U
    axi.ar.bits.size := log2Ceil((dataWidth + 7) / 8).U

    when (axi.r.fire) {
      rdata := axi.r.bits.data
      reading := false.B
      respWait := true.B
      // TODO: Do we want to check axi r last?
      // TODO: Add handling for error scenario
      //      readError := axi.r.bits.resp =/= AXI4Parameters.RESP_OKAY
    }

    // Handle response
    when (resp.ready) {
      resp.valid := respWait
      resp.bits := rdata
      respWait := false.B
    }

    // AXI Master Write
    when (axi.aw.ready) {
      writing := true.B
    }

    // This interface does not do write
    axi.aw <> DontCare
    axi.w <> DontCare
    axi.b <> DontCare

    axi.aw.valid := false.B
    axi.w.valid := false.B
    axi.b.ready := false.B

    busy := !reading && !respWait
  }
}

abstract class MMIOSlave(
                     csrAddress: AddressSet,
                     scratchpadAddress: AddressSet,
                     beatBytes: Int = 4
                   )(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "aximmio",
    compat = Seq("com", "aximmio"),
    base = csrAddress.base,
    size = csrAddress.mask+1,
    beatBytes = beatBytes))
{

//  val axiMasterNode = LazyModule(new AXIReadMaster(scratchpadAddress)(p))

  lazy val module = new LazyModuleImp(this) { outer =>

//    val impl = axiMasterNode.module
//
//    val busy = Wire(Bool())
//    val req = Wire(DecoupledIO(UInt(64.W)))
//    val resp = Wire(DecoupledIO(UInt(64.W)))

    val busy = IO(Input(Bool()))
    val req = IO(Flipped(Decoupled(UInt(64.W))))
    val resp = IO(Decoupled(UInt(64.W)))

    regmap(
      beatBytes * 0 -> Seq(RegField.r(64, busy)),
      beatBytes * 1 -> Seq(RegField.w(64, req)),
      beatBytes * 2 -> Seq(RegField.r(64, resp))
    )
  }
}

class AXIMMIOSlave(
   csrAddress: AddressSet,
   scratchpadAdderss: AddressSet,
   beatBytes: Int = 4
 )(implicit p: Parameters) extends MMIOSlave(csrAddress, scratchpadAdderss, beatBytes) with HasAXI4ControlRegMap

class AXIMMIONode(
 csrAddress: AddressSet,
 scratchpadAdderss: AddressSet,
 beatBytes: Int = 4
)(implicit p: Parameters) extends LazyModule {

  val node = AXI4IdentityNode()

  val axiMasterNode = LazyModule(new AXIReadMaster(scratchpadAdderss)(p))
  val axiSlaveNode = LazyModule(new AXIMMIOSlave(csrAddress, scratchpadAdderss, beatBytes)(p))

//  node := axiMasterNode.axiNode
  axiSlaveNode.node := node

  lazy val module = new LazyModuleImp(this) { outer =>
    axiMasterNode.module.req <> axiSlaveNode.module.req
    axiMasterNode.module.resp <> axiSlaveNode.module.resp
  }
}

class AXIRAMXbarCSRMaster(val csrAddress: AddressSet,
  val scratchpadAddress: AddressSet,
  val beatBytes: Int = 4,
  val id: IdRange = IdRange(0, 1),
  val aligned: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val mem = AXI4IdentityNode()

  val axiSlave = LazyModule(new AXIMMIOSlave(csrAddress, scratchpadAddress, beatBytes))
  val axiMaster = LazyModule(new AXIReadMaster(scratchpadAddress))

  val ram = AXI4RAM(
    address = scratchpadAddress,
    executable = false,
    beatBytes = beatBytes,
    devName = Some("com,axiramxbar"),
    errors = Nil,
  )
  val ramXbar = AXI4Xbar()
  val topXbar = AXI4Xbar()

  ram :=* topXbar

  axiSlave.node := topXbar
  topXbar := mem

  lazy val module = new LazyModuleImp(this) {
    axiMaster.module.req <> axiSlave.module.req
    axiMaster.module.resp <> axiSlave.module.resp
    axiSlave.module.busy := axiMaster.module.busy
//    axiMaster.module.req <> DontCare
//    axiMaster.module.resp <> DontCare

    mem.out.foreach { o => o._2.slave.slaves.foreach(s => println(s"${s.name} is ${s.interleavedId}")) }
  }
}
