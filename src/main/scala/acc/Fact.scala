package accelerators.acc

import chipsalliance.rocketchip.config.Config
import chisel3._
import chisel3.util._
import chisel3.experimental.BaseModule
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes, PBUS, TLBusWrapperLocation}
import freechips.rocketchip.tilelink.{HasTLControlRegMap, TLBuffer, TLFragmenter, TLRegBundle, TLRegModule, TLRegisterRouter, TLWidthWidget}

case class FactParams(
  address: BigInt,
  width: Int,
  slaveWhere: TLBusWrapperLocation = PBUS
)

case object FactKey extends Field[Option[FactParams]](None)

class FactIO(val w: Int) extends Bundle {
  val n     = Flipped(Decoupled(UInt(w.W)))
  val fact  = Decoupled(UInt(w.W))
  val busy  = Output(Bool())
}

trait FactTopIO extends Bundle {
  val fact_busy = Bool()
}

trait HasFactIO extends BaseModule {
  val w: Int
  val io = IO(new FactIO(w))
}

class FactModuleChiselModule(val w: Int) extends Module with HasFactIO {
  val idle :: run :: done :: Nil = Enum(3)
  val state: UInt = RegInit(idle)
  private val res   = RegInit(1.U(w.W))
  val tmp: UInt = RegInit(1.U(w.W))

  io.n.ready    := state === idle
  io.fact.valid := state === done
  io.fact.bits  := res

  // State Transition
  when (state === idle && io.n.valid) {
    state       := run
  } .elsewhen(state === run && tmp === 1.U) {
    state       := done
  } .elsewhen (state === done && io.fact.ready) {
    state       := idle
  }

  // State behaviour
  when (state === idle && io.n.valid) {
    tmp       := io.n.bits
  } .elsewhen (state === run && tmp > 1.U) {
    res       := res * tmp
    tmp       := tmp - 1.U
  }

  io.busy     := state =/= idle
}

trait FactModule extends HasRegMap {
  val io: FactTopIO
  implicit val p: Parameters

  def params: FactParams

  val status  = Wire(UInt(2.W))
  val n       = Wire(new DecoupledIO(UInt(params.width.W)))
  val fact    = Wire(new DecoupledIO(UInt(params.width.W)))

  val impl    = Module(new FactModuleChiselModule(params.width))

  impl.io.n.bits := n.bits
  impl.io.n.valid := n.valid
  n.ready := impl.io.n.ready

  fact.bits := impl.io.fact.bits
  fact.valid := impl.io.fact.valid
  impl.io.fact.ready := fact.ready

  status := Cat(impl.io.n.ready, impl.io.fact.valid)
  io.fact_busy     := impl.io.busy

  regmap(
    0x00 -> Seq(RegField.r(2, status)),
    0x04 -> Seq(RegField.w(params.width, n)),
    0x08 -> Seq(RegField.r(params.width, fact))
  )

}

class FactModuleTL(params: FactParams, beatBytes: Int)(implicit p: Parameters) extends TLRegisterRouter(
  params.address, "fact", Seq("mark,fact"),
  beatBytes = beatBytes)(
  new TLRegBundle(params, _) with FactTopIO)(
  new TLRegModule(params, _, _) with FactModule)


trait CanHavePeripheryFact { this: BaseSubsystem =>
  private val portName = "fact"

  val fact = p(FactKey) match {
    case Some(params) => {
      val tlbus = locateTLBusWrapper(params.slaveWhere)
      val m = LazyModule(new FactModuleTL(params, tlbus.beatBytes)(p))

      tlbus.toVariableWidthSlave(Some(portName)) { m.node }

      Some(m)
    }
    case None => None
  }
}

trait CanHavePeripheryFactImp extends LazyModuleImp {
  val outer: CanHavePeripheryFact

  val m_busy = outer.fact match {
    case Some(m) => {
      val busy = IO(Output(Bool()))
      busy := m.module.io.fact_busy
      Some(busy)
    }
    case None => None
  }
}


class WithFact(address: BigInt = 0x1000, width: Int = 32) extends Config((site, here, up) => {
  case FactKey => Some(FactParams(address, width))
})
