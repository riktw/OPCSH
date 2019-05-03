package mylib
import spinal.core._
import spinal.lib.misc.HexTools
import spinal.lib._
import spinal.lib.io.TriStateArray
import spinal.lib.com.uart._

class OPCTop(onChipRamHexFile : String) extends Component {
	val io = new Bundle {
		val dout_ram = out  Bits (8 bits)
		val addr_ram = out  Bits (10 bits)
		val we_ram = out Bool
		val uart = master(Uart())
		val gpio = master(TriStateArray(8))
		val pruart = master(Uart())
	}

	val bootloaderReset = Bool
	bootloaderReset := clockDomain.readResetWire

	val areaCPU = new ResetArea(bootloaderReset, false){		//New reset area so uart bootloader can reset the CPU
		val CPU = new OPC
		val CPUperipheral = new OPCperiph
	}

	val uartCtrlConfig = UartCtrlGenerics(
		dataWidthMax = 8,
		clockDividerWidth = 16,
		preSamplingSize = 1,
		samplingSize = 5,
		postSamplingSize = 2)
  val uartCtrl = new UartCtrl(uartCtrlConfig)
  uartCtrl.io.config.clockDivider := 156 //13 //115200BAUD
  uartCtrl.io.config.frame.dataLength := 7  //8 bits
  uartCtrl.io.config.frame.parity := UartParityType.NONE
  uartCtrl.io.config.frame.stop := UartStopType.ONE

	uartCtrl.io.write.valid := False
  uartCtrl.io.write.payload := 0
  uartCtrl.io.uart <> io.pruart

	areaCPU.CPUperipheral.io.dout_ram := areaCPU.CPU.io.dout_ram
	areaCPU.CPUperipheral.io.addr_ram := areaCPU.CPU.io.addr_ram
	areaCPU.CPUperipheral.io.we_ram := areaCPU.CPU.io.we_ram
	io.uart <> areaCPU.CPUperipheral.io.uart
	io.gpio <> areaCPU.CPUperipheral.io.GPIO
	areaCPU.CPU.io.IRQ := areaCPU.CPUperipheral.io.IRQ

	val RAM = Mem(Bits(8 bits),wordCount = 1024)
	val ROM = Mem(Bits(16 bits),wordCount = 1024)

	//on boot, if any data is send to UART, pull CPU in reset
	//collect a full program and deassert reset for CPU
	val progCounter = Reg(UInt(13 bits)) init(U"b1111111111111")		//init all 1's so when the first data word is
	// received it's set to address 0.
	val progData = Reg(UInt(16 bits)) init(0)
	val progDataValid, progdataByte = Reg(Bool) init(False)

	//get bytes, when 2 collected, store and repeat
	when(uartCtrl.io.read.valid && progData =/= 1024){
		when(progdataByte === False){
			progdataByte := True
			progData := (uartCtrl.io.read.payload.asUInt << 8)
			progDataValid := False
		}.otherwise{
			progdataByte := False
			progData := progData + uartCtrl.io.read.payload.asUInt
			progDataValid := True
			progCounter := progCounter + 1
		}
	}.elsewhen(progCounter === 1024){
		progCounter := U"b1111111111111"
		progDataValid := False
	}.elsewhen(progCounter =/= U"b1111111111111"){
		bootloaderReset := True
	}
	//default program
	HexTools.initRam(ROM, onChipRamHexFile, 0x0)

	RAM.write(
		enable  = areaCPU.CPU.io.we_ram,
		address = areaCPU.CPU.io.addr_ram.asUInt,
		data    = areaCPU.CPU.io.dout_ram
	)

	ROM.write(
		enable  = progDataValid,
		address = progCounter.resized,
		data    = progData.asBits
	)

	val readValid = !areaCPU.CPU.io.we_ram
	when(areaCPU.CPU.io.addr_ram.asUInt >= "d896") {
		areaCPU.CPU.io.din_ram := areaCPU.CPUperipheral.io.din_ram
	}.otherwise {
		areaCPU.CPU.io.din_ram := RAM.readSync(
			enable = readValid,
			address = areaCPU.CPU.io.addr_ram.asUInt
		)
	}

	areaCPU.CPU.io.din_instr := ROM.readSync(
		address = areaCPU.CPU.io.addr_instr.asUInt.resized
	)

	io.dout_ram := areaCPU.CPU.io.dout_ram
	io.addr_ram := areaCPU.CPU.io.addr_ram
	io.we_ram := areaCPU.CPU.io.we_ram
}


//Generate the MyTopLevel's VHDL
object OPCVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new OPCTop("hex/Test_periph.hex"))
  }
}

//Generate the MyTopLevel's Verilog
object OPCVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new OPCTop("hex/Test_periph.hex"))
  }
}


