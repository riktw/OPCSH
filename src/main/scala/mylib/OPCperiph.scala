package mylib
import spinal.core._
import spinal.lib._
import spinal.lib.io.TriStateArray
import spinal.lib.com.uart._

case class Timer(width : Int) extends Component{
  val io = new Bundle{
    val tick      = in Bool
    val clear     = in Bool
    val limit     = in UInt(width bits)
    val divider   = in UInt(width bits)

    val full  		= out Bool
    val value     = out UInt(width bits)
  }

  val counter = Reg(UInt(width bits)) init(0)
  val divider = Reg(UInt(width bits)) init(0)
  when(io.tick && !io.full){
    divider := divider + 1
    when(divider >= io.divider){
      counter := counter + 1
      divider := 0
    }
  }
  when(io.clear){
    counter := 0
    divider := 0
  }

  io.full := counter === io.limit && io.tick
  io.value := counter
}

class OPCperiph extends Component {
  val io = new Bundle {
    val din_ram = out Bits (8 bits)
    val dout_ram = in Bits (8 bits)
    val addr_ram = in Bits (10 bits)
    val we_ram = in Bool
    val IRQ = out Bool

    val GPIO = master(TriStateArray(8 bits))
    val uart = master(Uart())
  }

  val timer1 = new Timer(16)
  val timer_limit, timer_divider = Reg(UInt(16 bits)) init(0)
	val timer_settings = Reg(UInt(8 bits)) init(0)
  timer1.io.limit := timer_limit
  timer1.io.divider := timer_divider
  timer1.io.tick := timer_settings(0)
  timer1.io.clear := timer_settings(1)
  io.IRQ := timer1.io.full

  val gpio_writeEnable, gpio_write = Reg(Bits(8 bits)) init("00000000")
  io.GPIO.write := gpio_write
  io.GPIO.writeEnable := gpio_writeEnable

  val uart_readValid, uart_read, uart_writeValid, uart_write, uart_writeDone = Reg(Bits(8 bits)) init("00000000")
  val uartCtrlConfig = UartCtrlGenerics(
    dataWidthMax = 8,
    clockDividerWidth = 16,
    preSamplingSize = 1,
    samplingSize = 5,
    postSamplingSize = 2)
  val uartCtrl = new UartCtrl(uartCtrlConfig)
  uartCtrl.io.config.clockDivider := 156  //12Mhz clock, 9600BAUD
  uartCtrl.io.config.frame.dataLength := 7  //8 bits
  uartCtrl.io.config.frame.parity := UartParityType.NONE
  uartCtrl.io.config.frame.stop := UartStopType.ONE

  uartCtrl.io.write.valid := uart_writeValid.asBool
  uartCtrl.io.write.payload := uart_write

  uartCtrl.io.uart <> io.uart

  uart_readValid := uartCtrl.io.read.valid.asBits.resized
  uart_read := uartCtrl.io.read.payload

  io.din_ram := 0

  when(uartCtrl.io.write.ready){
    uart_writeDone := 1
  }

  when(io.we_ram){
    when(io.addr_ram === "d897"){
      gpio_writeEnable := io.dout_ram
    }.elsewhen(io.addr_ram === "d898") {
      gpio_write := io.dout_ram
    
		}.elsewhen(io.addr_ram === "d907"){
      uart_writeDone := io.dout_ram
    }.elsewhen(io.addr_ram === "d908"){
      uart_write := io.dout_ram
    }.elsewhen(io.addr_ram === "d909"){
      uart_writeValid := io.dout_ram
    
		}.elsewhen(io.addr_ram === "d915"){
      timer_settings := io.dout_ram.asUInt
    }.elsewhen(io.addr_ram === "d916"){
      timer_divider(7 downto 0) := io.dout_ram.asUInt
		}.elsewhen(io.addr_ram === "d917"){
      timer_divider(15 downto 8) := io.dout_ram.asUInt
    }.elsewhen(io.addr_ram === "d918"){
      timer_limit(7 downto 0) := io.dout_ram.asUInt
    }.elsewhen(io.addr_ram === "d919"){
      timer_limit(15 downto 8) := io.dout_ram.asUInt
    }


  }.otherwise{
    when(io.addr_ram === "d898") {
      io.din_ram := gpio_write
    }.elsewhen(io.addr_ram === "d899"){
      io.din_ram := io.GPIO.read
			
    }.elsewhen(io.addr_ram === "d905") {
      io.din_ram := uart_readValid
    }.elsewhen(io.addr_ram === "d906"){
      io.din_ram := uart_read
    }.elsewhen(io.addr_ram === "d907"){
      io.din_ram := uart_writeDone
    
		}.elsewhen(io.addr_ram === "d915"){
      io.din_ram := timer_settings.asBits
      timer_settings := 0
    }
  }
}