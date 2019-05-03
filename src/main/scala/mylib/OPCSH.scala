package mylib
import spinal.core._
class OPC extends Component {
  val io = new Bundle {
    val din_instr = in  Bits (16 bits); val addr_instr = out Bits(13 bits)
    val din_ram = in  Bits (8 bits); val dout_ram = out  Bits (8 bits)
    val addr_ram = out  Bits (10 bits); val we_ram = out Bool; val IRQ = in Bool
  }
  val r_addr = Reg(Bits(10 bits)) init("0000000000")
  val r_pc, r_pcIRQ = Reg(UInt(13 bits)) init(0)
  val r_acc, r_accIRQ = Reg(UInt(8 bits)) init(0)
  val state = Reg(UInt(2 bits)) init(2)
  val instr = Reg(UInt(16 bits)) init(0)
  io.addr_instr := r_pc.asBits
  io.dout_ram := "x00"
  io.addr_ram := io.din_instr(9 downto 0).asBits
  io.we_ram := False
  when(state === 0){		//fetch new instruction stage
    instr := io.din_instr.asUInt
    state := 1
  }.elsewhen(state === 1){		//Stage for ALU part for instruction, and handle PC (increase or do a jump)
    r_pc := r_pc + 1
    val input = Mux(instr(13) && !(!instr(15) && instr(14)), io.din_ram.asUInt, instr(7 downto 0))
    when(instr(15) === False) {
      val tr_acc = instr(12 downto 10).mux(		//ACC with IMM/RAM instructions
        0 -> (r_acc + input),
        1 -> (r_acc - input),
        2 -> (r_acc & input),
        3 -> (r_acc | input),
        4 -> (r_acc ^ input),
        5 -> (r_acc |>> input).resize(8),
        6 -> (r_acc |<< input).resize(8),
        7 -> Mux((r_acc < input), U(1, 8 bits), U(0, 8 bits)))
      when((instr(15) === False) && (instr(14) === True) && (tr_acc === 0)){
        r_pc := r_pc - ((((instr(13).asUInt << 2) + instr(9 downto 8))+1)<<1).resized
      }
      r_acc := tr_acc
    }.elsewhen(instr(15) === True && instr(14) === False && instr(13) === False){
      r_acc := instr(12 downto 10).mux(		//needed some more, ACC with IMM for signed cmp and shift arith
        0 -> (r_pc(12 downto 8) >> 8).resized,
        1 -> (r_pc(7 downto 0) + input),
        5 -> (r_acc >> input).resize(8),
        7 -> Mux(r_acc.asSInt < input.asSInt, U(1, 8 bits), U(0, 8 bits)),
        default -> r_acc)
    }
    when(instr(15) === True && instr(14) === True && instr(13) === False && r_acc === 0){
      r_pc := instr(12 downto 0)
    }.elsewhen(instr(15) === True && instr(14) === True && instr(13) === True){
      r_pc := ((r_acc << 8) + io.din_ram.asUInt).resized
    }.elsewhen(io.IRQ === True && r_pc =/= "0000100000000"){		//IRQ, store PC and acc
      r_pcIRQ := r_pc
      r_accIRQ := r_acc
      r_pc := "0000100000000"
    }.elsewhen(instr === "x8800"){
      r_pc := r_pcIRQ
      r_acc := r_accIRQ
    }
    state := 2
  }.elsewhen(state === 2){		//RAM writeback stage
    when(instr(15) === True && instr(14) === False && instr(13) === True){
      io.we_ram := True
      io.dout_ram := r_acc.asBits
    }
    state := 0
  }
}