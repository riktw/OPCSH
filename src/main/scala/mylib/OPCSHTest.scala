package mylib

import spinal.core._
import spinal.core.sim._

//MyTopLevel's testbench
object MyTopLevelSim {

  def main(args: Array[String]) {

    SimConfig.withWave.doSim(new OPCTop(args(0))){ dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(1)
      dut.io.pruart.rxd #= true;

      //sim boots using provided ROM until a value is written to addr 512,
      // checks if the value is as expected and lets user know.
      //runMain mylib.MyTopLevelSim "hex/Test_andi.hex" 10 runs test_andi.hex and tests if the result is 10
      var done = false
      var maxSimulationCycles = 0;
      while(!done){
        dut.clockDomain.waitSampling(1)
        maxSimulationCycles += 1
        if(dut.io.we_ram.toBoolean && dut.io.addr_ram.toInt == 512){
          done = true   //any write to addr 512 aborts simulation
        }
        if(maxSimulationCycles == 150000){  //after X cycles, also abort
          assert(dut.io.dout_ram.toInt == 0, message = "Infinite loop detected")
          done = true
        }
      }

      //Check if written data to 512 is correct
      assert(dut.io.dout_ram.toInt == args(1).toInt, message = "Output incorrect")

      dut.clockDomain.waitSampling(1000)
    }
  }
}


