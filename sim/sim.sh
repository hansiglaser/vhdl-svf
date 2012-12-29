#!/bin/bash
#
# Start the simulation
#
# Be sure to create a Makefile first and compile all VHDL files.
#

vsim "work.tb_svf(behavior)" -do "do wave.do ; run -all"
