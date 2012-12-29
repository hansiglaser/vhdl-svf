#!/bin/bash
#
# Create a Makefile.
#
# This tool uses ModelSim's "vmake" to generate a Makefile. Therefore all
# VHDL files are first compiled by hand to tell ModelSim their order. Then
# the Makefile is generated.
#

APPVHDL_PATH=../vhdl

VCOM_OPTS=""
#VCOM_OPTS="+acc -fsmverbose"

if ! which vlib > /dev/null ; then
  echo "ERROR: Couldn't find ModelSim executables."
  exit 1
fi

if [ -f Makefile ] ; then
  echo "ERROR: Makefile already exists. Please remove it before calling $0"
  exit 1
fi

# create library work
if [ ! -d work ] ; then
  echo "Creating library 'work'"
  vlib work
  vmap work work
fi

# compile all files
vcom ${VCOM_OPTS} -work work -2002  $APPVHDL_PATH/svf-p.vhd
vcom ${VCOM_OPTS} -work work -2002  $APPVHDL_PATH/tb_svf.vhd

# create Makefile
echo "Writing Makefile"
vmake -nolinewrap > Makefile
