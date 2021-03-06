vhdl-svf -- SVF (Serial Vector Format) interpreter to control a JTAG TAP
========================================================================

This repository provides an SVF interpreter fully written in VHDL which
parses an SVF file and performs the specified JTAG bus operations. Usage
scenarios are testing chip designs with integrated JTAG TAPs.


License
-------

    Copyright (C) 2012 Johann Glaser <Johann.Glaser@gmx.at>

    This VHDL package is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301



Directory Structure
-------------------

  ``doc/``
    SVF specification

  ``vhdl/``
    VHDL package and testbench

  ``sim/``
    scripts to compile and simulate using ModelSim/QuestaSim

  ``svf-files/``
    SVF files used by the testbench


Further Documentation
---------------------

See the header of `vhdl/svf-p.vhd <vhdl-svf/blob/master/vhdl/svf-p.vhd>`_ for all further documentation.
