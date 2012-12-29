

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.svf.all;

entity tb_svf is
end tb_svf;

architecture behavior of tb_svf is

  constant SVFPath : string := "../svf-files/";
  
  signal TMS : std_logic;
  signal TDI : std_logic;
  signal TCK : std_logic;
  signal TDO : std_logic;
begin  -- behavior

  test1: process
  begin  -- process test1
    TDO <= '0';

    wait for 1 us;

    -- process SVF files
    ProcessSVF(SVFPath & "test-commands.svf",           TMS,TDI,TCK,TDO);
    ProcessSVF(SVFPath & "test-nonl.svf",               TMS,TDI,TCK,TDO);
    ProcessSVF(SVFPath & "test-runtest.svf",            TMS,TDI,TCK,TDO);
    ProcessSVF(SVFPath & "test-xc9536xl.svf",           TMS,TDI,TCK,TDO);
    ProcessSVF(SVFPath & "test-a2f200m3f-ERASE.svf",    TMS,TDI,TCK,TDO);
    ProcessSVF(SVFPath & "test-a2f200m3f-PROGRAM.svf",  TMS,TDI,TCK,TDO);

    wait for 1 us;

    ---------------------------------------------------------------------------
    -- Simulation is finished
    ---------------------------------------------------------------------------
    assert false
      report "### simulation is finished ###"
      severity failure ;

  end process test1;

end behavior;
