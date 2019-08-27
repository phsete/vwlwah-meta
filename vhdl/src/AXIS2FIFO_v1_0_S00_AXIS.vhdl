library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AXIS2FIFO_v1_0_S00_AXIS is
    generic (
                -- AXI4Stream sink: Data Width
                C_S_AXIS_TDATA_WIDTH	: integer	:= 32
            );
    port (
             inputData: out std_logic_vector (C_S_AXIS_TDATA_WIDTH-1 downto 0);
             inputWren: out std_logic;
             inputFull: in std_logic;
             inputAlmostFull: in std_logic;
             inputFinal: out std_logic;

             -- AXI4Stream sink: Clock
             S_AXIS_ACLK	: in std_logic;
             -- AXI4Stream sink: Reset
             S_AXIS_ARESETN	: in std_logic;
             -- Ready to accept data in
             S_AXIS_TREADY	: out std_logic;
             -- Data in
             S_AXIS_TDATA	: in std_logic_vector(C_S_AXIS_TDATA_WIDTH-1 downto 0);
             -- Byte qualifier
             --S_AXIS_TSTRB	: in std_logic_vector((C_S_AXIS_TDATA_WIDTH/8)-1 downto 0);
             -- Indicates boundary of last packet
             S_AXIS_TLAST	: in std_logic;
             -- Data is in valid
             S_AXIS_TVALID	: in std_logic
         );
end AXIS2FIFO_v1_0_S00_AXIS;

architecture arch_imp of AXIS2FIFO_v1_0_S00_AXIS is

begin
    S_AXIS_TREADY <= '1' when (inputAlmostFull = '0' and inputFull = '0') else '0';

    process(S_AXIS_ACLK)
    begin
        if (rising_edge (S_AXIS_ACLK)) then
            if(S_AXIS_ARESETN = '0') then
                inputWren <= '0';
                inputFinal <= '0';
            else
                if (inputFull = '0') then
                    if (S_AXIS_TVALID = '1' and inputAlmostFull = '0' and inputFull = '0') then
                        inputWren <= '1';
                        inputData <= S_AXIS_TDATA;
                    else
                        inputWren <= '0';
                    end if;
                    if (inputAlmostFull = '1' or inputFull = '1') then
                    end if;
                    if (S_AXIS_TLAST = '1') then
                        inputFinal <= '1';
                    end if;
                else
                    inputWren <= '0';
                end if;
            end if;
        end if;
    end process;

end arch_imp;
