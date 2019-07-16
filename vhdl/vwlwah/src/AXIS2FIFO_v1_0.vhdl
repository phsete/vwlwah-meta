library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity AXIS2FIFO_v1_0 is
    generic (
        -- Parameters of Axi Slave Bus Interface S00_AXIS
                C_S00_AXIS_TDATA_WIDTH	: integer	:= 32
);
port (
         InputData: out std_logic_vector (C_S00_AXIS_TDATA_WIDTH-1 downto 0);
         InputFull: in std_logic;
         InputAlmostFull: in std_logic;
         InputWren: out std_logic;
         InputFinal: out std_logic;

        -- Ports of Axi Slave Bus Interface S00_AXIS
         s00_axis_aclk	: in std_logic;
         s00_axis_aresetn	: in std_logic;
         s00_axis_tready	: out std_logic;
         s00_axis_tdata	: in std_logic_vector(C_S00_AXIS_TDATA_WIDTH-1 downto 0);
        --s00_axis_tstrb	: in std_logic_vector((C_S00_AXIS_TDATA_WIDTH/8)-1 downto 0);
         s00_axis_tlast	: in std_logic;
         s00_axis_tvalid	: in std_logic
    );
end AXIS2FIFO_v1_0;

architecture arch_imp of AXIS2FIFO_v1_0 is

    -- component declaration
    component AXIS2FIFO_v1_0_S00_AXIS is
        generic (
                    C_S_AXIS_TDATA_WIDTH	: integer	:= 32
                );
        port (
                 inputData: out std_logic_vector (C_S_AXIS_TDATA_WIDTH-1 downto 0);
                 inputFull: in std_logic;
                 inputWren: out std_logic;
                 inputAlmostFull: in std_logic;
                 inputFinal: out std_logic;
                 S_AXIS_ACLK	: in std_logic;
                 S_AXIS_ARESETN	: in std_logic;
                 S_AXIS_TREADY	: out std_logic;
                 S_AXIS_TDATA	: in std_logic_vector(C_S_AXIS_TDATA_WIDTH-1 downto 0);
        --S_AXIS_TSTRB	: in std_logic_vector((C_S_AXIS_TDATA_WIDTH/8)-1 downto 0);
                 S_AXIS_TLAST	: in std_logic;
                 S_AXIS_TVALID	: in std_logic
             );
    end component AXIS2FIFO_v1_0_S00_AXIS;

begin

-- Instantiation of Axi Bus Interface S00_AXIS
    AXIS2FIFO_v1_0_S00_AXIS_inst : AXIS2FIFO_v1_0_S00_AXIS
    generic map (
                    C_S_AXIS_TDATA_WIDTH	=> C_S00_AXIS_TDATA_WIDTH
                )
    port map (
                 inputData => InputData,
                 inputFull => InputFull,
                 inputAlmostFull => InputAlmostFull,
                 inputWren => InputWren,
                 inputFinal => InputFinal,
                 S_AXIS_ACLK	=> s00_axis_aclk,
                 S_AXIS_ARESETN	=> s00_axis_aresetn,
                 S_AXIS_TREADY	=> s00_axis_tready,
                 S_AXIS_TDATA	=> s00_axis_tdata,
        --S_AXIS_TSTRB	=> s00_axis_tstrb,
                 S_AXIS_TLAST	=> s00_axis_tlast,
                 S_AXIS_TVALID	=> s00_axis_tvalid
             );

end arch_imp;
