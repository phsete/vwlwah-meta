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
-- Define the states of state machine
-- The control state machine oversees the writing of input streaming data to the FIFO,
-- and outputs the streaming data from the FIFO
type state is (IDLE, WRITE_FIFO);

signal axis_tready	: std_logic;
-- State variable
signal  mst_exec_state : state;  

-- FIFO implementation signals
-- FIFO full flag
signal fifo_full_flag : std_logic;
-- sink has accepted all the streaming data and stored in FIFO
signal writes_done : std_logic; -- TODO: what does this do?

begin
    -- Control state machine implementation
    process(S_AXIS_ACLK)
    begin
        if (rising_edge (S_AXIS_ACLK)) then
            if(S_AXIS_ARESETN = '0') then
                -- Synchronous reset (active low)
                mst_exec_state <= IDLE;
            else
                case (mst_exec_state) is
                    when IDLE => 
                        -- The sink starts accepting tdata when 
                        -- there tvalid is asserted to mark the
                        -- presence of valid streaming data 
                        if (S_AXIS_TVALID = '1')then
                            mst_exec_state <= WRITE_FIFO;
                        else
                            mst_exec_state <= IDLE;
                        end if;

                    when WRITE_FIFO => 
                        -- When the sink has accepted all the streaming input data,
                        -- the interface swiches functionality to a streaming master
                        if (writes_done = '1') then
                            mst_exec_state <= IDLE;
                        else
                            -- The sink accepts and stores tdata 
                            -- into FIFO
                            mst_exec_state <= WRITE_FIFO;
                        end if;
                end case;
            end if;  
        end if;
    end process;

    -- AXI Streaming Sink 
    -- 
    -- The connected FIFO is always ready to accept the S_AXIS_TDATA  until
    -- the FIFO is not filled (inputFull input)
    axis_tready <= '1' when (inputAlmostFull = '0' and inputFull = '0') else '0';
    S_AXIS_TREADY <= axis_tready;

    process(S_AXIS_ACLK)
    begin
        if (rising_edge (S_AXIS_ACLK)) then
            if(S_AXIS_ARESETN = '0') then
                writes_done <= '0';
                inputWren <= '0';
                inputFinal <= '0';
            else
                if (inputFull = '0') then
                    if (S_AXIS_TVALID = '1' and inputAlmostFull = '0' and inputFull = '0') then
                        inputWren <= '1';
                        writes_done <= '0';
                        inputData <= S_AXIS_TDATA;
                    else
                        inputWren <= '0';
                    end if;
                    if (inputAlmostFull = '1' or inputFull = '1') then
                        writes_done <= '1';
                    end if;
                    if (S_AXIS_TLAST = '1') then
                        inputFinal <= '1';
                        writes_done <= '1';
                    end if;
                else
                    inputWren <= '0';
                end if;
            end if;
        end if;
    end process;

end arch_imp;
