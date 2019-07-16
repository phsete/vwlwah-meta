library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity FIFO2AXIS_v1_0_M00_AXIS is
    generic (
                -- Width of S_AXIS address bus
                C_M_AXIS_TDATA_WIDTH	: integer	:= 32;
                -- Start count is the number of clock cycles the master will wait before transaction
                C_M_START_COUNT	: integer	:= 32
            );
    port (
             outputData: in std_logic_vector (C_M_AXIS_TDATA_WIDTH-1 downto 0);
             outputEmpty: in std_logic;
             outputRden: out std_logic;
             outputFinal: in std_logic;

             -- Global ports
             M_AXIS_ACLK	: in std_logic;
             M_AXIS_ARESETN	: in std_logic;
             M_AXIS_TVALID	: out std_logic;
             M_AXIS_TDATA	: out std_logic_vector(C_M_AXIS_TDATA_WIDTH-1 downto 0);
             M_AXIS_TSTRB	: out std_logic_vector((C_M_AXIS_TDATA_WIDTH/8)-1 downto 0);
             M_AXIS_TLAST	: out std_logic;
             M_AXIS_TREADY	: in std_logic
         );
end FIFO2AXIS_v1_0_M00_AXIS;

architecture implementation of FIFO2AXIS_v1_0_M00_AXIS is

    -- Define the states of state machine
    -- The control state machine oversees the writing of input streaming data to the FIFO,
    -- and outputs the streaming data from the FIFO
    type state is (
    IDLE,          -- This is the initial/idle state
    INIT_COUNTER,  -- This state initializes the counter, once the counter reaches C_M_START_COUNT count, the state machine changes state to INIT_WRITE
    SEND_STREAM);  -- In this state the stream data is output through M_AXIS_TDATA state variable
    signal  mst_exec_state : state;

    signal outputEmpty_delay: std_logic;
    signal outputRden_delay: std_logic;
    signal tx_en_delay: std_logic;

    -- AXI Stream internal signals
    signal count	: integer;
    signal axis_tvalid	: std_logic;
    --signal axis_tvalid_delay	: std_logic; -- streaming data valid delayed by one clock cycle
    signal axis_tlast	: std_logic;
    signal axis_tlast_delay	: std_logic; -- last of the streaming data delayed by one clock cycle
    signal stream_data_out	: std_logic_vector(C_M_AXIS_TDATA_WIDTH-1 downto 0);

    signal tx_en	: std_logic;

    -- fist buffer
    signal valid_buf_0: boolean := false;
    signal buf_0: std_logic_vector(C_M_AXIS_TDATA_WIDTH-1 downto 0);
    -- second buffer
    signal valid_buf_1: boolean := false;
    signal buf_1: std_logic_vector(C_M_AXIS_TDATA_WIDTH-1 downto 0);

begin
    -- Control state machine implementation
    process(M_AXIS_ACLK)
    begin
        if (rising_edge (M_AXIS_ACLK)) then
            if(M_AXIS_ARESETN = '0') then
                -- Synchronous reset (active low)
                mst_exec_state <= IDLE;
                count <= 0;
            else
                case (mst_exec_state) is
                    when IDLE =>
                        -- The slave starts accepting tdata when tvalid is asserted to mark the presence of valid streaming data
                        mst_exec_state <= INIT_COUNTER;

                    when INIT_COUNTER =>
                        -- This state is responsible to wait for user defined C_M_START_COUNT number of clock cycles.
                        if ( count = (C_M_START_COUNT - 1)) then
                            mst_exec_state  <= SEND_STREAM;
                        else
                            count <= count + 1;
                            mst_exec_state  <= INIT_COUNTER;
                        end if;

                    when SEND_STREAM  =>
                        -- The streaming master functionality starts when the master drives output tdata from the FIFO and the slave has finished storing the S_AXIS_TDATA
                        mst_exec_state <= SEND_STREAM;
                end case;
            end if;
        end if;
    end process;

    -- I/O Connections assignments
    M_AXIS_TDATA	<= stream_data_out;
    M_AXIS_TSTRB	<= (others => '1');

    --axis_tvalid is asserted when the control state machine's state is SEND_STREAM and output data is available
    axis_tvalid <= '1' when ((mst_exec_state = SEND_STREAM) and valid_buf_1) else '0';

    -- Delay the outputEmpty, tx_en, axis_tvalid and axis_tlast signal by one clock cycle
    -- to match the latency of M_AXIS_TDATA
    process(M_AXIS_ACLK)
    begin
        if (rising_edge (M_AXIS_ACLK)) then
            if(M_AXIS_ARESETN = '0') then
                --axis_tvalid_delay <= '0';
                axis_tlast_delay <= '0';
                axis_tlast <= '0';
                outputEmpty_delay <= '1';
                tx_en_delay <= '0';
            else
                if (M_AXIS_TREADY = '1') then
                    axis_tlast <= outputFinal and outputRden;
                    axis_tlast_delay <= axis_tlast;
                    outputEmpty_delay <= outputEmpty;
                end if;

                --axis_tvalid_delay <= axis_tvalid;
                tx_en_delay <= tx_en;
                outputRden_delay <= outputRden;

                M_AXIS_TVALID	<= axis_tvalid;
                M_AXIS_TLAST <= axis_tlast_delay;
            end if;
        end if;
    end process;

    tx_en <= M_AXIS_TREADY and axis_tvalid;

    -- reading data from the fifo
    process(M_AXIS_ACLK)
        variable sig_one: integer := 1;
    begin
        if (rising_edge(M_AXIS_ACLK)) then
            if (M_AXIS_ARESETN = '0') then
                outputRden <= '0';
                stream_data_out <= std_logic_vector(to_unsigned(sig_one,C_M_AXIS_TDATA_WIDTH));
                valid_buf_0 <= false;
                valid_buf_1 <= false;
            else
                if (M_AXIS_TREADY = '1') then
                    -- obtain data
                    if (outputRden_delay = '1' or outputFinal = '1') then
                        buf_0 <= outputData;
                        valid_buf_0 <= outputEmpty_delay = '0';
                    end if;

                    stream_data_out <= buf_1;
                    buf_1 <= buf_0;
                    valid_buf_1 <= valid_buf_0;

                    -- set next read state
                    if (M_AXIS_TREADY = '1' and outputEmpty = '0') then
                        outputRden <= '1';
                    else
                        outputRden <= '0';
                    end if;
                end if;
            end if;
        end if;
    end process;

end implementation;
