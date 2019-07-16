library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity FIFO2AXIS_tb is
    end FIFO2AXIS_tb;

architecture behav of FIFO2AXIS_tb is

    -- found this function implementation at: https://stackoverflow.com/questions/15406887/vhdl-convert-vector-to-string
    function to_string ( a: std_logic_vector) return string is
        variable b : string (1 to a'length) := (others => NUL);
        variable stri : integer := 1; 
    begin
        for i in a'range loop
            b(stri) := std_logic'image(a((i)))(2);
            stri := stri+1;
        end loop;
        return b;
    end function;

    -- Declaration of the components that will be instantiated.
    component axis2fifo is
        generic (
                    C_M00_AXIS_TDATA_WIDTH	: integer	:= 5;
                    C_M00_AXIS_START_COUNT	: integer	:= 3
                );
        port (
                 OutputData: in std_logic_vector (C_M00_AXIS_TDATA_WIDTH-1 downto 0);
                 OutputEmpty: in std_logic;
                 OutputRden: out std_logic;
                 OutputFinal: in std_logic;
                 m00_axis_aclk	: in std_logic;
                 m00_axis_aresetn	: in std_logic;
                 m00_axis_tvalid	: out std_logic;
                 m00_axis_tdata	: out std_logic_vector(C_M00_AXIS_TDATA_WIDTH-1 downto 0);
                 m00_axis_tstrb	: out std_logic_vector((C_M00_AXIS_TDATA_WIDTH/8)-1 downto 0);
                 m00_axis_tlast	: out std_logic;
                 m00_axis_tready	: in std_logic
             );
    end component axis2fifo;

    component output_fifo
        Generic (
                    constant addr_width: natural := 4;
                    constant word_size: natural := 5
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (word_size-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (word_size-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               ALMOST_FULL  : out STD_LOGIC;
               FINAL_IN: in STD_LOGIC;
               FINAL_OUT: out STD_LOGIC;
               RESET: in std_logic;
               CLK   : in  STD_LOGIC
           );
    end component;

    --  Specifies which entity is bound with the component.
    for axis2fifo_0: axis2fifo use entity work.FIFO2AXIS_v1_0;
    for output_fifo_0: output_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk:             std_logic_vector(4 downto 0);
    signal empty:           std_logic;
    signal rd_en:           std_logic;
    signal final:           std_logic;

    -- outer signals
    signal outer_reset:       std_logic;
    signal outer_clk:         std_logic;
    signal outer_input:       std_logic_vector(4 downto 0);
    signal outer_wr_en:       std_logic;
    signal outer_output:      std_logic_vector(4 downto 0);
    signal outer_final_in:    std_logic;

    signal outer_tready:      std_logic;
    signal outer_tlast:       std_logic;
    signal outer_tvalid:      std_logic;

    begin
        --  Component instantiation.
        axis2fifo_0: axis2fifo
        port map (m00_axis_aclk => outer_clk,
                  m00_axis_tdata => outer_output,
                  m00_axis_aresetn => outer_reset,
                  m00_axis_tlast => outer_tlast,
                  m00_axis_tready => outer_tready,
                  m00_axis_tvalid => outer_tvalid,
                  OutputData => blk,
                  OUtputRden => rd_en,
                  OutputFinal => final,
                  OutputEmpty => empty);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  BLK_IN => outer_input,
                  WR_EN => outer_wr_en,
                  BLK_OUT => blk,
                  RD_EN => rd_en,
                  EMPTY => empty,
                  FINAL_IN => outer_final_in,
                  FINAL_OUT => final,
                  Reset => outer_reset);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the FIFO2AXIS_v1_0.
            outer_reset:       std_logic;
            outer_input:       std_logic_vector(4 downto 0);
            outer_wr_en:       std_logic;
            outer_final_in:    std_logic;
            outer_tready:      std_logic;
            -- The expected outputs
            outer_tdata:       std_logic_vector(4 downto 0);
            outer_tlast:       std_logic;
            outer_tvalid:      std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (('1', "00000", '0', '0', '1', "UUUUU", 'U', 'U'),  -- 00
         ('0', "00000", '0', '0', '1', "00001", 'U', 'U'),  -- 01
         ('1', "00001", '1', '0', '1', "UUUUU", '0', '0'),  -- 02
         ('1', "00010", '1', '0', '1', "UUUUU", '0', '0'),  -- 03
         ('1', "00100", '1', '0', '1', "UUUUU", '0', '0'),  -- 04
         ('1', "01000", '1', '0', '1', "UUUUU", '0', '0'),  -- 05
         ('1', "10000", '1', '1', '1', "UUUUU", '0', '0'),  -- 06
         ('1', "00000", '0', '0', '1', "00001", '0', '1'),  -- 07
         ('1', "00000", '0', '0', '1', "00010", '0', '1'),  -- 08
         ('1', "00000", '0', '0', '1', "00100", '0', '1'),  -- 09
         ('1', "00000", '0', '0', '1', "01000", '0', '1'),  -- 10
         ('1', "00000", '0', '0', '1', "10000", '1', '1'),  -- 11
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 12
         ('0', "00000", '0', '0', '1', "00001", '0', '0'),  -- 13
         ('1', "00001", '1', '0', '1', "10000", '0', '0'),  -- 14
         ('1', "00010", '1', '0', '1', "10000", '0', '0'),  -- 15
         ('1', "00100", '1', '0', '1', "10000", '0', '0'),  -- 16
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 17
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 18
         ('1', "00000", '0', '0', '1', "00001", '0', '1'),  -- 19
         ('1', "00000", '0', '0', '1', "00010", '0', '1'),  -- 20
         ('1', "00000", '0', '0', '1', "00100", '0', '1'),  -- 21
         ('1', "01000", '1', '0', '1', "00100", '0', '0'),  -- 22
         ('1', "10000", '1', '1', '1', "00100", '0', '0'),  -- 23
         ('1', "00000", '0', '0', '1', "00100", '0', '0'),  -- 24
         ('1', "00000", '0', '0', '1', "00100", '0', '0'),  -- 25
         ('1', "00000", '0', '0', '1', "00100", '0', '0'),  -- 26
         ('1', "00000", '0', '0', '1', "01000", '0', '1'),  -- 27
         ('1', "00000", '0', '0', '1', "10000", '1', '1'),  -- 28
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 29
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 30
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 31
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 32
         ('0', "00000", '0', '0', '1', "00001", '0', '0'),  -- 33
         ('1', "00001", '1', '0', '0', "00001", '0', '0'),  -- 34
         ('1', "00010", '1', '0', '0', "00001", '0', '0'),  -- 35
         ('1', "00100", '1', '0', '0', "00001", '0', '0'),  -- 36
         ('1', "01000", '1', '0', '0', "00001", '0', '0'),  -- 37
         ('1', "10000", '1', '1', '0', "00001", '0', '0'),  -- 38
         ('1', "00000", '0', '0', '0', "00001", '0', '0'),  -- 39
         ('1', "00000", '0', '0', '0', "00001", '0', '0'),  -- 40
         ('1', "00000", '0', '0', '0', "00001", '0', '0'),  -- 41
         ('1', "00000", '0', '0', '0', "00001", '0', '0'),  -- 42
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 43
         ('1', "00000", '0', '0', '0', "10000", '0', '0'),  -- 44
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 45
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 46
         ('1', "00000", '0', '0', '1', "00001", '0', '1'),  -- 47
         ('1', "00000", '0', '0', '1', "00010", '0', '1'),  -- 48
         ('1', "00000", '0', '0', '0', "00010", '0', '1'),  -- 49
         ('1', "00000", '0', '0', '1', "00100", '0', '1'),  -- 50
         ('1', "00000", '0', '0', '0', "00100", '0', '1'),  -- 51
         ('1', "00000", '0', '0', '0', "00100", '0', '1'),  -- 52
         ('1', "00000", '0', '0', '1', "01000", '0', '1'),  -- 53
         ('1', "00000", '0', '0', '1', "10000", '1', '1'),  -- 54
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 55
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 56
         ('1', "00000", '0', '0', '1', "10000", '0', '0'),  -- 57
         ('1', "00000", '0', '0', '1', "10000", '0', '0')); -- 58
        begin
            assert false report "begin of test" severity note;
            
            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_reset <= patterns(i).outer_reset;
                outer_input <= patterns(i).outer_input;
                outer_wr_en <= patterns(i).outer_wr_en;
                outer_tready <= patterns(i).outer_tready;
                outer_final_in <= patterns(i).outer_final_in;

                -- simulate the clock
                outer_clk <= '1';
                wait for 1 ns;
                outer_clk <= '0';

                --  Wait for the results.
                wait for 1 ns;

                --  Check the outputs.
                assert outer_tvalid = patterns(i).outer_tvalid
                report "tvalid state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_tvalid) & " but found " & std_logic'image(outer_tvalid) severity error;

                assert outer_tlast = patterns(i).outer_tlast
                report "tlast state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_tlast) & " but found " & std_logic'image(outer_tlast) severity error;

                assert outer_output = patterns(i).outer_tdata
                report "bad decoding in test " & integer'image(i) & ". Expected: " & to_string(patterns(i).outer_tdata) & " but found " & to_string(outer_output) severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
