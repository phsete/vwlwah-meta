library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity AXIS2FIFO_tb is
    end AXIS2FIFO_tb;

architecture behav of AXIS2FIFO_tb is

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
                    C_S00_AXIS_TDATA_WIDTH	: integer	:= 5
                );
        port (
                 inputData: out std_logic_vector (C_S00_AXIS_TDATA_WIDTH-1 downto 0);
                 inputFull: in std_logic;
                 inputWren: out std_logic;
                 inputAlmostFull: in std_logic;
                 inputFinal: out std_logic;
                 s00_axis_aclk	: in std_logic;
                 s00_axis_aresetn	: in std_logic;
                 s00_axis_tready	: out std_logic;
                 s00_axis_tdata	: in std_logic_vector(C_S00_AXIS_TDATA_WIDTH-1 downto 0);
                 s00_axis_tlast	: in std_logic;
                 s00_axis_tvalid	: in std_logic
             );
    end component axis2fifo;

    component input_fifo
        Generic (
                    constant addr_width: natural := 2;
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
    for axis2fifo_0: axis2fifo use entity work.AXIS2FIFO_v1_0;
    for input_fifo_0: input_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk_out:         std_logic_vector(4 downto 0);
    signal in_empty:        std_logic;
    signal out_full:        std_logic;
    signal out_almost_full: std_logic;
    signal in_rd:           std_logic;
    signal out_wr:          std_logic;
    signal final_in:        std_logic;
    signal final_out:       std_logic;

    -- outer signals
    signal outer_clk:         std_logic;
    signal outer_input:       std_logic_vector(4 downto 0);
    signal outer_rd_en:       std_logic;
    signal outer_output:      std_logic_vector(4 downto 0);
    signal outer_empty:       std_logic;
    signal outer_final_out:   std_logic;
    signal outer_reset:       std_logic;

    signal outer_tready:      std_logic;
    signal outer_tlast:       std_logic;
    signal outer_tvalid:      std_logic;

    begin
        --  Component instantiation.
        axis2fifo_0: axis2fifo
        port map (s00_axis_aclk => outer_clk,
                  s00_axis_tdata => outer_input,
                  s00_axis_aresetn => outer_reset,
                  s00_axis_tlast => outer_tlast,
                  s00_axis_tready => outer_tready,
                  s00_axis_tvalid => outer_tvalid,
                  InputData => blk_out,
                  InputFull => out_full,
                  InputAlmostFull => out_almost_full,
                  InputFinal => final_out,
                  InputWren => out_wr);

        input_fifo_0: input_fifo
        port map (CLK => outer_clk,
                  BLK_IN => blk_out,
                  WR_EN => out_wr,
                  BLK_OUT => outer_output,
                  RD_EN => outer_rd_en,
                  EMPTY => outer_empty,
                  FINAL_IN => final_out,
                  FINAL_OUT => outer_final_out,
                  Reset => outer_reset,
                  ALMOST_FULL => out_almost_full,
                  FULL => out_full);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the AXIS2FIFO_v1_0.
            outer_reset: std_logic;
            outer_input: std_logic_vector(4 downto 0);
            outer_tlast: std_logic;
            outer_tvalid:std_logic;
            outer_rd_en: std_logic;
            --  The expected outputs of the FIFO
            outer_output: std_logic_vector(4 downto 0);
            outer_empty: std_logic;
            outer_final_out: std_logic;
            outer_tready: std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (('1', "00000", '0', '0', '0', "UUUUU", '1', '0', '0'),  -- 00
         ('0', "00000", '0', '0', '0', "UUUUU", '1', '0', '0'),  -- 01
         ('1', "00000", '0', '0', '0', "UUUUU", '1', '0', '0'),  -- 02
         ('1', "00001", '0', '1', '0', "UUUUU", '1', '0', '1'),  -- 03
         ('1', "00010", '0', '1', '0', "UUUUU", '0', '0', '1'),  -- 04
         ('1', "00100", '1', '1', '0', "UUUUU", '0', '0', '1'),  -- 05
         ('1', "00000", '0', '0', '0', "UUUUU", '0', '0', '0'),  -- 06
         ('1', "00000", '0', '0', '0', "UUUUU", '0', '0', '0'),  -- 07
         ('1', "00000", '0', '0', '1', "00001", '0', '0', '0'),  -- 08
         ('1', "00000", '0', '0', '1', "00010", '0', '0', '0'),  -- 09
         ('1', "00000", '0', '0', '1', "00100", '1', '1', '0'),  -- 20
         ('1', "00000", '0', '0', '1', "00100", '1', '1', '0'),  -- 21
         ('0', "00000", '0', '0', '0', "00100", '1', '1', '0'),  -- 22
         ('1', "00001", '0', '1', '0', "00100", '1', '1', '0'),  -- 23
         ('1', "00010", '0', '1', '0', "00100", '1', '1', '0'),  -- 24
         ('1', "00100", '0', '1', '0', "00100", '1', '1', '0'),  -- 25
         ('1', "01000", '0', '1', '0', "00100", '1', '1', '0'),  -- 26
         ('1', "01000", '0', '1', '0', "00100", '1', '1', '0'),  -- 27
         ('1', "01000", '0', '1', '1', "00001", '1', '1', '0'),  -- 28
         ('1', "01000", '0', '1', '1', "00010", '1', '1', '0'),  -- 29
         ('1', "01000", '0', '1', '0', "00100", '1', '1', '0'),  -- 30
         ('1', "10000", '1', '1', '0', "00100", '1', '1', '0'),  -- 31
         ('1', "00000", '0', '0', '0', "00100", '1', '1', '0'),  -- 32
         ('1', "00000", '0', '0', '0', "00100", '1', '1', '0'),  -- 33
         ('1', "00000", '0', '0', '0', "00100", '1', '1', '0'),  -- 34
         ('1', "00000", '0', '0', '1', "00100", '1', '1', '0'),  -- 35
         ('1', "00000", '0', '0', '1', "00100", '1', '1', '0'),  -- 36
         ('1', "00000", '0', '0', '1', "00100", '1', '1', '0'),  -- 37
         ('1', "00000", '0', '0', '0', "00100", '1', '1', '0')); -- 38
        begin
            assert false report "begin of test" severity note;
            
            wait for 0.5 ns;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_reset <= patterns(i).outer_reset;
                outer_input <= patterns(i).outer_input;
                outer_tlast <= patterns(i).outer_tlast;
                outer_tvalid <= patterns(i).outer_tvalid;
                outer_rd_en <= patterns(i).outer_rd_en;

                -- simulate the clock
                wait for 0.5 ns;
                outer_clk <= '0';
                wait for 1 ns;
                outer_clk <= '1';

                --  Wait for the results.
                wait for 0.5 ns;

                --  Check the outputs.
                assert outer_tready = patterns(i).outer_tready
                report "tready state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_tready) & " but found " & std_logic'image(outer_tready) severity error;

                assert outer_empty = patterns(i).outer_empty
                report "empty state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_empty) & " but found " & std_logic'image(outer_empty) severity error;

                assert outer_final_out = patterns(i).outer_final_out
                report "final state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_final_out) & " but found " & std_logic'image(outer_final_out) severity error;

                assert outer_output = patterns(i).outer_output
                report "bad decoding in test " & integer'image(i) & ". Expected: " & to_string(patterns(i).outer_output) & " but found " & to_string(outer_output) severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
