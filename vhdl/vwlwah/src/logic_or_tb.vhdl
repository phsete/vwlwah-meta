library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;

--  A testbench has no ports.
entity logic_or_tb is
    end logic_or_tb;

architecture behav of logic_or_tb is

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
    component logic_or is
        Generic (
                    word_size:           natural := 5;
                    num_inputs:          natural := 2;
                    fill_counter_size:   natural := 32
                );
        port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(num_inputs*word_size-1 downto 0);
                 in_empty:      in std_logic_vector(0 to num_inputs-1);
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(word_size-1 downto 0);
                 in_rd:         out std_logic_vector(0 to num_inputs-1);
                 out_wr:        out std_logic;
                 final_in:      in std_logic_vector(0 to num_inputs-1);
                 final_out:     out std_logic;
                 reset:         in std_logic
             );
    end component;

    component input_fifo
        Generic (
                    constant Addrbreite: natural := 3;
                    constant Wortbreite: natural := 5
                );
        Port ( Din   : in  STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               Wr    : in  STD_LOGIC;
               Dout  : out STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               Rd    : in  STD_LOGIC;
               Empty : out STD_LOGIC;
               Full  : out STD_LOGIC;
               CLK   : in  STD_LOGIC;
               Final_in: in std_logic;
               Final_out: out std_logic;
               Reset: in std_logic
           );
    end component;

    component output_fifo
        Generic (
                    constant Addrbreite: natural := 3;
                    constant Wortbreite: natural := 5
                );
        Port ( Din   : in  STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               Wr    : in  STD_LOGIC;
               Dout  : out STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               Rd    : in  STD_LOGIC;
               Empty : out STD_LOGIC;
               Full  : out STD_LOGIC;
               CLK   : in  STD_LOGIC;
               Final_in: in std_logic;
               Final_out: out std_logic;
               Reset: in std_logic
           );
    end component;

    --  Specifies which entity is bound with the component.
    for logic_or_0: logic_or use entity work.logic_or;
    for input_fifo_0: input_fifo use entity work.FIFO_bb;
    for input_fifo_1: input_fifo use entity work.FIFO_bb;
    for output_fifo_0: output_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk_in:          std_logic_vector(9 downto 0);
    signal blk_out:         std_logic_vector(4 downto 0);
    signal in_empty:        std_logic_vector(0 to 1);
    signal out_full:        std_logic;
    signal in_rd:           std_logic_vector(0 to 1);
    signal out_wr:          std_logic;
    signal final_in:        std_logic_vector(0 to 1);
    signal final_out:       std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input0:   std_logic_vector(4 downto 0);
    signal outer_input1:   std_logic_vector(4 downto 0);
    signal outer_wr_en0:   std_logic;
    signal outer_wr_en1:   std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(4 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full0:    std_logic;
    signal outer_full1:    std_logic;
    signal outer_final_in0:  std_logic;
    signal outer_final_in1:  std_logic;
    signal outer_final_out:  std_logic;
    signal outer_reset:      std_logic;

    begin
        --  Component instantiation.
        logic_or_0: logic_or
        port map (clk => outer_clk,
                  blk_in => blk_in,
                  blk_out => blk_out,
                  in_empty => in_empty,
                  out_full => out_full,
                  in_rd => in_rd,
                  final_in => final_in,
                  final_out => final_out,
                  reset => outer_reset,
                  out_wr => out_wr);

        input_fifo_0: input_fifo
        port map (CLK => outer_clk,
                  Din => outer_input0,
                  Wr => outer_wr_en0,
                  Dout => blk_in(4 downto 0),
                  Rd => in_rd(0),
                  Empty => in_empty(0),
                  Final_in => outer_final_in0,
                  Final_out => final_in(0),
                  Reset => outer_reset,
                  Full => outer_full0);

        input_fifo_1: input_fifo
        port map (CLK => outer_clk,
                  Din => outer_input1,
                  Wr => outer_wr_en1,
                  Dout => blk_in(9 downto 5),
                  Rd => in_rd(1),
                  Empty => in_empty(1),
                  Final_in => outer_final_in1,
                  Final_out => final_in(1),
                  Reset => outer_reset,
                  Full => outer_full1);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  Din => blk_out,
                  Wr => out_wr,
                  Dout => outer_output,
                  Rd => outer_rd_en,
                  Empty => outer_empty,
                  Final_in => final_out,
                  Final_out => outer_final_out,
                  Reset => outer_reset,
                  Full => out_full);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the logic_or.
            outer_reset: std_logic;
            outer_final_in0: std_logic;
            outer_input0: std_logic_vector(4 downto 0);
            outer_final_in1: std_logic;
            outer_input1: std_logic_vector(4 downto 0);
            outer_wr_en0: std_logic;
            outer_wr_en1: std_logic;
            --  The expected outputs of the logic_or.
            outer_output: std_logic_vector(4 downto 0);
            outer_empty: std_logic;
            outer_final_out: std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (('0', '0', "00001", '0', "UUUUU", '1', '0', "UUUUU", '1', '0'),  -- 00
         ('0', '0', "10011", '0', "00001", '1', '1', "UUUUU", '1', '0'),  -- 01
         ('0', '0', "11001", '0', "10011", '1', '1', "UUUUU", '1', '0'),  -- 02
         ('0', '0', "11010", '0', "11001", '1', '1', "UUUUU", '1', '0'),  -- 03
         ('0', '0', "00001", '0', "11010", '1', '1', "UUUUU", '1', '0'),  -- 04
         ('0', '0', "00010", '0', "00001", '1', '1', "UUUUU", '1', '0'),  -- 05
         ('0', '0', "00100", '0', "00010", '1', '1', "UUUUU", '1', '0'),  -- 06
         ('0', '0', "01000", '0', "00100", '1', '1', "UUUUU", '0', '0'),  -- 07
         ('0', '1', "00010", '0', "01000", '1', '1', "00001", '1', '0'),  -- 08
         ('0', '0', "00000", '1', "00001", '0', '1', "00001", '1', '0'),  -- 09
         ('0', '0', "00000", '0', "00000", '0', '0', "00001", '0', '0'),  -- 10
         ('0', '0', "00000", '0', "00000", '0', '0', "10011", '1', '0'),  -- 11
         ('0', '0', "00000", '0', "00000", '0', '0', "10011", '1', '0'),  -- 12
         ('0', '0', "00000", '0', "00000", '0', '0', "10011", '1', '0'),  -- 13
         ('0', '0', "00000", '0', "00000", '0', '0', "10011", '0', '0'),  -- 14
         ('0', '0', "00000", '0', "00000", '0', '0', "11001", '0', '0'),  -- 15
         ('0', '0', "00000", '0', "00000", '0', '0', "11010", '1', '0'),  -- 16
         ('0', '0', "00000", '0', "00000", '0', '0', "11010", '1', '0'),  -- 17
         ('0', '0', "00000", '0', "00000", '0', '0', "11010", '0', '0'),  -- 18
         ('0', '0', "00000", '0', "00000", '0', '0', "00001", '1', '0'),  -- 19
         ('0', '0', "00000", '0', "00000", '0', '0', "00001", '0', '0'),  -- 20
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 21
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 22
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 23
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 24
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 25
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 26
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 27
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 28
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 29
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 30
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 31
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '1'),  -- 32
         ('1', '0', "00000", '0', "00000", '0', '0', "00010", '1', '0'),  -- 33
         ('0', '0', "00000", '0', "00000", '0', '0', "00010", '1', '0')); -- 34
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_wr_en0 <= patterns(i).outer_wr_en0;
                outer_wr_en1 <= patterns(i).outer_wr_en1;
                outer_input0<= patterns(i).outer_input0;
                outer_input1<= patterns(i).outer_input1;
                outer_reset <= patterns(i).outer_reset;
                outer_final_in0 <= patterns(i).outer_final_in0;
                outer_final_in1 <= patterns(i).outer_final_in1;
                outer_rd_en <= '1';

                -- simulate the clock
                outer_clk <= '0';
                wait for 1 ns;
                outer_clk <= '1';

                --  Wait for the results.
                wait for 1 ns;

                --  Check the outputs.
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
