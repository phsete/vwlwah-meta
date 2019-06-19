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
                 out_wr:        out std_logic
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
               CLK   : in  STD_LOGIC
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
               CLK   : in  STD_LOGIC
           );
    end component;

    --  Specifies which entity is bound with the component.
    for logic_or_0: logic_or use entity work.logic_or;
    for input_fifo_0: input_fifo use entity work.FIFO;
    for input_fifo_1: input_fifo use entity work.FIFO;
    for output_fifo_0: output_fifo use entity work.FIFO;

    -- inner signals
    signal blk_in:          std_logic_vector(9 downto 0);
    signal blk_out:         std_logic_vector(4 downto 0);
    signal in_empty:        std_logic_vector(0 to 1);
    signal out_full:        std_logic;
    signal in_rd:           std_logic_vector(0 to 1);
    signal out_wr:          std_logic;

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

    begin
        --  Component instantiation.
        logic_or_0: logic_or
        port map (clk => outer_clk,
                  blk_in => blk_in,
                  blk_out => blk_out,
                  in_empty => in_empty,
                  out_full => out_full,
                  in_rd => in_rd,
                  out_wr => out_wr);

        input_fifo_0: input_fifo
        port map (CLK => outer_clk,
                  Din => outer_input0,
                  Wr => outer_wr_en0,
                  Dout => blk_in(4 downto 0),
                  Rd => in_rd(0),
                  Empty => in_empty(0),
                  Full => outer_full0);

        input_fifo_1: input_fifo
        port map (CLK => outer_clk,
                  Din => outer_input1,
                  Wr => outer_wr_en1,
                  Dout => blk_in(9 downto 5),
                  Rd => in_rd(1),
                  Empty => in_empty(1),
                  Full => outer_full1);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  Din => blk_out,
                  Wr => out_wr,
                  Dout => outer_output,
                  Rd => outer_rd_en,
                  Empty => outer_empty,
                  Full => out_full);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the logic_or.
            outer_input0: std_logic_vector(4 downto 0);
            outer_input1: std_logic_vector(4 downto 0);
            outer_wr_en0: std_logic;
            outer_wr_en1: std_logic;
            --  The expected outputs of the logic_or.
            outer_output: std_logic_vector(4 downto 0);
            outer_empty: std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (("00001", "00001", '1', '1', "UUUUU", '1'),  -- 00
         ("10011", "10011", '1', '1', "UUUUU", '1'),  -- 01
         ("11001", "11001", '1', '1', "UUUUU", '1'),  -- 02
         ("11010", "11010", '1', '1', "UUUUU", '1'),  -- 03
         ("00001", "00001", '1', '1', "UUUUU", '1'),  -- 04
         ("00010", "00010", '1', '1', "UUUUU", '1'),  -- 05
         ("00100", "00100", '0', '0', "UUUUU", '1'),  -- 06
         ("01000", "01000", '0', '0', "UUUUU", '1'),  -- 07
         ("00001", "00001", '0', '0', "UUUUU", '1'),  -- 08
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 09
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 10
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 11
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 12
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 13
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 14
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 15
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 16
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 17
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 18
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 19
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 20
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 21
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 21
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 21
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 21
         ("00000", "00000", '0', '0', "UUUUU", '1'),  -- 21
         ("00000", "00000", '0', '0', "UUUUU", '1')); -- 21
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_wr_en0 <= patterns(i).outer_wr_en0;
                outer_wr_en1 <= patterns(i).outer_wr_en1;
                outer_input0<= patterns(i).outer_input0;
                outer_input1<= patterns(i).outer_input1;
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

                assert outer_output = patterns(i).outer_output
                report "bad decoding in test " & integer'image(i) & ". Expected: " & to_string(patterns(i).outer_output) & " but found " & to_string(outer_output) severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
