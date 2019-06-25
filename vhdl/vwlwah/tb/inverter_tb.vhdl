library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity inverter_tb is
    end inverter_tb;

architecture behav of inverter_tb is

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
    component inverter is
        Generic (
                    word_size:           natural := 5;
                    fill_counter_size:   natural := 32
                );
        port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(word_size-1 downto 0);
                 in_empty:      in std_logic;
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(word_size-1 downto 0);
                 in_rd:         out std_logic;
                 final_in: in STD_LOGIC;
                 final_out: out STD_LOGIC;
                 RESET: in std_logic;
                 out_wr:        out std_logic
             );
    end component;

    component input_fifo
        Generic (
                    constant Addrbreite: natural := 3;
                    constant Wortbreite: natural := 5
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               FINAL_IN: in STD_LOGIC;
               FINAL_OUT: out STD_LOGIC;
               RESET: in std_logic;
               CLK   : in  STD_LOGIC
           );
    end component;

    component output_fifo
        Generic (
                    constant Addrbreite: natural := 3;
                    constant Wortbreite: natural := 5
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               FINAL_IN: in STD_LOGIC;
               FINAL_OUT: out STD_LOGIC;
               RESET: in std_logic;
               CLK   : in  STD_LOGIC
           );
    end component;

    --  Specifies which entity is bound with the component.
    for inverter_0: inverter use entity work.inverter;
    for input_fifo_0: input_fifo use entity work.FIFO_bb;
    for output_fifo_0: output_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk_in:          std_logic_vector(4 downto 0);
    signal blk_out:         std_logic_vector(4 downto 0);
    signal in_empty:        std_logic;
    signal out_full:        std_logic;
    signal in_rd:           std_logic;
    signal out_wr:          std_logic;
    signal final_in:        std_logic;
    signal final_out:       std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input:    std_logic_vector(4 downto 0);
    signal outer_wr_en:    std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(4 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full:     std_logic;
    signal outer_final_in:  std_logic;
    signal outer_final_out: std_logic;
    signal outer_reset:    std_logic;

    begin
        --  Component instantiation.
        inverter_0: inverter
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
                  BLK_IN => outer_input,
                  WR_EN => outer_wr_en,
                  BLK_OUT => blk_in,
                  RD_EN => in_rd,
                  EMPTY => in_empty,
                  FINAL_IN => outer_final_in,
                  FINAL_OUT => final_in,
                  Reset => outer_reset,
                  FULL => outer_full);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  BLK_IN => blk_out,
                  WR_EN => out_wr,
                  BLK_OUT => outer_output,
                  RD_EN => outer_rd_en,
                  EMPTY => outer_empty,
                  FINAL_IN => final_out,
                  FINAL_OUT => outer_final_out,
                  Reset => outer_reset,
                  FULL => out_full);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the inverter.
            outer_reset: std_logic;
            outer_final_in: std_logic;
            outer_input: std_logic_vector(4 downto 0);
            outer_wr_en: std_logic;
            outer_rd_en: std_logic;
            --  The expected outputs of the inverter.
            outer_output: std_logic_vector(4 downto 0);
            outer_empty: std_logic;
            outer_final_out: std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (('0', '0', "00001", '1', '1', "UUUUU", '1', '0'),  -- 00
         ('0', '0', "10011", '1', '1', "UUUUU", '1', '0'),  -- 01
         ('0', '0', "11001", '1', '1', "UUUUU", '1', '0'),  -- 02
         ('0', '0', "11010", '1', '1', "UUUUU", '0', '0'),  -- 03
         ('0', '0', "00001", '1', '1', "01110", '0', '0'),  -- 04
         ('0', '1', "00010", '1', '1', "11011", '0', '0'),  -- 05
         ('0', '0', "00000", '0', '1', "10001", '0', '0'),  -- 06
         ('0', '0', "00000", '0', '1', "10010", '0', '0'),  -- 07
         ('0', '0', "00000", '0', '1', "01110", '0', '0'),  -- 08
         ('0', '0', "00000", '0', '1', "01101", '1', '1'),  -- 09
         ('0', '0', "00000", '0', '1', "01101", '1', '1'),  -- 10
         ('1', '0', "00000", '0', '1', "01101", '1', '0'),  -- 11
         ('0', '0', "01110", '1', '1', "01101", '1', '0'),  -- 12
         ('0', '0', "01101", '1', '1', "01101", '1', '0'),  -- 13
         ('0', '0', "00000", '0', '1', "01101", '1', '0'),  -- 14
         ('0', '0', "00000", '0', '1', "01101", '0', '0'),  -- 15
         ('0', '0', "00000", '0', '1', "00001", '0', '0'),  -- 16
         ('0', '0', "00000", '0', '1', "00010", '1', '0'),  -- 17
         ('0', '0', "00000", '0', '1', "00010", '1', '0'),  -- 18
         ('0', '0', "00000", '0', '1', "00010", '1', '0'),  -- 19
         ('0', '0', "01011", '1', '1', "00010", '1', '0'),  -- 20
         ('0', '1', "00111", '1', '1', "00010", '1', '0'),  -- 21
         ('0', '0', "00000", '0', '1', "00010", '1', '0'),  -- 21
         ('0', '0', "00000", '0', '1', "00010", '0', '0'),  -- 21
         ('0', '0', "00000", '0', '1', "00100", '0', '0'),  -- 21
         ('0', '0', "00000", '0', '1', "01000", '1', '1'),  -- 21
         ('0', '0', "00000", '0', '1', "01000", '1', '1')); -- 21
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_wr_en <= patterns(i).outer_wr_en;
                outer_input <= patterns(i).outer_input;
                outer_rd_en <= patterns(i).outer_rd_en;
                outer_reset <= patterns(i).outer_reset;
                outer_final_in <= patterns(i).outer_final_in;

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
