library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity fifo_b32_tb is
    end fifo_b32_tb;

architecture behav of fifo_b32_tb is

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
    component fifo_b32
        Generic (
                    constant addr_width: natural := 3;
                    constant word_size: natural := 5
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (word_size-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (31 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               FINAL_IN:  in  STD_LOGIC;
               FINAL_OUT: out STD_LOGIC;
               RESET: in STD_LOGIC;
               CLK   : in  STD_LOGIC
           );
    end component;

    --  Specifies which entity is bound with the component.
    for fifo_0: fifo_b32 use entity work.fifo_b32;

    -- outer signals
    signal outer_clk:      std_logic;
    signal final_in:       std_logic;
    signal final_out:      std_logic;
    signal outer_input:    std_logic_vector(4 downto 0);
    signal outer_wr_en:    std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(31 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full:     std_logic;
    signal outer_reset:    std_logic;

    begin
        --  Component instantiation.
        fifo_0: fifo_b32
        port map (CLK => outer_clk,
                  FINAL_IN => final_in,
                  FINAL_OUT => final_out,
                  BLK_IN => outer_input,
                  WR_EN => outer_wr_en,
                  BLK_OUT => outer_output,
                  RD_EN => outer_rd_en,
                  EMPTY => outer_empty,
                  FULL => outer_full,
                  RESET => outer_reset);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the fifo.
            outer_reset: std_logic;
            final_in:    std_logic;
            outer_input: std_logic_vector(4 downto 0);
            outer_wr_en: std_logic;
            outer_rd_en: std_logic;
            --  The expected outputs of the fifo.
            outer_output: std_logic_vector(31 downto 0);
            outer_empty: std_logic;
            final_out:   std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (('1', '0', "00001", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '1', '0'),  -- 00
         ('1', '0', "00010", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '1', '0'),  -- 01
         ('1', '0', "00100", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '1', '0'),  -- 02
         ('1', '0', "01000", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '1', '0'),  -- 03
         ('1', '0', "10000", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '1', '0'),  -- 04
         ('1', '0', "00001", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '1', '0'),  -- 05
         ('1', '0', "00010", '1', '1', "UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU", '0', '0'),  -- 06
         ('1', '0', "00100", '1', '1', "00001000100010001000100000000100", '1', '0'),  -- 07
         ('1', '0', "01000", '1', '1', "00001000100010001000100000000100", '1', '0'),  -- 08
         ('1', '0', "10000", '1', '1', "00001000100010001000100000000100", '1', '0'),  -- 09
         ('1', '0', "00001", '1', '1', "00001000100010001000100000000100", '1', '0'),  -- 10
         ('1', '0', "00010", '1', '1', "00001000100010001000100000000100", '1', '0'),  -- 11
         ('1', '0', "00100", '1', '1', "00001000100010001000100000000100", '0', '0'),  -- 12
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 13
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 14
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 15
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 16
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 17
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 18
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 19
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 20
         ('1', '0', "00000", '1', '0', "00001000100010001000100000000100", '0', '0'),  -- 21
         ('1', '0', "00000", '1', '1', "01000100010001000000001000100010", '0', '0'),  -- 22
         ('1', '0', "00000", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 23
         ('1', '0', "00000", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 24
         ('1', '0', "00011", '1', '1', "00000000000000000000000000000000", '0', '0'),  -- 25
         ('1', '0', "11111", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 26
         ('1', '0', "11111", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 27
         ('1', '0', "11111", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 28
         ('1', '0', "11111", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 29
         ('1', '0', "11111", '1', '1', "00000000000000000000000000000000", '1', '0'),  -- 30
         ('1', '1', "11111", '1', '1', "00000000000000000000000000000000", '0', '0'),  -- 31
         ('1', '0', "00000", '0', '1', "11111111111111111111111111111111", '1', '1'),  -- 32
         ('0', '0', "00000", '0', '1', "11111111111111111111111111111111", '1', '0'),  -- 33
         ('1', '0', "10000", '1', '1', "11111111111111111111111111111111", '1', '0'),  -- 34
         ('1', '0', "10001", '1', '1', "11111111111111111111111111111111", '1', '0'),  -- 35
         ('1', '0', "10010", '1', '1', "11111111111111111111111111111111", '1', '0'),  -- 36
         ('1', '0', "10011", '1', '1', "11111111111111111111111111111111", '1', '0'),  -- 37
         ('1', '0', "10100", '1', '1', "11111111111111111111111111111111", '1', '0'),  -- 38
         ('1', '0', "10101", '1', '1', "11111111111111111111111111111111", '1', '0'),  -- 39
         ('1', '1', "10110", '1', '1', "11111111111111111111111111111111", '0', '0'),  -- 40
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 41
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 42
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 43
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 44
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 45
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 46
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 47
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 48
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 49
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 50
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 51
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 52
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 53
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 54
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 55
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 56
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 57
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 58
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 59
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 60
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 61
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 62
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1'),  -- 63
         ('1', '0', "00000", '0', '1', "10000100011001010011101001010110", '1', '1')); -- 64

        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_wr_en <= patterns(i).outer_wr_en;
                outer_input <= patterns(i).outer_input;
                outer_rd_en <= patterns(i).outer_rd_en;
                final_in <= patterns(i).final_in;
                outer_reset <= patterns(i).outer_reset;

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
                report "bad encoding in test " & integer'image(i) & ". Expected: " & to_string(patterns(i).outer_output) & " but found " & to_string(outer_output) severity error;

                assert final_out = patterns(i).final_out
                report "bad final state in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).final_out) & " but found " & std_logic'image(final_out) severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
