library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity decoder_tb is
    end decoder_tb;

architecture behav of decoder_tb is

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
    component decoder
        generic (
                    constant word_size: natural := 5;
                    constant fill_counter_size: natural := 32
                );
        port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(4 downto 0);
                 in_empty:      in std_logic;
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(3 downto 0);
                 in_rd:         out std_logic;
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
                    constant Wortbreite: natural := 4
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
    for decoder_0: decoder use entity work.decoder;
    for input_fifo_0: input_fifo use entity work.FIFO;
    for output_fifo_0: output_fifo use entity work.FIFO;

    -- inner signals
    signal blk_in:          std_logic_vector(4 downto 0);
    signal blk_out:         std_logic_vector(3 downto 0);
    signal in_empty:        std_logic;
    signal out_full:        std_logic;
    signal in_rd:           std_logic;
    signal out_wr:          std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input:    std_logic_vector(4 downto 0);
    signal outer_wr_en:    std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(3 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full:     std_logic;

    begin
        --  Component instantiation.
        decoder_0: decoder
        port map (clk => outer_clk,
                  blk_in => blk_in,
                  blk_out => blk_out,
                  in_empty => in_empty,
                  out_full => out_full,
                  in_rd => in_rd,
                  out_wr => out_wr);

        input_fifo_0: input_fifo
        port map (CLK => outer_clk,
                  Din => outer_input,
                  Wr => outer_wr_en,
                  Dout => blk_in,
                  Rd => in_rd,
                  Empty => in_empty,
                  Full => outer_full);

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
            --  The inputs of the decoder.
            outer_input: std_logic_vector(4 downto 0);
            outer_wr_en: std_logic;
            outer_rd_en: std_logic;
            --  The expected outputs of the decoder.
            outer_output: std_logic_vector(3 downto 0);
            outer_empty: std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (("10011", '1', '1', "UUUU", '1'),  -- 00
         ("00001", '1', '1', "UUUU", '1'),  -- 01
         ("11001", '1', '1', "UUUU", '1'),  -- 02
         ("11010", '1', '1', "UUUU", '1'),  -- 03
         ("00001", '1', '1', "UUUU", '0'),  -- 04
         ("00010", '1', '1', "0000", '0'),  -- 05
         ("00100", '0', '1', "0000", '0'),  -- 06
         ("01000", '0', '1', "0001", '0'),  -- 07
         ("00001", '0', '1', "1111", '0'),  -- 08
         ("00000", '0', '1', "1111", '0'),  -- 09
         ("00000", '0', '1', "1111", '0'),  -- 10
         ("00000", '0', '1', "0001", '0'),  -- 11
         ("00000", '0', '1', "0010", '1'),  -- 12
         ("00000", '0', '1', "0010", '1'),  -- 13
         ("00000", '0', '1', "0010", '1'),  -- 14
         ("00000", '0', '1', "0010", '1'),  -- 15
         ("00000", '0', '1', "0010", '1'),  -- 16
         ("00000", '0', '1', "0010", '1'),  -- 17
         ("00000", '0', '1', "0010", '1'),  -- 18
         ("00000", '0', '1', "0010", '1'),  -- 19
         ("00000", '0', '1', "0010", '1'),  -- 20
         ("00000", '0', '1', "0010", '1'),  -- 21
         ("00000", '0', '1', "0010", '1'),  -- 21
         ("00000", '0', '1', "0010", '1'),  -- 21
         ("00000", '0', '1', "0010", '1'),  -- 21
         ("00000", '0', '1', "0010", '1'),  -- 21
         ("00000", '0', '1', "0010", '1')); -- 21
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_wr_en <= patterns(i).outer_wr_en;
                outer_input <= patterns(i).outer_input;
                outer_rd_en <= patterns(i).outer_rd_en;

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
