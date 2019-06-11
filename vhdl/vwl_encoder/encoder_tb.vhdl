library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity encoder_tb is
    end encoder_tb;

architecture behav of encoder_tb is

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
    component encoder
        port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(3 downto 0);
                 in_empty:      in std_logic;
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(4 downto 0);
                 in_rd:         out std_logic;
                 out_wr:        out std_logic
             );
    end component;

    component input_fifo
        Generic (
                    constant Addrbreite: natural := 8;
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

    component output_fifo
        Generic (
                    constant Addrbreite: natural := 8;
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
    for encoder_0: encoder use entity work.encoder;
    for input_fifo_0: input_fifo use entity work.FIFO;
    for output_fifo_0: output_fifo use entity work.FIFO;

    -- inner signals
    signal blk_in:          std_logic_vector(3 downto 0);
    signal blk_out:         std_logic_vector(4 downto 0);
    signal in_empty:        std_logic;
    signal out_full:        std_logic;
    signal in_rd:           std_logic;
    signal out_wr:          std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input:    std_logic_vector(3 downto 0);
    signal outer_wr_en:    std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(4 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full:     std_logic;

    begin
        --  Component instantiation.
        encoder_0: encoder
        -- THIS IS HOW IT WORKS WITH GENERICS: generic map (WORD_SIZE <= 5)
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
            --  The inputs of the encoder.
            blk_in: std_logic_vector(3 downto 0);
            write_enable: std_logic;
            read_enable: std_logic;
            --  The expected outputs of the encoder.
            blk_out: std_logic_vector(4 downto 0);
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (("1111", '1', '0', "00000"),
         ("1111", '1', '0', "00000"),
         ("1111", '1', '0', "00000"),
         ("0000", '1', '0', "10010"),
         ("0000", '1', '0', "10001"),
         ("0000", '1', '0', "00000"),
         ("0001", '1', '0', "11011"),
         ("0010", '1', '0', "00001"),
         ("0100", '1', '0', "00010"),
         ("1111", '1', '0', "00100"),
         ("1111", '1', '0', "00000"),
         ("1111", '1', '0', "00000"),
         ("0000", '1', '0', "11011"),
         ("0000", '1', '0', "00000"),
         ("0000", '1', '0', "00000"),
         ("0001", '1', '0', "10011"),
         ("0010", '1', '0', "00001"),
         ("0100", '1', '0', "00010"),
         ("0000", '1', '0', "00100"),
         ("0000", '1', '0', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"),
         ("0000", '0', '1', "00000"));
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                blk_in <= patterns(i).blk_in;

                -- simulate the clock
                outer_clk <= '0';
                wait for 1 ns;
                outer_clk <= '1';

                --  Wait for the results.
                wait for 1 ns;

                --  Check the outputs.
                --assert empty = patterns(i).empty
                --report "empty state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).empty) & " but found " & std_logic'image(empty) severity error;

                --if (empty = '0') then
                    --assert blk_out = patterns(i).blk_out
                    --report "bad encoding in test " & integer'image(i) & ". Expected: " & to_string(patterns(i).blk_out) & " but found " & to_string(blk_out) severity error;
                --end if;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
