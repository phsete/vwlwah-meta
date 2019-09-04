library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;
library work;

--  A testbench has no ports.
entity logic_or_big is
    end logic_or_big;

architecture behav of logic_or_big is

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
                    word_size:           natural := 4;
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
                    constant addr_width: natural := 3;
                    constant word_size: natural := 4
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (word_size-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (word_size-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               CLK   : in  STD_LOGIC;
               FINAL_IN: in std_logic;
               FINAL_OUT: out std_logic;
               RESET: in std_logic
           );
    end component;

    component output_fifo
        Generic (
                    constant addr_width: natural := 3;
                    constant word_size: natural := 4
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (word_size-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (word_size-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               CLK   : in  STD_LOGIC;
               FINAL_IN: in std_logic;
               FINAL_OUT: out std_logic;
               RESET: in std_logic
           );
    end component;

    --  Specifies which entity is bound with the component.
    for logic_or_0: logic_or use entity work.logic_or;
    for input_fifo_0: input_fifo use entity work.FIFO_bb;
    for input_fifo_1: input_fifo use entity work.FIFO_bb;
    for output_fifo_0: output_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk_in:          std_logic_vector(7 downto 0);
    signal blk_out:         std_logic_vector(3 downto 0);
    signal in_empty:        std_logic_vector(0 to 1);
    signal out_full:        std_logic;
    signal in_rd:           std_logic_vector(0 to 1);
    signal out_wr:          std_logic;
    signal final_in:        std_logic_vector(0 to 1);
    signal final_out:       std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input0:   std_logic_vector(3 downto 0);
    signal outer_input1:   std_logic_vector(3 downto 0);
    signal outer_wr_en0:   std_logic;
    signal outer_wr_en1:   std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(3 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full0:    std_logic;
    signal outer_full1:    std_logic;
    signal outer_final_in0:  std_logic;
    signal outer_final_in1:  std_logic;
    signal outer_final_out:  std_logic;
    signal outer_reset:      std_logic;

    file input_buf0: text;
    file input_buf1: text;
    file output_buf: text;

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
                  BLK_IN => outer_input0,
                  WR_EN => outer_wr_en0,
                  BLK_OUT => blk_in(3 downto 0),
                  RD_EN => in_rd(0),
                  EMPTY => in_empty(0),
                  FINAL_IN => outer_final_in0,
                  FINAL_OUT => final_in(0),
                  RESET => outer_reset,
                  FULL => outer_full0);

        input_fifo_1: input_fifo
        port map (CLK => outer_clk,
                  BLK_IN => outer_input1,
                  WR_EN => outer_wr_en1,
                  BLK_OUT => blk_in(7 downto 4),
                  RD_EN => in_rd(1),
                  EMPTY => in_empty(1),
                  FINAL_IN => outer_final_in1,
                  FINAL_OUT => final_in(1),
                  RESET => outer_reset,
                  FULL => outer_full1);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  BLK_IN => blk_out,
                  WR_EN => out_wr,
                  BLK_OUT => outer_output,
                  RD_EN => outer_rd_en,
                  EMPTY => outer_empty,
                  FINAL_IN => final_out,
                  FINAL_OUT => outer_final_out,
                  RESET => outer_reset,
                  FULL => out_full);

        --  This process does the real job.
        -- see: https://vhdlguide.readthedocs.io/en/latest/vhdl/testbench.html
        process
            variable read_col_from_input_buf0:  line;
            variable read_col_from_input_buf1:  line;
            variable read_col_from_output_buf: line;

            variable input_final0:  std_logic;
            variable input_final1:  std_logic;
            variable input_word0:   std_logic_vector(3 downto 0);
            variable input_word1:   std_logic_vector(3 downto 0);
            variable expected_final: std_logic;
            variable expected_word:  std_logic_vector(3 downto 0);

            variable space: character;
            variable available: boolean;
            variable output_count: integer := 0;
        begin
            file_open(input_buf0, "tb/data/or_in_a.txt", read_mode);
            file_open(input_buf1, "tb/data/or_in_b.txt", read_mode);
            file_open(output_buf, "tb/data/or_out.txt", read_mode);

            -- first perform full reset
            outer_reset <= '0';
            outer_clk <= '0';
            wait for 1 ns;
            outer_clk <= '1';
            wait for 1 ns;
            outer_reset <= '1';

            report "beginning tests";

            while not outer_final_out loop
                if (outer_full0 = '0') and (not endfile(input_buf0)) then
                    -- read input
                    readline(input_buf0, read_col_from_input_buf0);
                    read(read_col_from_input_buf0, input_final0);
                    read(read_col_from_input_buf0, space);
                    read(read_col_from_input_buf0, input_word0);

                    -- assign inputs
                    outer_wr_en0 <= '1';
                    outer_input0 <= input_word0;
                    outer_final_in0 <= input_final0;
                    outer_reset <= '1';
                else
                    outer_wr_en0 <= '0';
                    outer_final_in0 <= '0';
                    outer_reset <= '1';
                end if;

                if (outer_full1 = '0') and (not endfile(input_buf1)) then
                    -- read input
                    readline(input_buf1, read_col_from_input_buf1);
                    read(read_col_from_input_buf1, input_final1);
                    read(read_col_from_input_buf1, space);
                    read(read_col_from_input_buf1, input_word1);

                    -- assign inputs
                    outer_wr_en1 <= '1';
                    outer_input1 <= input_word1;
                    outer_final_in1 <= input_final1;
                    outer_reset <= '1';
                else
                    outer_wr_en1 <= '0';
                    outer_final_in1 <= '0';
                    outer_reset <= '1';
                end if;

                available := outer_empty = '0';
                if available then
                    outer_rd_en <= '1';
                    output_count := output_count + 1;
                end if;

                outer_clk <= '0';
                wait for 1 ns;
                outer_clk <= '1';
                wait for 1 ns;

                -- check outputs
                if available then
                    assert not endfile(output_buf)
                    report "output has more words than output file has lines" severity error;

                    readline(output_buf, read_col_from_output_buf);
                    read(read_col_from_output_buf, expected_final);
                    read(read_col_from_output_buf, space);
                    read(read_col_from_output_buf, expected_word);

                    assert outer_output = expected_word
                    report "bad encoding in test " & integer'image(output_count) & ". Expected: " & to_string(expected_word) & " but found " & to_string(outer_output) severity error;

                    assert outer_final_out = expected_final
                    report "bad final state in test " & integer'image(output_count) & ". Expected: " & std_logic'image(expected_final) & " but found " & std_logic'image(outer_final_out) severity error;
                end if;
            end loop;

            assert endfile(output_buf)
            report "output has fewer words than output file has lines" severity error;

            if endfile(output_buf) then
                report "all tests done";
            end if;

            wait for 10 ns;

            file_close(input_buf0);
            file_close(input_buf1);
            file_close(output_buf);
            wait;
        end process;
    end behav;
