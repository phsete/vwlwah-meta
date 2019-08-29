library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

--  A testbench has no ports.
entity fifo_b32_big is
    end fifo_b32_big;

architecture behav of fifo_b32_big is

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
                    constant word_size: natural := 9
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
    signal outer_input:    std_logic_vector(8 downto 0);
    signal outer_wr_en:    std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(31 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full:     std_logic;
    signal outer_reset:    std_logic;

    file input_buf: text;
    file output_buf: text;

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
        --  This process does the real job.
        -- see: https://vhdlguide.readthedocs.io/en/latest/vhdl/testbench.html
        process
            variable read_col_from_input_buf:  line;
            variable read_col_from_output_buf: line;

            variable input_final:  std_logic;
            variable input_word:   std_logic_vector(8 downto 0);
            variable expected_final: std_logic;
            variable expected_word:  std_logic_vector(31 downto 0);

            variable space: character;
            variable available: boolean;
            variable output_count: integer := 0;
        begin
            file_open(input_buf, "tb/data/fifo_b32_in.txt", read_mode);
            file_open(output_buf, "tb/data/fifo_b32_out.txt", read_mode);

            -- first perform full reset
            outer_reset <= '0';
            outer_clk <= '0';
            wait for 1 ns;
            outer_clk <= '1';
            wait for 1 ns;
            outer_reset <= '1';

            report "beginning tests";

            while not final_out loop
                if (outer_full = '0') and (not endfile(input_buf)) then
                    -- read input
                    readline(input_buf, read_col_from_input_buf);
                    read(read_col_from_input_buf, input_final);
                    read(read_col_from_input_buf, space);
                    read(read_col_from_input_buf, input_word);

                    -- assign inputs
                    outer_wr_en <= '1';
                    outer_input <= input_word;
                    final_in <= input_final;
                    outer_reset <= '1';
                else
                    outer_wr_en <= '0';
                    final_in <= '0';
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

                    assert final_out = expected_final
                    report "bad final state in test " & integer'image(output_count) & ". Expected: " & std_logic'image(expected_final) & " but found " & std_logic'image(final_out) severity error;
                end if;
            end loop;

            assert endfile(output_buf)
            report "output has fewer words than output file has lines" severity error;

            if endfile(output_buf) then
                report "all tests done";
            end if;

            wait for 10 ns;

            file_close(input_buf);
            file_close(output_buf);
            wait;
        end process;
    end behav;
