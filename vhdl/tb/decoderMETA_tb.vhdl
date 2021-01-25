-- based on testbench example from https://vhdlguide.readthedocs.io/en/latest/vhdl/testbench.html
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

--  A testbench has no ports.
entity decoderMETA_tb is
    end decoderMETA_tb;

architecture behav of decoderMETA_tb is

    constant general_word_size : natural := 20;

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
        Generic (
                    constant word_size: natural := general_word_size;
                    constant fill_counter_size: natural := 32
                );
        Port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(word_size-1 downto 0);
                 in_empty:      in std_logic;
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(word_size-1 downto 0);
                 in_rd:         out std_logic;
                 out_wr:        out std_logic;
                 final_in:      in std_logic;
                 final_out:     out std_logic;
                 reset:         in std_logic
             );
    end component;

    component decoderMETA
        Generic (
                    constant word_size: natural := general_word_size;
                    constant fill_counter_size: natural := 32
                );
        Port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(word_size-1 downto 0);
                 in_empty:      in std_logic;
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(word_size-1 downto 0);
                 in_rd:         out std_logic;
                 out_wr:        out std_logic;
                 final_in:      in std_logic;
                 final_out:     out std_logic;
                 reset:         in std_logic
             );
    end component;

    component input_fifo
        Generic (
                    constant addr_width: natural := 3;
                    constant word_size: natural := general_word_size
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

    component mid_fifo
        Generic (
                    constant addr_width: natural := 3;
                    constant word_size: natural := general_word_size
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
                    constant word_size: natural := general_word_size
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
    for decoderMETA_0: decoderMETA use entity work.decoderMETA;
    for mid_fifo_0: mid_fifo use entity work.FIFO_bb;
    for output_fifo_0: output_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk_in:          std_logic_vector(general_word_size-1 downto 0);
    signal blk_out:         std_logic_vector(general_word_size-1 downto 0);
    signal blk_in_meta:     std_logic_vector(general_word_size-1 downto 0);
    signal blk_out_meta:    std_logic_vector(general_word_size-1 downto 0);
    signal in_empty:        std_logic;
    signal in_empty_meta:   std_logic;
    signal out_full:        std_logic;
    signal in_rd:           std_logic;
    signal out_wr:          std_logic;
    signal out_full_meta:   std_logic;
    signal in_rd_meta:      std_logic;
    signal out_wr_meta:     std_logic;
    signal final_in:        std_logic;
    signal final_out:       std_logic;
    signal final_in_meta:   std_logic;
    signal final_out_meta:   std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input:    std_logic_vector(general_word_size-1 downto 0);
    signal outer_wr_en:    std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(general_word_size-1 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full:     std_logic;
    signal outer_final_in: std_logic;
    signal outer_final_out:std_logic;
    signal outer_reset:    std_logic;

    file input_buf: text;
    file output_buf: text;

    begin
        --  Component instantiation.

        decoderMETA_0: decoderMETA
        port map (clk => outer_clk,
                blk_in => blk_in_meta,
                blk_out => blk_out_meta,
                in_empty => in_empty_meta,
                out_full => out_full_meta,
                in_rd => in_rd_meta,
                final_in => final_in_meta,
                final_out => final_out_meta,
                reset => outer_reset,
                out_wr => out_wr_meta);

        mid_fifo_0: mid_fifo
        port map (CLK => outer_clk,
                BLK_IN => outer_input,
                WR_EN => outer_wr_en,
                BLK_OUT => blk_in_meta,
                RD_EN => in_rd_meta,
                EMPTY => in_empty_meta,
                FINAL_IN => outer_final_in,
                FINAL_OUT => final_in_meta,
                RESET => outer_reset,
                FULL => outer_full);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  BLK_IN => blk_out_meta,
                  WR_EN => out_wr_meta,
                  BLK_OUT => outer_output,
                  RD_EN => outer_rd_en,
                  EMPTY => outer_empty,
                  FINAL_IN => final_out_meta,
                  FINAL_OUT => outer_final_out,
                  RESET => outer_reset,
                  FULL => out_full_meta);

        --  This process does the real job.
        process
            variable read_col_from_input_buf:  line;
            variable read_col_from_output_buf: line;

            variable input_final:  std_logic;
            variable input_word:   std_logic_vector(general_word_size-1 downto 0);

            variable space: character;
            variable available: boolean;
        begin
            file_open(input_buf, "tb/data/decoder32_in.txt", read_mode);

            -- first perform full reset
            outer_reset <= '0';
            outer_clk <= '0';
            wait for 1 ns;
            outer_clk <= '1';
            wait for 1 ns;
            outer_reset <= '1';

            report "beginning tests";

            while not outer_final_out loop
                if (outer_full = '0') and (not endfile(input_buf)) then
                    -- read input
                    readline(input_buf, read_col_from_input_buf);
                    read(read_col_from_input_buf, input_final);
                    read(read_col_from_input_buf, space);
                    read(read_col_from_input_buf, input_word);

                    -- assign inputs
                    outer_wr_en <= '1';
                    outer_input <= input_word;
                    outer_final_in <= input_final;
                    outer_reset <= '1';
                else
                    outer_wr_en <= '0';
                    outer_final_in <= '0';
                    outer_reset <= '1';
                end if;

                available := outer_empty = '0';
                if available then
                    outer_rd_en <= '1';
                end if;

                outer_clk <= '0';
                wait for 1 ns;
                outer_clk <= '1';
                wait for 1 ns;
            end loop;

            wait for 10 ns;

            file_close(input_buf);
            wait;
        end process;
    end behav;
