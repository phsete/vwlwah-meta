library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity encoder is
    -- THIS IS HOW IT WORKS WITH GENERICS: generic (WORD_SIZE: integer);
    -- blk_in: next block of size w-1
    -- blk_out: next block of size w-1
    port (
             clk:           in std_logic;
             blk_in:        in std_logic_vector(3 downto 0);
             in_empty:      in std_logic;
             out_full:      in std_logic;
             blk_out:       out std_logic_vector(4 downto 0);
             in_rd:         out std_logic;
             out_wr:        out std_logic
         );

end encoder;

architecture IMP of encoder is

    signal zero_fill_length:    unsigned(31 downto 0) := to_unsigned(0, 32);
    signal one_fill_length:     unsigned(31 downto 0) := to_unsigned(0, 32);
    signal input_available:     std_logic := '0';
    signal input_buffer:        std_logic_vector(3 downto 0);
    signal literal_buffer:      std_logic_vector(3 downto 0);
    signal output_buffer:       std_logic_vector(4 downto 0);
    signal in_rd_loc:           std_logic := '1';
    signal out_wr_loc:          std_logic;
    signal running:             std_logic := '1';

begin
    process (clk)

        -- writes a literal word to the output buffer
        function emit_literal (content: std_logic_vector(3 downto 0)) return std_logic_vector is
            variable buf: std_logic_vector(4 downto 0) := "UUUUU";
        begin
            -- determine output representation and write word to output buffer
            buf(4) := '0';
            buf(3 downto 0) := content;
            return buf;
        end emit_literal;

        -- writes a sequence of fill words to the output buffer
        function emit_fill (fill_type: std_logic; length: unsigned; word_no: natural) return std_logic_vector is
                             variable length_vector: std_logic_vector(31 downto 0);
                             variable lowest_bit_idx: natural;
                             variable buf: std_logic_vector(4 downto 0) := "UUUUU";
        begin
            length_vector := std_logic_vector(length);
            lowest_bit_idx := word_no * 3;
            if length_vector(31 downto lowest_bit_idx) /= (31 downto lowest_bit_idx => '0') then
                buf(4)          := '1';
                buf(3)          := fill_type;
                buf(2 downto 0) := length_vector(lowest_bit_idx + 2 downto lowest_bit_idx);
            end if;
            return buf;
        end emit_fill;

    begin
        if (clk'event and clk='1') then

            out_wr_loc <= '0';

            if (running = '1' and in_rd_loc = '0') then                 -- emit old literal from last cycle
                output_buffer <= emit_literal(literal_buffer);
                in_rd_loc <= '1';
                out_wr_loc <= '1';
            elsif (running = '1' and input_available = '1') then        -- ready to read input value
                if (input_buffer = "0000") then                         -- input is zero fill, emit previously started one fill first
                    if (one_fill_length /= to_unsigned(0, 32)) then
                        output_buffer <= emit_fill('1', one_fill_length, 0);
                        one_fill_length <= to_unsigned(0, 32);
                        in_rd_loc <= '1';
                        out_wr_loc <= '1';
                    end if;

                    zero_fill_length <= zero_fill_length + 1;
                elsif (input_buffer = "1111") then                      -- input is one fill, emit previously started zero fill first
                    if (zero_fill_length /= to_unsigned(0, 32)) then
                        output_buffer <= emit_fill('0', zero_fill_length, 0);
                        zero_fill_length <= to_unsigned(0, 32);
                        in_rd_loc <= '1';
                        out_wr_loc <= '1';
                    end if;

                    one_fill_length <= one_fill_length + 1;
                else                                                    -- input is literal word, emit previously started fill words first
                    if (zero_fill_length /= to_unsigned(0, 32)) then
                        output_buffer <= emit_fill('0', zero_fill_length, 0);
                        zero_fill_length <= to_unsigned(0, 32);
                        literal_buffer <= input_buffer;                 -- backup input for later use
                        in_rd_loc <= '0';
                        out_wr_loc <= '1';
                    elsif (one_fill_length /= to_unsigned(0, 32)) then
                        output_buffer <= emit_fill('1', one_fill_length, 0);
                        one_fill_length <= to_unsigned(0, 32);
                        literal_buffer <= input_buffer;                 -- backup input for later use
                        in_rd_loc <= '0';
                        out_wr_loc <= '1';
                    else
                        output_buffer <= emit_literal(input_buffer);
                        in_rd_loc <= '1';
                        out_wr_loc <= '1';
                    end if;
                end if;
            elsif (running = '1' and input_available = '0') then
                if (zero_fill_length /= to_unsigned(0, 32)) then
                    output_buffer <= emit_fill('0', zero_fill_length, 0);
                    zero_fill_length <= to_unsigned(0, 32);
                    in_rd_loc <= '1';
                    out_wr_loc <= '1';
                elsif (one_fill_length /= to_unsigned(0, 32)) then
                    output_buffer <= emit_fill('1', one_fill_length, 0);
                    one_fill_length <= to_unsigned(0, 32);
                    in_rd_loc <= '1';
                    out_wr_loc <= '1';
                end if;
            end if;
            
            if (in_rd_loc = '1' and in_empty = '0') then
                input_available <= '1';
            else
                input_available <= '0';
            end if;

        end if;

        if (clk'event and clk='0') then
            if (input_available = '1') then
                input_buffer <= blk_in;
            end if;
            
            if (out_wr_loc = '1' and out_full = '0') then               -- ready to write output value
                blk_out <= output_buffer;
            end if;

            if (out_full = '0') then
                running <= '1';
            else
                running <= '0';
            end if;

        end if;

    end process;

    in_rd  <= in_rd_loc;
    out_wr <= out_wr_loc;

end IMP;
