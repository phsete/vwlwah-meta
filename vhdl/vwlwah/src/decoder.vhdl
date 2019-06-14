library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;

entity decoder is
    Generic (
                word_size:           natural := 5;
                fill_counter_size:   natural := 32
            );
    port (
             clk:           in std_logic;
             blk_in:        in std_logic_vector(word_size-1 downto 0);
             in_empty:      in std_logic;
             out_full:      in std_logic;
             blk_out:       out std_logic_vector(word_size-2 downto 0);
             in_rd:         out std_logic;
             out_wr:        out std_logic
         );

end decoder;

architecture IMP of decoder is

    type Word is (W_NONE, W_0FILL, W_1FILL, W_LITERAL);

    signal input_fill_length:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal current_word_buffer: std_logic_vector(word_size-1 downto 0);
    signal next_word_buffer:    std_logic_vector(word_size-1 downto 0);
    signal output_buffer:       std_logic_vector(word_size-2 downto 0);
    signal ready_to_read:       std_logic;
    signal in_rd_loc:           std_logic;
    signal out_wr_loc:          std_logic;
    signal running:             std_logic := '1';
    signal current_type:        Word := W_NONE;
    signal next_type:           Word := W_NONE;

begin
    process (clk)

        -- writes a literal word to the output buffer
        function emit_literal (content: std_logic_vector(word_size-1 downto 0)) return std_logic_vector is
            variable buf: std_logic_vector(word_size-2 downto 0);
        begin
            -- determine output representation and write word to output buffer
            buf(word_size-2 downto 0) := content(word_size-2 downto 0);
            return buf;
        end emit_literal;

        -- writes a fill word to the output buffer
        function emit_fill (fill_type: std_logic) return std_logic_vector is
            variable buf: std_logic_vector(word_size-2 downto 0);
        begin
            for idx in word_size-2 downto 0 loop
                buf(idx)    := fill_type;
            end loop;
            return buf;
        end emit_fill;

        function parse_fill_length (old_fill_length: unsigned(fill_counter_size-1 downto 0); fill_word: std_logic_vector(word_size-1 downto 0)) return unsigned is
            variable new_fill_length: unsigned(fill_counter_size-1 downto 0);
        begin
            new_fill_length := shift_left(old_fill_length, word_size-2);
            for idx in word_size-3 downto 0 loop
                new_fill_length(idx) := fill_word(idx);
            end loop;
            return new_fill_length;
        end parse_fill_length;

        function parse_word_type (input_word: std_logic_vector(word_size-1 downto 0)) return Word is
        begin
            if input_word(word_size-1) = '0' then
                return W_LITERAL;
            elsif input_word(word_size-2) = '0' then
                return W_0FILL;
            else
                return W_1FILL;
            end if;
        end parse_word_type;

    begin
        if (clk'event and clk='1' and running = '1') then

            case current_type is
                when W_0FILL =>
                    out_wr_loc <= '1';
                    if (next_type = W_0FILL) then
                        output_buffer <= emit_fill('0');
                        output_fill_length <= output_fill_length + 1;
                        ready_to_read <= '1';
                    else
                        if (parse_fill_length(input_fill_length, current_word_buffer) - output_fill_length > 1) then
                            output_buffer <= emit_fill('0');
                            output_fill_length <= output_fill_length + 1;
                            ready_to_read <= '0'; -- wait with next read until emission of this fill is completed
                        elsif (parse_fill_length(input_fill_length, current_word_buffer) - output_fill_length = 1) then
                            output_buffer <= emit_fill('0');
                            input_fill_length <= (others => '0');
                            output_fill_length <= (others => '0');
                            ready_to_read <= '1';
                        end if;
                    end if;
                    input_fill_length <= parse_fill_length(input_fill_length, current_word_buffer);

                when W_1FILL =>
                    out_wr_loc <= '1';
                    if (next_type = W_1FILL) then
                        output_buffer <= emit_fill('1');
                        output_fill_length <= output_fill_length + 1;
                        ready_to_read <= '1';
                    else
                        if (parse_fill_length(input_fill_length, current_word_buffer) - output_fill_length > 1) then
                            output_buffer <= emit_fill('1');
                            output_fill_length <= output_fill_length + 1;
                            ready_to_read <= '0'; -- wait with next read until emission of this fill is completed
                        elsif (parse_fill_length(input_fill_length, current_word_buffer) - output_fill_length = 1) then
                            output_buffer <= emit_fill('1');
                            input_fill_length <= (others => '0');
                            output_fill_length <= (others => '0');
                            ready_to_read <= '1';
                        end if;
                    end if;
                    input_fill_length <= parse_fill_length(input_fill_length, current_word_buffer);

                when W_LITERAL =>
                    input_fill_length <= (others => '0');
                    output_fill_length <= (others => '0');
                    output_buffer <= emit_literal(current_word_buffer);
                    out_wr_loc <= '1';
                    ready_to_read <= '1';
                when others =>
                    out_wr_loc <= '0';
                    ready_to_read <= '1';
            end case;
        end if;

        if (ready_to_read = '1' and in_empty = '0') then
            in_rd_loc <= '1';
        else
            in_rd_loc <= '0';
        end if;

        if (clk'event and clk='0') then

            if (in_rd_loc = '1') then
                current_word_buffer <= next_word_buffer;
                current_type <= next_type;
                if (in_empty = '0') then
                    next_word_buffer <= blk_in;
                    next_type <= parse_word_type(blk_in);
                else
                    next_word_buffer <= (others => 'U');
                    next_type <= W_NONE;
                end if;
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
