library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;

entity encoder is
    Generic (
                word_size:           natural := 5;
                fill_counter_size:   natural := 32
            );
    port (
             clk:           in std_logic;
             blk_in:        in std_logic_vector(word_size-2 downto 0);
             in_empty:      in std_logic;
             out_full:      in std_logic;
             blk_out:       out std_logic_vector(word_size-1 downto 0);
             in_rd:         out std_logic;
             out_wr:        out std_logic
         );

end encoder;

architecture IMP of encoder is

    type Word is (W_LITERAL, W_0FILL, W_1FILL, W_0FILL_1FILL, W_1FILL_0FILL, W_0FILL_LITERAL, W_1FILL_LITERAL, W_NONE);

    signal zero_fill_length:    unsigned(fill_counter_size-1 downto 0) := to_unsigned(0, fill_counter_size);
    signal one_fill_length:     unsigned(fill_counter_size-1 downto 0) := to_unsigned(0, fill_counter_size);
    signal num_fill_words:      natural := 0;
    signal input_available:     std_logic := '0';
    signal input_buffer:        std_logic_vector(word_size-2 downto 0);
    signal literal_buffer:      std_logic_vector(word_size-2 downto 0);
    signal output_buffer:       std_logic_vector(word_size-1 downto 0);
    signal out_wr_loc:          std_logic;
    signal running:             std_logic := '1';
    signal buffer_type:         Word := W_NONE;

begin
    process (clk)

        -- writes a literal word to the output buffer
        function emit_literal (content: std_logic_vector(word_size-2 downto 0)) return std_logic_vector is
            variable buf: std_logic_vector(word_size-1 downto 0);
        begin
            -- determine output representation and write word to output buffer
            buf(word_size-1) := '0';
            buf(word_size-2 downto 0) := content;
            return buf;
        end emit_literal;

        -- writes a sequence of fill words to the output buffer
        function emit_fill (fill_type: std_logic; length: unsigned; word_no: natural) return std_logic_vector is
            variable length_vector: std_logic_vector(fill_counter_size-1 downto 0);
            variable lowest_bit_idx: natural;
            variable buf: std_logic_vector(word_size-1 downto 0);
        begin
            length_vector := std_logic_vector(length);
            lowest_bit_idx := word_no * 3;
            if length_vector(fill_counter_size-1 downto lowest_bit_idx) /= (fill_counter_size-1 downto lowest_bit_idx => '0') then
                buf(word_size-1)          := '1';
                buf(word_size-2)          := fill_type;
                buf(word_size-3 downto 0) := length_vector(lowest_bit_idx + (word_size-3) downto lowest_bit_idx);
            end if;
            return buf;
        end emit_fill;

        function fill_word_count (length: unsigned) return natural is
            variable n_bit: natural;
        begin
            if length = to_unsigned(0, fill_counter_size) then
                return 0;
            end if;
            for i in fill_counter_size-1 downto 0 loop
                if length(i) = '1' then
                    return i / (word_size-2);
                end if;
            end loop;  -- i
            return 1;
        end fill_word_count;

        procedure handle_0F_1F is
        begin
            output_buffer <= emit_fill('0', zero_fill_length, num_fill_words);
            if (num_fill_words > 0) then
                buffer_type <= W_0FILL_1FILL;
                num_fill_words <= num_fill_words - 1;
                out_wr_loc <= '1';
            else
                buffer_type <= W_NONE;
                zero_fill_length <= to_unsigned(0, fill_counter_size); num_fill_words <= 0;
                out_wr_loc <= '1';

                -- output done, finally start new one fill
                one_fill_length <= one_fill_length + 1;
                num_fill_words <= fill_word_count(one_fill_length);
            end if;
        end procedure;

        procedure handle_1F_0F is
        begin
            output_buffer <= emit_fill('1', zero_fill_length, num_fill_words);
            if (num_fill_words > 0) then
                buffer_type <= W_1FILL_0FILL;
                num_fill_words <= num_fill_words - 1;
                out_wr_loc <= '1';
            else
                buffer_type <= W_NONE;
                one_fill_length <= to_unsigned(0, fill_counter_size);
                num_fill_words <= 0;
                out_wr_loc <= '1';

                -- output done, finally start new zero fill
                zero_fill_length <= zero_fill_length + 1;
                num_fill_words <= fill_word_count(zero_fill_length);
            end if;
        end procedure;

        procedure handle_0F_L is
        begin
            output_buffer <= emit_fill('0', zero_fill_length, num_fill_words);
            if (num_fill_words > 0) then
                buffer_type <= W_0FILL_LITERAL;
                -- backup input for later use
                literal_buffer <= input_buffer;
                num_fill_words <= num_fill_words - 1;
                out_wr_loc <= '1';
            else
                buffer_type <= W_LITERAL;
                -- backup input for later use
                literal_buffer <= input_buffer;
                zero_fill_length <= to_unsigned(0, fill_counter_size);
                num_fill_words <= 0;
                out_wr_loc <= '1';
            end if;
        end procedure;

        procedure handle_1F_L is
        begin
            output_buffer <= emit_fill('1', one_fill_length, num_fill_words);
            if (num_fill_words > 0) then
                buffer_type <= W_1FILL_LITERAL;
                -- backup input for later use
                literal_buffer <= input_buffer;
                num_fill_words <= num_fill_words - 1;
                out_wr_loc <= '1';
            else
                buffer_type <= W_LITERAL;
                -- backup input for later use
                literal_buffer <= input_buffer;
                one_fill_length <= to_unsigned(0, fill_counter_size);
                num_fill_words <= 0;
                out_wr_loc <= '1';
            end if;
        end procedure;

        procedure handle_0F is
        begin
            output_buffer <= emit_fill('0', zero_fill_length, num_fill_words);
            if (num_fill_words > 0) then
                buffer_type <= W_0FILL;
                num_fill_words <= num_fill_words - 1;
                out_wr_loc <= '1';
            else
                buffer_type <= W_NONE;
                zero_fill_length <= to_unsigned(0, fill_counter_size);
                num_fill_words <= 0;
                out_wr_loc <= '1';
            end if;
        end procedure;

        procedure handle_1F is
        begin
            output_buffer <= emit_fill('0', one_fill_length, num_fill_words);
            if (num_fill_words > 0) then
                buffer_type <= W_1FILL;
                num_fill_words <= num_fill_words - 1;
                out_wr_loc <= '1';
            else
                buffer_type <= W_NONE;
                one_fill_length <= to_unsigned(0, fill_counter_size);
                num_fill_words <= 0;
                out_wr_loc <= '1';
            end if;
        end procedure;

    begin
        if (clk'event and clk='1' and running = '1') then

            out_wr_loc <= '0';

            case buffer_type is
                when W_LITERAL =>
                    output_buffer <= emit_literal(literal_buffer);
                    buffer_type <= W_NONE;
                    out_wr_loc <= '1';
                when W_0FILL_1FILL =>
                    handle_0F_1F;
                when W_1FILL_0FILL =>
                    handle_1F_0F;
                when W_0FILL_LITERAL =>
                    handle_0F_L;
                when W_1FILL_LITERAL =>
                    handle_1F_L;
                when W_0FILL =>
                    handle_0F;
                when W_1FILL =>
                    handle_1F;
                when W_NONE =>
                    if (input_available = '1') then
                        -- ready to read input value
                        if input_buffer = (word_size-2 downto 0 => '0') then
                            -- input is zero fill, emit previously started one fill first
                            if (one_fill_length /= to_unsigned(0, fill_counter_size)) then
                                handle_1F_0F;
                            else
                                -- no output yet, count further
                                num_fill_words <= fill_word_count(zero_fill_length + 1);
                                zero_fill_length <= zero_fill_length + 1;
                            end if;
                        elsif input_buffer = (word_size-2 downto 0 => '1') then
                                -- input is one fill, emit previously started zero fill first
                            if (zero_fill_length /= to_unsigned(0, fill_counter_size)) then
                                handle_0F_1F;
                            else
                                    -- no output yet, count further
                                num_fill_words <= fill_word_count(one_fill_length + 1);
                                one_fill_length <= one_fill_length + 1;
                            end if;
                        else
                                -- input is literal word, emit previously started fill words first
                            if (zero_fill_length /= to_unsigned(0, fill_counter_size)) then
                                handle_0F_L;
                            elsif (one_fill_length /= to_unsigned(0, fill_counter_size)) then
                                handle_1F_L;
                            else
                                output_buffer <= emit_literal(input_buffer);
                                buffer_type <= W_NONE;
                                out_wr_loc <= '1';
                            end if;
                        end if;
                    else
                        if (zero_fill_length /= to_unsigned(0, fill_counter_size)) then
                            handle_0F;
                        elsif (one_fill_length /= to_unsigned(0, fill_counter_size)) then
                            handle_1F;
                        end if;
                    end if;
            end case;

            if (buffer_type = W_NONE and in_empty = '0') then
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

    in_rd  <= '1' when buffer_type = W_NONE else '0';
    out_wr <= out_wr_loc;

end IMP;
