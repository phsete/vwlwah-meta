library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;

entity logic_or is
    Generic (
                word_size:           natural := 5;
                num_inputs:          natural := 2;
                fill_counter_size:   natural := 32
            );
    port (
             clk:           in std_logic;
             blk_in:        std_logic_vector(num_inputs*word_size-1 downto 0);
             in_empty:      in std_logic_vector(0 to num_inputs-1);
             out_full:      in std_logic;
             blk_out:       out std_logic_vector(word_size-1 downto 0);
             in_rd:         out std_logic_vector(0 to num_inputs-1);
             out_wr:        out std_logic;
             final_in:      in std_logic_vector(0 to num_inputs-1);
             final_out:     out std_logic;
             reset:         in std_logic
         );

end logic_or;

architecture IMP of logic_or is

    type Word is (W_NONE, W_0FILL, W_1FILL, W_LITERAL);

    signal output_buffer:        std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:      std_logic_vector(0 to num_inputs-1) := (others => '0');
    signal in_rd_loc:            std_logic_vector(0 to num_inputs-1) := (others => '0');
    signal out_wr_loc:           std_logic;
    signal running:              std_logic := '1';

    type word_array is array(integer range <>) of std_logic_vector(word_size-1 downto 0);
    signal current_word:         word_array(0 to num_inputs-1) := (others => (others => 'U'));
    signal next_word:            word_array(0 to num_inputs-1) := (others => (others => 'U'));

    type length_array is array(integer range <>) of unsigned(fill_counter_size-1 downto 0);
    signal input_length:         length_array(0 to num_inputs-1) := (others => (others => '0'));
    signal consumed_length:      length_array(0 to num_inputs-1) := (others => (others => '0'));
    
    signal output_length:        unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_words_left:    integer := 0;
    signal done_consuming:       std_logic_vector(0 to num_inputs-1) := (others => '1');

    type type_array is array(integer range <>) of Word;
    signal current_type:         type_array(0 to num_inputs-1) := (others => W_NONE);
    signal next_type:            type_array(0 to num_inputs-1) := (others => W_NONE);

    signal next_output_block:    std_logic_vector(word_size-2 downto 0);

    signal final_received:       std_logic_vector(0 to num_inputs-1) := (others => '0');

begin
    process (clk)
        --
        -- writes a literal word to the output buffer
        --
        function emit_literal (content: std_logic_vector(word_size-2 downto 0)) return std_logic_vector is
            variable buf: std_logic_vector(word_size-1 downto 0);
        begin
            -- determine output representation and write word to output buffer
            buf(word_size-1) := '0';
            buf(word_size-2 downto 0) := content(word_size-2 downto 0);
            return buf;
        end emit_literal;

        --
        -- writes a fill word to the output buffer
        --
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

        --
        -- returns the number of fill words needed to represent a fill of size 'length'
        --
        function fill_word_count (length: unsigned) return natural is
            variable n_bit: natural;
        begin
            if length = to_unsigned(0, fill_counter_size) then
                return 0;
            end if;
            for i in fill_counter_size-1 downto 0 loop
                if length(i) = '1' then
                    return (i / (word_size-2)) + 1;
                end if;
            end loop;
            return 1;
        end fill_word_count;

        --
        -- add value of new fill word to existing fill length
        --
        function parse_fill_length (old_fill_length: unsigned(fill_counter_size-1 downto 0);
                                    fill_word: std_logic_vector(word_size-1 downto 0)) return unsigned is
            variable new_fill_length: unsigned(fill_counter_size-1 downto 0);
        begin
            new_fill_length := shift_left(old_fill_length, word_size-2);
            for idx in word_size-3 downto 0 loop
                new_fill_length(idx) := fill_word(idx);
            end loop;
            return new_fill_length;
        end parse_fill_length;

        --
        -- determine the word type of input_word by parsing the control bits
        -- (MSB for literals and MSB, MSB-1 for fills)
        --
        function parse_word_type (input_word: std_logic_vector(word_size-1 downto 0)) return Word is
        begin
            if input_word(word_size-1) = '0' then
                return W_LITERAL;
            elsif input_word(word_size-2) = '0' then
                return W_0FILL;
            elsif input_word(word_size-2) = '1' then
                return W_1FILL;
            else
                return W_NONE;
            end if;
        end parse_word_type;

        --
        -- determine the type of input_word by parsing identifying the contents as fill or literal
        --
        function parse_block_type (input_word: std_logic_vector(word_size-2 downto 0)) return Word is
            variable one_fill: boolean := true;
            variable zero_fill: boolean := true;
        begin
            for bit_idx in 0 to word_size-2 loop
                case input_word(bit_idx) is
                    when '0' =>
                        one_fill := false;
                    when '1' =>
                        zero_fill := false;
                    when others =>
                        return W_NONE;
                end case;
            end loop;

            if zero_fill then
                return W_0FILL;
            elsif one_fill then
                return W_1FILL;
            else
                return W_LITERAL;
            end if;
        end parse_block_type;

        --
        -- returns true if none of the inputs are empty
        --
        procedure set_input_available is
        begin
            for input_idx in 0 to num_inputs-1 loop
                if (in_empty(input_idx) = '0') then
                    input_available(input_idx) <= '1';
                else
                    input_available(input_idx) <= '0';
                end if;
            end loop;
        end procedure;

        --
        -- determines if enough input is read in order to start evaluating
        --
        impure function done_reading return boolean is
            variable done: boolean := true;
            variable done_loc: boolean := false;
        begin
            for input_idx in 0 to num_inputs-1 loop
                done_loc := (current_type(input_idx) = W_0FILL or current_type(input_idx) = W_1FILL);
                done_loc := done_loc and (current_type(input_idx) /= next_type(input_idx));
                done_loc := done_loc or (current_type(input_idx) = W_LITERAL);
                done_loc := done_loc or (current_type(input_idx) = W_NONE);
                done := done and done_loc;
            end loop;
            return done;
        end done_reading;

        --
        -- determines if at least one input is fully consumed
        --
        procedure set_done_consuming is
            variable done: boolean := false;
        begin
            for input_idx in 0 to num_inputs-1 loop
                if (input_length(input_idx) <= consumed_length(input_idx)) then
                    done_consuming(input_idx) <= '1';
                else
                    done_consuming(input_idx) <= '0';
                end if;
            end loop;
        end procedure;

        --
        -- decodes the literal representation of a single block
        --
        function decode_single (input_block: std_logic_vector(word_size-1 downto 0)) return std_logic_vector is
            variable decoded_block: std_logic_vector(word_size-2 downto 0) := (others => 'U');
            variable word_type: Word := parse_word_type(input_block);
        begin
            case word_type is
                when W_LITERAL =>
                    decoded_block := input_block(word_size-2 downto 0);
                when W_0FILL =>
                    decoded_block := (others => '0');
                when W_1FILL =>
                    decoded_block := (others => '1');
                when others =>
            end case;
            return decoded_block;
        end decode_single;

        --
        -- implementation of logic or
        --
        function logic_function (in0, in1: std_logic_vector(word_size-2 downto 0)) return std_logic_vector is
        begin
            return in0 or in1;
        end logic_function;

        --
        -- apply logic function to all inputs
        --
        function get_output_block_value (input_words: word_array(0 to num_inputs-1)) return std_logic_vector is
            variable output_block: std_logic_vector(word_size-2 downto 0) := (others => 'U');
        begin
            output_block := decode_single(input_words(0));
            for input_idx in 1 to num_inputs-1 loop
                output_block := logic_function(output_block, decode_single(input_words(input_idx)));
            end loop;
            return output_block;
        end get_output_block_value;
        
        --
        -- convert boolean to std_logic
        --
        function to_std_logic (input: boolean) return std_logic is
        begin
            if input then
                return '1';
            else
                return '0';
            end if;
        end to_std_logic;

        --
        -- selects the length of the shortest input
        --
        impure function consumable_length return unsigned is
            variable minimum_length: unsigned(fill_counter_size-1 downto 0) := (others => '1');
        begin
            for input_idx in 0 to num_inputs-1 loop
                if input_length(input_idx) < minimum_length then
                    minimum_length := input_length(input_idx) - consumed_length(input_idx);
                end if;
            end loop;
            return minimum_length;
        end consumable_length;

        --
        -- checks the final_received value and input buffers to determine final_out state
        --
        impure function is_final return boolean is
            variable ret_value: boolean := false;
        begin
            for input_idx in 0 to num_inputs-1 loop
                ret_value := ret_value or (final_received(input_idx) = '1'
                                           and next_type(input_idx) = W_NONE
                                           and input_length(input_idx) = consumed_length(input_idx));
            end loop;
            ret_value := ret_value and output_words_left <= 1;
            return ret_value;
        end is_final;

        --
        -- read the input of the reset pin and reset all values if it is '1'
        -- TODO: update with new signals
        --
        procedure check_reset is
        begin
            if (reset = '1') then
                output_buffer <= (others => 'U');
                input_available <= (others => '0');
                done_consuming <= (others => '1');
                in_rd_loc <= (others => '0');
                running <= '1';
                current_word <= (others => (others => 'U'));
                next_word <= (others => (others => 'U'));
                input_length <= (others => (others => '0'));
                consumed_length <= (others => (others => '0'));
                output_length <= (others => '0');
                output_words_left <= 0;
                current_type <= (others => W_NONE);
                next_type <= (others => W_NONE);
                final_received <= (others => '0');
            end if;
        end procedure;

        --
        -- adds n to all consumed_length values
        --
        procedure consume (n: unsigned(fill_counter_size-1 downto 0)) is
        begin
            for input_idx in 0 to num_inputs-1 loop
                consumed_length(input_idx) <= consumed_length(input_idx) + n;
            end loop;
        end procedure;

        --
        -- TODO: documentation
        --
        procedure read_input (input_idx: natural) is
            variable new_fill_length: unsigned(fill_counter_size-1 downto 0);
            variable new_read_word:   std_logic_vector(word_size-1 downto 0);
        begin
            if (in_rd_loc(input_idx) = '1') then
                if next_type(input_idx) = W_0FILL or next_type(input_idx) = W_1FILL then
                    --prepare for next fill
                    if (current_type(input_idx) = next_type(input_idx)) then
                        new_fill_length := parse_fill_length(input_length(input_idx), next_word(input_idx));
                    else
                        new_fill_length := parse_fill_length((others => '0') , next_word(input_idx));
                        consumed_length(input_idx) <= (others => '0');
                    end if;
                    input_length(input_idx) <= new_fill_length;
                    if (new_fill_length > consumed_length(input_idx) + 1) then
                        in_rd_loc(input_idx) <= '0';
                    else
                        in_rd_loc(input_idx) <= '1';
                    end if;
                elsif next_type(input_idx) = W_LITERAL then
                    input_length(input_idx) <= to_unsigned(1, fill_counter_size);
                    consumed_length(input_idx) <= (others => '0');
                    in_rd_loc(input_idx) <= to_std_logic(done_reading and output_words_left = 0);
                else
                    input_length(input_idx) <= to_unsigned(0, fill_counter_size);
                    consumed_length(input_idx) <= (others => '0');
                    in_rd_loc(input_idx) <= to_std_logic(done_reading and output_words_left = 0);
                end if;

                -- read the next word and push buffers forward
                current_word(input_idx) <= next_word(input_idx);
                current_type(input_idx) <= next_type(input_idx);
                if (not final_received(input_idx) = '1') then
                    new_read_word := blk_in(((input_idx+1) * word_size) - 1 downto input_idx * word_size);
                    next_word(input_idx) <= new_read_word;
                    next_type(input_idx) <= parse_word_type(new_read_word);
                else
                    next_word(input_idx) <= (others => 'U');
                    next_type(input_idx) <= W_NONE;
                end if;
            else
                -- if no word was read, check fill length to determine next read state
                if (input_length(input_idx) > consumed_length(input_idx) + 1) then
                    in_rd_loc(input_idx) <= '0';
                else
                    in_rd_loc(input_idx) <= '1';
                end if;
            end if;
        end procedure;

    begin
        --
        -- do logic on rising edge of clock signal
        --
        if (clk'event and clk='1' and running = '1') then
            if (done_reading and output_words_left = 0) then
                consume(consumable_length);
                output_length <= consumable_length;
                output_words_left <= fill_word_count(consumable_length); -- does also work for literals
                next_output_block <= get_output_block_value(current_word);
                out_wr_loc <= '0';
            elsif (done_reading and output_words_left > 0) then
                case parse_block_type(next_output_block) is
                    when W_0FILL =>
                        output_buffer <= emit_fill('0', output_length, output_words_left - 1);
                        output_words_left <=  output_words_left- 1;
                        out_wr_loc <= '1';
                    when W_1FILL =>
                        output_buffer <= emit_fill('1', output_length, output_words_left - 1);
                        output_words_left <= output_words_left - 1;
                        out_wr_loc <= '1';
                    when W_LITERAL =>
                        output_buffer <= emit_literal(next_output_block);
                        output_words_left <= output_words_left - 1;
                        out_wr_loc <= '1';
                    when others =>
                        out_wr_loc <= '0';
                end case;
            else
                out_wr_loc <= '0';
            end if;

            final_out <= to_std_logic(is_final);

            check_reset;
        end if;

        --
        -- do I/O on falling edge of clock signal
        --
        if (clk'event and clk='0') then
            for input_idx in 0 to num_inputs-1 loop
                if (in_rd_loc(input_idx) = '1' or (running = '1' and output_words_left = 0)) then
                    if (input_available(input_idx) = '1' or final_received(input_idx) = '1') then
                        read_input(input_idx);
                    end if;
                    if (final_in(input_idx) = '1') then
                        final_received(input_idx) <= '1';
                    end if;
                else
                    in_rd_loc <= (others => '0');
                end if;
            end loop;

            set_done_consuming;
            set_input_available;

            -- ready to write output value
            if (out_wr_loc = '1' and out_full = '0') then
                blk_out <= output_buffer;
            end if;

            -- stop processing if output buffer is full
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
