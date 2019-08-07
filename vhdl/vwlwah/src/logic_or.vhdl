library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
use work.utils.all;

entity logic_or is
    Generic (
        word_size:              natural := 5;
        num_inputs:             natural := 2;
        fill_counter_size:      natural := 32
    );
    port (
        CLK:                in  std_logic;
        RESET:              in  std_logic;
        IN_EMPTY:           in  std_logic_vector(0 to num_inputs-1);
        FINAL_IN:           in  std_logic_vector(0 to num_inputs-1);
        BLK_IN:             in  std_logic_vector(num_inputs*word_size-1 downto 0);
        OUT_FULL:           in  std_logic;
        OUT_WR:             out std_logic;
        BLK_OUT:            out std_logic_vector(word_size-1 downto 0);
        IN_RD:              out std_logic_vector(0 to num_inputs-1);
        FINAL_OUT:          out std_logic
    );
end logic_or;

architecture IMP of logic_or is

    signal output_buffer:        std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:      std_logic_vector(0 to num_inputs-1)    := (others => '0');
    signal in_rd_loc:            std_logic_vector(0 to num_inputs-1)    := (others => '0');
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

    signal current_output_block: std_logic_vector(word_size-2 downto 0);
    signal next_output_block:    std_logic_vector(word_size-2 downto 0);

    signal final_received:       std_logic_vector(0 to num_inputs-1) := (others => '0');

    signal continue_last_output: boolean := true;

begin
    process (CLK)

        --
        -- returns the decoded block value of input no "input_idx" at offset "offset"
        --
        impure function get_input_block_at (input_idx: natural; offset: unsigned)
        return std_logic_vector is
        begin
            if (offset < input_length(input_idx) - consumed_length(input_idx)) then
                    -- offset points to the current word
                return decode_single(word_size, current_word(input_idx));
            else
                    -- offset is too large, use the next word
                return decode_single(word_size, next_word(input_idx));
            end if;
        end get_input_block_at;

        --
        -- apply logic function to all inputs
        --
        impure function get_output_block_value (offset: unsigned)
        return std_logic_vector is
            variable output_block: std_logic_vector(word_size-2 downto 0) := (others => 'U');
        begin
            output_block := get_input_block_at(0, offset);
            for input_idx in 1 to num_inputs-1 loop
                -- select the input block by using the given offset
                -- use the logic function to map all inputs and obtain a single output
                output_block := logic_function(output_block, get_input_block_at(input_idx, offset));
            end loop;
            return output_block;
        end get_output_block_value;

        ----------------------
        -- IMPURE FUNCTIONS --
        ----------------------

        --
        -- determines if enough input is read in order to start evaluating
        --
        impure function done_reading
        return boolean is
            variable done: boolean := true;
            variable done_loc: boolean := false;
        begin
            for input_idx in 0 to num_inputs-1 loop
                -- reading is done, when the current type is either a literal or none or if the
                -- current type is a fill that does not continue in the next word
                done_loc := (current_type(input_idx) = W_0FILL or current_type(input_idx) = W_1FILL);
                done_loc := done_loc and (current_type(input_idx) /= next_type(input_idx));
                done_loc := done_loc or (current_type(input_idx) = W_LITERAL);
                done_loc := done_loc or (current_type(input_idx) = W_NONE and final_received(input_idx) = '1');
                -- aggregate local reading states
                done := done and done_loc;
            end loop;
            return done;
        end done_reading;

        --
        -- returns the length of the shortest input
        --
        impure function consumable_length
        return unsigned is
            variable minimum_length: unsigned(fill_counter_size-1 downto 0) := (others => '1');
        begin
            for input_idx in 0 to num_inputs-1 loop
                if input_length(input_idx) - consumed_length(input_idx) < minimum_length then
                    minimum_length := input_length(input_idx) - consumed_length(input_idx);
                end if;
            end loop;
            return minimum_length;
        end consumable_length;

        --
        -- checks the final_received value and input buffers to determine FINAL_OUT state
        --
        impure function is_final
        return boolean is
            variable ret_value: boolean := false;
        begin
            for input_idx in 0 to num_inputs-1 loop
                -- an input is final if it's final signal has been received and there is neither
                -- a next word nor consumable length in the current word left
                ret_value := ret_value or (final_received(input_idx) = '1'
                                           and next_type(input_idx) = W_NONE
                                           and input_length(input_idx) = consumed_length(input_idx));
            end loop;
            return ret_value;
        end is_final;

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- sets input_available to true for all non-empty inputs
        --
        procedure set_input_available is
        begin
            for input_idx in 0 to num_inputs-1 loop
                if (IN_EMPTY(input_idx) = '0') then
                    input_available(input_idx) <= '1';
                else
                    input_available(input_idx) <= '0';
                end if;
            end loop;
        end procedure;

        --
        -- sets done_consuming to true for all fully consumed current words
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
        -- read the input of the RESET pin and resets all values if it is '1'
        --
        procedure check_RESET is
        begin
            if (RESET = '0') then
                output_buffer         <= (others => 'U');
                input_available       <= (others => '0');
                in_rd_loc             <= (others => '0');
                out_wr_loc            <= '0';
                running               <= '1';

                current_word          <= (others => (others => 'U'));
                next_word             <= (others => (others => 'U'));

                input_length          <= (others => (others => '0'));
                consumed_length       <= (others => (others => '0'));
                
                output_length         <= (others => '0');
                output_words_left     <= 0;
                done_consuming        <= (others => '1');

                current_type          <= (others => W_NONE);
                next_type             <= (others => W_NONE);

                final_received        <= (others => '0');

                continue_last_output  <= true;
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
        -- prepare the next word to become the current word and read a new next word
        --
        procedure read_input (input_idx: natural) is
            variable new_fill_length: unsigned(fill_counter_size-1 downto 0);
            variable new_read_word:   std_logic_vector(word_size-1 downto 0) := BLK_IN(((input_idx+1) * word_size) - 1 downto input_idx * word_size);
        begin
            if (in_rd_loc(input_idx) = '1') then
                -- an input word is ready to be read into the next_word buffer
                -- --> handle current next_word buffer content first as it will become the current word now
                if next_type(input_idx) = W_0FILL or next_type(input_idx) = W_1FILL then
                    --prepare for next fill
                    if (current_type(input_idx) = next_type(input_idx)) then
                        -- an old fill is extended
                        new_fill_length := parse_fill_length(word_size, fill_counter_size, input_length(input_idx), next_word(input_idx));
                    else
                        -- a new fill begins
                        new_fill_length := parse_fill_length(word_size, fill_counter_size, to_unsigned(0, fill_counter_size), next_word(input_idx));
                        consumed_length(input_idx) <= (others => '0');
                    end if;

                    input_length(input_idx) <= new_fill_length;

                    if ((consumed_length(input_idx) = new_fill_length) or (parse_word_type(word_size, new_read_word) = next_type(input_idx))) then
                        -- if all output is done, continue reading to see whether or not the fill needs to be extended
                        in_rd_loc(input_idx) <= '1';
                    else
                        in_rd_loc(input_idx) <= '0';
                    end if;
                elsif next_type(input_idx) = W_LITERAL then
                        -- prepare to handle a literal word
                    input_length(input_idx) <= to_unsigned(1, fill_counter_size);
                    consumed_length(input_idx) <= (others => '0');
                        -- read further once all reading and writing is done
                    in_rd_loc(input_idx) <= to_std_logic(done_reading and continue_last_output);
                else
                    if final_received(input_idx) = '0' then
                        -- word type is unknown --> set everything to 0
                        input_length(input_idx) <= to_unsigned(0, fill_counter_size);
                        consumed_length(input_idx) <= (others => '0');
                        -- read further once all reading and writing is done
                        in_rd_loc(input_idx) <= to_std_logic(done_reading and continue_last_output);
                    end if;
                end if;

                -- read the next word and push buffers forward
                current_word(input_idx) <= next_word(input_idx);
                current_type(input_idx) <= next_type(input_idx);
                if (final_received(input_idx) = '0') then
                    -- not final, read the respective input block
                    next_word(input_idx) <= new_read_word;
                    next_type(input_idx) <= parse_word_type(word_size, new_read_word);
                else
                    -- final buffer value is still needed
                    -- --> keep next_word value but set it as invalid
                    next_type(input_idx) <= W_NONE;
                end if;
            else
                -- all output is done but no word was read --> read in next cycle
                in_rd_loc(input_idx) <= '1';
            end if;
        end procedure;

        ---------------
        -- VARIABLES --
        ---------------

        variable output_length_var: unsigned(fill_counter_size-1 downto 0) := (others => '0');
        variable output_words_left_var: integer := 0;
        variable continue_last_output_var: boolean := true;
        variable current_output_block_var: std_logic_vector(word_size-2 downto 0) := (others => 'U');
        variable next_output_block_var: std_logic_vector(word_size-2 downto 0) := (others => 'U');

    begin
        --
        -- do logic and prepare output on rising edge of clock signal
        --
        if (CLK'event and CLK = '1' and running = '1') then
            -- initialize variables
            output_length_var := output_length;
            output_words_left_var := output_words_left;
            continue_last_output_var := continue_last_output;
            current_output_block_var := current_output_block;
            next_output_block_var := next_output_block;

            -- TODO: reverse meaning of continue_last_output
            if (done_reading and continue_last_output and not is_final) then
                -- begin a new output or continue an existing one
                consume(consumable_length);
                output_length_var := output_length + consumable_length;
                output_words_left_var := fill_words_needed(word_size, fill_counter_size, output_length_var); -- does also work for literals
                current_output_block_var := get_output_block_value(to_unsigned(0, fill_counter_size));
                next_output_block_var := get_output_block_value(consumable_length);

                -- extend this output if it is a fill of the same type as the following one
                continue_last_output_var := (get_output_block_value(to_unsigned(0, fill_counter_size))
                                        = get_output_block_value(consumable_length))
                                        and parse_block_type(word_size, next_output_block) /= W_LITERAL;

                out_wr_loc <= '0';
            elsif (done_reading and output_words_left_var > 0) then
                -- start emitting the previously determined output
                case parse_block_type(word_size, current_output_block_var) is
                    when W_0FILL =>
                        output_buffer <= encode_fill(word_size, fill_counter_size, '0', output_length_var, output_words_left_var - 1);
                        output_words_left_var :=  output_words_left_var - 1;
                        out_wr_loc <= '1';
                    when W_1FILL =>
                        output_buffer <= encode_fill(word_size, fill_counter_size, '1', output_length_var, output_words_left_var - 1);
                        output_words_left_var := output_words_left_var - 1;
                        out_wr_loc <= '1';
                    when W_LITERAL =>
                        output_buffer <= encode_literal(word_size, current_output_block_var);
                        output_words_left_var := output_words_left_var - 1;
                        out_wr_loc <= '1';
                    when others =>
                        out_wr_loc <= '0';
                end case;

                -- reset output length on last word of current output
                if (output_words_left_var = 0) then
                    output_length_var := to_unsigned(0, fill_counter_size);
                    continue_last_output_var := true;
                end if;
            else
                -- output isn't ready yet
                out_wr_loc <= '0';
            end if;

                -- persist variables
            output_length <= output_length_var;
            output_words_left <= output_words_left_var;
            continue_last_output <= continue_last_output_var;
            current_output_block <= current_output_block_var;
            next_output_block <= next_output_block_var;

            FINAL_OUT <= to_std_logic(is_final and output_words_left <= 1);
        end if;

        --
        -- do I/O on falling edge of clock signal
        --
        if (CLK'event and CLK = '0') then
            for input_idx in 0 to num_inputs-1 loop
                if (in_rd_loc(input_idx) = '1' or (running = '1' and consumed_length(input_idx) = input_length(input_idx))) then
                    -- reading is intended or output is done and a new input is needed
                    if (input_available(input_idx) = '1' or final_received(input_idx) = '1') then
                        read_input(input_idx);
                    end if;

                    if (FINAL_IN(input_idx) = '1') then
                        final_received(input_idx) <= '1';
                    end if;
                else
                    -- don't read in next cycle
                    in_rd_loc(input_idx) <= '0';
                end if;
            end loop;

            set_done_consuming;
            set_input_available;

            -- ready to write output value
            if (out_wr_loc = '1' and OUT_FULL = '0') then
                BLK_OUT <= output_buffer;
            end if;

            -- stop processing if output buffer is full
            if (OUT_FULL = '0') then
                running <= '1';
            else
                running <= '0';
            end if;
        end if;

        check_RESET;
    end process;

    IN_RD  <= in_rd_loc;
    OUT_WR <= out_wr_loc;

end IMP;
