library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity sizedown is
    Generic (
        word_size:              natural := 5;
        scaling_factor:         natural := 2;
        fill_counter_size:      natural := 32
    );
    port (
        CLK:                in  std_logic;
        RESET:              in  std_logic;
        IN_EMPTY:           in  std_logic;
        FINAL_IN:           in  std_logic;
        BLK_IN:             in  std_logic_vector(word_size-1 downto 0);
        OUT_FULL:           in  std_logic;
        OUT_WR:             out std_logic;
        BLK_OUT:            out std_logic_vector(scale_down(word_size, scaling_factor)-1 downto 0);
        IN_RD:              out std_logic;
        FINAL_OUT:          out std_logiC
    );
    constant output_word_size: natural := scale_down(word_size, scaling_factor);
    constant output_fill_counter_size: natural := fill_counter_size + log2ceil(scaling_factor);
end sizedown;

architecture IMP of sizedown is

    signal input_fill_length:   unsigned(output_fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(output_fill_counter_size-1 downto 0) := (others => '0');
    signal current_word:        std_logic_vector(word_size-1        downto 0) := (others => 'U');
    signal last_word:           std_logic_vector(word_size-1        downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(output_word_size-1 downto 0) := (others => 'U');
    signal running:             std_logic := '1';
    signal final:               boolean := false;
    signal final_delay:         boolean := false;
    signal current_type:        Word := W_NONE;
    signal last_type:           Word := W_NONE;
    signal output_type:         Word := W_NONE;
    signal in_rd_loc:           std_logic;
    signal out_wr_loc:          std_logic;
    signal last_miniblock_type: Word := W_NONE;

    signal current_miniblocks:  natural := 0;
    signal miniblocks_handled:  natural := 0;

    signal output_words_left:   integer := 0;

begin
    process (CLK)
        variable input_fill_length_var:   unsigned(output_fill_counter_size-1 downto 0) := (others => '0');
        variable output_fill_length_var:  unsigned(output_fill_counter_size-1 downto 0) := (others => '0');
        variable output_words_left_var:   integer := 0;
        variable current_miniblock_var:   std_logic_vector(output_word_size-1 downto 0) := (others => 'U');
        variable current_miniblock_type:  Word := W_NONE;

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_reset is
        begin
            if (RESET = '0') then
                input_fill_length       <= (others => '0');
                output_fill_length      <= (others => '0');
                current_word            <= (others => 'U');
                output_buffer           <= (others => 'U');
                running                 <= '1';
                current_type            <= W_NONE;
                last_type               <= W_NONE;
                output_type             <= W_NONE;
                final                   <= false;
                final_delay             <= false;
                output_words_left       <= 0;
                current_miniblocks      <= 0;
                miniblocks_handled      <= 0;
                last_miniblock_type     <= W_NONE;
            end if;
        end procedure;

    begin
        --
        -- rising edge
        --
        if (CLK'event and CLK = '1' and running = '1') then
            if (output_words_left = 0 and miniblocks_handled < current_miniblocks) then
                -- all previous output is done

                case current_type is
                    when W_0FILL | W_1FILL =>
                        if (last_type = current_type or (last_type = W_LITERAL and last_miniblock_type = current_type)) then
                            -- last fill word is extended in current word
                            input_fill_length_var := parse_fill_length(word_size,
                                                     output_fill_counter_size,
                                                     input_fill_length,
                                                     current_word);
                            input_fill_length <= input_fill_length_var;
                            output_fill_length <= output_fill_length + shift_left(input_fill_length_var, log2ceil(scaling_factor));
                            out_wr_loc <= '0';
                        elsif (last_type = W_0FILL or last_type = W_1FILL or (last_type = W_LITERAL and (last_miniblock_type = W_0FILL or last_miniblock_type = W_1FILL))) then
                            -- switching from one fill type to another
                            output_fill_length_var := (others => '0');
                            output_fill_length_var := shift_left(input_fill_length, log2ceil(scaling_factor));
                            input_fill_length <= (others => '0');
                            output_fill_length <= output_fill_length_var;
                            output_words_left_var := fill_words_needed(output_word_size,
                                                     output_fill_counter_size,
                                                     output_fill_length_var);
                            if (last_type = W_0FILL) then
                                output_buffer <= encode_fill(output_word_size,
                                                 output_fill_counter_size,
                                                 '0', output_fill_length_var,
                                                 output_words_left_var-1);
                            elsif (last_type = W_1FILL) then
                                output_buffer <= encode_fill(output_word_size,
                                                 output_fill_counter_size,
                                                 '1', output_fill_length_var,
                                                 output_words_left_var-1);
                            end if;
                            out_wr_loc <= '1';
                            -- starting a new fill
                            input_fill_length_var := parse_fill_length(word_size,
                                                     output_fill_counter_size,
                                                     to_unsigned(0, output_fill_counter_size),
                                                     current_word);
                            input_fill_length <= input_fill_length_var;
                            output_fill_length <= output_fill_length + shift_left(input_fill_length_var, log2ceil(scaling_factor));
                            output_words_left <= output_words_left_var-1;
                        else
                            -- starting a new fill
                            input_fill_length_var := parse_fill_length(word_size,
                                                     output_fill_counter_size,
                                                     to_unsigned(0, output_fill_counter_size),
                                                     current_word);
                            input_fill_length <= input_fill_length_var;
                            output_fill_length <= output_fill_length + shift_left(input_fill_length_var, log2ceil(scaling_factor));
                            out_wr_loc <= '0';
                        end if;
                        output_type <= last_type;
                        miniblocks_handled <= miniblocks_handled + 1;
                    when W_LITERAL =>
                        current_miniblock_var := split_literal(word_size, current_word, scaling_factor, current_miniblocks - 1 - miniblocks_handled);
                        current_miniblock_type := parse_block_type(output_word_size, current_miniblock_var);
                        last_miniblock_type <= current_miniblock_type;
                        if ((output_fill_length > 0) and ((current_miniblock_type = W_LITERAL) or (last_miniblock_type /= current_miniblock_type))) then
                            -- current part of literal is either a literal itself or a different type of fill
                            -- finish predecessing fill first
                            input_fill_length <= (others => '0');
                            output_words_left_var := fill_words_needed(output_word_size,
                                                     output_fill_counter_size,
                                                     output_fill_length);
                            if (last_miniblock_type = W_0FILL) then
                                output_buffer <= encode_fill(output_word_size,
                                                 output_fill_counter_size,
                                                 '0', output_fill_length,
                                                 output_words_left_var-1);
                            elsif (last_miniblock_type = W_0FILL) then
                                output_buffer <= encode_fill(output_word_size,
                                                 output_fill_counter_size,
                                                 '1', output_fill_length,
                                                 output_words_left_var-1);
                            end if;
                            output_words_left <= output_words_left_var - 1;
                            out_wr_loc <= '1';
                            output_type <= last_miniblock_type;
                            
                            if output_words_left_var = 1 then
                                output_fill_length <= (others => '0');
                            end if;
                        elsif (current_miniblock_type = W_LITERAL) then
                            -- current word is a literal, no word to continue
                            output_words_left <= 0;
                            output_buffer <= current_miniblock_var;
                            out_wr_loc <= '1';
                            output_type <= current_miniblock_type;
                            miniblocks_handled <= miniblocks_handled + 1;
                        elsif (output_fill_length > 0 and (last_miniblock_type = current_miniblock_type)) then
                            -- the current miniblock continues the last fill
                            output_fill_length <= output_fill_length + 1;
                            miniblocks_handled <= miniblocks_handled + 1;
                        elsif (current_miniblock_type = W_0FILL or current_miniblock_type = W_1FILL) then
                            -- starting a new fill
                            output_fill_length <= to_unsigned(1, output_fill_counter_size);
                            out_wr_loc <= '0';
                            miniblocks_handled <= miniblocks_handled + 1;
                        end if;
                    when others =>
                        -- current word is unknown -> input is final
                        if (output_fill_length > 0) then
                            output_fill_length_var := (others => '0');
                            output_fill_length_var := output_fill_length + shift_left(input_fill_length, log2ceil(scaling_factor));
                            input_fill_length <= (others => '0');
                            output_fill_length <= output_fill_length_var;
                            output_words_left_var := fill_words_needed(output_word_size,
                                                     output_fill_counter_size,
                                                     output_fill_length_var);
                            if (last_type = W_0FILL) then
                                output_buffer <= encode_fill(output_word_size,
                                                 output_fill_counter_size,
                                                 '0', output_fill_length_var,
                                                 output_words_left_var-1);
                            elsif (last_type = W_1FILL) then
                                output_buffer <= encode_fill(output_word_size,
                                                 output_fill_counter_size,
                                                 '1', output_fill_length_var,
                                                 output_words_left_var-1);
                            end if;
                            output_words_left <= output_words_left_var - 1;
                            out_wr_loc <= '1';
                            output_type <= last_type;
                            
                            if output_words_left_var = 1 then
                                output_fill_length <= (others => '0');
                            end if;
                        else
                            out_wr_loc <= '0';
                        end if;
                end case;
            elsif (output_words_left > 0) then
                -- there is still output to do
                case output_type is
                    when W_0FILL =>
                        output_buffer <= encode_fill(output_word_size,
                                         output_fill_counter_size,
                                         '0', output_fill_length,
                                         output_words_left-1);
                        output_words_left <= output_words_left-1;
                        out_wr_loc <= '1';
                    when W_1FILL =>
                        output_buffer <= encode_fill(output_word_size,
                                         output_fill_counter_size,
                                         '1', output_fill_length,
                                         output_words_left-1);
                        output_words_left <= output_words_left-1;
                        out_wr_loc <= '1';
                    when W_LITERAL =>
                        output_buffer <= split_literal(word_size,
                                         last_word,
                                         scaling_factor,
                                         output_words_left-1);
                        output_words_left <= output_words_left - 1;
                        out_wr_loc <= '1';
                    when others =>
                        out_wr_loc <= '0';
                end case;

                if output_words_left = 1 then
                    -- this was the last word of the current output
                    output_fill_length <= (others => '0');
                end if;
            elsif (miniblocks_handled < current_miniblocks) then
                -- we are currently handling a fill
            else
                out_wr_loc <= '0';
            end if;

            if (FINAL_IN = '1') then
                final_delay <= final;
                final <= true;
            end if;
        end if;

        --
        -- falling edge
        --
        if (CLK'event and CLK = '0') then
            -- read the next word
            if (in_rd_loc = '1') then
                current_word <= BLK_IN;
                current_type <= parse_word_type(word_size, BLK_IN);
                case parse_word_type(word_size, BLK_IN) is
                    when W_NONE =>
                        if not(final) then
                            current_miniblocks <= 0;
                        end if;
                    when W_LITERAL =>
                        current_miniblocks <= 4;
                    when others =>
                        current_miniblocks <= 1;
                end case;
                miniblocks_handled <= 0;
                -- push buffers forward
                last_word    <= current_word;
                last_type    <= current_type;
                if (current_type = W_0FILL or current_type = W_0FILL) then
                    last_miniblock_type <= current_type;
                end if;
            elsif (final_delay and miniblocks_handled = current_miniblocks) then
                -- don't process any further
                current_word <= (others => 'U');
                current_type <= W_NONE;
                current_miniblocks <= scaling_factor when parse_word_type(word_size, BLK_IN) = W_LITERAL else 1;
                -- push buffers forward
                last_word    <= current_word;
                last_type    <= current_type;
            end if;

            -- determine next read state
            if (in_empty = '1') then
                -- cant' read when there's no input
                in_rd_loc    <= '0';
            elsif (output_words_left > 1) then
                in_rd_loc    <= '0';
            elsif (final_delay) then
                -- finally done
                in_rd_loc    <= '0';
            elsif (in_rd_loc = '1') then -- TODO: and scaling_factor > 1 and parse_word_type(word_size, BLK_IN) = W_LITERAL) then
                -- after reading a literal, we can only read if scaling factor is 1
                -- otherwise we need more cycles to output the resulting literals
                in_rd_loc    <= '0';
            elsif (miniblocks_handled < current_miniblocks) then
                in_rd_loc <= '0';
            else
                in_rd_loc    <= '1';
            end if;

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

        check_reset;

        FINAL_OUT <= '1' when (final_delay and output_words_left = 0 and input_fill_length = 0 and miniblocks_handled = current_miniblocks) else '0';
        OUT_WR <= out_wr_loc;

    end process;

    IN_RD  <= in_rd_loc;
end IMP;
