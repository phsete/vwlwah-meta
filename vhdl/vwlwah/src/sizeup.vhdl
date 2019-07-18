library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.utils.all;

entity sizeup is
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
        BLK_OUT:            out std_logic_vector(scale_up(word_size, scaling_factor)-1 downto 0);
        IN_RD:              out std_logic;
        FINAL_OUT:          out std_logiC
    );
    constant output_word_size: natural := scale_up(word_size, scaling_factor);
end sizeup;

architecture IMP of sizeup is

    signal input_fill_length:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(fill_counter_size-1 downto 0) := (others => '0');
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

    signal output_words_left:   integer := 0;
    signal current_word_handled:boolean := true;
    signal literal_buffer:      std_logic_vector(output_word_size-1 downto 0) := (others => 'U');
    -- points to the next free sub-block of the literal buffer
    -- counting from scaling_factor-1 downto 0
    signal literal_buffer_pos:  unsigned(log2ceil(scaling_factor)-1 downto 0) := (others => '1');
    signal free_buffer_space:   natural := scaling_factor;

    -- indicates a full literal buffer which cannot be printed immediately
    signal pending_literal:     boolean := false;

begin
    process (CLK)
        variable input_fill_length_var:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
        variable output_fill_length_var:  unsigned(fill_counter_size-1 downto 0) := (others => '0');
        variable output_words_left_var:   integer := 0;
        variable output_fill_remainder:   integer := 0;

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
                literal_buffer          <= (others => 'U');
                running                 <= '1';
                current_type            <= W_NONE;
                last_type               <= W_NONE;
                output_type             <= W_NONE;
                final                   <= false;
                final_delay             <= false;
                output_words_left       <= 0;
                current_word_handled    <= true;
                literal_buffer_pos      <= (others => '1');
                pending_literal         <= false;
            end if;
        end procedure;

        procedure start_new_fill (input_word: std_logic_vector(word_size-1 downto 0)) is
        begin
            input_fill_length <= parse_fill_length(word_size,
                                 fill_counter_size,
                                 to_unsigned(0, fill_counter_size),
                                 input_word);
        end procedure;

        procedure extend_fill (input_word: std_logic_vector(word_size-1 downto 0)) is
        begin
            input_fill_length <= parse_fill_length(word_size,
                                 fill_counter_size,
                                 input_fill_length,
                                 input_word);
        end procedure;
        
        procedure extend_literal_loc (allow_output: boolean;
                                      input_block: std_logic_vector(word_size-2 downto 0);
                                      num_extensions: natural) is
            variable result: std_logic_vector(output_word_size-1 downto 0);
        begin
            if num_extensions /= 0 then
                result := extend_literal(word_size,
                literal_buffer,
                output_word_size,
                input_block,
                to_integer(literal_buffer_pos),
                to_integer(literal_buffer_pos)-num_extensions+1);
                literal_buffer_pos <= literal_buffer_pos - num_extensions; -- TODO cannot assign twice in one cycle
                if (literal_buffer_pos = 0 and allow_output) then
                -- a complete output word has been built
                    output_buffer <= result;
                else
                    literal_buffer <= result;
                    pending_literal <= not allow_output;
                end if;
            end if;
        end procedure;

    begin

        free_buffer_space <= to_integer(literal_buffer_pos) + 1;

        --
        -- rising edge
        --
        if (CLK'event and CLK = '1' and running = '1') then
            if (output_words_left = 0 and not current_word_handled and not pending_literal) then
                -- all output is done
                -- push buffers forward
                last_word    <= current_word;
                last_type    <= current_type;

                case current_type is
                    when W_0FILL =>
                        case current_type is
                            when W_0FILL =>
                                extend_fill(current_word);
                                out_wr_loc <= '0';
                            when W_1FILL =>
                                -- TODO
                            when W_LITERAL =>
                                -- TODO
                            when others =>
                                if (free_buffer_space < 4) then
                                    if (input_fill_length < free_buffer_space) then
                                        extend_literal_loc(false, decode_fill(word_size, '0'), to_integer(input_fill_length));
                                        out_wr_loc <= '0';
                                    elsif (input_fill_length = free_buffer_space) then
                                        extend_literal_loc(false, decode_fill(word_size, '0'), integer(free_buffer_space));
                                        out_wr_loc <= '1';
                                    else
                                        extend_literal_loc(true, decode_fill(word_size, '0'), integer(free_buffer_space));
                                        out_wr_loc <= '1';

                                        -- calculate output fill length
                                        output_fill_length_var := (input_fill_length - free_buffer_space) / scaling_factor;
                                        output_fill_remainder := to_integer((input_fill_length - free_buffer_space)) mod scaling_factor;
                                        input_fill_length <= (others => '0');
                                        output_fill_length <= output_fill_length_var;
                                        output_words_left <= fill_words_needed(output_word_size,
                                            fill_counter_size,
                                            output_fill_length_var);
                                        output_type <= current_type;

                                        -- fill remaining literal
                                        extend_literal_loc(false, decode_fill(word_size, '0'), integer(output_fill_remainder)); -- TODO: cannot assign twice in one cycle
                                    end if;
                                else
                                    -- calculate output fill length
                                    output_fill_length_var := (input_fill_length - free_buffer_space) / scaling_factor;
                                    output_fill_remainder := to_integer((input_fill_length - free_buffer_space)) mod scaling_factor;
                                    input_fill_length <= (others => '0');
                                    output_fill_length <= output_fill_length_var;
                                    output_words_left_var := fill_words_needed(output_word_size, fill_counter_size, output_fill_length_var);

                                    -- output first fill word
                                    output_buffer <= encode_fill(output_word_size, fill_counter_size, '0', output_fill_length_var, output_words_left_var-1);
                                    out_wr_loc <= '1';
                                    output_words_left <= output_words_left_var-1;
                                    output_type <= current_type;

                                    -- fill remaining literal
                                    extend_literal_loc(false, decode_fill(word_size, '0'), integer(output_fill_remainder));
                                end if;
                        end case;
                    when W_1FILL =>
                        case current_type is
                            when W_0FILL =>
                                -- TODO
                            when W_1FILL =>
                                extend_fill(current_word);
                                out_wr_loc <= '0';
                            when W_LITERAL =>
                                -- TODO
                            when others =>
                                -- TODO
                        end case;
                    when W_LITERAL =>
                        case current_type is
                            when W_0FILL =>
                                start_new_fill(current_word);
                                out_wr_loc <= '0';
                            when W_1FILL =>
                                start_new_fill(current_word);
                                out_wr_loc <= '0';
                            when W_LITERAL =>
                                extend_literal_loc(true, decode_literal(word_size, current_word), 1);
                                out_wr_loc <= to_std_logic(literal_buffer_pos = 0);
                            when others =>
                                out_wr_loc <= '0';
                        end case;
                    when others =>
                        case current_type is
                            when W_0FILL =>
                                start_new_fill(current_word);
                                out_wr_loc <= '0';
                            when W_1FILL =>
                                start_new_fill(current_word);
                                out_wr_loc <= '0';
                            when W_LITERAL =>
                                extend_literal_loc(true, decode_literal(word_size, current_word), 1);
                                out_wr_loc <= to_std_logic(literal_buffer_pos = 0);
                            when others =>
                                out_wr_loc <= '0';
                        end case;
                end case;
                current_word_handled <= true;
            elsif (output_words_left > 0) then
                -- there is still output to do
                case output_type is
                    when W_0FILL =>
                        output_buffer <= encode_fill(output_word_size,
                                         fill_counter_size,
                                         '0', output_fill_length,
                                         output_words_left-1);
                        output_words_left <= output_words_left-1;
                        out_wr_loc <= '1';
                    when W_1FILL =>
                        output_buffer <= encode_fill(output_word_size,
                                         fill_counter_size,
                                         '1', output_fill_length,
                                         output_words_left-1);
                        output_words_left <= output_words_left-1;
                        out_wr_loc <= '1';
                    when others =>
                        out_wr_loc <= '0';
                end case;
            elsif (pending_literal) then
                output_buffer <= literal_buffer;
                out_wr_loc <= '1';
                pending_literal <= false;
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
                current_word_handled <= false;
            elsif (final_delay and current_word_handled) then
                -- don't process any further
                current_word <= (others => 'U');
                current_type <= W_NONE;
                current_word_handled <= false;
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
            elsif (in_rd_loc = '1') then
                in_rd_loc    <= '0';
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

        FINAL_OUT <= '1' when (current_type = W_NONE and final_delay and output_words_left = 0 and input_fill_length = 0) else '0';
        OUT_WR <= out_wr_loc;

    end process;

    IN_RD  <= in_rd_loc;
end IMP;
