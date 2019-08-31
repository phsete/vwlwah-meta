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
    signal current_type:        Word := W_NONE;
    signal last_type:           Word := W_NONE;
    signal output_type:         Word := W_NONE;
    signal in_rd_loc:           std_logic;
    signal out_wr_loc:          std_logic;

    signal output_words_left:   integer := 0;
    signal current_word_handled:boolean := true;
    signal literal_buffer:      std_logic_vector(output_word_size-1 downto 0) := (others => '0');
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
                literal_buffer          <= (others => '0');
                running                 <= '1';
                current_type            <= W_NONE;
                last_type               <= W_NONE;
                output_type             <= W_NONE;
                final                   <= false;
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

        procedure FILL_to_NONE (fill_type: std_logic) is
        begin
            if (free_buffer_space < scaling_factor) then
                -- the buffer has contents
                if (input_fill_length < free_buffer_space) then
                    -- the fill fits into the buffer without filling it
                    assert to_integer(input_fill_length) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 110)" severity note;
                    literal_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), to_integer(input_fill_length));
                    literal_buffer_pos <= literal_buffer_pos - input_fill_length;
                    out_wr_loc <= '0';
                elsif (input_fill_length = free_buffer_space) then
                    -- the fill uses all the remaining buffer space
                    assert integer(free_buffer_space) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 117)" severity note;
                    output_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), integer(free_buffer_space));
                    literal_buffer_pos <= (others => '1');
                    out_wr_loc <= '1';
                else
                    -- the fill uses more than the remaining buffer space
                    assert integer(free_buffer_space) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 124)" severity note;
                    output_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), integer(free_buffer_space));
                    -- literal_buffer_pos is set afterwards
                    out_wr_loc <= '1';

                    -- calculate output fill length
                    output_fill_length_var := (input_fill_length - free_buffer_space) / scaling_factor;
                    output_fill_remainder := to_integer((input_fill_length - free_buffer_space)) mod scaling_factor;
                    output_fill_length <= output_fill_length_var;
                    output_words_left <= fill_words_needed(output_word_size,
                                         fill_counter_size,
                                         output_fill_length_var);
                    if (fill_type = '0') then
                        output_type <= W_0FILL;
                    else
                        output_type <= W_1FILL;
                    end if;

                    -- fill remaining literal
                    assert integer(output_fill_remainder) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 144)" severity note;
                    --                                                                                                              \------???------/
                    literal_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), scaling_factor-1, integer(output_fill_remainder));
                    literal_buffer_pos <= to_unsigned(scaling_factor - output_fill_remainder - 1, log2ceil(scaling_factor));
                end if;
            else -- the buffer doesn't have contents
                 -- calculate output fill length
                output_fill_length_var := input_fill_length / scaling_factor;
                output_fill_remainder := to_integer(input_fill_length) mod scaling_factor;
                output_fill_length <= output_fill_length_var;
                output_words_left_var := fill_words_needed(output_word_size, fill_counter_size, output_fill_length_var);

                if (output_words_left_var > 0) then
                    -- output first fill word
                    output_buffer <= encode_fill(output_word_size, fill_counter_size, fill_type, output_fill_length_var, output_words_left_var-1);
                    out_wr_loc <= '1';
                    output_words_left <= output_words_left_var-1;
                    if (fill_type = '0') then
                        output_type <= W_0FILL;
                    else
                        output_type <= W_1FILL;
                    end if;
                else
                    out_wr_loc <= '0';
                end if;

                -- fill remaining literal
                    assert integer(output_fill_remainder) < to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 172)" severity note;
                literal_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), integer(output_fill_remainder));
                literal_buffer_pos <= literal_buffer_pos - output_fill_remainder;
            end if;
        end procedure;

        procedure FILL_to_LITERAL (fill_type: std_logic) is
                                   variable literal_buffer_var:      std_logic_vector(output_word_size-1 downto 0) := (others => 'U');
                                   variable literal_buffer_pos_var:  unsigned(log2ceil(scaling_factor)-1 downto 0) := (others => '1');
        begin
            if (free_buffer_space < scaling_factor) then
                -- the buffer has contents
                if (input_fill_length + 1 < free_buffer_space) then
                    -- the fill and the literal fit into the buffer without filling it
                    assert to_integer(input_fill_length) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 187)" severity note;
                    literal_buffer_var := extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), to_integer(input_fill_length));
                    literal_buffer_pos_var := to_unsigned(to_integer(literal_buffer_pos - input_fill_length), log2ceil(scaling_factor));

                    assert 1 <= to_integer(literal_buffer_pos_var) + 1
                    report "trying to push more blocks into a literal word than possible (error 192)" severity note;
                    literal_buffer <= extend_literal(word_size, literal_buffer_var, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                    literal_buffer_pos <= literal_buffer_pos_var - 1;
                    out_wr_loc <= '0';
                elsif (input_fill_length + 1 = free_buffer_space) then
                    -- the fill fits into the buffer and the literal fills it
                    assert to_integer(input_fill_length) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 199)" severity note;
                    literal_buffer_var := extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), to_integer(input_fill_length));
                    literal_buffer_pos_var := to_unsigned(to_integer(literal_buffer_pos - input_fill_length), log2ceil(scaling_factor));

                    assert 1 <= to_integer(literal_buffer_pos_var) + 1
                    report "trying to push more blocks into a literal word than possible (error 204)" severity note;
                    output_buffer <= extend_literal(word_size, literal_buffer_var, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                    literal_buffer_pos <= literal_buffer_pos_var - 1;
                    out_wr_loc <= '1';
                elsif (input_fill_length = free_buffer_space) then
                    -- the fill fills the remaining buffer space and the literal starts a new buffer
                    assert to_integer(input_fill_length) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 211)" severity note;
                    output_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), to_integer(input_fill_length));
                    literal_buffer_pos_var := (others => '1');

                    assert 1 <= to_integer(literal_buffer_pos_var) + 1
                    report "trying to push more blocks into a literal word than possible (error 216)" severity note;
                    literal_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                    literal_buffer_pos <= literal_buffer_pos_var - 1;
                    out_wr_loc <= '1';
                else
                    -- the fill uses more than the remaining buffer space
                    assert integer(free_buffer_space) <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 223)" severity note;
                    output_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), integer(free_buffer_space));
                    literal_buffer_pos_var := (others => '1');
                    out_wr_loc <= '1';

                    -- calculate output fill length
                    output_fill_length_var := (input_fill_length - free_buffer_space) / scaling_factor;
                    output_fill_remainder := to_integer((input_fill_length - free_buffer_space)) mod scaling_factor;
                    output_fill_length <= output_fill_length_var;
                    output_words_left <= fill_words_needed(output_word_size,
                                         fill_counter_size,
                                         output_fill_length_var);
                    if (fill_type = '0') then
                        output_type <= W_0FILL;
                    else
                        output_type <= W_1FILL;
                    end if;

                    if (output_fill_remainder + 1 < scaling_factor) then
                        -- remaining blocks and literal fit into buffer without filling it
                        assert output_fill_remainder <= to_integer(literal_buffer_pos_var) + 1
                        report "trying to push more blocks into a literal word than possible (error 244)" severity note;
                        literal_buffer_var := extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos_var), output_fill_remainder);
                        literal_buffer_pos_var := to_unsigned(to_integer(literal_buffer_pos_var - output_fill_remainder), log2ceil(scaling_factor));

                        assert 1 <= to_integer(literal_buffer_pos_var) + 1
                        report "trying to push more blocks into a literal word than possible (error 249)" severity note;
                        literal_buffer <= extend_literal(word_size, literal_buffer_var, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                        literal_buffer_pos <= literal_buffer_pos_var - 1;
                    else
                        -- remaining blocks fit into the buffer and the literal fills it
                        assert output_fill_remainder <= to_integer(literal_buffer_pos_var) + 1
                        report "trying to push more blocks into a literal word than possible (error 255)" severity note;
                        literal_buffer_var := extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos_var), output_fill_remainder);
                        literal_buffer_pos_var := to_unsigned(to_integer(literal_buffer_pos_var - output_fill_remainder), log2ceil(scaling_factor));

                        assert 1 <= to_integer(literal_buffer_pos_var) + 1
                        report "trying to push more blocks into a literal word than possible (error 260)" severity note;
                        literal_buffer <= extend_literal(word_size, literal_buffer_var, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                        literal_buffer_pos <= literal_buffer_pos_var - 1;
                        pending_literal <= true;
                    end if;
                end if;
            else -- the buffer doesn't have contents
                 -- calculate output fill length
                output_fill_length_var := input_fill_length / scaling_factor;
                output_fill_remainder := to_integer(input_fill_length) mod scaling_factor;
                output_fill_length <= output_fill_length_var;
                output_words_left_var := fill_words_needed(output_word_size, fill_counter_size, output_fill_length_var);

                if (output_words_left_var > 0) then
                    -- output first fill word
                    output_buffer <= encode_fill(output_word_size, fill_counter_size, fill_type, output_fill_length_var, output_words_left_var-1);
                    out_wr_loc <= '1';
                    output_words_left <= output_words_left_var-1;
                    if (fill_type = '0') then
                        output_type <= W_0FILL;
                    else
                        output_type <= W_1FILL;
                    end if;
                else
                    out_wr_loc <= '0';
                end if;

                literal_buffer_pos_var := (others => '1');
                if (output_fill_remainder + 1 < scaling_factor) then
                    -- remaining blocks and literal fit into buffer without filling it
                    assert output_fill_remainder <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 291)" severity note;
                    literal_buffer_var := extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), output_fill_remainder);
                    literal_buffer_pos_var := to_unsigned(to_integer(literal_buffer_pos_var - output_fill_remainder), log2ceil(scaling_factor));

                    assert 1 <= to_integer(literal_buffer_pos_var) + 1
                    report "trying to push more blocks into a literal word than possible (error 296)" severity note;
                    literal_buffer <= extend_literal(word_size, literal_buffer_var, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                    literal_buffer_pos <= literal_buffer_pos_var - 1;
                else
                    -- remaining blocks fit into the buffer and the literal fills it
                    assert output_fill_remainder <= to_integer(literal_buffer_pos) + 1
                    report "trying to push more blocks into a literal word than possible (error 302)" severity note;
                    literal_buffer_var := extend_literal(word_size, literal_buffer, output_word_size, decode_fill(word_size, fill_type), to_integer(literal_buffer_pos), output_fill_remainder);
                    literal_buffer_pos_var := to_unsigned(to_integer(literal_buffer_pos_var - output_fill_remainder), log2ceil(scaling_factor));

                    assert 1 <= to_integer(literal_buffer_pos_var) + 1
                    report "trying to push more blocks into a literal word than possible (error 307)" severity note;
                    literal_buffer <= extend_literal(word_size, literal_buffer_var, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos_var), 1);
                    literal_buffer_pos <= literal_buffer_pos_var - 1;
                    pending_literal <= true;
                end if;
            end if;
        end procedure;

        procedure LITERAL_to_LITERAL is
        begin
            if (literal_buffer_pos = 0) then
                assert 1 <= to_integer(literal_buffer_pos) + 1
                report "trying to push more blocks into a literal word than possible (error 319)" severity note;
                output_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos), 1);
                out_wr_loc <= '1';
            else
                assert 1 <= to_integer(literal_buffer_pos) + 1
                report "trying to push more blocks into a literal word than possible (error 324)" severity note;
                literal_buffer <= extend_literal(word_size, literal_buffer, output_word_size, decode_literal(word_size, current_word), to_integer(literal_buffer_pos), 1);
                out_wr_loc <= '0';
            end if;

            literal_buffer_pos <= literal_buffer_pos-1;
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

                case last_type is
                    when W_0FILL =>
                        case current_type is
                            when W_0FILL =>
                                extend_fill(current_word);
                                out_wr_loc <= '0';
                            when W_1FILL =>
                                FILL_to_NONE('0');
                                start_new_fill(current_word);
                            when W_LITERAL =>
                                FILL_to_LITERAL('0');
                                input_fill_length <= (others => '0');
                            when others =>
                                FILL_to_NONE('0');
                                input_fill_length <= (others => '0');
                        end case;
                    when W_1FILL =>
                        case current_type is
                            when W_0FILL =>
                                FILL_to_NONE('1');
                                start_new_fill(current_word);
                            when W_1FILL =>
                                extend_fill(current_word);
                                out_wr_loc <= '0';
                            when W_LITERAL =>
                                FILL_to_LITERAL('1');
                                input_fill_length <= (others => '0');
                            when others =>
                                FILL_to_NONE('1');
                                input_fill_length <= (others => '0');
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
                                LITERAL_to_LITERAL;
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
                                LITERAL_to_LITERAL;
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
            elsif (final and current_word_handled) then
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
            elsif (final) then
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

        OUT_WR <= out_wr_loc;

    end process;

    FINAL_OUT <= '1' when (current_type = W_NONE and final and output_words_left = 0 and input_fill_length = 0 and not pending_literal) else '0';
    IN_RD  <= in_rd_loc;
end IMP;
