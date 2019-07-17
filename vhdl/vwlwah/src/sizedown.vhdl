library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
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

    signal input_fill_length:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(output_fill_counter_size-1 downto 0) := (others => '0');
    signal current_word:        std_logic_vector(word_size-1        downto 0) := (others => 'U');
    signal last_word:           std_logic_vector(word_size-1        downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(output_word_size-1 downto 0) := (others => 'U');
    signal input_available:     std_logic := '0';
    signal running:             std_logic := '1';
    signal final:               boolean := false;
    signal final_delay:         boolean := false;
    signal current_type:        Word := W_NONE;
    signal last_type:           Word := W_NONE;
    signal in_rd_loc:           std_logic;
    signal out_wr_loc:          std_logic;

    signal output_words_left:   integer := 0;

begin
    process (CLK)

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_reset is
        begin
            if (RESET = '0') then
                input_fill_length   <= (others => '0');
                output_fill_length  <= (others => '0');
                current_word        <= (others => 'U');
                output_buffer       <= (others => 'U');
                input_available     <= '0';
                running             <= '1';
                current_type        <= W_NONE;
                last_type           <= W_NONE;
                final               <= false;
                final_delay         <= false;
                output_words_left   <= 0;
            end if;
        end procedure;

    begin
        --
        -- rising edge
        --
        if (CLK'event and CLK = '1' and running = '1') then
            if (output_words_left = 0) then
                -- push buffers forward
                last_word    <= current_word;
                last_type    <= current_type;

                case current_type is
                    when W_0FILL | W_1FILL =>
                        output_words_left <= fill_words_needed(output_word_size,
                                             output_fill_counter_size,
                                             input_fill_length * scaling_factor);
                        input_fill_length <= parse_fill_length(word_size,
                                             fill_counter_size,
                                             input_fill_length,
                                             current_word);
                    when W_LITERAL =>
                        output_words_left <= scaling_factor - 1;
                        output_buffer <= split_literal(word_size,
                                         current_word,
                                         scaling_factor,
                                         scaling_factor-1);
                        out_wr_loc <= '1';
                    when others =>
                        out_wr_loc <= '0';
                        output_words_left <= 0;
                end case;
            else
                case last_type is
                    when W_0FILL =>
                        -- TODO
                    when W_1FILL =>
                        -- TODO
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
            end if;

            input_available <= not(IN_EMPTY);

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
            elsif (final and output_words_left = 1) then
                -- don't process any further
                current_word <= (others => 'U');
                current_type <= W_NONE;
            end if;

            -- determine next read state
            if (input_available = '0') then
                -- cant' read when there's no input
                in_rd_loc    <= '0';
            elsif (output_words_left > 2) then
                -- > 2 works, > 1 does not work. TODO: why?
                in_rd_loc    <= '0';
            elsif (final_delay) then
                -- finally done
                in_rd_loc    <= '0';
            elsif (in_rd_loc = '1' and scaling_factor > 1 and parse_word_type(word_size, BLK_IN) = W_LITERAL) then
                -- after reading a literal, we can only read if scaling factor is 1
                -- otherwise we need more cycles to output the resulting literals
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

        FINAL_OUT <= '1' when (final_delay and output_words_left = 0) else '0';

        -- TODO leave these two here or better put them outside the process
        IN_RD  <= in_rd_loc;
        OUT_WR <= out_wr_loc;

    end process;

end IMP;
