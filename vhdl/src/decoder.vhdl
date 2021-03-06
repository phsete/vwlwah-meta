library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
use work.utils.all;

entity decoder is
    Generic (
        word_size:              natural := 5;
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
        BLK_OUT:            out std_logic_vector(word_size-2 downto 0);
        IN_RD:              out std_logic;
        FINAL_OUT:          out std_logic
    );
end decoder;

architecture IMP of decoder is

    signal input_fill_length:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal current_word_buffer: std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal next_word_buffer:    std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(word_size-2 downto 0) := (others => 'U');
    signal input_available:     std_logic := '0';
    signal running:             std_logic := '1';
    signal final:               boolean := false;
    signal IN_RD_loc:           std_logic;
    signal OUT_WR_loc:          std_logic;
    signal current_type:        Word := W_NONE;
    signal next_type:           Word := W_NONE;

begin
    process (CLK)

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- handles decoding of the current fill word
        --
        procedure handle_F (fill_type: std_logic) is
        begin
            if (next_type = current_type) then
                -- the current fill is a concatenated fill and therfore is still active
                if (input_fill_length - output_fill_length > 1 or IN_EMPTY = '0' or final) then
                    -- a word can only be written if the input length is not reached or
                    -- if the buffer pipeline is stepped forward in the next cycle
                    -- prepare to output the current fill block
                    OUT_WR_loc <= '1';
                    output_buffer <= decode_fill(word_size, fill_type);
                    -- increase number of output words
                    output_fill_length <= output_fill_length + 1;
                end if;
            else
                if (input_fill_length - output_fill_length > 1) then
                    -- the current fill is not fully decoded yet
                    -- prepare to output the current fill block
                    OUT_WR_loc <= '1';
                    output_buffer <= decode_fill(word_size, fill_type);
                    -- increase number of output words
                    output_fill_length <= output_fill_length + 1;
                elsif (input_fill_length - output_fill_length = 1) then
                    -- the current fill ends with this output word
                    -- prepare to output the current fill block
                    OUT_WR_loc <= '1';
                    output_buffer <= decode_fill(word_size, fill_type);
                    -- RESET both fill length counters
                    input_fill_length <= (others => '0');
                    output_fill_length <= (others => '0');

                    if (final) then
                        -- mark the end of all output
                        FINAL_OUT <= '1';
                    end if;
                else 
                    OUT_WR_loc <= '0';
                end if;
            end if;
        end procedure;

        --
        -- handles decoding of the current literal word
        --
        procedure handle_L is
        begin
            -- prepare to output the current literal word
            output_buffer <= decode_literal(word_size, current_word_buffer);
            OUT_WR_loc <= '1';

            if (final) then
                -- mark the end of all output
                FINAL_OUT <= '1';
            end if;
        end procedure;

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_RESET is
        begin
            if (RESET = '0') then
                input_fill_length   <= (others => '0');
                output_fill_length  <= (others => '0');
                current_word_buffer <= (others => 'U');
                next_word_buffer    <= (others => 'U');
                output_buffer       <= (others => 'U');
                input_available     <= '0';
                running             <= '1';
                final               <= false;
                current_type        <= W_NONE;
                next_type           <= W_NONE;
            end if;
        end procedure;

    begin
        -- rising clock signal
        -- do logic and prepare output
        if (CLK'event and CLK='1') then
            -- don't write by default
            OUT_WR_loc <= '0';

            if (running = '1') then
                -- handle the current word type
                case current_type is
                    when W_0FILL =>
                        handle_F('0');
                    when W_1FILL =>
                        handle_F('1');
                    when W_LITERAL =>
                        handle_L;
                    when others =>
                end case;
            end if;

            input_available <= not(IN_EMPTY);

            if (FINAL_IN = '1') then
                final <= true;
            end if;

        end if;

        -- falling clock signal
        -- reads inputs, steps buffer pipeline forward and determines future read state
        if (CLK'event and CLK='0') then
            if (IN_RD_loc = '1' and running = '1' and (input_available = '1' or final)) then
                if next_type = W_0FILL or next_type = W_1FILL then
                    -- the next word to handle is a fill word
                    -- --> decode it's length
                    input_fill_length <= parse_fill_length(word_size, fill_counter_size, input_fill_length, next_word_buffer);

                    -- determine whether or not to continue reading in the next cycle
                    if (parse_fill_length(word_size, fill_counter_size, input_fill_length, next_word_buffer) > output_fill_length + 1) then
                        IN_RD_loc <= '0';
                    else
                        IN_RD_loc <= '1';
                    end if;
                else
                    -- the next type is either
                    IN_RD_loc <= '1';
                end if;

                -- read the next word and push buffers forward
                current_word_buffer <= next_word_buffer;
                current_type <= next_type;

                if (input_available = '1') then
                    -- there is a next word available. read it.
                    next_word_buffer <= BLK_IN;
                    next_type <= parse_word_type(word_size, BLK_IN);
                else
                    -- final state is reached. the next word is undefined
                    next_word_buffer <= (others => 'U');
                    next_type <= W_NONE;
                end if;
            else
                -- if no word was read, check fill length to determine next read state
                if (input_fill_length > output_fill_length + 1) then
                    IN_RD_loc <= '0';
                else
                    IN_RD_loc <= '1';
                end if;
            end if;

            -- write next block if available
            if (OUT_WR_loc = '1' and OUT_FULL = '0') then
                BLK_OUT <= output_buffer;
            end if;

            -- stop processing if output buffer is full
            if (OUT_FULL = '0') then
                running <= '1';
            else
                running <= '0';
            end if;
        end if;

        -- wait for a RESET signal
        check_RESET;
    end process;

    IN_RD  <= IN_RD_loc;
    OUT_WR <= OUT_WR_loc;

end IMP;
