library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
use work.utils.all;

entity encoderMETA is
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
        BLK_OUT:            out std_logic_vector(word_size-1 downto 0);
        IN_RD:              out std_logic;
        FINAL_OUT:          out std_logic
    );
end encoderMETA;

architecture IMP of encoderMETA is

    type Word_Sequence is (W_LITERAL, W_0FILL, W_1FILL, W_0FILL_1FILL, W_1FILL_0FILL, W_0FILL_LITERAL, W_1FILL_LITERAL, W_NONE);

    signal zero_fill_length:    unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal one_fill_length:     unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal input_buffer:        std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal literal_buffer:      std_logic_vector(word_size-2 downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:     std_logic := '0';
    signal out_wr_loc:          std_logic := '0';
    signal running:             std_logic := '1';
    signal fill_words_left:     natural := 0;
    signal final:               boolean := false;
    signal buffer_type:         Word_Sequence := W_NONE;

begin
    process (CLK)

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_RESET is
        begin
            if (RESET = '0') then
                zero_fill_length    <= (others => '0');
                one_fill_length     <= (others => '0');
                input_buffer        <= (others => 'U');
                literal_buffer      <= (others => 'U');
                output_buffer       <= (others => 'U');
                input_available     <= '0';
                out_wr_loc          <= '0';
                running             <= '1';
                fill_words_left     <= 0;
                final               <= false;
                buffer_type         <= W_NONE;
            end if;
        end procedure;

        --
        -- looks at the input buffer and calls the appropriate procedure
        --
        procedure handle_next_block is
        begin
            if (input_available = '1') then
                -- ready to read input value
                case parse_vwlwah_block_type(word_size, input_buffer) is
                    when W_0FILL =>
                        output_buffer <= (word_size-1 downto 1 => '0') & '1';
                    when W_1FILL =>
                        output_buffer <= (word_size-1 downto 2 => '0') & "10";
                    when W_LITERAL =>
                        output_buffer <= (word_size-1 downto 3 => '0') & "100";
                    when others =>
                end case;
            else
                -- no input and not final -> stall
                out_wr_loc <= '0';
            end if;

            if (final) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

    begin

        --
        -- rising edge
        --
        if (CLK'event and CLK='1') then
            if (running = '1') then
                -- write by default, set to '0' otherwise
                out_wr_loc <= '1';

                case buffer_type is
                    when W_NONE =>
                        handle_next_block;
                    when others =>
                end case;

                if (buffer_type = W_NONE and IN_EMPTY = '0') then
                    input_available <= '1';
                else
                    input_available <= '0';
                end if;
            end if;
        end if;

        --
        -- falling edge
        --
        if (CLK'event and CLK='0') then
            if (input_available = '1' and not final) then
                -- ready to read input value
                input_buffer <= BLK_IN;
                if (FINAL_IN = '1') then
                    final <= true;
                end if;
            end if;

            if (out_wr_loc = '1' and OUT_FULL = '0') then
                -- ready to write output value
                BLK_OUT <= output_buffer;
            end if;

            if (OUT_FULL = '0') then
                -- stall if output buffer is busy
                running <= '1';
            else
                running <= '0';
            end if;

        end if;

        check_reset;
    end process;

    IN_RD  <= '1' when buffer_type = W_NONE else '0';
    OUT_WR <= out_wr_loc;

end IMP;
