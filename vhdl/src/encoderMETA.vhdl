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

    -- found this function implementation at: https://stackoverflow.com/questions/15406887/vhdl-convert-vector-to-string
    function to_string ( a: std_logic_vector) return string is
        variable b : string (1 to a'length) := (others => NUL);
        variable stri : integer := 1; 
    begin
        for i in a'range loop
            b(stri) := std_logic'image(a((i)))(2);
            stri := stri+1;
        end loop;
        return b;
    end function;

    type Word_Sequence is (W_LITERAL, W_0FILL, W_1FILL, W_FL, W_FLF, W_LF, W_LFL, W_NONE);

    signal zero_fill_length:        unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal one_fill_length:         unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal input_buffer:            std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal literal_buffer:          std_logic_vector(word_size-2 downto 0) := (others => 'U');
    signal output_buffer:           std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:         std_logic := '0';
    signal out_wr_loc:              std_logic := '0';
    signal running:                 std_logic := '1';
    signal fill_words_left:         natural := 0;
    signal final:                   boolean := false;
    signal buffer_type:             Word_Sequence := W_NONE;
    signal previous_buffer_type:    Word_Sequence := W_NONE;

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
                zero_fill_length        <= (others => '0');
                one_fill_length         <= (others => '0');
                input_buffer            <= (others => 'U');
                literal_buffer          <= (others => 'U');
                output_buffer           <= (others => 'U');
                input_available         <= '0';
                out_wr_loc              <= '0';
                running                 <= '1';
                fill_words_left         <= 0;
                final                   <= false;
                buffer_type             <= W_NONE;
                previous_buffer_type    <= W_NONE;
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
                        zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                        if(previous_buffer_type = W_FL) then
                            buffer_type <= W_FLF;
                        elsif(previous_buffer_type = W_LITERAL) then
                            buffer_type <= W_LF;
                        else
                            buffer_type <= W_0FILL;
                        end if;
                    when W_1FILL =>
                        one_fill_length(word_size-3 downto 0) <= unsigned(input_buffer(word_size-3 downto 0));
                        buffer_type <= W_1FILL;
                    when W_LITERAL =>
                        literal_buffer <= input_buffer(word_size-2 downto 0);
                        if(previous_buffer_type = W_0FILL) then
                            buffer_type <= W_FL;
                        elsif(previous_buffer_type = W_LF) then
                            buffer_type <= W_LFL;
                        else
                            buffer_type <= W_LITERAL;
                        end if;
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

        --
        -- handle 1 Fill
        --
        procedure handle_1F is
        begin
            -- output of 1 fill (as Literal)
            output_buffer <= (word_size-1 downto 0 => '1');
            report("out:1F");

            if (one_fill_length > 1) then
                -- the fill continues
                buffer_type <= W_1FILL;
                one_fill_length <= one_fill_length - 1;
            else
                -- reset counters and buffer type to read next word
                buffer_type <= W_NONE;
                one_fill_length <= to_unsigned(0, fill_counter_size);

                if (final) then
                    FINAL_OUT <= '1';
                end if;
            end if;
        end procedure;

        --
        -- handle 0 Fill
        --
        procedure handle_0F is
        begin
            -- output of 0 fill
            output_buffer <= encode_fill_compax(word_size, fill_counter_size, zero_fill_length);
            report("out:0F");
            buffer_type <= W_NONE;

            if (final) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

        --
        -- handle Literal
        --
        procedure handle_L is
        begin
            -- output of Literal
            output_buffer <= encode_literal_compax(word_size, literal_buffer);
            report("out:L");
            buffer_type <= W_NONE;

            if (final) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

        procedure handle_FL is
        begin
            report("stat:FL");
            buffer_type <= W_NONE;

            if (final) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

        procedure handle_FLF is
        begin
            -- output_buffer setzen
            report("stat:FLF");
            buffer_type <= W_NONE;

            if (final) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

        procedure handle_LF is
        begin
            report("stat:LF");
            buffer_type <= W_NONE;

            if (final) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

        procedure handle_LFL is
        begin
            -- output_buffer setzen
            report("stat:LFL");
            buffer_type <= W_NONE;

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
                    when W_1FILL =>
                        handle_1F;
                    when W_0FILL =>
                        handle_0F;
                    when W_LITERAL =>
                        handle_L;
                    when W_FL =>
                        handle_FL;
                    when W_FLF =>
                        handle_FLF;
                    when W_LF =>
                        handle_LF;
                    when W_LFL =>
                        handle_LFL;
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
            if(buffer_type /= W_NONE) then
                previous_buffer_type <= buffer_type;
            end if;
            
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
