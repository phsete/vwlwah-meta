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

    type Word_Sequence is (W_LITERAL, W_NCLITERAL, W_0FILL, W_1FILL, W_FL, W_FLF, W_OF, W_OFF, W_OFF2, W_LF, W_LFL, W_NONE, W_WAIT);

    signal zero_fill_length:        unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal flf_zero_fill_length:    unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal one_fill_length:         unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal input_buffer:            std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal literal_buffer:          std_logic_vector(word_size-2 downto 0) := (others => 'U');
    signal snd_literal_buffer:      std_logic_vector(word_size-2 downto 0) := (others => 'U');
    signal lfl_literal_buffer:      std_logic_vector(word_size-2 downto 0) := (others => 'U');
    signal output_buffer:           std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:         std_logic := '0';
    signal out_wr_loc:              std_logic := '0';
    signal running:                 std_logic := '1';
    signal fill_words_left:         natural := 0;
    signal final:                   boolean := false;
    signal buffer_type:             Word_Sequence := W_NONE;
    signal state:                   Word_Sequence := W_NONE;

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
                flf_zero_fill_length    <= (others => '0');
                one_fill_length         <= (others => '0');
                input_buffer            <= (others => 'U');
                literal_buffer          <= (others => 'U');
                snd_literal_buffer      <= (others => 'U');
                lfl_literal_buffer      <= (others => 'U');
                output_buffer           <= (others => 'U');
                input_available         <= '0';
                out_wr_loc              <= '0';
                running                 <= '1';
                fill_words_left         <= 0;
                final                   <= false;
                buffer_type             <= W_NONE;
                state                   <= W_NONE;
            end if;
        end procedure;

        procedure check_final is
        begin
            if (final) then
                FINAL_OUT <= '1';
                --report("final");
            end if;
        end procedure;

        procedure check_input_available is
        begin
            if (buffer_type = W_NONE and IN_EMPTY = '0') then
                input_available <= '1';
            else
                input_available <= '0';
            end if;
        end procedure;
 
        procedure output_1Fill is
        begin
            report("output 1-Fill");
            -- output of 1 fill (as Literal)
            output_buffer <= (word_size-1 downto 0 => '1');
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';

            if (one_fill_length > 1) then
                -- the fill continues
                buffer_type <= W_1FILL;
                one_fill_length <= one_fill_length - 1;
            else
                -- reset counters and buffer type to read next word
                buffer_type <= W_NONE;
                one_fill_length <= to_unsigned(0, fill_counter_size);
                check_final;
            end if;
        end procedure;

        procedure output_0Fill is
        begin
            report("output 0-Fill");
            -- output of 0 fill
            output_buffer <= encode_fill_compax(word_size, fill_counter_size, zero_fill_length);
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
            -- kein extended Fill!

            buffer_type <= W_NONE;
            --check_final;
        end procedure;

        procedure output_Literal is
        begin
            report("output Literal");
            -- output of Literal
            output_buffer <= encode_literal_compax(word_size, literal_buffer);
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
            if(not is_all(snd_literal_buffer, 'U')) then
                literal_buffer <= snd_literal_buffer;
                snd_literal_buffer <= (others => 'U');
            else
                literal_buffer <= (others => 'U');
            end if;

            buffer_type <= W_NONE;
            --check_final;
        end procedure;

        procedure output_FLF is
        begin
            report("output FLF");
            -- output of FLF
            output_buffer <= encode_flf_main(word_size, literal_buffer);
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
            literal_buffer <= (others => 'U');

            buffer_type <= W_OFF;
        end procedure;

        procedure output_FF is
        begin
            -- output of FLF
            if(buffer_type = W_OFF or buffer_type = W_OF) then
                report("output fill of flf/lfl");
                output_buffer <= encode_flf_fill(word_size, zero_fill_length);
                if(buffer_type = W_OFF) then
                    buffer_type <= W_OFF2;
                else
                    buffer_type <= W_NONE;
                    check_final;
                end if;
            else
                report("output fill of flf");
                output_buffer <= encode_flf_fill(word_size, flf_zero_fill_length);
                buffer_type <= W_NONE;
                check_final;
            end if;
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
        end procedure;

        procedure output_LFL is
        begin
            report("output LFL");
            -- output of LFL
            output_buffer <= encode_lfl(word_size, literal_buffer, lfl_literal_buffer);
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
            literal_buffer <= (others => 'U');
            lfl_literal_buffer <= (others => 'U');

            buffer_type <= W_OF;
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
                        report("detected 0-Fill");
                        if(state = W_NONE) then -- set new fill length
                            state <= W_0FILL;
                            zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                        elsif(state = W_0FILL) then -- output previous 0-Fill and set new fill length
                            output_0Fill;
                            zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                        elsif(state = W_FL) then -- set second fill length and output flf (reset fill lengths?)
                            flf_zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_NONE;
                            output_FLF;
                        elsif(state = W_LITERAL) then -- set new fill length
                            state <= W_LF;
                            zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                        elsif(state = W_LF) then -- output previous Literal and 0-Fill and set new fill length
                            output_Literal;
                            buffer_type <= W_0FILL; -- outputs 0-Fill next clock cycle
                            zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_0FILL;
                        elsif(state = W_NCLITERAL) then
                            output_Literal;
                            zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_0FILL;
                        elsif(state = W_1FILL) then
                            output_1Fill;
                            zero_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_0FILL;
                        end if;
                    when W_1FILL =>
                        report("detected 1-Fill");
                        if(state = W_NONE) then -- set new fill length
                            one_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_1FILL;
                        elsif(state = W_0FILL) then -- output previous 0-Fill and set new fill length
                            output_0Fill;
                            one_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_1FILL;
                        elsif(state = W_FL) then -- set second fill length and output flf (reset fill lengths?)
                            output_0Fill;
                            buffer_type <= W_LITERAL;
                            one_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_1FILL;
                        elsif(state = W_LITERAL) then -- set new fill length
                            output_Literal;
                            one_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_1FILL;
                        elsif(state = W_LF) then -- output previous Literal and 0-Fill and set new fill length
                            output_Literal;
                            buffer_type <= W_0FILL;
                            one_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_1FILL;
                        elsif(state = W_NCLITERAL) then
                            output_Literal;
                            one_fill_length <= unsigned("00" & input_buffer(word_size-3 downto 0));
                            state <= W_1FILL;
                        end if;
                    when W_LITERAL =>
                        report("detected Literal");
                        if(state = W_NONE) then
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                state <= W_NCLITERAL;
                            else
                                state <= W_LITERAL;
                            end if;
                            literal_buffer <= input_buffer(word_size-2 downto 0);
                        elsif(state = W_0FILL) then
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                output_0Fill;
                                state <= W_NCLITERAL;
                            else
                                state <= W_FL;
                            end if;
                            literal_buffer <= input_buffer(word_size-2 downto 0);
                        elsif(state = W_FL) then
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                state <= W_NCLITERAL;
                            else
                                state <= W_LITERAL;
                            end if;
                            output_0Fill;
                            buffer_type <= W_LITERAL; -- outputs Literal next clock cycle
                            snd_literal_buffer <= input_buffer(word_size-2 downto 0);
                        elsif(state = W_LITERAL) then
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                state <= W_NCLITERAL;
                            else
                                state <= W_LITERAL;
                            end if;
                            output_Literal;
                            literal_buffer <= input_buffer(word_size-2 downto 0);
                        elsif(state = W_LF) then -- (reset literal buffers?)
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                output_Literal;
                                buffer_type <= W_0FILL;
                                literal_buffer <= input_buffer(word_size-2 downto 0);
                                state <= W_NCLITERAL;
                            else
                                state <= W_NONE;
                                lfl_literal_buffer <= input_buffer(word_size-2 downto 0);
                                buffer_type <= W_LFL;
                            end if;
                        elsif(state = W_NCLITERAL) then
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                state <= W_NCLITERAL;
                            else
                                state <= W_LITERAL;
                            end if;
                            output_Literal;
                            buffer_type <= W_NONE;
                            literal_buffer <= input_buffer(word_size-2 downto 0);
                        elsif(state = W_1FILL) then
                            if(not is_compressable(word_size, input_buffer(word_size-2 downto 0))) then
                                state <= W_NCLITERAL;
                            else
                                state <= W_LITERAL;
                            end if;
                            output_1Fill;
                            literal_buffer <= input_buffer(word_size-2 downto 0);
                        end if;
                    when others =>
                        report("Error while handling next block type!");
                end case;
            elsif (FINAL_IN = '1' and buffer_type = W_NONE) then
                -- output remaining buffers
                if (state = W_0FILL) then
                    output_0Fill;
                elsif (state = W_FL) then
                    output_0Fill;
                    buffer_type <= W_LITERAL;-- outputs Literal next clock cycle
                elsif (state = W_LITERAL or state = W_NCLITERAL) then
                    output_Literal;
                elsif (state = W_LF) then
                    output_Literal;
                    buffer_type <= W_0FILL;-- outputs 0-Fill next clock cycle
                elsif (state = W_1FILL) then
                    output_1Fill;
                else
                    --out_wr_loc <= '0';
                end if;
                state <= W_NONE;
                check_final;
            end if;
        end procedure;

    begin

        --
        -- rising edge
        --
        if (CLK'event and CLK='1') then
            if(buffer_type /= W_OFF and buffer_type /= W_OFF2) then
                out_wr_loc <= '0';
            end if;
            if (running = '1') then
                case buffer_type is
                    when W_NONE =>
                        handle_next_block;
                    when W_1FILL =>
                        output_1Fill;
                    when W_0FILL =>
                        output_0Fill;
                    when W_LITERAL =>
                        output_Literal;
                    when W_OFF =>
                        output_FF;
                    when W_OFF2 =>
                        output_FF;
                    when W_OF =>
                        output_FF;
                    when W_LFL =>
                        output_LFL;
                    when others =>
                end case;

                if(buffer_type = W_NONE) then
                    check_input_available;
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
            end if;

            if (FINAL_IN = '1' and state = W_NONE) then
                final <= true;
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
