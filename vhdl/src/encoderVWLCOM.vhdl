library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
use work.utils.all;

entity encoderVWLCOM is
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
end encoderVWLCOM;

architecture IMP of encoderVWLCOM is

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

    type Word_Sequence is (W_LITERAL, W_NCLITERAL, W_0FILL, W_1FILL, W_FL, W_FLF, W_EF, W_LF, W_LFL, W_NONE, W_WAIT);

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
    signal ceiled_eighth:           natural := 0;
    signal flf_of_size:             natural := 0;
    signal lfl_of_size:             natural := 0;
    signal skip_check_input:        boolean := false;

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
                ceiled_eighth           <= 0;
                skip_check_input        <= false;
            end if;
        end procedure;

        procedure check_final is
        begin
            if (final) then
                FINAL_OUT <= '1';
                report("final");
            end if;
        end procedure;

        procedure check_input_available is
        begin
            if ((buffer_type = W_NONE and IN_EMPTY = '0') or (skip_check_input and IN_EMPTY = '0')) then
                input_available <= '1';
            else
                input_available <= '0';
            end if;
            if(buffer_type = W_NONE) then
                skip_check_input <= false;
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
            output_buffer <= encode_fill_vwlcom(word_size, fill_counter_size, zero_fill_length, fill_words_left - 1);
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';

            if (fill_words_left > 1) then
                -- the fill continues
                buffer_type <= W_0FILL;
                fill_words_left <= fill_words_left - 1;
            else
                -- reset counters and buffer type to read next word
                buffer_type <= W_NONE;
                zero_fill_length <= to_unsigned(0, fill_counter_size);
                fill_words_left <= 0;
            end if;
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
            variable first_overflow: std_logic_vector(flf_of_size downto 0);
            variable second_overflow: std_logic_vector(flf_of_size downto 0);
        begin
            report("output FLF");
            if(zero_fill_length < unsigned(std_logic_vector(to_unsigned(1, 1)) & std_logic_vector(to_unsigned(0, (word_size-8-ceiled_eighth)/2)))) then
                if(unsigned(input_buffer(word_size-3 downto 0)) < unsigned(std_logic_vector(to_unsigned(1, 1)) & std_logic_vector(to_unsigned(0, (word_size-8-ceiled_eighth)/2)))) then
                    -- output of FLF -> no extended Fills
                    output_buffer <= encode_flf_vwlcom(word_size, literal_buffer, zero_fill_length, unsigned(input_buffer(word_size-3 downto 0)), false, false);
                    buffer_type <= W_NONE;
                    check_final;
                else
                    -- output of FLF -> second extended Fill
                    second_overflow := (others => '0');
                    second_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size) := (others => '1');
                    second_overflow := std_logic_vector(second_overflow) and std_logic_vector(to_unsigned(to_integer(unsigned(input_buffer(word_size-3 downto 0))), word_size+(word_size-8-ceiled_eighth)/2));
                    output_buffer <= encode_flf_vwlcom(word_size, literal_buffer, zero_fill_length, unsigned(second_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size)), false, true);
                    buffer_type <= W_NONE;
                    --check_final;
                end if;
            else
                if(unsigned(input_buffer(word_size-3 downto 0)) < unsigned(std_logic_vector(to_unsigned(1, 1)) & std_logic_vector(to_unsigned(0, (word_size-8-ceiled_eighth)/2)))) then
                    -- output of FLF -> first extended Fill
                    first_overflow := (others => '0');
                    first_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size) := (others => '1');
                    first_overflow := std_logic_vector(first_overflow) and std_logic_vector(to_unsigned(to_integer(zero_fill_length), word_size+(word_size-8-ceiled_eighth)/2));
                    output_buffer <= encode_flf_vwlcom(word_size, literal_buffer, unsigned(first_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size)), unsigned(input_buffer(word_size-3 downto 0)), true, false);
                    buffer_type <= W_EF;
                    --check_final;
                else
                    -- output of FLF -> both extended Fills
                    first_overflow := (others => '0');
                    first_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size) := (others => '1');
                    first_overflow := std_logic_vector(first_overflow) and std_logic_vector(to_unsigned(to_integer(zero_fill_length), word_size+(word_size-8-ceiled_eighth)/2));
                    second_overflow := (others => '0');
                    second_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size) := (others => '1');
                    second_overflow := std_logic_vector(second_overflow) and std_logic_vector(to_unsigned(to_integer(unsigned(input_buffer(word_size-3 downto 0))), word_size+(word_size-8-ceiled_eighth)/2));
                    output_buffer <= encode_flf_vwlcom(word_size, literal_buffer, unsigned(first_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size)), unsigned(second_overflow(word_size+(word_size-8-ceiled_eighth)/2-1 downto word_size)), true, true);
                    buffer_type <= W_EF;
                    --check_final;
                end if;
            end if;
            
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
            literal_buffer <= (others => 'U');
        end procedure;

        procedure output_EF is
        begin
            report("would output extended Fill here ...");
        end procedure;

        procedure output_LFL is
            variable overflow: std_logic_vector(lfl_of_size downto 0);
        begin
            report("output LFL");

            if(zero_fill_length < unsigned(std_logic_vector(to_unsigned(1, 1)) & std_logic_vector(to_unsigned(0, word_size-10-ceiled_eighth*2)))) then
                -- output of LFL
                output_buffer <= encode_lfl_vwlcom(word_size, literal_buffer, input_buffer(word_size-2 downto 0), zero_fill_length, false);
                buffer_type <= W_NONE;
                check_final;
            else
                -- fill is to long
                -- output of LFL
                overflow := (others => '0');
                overflow(2*word_size-11-ceiled_eighth*2 downto word_size) := (others => '1');
                overflow := std_logic_vector(overflow) and std_logic_vector(to_unsigned(to_integer(zero_fill_length), 2*word_size-10-ceiled_eighth*2));
                output_buffer <= encode_lfl_vwlcom(word_size, literal_buffer, input_buffer(word_size-2 downto 0), unsigned(overflow(2*word_size-11-ceiled_eighth*2 downto word_size)), true);
                zero_fill_length <= zero_fill_length(word_size-1 downto 0);
                buffer_type <= W_EF;
                skip_check_input <= true;
            end if;
            
            -- write by default, set to '0' otherwise
            out_wr_loc <= '1';
            literal_buffer <= (others => 'U');
            lfl_literal_buffer <= (others => 'U');
        end procedure;

        --
        -- looks at the input buffer and calls the appropriate procedure
        --
        procedure handle_next_block is
            variable left_temp: unsigned(fill_counter_size-1 downto 0);
        begin
            if (input_available = '1') then
                report("check");
                -- ready to read input value
                case parse_vwlwah_block_type(word_size, input_buffer) is
                    when W_0FILL =>
                        report("detected 0-Fill");
                        if(state = W_NONE) then -- set new fill length
                            state <= W_0FILL;
                            zero_fill_length(word_size-3 downto 0) <= unsigned(input_buffer(word_size-3 downto 0));
                            fill_words_left <= 1;
                        elsif(state = W_0FILL) then -- output previous 0-Fill and set new fill length
                            left_temp := zero_fill_length;
                            left_temp := shift_left(left_temp, word_size-2);
                            left_temp(word_size-3 downto 0) := unsigned(input_buffer(word_size-3 downto 0));
                            fill_words_left <= fill_words_needed(word_size, fill_counter_size, left_temp);
                            zero_fill_length <= left_temp;
                        elsif(state = W_FL) then -- set second fill length and output flf (reset fill lengths?)
                            flf_zero_fill_length(word_size-3 downto 0) <= unsigned(input_buffer(word_size-3 downto 0));
                            state <= W_NONE;
                            output_FLF;
                        elsif(state = W_LITERAL) then -- set new fill length
                            state <= W_LF;
                            zero_fill_length(word_size-3 downto 0) <= unsigned(input_buffer(word_size-3 downto 0));
                        elsif(state = W_LF) then -- output previous Literal and 0-Fill and set new fill length
                            output_Literal;
                            left_temp := zero_fill_length;
                            left_temp := shift_left(left_temp, word_size-2);
                            left_temp(word_size-3 downto 0) := unsigned(input_buffer(word_size-3 downto 0));
                            fill_words_left <= fill_words_needed(word_size, fill_counter_size, left_temp);
                            zero_fill_length <= left_temp;
                            state <= W_0FILL;
                        elsif(state = W_NCLITERAL) then
                            output_Literal;
                            zero_fill_length(word_size-3 downto 0) <= unsigned(input_buffer(word_size-3 downto 0));
                            state <= W_0FILL;
                        elsif(state = W_1FILL) then
                            output_1Fill;
                            zero_fill_length(word_size-3 downto 0) <= unsigned(input_buffer(word_size-3 downto 0));
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
                                output_LFL;
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
            elsif (final) then
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

        ceiled_eighth <= word_size/8;
        if(word_size > ceiled_eighth*8) then
            ceiled_eighth <= ceiled_eighth + 1;
        end if;

        flf_of_size <= word_size+(word_size-8-ceiled_eighth)/2-1;
        lfl_of_size <= 2*word_size-11-ceiled_eighth*2;

        --
        -- rising edge
        --
        if (CLK'event and CLK='1') then
            out_wr_loc <= '0';
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
                    when others =>
                end case;

                check_input_available;
            end if;
        end if;

        --
        -- falling edge
        --
        if (CLK'event and CLK='0') then            
            if (input_available = '1' and not final) then
                -- ready to read input value
                input_buffer <= BLK_IN;
                if (FINAL_IN = '1' and buffer_type = W_NONE) then
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
