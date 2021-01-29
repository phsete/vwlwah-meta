library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
use work.utils.all;

entity decoderMeta is
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
end decoderMeta;

architecture IMP of decoderMeta is

    signal input_fill_length:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal input_buffer:        std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:     std_logic := '0';
    signal running:             std_logic := '1';
    signal final:               boolean := false;
    signal reset_buffer_type:   boolean := false;
    signal IN_RD_loc:           std_logic;
    signal OUT_WR_loc:          std_logic;
    signal buffer_type:         CompaxWord := W_NONE;
    signal state:               CompaxWord := W_NONE;
    signal lfl_buffer:          std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal flf_buffer:          std_logic_vector(word_size-1 downto 0) := (others => 'U');

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
            report("Fill");
            output_buffer <= decode_fill_compax(word_size, fill_type, input_buffer);
            if(fill_type = '1' and buffer_type = W_1FILL) then
                state <= W_1FILL;
                input_fill_length <= input_fill_length + 1;
                report("1-Fill");
            else
                OUT_WR_loc <= '1';
                reset_buffer_type <= true;
                if (final) then
                    -- mark the end of all output
                    FINAL_OUT <= '1';
                end if;
            end if;
        end procedure;

        procedure output_1Fill is
        begin
            report("out 1-Fill");
            output_buffer <= encode_fill(word_size, fill_counter_size, '1', input_fill_length, 0);
            input_fill_length <= (others => '0');
            state <= W_NONE;
            OUT_WR_loc <= '1';
            if (final) then
                -- mark the end of all output
                FINAL_OUT <= '1';
            end if;
        end procedure;

        --
        -- handles decoding of the current literal word
        --
        procedure handle_L is
        begin
            report("Literal");
            -- prepare to output the current literal word
            output_buffer <= decode_literal_compax(word_size, input_buffer);
            OUT_WR_loc <= '1';
            reset_buffer_type <= true;

            if (final) then
                -- mark the end of all output
                FINAL_OUT <= '1';
            end if;
        end procedure;

        --
        -- handles decoding of the current FLF word
        --
        procedure handle_FLF is
        begin
            report("FLF");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_f_compax(word_size, input_buffer);
            OUT_WR_loc <= '1';
            state <= W_FLF_F2;

            if (final) then
                -- mark the end of all output
                FINAL_OUT <= '1';
            end if;
        end procedure;

        procedure handle_FLF_F (fill_no: natural) is
        begin
            report("FLF_F" & integer'image(fill_no));
            -- prepare to output the current literal word           
            if fill_no = 0 then
                flf_buffer <= decode_flf_compax(word_size, input_buffer);
                state <= W_FLF;
            elsif fill_no = 2 then
                output_buffer <= flf_buffer;
                OUT_WR_loc <= '1';
                state <= W_FLF_L;
            else
                output_buffer <= decode_flf_f_compax(word_size, input_buffer);
                OUT_WR_loc <= '1';
                state <= W_NONE;
                reset_buffer_type <= true;
            end if;

            if (final) then
                -- mark the end of all output
                FINAL_OUT <= '1';
            end if;
        end procedure;

        --
        -- handles decoding of the current LFL word
        --
        procedure handle_LFL is
        begin
            report("LFL");
            state <= W_LFL;
            -- prepare to output the current literal word
            output_buffer <= decode_lfl_compax(word_size, input_buffer, 1);
            lfl_buffer <= decode_lfl_compax(word_size, input_buffer, 0);
            OUT_WR_loc <= '1';
            reset_buffer_type <= true;
        end procedure;

        procedure handle_LFL_F is
        begin
            report("LFL_F");
            state <= W_LFL_F;
            -- prepare to output the current fill
            output_buffer <= decode_lfl_f_compax(word_size, input_buffer);
            OUT_WR_loc <= '1';
        end procedure;

        procedure handle_LFL_L2 is
        begin
            report("LFL_L2");
            -- prepare to output the current literal word
            output_buffer <= lfl_buffer;
            OUT_WR_loc <= '1';
            state <= W_NONE;

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
                input_buffer        <= (others => 'U');
                output_buffer       <= (others => 'U');
                input_available     <= '0';
                running             <= '1';
                final               <= false;
                reset_buffer_type   <= false;
                buffer_type         <= W_NONE;
                state               <= W_NONE;
            end if;
        end procedure;

    begin
        -- rising clock signal
        -- do logic and prepare output
        if (CLK'event and CLK='1') then
            -- don't write by default
            OUT_WR_loc <= '0';

            if (running = '1' and state = W_NONE) then
                -- handle the current word type

                case buffer_type is
                    when W_0FILL =>
                        handle_F('0');
                    when W_1FILL =>
                        handle_F('1');
                    when W_LITERAL =>
                        handle_L;
                    when W_FLF =>
                        handle_FLF_F(0);
                    when W_LFL =>
                        handle_LFL;
                    when others =>
                end case;
            end if;

            if(running = '1' and state /= W_NONE) then
                case state is
                    when W_FLF =>
                        handle_FLF;
                    when W_FLF_F2 =>
                        handle_FLF_F(2);
                    when W_FLF_L =>
                        handle_FLF_F(1);
                    when W_LFL =>
                        handle_LFL_F;
                    when W_LFL_F =>
                        handle_LFL_L2;
                    when W_1FILL =>
                    if(buffer_type = W_NONE) then
                        output_1Fill;
                    else
                        handle_F('1');
                    end if;
                    when others =>
                end case;
            end if;

            input_available <= not(IN_EMPTY);

            -- temporary fix in vivado for duplicate driver
            if (FINAL_IN = '1') then
                final <= true;
            end if;

        end if;

        -- falling clock signal
        -- reads inputs, steps buffer pipeline forward and determines future read state
        if (CLK'event and CLK='0') then
            if (state = W_FLF or state = W_FLF_L or state = W_LFL_F) then
                IN_RD_loc <= '0';
            else
                IN_RD_loc <= '1';
            end if;
            
            if(reset_buffer_type) then
                reset_buffer_type <= false;
                buffer_type <= W_NONE;
            end if;

            if (IN_RD_loc = '1' and running = '1' and (input_available = '1' or final)) then
                -- read the next word and push buffers forward

                if(state = W_LFL) then
                    input_buffer <= BLK_IN;
                end if;

                if (input_available = '1' and not final and not reset_buffer_type) then
                    -- there is a next word available. read it.
                    input_buffer <= BLK_IN;
                    buffer_type <= parse_word_type_compax(word_size, BLK_IN);
                end if;
            else
                -- if no word was read, check fill length to determine next read state
                if (state = W_FLF or state = W_FLF_L) then
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
