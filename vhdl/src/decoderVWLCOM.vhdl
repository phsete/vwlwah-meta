library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;
use work.utils.all;

entity decoderVWLCOM is
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
end decoderVWLCOM;

architecture IMP of decoderVWLCOM is

    signal input_fill_length:   unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal output_fill_length:  unsigned(fill_counter_size-1 downto 0) := (others => '0');
    signal input_buffer:        std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:     std_logic := '0';
    signal running:             std_logic := '1';
    signal final:               boolean := false;
    signal finished:            boolean := false;
    signal IN_RD_loc:           std_logic;
    signal OUT_WR_loc:          std_logic;
    signal buffer_type:         CompaxWord := W_NONE;
    signal lfl_buffer:          std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal flf_buffer:          std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal lfle_buffer:         std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal flfe_buffer:         std_logic_vector(word_size-1 downto 0) := (others => 'U');

begin
    process (CLK)

        ----------------
        -- PROCEDURES --
        ----------------

        procedure check_final is
        begin
            if(final and finished) then
                FINAL_OUT <= '1';
            end if;
        end procedure;

        --
        -- handles decoding of the current fill word
        --
        procedure handle_F is
        begin
            report("Fill");
            output_buffer <= decode_fill_compax(word_size, '0', input_buffer);
            buffer_type <= W_NONE;
            check_final;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current literal word
        --
        procedure handle_L is
        begin
            report("Literal");
            -- prepare to output the current literal word
            output_buffer <= decode_literal_compax(word_size, input_buffer);
            buffer_type <= W_NONE;
            check_final;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current FLF word
        --
        procedure handle_FLF is
        begin
            report("FLF_F1");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_f_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLF_L;
        end procedure;

        procedure handle_FLF_L is
        begin
            report("FLF_L");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_l_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLF_F2;
        end procedure;

        procedure handle_FLF_F is
        begin
            report("FLF_F2");
            output_buffer <= decode_flf_f2_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;

        --
        -- handles decoding of the current LFL word
        --
        procedure handle_LFL is
        begin
            report("LFL_L1");
            -- prepare to output the current literal word
            lfl_buffer <= input_buffer;
            output_buffer <= decode_lfl_compax(word_size, input_buffer, 1);
            buffer_type <= W_LFL_F;
            OUT_WR_loc <= '1';
        end procedure;

        procedure handle_LFL_F is
        begin
            report("LFL_F");
            -- prepare to output the current fill
            output_buffer <= decode_lfl_f_compax(word_size, input_buffer);
            buffer_type <= W_LFL_L2;
            OUT_WR_loc <= '1';
        end procedure;

        procedure handle_LFL_L2 is
        begin
            report("LFL_L2");
            -- prepare to output the current literal word
            output_buffer <= lfl_buffer;
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;

        procedure handle_next_word is
        begin
            report("next word");
            if (IN_RD_loc = '1' and running = '1' and (input_available = '1' or final) and not finished) then
                -- read the next word and push buffers forward
                input_buffer <= BLK_IN;
                buffer_type <= parse_word_type_vwlcom(word_size, BLK_IN);

                if(final) then
                    finished <= true;
                end if;
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
                finished            <= false;
                buffer_type         <= W_NONE;
                lfl_buffer          <= (others => 'U');
                flf_buffer          <= (others => 'U');
                lfle_buffer         <= (others => 'U');
                flfe_buffer         <= (others => 'U');
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

                case buffer_type is
                    when W_0FILL =>
                        handle_F;
                    when W_LITERAL =>
                        handle_L;
                    when W_FLF =>
                        handle_FLF;
                    when W_FLF_L =>
                        handle_FLF_L;
                    when W_FLF_F2 =>
                        handle_FLF_F;
                        handle_next_word;
                    when W_LFL =>
                        handle_LFL;
                    when W_LFL_F =>
                        handle_LFL_F;
                    when W_LFL_L2 =>
                        handle_LFL_L2;
                        handle_next_word;
                    when W_NONE =>
                        handle_next_word;
                    when others =>
                end case;
            end if;

            input_available <= not(IN_EMPTY);
        end if;

        -- falling clock signal
        -- reads inputs, steps buffer pipeline forward and determines future read state
        if (CLK'event and CLK='0') then
            if(buffer_type = W_NONE or buffer_type = W_0FILL or buffer_type = W_LITERAL or buffer_type = W_FLF_F2 or buffer_type = W_LFL_L2) then
                IN_RD_loc <= '1';
            else
                IN_RD_loc <= '0';
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

            if (FINAL_IN = '1') then
                final <= true;
            end if;
        end if;

        -- wait for a RESET signal
        check_RESET;
    end process;

    IN_RD  <= IN_RD_loc;
    OUT_WR <= OUT_WR_loc;

end IMP;
