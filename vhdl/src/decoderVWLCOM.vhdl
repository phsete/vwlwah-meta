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

        -- check if state is final and finished and output FINAL_OUT signal
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
            -- prepare to output the current Fill word
            output_buffer <= decode_flf_f_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLF_L;
        end procedure;

        --
        -- handles decoding of the current FLF Literal word
        --
        procedure handle_FLF_L is
        begin
            report("FLF_L");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_l_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLF_F2;
        end procedure;

        --
        -- handles decoding of the current FLF second Fill word
        --
        procedure handle_FLF_F is
        begin
            report("FLF_F2");
            -- prepare to output the current Fill word
            output_buffer <= decode_flf_f2_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;
        
        --
        -- prepares decoding of an FLF word if both Fills are extended
        --
        procedure handle_FLFe is
        begin
            flf_buffer <= input_buffer;
            OUT_WR_loc <= '0';
            buffer_type <= W_FLFe_Fe;
        end procedure;
        
        --
        -- handles decoding of the current FLF first Fill extension when both Fills are extended
        --
        procedure handle_FLFe_e is
        begin
            report("FLF_F1");
            -- prepare to output the current fill extension
            output_buffer <= decode_flf_fe_vwlcom(word_size, flf_buffer, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe_L;
        end procedure;
        
        --
        -- handles decoding of the current FLF Literal word when both Fills are extended
        --
        procedure handle_FLFe_L is
        begin
            report("FLF_L");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_l_vwlcom(word_size, flf_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe_F2e;
        end procedure;

        --
        -- handles decoding of the current FLF second Fill word and its extension when both Fills are extended
        --
        procedure handle_FLFe_F is
        begin
            report("FLF_F2");
            -- prepare to output the current fill extension
            output_buffer <= decode_flf_f2e_vwlcom(word_size, flf_buffer, input_buffer);
            flf_buffer <= (others => 'U');
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;

        --
        -- handles decoding of the current FLF first Fill word when the first Fill is extended
        --
        procedure handle_FLFe1 is
        begin
            report("FLF_F1");
            -- prepare to output the current literal word
            flf_buffer <= input_buffer;
            output_buffer <= decode_flf_f_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe1_Fe;
        end procedure;
        
        --
        -- handles decoding of the current FLF first Fill extension when the first Fill is extended
        --
        procedure handle_FLFe1_e is
        begin
            report("FLF_F1e");
            -- prepare to output the current fill extension
            output_buffer <= decode_fill_compax(word_size, '0', input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe1_L;
        end procedure;
        
        --
        -- handles decoding of the current FLF Literal word when the first Fill is extended
        --
        procedure handle_FLFe1_L is
        begin
            report("FLF_L");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_l_vwlcom(word_size, flf_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe1_F2;
        end procedure;

        --
        -- handles decoding of the current FLF second Fill word when the first Fill is extended
        --
        procedure handle_FLFe1_F is
        begin
            report("FLF_F2");
            -- prepare to output the current fill extension
            output_buffer <= decode_flf_f2_vwlcom(word_size, flf_buffer);
            flf_buffer <= (others => 'U');
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;

        --
        -- handles decoding of the current FLF first Fill word when the second Fill is extended
        --
        procedure handle_FLFe2 is
        begin
            report("FLF_F1");
            -- prepare to output the current literal word
            flf_buffer <= input_buffer;
            output_buffer <= decode_flf_f_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe2_L;
        end procedure;
        
        --
        -- handles decoding of the current FLF Literal word when the second Fill is extended
        --
        procedure handle_FLFe2_L is
        begin
            report("FLF_L");
            -- prepare to output the current literal word
            output_buffer <= decode_flf_l_vwlcom(word_size, flf_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe2_F2;
        end procedure;

        --
        -- handles decoding of the current FLF second Fill word when the second Fill is extended
        --
        procedure handle_FLFe2_F is
        begin
            report("FLF_F2");
            -- prepare to output the current fill
            output_buffer <= decode_flf_f2_vwlcom(word_size, flf_buffer);
            flf_buffer <= (others => 'U');
            OUT_WR_loc <= '1';
            buffer_type <= W_FLFe2_F2e;
        end procedure;

        --
        -- handles decoding of the current FLF second Fill extension when the second Fill is extended
        --
        procedure handle_FLFe2_Fe is
        begin
            report("FLF_F2e");
            -- prepare to output the current fill extension
            output_buffer <= decode_fill_compax(word_size, '0', input_buffer);
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
            output_buffer <= decode_lfl_l_vwlcom(word_size, input_buffer);
            buffer_type <= W_LFL_F;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current LFL Fill word
        --
        procedure handle_LFL_F is
        begin
            report("LFL_F");
            -- prepare to output the current fill
            output_buffer <= decode_lfl_f_vwlcom(word_size, input_buffer);
            buffer_type <= W_LFL_L2;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current LFL second Literal word
        --
        procedure handle_LFL_L2 is
        begin
            report("LFL_L2");
            -- prepare to output the current literal word
            output_buffer <= decode_lfl_l2_vwlcom(word_size, input_buffer);
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;

        --
        -- handles decoding of the current LFL word when the Fill is extended
        --
        procedure handle_LFLe is
        begin
            report("LFL_L1");
            -- prepare to output the current literal word
            lfl_buffer <= input_buffer;
            output_buffer <= decode_lfl_l_vwlcom(word_size, input_buffer);
            buffer_type <= W_LFL_Fe;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current LFL Fill word when the Fill is extended
        --
        procedure handle_LFLe_F is
        begin
            report("LFL_F");
            -- prepare to output the current fill
            output_buffer <= decode_lfl_f_vwlcom(word_size, input_buffer);
            buffer_type <= W_LFL_FLe;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current LFL Fill extension when the Fill is extended
        --
        procedure handle_LFLe_Fe is
        begin
            report("LFL_Fe");
            -- prepare to output the current fill extension
            output_buffer <= decode_fill_compax(word_size, '0', input_buffer);
            buffer_type <= W_LFLe_L2;
            OUT_WR_loc <= '1';
        end procedure;

        --
        -- handles decoding of the current LFL second Literal word when the Fill is extended
        --
        procedure handle_LFLe_L2 is
        begin
            report("LFL_L2");
            -- prepare to output the current literal word
            output_buffer <= decode_lfl_l2_vwlcom(word_size, lfl_buffer);
            lfl_buffer <= (others => 'U');
            OUT_WR_loc <= '1';
            buffer_type <= W_NONE;
            check_final;
        end procedure;

        --
        -- process the next word and load it into buffer
        --
        procedure handle_next_word (set_buffer_type: boolean) is
        begin
            if (IN_RD_loc = '1' and running = '1' and (input_available = '1' or final) and not finished) then
                -- read the next word and push buffers forward
                input_buffer <= BLK_IN;
                if(set_buffer_type) then
                    buffer_type <= parse_word_type_vwlcom(word_size, BLK_IN);
                end if;

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
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_LITERAL =>
                        handle_L;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_FLF =>
                        handle_FLF;
                    when W_FLF_L =>
                        handle_FLF_L;
                    when W_FLF_F2 =>
                        handle_FLF_F;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_FLFe =>
                        handle_FLFe;
                        -- preload next word into buffer to finish fill
                        handle_next_word(false);
                    when W_FLFe_Fe =>
                        handle_FLFe_e;
                    when W_FLFe_L =>
                        handle_FLFe_L;
                    when W_FLFe_F2e =>
                        handle_FLFe_F;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_FLFe1 =>
                        handle_FLFe1;
                        -- preload next word into buffer to finish fill
                        handle_next_word(false);
                    when W_FLFe1_Fe =>
                        handle_FLFe1_e;
                    when W_FLFe1_L =>
                        handle_FLFe1_L;
                    when W_FLFe1_F2 =>
                        handle_FLFe1_F;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_FLFe2 =>
                        handle_FLFe2;
                    when W_FLFe2_L =>
                        handle_FLFe2_L;
                    when W_FLFe2_F2 =>
                        handle_FLFe2_F;
                        -- preload next word into buffer to finish fill
                        handle_next_word(false);
                    when W_FLFe2_F2e =>
                        handle_FLFe2_Fe;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_LFL =>
                        handle_LFL;
                    when W_LFL_F =>
                        handle_LFL_F;
                    when W_LFL_L2 =>
                        handle_LFL_L2;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_LFLe =>
                        handle_LFLe;
                    when W_LFL_Fe =>
                        handle_LFLe_F;
                        -- preload next word into buffer to finish fill
                        handle_next_word(false);
                    when W_LFL_FLe =>
                        handle_LFLe_Fe;
                    when W_LFLe_L2 =>
                        handle_LFLe_L2;
                        -- preload next word into buffer
                        handle_next_word(true);
                    when W_NONE =>
                    -- preload next word into buffer
                        handle_next_word(true);
                    when others =>
                end case;
            end if;

            input_available <= not(IN_EMPTY);
        end if;

        -- falling clock signal
        -- reads inputs, steps buffer pipeline forward and determines future read state
        if (CLK'event and CLK='0') then
            -- set IN_RD when ready to read next word
            if(buffer_type = W_NONE or buffer_type = W_0FILL or buffer_type = W_LITERAL or buffer_type = W_FLF_F2 or buffer_type = W_LFL_L2 or buffer_type = W_LFLe_L2 or buffer_type = W_LFL_Fe or buffer_type = W_FLFe or buffer_type = W_FLFe_F2e or buffer_type = W_FLFe2_F2 or buffer_type = W_FLFe2_F2e or buffer_type = W_FLFe1_F2 or buffer_type = W_FLFe1) then
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
