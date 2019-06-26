library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;

entity inverter is
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
        FINAL_OUT:          out std_logiC
    );
end inverter;

architecture IMP of inverter is

    type Word is (W_NONE, W_0FILL, W_1FILL, W_LITERAL);

    signal current_word:        std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal output_buffer:       std_logic_vector(word_size-1 downto 0) := (others => 'U');
    signal input_available:     std_logic := '0';
    signal running:             std_logic := '1';
    signal final:               boolean := false;
    signal current_type:        Word := W_NONE;
    signal IN_RD_loc:           std_logic;
    signal out_wr_loc:          std_logic;

begin
    process (CLK)

        --------------
        -- FUNCTIOS --
        --------------

        --
        -- determine the word type of input_word by parsing the control bits
        -- (MSB for literals and MSB, MSB-1 for fills)
        --
        function parse_word_type (input_word: std_logic_vector(word_size-1 downto 0))
        return Word is
        begin
            if input_word(word_size-1) = '0' then
                return W_LITERAL;
            elsif input_word(word_size-2) = '0' then
                return W_0FILL;
            elsif input_word(word_size-2) = '1' then
                return W_1FILL;
            else
                return W_NONE;
            end if;
        end parse_word_type;

        --
        -- returns the inverse version of the input fill
        --
        function invert_F (input: std_logic_vector(word_size-1 downto 0))
        return std_logic_vector is
            variable output: std_logic_vector(word_size-1 downto 0) := (others => 'U');
        begin
            output(word_size-1) := input(word_size-1);
            output(word_size-2) := not(input(word_size-2));
            output(word_size-3 downto 0) := input(word_size-3 downto 0);
            return output;
        end invert_F;

        --
        -- returns the inverse version of the input literal
        --
        function invert_L (input: std_logic_vector(word_size-1 downto 0))
        return std_logic_vector is
            variable output: std_logic_vector(word_size-1 downto 0);
        begin
            output(word_size-1) := input(word_size-1);
            for i in word_size-2 downto 0 loop
                output(i) := not(input(i));
            end loop;
            return output;
        end invert_L;

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_reset is
        begin
            if (RESET = '1') then
                current_word    <= (others => 'U');
                output_buffer   <= (others => 'U');
                input_available <= '0';
                running         <= '1';
                current_type    <= W_NONE;
                final           <= false;
            end if;
        end procedure;

    begin
        --
        -- rising edge
        --
        if (CLK'event and CLK = '1' and running = '1') then
            case current_type is
                when W_0FILL | W_1FILL =>
                    output_buffer <= invert_F(current_word);
                    out_wr_loc <= '1';
                when W_LITERAL =>
                    output_buffer <= invert_L(current_word);
                    out_wr_loc <= '1';
                when others =>
                    out_wr_loc <= '0';
            end case;

            input_available <= not(IN_EMPTY);

            if (FINAL_IN = '1') then
                final <= true;
            end if;
        end if;

        --
        -- falling edge
        --
        if (CLK'event and CLK = '0') then
            -- read the next word and push buffers forward
            if (input_available = '1' and not final) then
                current_word <= BLK_IN;
                current_type <= parse_word_type(BLK_IN);
            else
                current_word <= (others => 'U');
                current_type <= W_NONE;
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

    end process;

    FINAL_OUT <= '1' when final else '0';
    IN_RD  <= '1';
    OUT_WR <= out_wr_loc;

end IMP;
