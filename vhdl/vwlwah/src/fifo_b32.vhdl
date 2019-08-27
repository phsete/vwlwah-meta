-- inspired by: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity FIFO_B32 is
    Generic (
        addr_width:             natural := 8;
        word_size:              natural := 8
    );
    Port (
        CLK:                    in  std_logic;
        RESET:                  in  std_logic;
        FINAL_IN:               in  std_logic;
        BLK_IN:                 in  std_logic_vector (word_size-1 downto 0);
        WR_EN:                  in  std_logic;
        RD_EN:                  in  std_logic;
        BLK_OUT:                out std_logic_vector (31 downto 0);
        EMPTY:                  out std_logic;
        FULL:                   out std_logic;
        ALMOST_FULL:            out std_logic;
        FINAL_OUT:              out std_logic
    );
end FIFO_B32;

architecture IMP of FIFO_B32 is

    signal wrcnt : unsigned (addr_width-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (addr_width-1 downto 0) := (others => '0');
    signal wrpos : unsigned (4 downto 0) := (others => '1'); -- next free bit index (=31)
    signal final: boolean := false;
    signal final_idx : unsigned(addr_width-1 downto 0) := (others => '0');
    type speicher is array(0 to (2**addr_width)-1) of unsigned(31 downto 0);
    signal memory : speicher;   
    signal final_out_loc : std_logic;
    signal full_loc  : std_logic;
    signal almost_full_loc  : std_logic;
    signal empty_loc : std_logic;

begin
    process (CLK)
        
        ----------------------
        -- IMPURE FUNCTIONS --
        ----------------------

        --
        -- returns the next 32-bit word from the buffer
        --
        impure function read_next
        return std_logic_vector is
        begin
            rdcnt <= rdcnt+1;
            return std_logic_vector(memory(to_integer(rdcnt)));
        end read_next;

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- appends a new b-bit word at the end of the buffer
        --

        procedure write_next (data_in: std_logic_vector(word_size-1 downto 0)) is
            variable word1: std_logic_vector(31 downto 0);
            variable word2: std_logic_vector(31 downto 0);
            variable new_word1_mask: std_logic_vector(31 downto 0) := (others => '0');
            variable new_word2_mask: std_logic_vector(31 downto 0) := (others => '0');
            variable new_word1: std_logic_vector(31 downto 0) := (others => '0');
            variable new_word2: std_logic_vector(31 downto 0) := (others => '0');
            variable bits_in_word1: natural;
            variable bits_in_word2: natural;
        begin
            -- read values from memory
            word1 := std_logic_vector(memory(to_integer(wrcnt)));
            word2 := std_logic_vector(memory(to_integer(wrcnt+1)));
            -- right align input data in new_word1
            new_word1(word_size-1 downto 0) := data_in;
            new_word1_mask(word_size-1 downto 0) := (others => '1');
            -- left align input data in new_word2
            new_word2(31 downto 32 - word_size) := data_in;
            new_word2_mask(31 downto 32 - word_size) := (others => '1');
            if (to_integer(wrpos) >= word_size-1) then -- enough bits left in word1
                new_word1 := std_logic_vector(shift_left(unsigned(new_word1), to_integer(wrpos) + 1 - word_size));
                new_word1_mask := std_logic_vector(shift_left(unsigned(new_word1_mask), to_integer(wrpos) + 1 - word_size));
                word1 := word1 AND NOT(new_word1_mask); -- set relevant bits to 0 in order to overwrite them
                word1 := word1 OR new_word1;
                memory(to_integer(wrcnt)) <= unsigned(word1);

                if (to_integer(wrpos) = word_size-1) then -- exactly enough bits left in word1
                    wrcnt <= wrcnt+1;
                end if;
            else
                bits_in_word1 := to_integer(wrpos) + 1;
                bits_in_word2 := word_size - bits_in_word1;

                new_word1 := std_logic_vector(shift_right(unsigned(new_word1), word_size - bits_in_word1));
                new_word1_mask := std_logic_vector(shift_right(unsigned(new_word1_mask), word_size - bits_in_word1));
                word1 := word1 AND NOT(new_word1_mask); -- set relevant bits to 0 in order to overwrite them
                word1 := word1 OR new_word1;
                memory(to_integer(wrcnt)) <= unsigned(word1);

                new_word2 := std_logic_vector(shift_left(unsigned(new_word2), word_size - bits_in_word2));
                new_word2_mask := std_logic_vector(shift_left(unsigned(new_word2_mask), word_size - bits_in_word2));
                word2 := word2 AND NOT(new_word2_mask);
                word2 := word2 OR new_word2;
                memory(to_integer(wrcnt + 1)) <= unsigned(word2);

                wrcnt <= wrcnt+1;
            end if;
            wrpos <= wrpos - word_size;
        end procedure;

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_reset is
        begin
            if (RESET = '0') then
                rdcnt       <= (others => '0');
                wrcnt       <= (others => '0');
                wrpos       <= (others => '1');
                final_idx   <= (others => '0');
                final       <= false;
            end if;
        end procedure;

    begin
        assert word_size <= 32
        report "FIFO_b32: b must not be greater than 32 (is " & natural'image(word_size) & " )"  severity error;

        if (CLK'event and CLK = '1') then
            if (WR_EN = '1' and full_loc = '0' and not final) then
                -- write input to buffer
                write_next(BLK_IN);

                if (FINAL_IN = '1') then
                    final <= true;
                    final_idx <= wrcnt;
                end if;
            end if;

            if (RD_EN = '1' and empty_loc = '0' and final_out_loc = '0') then
                -- read output from buffer
                BLK_OUT <= read_next;
            end if;
        end if;

        check_reset;
    end process;

    final_out_loc   <= '1' when (final and (rdcnt = final_idx +1))  else '0';
    full_loc        <= '1' when (rdcnt = wrcnt+1)                   else '0';
    almost_full_loc <= '1' when (rdcnt = wrcnt+2)                   else '0';
    empty_loc       <= '1' when (rdcnt = wrcnt)                     else '0';
    FINAL_OUT       <= final_out_loc;
    FULL            <= full_loc;
    ALMOST_FULL     <= almost_full_loc;
    EMPTY           <= empty_loc;
end IMP;
