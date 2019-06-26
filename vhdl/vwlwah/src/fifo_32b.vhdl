-- inspired by: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity FIFO_32B is
    Generic (
        addr_width:             natural := 8;
        word_size:              natural := 8
    );
    Port (
        CLK:                    in  std_logic;
        RESET:                  in  std_logic;
        FINAL_IN:               in  std_logic;
        BLK_IN:                 in  std_logic_vector (31 downto 0);
        WR_EN:                  in  std_logic;
        RD_EN:                  in  std_logic;
        BLK_OUT:                out std_logic_vector (word_size-1 downto 0);
        EMPTY:                  out std_logic;
        FULL:                   out std_logic;
        FINAL_OUT:              out std_logic
    );
end FIFO_32B;

architecture IMP of FIFO_32B is

    signal wrcnt : unsigned (addr_width-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (addr_width-1 downto 0) := (others => '0');
    signal rdpos : unsigned (4 downto 0) := (others => '1'); -- next unread bit index
    signal final_idx: unsigned (addr_width-1 downto 0) := (others => '0');
    signal final : boolean := false;
    type speicher is array(0 to (2**addr_width)-1) of unsigned(31 downto 0);
    signal memory : speicher;   
    signal full_loc  : std_logic;
    signal empty_loc : std_logic;
    signal final_out_loc:  std_logic;

begin
    process (CLK)
        
        ---------------
        -- FUNCTIONS --
        ---------------

        --
        -- returns the next b-bit word from the buffer
        --
        impure function read_next
        return std_logic_vector is
            variable word1: std_logic_vector(31 downto 0);
            variable word2: std_logic_vector(31 downto 0);
            variable bits_in_word1: natural;
            variable bits_in_word2: natural;
            variable out_word: std_logic_vector(word_size-1 downto 0);
        begin
            word1 := std_logic_vector(memory(to_integer(rdcnt)));
            word2 := std_logic_vector(memory(to_integer(rdcnt+1)));
            if (to_integer(rdpos) > word_size-1) then -- enough bits left in word1
                out_word := word1(to_integer(rdpos) downto to_integer(rdpos)-word_size+1);
            elsif (to_integer(rdpos) = word_size-1) then
                out_word := word1(to_integer(rdpos) downto 0);
                rdcnt <= rdcnt+1;
            else
                bits_in_word1 := to_integer(rdpos) + 1;
                bits_in_word2 := word_size - bits_in_word1;
                out_word(word_size-1 downto word_size-bits_in_word1) := word1(bits_in_word1-1 downto 0);
                out_word(bits_in_word2-1 downto 0) := word2(31 downto 31-bits_in_word2+1);
                rdcnt <= rdcnt+1;
            end if;
            rdpos <= rdpos - word_size;
            return out_word;
        end read_next;

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- appends a new 32-bit word at the end of the buffer
        --

        procedure write_next (data_in: std_logic_vector(31 downto 0)) is
        begin
            memory(to_integer(wrcnt)) <= unsigned(data_in);
            wrcnt <= wrcnt+1;
        end procedure;

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_reset is
        begin
            if (RESET = '1') then
                wrcnt       <= (others => '0');
                rdcnt       <= (others => '0');
                rdpos       <= (others => '1');
                final_idx   <= (others => '0');
                final       <= false;
            end if;
        end procedure;

    begin
        assert word_size <= 32
        report "FIFO_32b: b must not be greater than 32 (is " & natural'image(word_size) & " )"  severity error;

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

    final_out_loc   <= '1' when (final and ((rdcnt=final_idx and rdpos < word_size-1) or rdcnt=final_idx+1))    else '0';
    full_loc        <= '1' when (rdcnt = wrcnt+1)                                                               else '0';
    empty_loc       <= '1' when (rdcnt = wrcnt) or (rdcnt+1 = wrcnt and rdpos < word_size-1)                    else '0';
    FINAL_OUT       <= final_out_loc;
    FULL            <= full_loc;
    EMPTY           <= empty_loc;
end IMP;
