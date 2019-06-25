-- angelehnt an: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity FIFO_B32 is
    Generic (
                Addrbreite  : natural := 8;  -- Speicherlänge = 2^Addrbreite
                Wortbreite  : natural := 8
            );
    Port ( BLK_IN   : in  std_logic_vector (Wortbreite-1 downto 0);
           WR_EN    : in  std_logic;
           BLK_OUT  : out std_logic_vector (31 downto 0);
           RD_EN    : in  std_logic;
           EMPTY : out std_logic;
           FULL  : out std_logic;
           CLK   : in  std_logic;
           FINAL_IN: in std_logic;
           FINAL_OUT: out std_logic;
           RESET: in std_logic
       );
end FIFO_B32;

architecture IMP of FIFO_B32 is

    signal wrcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal wrpos : unsigned (4 downto 0) := (others => '1'); -- next unread bit index
    signal final: boolean := false;
    signal final_idx : unsigned(Addrbreite-1 downto 0) := (others => '0');
    type speicher is array(0 to (2**Addrbreite)-1) of unsigned(31 downto 0);
    signal memory : speicher;   
    signal final_out_loc : std_logic;
    signal full_loc  : std_logic;
    signal empty_loc : std_logic;

begin
    process (CLK)
        impure function read_next return std_logic_vector is
        begin
            rdcnt <= rdcnt+1;
            return std_logic_vector(memory(to_integer(rdcnt)));
        end read_next;

        procedure write_next (data_in: std_logic_vector(Wortbreite-1 downto 0)) is
            variable word1: std_logic_vector(31 downto 0);
            variable word2: std_logic_vector(31 downto 0);
            variable bits_in_word1: natural;
            variable bits_in_word2: natural;
        begin
            word1 := std_logic_vector(memory(to_integer(wrcnt)));
            word2 := std_logic_vector(memory(to_integer(wrcnt+1)));
            if (to_integer(wrpos) > Wortbreite-1) then -- enough bits left in word1
                memory(to_integer(wrcnt))(to_integer(wrpos) downto to_integer(wrpos)-Wortbreite+1) <= unsigned(data_in);
            elsif (to_integer(wrpos) = Wortbreite-1) then
                memory(to_integer(wrcnt))(to_integer(wrpos) downto 0) <= unsigned(data_in);
                wrcnt <= wrcnt+1;
            else
                bits_in_word1 := to_integer(wrpos) + 1;
                bits_in_word2 := Wortbreite - bits_in_word1;
                memory(to_integer(wrcnt))(bits_in_word1-1 downto 0) <= unsigned(data_in(Wortbreite-1 downto Wortbreite-bits_in_word1));
                memory(to_integer(wrcnt+1))(31 downto 31-bits_in_word2+1) <= unsigned(data_in(bits_in_word2-1 downto 0));
                wrcnt <= wrcnt+1;
            end if;
            wrpos <= wrpos - Wortbreite;
        end procedure;

    begin
        assert Wortbreite <= 32
        report "FIFO_b32: b must not be greater than 32 (is " & natural'image(Wortbreite) & " )"  severity error;

        if (clk'event and clk='1') then
            if (WR_EN='1' and full_loc='0' and not final) then
                write_next(BLK_IN);
                if (FINAL_IN = '1') then
                    final <= true;
                    final_idx <= wrcnt;
                end if;
            end if;
            if (RD_EN='1' and empty_loc='0' and final_out_loc='0') then
                BLK_OUT <= read_next;
            end if;
            if (RESET='1') then
                wrcnt <= (others => '0');
                rdcnt <= (others => '0');
                wrpos <= (others => '1');
                final_idx <= (others => '0');
                final <= false;
            end if;
        end if;
    end process;
final_out_loc <= '1' when (final and (rdcnt = final_idx +1)) else '0';
full_loc  <= '1' when rdcnt = wrcnt+1 else '0';
empty_loc <= '1' when rdcnt = wrcnt else '0';
FINAL_OUT <= final_out_loc;
FULL  <= full_loc;
EMPTY <= empty_loc;
end IMP;
