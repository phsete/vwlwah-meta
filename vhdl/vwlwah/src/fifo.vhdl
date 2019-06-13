-- quelle: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FIFO is
    Generic (
                Addrbreite  : natural := 8;  -- Speicherlänge = 2^Addrbreite
                Wortbreite  : natural := 8
            );
    Port ( Din   : in  STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
           Wr    : in  STD_LOGIC;
           Dout  : out STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
           Rd    : in  STD_LOGIC;
           Empty : out STD_LOGIC;
           Full  : out STD_LOGIC;
           CLK   : in  STD_LOGIC
       );
end FIFO;

architecture Verhalten of FIFO is

    signal wrcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    type speicher is array(0 to (2**Addrbreite)-1) of unsigned(Wortbreite-1 downto 0);
    signal memory : speicher;   
    signal full_loc  : std_logic;
    signal empty_loc : std_logic;

begin
    process (CLK)
    begin
        if (clk'event and clk='1') then
            if (Wr='1' and full_loc='0') then
                memory(to_integer(wrcnt)) <= unsigned(Din);
                wrcnt <= wrcnt+1;
            end if;
            if (Rd='1' and empty_loc='0') then
                Dout <= std_logic_vector(memory(to_integer(rdcnt))); -- Adresse getaktet --> BRAM
                rdcnt <= rdcnt+1;
            end if;
        end if;
    end process;
full_loc  <= '1' when rdcnt = wrcnt+1 else '0';
empty_loc <= '1' when rdcnt = wrcnt   else '0';
Full  <= full_loc;
Empty <= empty_loc;
end Verhalten;
