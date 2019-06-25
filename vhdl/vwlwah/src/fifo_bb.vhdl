-- quelle: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo_bb is
    Generic (
                Addrbreite  : natural := 8;  -- Speicherlänge = 2^Addrbreite
                Wortbreite  : natural := 8
            );
    Port ( BLK_IN:         in  std_logic_vector (Wortbreite-1 downto 0);
           WR_EN:          in  std_logic;
           BLK_OUT:        out std_logic_vector (Wortbreite-1 downto 0);
           RD_EN:          in  std_logic;
           EMPTY:       out std_logic;
           FULL:        out std_logic;
           FINAL_IN:    in  std_logic;
           FINAL_OUT:   out std_logic;
           CLK:         in  std_logic;
           RESET:       in  std_logic
       );
end fifo_bb;

architecture IMP of fifo_bb is

    signal wrcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal final_idx: unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal final : boolean := false;
    type speicher is array(0 to (2**Addrbreite)-1) of unsigned(Wortbreite-1 downto 0);
    signal memory : speicher;   
    signal final_out_loc : std_logic;
    signal full_loc  : std_logic;
    signal empty_loc : std_logic;

begin
    process (CLK)
    begin
        if (clk'event and clk='1') then
            if (WR_EN='1' and full_loc='0') then
                memory(to_integer(wrcnt)) <= unsigned(BLK_IN);
                if (FINAL_IN = '1' and not final) then
                    final <= true;
                    final_idx <= wrcnt;
                end if;
                wrcnt <= wrcnt+1;
            end if;
            if (RD_EN='1' and empty_loc='0' and final_out_loc = '0') then
                BLK_OUT <= std_logic_vector(memory(to_integer(rdcnt))); -- Adresse getaktet --> BRAM
                rdcnt <= rdcnt+1;
            end if;
            if (RESET = '1') then
                wrcnt <= (others => '0');
                rdcnt <= (others => '0');
                final_idx <= (others => '0');
                final <= false;
            end if;
        end if;
    end process;
    final_out_loc <= '1' when (final and final_idx+1 = rdcnt) else '0';
    full_loc  <= '1' when rdcnt = wrcnt+1 else '0';
    empty_loc <= '1' when rdcnt = wrcnt   else '0';
    FINAL_OUT <= final_out_loc;
    FULL  <= full_loc;
    EMPTY <= empty_loc;
end IMP;
