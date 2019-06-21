-- quelle: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity fifo_bb is
    Generic (
                Addrbreite  : natural := 8;  -- Speicherlänge = 2^Addrbreite
                Wortbreite  : natural := 8
            );
    Port ( Din:         in  STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
           Wr:          in  STD_LOGIC;
           Dout:        out STD_LOGIC_VECTOR (Wortbreite-1 downto 0);
           Rd:          in  STD_LOGIC;
           Empty:       out STD_LOGIC;
           Full:        out STD_LOGIC;
           Final_in:    in  STD_LOGIC;
           Final_out:   out STD_LOGIC;
           CLK:         in  STD_LOGIC;
           RESET:       in  STD_LOGIC
       );
end fifo_bb;

architecture Verhalten of fifo_bb is

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
            if (Wr='1' and full_loc='0') then
                memory(to_integer(wrcnt)) <= unsigned(Din);
                if (Final_in = '1' and not final) then
                    final <= true;
                    final_idx <= wrcnt;
                end if;
                wrcnt <= wrcnt+1;
            end if;
            if (Rd='1' and empty_loc='0' and final_out_loc = '0') then
                Dout <= std_logic_vector(memory(to_integer(rdcnt))); -- Adresse getaktet --> BRAM
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
    Final_out <= final_out_loc;
    Full  <= full_loc;
    Empty <= empty_loc;
end Verhalten;
