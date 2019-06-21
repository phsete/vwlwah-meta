-- angelehnt an: http://www.lothar-miller.de/s9y/archives/21-FIFO.html
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FIFO_32B is
    Generic (
                Addrbreite  : natural := 8;  -- Speicherlänge = 2^Addrbreite
                Wortbreite  : natural := 8
            );
    Port ( Din:         in  STD_LOGIC_VECTOR (31 downto 0);
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
end FIFO_32B;

architecture Verhalten of FIFO_32B is

    signal wrcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal rdpos : unsigned (4 downto 0) := (others => '1'); -- next unread bit index
    signal final_idx: unsigned (Addrbreite-1 downto 0) := (others => '0');
    signal final : boolean := false;
    type speicher is array(0 to (2**Addrbreite)-1) of unsigned(31 downto 0);
    signal memory : speicher;   
    signal full_loc  : std_logic;
    signal empty_loc : std_logic;
    signal final_out_loc:  std_logic;

begin
    process (CLK)
        procedure write_next (data_in: std_logic_vector(31 downto 0)) is
        begin
            memory(to_integer(wrcnt)) <= unsigned(data_in);
            wrcnt <= wrcnt+1;
        end procedure;

        impure function read_next return std_logic_vector is
            variable word1: std_logic_vector(31 downto 0);
            variable word2: std_logic_vector(31 downto 0);
            variable bits_in_word1: natural;
            variable bits_in_word2: natural;
            variable out_word: std_logic_vector(Wortbreite-1 downto 0);
        begin
            word1 := std_logic_vector(memory(to_integer(rdcnt)));
            word2 := std_logic_vector(memory(to_integer(rdcnt+1)));
            if (to_integer(rdpos) > Wortbreite-1) then -- enough bits left in word1
                out_word := word1(to_integer(rdpos) downto to_integer(rdpos)-Wortbreite+1);
            elsif (to_integer(rdpos) = Wortbreite-1) then
                out_word := word1(to_integer(rdpos) downto 0);
                rdcnt <= rdcnt+1;
            else
                bits_in_word1 := to_integer(rdpos) + 1;
                bits_in_word2 := Wortbreite - bits_in_word1;
                out_word(Wortbreite-1 downto Wortbreite-bits_in_word1) := word1(bits_in_word1-1 downto 0);
                out_word(bits_in_word2-1 downto 0) := word2(31 downto 31-bits_in_word2+1);
                rdcnt <= rdcnt+1;
            end if;
            rdpos <= rdpos - Wortbreite;
            return out_word;
        end read_next;

        impure function is_final return boolean is
            variable result : boolean;
        begin
            result := final and rdcnt = final_idx;
            result := result and rdpos <= Wortbreite-1;
            return result;
        end is_final;

    begin
        assert Wortbreite <= 32
        report "FIFO_32b: b must not be greater than 32 (is " & natural'image(Wortbreite) & " )"  severity error;

        if (clk'event and clk='1') then
            if (Wr='1' and full_loc='0' and not final) then
                write_next(Din);
                if (Final_in = '1') then
                    final <= true;
                    final_idx <= wrcnt;
                end if;
            end if;
            if (Rd='1' and empty_loc='0' and final_out_loc='0') then
                Dout <= read_next;
            end if;
            if (RESET = '1') then
                wrcnt <= (others => '0');
                rdcnt <= (others => '0');
                rdpos <= (others => '1'); -- next unread bit index
                final_idx <= (others => '0');
                final <= false;
            end if;
        end if;
    end process;
    final_out_loc <= '1' when (final and ((rdcnt=final_idx and rdpos < Wortbreite-1) or rdcnt=final_idx+1)) else '0';
    full_loc  <= '1' when rdcnt = wrcnt+1 else '0';
    empty_loc <= '1' when rdcnt = wrcnt or (rdcnt+1 = wrcnt and rdpos < Wortbreite-1)  else '0';
    Final_out <= final_out_loc;
    Full  <= full_loc;
    Empty <= empty_loc;
end Verhalten;
