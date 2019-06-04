library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity encoder is
    -- THIS IS HOW IT WORKS WITH GENERICS: generic (WORD_SIZE: integer);
    -- blk_in: next block of size w-1
    -- blk_out: next block of size w-1
    port (
        clk:        in std_logic;
        blk_in:     in std_logic_vector(3 downto 0);
        blk_out:    out std_logic_vector(4 downto 0)
    );
end encoder;

architecture IMP of encoder is
begin
    process (clk)
    begin
        if clk'event and clk='1' then
            blk_out(4) <= '0';
            blk_out(3 downto 0) <= blk_in;
        end if;
        end process;
end IMP;
