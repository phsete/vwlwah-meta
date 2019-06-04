library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity multiplier is

    generic (
        WORD_SIZE: integer
            );
    -- blk_in: next block of size w-1
    -- blk_out: next block of size w-1
    port (
        clk:        in std_logic;
        blk_in:     in std_logic_vector(WORD_SIZE - 2 downto 0);
        blk_out:    out std_logic_vector(WORD_SIZE - 1 downto 0)
    );
end multiplier;

architecture IMP of multiplier is
begin
    process (clk)
    begin
        if clk'event and clk='1' then
            blk_out(WORD_SIZE - 1) <= '0';
            blk_out(WORD_SIZE - 2 downto 0) <= blk_in;
        end if;
        end process;
end IMP;
