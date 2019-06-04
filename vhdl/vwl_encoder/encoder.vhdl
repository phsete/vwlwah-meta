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

    signal zero_fill_length : natural := 1; -- fill_length is always 1 ahead
    signal one_fill_length : natural := 1; -- fill_length is always 1 ahead
end encoder;

architecture IMP of encoder is
begin
    process (clk)
    begin
        if clk'event and clk='1' then
            if blk_in = "0000" then
                -- input is zero fill
                one_fill_length <= 1; -- set back one fill length
                blk_out(4) <= '1';
                blk_out(3) <= '0';
                blk_out(2 downto 0) <= std_logic_vector(to_unsigned(zero_fill_length, 3));
                zero_fill_length <= zero_fill_length + 1;
            elsif blk_in = "1111" then
                -- input is one fill
                zero_fill_length <= 1; -- set back zero fill length
                blk_out(4) <= '1';
                blk_out(3) <= '1';
                blk_out(2 downto 0) <= std_logic_vector(to_unsigned(one_fill_length, 3));
                one_fill_length <= one_fill_length + 1;
            else
                -- input is literal word
                zero_fill_length <= 1;
                one_fill_length <= 1;
                blk_out(4) <= '0';
                blk_out(3 downto 0) <= blk_in;
            end if;
        end if;
        end process;
end IMP;
