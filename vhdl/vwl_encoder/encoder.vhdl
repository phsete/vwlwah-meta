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

    function emit_literal (content: std_logic_vector(3 downto 0)) return std_logic_vector is
        variable output: std_logic_vector(4 downto 0);
    begin
        output(4) := '0';
        output(3 downto 0) := content;
        return output;
    end function emit_literal;

    function emit_fill (fill_type: std_logic; length: natural) return std_logic_vector is
        variable output: std_logic_vector(4 downto 0);
    begin
        output(4) := '1';
        output(3) := fill_type;
        output(2 downto 0) := std_logic_vector(to_unsigned(length, 3));
        return output;
    end function emit_fill;

begin
    process (clk)
    begin
        if clk'event and clk='1' then
            if blk_in = "0000" then
                -- input is zero fill
                one_fill_length <= 1; -- set back one fill length
                blk_out <= emit_fill ('0', zero_fill_length);
                zero_fill_length <= zero_fill_length + 1;
            elsif blk_in = "1111" then
                -- input is one fill
                zero_fill_length <= 1; -- set back zero fill length
                blk_out <= emit_fill ('1', one_fill_length);
                one_fill_length <= one_fill_length + 1;
            else
                -- input is literal word
                zero_fill_length <= 1;
                one_fill_length <= 1;
                blk_out <= emit_literal(blk_in);
            end if;
        end if;
    end process;
end IMP;
