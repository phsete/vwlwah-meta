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
             blk_out_0:  out std_logic_vector(4 downto 0);
             blk_out_1:  out std_logic_vector(4 downto 0);
             blk_out_2:  out std_logic_vector(4 downto 0);
             blk_out_3:  out std_logic_vector(4 downto 0)
         );

         signal zero_fill_length : unsigned (31 downto 0) := to_unsigned(1, 32); -- fill_length is always 1 ahead
         signal one_fill_length : unsigned (31 downto 0) := to_unsigned(1, 32); -- fill_length is always 1 ahead
        
end encoder;

architecture IMP of encoder is

    function emit_literal (content: std_logic_vector(3 downto 0)) return std_logic_vector is
        variable output: std_logic_vector(4 downto 0);
    begin
        output(4) := '0';
        output(3 downto 0) := content;
        return output;
    end function emit_literal;

    function emit_fill (fill_type: std_logic; length: unsigned; word_no: natural) return std_logic_vector is
        variable output: std_logic_vector(4 downto 0);
        variable length_vector: std_logic_vector(31 downto 0);
        variable lowest_bit_idx: natural;
    begin
        length_vector := std_logic_vector(length);
        lowest_bit_idx := word_no * 3;
        if length_vector(31 downto lowest_bit_idx) /= (31 downto lowest_bit_idx => '0') then
            output(4) := '1';
            output(3) := fill_type;
            output(2 downto 0) := length_vector(lowest_bit_idx + 2 downto lowest_bit_idx);
        else
            output := (others => '0');
        end if;
        return output;
    end function emit_fill;

begin
    process (clk)
    begin
        if clk'event and clk='1' then
            if blk_in = "0000" then
                -- input is zero fill
                one_fill_length <= to_unsigned(1, 32); -- set back one fill length
                blk_out_0 <= emit_fill ('0', zero_fill_length, 0);
                blk_out_1 <= emit_fill ('0', zero_fill_length, 1);
                blk_out_2 <= emit_fill ('0', zero_fill_length, 2);
                blk_out_3 <= emit_fill ('0', zero_fill_length, 3);
                zero_fill_length <= zero_fill_length + 1;
            elsif blk_in = "1111" then
                -- input is one fill
                zero_fill_length <= to_unsigned(1, 32); -- set back zero fill length
                blk_out_0 <= emit_fill ('1', one_fill_length, 0);
                blk_out_1 <= emit_fill ('1', one_fill_length, 1);
                blk_out_2 <= emit_fill ('1', one_fill_length, 2);
                blk_out_3 <= emit_fill ('1', one_fill_length, 3);
                one_fill_length <= one_fill_length + 1;
            else
                -- input is literal word
                zero_fill_length <= to_unsigned(1, 32);
                one_fill_length <= to_unsigned(1, 32);
                blk_out_0 <= emit_literal(blk_in);
            end if;
        end if;
    end process;
end IMP;
