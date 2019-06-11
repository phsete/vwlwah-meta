library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity encoder is
    -- THIS IS HOW IT WORKS WITH GENERICS: generic (WORD_SIZE: integer);
    -- blk_in: next block of size w-1
    -- blk_out: next block of size w-1
    port (
             clk:           in std_logic;
             blk_in:        in std_logic_vector(3 downto 0);
             in_empty:      in std_logic;
             out_full:      in std_logic;
             blk_out:       out std_logic_vector(4 downto 0);
             in_rd:         out std_logic;
             out_wr:        out std_logic
         );

    signal zero_fill_length: unsigned(31 downto 0) := to_unsigned(0, 32);
    signal one_fill_length:  unsigned(31 downto 0) := to_unsigned(0, 32);

end encoder;

architecture IMP of encoder is

begin
    process (clk)

        -- writes a literal word to the output buffer
        procedure emit_literal (content: std_logic_vector(3 downto 0)) is
                                variable output_word: std_logic_vector(4 downto 0);
        begin
            -- determine output representation and write word to output buffer
            output_word(4) := '0';
            output_word(3 downto 0) := content;
            blk_out <= output_word;
        end procedure;

        -- writes a sequence of fill words to the output buffer
        procedure emit_fill (fill_type: std_logic; length: unsigned) is
                             variable length_vector: std_logic_vector(31 downto 0);
                             variable lowest_bit_idx: natural;
                             variable output_word: std_logic_vector(4 downto 0);
        begin
            length_vector := std_logic_vector(length);
            for word_no in 0 downto 0 loop
                lowest_bit_idx := word_no * 3;
                if length_vector(31 downto lowest_bit_idx) /= (31 downto lowest_bit_idx => '0') then
                    output_word(4) := '1';
                    output_word(3) := fill_type;
                    output_word(2 downto 0) := length_vector(lowest_bit_idx + 2 downto lowest_bit_idx);
                    blk_out <= output_word;
                end if;
            end loop;
        end procedure;

    begin
        if (clk'event and clk='1') then

                -- handle input compression
            if (blk_in = "0000") then
                    -- input is zero fill, emit previously started one fill first
                if (one_fill_length /= to_unsigned(0, 32)) then
                    emit_fill ('1', one_fill_length);
                    one_fill_length <= to_unsigned(0, 32);
                end if;

                zero_fill_length <= zero_fill_length + 1;
            elsif (blk_in = "1111") then
                    -- input is one fill, emit previously started zero fill first
                if (zero_fill_length /= to_unsigned(0, 32)) then
                    emit_fill ('0', zero_fill_length);
                    zero_fill_length <= to_unsigned(0, 32);
                end if;

                one_fill_length <= one_fill_length + 1;
            else
                    -- input is literal word, emit previously started fill words first
                if (zero_fill_length /= to_unsigned(0, 32)) then
                    emit_fill ('0', zero_fill_length);
                    zero_fill_length <= to_unsigned(0, 32);
                elsif (one_fill_length /= to_unsigned(0, 32)) then
                    emit_fill ('1', one_fill_length);
                    one_fill_length <= to_unsigned(0, 32);
                end if;

                    -- now emit current literal word
                emit_literal(blk_in);
            end if;

        end if;
    end process;

end IMP;
