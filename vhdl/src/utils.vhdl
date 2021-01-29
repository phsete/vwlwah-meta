library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package utils is
    type Word is (W_NONE, W_0FILL, W_1FILL, W_LITERAL);
    type CompaxWord is (W_NONE, W_0FILL, W_1FILL, W_LITERAL, W_FLF, W_LFL, W_FLF_F1, W_FLF_F2, W_FLF_L, W_LFL_L1, W_LFL_F, W_LFL_L2);

    function is_all (vec : std_logic_vector;
    val : std_logic)
    return boolean;

    function get_dirty_quarter (word_size: natural;
    input_word: std_logic_vector)
    return natural;

    function get_dirty_eighth (word_size: natural;
    input_word: std_logic_vector)
    return natural;

    function is_compressable (word_size: natural;
    input_word: std_logic_vector)
    return boolean;

    function scale_down (word_size: natural;
    factor: natural)
    return natural;

    function scale_up (word_size: natural;
    factor: natural)
    return natural;

    function split_literal (word_size: natural;
    input_word: std_logic_vector;
    factor: natural;
    index: natural)
    return std_logic_vector;

    function extend_literal (word_size: natural;
    old_literal: std_logic_vector;
    new_word_size: natural;
    new_block: std_logic_vector;
    from_index: natural;
    num_extensions: natural)
    return std_logic_vector;

    function parse_word_type (word_size: natural;
    input_word: std_logic_vector)
    return Word;

    function parse_word_type_compax (word_size: natural;
    input_word: std_logic_vector)
    return CompaxWord;

    function invert_F (word_size: natural;
    input_word: std_logic_vector)
    return std_logic_vector;

    function invert_L (word_size: natural;
    input_word: std_logic_vector)
    return std_logic_vector;

    function decode_literal (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function decode_fill (word_size: natural;
    fill_type: std_logic)
    return std_logic_vector;

    function decode_literal_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function decode_fill_compax (word_size: natural;
    fill_type: std_logic;
    content: std_logic_vector)
    return std_logic_vector;

    function decode_flf_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function decode_flf_f_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function decode_lfl_compax (word_size: natural;
    content: std_logic_vector;
    literal_no: natural)
    return std_logic_vector;

    function decode_lfl_f_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function parse_fill_length (word_size: natural;
    fill_counter_size: natural;
    old_fill_length: unsigned;
    fill_word: std_logic_vector)
    return unsigned;

    function encode_literal (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function encode_literal_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector;

    function encode_fill (word_size: natural;
    fill_counter_size: natural;
    fill_type: std_logic;
    length: unsigned;
    word_no: natural)
    return std_logic_vector;

    function encode_fill_compax (word_size: natural;
    fill_counter_size: natural;
    length: unsigned)
    return std_logic_vector;

    function encode_fill_vwlcom (word_size: natural;
    fill_counter_size: natural;
    length: unsigned;
    word_no: natural)
    return std_logic_vector;

    function encode_flf_main (word_size: natural;
    literal_buffer: std_logic_vector)
    return std_logic_vector;

    function encode_flf_fill (word_size: natural;
    zero_fill_length: unsigned)
    return std_logic_vector;

    function encode_lfl (word_size: natural;
    literal_buffer: std_logic_vector;
    lfl_literal_buffer: std_logic_vector)
    return std_logic_vector;

    function encode_lfl_vwlcom (word_size: natural;
    literal_buffer: std_logic_vector;
    lfl_literal_buffer: std_logic_vector;
    zero_fill_length: unsigned;
    to_long: boolean)
    return std_logic_vector;

    function fill_words_needed (word_size: natural;
    fill_counter_size: natural;
    length: unsigned)
    return natural;

    function parse_block_type (word_size: natural;
    input_word: std_logic_vector)
    return Word;

    function parse_vwlwah_block_type (word_size: natural;
    input_word: std_logic_vector)
    return Word;

    function logic_function (in0, in1: std_logic_vector)
    return std_logic_vector;

    function to_std_logic (input: boolean)
    return std_logic;

    function decode_single (word_size: natural;
    input_block: std_logic_vector)
    return std_logic_vector;

    function log2ceil (n : natural)
    return natural;
end;

package body utils is

    function is_all (vec : std_logic_vector; val : std_logic) return boolean is
        constant all_bits : std_logic_vector(vec'range) := (others => val);
    begin
        return vec = all_bits;
    end function;

    function get_dirty_quarter (word_size: natural; input_word: std_logic_vector) return natural is
    begin
        if(is_all(input_word(word_size-2 downto (word_size/4)*3), '0')) then -- 0XXX
            if(is_all(input_word((word_size/4)*3-1 downto (word_size/4)*2), '0')) then -- 00XX
                if(is_all(input_word((word_size/4)*2-1 downto word_size/4), '0')) then -- 000X
                    return 1;
                else -- 001X
                    if(is_all(input_word(word_size/4-1 downto 0), '0')) then -- 0010
                        return 2;
                    else -- 0011
                        return 0;
                    end if;
                end if;
            else -- 01XX
                if(is_all(input_word((word_size/4)*2-1 downto word_size/4), '0') and is_all(input_word(word_size/4-1 downto 0), '0')) then -- 0100
                    return 3;
                else -- 01PP
                    return 0;
                end if;
            end if;
        else -- 1XXX
            if(is_all(input_word((word_size/4)*3-1 downto (word_size/4)*2), '0') and is_all(input_word((word_size/4)*2-1 downto word_size/4), '0') and is_all(input_word(word_size/4-1 downto 0), '0')) then -- 1000
                return 4;
            else -- 1PPP
                return 0;
            end if;
        end if;
    end function;

    function get_dirty_eighth (word_size: natural; input_word: std_logic_vector) return natural is
        variable input_word_headless: std_logic_vector(word_size-2 downto 0);
        variable dirty_eighth: natural;
        variable ceiled_eighth: natural;
        variable one: unsigned((word_size/8+1)*8 downto 0);
    begin
        one := (others => '0');
        one(0) := '1';
        dirty_eighth := 0;
        input_word_headless := input_word(word_size-2 downto 0);

        ceiled_eighth := word_size/8;
        if(word_size > ceiled_eighth*8) then
            ceiled_eighth := ceiled_eighth + 1;
        end if;

        for i in 8 downto 1 loop
            if(unsigned(input_word_headless) < shift_left(one, i*ceiled_eighth)) then
                dirty_eighth := i;
            else
                return dirty_eighth;
            end if;
        end loop;

        return dirty_eighth;
    end function;

    function is_compressable (word_size: natural; input_word: std_logic_vector) return boolean is
    begin
        if(get_dirty_quarter(word_size, input_word) > 0) then
            return true;
        else
            return false;
        end if;
    end function;

    --
    -- determines the new word size when blocks are splitted into facor parts
    --
    function scale_down (word_size: natural;
    factor: natural)
    return natural is
    begin
        return ((word_size - 1) / factor) + 1;
    end scale_down;

    --
    -- determines the new word size when factor blocks are combined into one
    --
    function scale_up (word_size: natural;
    factor: natural)
    return natural is
    begin
        return ((word_size - 1) * factor) + 1;
    end scale_up;

    --
    -- splits the literal word into factor parts and returns the part no index
    --
    function split_literal (word_size: natural;
    input_word: std_logic_vector;
    factor: natural;
    index: natural)
    return std_logic_vector is
        variable result: std_logic_vector(scale_down(word_size, factor)-1 downto 0);
        variable upper_bound: natural;
        variable lower_bound: natural;
    begin
        lower_bound := (word_size-1) / factor * index;
        upper_bound := (word_size-1) / factor * (index + 1);
        result(scale_down(word_size, factor)-1) := '0';
        for idx in scale_down(word_size, factor)-2 downto 0 loop
            result(idx) := input_word(lower_bound + idx);
        end loop;

        return result;
    end split_literal;

    --
    -- extends the old literal by adding 'num_extensions' new blocks at block position from_index
    -- ranging from scaling_factor-1 to 0
    --
    function extend_literal (word_size: natural;
    old_literal: std_logic_vector;
    new_word_size: natural;
    new_block: std_logic_vector;
    from_index: natural;
    num_extensions: natural)
    return std_logic_vector is
        variable new_literal: std_logic_vector(new_word_size-1 downto 0);
        variable new_literal_mask: std_logic_vector(new_word_size-1 downto 0);
        variable new_literal_tmp: std_logic_vector(new_word_size-1 downto 0);
        variable upper_bound: natural;
        variable lower_bound: natural;
    begin
        new_literal := old_literal;
        for extension in 0 to num_extensions-1 loop
            new_literal_tmp := (others => '0');
            new_literal_mask := (others => '1');
            new_literal_tmp(word_size-2 downto 0) := new_block;
            new_literal_mask(word_size-2 downto 0) := (others => '0');
            new_literal_tmp := std_logic_vector(shift_left(unsigned(new_literal_tmp), (word_size-1) * from_index - extension));
            new_literal_mask := std_logic_vector(shift_left(unsigned(new_literal_mask), (word_size-1) * from_index - extension));
            new_literal := (new_literal and new_literal_mask) or new_literal_tmp;
        end loop;
        return new_literal;
    end extend_literal;

    --
    -- determine the word type of input_word by parsing the control bits
    -- (MSB for literals and MSB, MSB-1 for fills)
    --
    function parse_word_type (word_size: natural;
    input_word: std_logic_vector)
    return Word is
    begin
        if input_word(word_size-1) = '0' then
            return W_LITERAL;
        elsif input_word(word_size-2) = '0' then
            return W_0FILL;
        elsif input_word(word_size-2) = '1' then
            return W_1FILL;
        else
            return W_NONE;
        end if;
    end parse_word_type;

    --
    -- determine the word type of input_word by parsing the control bits for compax encoding
    --
    function parse_word_type_compax (word_size: natural;
    input_word: std_logic_vector)
    return CompaxWord is
    begin
        if input_word(word_size-1) = '1' then
            if is_all(input_word(word_size-1 downto 0), '1') then
                return W_1FILL;
            else
                return W_LITERAL;
            end if;
        elsif input_word(word_size-2) = '0' then
            if input_word(word_size-3) = '1' then
                return W_LFL;
            else
                return W_0FILL;
            end if;
        elsif input_word(word_size-2) = '1' then
            return W_FLF;
        else
            return W_NONE;
        end if;
    end parse_word_type_compax;

    --
    -- returns the inverse version of the input fill
    --
    function invert_F (word_size: natural;
    input_word: std_logic_vector)
    return std_logic_vector is
        variable output: std_logic_vector(word_size-1 downto 0) := (others => 'U');
    begin
        output(word_size-1) := input_word(word_size-1);
        output(word_size-2) := not(input_word(word_size-2));
        output(word_size-3 downto 0) := input_word(word_size-3 downto 0);
        return output;
    end invert_F;

    --
    -- returns the inverse version of the input literal
    --
    function invert_L (word_size: natural;
    input_word: std_logic_vector)
    return std_logic_vector is
        variable output: std_logic_vector(word_size-1 downto 0);
    begin
        output(word_size-1) := input_word(word_size-1);
        for i in word_size-2 downto 0 loop
            output(i) := not(input_word(i));
        end loop;
        return output;
    end invert_L;

    --
    -- returns a decoded literal derived from the encoded input literal
    --
    function decode_literal (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
    begin
        -- determine output representation and write word to output buffer
        return content(word_size-2 downto 0);
    end decode_literal;

    --
    -- returns a decoded fill block of the given type
    --
    function decode_fill (word_size: natural;
    fill_type: std_logic)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-2 downto 0);
    begin
        -- fill all bits of buf with the given type
        for idx in word_size-2 downto 0 loop
            buf(idx)    := fill_type;
        end loop;

        return buf;
    end decode_fill;

    --
    -- returns a decoded literal derived from the encoded input literal
    --
    function decode_literal_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
    begin
        -- determine output representation and write word to output buffer
        return '0' & content(word_size-2 downto 0);
    end decode_literal_compax;

    --
    -- returns a decoded fill block of the given type
    --
    function decode_fill_compax (word_size: natural;
    fill_type: std_logic;
    content: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        buf(word_size-1 downto word_size-2) := '1' & fill_type;
        buf(word_size-3 downto 0) := content(word_size-3 downto 0);

        return buf;
    end decode_fill_compax;

    function decode_flf_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
        variable index: natural;
    begin
        index := natural(to_integer(unsigned(content(word_size-4 downto word_size-5))));
        buf := (others => '0');
        buf(index*(word_size/4)+word_size/4-1 downto index*(word_size/4)) := content(word_size/4-1 downto 0);

        return buf;
    end decode_flf_compax;

    function decode_flf_f_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        buf(word_size-1 downto word_size-2) := "10";
        buf(word_size-3 downto word_size/4) := (others => '0');
        buf(word_size/4-1 downto 0) := content(word_size/4-1 downto 0);

        return buf;
    end decode_flf_f_compax;

    function decode_lfl_compax (word_size: natural;
    content: std_logic_vector;
    literal_no: natural)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
        variable index: natural;
    begin
        index := natural(to_integer(unsigned(content(word_size-4-(2-literal_no*2) downto word_size-5-(2-literal_no*2)))));
        report("index: " & integer'image(index));
        buf := (others => '0');
        buf(index*(word_size/4)+word_size/4-1 downto index*(word_size/4)) := content(word_size/4-1+literal_no*(word_size/4) downto literal_no*(word_size/4));

        return buf;
    end decode_lfl_compax;

    function decode_lfl_f_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        buf(word_size-1 downto word_size-2) := "10";
        --buf(word_size-3 downto word_size/4) := (others => '0');
        buf(word_size-3 downto 0) := content(word_size-3 downto 0);

        return buf;
    end decode_lfl_f_compax;

    --
    -- concatenates the length of an encoded fill to the decoded old_fill_length and
    -- returns the result
    --
    function parse_fill_length (word_size: natural;
    fill_counter_size: natural;
    old_fill_length: unsigned;
    fill_word: std_logic_vector)
    return unsigned is
        variable new_fill_length: unsigned(fill_counter_size-1 downto 0);
    begin
        -- shift old fill length by the number of new bits from the next fill word
        new_fill_length := shift_left(old_fill_length, word_size-2);

        -- copy the newly obtained bits to the least significant positions of new_fill_length
        for idx in word_size-3 downto 0 loop
            new_fill_length(idx) := fill_word(idx);
        end loop;

        return new_fill_length;
    end parse_fill_length;

    --
    -- returns an encoded literal word with the given content
    --
    function encode_literal (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        -- set control bit and copy word contents
        buf(word_size-1) := '0';
        buf(word_size-2 downto 0) := content;
        return buf;
    end encode_literal;

    --
    -- returns an encoded literal word with the given content in compax format
    --
    function encode_literal_compax (word_size: natural;
    content: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        -- set control bit and copy word contents
        buf(word_size-1) := '1';
        buf(word_size-2 downto 0) := content;
        return buf;
    end encode_literal_compax;

    --
    -- returns an encoded fill word of the given type.
    -- the total length of the fill is gven by the parameter length.
    -- for concatenated fills, word_no is the reversed position of the fill word inside the concatenated group
    --
    function encode_fill (word_size: natural;
    fill_counter_size: natural;
    fill_type: std_logic;
    length: unsigned;
    word_no: natural)
    return std_logic_vector is
        variable length_vector: std_logic_vector(fill_counter_size-1 downto 0);
        variable lowest_bit_idx: natural;
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        length_vector := std_logic_vector(length);
        lowest_bit_idx := word_no * (word_size-2);

        -- set control bits
        buf(word_size-1)          := '1';
        buf(word_size-2)          := fill_type;

        -- copy the related (word_size-2) bits of the length representation vector
        buf(word_size-3 downto 0) := std_logic_vector(shift_right(unsigned(length_vector), lowest_bit_idx)(word_size-3 downto 0));
        return buf;
    end encode_fill;

    --
    -- returns an encoded flf word
    --
    function encode_flf_main (word_size: natural;
    literal_buffer: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
        variable num_ununsed: natural;
        variable dirty_quarter: natural;
    begin
        dirty_quarter := get_dirty_quarter(word_size, literal_buffer);
        num_ununsed := word_size-5-(word_size/4);
        if(word_size > (word_size/4)*4) then
            num_ununsed := num_ununsed + 1;
        end if;

        buf(word_size-1 downto word_size-3) := "010";
        buf(word_size-4 downto word_size-5) := std_logic_vector(to_unsigned(dirty_quarter-1, 2));

        if(num_ununsed > 5) then
            buf(word_size-6 downto word_size-num_ununsed-5) := (others => '0');
        end if;

        if(dirty_quarter = 4) then
            buf(word_size-1-(word_size/4)*(dirty_quarter-1)) := '0';
            buf(word_size-2-(word_size/4)*(dirty_quarter-1) downto 0) := literal_buffer(word_size-2 downto (word_size/4)*(dirty_quarter-1));
        else
            buf(word_size/4-1 downto 0) := literal_buffer((word_size/4)*dirty_quarter-1 downto (word_size/4)*(dirty_quarter-1));
        end if;

        return buf;
    end encode_flf_main;

    function encode_flf_fill (word_size: natural;
    zero_fill_length: unsigned)
    return std_logic_vector is
    begin
        return std_logic_vector(zero_fill_length(word_size-1 downto 0));
    end encode_flf_fill;

    --
    -- returns an encoded lfl word
    --
    function encode_lfl (word_size: natural;
    literal_buffer: std_logic_vector;
    lfl_literal_buffer: std_logic_vector)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
        variable num_ununsed: natural;
        variable dirty_quarter: natural;
        variable dirty_quarter_snd: natural;
        variable ceiled_quarter: natural;
    begin
        ceiled_quarter := word_size/4;
        if(word_size > ceiled_quarter*4) then
            ceiled_quarter := ceiled_quarter + 1;
        end if;

        dirty_quarter := get_dirty_quarter(word_size, literal_buffer);
        dirty_quarter_snd := get_dirty_quarter(word_size, lfl_literal_buffer);

        -- probably wrong!
        num_ununsed := (word_size/4)*2-7-((word_size/4)*4-word_size);

        buf(word_size-1 downto word_size-3) := "001";
        buf(word_size-4 downto word_size-5) := std_logic_vector(to_unsigned(dirty_quarter-1, 2));
        buf(word_size-6 downto word_size-7) := std_logic_vector(to_unsigned(dirty_quarter_snd-1, 2));

        if(num_ununsed > 0) then
            buf(word_size-8 downto word_size-num_ununsed-7) := (others => '0');
        end if;

        if(dirty_quarter = 4) then
            buf((word_size/4)+word_size-1-(word_size/4)*(dirty_quarter-1)) := '0';
            buf((word_size/4)+word_size-2-(word_size/4)*(dirty_quarter-1) downto word_size/4) := literal_buffer(word_size-2 downto (word_size/4)*(dirty_quarter-1));
        else
            buf((word_size/4)+word_size/4-1 downto word_size/4) := literal_buffer((word_size/4)*dirty_quarter-1 downto (word_size/4)*(dirty_quarter-1));
        end if;

        if(dirty_quarter_snd = 4) then
            buf(word_size-1-(word_size/4)*(dirty_quarter_snd-1)) := '0';
            buf(word_size-2-(word_size/4)*(dirty_quarter_snd-1) downto 0) := lfl_literal_buffer(word_size-2 downto (word_size/4)*(dirty_quarter_snd-1));
        else
            buf(word_size/4-1 downto 0) := lfl_literal_buffer((word_size/4)*dirty_quarter_snd-1 downto (word_size/4)*(dirty_quarter_snd-1));
        end if;

        return buf;
    end encode_lfl;

    --
    -- returns an encoded lfl word
    --
    function encode_lfl_vwlcom (word_size: natural;
    literal_buffer: std_logic_vector;
    lfl_literal_buffer: std_logic_vector;
    zero_fill_length: unsigned;
    to_long: boolean)
    return std_logic_vector is
        variable buf: std_logic_vector(word_size-1 downto 0);
        variable extended_literal_buffer: std_logic_vector((word_size/8+1)*8 downto 0);
        variable extended_lfl_literal_buffer: std_logic_vector((word_size/8+1)*8 downto 0);
        variable num_ununsed: natural;
        variable dirty_eighth: natural;
        variable dirty_eighth_snd: natural;
        variable ceiled_eighth: natural;
    begin
        extended_literal_buffer := (others => '0');
        extended_lfl_literal_buffer := (others => '0');
        extended_literal_buffer(word_size-2 downto 0) := literal_buffer(word_size-2 downto 0);
        extended_lfl_literal_buffer(word_size-2 downto 0) := lfl_literal_buffer(word_size-2 downto 0);

        ceiled_eighth := word_size/8;
        if(word_size > ceiled_eighth*8) then
            ceiled_eighth := ceiled_eighth + 1;
        end if;

        dirty_eighth := get_dirty_eighth(word_size, literal_buffer);
        dirty_eighth_snd := get_dirty_eighth(word_size, lfl_literal_buffer);

        buf(word_size-1 downto word_size-3) := "001";
        buf(word_size-4 downto word_size-6) := std_logic_vector(to_unsigned(dirty_eighth-1, 3));
        buf(word_size-7 downto word_size-9) := std_logic_vector(to_unsigned(dirty_eighth_snd-1, 3));

        report("test: " & to_bstring(zero_fill_length));

        if(to_long) then
            buf(word_size-10) := '1';
        else
            buf(word_size-10) := '0';
        end if;

        buf(word_size-11 downto word_size-10-ceiled_eighth) := extended_literal_buffer(dirty_eighth*ceiled_eighth-1 downto dirty_eighth*ceiled_eighth-ceiled_eighth);

        buf(word_size-11-ceiled_eighth downto word_size-10-2*ceiled_eighth) := extended_lfl_literal_buffer(dirty_eighth_snd*ceiled_eighth-1 downto dirty_eighth_snd*ceiled_eighth-ceiled_eighth);

        -- only for fills with correct length -> longer fills should continue in next word
        buf(word_size-11-2*ceiled_eighth downto 0) := std_logic_vector(to_unsigned(to_integer(zero_fill_length), word_size-10-2*ceiled_eighth));

        return buf;
    end encode_lfl_vwlcom;

    --
    -- returns an encoded fill word of the given type in compax format.
    -- the total length of the fill is given by the parameter length.
    --
    function encode_fill_vwlcom (word_size: natural;
    fill_counter_size: natural;
    length: unsigned;
    word_no: natural)
    return std_logic_vector is
        variable length_vector: std_logic_vector(fill_counter_size-1 downto 0);
        variable lowest_bit_idx: natural;
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        length_vector := std_logic_vector(length);
        lowest_bit_idx := word_no * (word_size-3);

        -- set control bits
        buf(word_size-1 downto word_size-3) := "000";

        -- copy the related (word_size-3) bits of the length representation vector
        buf(word_size-4 downto 0) := std_logic_vector(shift_right(unsigned(length_vector), lowest_bit_idx)(word_size-4 downto 0));
        return buf;
    end encode_fill_vwlcom;

    --
    -- returns an encoded fill word of the given type in compax format.
    -- the total length of the fill is given by the parameter length.
    --
    function encode_fill_compax (word_size: natural;
    fill_counter_size: natural;
    length: unsigned)
    return std_logic_vector is
        variable length_vector: std_logic_vector(fill_counter_size-1 downto 0);
        variable buf: std_logic_vector(word_size-1 downto 0);
    begin
        length_vector := std_logic_vector(length);

        -- set control bits
        buf(word_size-1 downto word_size-3) := "000";

        -- copy the related (word_size-2) bits of the length representation vector
        buf(word_size-4 downto 0) := std_logic_vector(length_vector(word_size-4 downto 0));
        return buf;
    end encode_fill_compax;

    --
    -- returns the number of fill words needed to represent a fill of the given length
    --
    function fill_words_needed (word_size: natural;
    fill_counter_size: natural;
    length: unsigned)
    return natural is
    begin
        for i in fill_counter_size-1 downto 0 loop
            if length(i) = '1' then
                return (i / (word_size-2)) + 1;
            end if;
        end loop;
        return 0;
    end fill_words_needed;

    --
    -- determine the type of input_word by parsing identifying the contents as fill or literal
    --
    function parse_block_type (word_size: natural;
    input_word: std_logic_vector)
    return Word is
    variable one_fill: boolean := true;
    variable zero_fill: boolean := true;
    begin
        for bit_idx in 0 to word_size-2 loop
            case input_word(bit_idx) is
                when '0' =>
                    one_fill := false;
                when '1' =>
                    zero_fill := false;
                when others =>
                    return W_NONE;
            end case;
        end loop;

        if zero_fill then
            return W_0FILL;
        elsif one_fill then
            return W_1FILL;
        else
            return W_LITERAL;
        end if;
    end parse_block_type;

    --
    -- determine the type of input_word by parsing identifying the contents as fill or literal
    --
    function parse_vwlwah_block_type (word_size: natural;
    input_word: std_logic_vector)
    return Word is
    begin
        if input_word(word_size-1) = '0' then
            return W_LITERAL;
        elsif input_word(word_size-1) = '1' then
            if input_word(word_size-2) = '0' then
                return W_0FILL;
            else
                return W_1FILL;
            end if;
        else
            return W_NONE;
        end if;
    end parse_vwlwah_block_type;

    --
    -- actual implementation of the logic or function
    --
    function logic_function (in0, in1: std_logic_vector)
    return std_logic_vector is
    begin
        return in0 or in1;
    end logic_function;

    --
    -- convert boolean to std_logic
    --
    function to_std_logic (input: boolean)
    return std_logic is
    begin
        if input then
            return '1';
        else
            return '0';
        end if;
    end to_std_logic;

    --
    -- decodes the literal representation of a single block
    -- for fill words only a single representative block is returned
    --
    function decode_single (word_size: natural;
    input_block: std_logic_vector)
    return std_logic_vector is
        variable decoded_block: std_logic_vector(word_size-2 downto 0) := (others => 'U');
        variable word_type: Word := parse_word_type(word_size, input_block);
    begin
        case word_type is
            when W_LITERAL =>
                decoded_block := input_block(word_size-2 downto 0);
            when W_0FILL =>
                decoded_block := (others => '0');
            when W_1FILL =>
                decoded_block := (others => '1');
            when others =>
        end case;
        return decoded_block;
    end decode_single;

    --
    -- calculates log2 ceiling for values up to 32
    -- source: https://www.mikrocontroller.net/topic/61054
    --
    function log2ceil (n : natural)
    return natural is
        variable n_bit : unsigned(5 downto 0);
    begin  -- log2ceil
        if n = 0 then
            return 0;
        end if;
        n_bit := to_unsigned(n-1,6);
        for i in 5 downto 0 loop
            if n_bit(i) = '1' then
                return i+1;
            end if;
        end loop;
        return 1;
    end log2ceil;

end package body;
