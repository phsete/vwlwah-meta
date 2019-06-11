library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity encoder_tb is
    end encoder_tb;

architecture behav of encoder_tb is

    -- found this function implementation at: https://stackoverflow.com/questions/15406887/vhdl-convert-vector-to-string
    function to_string ( a: std_logic_vector) return string is
        variable b : string (1 to a'length) := (others => NUL);
        variable stri : integer := 1; 
    begin
        for i in a'range loop
            b(stri) := std_logic'image(a((i)))(2);
            stri := stri+1;
        end loop;
        return b;
    end function;

    --  Declaration of the component that will be instantiated.
    component encoder
        port (
                 clk:        in std_logic;
                 blk_in:     in std_logic_vector(3 downto 0);
                 blk_out_0:  out std_logic_vector(4 downto 0);
                 blk_out_1:  out std_logic_vector(4 downto 0);
                 blk_out_2:  out std_logic_vector(4 downto 0);
                 blk_out_3:  out std_logic_vector(4 downto 0)
             );
    end component;

    --  Specifies which entity is bound with the component.
    for encoder_0: encoder use entity work.encoder;
    signal clk:         std_logic;
    signal blk_in:      std_logic_vector(3 downto 0);
    signal blk_out_0:   std_logic_vector(4 downto 0);
    signal blk_out_1:   std_logic_vector(4 downto 0);
    signal blk_out_2:   std_logic_vector(4 downto 0);
    signal blk_out_3:   std_logic_vector(4 downto 0);

    begin
        --  Component instantiation.
        encoder_0: encoder
            -- THIS IS HOW IT WORKS WITH GENERICS: generic map (WORD_SIZE <= 5)
            port map (clk => clk, blk_in => blk_in,
                      blk_out_0 => blk_out_0,
                      blk_out_1 => blk_out_1,
                      blk_out_2 => blk_out_2,
                      blk_out_3 => blk_out_3);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the encoder.
            blk_in: std_logic_vector(3 downto 0);
            --  The expected outputs of the encoder.
            blk_out_0: std_logic_vector(4 downto 0);
            blk_out_1: std_logic_vector(4 downto 0);
            blk_out_2: std_logic_vector(4 downto 0);
            blk_out_3: std_logic_vector(4 downto 0);
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (("0000", "10001", "00000", "00000", "00000"),
        ("0000", "10010", "00000", "00000", "00000"),
        ("0000", "10011", "00000", "00000", "00000"),
        ("0000", "10100", "00000", "00000", "00000"),
        ("0000", "10101", "00000", "00000", "00000"),
        ("0000", "10110", "00000", "00000", "00000"),
        ("0000", "10111", "00000", "00000", "00000"),
        ("0000", "10000", "10001", "00000", "00000"),
        ("0000", "10001", "10001", "00000", "00000"),
        ("0000", "10010", "10001", "00000", "00000"),
        ("1111", "11001", "00000", "00000", "00000"),
        ("1111", "11010", "00000", "00000", "00000"),
        ("1111", "11011", "00000", "00000", "00000"),
        ("0001", "00001", "00000", "00000", "00000"),
        ("0010", "00010", "00000", "00000", "00000"),
        ("0100", "00100", "00000", "00000", "00000"),
        ("1111", "11001", "00000", "00000", "00000"),
        ("1111", "11010", "00000", "00000", "00000"),
        ("1111", "11011", "00000", "00000", "00000"),
        ("0000", "10001", "00000", "00000", "00000"),
        ("0000", "10010", "00000", "00000", "00000"),
        ("0000", "10011", "00000", "00000", "00000"),
        ("0001", "00001", "00000", "00000", "00000"),
        ("0010", "00010", "00000", "00000", "00000"),
        ("0100", "00100", "00000", "00000", "00000"),
        ("0000", "10001", "00000", "00000", "00000"),
        ("0000", "10010", "00000", "00000", "00000"),
        ("0000", "10011", "00000", "00000", "00000"));
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                blk_in <= patterns(i).blk_in;

                -- simulate the clock
                clk <= '0';
                wait for 1 ns;
                clk <= '1';

                --  Wait for the results.
                wait for 1 ns;

                --  Check the outputs.
                assert blk_out_0 = patterns(i).blk_out_0
                report "bad encoding in output word 0. Expected: " & to_string(patterns(i).blk_out_0) & " but found " & to_string(blk_out_0) severity error;
                assert blk_out_1 = patterns(i).blk_out_1
                report "bad encoding in output word 1. Expected: " & to_string(patterns(i).blk_out_1) & " but found " & to_string(blk_out_1) severity error;
                assert blk_out_2 = patterns(i).blk_out_2
                report "bad encoding in output word 2. Expected: " & to_string(patterns(i).blk_out_2) & " but found " & to_string(blk_out_2) severity error;
                assert blk_out_3 = patterns(i).blk_out_3
                report "bad encoding in output word 3. Expected: " & to_string(patterns(i).blk_out_3) & " but found " & to_string(blk_out_3) severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
