library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity encoder_tb is
    end encoder_tb;

architecture behav of encoder_tb is
    --  Declaration of the component that will be instantiated.
    component encoder
        port (
                 clk:        in std_logic;
                 blk_in:     in std_logic_vector(3 downto 0);
                 blk_out:    out std_logic_vector(4 downto 0)
             );
    end component;

    --  Specifies which entity is bound with the component.
    for encoder_0: encoder use entity work.encoder;
    signal clk:     std_logic;
    signal blk_in:  std_logic_vector(3 downto 0);
    signal blk_out: std_logic_vector(4 downto 0);
    begin
        --  Component instantiation.
        encoder_0: encoder
            -- THIS IS HOW IT WORKS WITH GENERICS: generic map (WORD_SIZE <= 5)
            port map (clk => clk, blk_in => blk_in, blk_out => blk_out);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the encoder.
            blk_in: std_logic_vector(3 downto 0);
            --  The expected outputs of the encoder.
            blk_out: std_logic_vector(4 downto 0);
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (("0000", "00000"),
        ("0000", "00000"),
        ("0001", "00001"));
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
                assert blk_out = patterns(i).blk_out
                report "bad encoding in literal word" severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
