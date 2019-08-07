library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;

--  A testbench has no ports.
entity logic_or_tb is
    end logic_or_tb;

architecture behav of logic_or_tb is

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

    -- Declaration of the components that will be instantiated.
    component logic_or is
        Generic (
                    word_size:           natural := 5;
                    num_inputs:          natural := 2;
                    fill_counter_size:   natural := 32
                );
        port (
                 clk:           in std_logic;
                 blk_in:        in std_logic_vector(num_inputs*word_size-1 downto 0);
                 in_empty:      in std_logic_vector(0 to num_inputs-1);
                 out_full:      in std_logic;
                 blk_out:       out std_logic_vector(word_size-1 downto 0);
                 in_rd:         out std_logic_vector(0 to num_inputs-1);
                 out_wr:        out std_logic;
                 final_in:      in std_logic_vector(0 to num_inputs-1);
                 final_out:     out std_logic;
                 reset:         in std_logic
             );
    end component;

    component input_fifo
        Generic (
                    constant addr_width: natural := 3;
                    constant word_size: natural := 5
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (word_size-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (word_size-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               CLK   : in  STD_LOGIC;
               FINAL_IN: in std_logic;
               FINAL_OUT: out std_logic;
               RESET: in std_logic
           );
    end component;

    component output_fifo
        Generic (
                    constant addr_width: natural := 3;
                    constant word_size: natural := 5
                );
        Port ( BLK_IN   : in  STD_LOGIC_VECTOR (word_size-1 downto 0);
               WR_EN    : in  STD_LOGIC;
               BLK_OUT  : out STD_LOGIC_VECTOR (word_size-1 downto 0);
               RD_EN    : in  STD_LOGIC;
               EMPTY : out STD_LOGIC;
               FULL  : out STD_LOGIC;
               CLK   : in  STD_LOGIC;
               FINAL_IN: in std_logic;
               FINAL_OUT: out std_logic;
               RESET: in std_logic
           );
    end component;

    --  Specifies which entity is bound with the component.
    for logic_or_0: logic_or use entity work.logic_or;
    for input_fifo_0: input_fifo use entity work.FIFO_bb;
    for input_fifo_1: input_fifo use entity work.FIFO_bb;
    for output_fifo_0: output_fifo use entity work.FIFO_bb;

    -- inner signals
    signal blk_in:          std_logic_vector(9 downto 0);
    signal blk_out:         std_logic_vector(4 downto 0);
    signal in_empty:        std_logic_vector(0 to 1);
    signal out_full:        std_logic;
    signal in_rd:           std_logic_vector(0 to 1);
    signal out_wr:          std_logic;
    signal final_in:        std_logic_vector(0 to 1);
    signal final_out:       std_logic;

    -- outer signals
    signal outer_clk:      std_logic;
    signal outer_input0:   std_logic_vector(4 downto 0);
    signal outer_input1:   std_logic_vector(4 downto 0);
    signal outer_wr_en0:   std_logic;
    signal outer_wr_en1:   std_logic;
    signal outer_rd_en:    std_logic;
    signal outer_output:   std_logic_vector(4 downto 0);
    signal outer_empty:    std_logic;
    signal outer_full0:    std_logic;
    signal outer_full1:    std_logic;
    signal outer_final_in0:  std_logic;
    signal outer_final_in1:  std_logic;
    signal outer_final_out:  std_logic;
    signal outer_reset:      std_logic;

    begin
        --  Component instantiation.
        logic_or_0: logic_or
        port map (clk => outer_clk,
                  blk_in => blk_in,
                  blk_out => blk_out,
                  in_empty => in_empty,
                  out_full => out_full,
                  in_rd => in_rd,
                  final_in => final_in,
                  final_out => final_out,
                  reset => outer_reset,
                  out_wr => out_wr);

        input_fifo_0: input_fifo
        port map (CLK => outer_clk,
                  BLK_IN => outer_input0,
                  WR_EN => outer_wr_en0,
                  BLK_OUT => blk_in(4 downto 0),
                  RD_EN => in_rd(0),
                  EMPTY => in_empty(0),
                  FINAL_IN => outer_final_in0,
                  FINAL_OUT => final_in(0),
                  RESET => outer_reset,
                  FULL => outer_full0);

        input_fifo_1: input_fifo
        port map (CLK => outer_clk,
                  BLK_IN => outer_input1,
                  WR_EN => outer_wr_en1,
                  BLK_OUT => blk_in(9 downto 5),
                  RD_EN => in_rd(1),
                  EMPTY => in_empty(1),
                  FINAL_IN => outer_final_in1,
                  FINAL_OUT => final_in(1),
                  RESET => outer_reset,
                  FULL => outer_full1);

        output_fifo_0: output_fifo
        port map (CLK => outer_clk,
                  BLK_IN => blk_out,
                  WR_EN => out_wr,
                  BLK_OUT => outer_output,
                  RD_EN => outer_rd_en,
                  EMPTY => outer_empty,
                  FINAL_IN => final_out,
                  FINAL_OUT => outer_final_out,
                  RESET => outer_reset,
                  FULL => out_full);

        --  This process does the real job.
        process
        type pattern_type is record
            --  The inputs of the logic_or.
            outer_reset: std_logic;
            outer_final_in0: std_logic;
            outer_final_in1: std_logic;
            outer_input0: std_logic_vector(4 downto 0);
            outer_input1: std_logic_vector(4 downto 0);
            outer_wr_en0: std_logic;
            outer_wr_en1: std_logic;
            --  The expected outputs of the logic_or.
            outer_output: std_logic_vector(4 downto 0);
            outer_empty: std_logic;
            outer_final_out: std_logic;
        end record;
        --  The patterns to apply.
        type pattern_array is array (natural range <>) of pattern_type;
        constant patterns : pattern_array :=
        (('1', '0', '0', "00001", "UUUUU", '1', '0', "UUUUU", '1', '0'),  -- 00
         ('1', '0', '0', "10011", "00001", '1', '1', "UUUUU", '1', '0'),  -- 01
         ('1', '0', '0', "11001", "10011", '1', '1', "UUUUU", '1', '0'),  -- 02
         ('1', '0', '0', "11010", "11001", '1', '1', "UUUUU", '1', '0'),  -- 03
         ('1', '0', '0', "00001", "11010", '1', '1', "UUUUU", '1', '0'),  -- 04
         ('1', '0', '0', "00010", "00001", '1', '1', "UUUUU", '1', '0'),  -- 05
         ('1', '0', '0', "00100", "00010", '1', '1', "UUUUU", '1', '0'),  -- 06
         ('1', '0', '0', "01000", "00100", '1', '1', "UUUUU", '0', '0'),  -- 07
         ('1', '1', '0', "00000", "01000", '1', '1', "00001", '1', '0'),  -- 08
         ('1', '0', '1', "00000", "00000", '0', '1', "00001", '0', '0'),  -- 09
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 10
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 11
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 12
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '0', '0'),  -- 13
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '0', '0'),  -- 14
         ('1', '0', '0', "00000", "00000", '0', '0', "11010", '1', '0'),  -- 15
         ('1', '0', '0', "00000", "00000", '0', '0', "11010", '1', '0'),  -- 16
         ('1', '0', '0', "00000", "00000", '0', '0', "11010", '0', '0'),  -- 17
         ('1', '0', '0', "00000", "00000", '0', '0', "00001", '1', '0'),  -- 18
         ('1', '0', '0', "00000", "00000", '0', '0', "00001", '0', '0'),  -- 19
         ('1', '0', '0', "00000", "00000", '0', '0', "00010", '1', '0'),  -- 20
         ('1', '0', '0', "00000", "00000", '0', '0', "00010", '0', '0'),  -- 21
         ('1', '0', '0', "00000", "00000", '0', '0', "00100", '1', '0'),  -- 22
         ('1', '0', '0', "00000", "00000", '0', '0', "00100", '0', '0'),  -- 23
         ('1', '0', '0', "00000", "00000", '0', '0', "01000", '1', '0'),  -- 24
         ('1', '0', '0', "00000", "00000", '0', '0', "01000", '0', '0'),  -- 25
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '1', '1'),  -- 26
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '1', '1'),  -- 27
         ('0', '0', '0', "00000", "00000", '0', '0', "10001", '1', '0'),  -- 28
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '1', '0'),  -- 29
         ('1', '0', '0', "10101", "10010", '1', '1', "10001", '1', '0'),  -- 30
         ('1', '0', '0', "00001", "00001", '1', '1', "10001", '1', '0'),  -- 31
         ('1', '1', '1', "10010", "10101", '1', '1', "10001", '1', '0'),  -- 32
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '1', '0'),  -- 33
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '1', '0'),  -- 34
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '1', '0'),  -- 35
         ('1', '0', '0', "00000", "00000", '0', '0', "10001", '0', '0'),  -- 36
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 37
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 38
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '0', '0'),  -- 39
         ('1', '0', '0', "00000", "00000", '0', '0', "00001", '1', '0'),  -- 40
         ('1', '0', '0', "00000", "00000", '0', '0', "00001", '0', '0'),  -- 41
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 42
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 43
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '0', '0'),  -- 44
         ('1', '0', '0', "00000", "00000", '0', '0', "00001", '1', '0'),  -- 45
         ('1', '0', '0', "00000", "00000", '0', '0', "00001", '0', '0'),  -- 46
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '1'),  -- 47
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '1'),  -- 48
         ('0', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 49
         ('1', '0', '1', "11001", "11011", '1', '1', "10010", '1', '0'),  -- 50
         ('1', '0', '0', "01111", "00000", '1', '0', "10010", '1', '0'),  -- 51
         ('1', '1', '0', "11001", "00000", '1', '0', "10010", '1', '0'),  -- 52
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 53
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 54
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '1', '0'),  -- 55
         ('1', '0', '0', "00000", "00000", '0', '0', "10010", '0', '0'),  -- 56
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '0'),  -- 57
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '0', '0'),  -- 58
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '0'),  -- 59
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '0', '0'),  -- 60
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 61
         ('0', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 62
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 63
         ('1', '0', '0', "10001", "10010", '1', '1', "11001", '1', '1'),  -- 64
         ('1', '0', '1', "10010", "10101", '1', '1', "11001", '1', '1'),  -- 65
         ('1', '0', '0', "00000", "00000", '1', '0', "11001", '1', '1'),  -- 66
         ('1', '0', '0', "10001", "00000", '1', '0', "11001", '1', '1'),  -- 67
         ('1', '1', '0', "10010", "00000", '1', '0', "11001", '1', '1'),  -- 68
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 69
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 70
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 71
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 72
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 73
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 74
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 75
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 76
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 77
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 78
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 79
         ('0', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 80
         ('1', '0', '0', "00001", "10001", '1', '1', "11001", '1', '1'),  -- 81
         ('1', '0', '0', "00010", "10001", '1', '1', "11001", '1', '1'),  -- 82
         ('1', '0', '0', "00100", "10001", '1', '1', "11001", '1', '1'),  -- 83
         ('1', '1', '1', "01000", "10001", '1', '1', "11001", '1', '1'),  -- 84
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 85
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 86
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 87
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 88
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 89
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 90
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 91
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 92
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 93
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 94
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 95
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 96
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 97
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 98
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  -- 99
         ('0', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --100
         ('1', '0', '0', "10001", "10001", '1', '1', "11001", '1', '1'),  --101
         ('1', '0', '0', "10000", "10000", '1', '1', "11001", '1', '1'),  --102
         ('1', '0', '0', "10001", "10001", '1', '1', "11001", '1', '1'),  --103
         ('1', '1', '1', "10000", "10000", '1', '1', "11001", '1', '1'),  --104
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --105
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --106
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --107
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --108
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --109
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --110
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --111
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --112
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --113
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --114
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --115
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --116
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --117
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --118
         ('0', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --119
         ('1', '0', '0', "00001", "10010", '1', '1', "11001", '1', '1'),  --120
         ('1', '0', '0', "10011", "00001", '1', '1', "11001", '1', '1'),  --121
         ('1', '0', '0', "00001", "10011", '1', '1', "11001", '1', '1'),  --122
         ('1', '0', '0', "10011", "00001", '1', '1', "11001", '1', '1'),  --123
         ('1', '1', '1', "00001", "10010", '1', '1', "11001", '1', '1'),  --124
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --125
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --126
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --127
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --128
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --129
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --130
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --131
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --132
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --133
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --134
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --135
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --136
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --137
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --138
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --139
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --140
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --141
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --142
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --143
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --144
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --145
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --146
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --147
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --148
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --149
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --140
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --151
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --152
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --153
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --154
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --155
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --156
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --157
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --158
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --159
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --160
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --161
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --162
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1'),  --163
         ('1', '0', '0', "00000", "00000", '0', '0', "11001", '1', '1')); --164
        begin
            assert false report "begin of test" severity note;

            --  Check each pattern.
            for i in patterns'range loop
                --  Set the inputs.
                outer_wr_en0 <= patterns(i).outer_wr_en0;
                outer_wr_en1 <= patterns(i).outer_wr_en1;
                outer_input0<= patterns(i).outer_input0;
                outer_input1<= patterns(i).outer_input1;
                outer_reset <= patterns(i).outer_reset;
                outer_final_in0 <= patterns(i).outer_final_in0;
                outer_final_in1 <= patterns(i).outer_final_in1;
                outer_rd_en <= '1';

                -- simulate the clock
                outer_clk <= '0';
                wait for 1 ns;
                outer_clk <= '1';

                --  Wait for the results.
                wait for 1 ns;

                --  Check the outputs.
                --assert outer_empty = patterns(i).outer_empty
                --report "empty state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_empty) & " but found " & std_logic'image(outer_empty) severity error;

                --assert outer_final_out = patterns(i).outer_final_out
                --report "final state incorrect in test " & integer'image(i) & ". Expected: " & std_logic'image(patterns(i).outer_final_out) & " but found " & std_logic'image(outer_final_out) severity error;

                --assert outer_output = patterns(i).outer_output
                --report "bad decoding in test " & integer'image(i) & ". Expected: " & to_string(patterns(i).outer_output) & " but found " & to_string(outer_output) severity error;
            end loop;

            assert false report "end of test" severity note;
            --  Wait forever; this will finish the simulation.
            wait;
        end process;
    end behav;
