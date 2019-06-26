-- inspired by://www.lothar-miller.de/s9y/archives/21-FIFO.html
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fifo_bb is
    Generic (
        addr_width:             natural := 8;
        word_size:              natural := 8
    );
    Port (
        CLK:                    in  std_logic;
        RESET:                  in  std_logic;
        FINAL_IN:               in  std_logic;
        BLK_IN:                 in  std_logic_vector (word_size-1 downto 0);
        WR_EN:                  in  std_logic;
        RD_EN:                  in  std_logic;
        BLK_OUT:                out std_logic_vector (word_size-1 downto 0);
        EMPTY:                  out std_logic;
        FULL:                   out std_logic;
        FINAL_OUT:              out std_logic
    );
end fifo_bb;

architecture IMP of fifo_bb is

    signal wrcnt : unsigned (addr_width-1 downto 0) := (others => '0');
    signal rdcnt : unsigned (addr_width-1 downto 0) := (others => '0');
    signal final_idx: unsigned (addr_width-1 downto 0) := (others => '0');
    signal final : boolean := false;
    type speicher is array(0 to (2**addr_width)-1) of unsigned(word_size-1 downto 0);
    signal memory : speicher;   
    signal final_out_loc : std_logic;
    signal full_loc  : std_logic;
    signal empty_loc : std_logic;

begin
    process (CLK)

        ---------------
        -- FUNCTIONS --
        ---------------

        --
        -- returns the next 32-bit word from the buffer
        --
        impure function read_next
        return std_logic_vector is
        begin
            rdcnt <= rdcnt+1;
            return std_logic_vector(memory(to_integer(rdcnt)));
        end read_next;

        ----------------
        -- PROCEDURES --
        ----------------

        --
        -- appends a new 32-bit word at the end of the buffer
        --

        procedure write_next (data_in: std_logic_vector(31 downto 0)) is
        begin
            memory(to_integer(wrcnt)) <= unsigned(data_in);
            wrcnt <= wrcnt+1;
        end procedure;

        --
        -- resets all internal signals to their default state if the RESET pin is high
        --
        procedure check_reset is
        begin
            if (RESET = '1') then
                wrcnt       <= (others => '0');
                rdcnt       <= (others => '0');
                final_idx   <= (others => '0');
                final       <= false;
            end if;
        end procedure;

    begin
        if (CLK'event and CLK = '1') then
            if (WR_EN = '1' and full_loc = '0' and not final) then
                -- write input to buffer
                write_next(BLK_IN);

                if (FINAL_IN = '1') then
                    final <= true;
                    final_idx <= wrcnt;
                end if;
            end if;

            if (RD_EN = '1' and empty_loc = '0' and final_out_loc = '0') then
                -- read output from buffer
                BLK_OUT <= read_next;
            end if;
        end if;

        check_reset;
    end process;

    final_out_loc   <= '1' when (final and final_idx+1 = rdcnt) else '0';
    full_loc        <= '1' when (rdcnt = wrcnt+1)               else '0';
    empty_loc       <= '1' when (rdcnt = wrcnt)                 else '0';
    FINAL_OUT       <= final_out_loc;
    FULL            <= full_loc;
    EMPTY           <= empty_loc;
end IMP;
