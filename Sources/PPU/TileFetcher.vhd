library IEEE;
use IEEE.STD_LOGIC_1164.all;

use IEEE.NUMERIC_STD.all;

entity TileFetcher is
  port(
    CLK  : in std_logic;
    CE   : in std_logic;
    RSTN : in std_logic;

    Loopy_v        : in unsigned(14 downto 0);
    FineXScrolling : in unsigned(2 downto 0);

    EnableRendering : in std_logic;
    PatternTableAddressOffset : in std_logic;

    Fine_HPOS : integer range 0 to 7;
   
    VRAM_Address : out unsigned(13 downto 0);
    VRAM_Data    : in  std_logic_vector(7 downto 0);

    TileColor : out unsigned(3 downto 0)
    );
end TileFetcher;

architecture Behavioral of TileFetcher is
  signal TilePattern0   : std_logic_vector(15 downto 0) := (others => '0');
  signal TilePattern1   : std_logic_vector(15 downto 0) := (others => '0');
  signal TileAttribute0 : unsigned(7 downto 0) := (others => '0');
  signal TileAttribute1 : unsigned(7 downto 0) := (others => '0');
  
  signal AttributeLatch0 : std_logic;
  signal AttributeLatch1 : std_logic;

  signal NextPattern0   : std_logic_vector(7 downto 0);
  signal NextPattern1   : std_logic_vector(7 downto 0);
  signal NextAttribute0 : std_logic;
  signal NextAttribute1 : std_logic;
  

begin
  process(TileAttribute0, TileAttribute1, TilePattern0, TilePattern1)
    variable attr_offset : integer range 0 to 7;
	 variable pattern_offset : integer range 0 to 15;
  begin
    attr_offset := to_integer(not FineXScrolling);	-- = 7 - FineXScrolling
    pattern_offset := to_integer('1' & not FineXScrolling);	-- = 15 - FineXScrolling

    TileColor <= TileAttribute1(attr_offset) & TileAttribute0(attr_offset) & 
                  TilePattern1(pattern_offset) & TilePattern0(pattern_offset);

  end process;

  SHIFT_REGS : process(CE, clk)
  begin
  if rising_edge(clk) and CE = '1' then
    if EnableRendering = '1' then
      TilePattern0 <= TilePattern0(14 downto 0) & "-";
      TilePattern1 <= TilePattern1(14 downto 0) & "-";
      TileAttribute0 <= TileAttribute0(6 downto 0) & AttributeLatch0;
      TileAttribute1 <= TileAttribute1(6 downto 0) & AttributeLatch1;
		
      if Fine_HPOS mod 8 = 0 then
        TilePattern0(7 downto 0) <= NextPattern0;
        TilePattern1(7 downto 0) <= NextPattern1;
        AttributeLatch0 <= NextAttribute0;
        AttributeLatch1 <= NextAttribute1;
      end if;
    end if;
  end if;
  end process;

  PREFETCH : process(CE, clk, rstn)
    -- This register is sometimes called PAR (Picture Address Register),
    -- in the 2C02 related patent document
    variable NextTileName : unsigned(7 downto 0);

    -- Helper variables
    variable attr_offset : unsigned(2 downto 1);
	 variable attr_offset0, attr_offset1 : integer range 0 to 7;
  begin
    if rstn = '0' then
      VRAM_Address <= (others => '0');
    elsif rising_edge(clk) and CE = '1' then
		if EnableRendering = '1' then
        case Fine_HPOS mod 8 is
          when 0 =>
            VRAM_Address <= "10" & Loopy_v(11 downto 0);
          when 1 =>
            NextTileName := unsigned(VRAM_Data);
          when 2 =>
            VRAM_Address <= "10" & Loopy_v(11 downto 10) & "1111" & Loopy_v(9 downto 7) & Loopy_v(4 downto 2);
          when 3 =>
            attr_offset    := Loopy_v(6) & Loopy_v(1);
				attr_offset0    := to_integer(attr_offset & '0');
				attr_offset1    := to_integer(attr_offset & '1');
            NextAttribute0 <= VRAM_Data(attr_offset0);
            NextAttribute1 <= VRAM_Data(attr_offset1);
          when 4 =>
            VRAM_Address <= "0" & PatternTableAddressOffset & NextTileName & "0" & Loopy_v(14 downto 12);
          when 5 =>
            NextPattern0 <= VRAM_Data;
          when 6 =>
            VRAM_Address <= "0" & PatternTableAddressOffset & NextTileName & "1" & Loopy_v(14 downto 12);
          when 7 =>
            NextPattern1 <= VRAM_Data;
--          TilePattern1 <= VRAM_Data & TilePattern1(15 downto 8);
--          TilePattern0 <= NextTilePattern0 & TilePattern0(15 downto 8);
--          TileAttribute <= NextTileAttribute;
          when others =>
        end case;
		end if;
    end if;
  end process;

end Behavioral;
