--


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.PPU_Pack.all;

entity NES_2C02 is
  port(
		clk  : in std_logic;  -- approximation to NES mainboard clock 21.47727
		rstn : in std_logic;

		-- CPU Bus
		ChipSelect_n : in  std_logic;
		ReadWrite    : in  std_logic;       -- Write to PPU on 0
		Address      : in  std_logic_vector(2 downto 0);
		Data_in		  : in  std_logic_vector(7 downto 0);
		Data_out	  : out std_logic_vector(7 downto 0);

		-- VRAM/VROM bus
		CHR_Address : out unsigned(13 downto 0);
		CHR_Data_in    : in  std_logic_vector(7 downto 0);
		CHR_Data_out   : out std_logic_vector(7 downto 0);
		CHR_wr, CHR_ena : out std_logic;
		VRAM_mirror : in std_logic_vector(1 downto 0);

		VBlank_n : out std_logic;  -- Tied to the CPU's Non-Maskable Interrupt (NMI)     

		-- Video mode
		Mode : in std_logic;	-- 0=VGA, 1=Scart
		Edge_cut : in std_logic;	-- 0= Full screen, 1= remove 8 pixel on edges

		--VGA Output
		VGA_VSYNC, VGA_HSYNC : out std_logic;
		color : out std_logic_vector(5 downto 0);

		test_hit : out std_logic
		);
end NES_2C02;

architecture arch of NES_2C02 is

	COMPONENT VRAM_4k
	  PORT (
		 clka : IN STD_LOGIC;
		 ena : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;

	COMPONENT line_buffer
	  PORT (
		 clka : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
		 clkb : IN STD_LOGIC;
		 addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
		 doutb : OUT STD_LOGIC_VECTOR(5 DOWNTO 0)
	  );
	END COMPONENT;

  -- Internal H/V Counters
  signal HPOS : integer range 0 to 340 := 0;
  signal VPOS : integer range 0 to 261 := 0;

	-- Temp signal for Synchro
	signal Sync_tmp : std_logic;

	constant Nb_tot_lines : integer := 525;
	constant Nb_lines_trame : integer := (Nb_tot_lines -1) / 2;

  -- Nes Clock / 4 Clock divider
  signal CE_cnt : unsigned(1 downto 0) := "00";
  signal CE, CE_2 : std_logic;
  
  -- Specifies the scrolling offset within an 8 bit block
  signal Fine_HPOS : integer range 0 to 7;


  signal VBlankFlag : std_logic;
  signal HitSpriteFlag : std_logic;

  signal Status_2000 : std_logic_vector(7 downto 0) := "00000000";
  signal Status_2001 : std_logic_vector(7 downto 0) := "00000010";

  signal Data_in_d  : std_logic_vector(7 downto 0) := "00000000";
  signal Addr_delay : std_logic_vector(2 downto 0);
  signal CPUPortDir : std_logic;

  signal ChipSelect_delay, Write_wait  : std_logic;


  -- Internal Muxer outputs for read access on the PPU Memory Bus
  signal PPU_Address : unsigned(13 downto 0);
  signal PPU_Data    : std_logic_vector(7 downto 0);

  -- This signal does not exist in the original hardware, which relies on bus capacitance
  signal CPUVRAM_WriteData : std_logic_vector(7 downto 0);
  signal CPUVRAM_Read      : std_logic;
  signal CPUVRAM_Write     : std_logic;

  type VRAMType is array (2047 downto 0) of std_logic_vector(7 downto 0);
  type PaletteRAMType is array (31 downto 0) of std_logic_vector(5 downto 0);

  signal VRAM      : VRAMType := (others => "00000000");
  signal VRAM_Data : std_logic_vector(7 downto 0);

  signal PaletteRAM : PaletteRAMType; --:= (
--    0  => "000000", 1 => "000001", 2 => "000010", 3 => "000011",
--    4  => "000100", 5 => "000101", 6 => "000110", 7 => "000111",
--    8  => "001000", 9 => "001001", 10 => "001010", 11 => "001011",
--    12 => "001100", 13 => "001101", 14 => "001110", 15 => "001111",
--    16 => "010000", 17 => "010001", 18 => "010010", 19 => "010011",
--    20 => "010100", 21 => "010101", 22 => "010110", 23 => "010111",
--    24 => "011000", 25 => "011001", 26 => "011010", 27 => "011011",
--    28 => "011100", 29 => "011101", 30 => "011110", 31 => "011111"
--    );

  signal SpriteVRAM_Address    : unsigned(13 downto 0);
  signal SpriteRAM_Address     : unsigned(7 downto 0) := X"00";
  signal SpriteRAM_Data_in     : std_logic_vector(7 downto 0);
  signal SpriteRAM_Data_out    : std_logic_vector(7 downto 0);
  signal SpriteRAM_WriteEnable : std_logic;

  signal SpriteColor              : unsigned(3 downto 0);
  signal SpriteForegroundPriority : std_logic;
  signal SpriteIsPrimary          : std_logic;
  signal SpriteOverflowFlag       : std_logic;

  signal TileVRAM_Address , test                  : unsigned(13 downto 0);

  signal TileColor : unsigned(3 downto 0);


  signal FineXScrolling : unsigned(2 downto 0) := "000";
  signal EnableTileRendering : std_logic;

  signal Loopy_t       : unsigned(14 downto 0) := (others => '0');
  signal Loopy_v       : unsigned(14 downto 0);
  signal ResetXCounter : std_logic;
  signal ResetYCounter : std_logic;
  signal IncXScroll    : std_logic;
  signal IncYScroll    : std_logic;
  signal LoadAddress   : std_logic;
  signal IncAddress    : std_logic;
  signal AddressStep   : std_logic;

  signal ena_VRAM : std_logic;
  
  signal addr_LB, addr_VGA : std_logic_vector(8 downto 0);
  signal din_LB, dout_LB : std_logic_vector(5 downto 0);
  signal we_LB : std_logic;
  
  signal Addr_VRAM : std_logic_vector(11 downto 0);
  
  signal v_rendering, h_rendering, th_rendering, sh_rendering, PreRendering : std_logic;

begin
  CHR_Address <= PPU_Address;
  CHR_Data_out <= CPUVRAM_WriteData;
  CHR_wr <= CPUVRAM_Write;
  CHR_ena <= not PPU_Address(13);
  
  VBlank_n <= VBlankFlag nand Status_2000(7);  -- Check on flag and VBlank Enable

  Fine_HPOS <= HPOS; -- mod 8;
  
  -- Signals = '1' when screen is rendering
  v_rendering <= '1' when (VPOS < 240 and Status_2001(4 downto 3) /= "00") else '0';
  h_rendering <= '1' when (HPOS < 256 and Status_2001(4 downto 3) /= "00") else '0';
	-- Sprite horizontal rendering
  sh_rendering <= h_rendering when Status_2001(2) = '1' else
						h_rendering when HPOS >= 8 else
						'0';
	-- Tile horizontal rendering
  th_rendering <= h_rendering when Status_2001(1) = '1' else
						h_rendering when HPOS >= 8 else
						'0';
  PreRendering <= '1' when VPOS = 261 else '0';
  
  process(clk)
	variable HPOS_tmp : integer range 0 to 340*4;
	variable HPOS_VGA : integer range -256 to 256;
	variable Sync, vs_tmp, hs_tmp, LB_select : std_logic;
	variable color_tmp : std_logic_vector(5 downto 0);
	constant h_offset : integer := -69;
	constant v_offset : integer := 0;
  begin
    if rising_edge(clk) then
		HPOS_tmp := HPOS_tmp + 1;
		HPOS_VGA := HPOS_VGA + 1;
		HPOS <= HPOS_tmp / 4;
		CE <= '0';
      if (HPOS_tmp mod 4) = 1 then
			CE <= '1';
		end if;
		CE_2 <= '0';
      if (HPOS_tmp mod 4) = 3 then
			CE_2 <= '1';
		end if;
		if (HPOS_tmp / 4) > 340 then
		HPOS_tmp := 0;
			if PreRendering = '0' then--VPOS < 261 then
				VPOS <= VPOS + 1;
				LB_select := not LB_select;
			else
				VPOS <= 0;
			end if;
		end if;
		
		-- NTSC Synchro
		Sync := '1';
		Case VPOS is
			when 239 + v_offset => --End by pre equalization
				if (HPOS_tmp >= 1162 and HPOS_tmp < 1212) then
					Sync := '0';
				end if;		
			when 240 + v_offset | 241 + v_offset | 246 + v_offset | 247 + v_offset =>
				if (HPOS_tmp >= (482 + h_offset) and HPOS_tmp < (532 + h_offset)) or (HPOS_tmp >= (1162 + h_offset) and HPOS_tmp < (1212 + h_offset)) then
					Sync := '0';
				end if;
			when 242 + v_offset =>
				if (HPOS_tmp >= (482 + h_offset) and HPOS_tmp < (532 + h_offset)) or (HPOS_tmp >= (1162 + h_offset)) then
					Sync := '0';
				end if;
			when 243 + v_offset | 244 + v_offset =>
				if (HPOS_tmp < (381 + h_offset)) or (HPOS_tmp >= (480 + h_offset) and HPOS_tmp < (1063 + h_offset)) or (HPOS_tmp >= (1162 + h_offset)) then
					Sync := '0';
				end if;
			when 245 + v_offset =>
				if (HPOS_tmp < (381 + h_offset)) or (HPOS_tmp >= (480 + h_offset) and HPOS_tmp < (1063 + h_offset)) or (HPOS_tmp >= (1162 + h_offset) and HPOS_tmp < (1212 + h_offset)) then
					Sync := '0';
				end if;
			when 248 + v_offset =>
				if (HPOS_tmp >= (482 + h_offset) and HPOS_tmp < (532 + h_offset)) or (HPOS_tmp >= (1162 + h_offset) and HPOS_tmp < (1263 + h_offset)) then
					Sync := '0';
				end if;		
			when others =>
				--Sync signal
				if HPOS_tmp >= (1162 + h_offset) and HPOS_tmp < (1263 + h_offset) then
					Sync := '0';
				end if;
		end case;

		
		--VGA timing and double line adapter
			--H_sync
				-- 4 VGA lines per PPU line (VGA 1280 * 960)
			if (HPOS_tmp >= 0 and HPOS_tmp < 0 + 21)					-- First line
				or (HPOS_tmp >= 341 and HPOS_tmp < 341 + 21)			-- Second line
				or (HPOS_tmp >= 682 and HPOS_tmp < 682 + 21)			-- Third line
				or (HPOS_tmp >= 1023 and HPOS_tmp < 1023 + 21) then	-- Fourth line
				hs_tmp := '0';
				HPOS_VGA := - 52;
			else
				hs_tmp := '1';
			end if;
			--V_sync
			if VPOS = 245 and HPOS_tmp < 1024 then	-- 245
				vs_tmp := '1';
			else
				vs_tmp := '0';
			end if;

			-- Read / Write addres for line buffer
			addr_VGA <= std_logic_vector(to_unsigned(VPOS mod 2, 1) & to_unsigned(HPOS_VGA, 8));

			-- Visible screen (delay of 1 clock to let time to fetch data from line buffer)
			if HPOS_VGA > 0 and HPOS_VGA <= 256 then
				color_tmp := dout_LB;
			else
				color_tmp := (others => '1');
			end if;
		  
		--Switch between VGA and Scart video mode
		if Mode = '0' then
			VGA_VSYNC <= vs_tmp;
			VGA_HSYNC <= hs_tmp;
			color <= color_tmp;
		else
			VGA_VSYNC <= '1';
			VGA_HSYNC <= Sync;
			color <= din_LB;
		end if;
    end if;
  end process;
  
  Inst_line_buffer : line_buffer
  PORT MAP (
    clka => not clk,
    wea(0) => we_LB,
    addra => addr_LB,
    dina => din_LB,
    clkb => not clk,
    addrb => addr_VGA,
    doutb => dout_LB
  );
  
  process(clk)
    variable color : integer range 0 to 31;
  begin
    if rising_edge(clk) then
		we_LB <= '0';
		if CE_2 = '1' then
			din_LB <= (others => '1');
			addr_LB <= std_logic_vector((not to_unsigned(VPOS mod 2, 1)) & to_unsigned(HPOS, 8));
			if Status_2001(4 downto 3) = "00" and PreRendering = '0' then
				we_LB <= '1';	-- This will empty the line buffer when rendering if off
			end if;
			if h_rendering = '1' then
				we_LB <= '1';
				if v_rendering = '1' then
					if (SpriteForegroundPriority = '0' or TileColor(1 downto 0) = "00") and SpriteColor(1 downto 0) /= "00" then
						color := to_integer('1' & SpriteColor);
					elsif TileColor(1 downto 0) /= "00" and th_rendering = '1' then
						color := to_integer(TileColor);
					else
						color := 0;
					end if;
					
					din_LB <= PaletteRAM(color);
					if Status_2001(0) = '1' then
						din_LB(3 downto 0) <= "0000";	-- Grayscale mode
					end if;
					-- Remove 8 pixels on edges
					if Edge_cut = '1' then
						if HPOS < 8 or HPOS >= 248 or VPOS < 8 or VPOS >= 231 then
							din_LB <= (others => '1');
						end if;
					end if;
--	        if VPOS >= 230 then
--				 din_LB <= PaletteRAM(HPOS / 8);
--	        end if;

	--if VPOS=0 or HPOS=255 then
	--	test_hit <= '0';
	--else
	--	test_hit <= '1';
	--end if;
				end if;
			end if;
		end if;
    end if;
  end process;

  CPU_PORT : process(clk)
    variable PPUDATA_read : std_logic_vector(7 downto 0);
	 variable delay : integer range 0 to 7;
  begin
    if rising_edge(clk) then
      if CE = '1' then	-- and ChipSelect_N = '1'
        if HPOS > 0 and HPOS < 3 and VPOS = 241 then      -- Start VBlank period
          VBlankFlag <= '1';
        elsif HPOS > 0 and HPOS < 3 and PreRendering = '1' then  -- End VBlank Period
          VBlankFlag    <= '0';
          HitSpriteFlag <= '0';
        end if;

        -- Hack: Increment sprite RAM address after write here, to avoid off-by-one condition
        if SpriteRAM_WriteEnable = '1' then
          SpriteRAM_Address <= SpriteRAM_Address + 1;
        end if;
		  -- SpriteRAM_Address is set to 0 during Sprite data fetch of rendering lines
		  if (v_rendering = '1' or PreRendering = '1') and h_rendering = '0' and EnableTileRendering = '0' then
			 SpriteRAM_Address <= (others => '0');
		  end if;

        CPUVRAM_Write         <= '0';
        CPUVRAM_Read          <= '0';
        SpriteRAM_WriteEnable <= '0';

        IncAddress  <= '0';
        AddressStep <= '0';
        LoadAddress <= '0';

        if CPUVRAM_Read = '1' or CPUVRAM_Write = '1' then
          IncAddress  <= '1';
          AddressStep <= Status_2000(2);

          if CPUVRAM_Read = '1' then
            PPUDATA_read := PPU_Data;
          end if;
        end if;

        -- Check for Sprite 0 collision
		  if th_rendering = '1' and sh_rendering = '1' and v_rendering = '1' then
			  if TileColor(1 downto 0) /= "00" and SpriteColor(1 downto 0) /= "00" 
							and SpriteIsPrimary = '1' and Status_2001(4 downto 3) = "11" then
				 HitSpriteFlag <= '1';
			  end if;
		  end if;
      end if;

      ChipSelect_delay <= ChipSelect_n;
      Data_in_d        <= Data_in;
		Addr_delay		  <= Address;

      -- Do reads on low CS, and writes onces on low WR
			--Save that writing has been asked
		if ChipSelect_n = '0' and ReadWrite = '0' then
			Write_wait <= '1';
		elsif Write_wait = '1' then
			 Write_wait <= '0';	--Save that writing has been done
          if Addr_delay = "000" then
            Status_2000           <= Data_in_d(7 downto 2) & "00";
            Loopy_t(11 downto 10) <= unsigned(Data_in_d(1 downto 0));
          elsif Addr_delay = "001" then
            Status_2001 <= Data_in_d;
          elsif Addr_delay = "011" then
            SpriteRAM_Address <= unsigned(Data_in_d);
          elsif Addr_delay = "100" then
            SpriteRAM_Data_in     <= Data_in_d;
            SpriteRAM_WriteEnable <= '1';
          elsif Addr_delay = "101" then
            if CPUPortDir = '0' then
              FineXScrolling      <= unsigned(Data_in_d(2 downto 0));
              Loopy_t(4 downto 0) <= unsigned(Data_in_d(7 downto 3));
            else
              Loopy_t(14 downto 12) <= unsigned(Data_in_d(2 downto 0));
              Loopy_t(9 downto 5)   <= unsigned(Data_in_d(7 downto 3));
            end if;
            CPUPortDir <= not CPUPortDir;
          elsif Addr_delay = "110" then
            if CPUPortDir = '0' then
              Loopy_t(14 downto 8) <= "0" & unsigned(Data_in_d(5 downto 0));
            else
              Loopy_t(7 downto 0) <= unsigned(Data_in_d);
              LoadAddress <= '1';
              -- Loading to Loopy_v happens in Loopy_control process
            end if;
            CPUPortDir <= not CPUPortDir;
          elsif Addr_delay = "111" then
            CPUVRAM_Write     <= '1';
            CPUVRAM_WriteData <= Data_in_d;

            -- Palette RAM is not actual RAM, just directly accessed registers, so implement it here
            if Loopy_v(14 downto 8) = X"3F" then
					if Loopy_v(1 downto 0) = "00" then
						-- In PPU spec, palette locations 3F00, 3F04, 3F08 and 3F0C and mirrored to 3F10, 3F14, 3F18 and 3F1C
						-- When writing to one of theses, both are written by the same value
						PaletteRAM(to_integer('0' & Loopy_v(3 downto 2) & "00")) <= Data_in_d(5 downto 0);	
						PaletteRAM(to_integer('1' & Loopy_v(3 downto 2) & "00")) <= Data_in_d(5 downto 0);						
					else
						PaletteRAM(to_integer(Loopy_v(4 downto 0))) <= Data_in_d(5 downto 0);
					end if;
            end if;
        end if;
		end if;
      if ChipSelect_delay = '1' and ChipSelect_n = '0' and ReadWrite = '1' then
        if Address = "000" then
          Data_out <= Status_2000;
        elsif Address = "001" then
          Data_out <= Status_2001;
        elsif Address = "010" then
--          Data_out <= (6 => HitSpriteFlag, 7 => VBlankFlag, others => '0');	-- Sprite overflow not implemented
			 Data_out <= VBlankFlag & HitSpriteFlag & SpriteOverflowFlag & "00000";
        elsif Address = "100" then
          Data_out          <= SpriteRAM_Data_out;
          SpriteRAM_Address <= SpriteRAM_Address + 1;
        elsif Address = "111" then
          Data_out     <= PPUDATA_read;
          CPUVRAM_Read <= '1';
          if Loopy_v(13 downto 8) = x"3F" then
            Data_out <= "00" & PaletteRAM(to_integer(Loopy_v(4 downto 0)));
          end if;
        else
--          Data_out <= (others => 'X');  -- This should be a write only register
        end if;
		 end if;
		if ChipSelect_delay = '0' and ChipSelect_n = '1' and Addr_delay = "010" then
          VBlankFlag <= '0';            -- Reset flag at the end of read period
          CPUPortDir <= '0';            -- Reset 16 bit register selector
      end if;
    end if;
  end process;

  PPU_ADDRESS_MUXER : process(CPUVRAM_Read, CPUVRAM_Write, Loopy_v, TileVRAM_Address, SpriteVRAM_Address, HPOS,
		h_rendering, v_rendering, PreRendering)
  begin
    if CPUVRAM_Read = '1' or CPUVRAM_Write = '1' or (v_rendering = '0' and PreRendering = '0') then
      PPU_Address <= Loopy_v(13 downto 0);
	 elsif h_rendering = '1' or HPOS >= 320 then
      PPU_Address <= TileVRAM_Address;
    else
      PPU_Address <= SpriteVRAM_Address;
    end if;
  end process;

  PPU_DATA_MUXER : process(PPU_Address, VRAM_Data, PaletteRAM, CHR_Data_in)
  begin

	 if PPU_Address(13) = '1' and PPU_Address(12 downto 8) /= "11111" then  -- VRAM, $3000 to $3EFF mirrored to $2000 to 2EFF
      PPU_Data <= VRAM_Data;
		ena_VRAM <= '1';
    else
      -- Default to external PPU Data
      PPU_Data <= CHR_Data_in;
		ena_VRAM <= '0';
    end if;
  end process;

  -- The nametable VRAM was an extern SRAM IC in the NES,
  -- here it is implemented internally with BRAM

--  INTERNAL_VRAM : process(clk)
--  begin
--    if rising_edge(clk) then
--      if CE = '1' and CPUVRAM_Write = '1' and PPU_Address(13 downto 12) = "10" then
--        VRAM(to_integer(PPU_Address(10 downto 0))) <= CPUVRAM_WriteData;
--      end if;
--      VRAM_Data <= VRAM(to_integer(PPU_Address(10 downto 0)));
--    end if;
--  end process;

INTERNAL_VRAM : VRAM_4k
  PORT MAP (
    clka => clk,
    ena => ena_VRAM,
    wea(0) => CPUVRAM_Write,
		addra => Addr_VRAM,
    dina => CPUVRAM_WriteData,
    douta => VRAM_Data
  );
		-- The cartridge has tri-state access to the address lines A10/A11,
		-- so it can either provide additional 2k of SRAM, or tie them to 0
		-- to mirror the address range of the upper nametables to the lower ones
		-- Horizontal mirroring is selected when VRAM_mirror = "11", else Vertical mirroring
Addr_VRAM <= '0' & std_logic_vector(PPU_Address(11) & PPU_Address(9 downto 0)) when VRAM_mirror = "11" else	-- Horizontal
				'0' & std_logic_vector(PPU_Address(10 downto 0)) when VRAM_mirror = "10" else	-- Vertical
				VRAM_mirror & std_logic_vector(PPU_Address(9 downto 0));		-- One screen upper or lower bank


  SPRITE_SEL : SpriteSelector
    port map(
      CLK                       => CLK,
      CE                        => CE,
      RSTN                      => RSTN,
      HPOS                      => HPOS,
      VPOS                      => VPOS,
      PatternTableAddressOffset => Status_2000(3),
      SpriteColor               => SpriteColor,
      SpriteForegroundPriority  => SpriteForegroundPriority,
      SpriteIsPrimary           => SpriteIsPrimary,
      SpriteOverflowFlag        => SpriteOverflowFlag,
      VRAM_Address              => SpriteVRAM_Address,
      VRAM_Data                 => PPU_Data,
      SpriteRAM_Address         => SpriteRAM_Address,
      SpriteRAM_Data_in         => SpriteRAM_Data_in,
      SpriteRAM_Data_out        => SpriteRAM_Data_out,
      SpriteRAM_WriteEnable     => SpriteRAM_WriteEnable,
		SpriteSize					  => Status_2000(5),
		SpriteEnable(1)			  => Status_2001(4),
		SpriteEnable(0)			  => Status_2001(2),
		v_rendering					  => v_rendering,
		h_rendering					  => h_rendering);

  TILE_FETCHER : TileFetcher
    port map (
      CLK                       => CLK,
      CE                        => CE,
      RSTN                      => RSTN,
      Loopy_v                   => Loopy_v,
      FineXScrolling            => FineXScrolling,
      EnableRendering           => EnableTileRendering,
      PatternTableAddressOffset => Status_2000(4),
      Fine_HPOS                 => Fine_HPOS,
      VRAM_Address              => TileVRAM_Address,
      VRAM_Data                 => PPU_Data,
      TileColor                 => TileColor);
  
  Loopy_Scrolling_1 : Loopy_Scrolling
    port map (
      clk           => CLK,
      CE            => CE,
      rst           => RSTN,
      Loopy_t       => Loopy_t,
      Loopy_v       => Loopy_v,
      ResetXCounter => ResetXCounter,
      ResetYCounter => ResetYCounter,
      IncXScroll    => IncXScroll,
      IncYScroll    => IncYScroll,
      LoadAddress   => LoadAddress,
      IncAddress    => IncAddress,
      AddressStep   => AddressStep);

  Loopy_control : process (HPOS, VPOS, Address, ReadWrite, ChipSelect_N, Status_2001,
			h_rendering, v_rendering, PreRendering)
  begin
    ResetXCounter <= '0';
    ResetYCounter <= '0';
    IncXScroll    <= '0';
    IncYScroll    <= '0';

    EnableTileRendering <= '0';
    
    if Status_2001(3) = '1' then
		 if PreRendering = '1' then
			if HPOS >= 280 and HPOS < 304 then
			  ResetYCounter <= '1';
			end if;
		 elsif v_rendering = '1' then
			if (HPOS >= 320 and HPOS < 336) or h_rendering = '1' then
			  EnableTileRendering <= '1';
			  if HPOS mod 8 = 7 then
				 IncXScroll <= '1';
			  end if;
			elsif HPOS = 256 then
			  IncYScroll <= '1';
			elsif HPOS = 257 then
			  ResetXCounter <= '1';
			end if;
		 end if;
	 end if;
  end process Loopy_Control;
test_hit <= SpriteOverflowFlag;

end arch;
