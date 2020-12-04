 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SpriteSelector is
  port (
    CLK : in std_logic;
    CE : in std_logic;
    RSTN : in std_logic;
        
    HPOS : in integer range 0 to 340;
    VPOS : in integer range 0 to 261;
    
		PatternTableAddressOffset : in std_logic;
    
    -- Selector output         
    SpriteColor : out unsigned(3 downto 0); -- Palette index of sprite at current VPOS/HPOS pixel position
    SpriteForegroundPriority : out std_logic; -- When '0', Sprite is only drawn when background is transparent ("00" Color)
    SpriteIsPrimary : out std_logic; -- Is '1' when the current output results from object #0, used for collision detection flag
        
    SpriteOverflowFlag : out std_logic; -- When more than 8 Sprites are detected on a scanline, this flag is set until the next VBlank period
        
    VRAM_Address : out unsigned(13 downto 0) := (others => '0');
    VRAM_Data : in std_logic_vector(7 downto 0);
        
    SpriteRAM_Address : in unsigned(7 downto 0);
    SpriteRAM_Data_in : in std_logic_vector(7 downto 0);
    SpriteRAM_Data_out : out std_logic_vector(7 downto 0);
    SpriteRAM_WriteEnable : in std_logic;
	 SpriteSize : in std_logic;
	 
	 SpriteEnable : in std_logic_vector(1 downto 0);	-- Bit 4 and 2 of PPU Mask register ($2001)
	 v_rendering, h_rendering : in std_logic
  );
end SpriteSelector;


architecture arch of SpriteSelector is
  
  -- Sprite RAM, also called (primary) OAM (Object Attribute Memory) is organized in 4 bytes per sprite,
  -- so 64 Sprites totalling 256 bytes can be described at a time.
  -- Memory layout of a Sprite Entry:
  -- RAM(SpriteNum * 4 + 0) = Y position, evaluated one scanline before the sprite is rendered
  -- RAM(SpriteNum * 4 + 1) = Tile Index
  -- RAM(SpriteNum * 4 + 2) = (7 => Y-Flip, 6 => X-Flip, 5 => BGPriority, (1 downto 0) => ColorPalette)
  -- RAM(SpriteNum * 4 + 3) = X position
  
    
  -- Each scanline, while memory access is reserved to the Background Tile Pipeline, the Sprite Selector
  -- selects up to 8 sprites from the OAM to be drawn in the next scanline and stores the necessary
  -- information in the "sprite temporary buffer".
  --
  -- After this period, the Sprite Selector gains read access to the PPU Memory bus and reads the
  -- corresponding tile patterns and writes them to the "secondary" OAM.
  
  
  -- The TempLineBuffer stores the result of the sprite evaluation phase. The MEMFETCH process
  -- reads 2 bytes from the Pattern Table, optionally flips them, and writes them to the Secondary OAM
  -- along with the remaining attributes.
  
  type TempLineBufferEntry is record
		patternIndex : unsigned(7 downto 0);
		ydiff : unsigned(7 downto 0);
		x : unsigned(7 downto 0);
		palette : unsigned(1 downto 0);
		xflip : std_logic;		
		foreground : std_logic;
		primary : std_logic;
  end record;
  
  constant sprites_per_line : natural := 8;
  
  constant TempLineBufferDefault : TempLineBufferEntry := (
    patternIndex => X"FF", ydiff => X"FF", x => X"FF",
    palette => "11", others => '1'
  );
  
	type TempLineBufferType is array(sprites_per_line - 1 downto 0) of TempLineBufferEntry;
  signal TempLineBuffer : TempLineBufferType;
    
  
  -- This Datatype	corresponds to the "secondary OAM"
	type LineBufferEntry is record
		x : unsigned(7 downto 0);
		pattern0 : unsigned(7 downto 0);
		pattern1 : unsigned(7 downto 0);
		palette : unsigned(1 downto 0);
		foreground : std_logic;
		primary : std_logic;
	end record;
  
  constant LineBufferDefault : LineBufferEntry := (
    x => X"00", pattern0 => X"00", pattern1 => X"00",
    palette => "00", foreground => '0', primary => '0'
  );
	
	type LineBufferType is array(sprites_per_line - 1 downto 0) of LineBufferEntry;
	signal SpriteLineBuffer : LineBufferType := (others => LineBufferDefault);
	signal PreRendering : std_logic;
	
	type SpriteRAMType is array(255 downto 0) of std_logic_vector(7 downto 0);
	signal SpriteRAM : SpriteRAMType; -- := (
--	   0 => X"30", 1 => X"23", 2 => "11111111", 3 => X"30",
--	   4 => X"30", 5 => X"33", 6 => "11111111", 7 => X"50",
--	   8 => X"50", 9 => X"25", 10 => "11111111", 11 => X"50",
--	   12 => X"70", 13 => X"55", 14 => "11111111", 16 => X"30",
--	   others => X"FF");
	
	signal NumSpritesFound : integer range 0 to 8 := 0;
	
	-- reverse Function used to implement the X-Flip feature
	function reverse(p: unsigned) return unsigned is
    variable temp: unsigned(p'reverse_range);
    variable result: unsigned(p'range);
  begin
    for i in p'range loop
      temp(i) := p(i);
  end loop; 
    result := temp;
    return result;
  end;
   
begin
  SPRITE_RAM_PORT_A : process (clk)
  begin
    if rising_edge(clk) and CE = '1' then
      SpriteRAM_Data_out <= SpriteRAM(to_integer(SpriteRAM_Address));
      if (SpriteRAM_WriteEnable = '1') then
        SpriteRAM(to_integer(SpriteRAM_Address)) <= SpriteRAM_Data_in;
      end if;
	end if;
  end process;
  
  -- The Line Buffer contains up to 8 sprites, select the first one with non-zero color
  PIXEL_MUX : process (SpriteLineBuffer, HPOS, SpriteEnable)
  variable sprite : LineBufferEntry;
  variable patternColor : unsigned(1 downto 0);
  begin
	
    SpriteColor <= "0000";
    SpriteForegroundPriority <= '1';
    SpriteIsPrimary <= '0';
    
	 if SpriteEnable = "11" or (SpriteEnable = "10" and HPOS > 7) then
		 for i in 7 downto 0 loop -- Loop backwards to prioritize the first entry, as it is written last
			sprite := SpriteLineBuffer(i);
			if sprite.x = 0 then
			  patternColor := sprite.pattern1(0) & sprite.pattern0(0);
			  
			  if patternColor /= "00" then
				 SpriteColor <= unsigned(sprite.palette & patternColor);
				 SpriteForegroundPriority <= sprite.foreground;
				 SpriteIsPrimary <= sprite.primary;
			  end if;
			end if;
		 end loop;
	 end if;
  end process;

	PreRendering <= '1' when VPOS = 261 else '0';
   
  SPRITE_LOOKUP : process (clk, rstn)
	   variable attributeByte : std_logic_vector(7 downto 0);
	   variable CurrentOAMIndex : integer range 0 to 255;
		variable OAMstep : std_logic_vector(2 downto 0);
		variable SecondOAMcnt : integer range 0 to 7;
		variable ydiff : unsigned(7 downto 0);
		variable LineSpriteFull : std_logic;
  	begin
		if rstn = '0' then
			--SpriteCache <= (others => (others => (others => '0')));
			--CurrentSpriteIndex <= 0;
		elsif rising_edge(clk) and CE = '1' then
		  
			if PreRendering = '1' and HPOS = 1 then
			  SpriteOverflowFlag <= '0';
			end if;
			
			if HPOS = 340 then
				NumSpritesFound <= 0;
--        TempLineBuffer <= (others => TempLineBufferDefault);
			  CurrentOAMIndex := 0;
			  OAMstep := "001";
			elsif h_rendering = '1' and v_rendering = '1' then
			
					-- Secondary OAM clear
				if HPOS < 64 then
					SecondOAMcnt := (HPOS / 8) mod 8;
					case HPOS mod 8 is
					 when 0 =>
						TempLineBuffer(SecondOAMcnt).ydiff <= x"FF";
					 when 1 =>
								-- In case of using MMC3 IRQ with 8x16 sprites, the PPU fetch a dummy byte from CHR_ROM
								-- of tile $FF if not the 8 sprites are used on a row.
						TempLineBuffer(SecondOAMcnt).patternIndex <= x"FF";
					 when 2 =>
						TempLineBuffer(SecondOAMcnt).palette <= "11";
						TempLineBuffer(SecondOAMcnt).foreground <= '1';
						TempLineBuffer(SecondOAMcnt).xflip <= '1';
						TempLineBuffer(SecondOAMcnt).primary <= '1';
					 when 3 =>
						TempLineBuffer(SecondOAMcnt).x <= x"FF";
					 when others =>
					end case;		
				 else
						-- Sprites evaluation
					if NumSpritesFound = 8 then	-- No more sprite to fetch, test for overflow
						LineSpriteFull := '1';
					else
						LineSpriteFull := '0';
					end if;
					case LineSpriteFull & OAMstep is
					 when "0001" | "1001" =>
						ydiff := to_unsigned(VPOS - to_integer(unsigned(SpriteRAM(CurrentOAMIndex))),ydiff'length);
						-- Test if the current sprite is on this row, if no, ydiff(7) := '1'
						if (ydiff(7 downto 3) and ("1111" & not SpriteSize)) /= "00000" or unsigned(SpriteRAM(CurrentOAMIndex)) >= 240 then
--						  ydiff(7) := '1';	-- Now unused because of TempLineBuffer initialization
						elsif LineSpriteFull = '1' then	-- A sprite as to be on line but already full
						  SpriteOverflowFlag <= '1';
						else
							TempLineBuffer(NumSpritesFound).ydiff <= ydiff;
						end if;
						OAMstep := "010";
						CurrentOAMIndex := CurrentOAMIndex + 1;
					 when "0010" =>
						if TempLineBuffer(NumSpritesFound).ydiff(7) = '0' then
						  TempLineBuffer(NumSpritesFound).patternIndex <= unsigned(SpriteRAM(CurrentOAMIndex));
						end if;
						-- Reads 2 OAM bytes at once
					  attributeByte := SpriteRAM(CurrentOAMIndex + 1);
					  TempLineBuffer(NumSpritesFound).palette <= unsigned(attributeByte(1 downto 0));
					  TempLineBuffer(NumSpritesFound).foreground <= attributeByte(5);
					  TempLineBuffer(NumSpritesFound).xflip <= attributeByte(6);
					  if attributeByte(7) = '1' then		--yflip
						TempLineBuffer(NumSpritesFound).ydiff(3 downto 0) <= 
								(SpriteSize & "111") xor (TempLineBuffer(NumSpritesFound).ydiff(3 downto 0));
					  end if;
					  TempLineBuffer(NumSpritesFound).primary <= '0';
					  if CurrentOAMIndex = 1 then
						 TempLineBuffer(NumSpritesFound).primary <= '1';
					  end if;
					  CurrentOAMIndex := CurrentOAMIndex + 2;
					  OAMstep := "100";
					 when "0100" =>
						TempLineBuffer(NumSpritesFound).x <= unsigned(SpriteRAM(CurrentOAMIndex));
						if TempLineBuffer(NumSpritesFound).ydiff(7) = '0' then
						  NumSpritesFound <= NumSpritesFound + 1;	--If sprite is on the row then go to fetch the next sprite
						end if;
						CurrentOAMIndex := CurrentOAMIndex + 1;
						OAMstep := "001";
					 when "1010" =>
						CurrentOAMIndex := CurrentOAMIndex + 2;
						OAMstep := "100";
					 when others =>
						CurrentOAMIndex := CurrentOAMIndex + 1;
						OAMstep := "001";				
					end case;
				end if;

		 
--			  case HPOS mod 4 is
--			    when 0 =>
--			      TempLineBuffer(NumSpritesFound).ydiff <= VPOS - unsigned(SpriteRAM(CurrentSpriteIndex * 4));
--			    when 1 =>
--			      if (TempLineBuffer(NumSpritesFound).ydiff(7 downto 3) and ("1111" & not SpriteSize)) = "00000" then
--			        TempLineBuffer(NumSpritesFound).patternIndex <= unsigned(SpriteRAM(CurrentSpriteIndex * 4 + 1));
--					else
--						-- In case of using MMC3 IRQ with 8x16 sprites, the PPU fetch a dummy byte from CHR_ROM
--						-- of tile $FF if not the 8 sprites are used on a row.
--					  TempLineBuffer(NumSpritesFound).patternIndex <= x"FF";
--			      end if;
--			    when 2 =>
----			      if (TempLineBuffer(NumSpritesFound).ydiff(7 downto 3) and ("1111" & not SpriteSize)) = "00000" then
--			        attributeByte := SpriteRAM(CurrentSpriteIndex * 4 + 2);
--			        TempLineBuffer(NumSpritesFound).palette <= unsigned(attributeByte(1 downto 0));
--			        TempLineBuffer(NumSpritesFound).foreground <= attributeByte(5);
--			        TempLineBuffer(NumSpritesFound).xflip <= attributeByte(6);
--					  if attributeByte(7) = '1' then		--yflip
--						TempLineBuffer(NumSpritesFound).ydiff(3 downto 0) <= 
--								(SpriteSize & "111") xor (TempLineBuffer(NumSpritesFound).ydiff(3 downto 0));
--					  end if;
--              TempLineBuffer(NumSpritesFound).primary <= '0';
--  			        if CurrentSpriteIndex = 0 then
--  			          TempLineBuffer(NumSpritesFound).primary <= '1';
--			        end if;			          
----			      end if;
--			    when 3 =>
--			      TempLineBuffer(NumSpritesFound).x <= unsigned(SpriteRAM(CurrentSpriteIndex * 4 + 3));
--			      if (TempLineBuffer(NumSpritesFound).ydiff(7 downto 3) and ("1111" & not SpriteSize)) = "00000" then
--			        NumSpritesFound <= NumSpritesFound + 1;	--If sprite is on the row then go to fetch the next sprite
--			      end if;
--			    when others =>
--		    end case;
		    
			end if;
		end if;
	end process;
	
	
	SPRITE_MEMFETCH : process (clk)
	variable currentSprite : integer range 0 to 7;
	variable patternAddress : unsigned(13 downto 0);
	variable fetchedByte : unsigned(7 downto 0);
	begin
	  if rising_edge(clk) and CE = '1' then
	  if v_rendering = '1' or PreRendering = '1' then
	    if HPOS > 0 and HPOS <= 256 then
			 for i in 7 downto 0 loop
				if SpriteLineBuffer(i).x > 0 then
				  SpriteLineBuffer(i).x <= SpriteLineBuffer(i).x - 1;
				else
				  SpriteLineBuffer(i).pattern0 <= '0' & SpriteLineBuffer(i).pattern0(7 downto 1); -- srl 1;
				  SpriteLineBuffer(i).pattern1 <= '0' & SpriteLineBuffer(i).pattern1(7 downto 1); -- srl 1;
				end if;
			 end loop;
		  elsif HPOS > 256 and HPOS <= 288 then
		    currentSprite := (HPOS mod 256) / 4;
			 
			if SpriteSize = '1' then        --16 pixels sprite
				  patternAddress := '0' & TempLineBuffer(currentSprite).patternIndex(0) & TempLineBuffer(currentSprite).patternIndex(7 downto 1)
							& TempLineBuffer(currentSprite).ydiff(3) & '0' &  TempLineBuffer(currentSprite).ydiff(2 downto 0);
			else
				  patternAddress := '0' & PatternTableAddressOffset & TempLineBuffer(currentSprite).patternIndex
							& '0' &  TempLineBuffer(currentSprite).ydiff(2 downto 0);
			end if;
			 
		    if (currentSprite < NumSpritesFound or SpriteSize = '1') and SpriteEnable(1) = '1' then 
		      case HPOS mod 4 is
		        when 1 =>
		          VRAM_Address <= patternAddress;
		        when 2 => 
						-- Tests if current sprite is on this row
					 fetchedByte := unsigned(VRAM_Data);
					 if TempLineBuffer(currentSprite).ydiff(7) = '1' then
						fetchedByte := x"00";
					 end if;
		          if TempLineBuffer(currentSprite).xflip = '0' then
		            fetchedByte := reverse(fetchedByte);
		          end if;
		          
		          SpriteLineBuffer(currentSprite).pattern0 <= fetchedByte;
		          
					 --VRAM_Address bit 3 = '1' to fetch the second bitmap
					 VRAM_Address <= patternAddress(13 downto 4) & '1' & patternAddress(2 downto 0);
		        when 3 =>		          
		          fetchedByte := unsigned(VRAM_Data);
					 if TempLineBuffer(currentSprite).ydiff(7) = '1' then
						fetchedByte := x"00";
					 end if;
		          if TempLineBuffer(currentSprite).xflip = '0' then
		            fetchedByte := reverse(fetchedByte);
		          end if;
		          
		          SpriteLineBuffer(currentSprite).pattern1 <= fetchedByte;
		          SpriteLineBuffer(currentSprite).x <= TempLineBuffer(currentSprite).x;
		          SpriteLineBuffer(currentSprite).palette <= TempLineBuffer(currentSprite).palette;
		          SpriteLineBuffer(currentSprite).primary <= TempLineBuffer(currentSprite).primary;
		          SpriteLineBuffer(currentSprite).foreground <= TempLineBuffer(currentSprite).foreground;
		          
		        when others =>
		      end case;
			 else
				SpriteLineBuffer(currentSprite).pattern0 <= (others => '0');
				SpriteLineBuffer(currentSprite).pattern1 <= (others => '0');
		    end if;
		    		  
		  end if;
	  end if;  
    end if;
  end process;
end arch;
