----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Paul Prudhomme
-- 
-- Create Date:    08:07:37 04/11/2019 
-- Design Name: 
-- Module Name:    RAM_Init - Behavioral 
-- Project Name: 
-- Target Devices:
-- Tool versions: 
-- Description: Implementation of a SPI Flash reader for ines files format. Includes 8k of RAM
--							saving on the SPI Flash.
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity INES_reader is
    Port ( clk, clk_50MHz, M2 : in  STD_LOGIC;
           reset : in  STD_LOGIC;
			  test : out std_logic_vector(7 downto 0);
           done : out  STD_LOGIC;
			  save_pending : out STD_LOGIC;
			  save_error : out STD_LOGIC;
			  irq_n : out  STD_LOGIC;
			  VRAM_mirror : out STD_LOGIC_VECTOR(1 downto 0);
			  -- SRAM1 ctrl
           address1 : in  STD_LOGIC_VECTOR (14 downto 0);
           data_out1, WRAM_dout : out  STD_LOGIC_VECTOR (7 downto 0);
			  data_in1 : in  STD_LOGIC_VECTOR (7 downto 0);
			  ena1, wr1, WRAM_ena : in STD_LOGIC;
			  -- SRAM1 port
			  SRAM1_add : out std_logic_vector(18 downto 0);
			  SRAM1_data : inout std_logic_vector(7 downto 0);
			  SRAM1_nCS1 : out std_logic;
			  SRAM1_nOE : out std_logic;
			  SRAM1_nWE : out std_logic;
			  -- SRAM2 ctrl
           address2 : in  STD_LOGIC_VECTOR (12 downto 0);
           data_out2 : out  STD_LOGIC_VECTOR (7 downto 0);
			  data_in2 : in  STD_LOGIC_VECTOR (7 downto 0);
			  ena2, wr2 : in STD_LOGIC;
			  -- SRAM2 port
			  SRAM2_add : out std_logic_vector(18 downto 0);
			  SRAM2_data : inout std_logic_vector(7 downto 0);
			  SRAM2_nCS1 : out std_logic;
			  SRAM2_nOE : out std_logic;
			  SRAM2_nWE : out std_logic;
			  -- SPI Flash
			  spi_clk  : buffer STD_LOGIC;
			  spi_cs   : out STD_LOGIC;
			  spi_din  : in STD_LOGIC;
			  spi_dout : out STD_LOGIC;
			  spi_busy : out STD_LOGIC;
			  DBG_state : out std_logic_vector(7 downto 0));
end INES_reader;

architecture Behavioral of INES_reader is

	COMPONENT Gestion_SPI_Flash
	 PORT(
		clk : IN  std_logic;
		rst : IN  std_logic;
		spi_clk : BUFFER std_logic;
		spi_cs : OUT  std_logic;
		spi_din : IN  std_logic;
		spi_dout : OUT  std_logic;
		data_read : OUT  std_logic_vector(7 downto 0);
		data_write : IN std_logic_vector(7 downto 0);
		start_address : IN  std_logic_vector(23 downto 0);
		ask_read_data : IN  std_logic;
		ask_write_data : IN  std_logic;
		ask_write_page : IN  std_logic;
		ask_erase_sector : IN  std_logic;
		data_ready, ready_state : OUT  std_logic;
		DBG_state : out std_logic_vector(7 downto 0)
	  );
	END COMPONENT;

	COMPONENT RAM_8k
	  PORT (
		 clka : IN STD_LOGIC;
		 ena : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		 clkb : IN STD_LOGIC;
		 web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addrb : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
		 dinb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;
	
	COMPONENT ID_RAM_16
	  PORT (
		 clka : IN STD_LOGIC;
		 ena : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;

	TYPE CONTROL IS(load_data0, header_count,
						load_data1, enable1, disable1, 
						load_data2, enable2, disable2,
						check_init, check_load, check_next,
						wram_init, wram_load, wload_data, wload_disable,
						write_wram, wait_sector_erase, page_program, wait_data_write, page_write, next_page,
						init_done, std_by);
	SIGNAL state : CONTROL :=std_by;

	signal SRAM2_data_prog : std_logic_vector(7 downto 0);
	signal SRAM2_dout : std_logic; 
	signal SRAM2_nWE_prog : std_logic;
	signal SRAM_add_prog, SRAM2_add_prog : std_logic_vector(18 downto 0);
	
	signal done_tmp : std_logic;
	
	-- SPI Flash signals
	signal start_address : std_logic_vector(23 downto 0);
	signal rst_spi, ready_state : std_logic;
	signal ask_erase_sector, ask_write_data, ask_write_page, ask_data_from_Flash, data_Flash_ready : std_logic;
	signal data_read_Flash, data_write : std_logic_vector(7 downto 0);

	-- INES File headers
	signal PRG_size, CHR_size, Flag6, Flag7 : std_logic_vector(7 downto 0) := x"00";

	-- Address masks
	-- Theses signals simulates the real size ROM
	signal add1_mask : std_logic_vector(4 downto 0);	--Min PRG ROM : 16k
	signal add2_mask : std_logic_vector(5 downto 0);	--Min CHR ROM : 8k

	-- Mapper signals
	signal mapper : integer range 0 to 255;
	signal mapper1reg0 : std_logic_vector(4 downto 2);
	signal mapper1reg1, mapper1reg2, mapper1reg3 : std_logic_vector(4 downto 0);
	signal mapper2reg : std_logic_vector(2 downto 0);
	signal mapper3reg : std_logic_vector(1 downto 0);
	signal add1_mapper, add1_map1, add1_map2, add1_map4 : std_logic_vector(18 downto 13);
	signal add2_mapper, add2_map1, add2_map3, add2_map4 : std_logic_vector(18 downto 10);
	signal mapper4_BankSelect, mapper4_BankData, mapper4_IrqLatch : std_logic_vector(7 downto 0);
	signal mapper4_IrqReload, mapper4_IrqEnable : std_logic;
	signal mapper4_R0, mapper4_R1, mapper4_R2, mapper4_R3 : std_logic_vector(7 downto 0);
	signal mapper4_R4, mapper4_R5, mapper4_R6, mapper4_R7 : std_logic_vector(7 downto 0);
	signal mapper4_RamProtect : std_logic_vector(1 downto 0);
	
	-- WRAM signals
	signal WRAM_douta, WRAM_doutb, WRAM_dinb : std_logic_vector(7 downto 0);
	signal WRAM_addr : std_logic_vector(12 downto 0);
	signal write_pending, WRAM_wra, WRAM_wrb, wram_protect : std_logic;
	signal WRAM_sector : std_logic_vector(7 downto 0);
	signal save_cnt : integer range 0 to 63;
	
	-- ID_RAM signals
	signal ID_RAM_din, ID_RAM_dout : std_logic_vector(7 downto 0);
	signal ID_RAM_addr : std_logic_vector(3 downto 0);
	signal ID_RAM_ena, ID_RAM_wr : std_logic;

begin

	Inst_Gestion_SPI_Flash : Gestion_SPI_Flash Port map(
				 clk      => clk_50MHz,
				 rst      => rst_spi,
				 spi_clk  => spi_clk,
				 spi_cs   => spi_cs,
				 spi_din  => spi_din,
				 spi_dout => spi_dout,
				 data_read => data_read_Flash,
				 data_write => data_write,
				 start_address => start_address,
				 ask_read_data => ask_data_from_Flash,
				 ask_write_data => ask_write_data,
				 ask_write_page => ask_write_page,
				 ask_erase_sector => ask_erase_sector,
				 data_ready => data_Flash_ready,
				 ready_state => ready_state,
				 DBG_state => DBG_state);	

process(clk)
	variable address_tmp : integer range 0 to 2097151 :=0;	--SPI ROM 2MB
	variable save_pending_tmp : std_logic := '0';
begin
if clk'event and clk='1' then
	if reset='1' then
		rst_spi <= '1';
		done_tmp <= '0';
		SRAM1_data <= (others => 'Z');
		SRAM1_nCS1 <= '1';
		SRAM1_nOE <= '1';
		SRAM1_nWE <= '1';
--		SRAM2_data <= (others => 'Z');
		SRAM2_dout <= '0';
		SRAM2_nCS1 <= '1';
		SRAM2_nOE <= '1';
		SRAM2_nWE_prog <= '1';
		WRAM_wrb <= '0';
		address_tmp := 0;
		save_pending_tmp := '0';
		ask_write_data <= '0';
		ask_write_page <= '0';
		ask_erase_sector <= '0';
		Flag6 <= x"00";
		Flag7 <= x"00";
		spi_busy <= '0';
		state <= load_data0;
	else
		CASE state IS
			WHEN std_by =>
				spi_busy <= '0';
			--Reads Header of .INES Files
			WHEN load_data0 =>
				spi_busy <= '1';
				rst_spi <= '0';
				start_address <= x"000000";	--Starts at the begining of the SPI ROM
				ask_data_from_Flash <= '1';
				state <= load_data0;	
				if data_Flash_ready ='1' then	-- Waiting for data read from SIP Flash
					CASE address_tmp IS
						-- Check for Ines file identifier "NES + MS-DOS end-of-file"
						WHEN 0 =>
							if data_read_Flash /= x"4E" then
								state <= std_by;
							end if;
						WHEN 1 =>
							if data_read_Flash /= x"45" then
								state <= std_by;
							end if;
						WHEN 2 =>
							if data_read_Flash /= x"53" then
								state <= std_by;
							end if;
						WHEN 3 =>
							if data_read_Flash /= x"1A" then
								state <= std_by;
							end if;
						WHEN 4 =>
							PRG_size <= data_read_Flash;
						WHEN 5 =>
							CHR_size <= data_read_Flash;
						WHEN 6 =>
							Flag6 <= data_read_Flash;
						WHEN 7 =>
							Flag7 <= data_read_Flash;
						WHEN others =>	
					END CASE;
					ask_data_from_Flash <= '0'; -- acknoledge data received
					state <= header_count;
				end if;				
			WHEN header_count =>
				if data_Flash_ready ='0' then
					ask_data_from_Flash <= '1';
					if address_tmp < 15 then		-- End of Header
						address_tmp := address_tmp + 1;
						state <= load_data0;
					else
						address_tmp := 0;
						ID_RAM_addr <= (others => '0');
						-- Calculate the sector of WRAM data. Starts at the next sector of 64kB after PRG_ROM + CHR_ROM
						WRAM_sector <= std_logic_vector(to_unsigned((to_integer(unsigned(PRG_size & '0')) + to_integer(unsigned(CHR_size))) / 8 + 1, 8));
						state <= load_data1;
					end if;
				end if;	

			--Reads PRG ROM
			--SRAM1 program
			--Program the ID_RAM for the 256 first bytes to identify what SPI ROM is connected
			WHEN load_data1 =>
				if data_Flash_ready ='1' then	-- Waiting for data read from SIP Flash
					SRAM1_nCS1 <= '0';
					SRAM1_nWE <= '0';
					SRAM1_data <= data_read_Flash;
					ask_data_from_Flash <= '0'; -- acknoledge data received
					SRAM_add_prog <= std_logic_vector(to_unsigned(address_tmp, 19));
					ID_RAM_din <= data_read_Flash;
					ID_RAM_addr <= std_logic_vector(to_unsigned(address_tmp, 4));
--					if address_tmp < 256 then
						ID_RAM_ena <= '1'; ID_RAM_wr <= '1';
--					else
--						ID_RAM_ena <= '0';
--					end if;
					state <= enable1;
				end if;
			WHEN enable1 =>
				SRAM1_nWE <= '1';
				state <= disable1;
			WHEN disable1 =>
				SRAM1_nCS1 <= '1';
				ID_RAM_wr <= '0';
				if data_Flash_ready ='0' then
					ask_data_from_Flash <= '1';
					add1_mask <= std_logic_vector(to_unsigned(address_tmp/16#04000#, add1_mask'length));
					address_tmp := address_tmp + 1;
					if address_tmp / 16#04000# < to_integer(unsigned(PRG_size)) then		-- Count blocks of $4000			
						state <= load_data1;
					else
						--End of reading PRG ROM
						SRAM1_data <= (others => 'Z');
						SRAM1_nCS1 <= '1';
						SRAM1_nOE <= '1';
						SRAM1_nWE <= '1';
						address_tmp := 0;
						if CHR_size = x"00" then
							state <= wram_init;
						else
							state <= load_data2;
						end if;
					end if;
				end if;

			--SRAM2 parameter, CHR ROM start in SPI Flash after reading PRG ROM			
			--SRAM2 program
			WHEN load_data2 =>
				if data_Flash_ready ='1' then	-- Waiting for data read from SIP Flash
					SRAM2_nCS1 <= '0';
					SRAM2_nWE_prog <= '0';
					SRAM2_data_prog <= data_read_Flash;
					SRAM2_dout <= '1';
					ask_data_from_Flash <= '0'; -- acknowledge data received
					SRAM_add_prog <= std_logic_vector(to_unsigned(address_tmp, 19));
					state <= enable2;
				else
					state <= load_data2;
				end if;
			WHEN enable2 =>
				SRAM2_nWE_prog <= '1';
				state <= disable2;
			WHEN disable2 =>
				SRAM2_nCS1 <= '1';
				if data_Flash_ready ='0' then
					ask_data_from_Flash <= '1';
					add2_mask <= std_logic_vector(to_unsigned(address_tmp/16#02000#, add2_mask'length));
					address_tmp := address_tmp + 1;
					if address_tmp / 16#02000# < to_integer(unsigned(CHR_size)) then		-- Count blocks of $2000			
						state <= load_data2;
					else
						--End of reading CHR ROM
						SRAM2_dout <= '0';
						SRAM2_nCS1 <= '1';
						SRAM2_nOE <= '1';
						SRAM2_nWE_prog <= '1';
						state <= wram_init;
					end if;
				end if;			

			-- Loading of WRAM data if exists
			WHEN wram_init =>	test <= x"E4";
				-- Test if there is WRAM
				if Flag6(1) = '1' then
					start_address <= WRAM_sector & x"0000";	-- WRAM is located at the begining of a sector
					state <= wram_load;
				else
					state <= init_done;
				end if;
			WHEN wram_load =>
				rst_spi <= '1';	-- Need to reset for load new start address
				address_tmp := 0;
				if ready_state = '1' then	-- Reset took in account
					rst_spi <= '0';		
					ask_data_from_Flash <= '1';
					state <= wload_data;
				end if;
			WHEN wload_data =>
				if data_Flash_ready ='1' then	-- Waiting for data read from SIP Flash
					WRAM_wrb <= '1';
					ask_data_from_Flash <= '0'; -- acknowledge data received
					WRAM_addr <= std_logic_vector(to_unsigned(address_tmp, 13));
					WRAM_dinb <= data_read_Flash;
					state <= wload_disable;
				end if;				
			WHEN wload_disable =>
				WRAM_wrb <= '0';
				if data_Flash_ready = '0' then
					ask_data_from_Flash <= '1';
					address_tmp := address_tmp + 1;
					if address_tmp < 8192 then		-- WRAM is 8192 bytes
						state <= wload_data;
					else
						--End of reading WRAM data
						save_cnt <= 1;
						state <= init_done;
					end if;
				end if;	

			WHEN init_done =>
				rst_spi <= '1';	-- Need to reset for load new start address
				done_tmp <= '1';
				SRAM1_nCS1 <= '0';
				SRAM1_nOE <= '0';
				SRAM1_nWE <= '1';
				SRAM2_nCS1 <= '0';
				SRAM2_nOE <= '0';
				SRAM2_nWE_prog <= '1';
				ask_write_data <= '0';
				ask_write_page <= '0';
				ask_erase_sector <= '0';
				ask_data_from_Flash <= '0';
				spi_busy <= '0';

				-- Writing to SPI for saving WRAM data
				if WRAM_ena = '1' and wr1 = '1' and Flag6(1) = '1' then
					save_pending_tmp := '1';
					save_cnt <= 32;
				end if;
				if save_pending_tmp = '1' then
					if save_cnt mod 64 = 0 then
						save_pending_tmp := '0';
						state <= check_init;
					end if;
				end if;
			
			-- Check if the SPI Flash correspond to the game loaded before writing
			WHEN check_init =>
				spi_busy <= '1';
				if ready_state = '1' then	-- Let time to revert to reset state
					rst_spi <= '0';
					ask_data_from_Flash <= '1';
					start_address <= "00" & PRG_size & "00000000000000";	-- Load last 16 bytes of PRG ROM
					ID_RAM_addr <= (others => '0');
					ID_RAM_ena <= '1';
					state <= check_load;
				end if;
			WHEN check_load =>
				if data_Flash_ready ='1' then
					ask_data_from_Flash <= '0'; -- acknowledge data received
					if ID_RAM_dout = data_read_Flash then
						if ID_RAM_addr = x"F" then
							rst_spi <= '1';
							state <= write_wram;	-- Check passed
						else
							state <= check_next;
						end if;
					else
						save_error <= '1';	-- Check failed
						state <= init_done;
					end if;
				end if;				
			WHEN check_next =>
				if data_Flash_ready = '0' then
					ask_data_from_Flash <= '1';
					ID_RAM_addr <= std_logic_vector(unsigned(ID_RAM_addr) + 1);
					state <= check_load;
				end if;
						
			-- Write the content of WRAM in Spi Flash
			WHEN write_wram =>
				if ready_state = '1' then	-- Let time to revert to reset state
					rst_spi <= '0';
					ask_erase_sector <= '1';
					start_address <= WRAM_sector & x"0000";
					WRAM_addr <= (others => '0');
					state <= wait_sector_erase;
				end if;
			WHEN wait_sector_erase =>
				if data_Flash_ready = '1' then
					ask_erase_sector <= '0';
					state <= page_program;
				end if;
			WHEN page_program =>
				ask_write_page <= '1';
				ask_write_data <= '1';
				data_write <= WRAM_doutb;			
				start_address <= WRAM_sector & "000" & WRAM_addr(12 downto 8) & x"00";
				if data_Flash_ready = '0' then
					WRAM_addr <= std_logic_vector(unsigned(WRAM_addr) + 1);
					state <= wait_data_write;
					ask_write_data <= '0';
				end if;		
			WHEN wait_data_write =>
				if data_Flash_ready = '1' then
					-- Test if a page of bytes has been sent
					if WRAM_addr(7 downto 0) = x"00" then
						ask_write_page <= '0';
						state <= page_write;
					else
						state <= page_program;
					end if;
				end if;
			WHEN page_write =>
				if ready_state = '1' then	-- Wait finish writing the page
					if WRAM_addr(12 downto 8) = x"00" then
						save_error <= '0';
						state <= init_done;
					else
						state <= page_program;
					end if;
				end if;
			WHEN next_page =>
				if ready_state = '1' then	-- Wait to return to idle state
					state <= page_program;
				end if;

		END CASE;
		
		-- Timer and watchdog for WRAM save
		if done_tmp = '1' then
			address_tmp := address_tmp + 1;	-- Use the address_tmp variable as it will be unsed otherway now
			if address_tmp >= 1048576 then
				address_tmp := 0;
				if save_cnt = 0  then
					state <= init_done;	-- Return to idle_state if timing too long
				else
					save_cnt <= save_cnt + 1;
				end if;
			end if;
		end if;
	end if;
end if;
	save_pending <= save_pending_tmp;
end process;
done <= done_tmp;


-----------------------------------------------------------
--	Mappers logic

-- Mapper number
mapper <= to_integer(unsigned(Flag7(3 downto 0) & Flag6(7 downto 4)));

-- Mapper address multiplexers
add1_mapper <= 		-- Mapper 1 (MMC1) PRG till 128k (PRG_size = 08)
						'0' & mapper1reg3(3 downto 1) & address1(14 downto 13) when (mapper1reg0(3) = '0' and mapper = 1) else
						'0' & mapper1reg3(3 downto 0) & address1(13) when (address1(14) = not mapper1reg0(2) and mapper = 1) else
						"00000" & address1(13) when (mapper1reg0(2) = '0' and mapper = 1) else
							-- Mapper 2 (UxROM)
						"00" & (address1(14) or mapper2reg(2))
						& (address1(14) or mapper2reg(1)) 
						& (address1(14) or mapper2reg(0))
						& address1(13) when mapper = 2 else
							-- Mapper 4 (MMC3)
						mapper4_R6(5 downto 0) when (address1(14 downto 13) = (mapper4_BankSelect(6) & '0') and mapper = 4) else
						"111110" when (address1(14 downto 13) = ((not mapper4_BankSelect(6)) & '0') and mapper = 4) else	-- Second last 8k bank
						mapper4_R7(5 downto 0) when (address1(14 downto 13) = "01" and mapper = 4) else
						"111111" when mapper = 4 else	-- Last 8k bank
							-- No mapper
						"----" & address1(14 downto 13);

add2_mapper <=			-- Mapper 1 (MMC1)
						"00" & mapper1reg1(4 downto 1) & address2(12 downto 10) when (mapper1reg0(4) = '0' and mapper = 1) else
						"00" & mapper1reg1(4 downto 0) & address2(11 downto 10) when (address2(12) = '0' and mapper = 1) else
						"00" & mapper1reg2(4 downto 0) & address2(11 downto 10) when mapper = 1 else
							-- Mapper 3 (CNROM)
						"1111" & mapper3reg & address2(12 downto 10) when mapper = 3 else
							-- Mapper 4 (MMC3)
						'0' & mapper4_R0(7 downto 1) & address2(10) when (address2(12 downto 11) = (mapper4_BankSelect(7) & '0') and mapper = 4) else
						'0' & mapper4_R1(7 downto 1) & address2(10) when (address2(12 downto 11) = (mapper4_BankSelect(7) & '1') and mapper = 4) else	
						'0' & mapper4_R2 when (address2(12 downto 10) = ((not mapper4_BankSelect(7)) & "00") and mapper = 4) else
						'0' & mapper4_R3 when (address2(12 downto 10) = ((not mapper4_BankSelect(7)) & "01") and mapper = 4) else
						'0' & mapper4_R4 when (address2(12 downto 10) = ((not mapper4_BankSelect(7)) & "10") and mapper = 4) else
						'0' & mapper4_R5 when mapper = 4 else
							-- No mapper
						"111110" & address2(12 downto 10);
						
-- Addresses outputs
SRAM1_add <=	SRAM_add_prog when done_tmp='0' else
					(add1_mapper(18 downto 14) and add1_mask) & add1_mapper(13) & address1(12 downto 0);
SRAM2_add <=	SRAM_add_prog when done_tmp='0' else
					(add2_mapper(18 downto 13) and add2_mask) & add2_mapper(12 downto 10) & address2(9 downto 0);
data_out1 <= (others => '-') when done_tmp='0' else SRAM1_data;
data_out2 <= (others => '-') when done_tmp='0' else SRAM2_data;
SRAM2_nWE <= SRAM2_nWE_prog when done_tmp='0' else
					(ena2 nand wr2) when CHR_size = x"00" else '1';
SRAM2_data <= SRAM2_data_prog when SRAM2_dout ='1' else
					data_in2 when (ena2='1' and wr2='1') else
					(others => 'Z');

-- Mapper registers
process(M2)
	variable map1_cnt : integer range 0 to 4;
	variable map1_shift_reg : std_logic_vector(3 downto 0);
	variable map1_reg : std_logic_vector(4 downto 0);
	variable map1_write : std_logic;
	variable map4_irq_cnt : integer range 0 to 255;
	variable map4_cnt : integer range 0 to 2;
begin
if M2'event and M2='0' then
	if done_tmp = '0' then
		mapper1reg0(3 downto 2) <= "11";		--Fix address $C000-$FFFF to last bank
		mapper2reg <= (others => '0');
		VRAM_mirror <= not(Flag6(3) & Flag6(0));
		mapper4_IrqEnable <= '0';
		mapper4_RamProtect <= (others => '0');
		irq_n <= '1';
	elsif ena1 = '1' and wr1 = '1' then
									--Mapper 1 (MMC1)
		if mapper = 1 and map1_write = '0' then
			map1_write := '1';
			if data_in1(7) = '1' then		--reset the shift register when MSB = 1
				map1_cnt := 0;
				mapper1reg0(3 downto 2) <= "11";
			else
				if map1_cnt = 4 then			--at the fifth write the data is loaded into register
					map1_cnt := 0;
					map1_reg := data_in1(0) & map1_shift_reg;
					case address1(14 downto 13) is	--select the register
						when "00" => mapper1reg0 <= map1_reg(4 downto 2);	-- Control
											VRAM_mirror <= map1_reg(1 downto 0);
						when "01" => mapper1reg1 <= map1_reg;	-- CHR Bank 0
						when "10" => mapper1reg2 <= map1_reg;	-- CHR Bank 1
						when "11" => mapper1reg3 <= map1_reg;	-- PRG Bank
						when others =>
					end case;
				else								--shift LSB into shift register
					map1_cnt := map1_cnt + 1;
					map1_shift_reg := data_in1(0) & map1_shift_reg(3 downto 1);	--LSB is first
				end if;
			end if;
		elsif mapper = 2 then
			mapper2reg <= data_in1(2 downto 0);
									--Mapper 3 (CNROM)
		elsif mapper = 3 then
			mapper3reg <= (data_in1(1) and CHR_size(2)) &  data_in1(0);
		elsif mapper = 4 then
			case address1(14 downto 13) & address1(0) is	--select the register
				when "000" => mapper4_BankSelect <= data_in1;
				when "001" =>
					case mapper4_BankSelect(2 downto 0) is
						when "000" => mapper4_R0 <= data_in1;
						when "001" => mapper4_R1 <= data_in1;
						when "010" => mapper4_R2 <= data_in1;
						when "011" => mapper4_R3 <= data_in1;
						when "100" => mapper4_R4 <= data_in1;
						when "101" => mapper4_R5 <= data_in1;
						when "110" => mapper4_R6 <= data_in1;
						when others => mapper4_R7 <= data_in1;
					end case;
				when "010" => VRAM_mirror <= '1' & data_in1(0);	-- 0: Vertical, 1: Horizontal
				when "011" => mapper4_RamProtect <= data_in1(7 downto 6);
				when "100" => mapper4_IrqLatch <= data_in1;
				when "101" => mapper4_IrqReload <= '1';
				when "110" => mapper4_IrqEnable <= '0';
									irq_n <= '1';
				when others => mapper4_IrqEnable <= '1';
			end case;
		end if;
	else
		map1_write := '0';		
	end if;
	
	-- IRQ Timer for mapper 4 (MMC3)
	if address2(12) = '0' then
		if map4_cnt /= 2 then	-- Count at least 2 M2 clock with address2(12) at 0
			map4_cnt := map4_cnt + 1;
		end if;
	elsif map4_cnt = 2 and address2(12) = '1' then	-- address2(12) rising edge
		map4_cnt := 0;
		if map4_irq_cnt = 0 then
			if mapper4_IrqEnable = '1' then
				irq_n <= '0';
			end if;
		end if;
		if map4_irq_cnt = 0 or mapper4_IrqReload = '1' then
			map4_irq_cnt := to_integer(unsigned(mapper4_IrqLatch));
			mapper4_IrqReload <= '0';
		else
			map4_irq_cnt := map4_irq_cnt - 1;
		end if;
	else
		map4_cnt := 0;
	end if;
end if;
end process;

--WRAM for cartdrige with save capabilities
	WRAM : RAM_8k
	  PORT MAP (
		 clka => not M2,
		 ena => WRAM_ena,
		 wea(0) => WRAM_wra,
		 addra => address1(12 downto 0),
		 dina => data_in1,
		 douta => WRAM_dout,
		 clkb => not clk,
		 web(0) => WRAM_wrb,
		 addrb => WRAM_addr,
		 dinb => WRAM_dinb,
		 doutb => WRAM_doutb
	  );
	WRAM_wra <= wr1 and (not mapper4_RamProtect(0));	-- Wrtie protection from MMC3

-- 16 bytes RAM for save the last bytes from PRG ROM (the irq, nmi and reset vectors) to identified the game before saving
	Inst_ID_RAM : ID_RAM_16
	  PORT MAP (
		 clka => not clk,
		 ena => ID_RAM_ena,
		 wea(0) => ID_RAM_wr,
		 addra => ID_RAM_addr,
		 dina => ID_RAM_din,
		 douta => ID_RAM_dout
	  );

end Behavioral;
