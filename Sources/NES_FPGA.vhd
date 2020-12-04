
----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    12:42:58 05/19/2020 
-- Design Name: 
-- Module Name:    NES_FPGA - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: NES reproduction for Xilinx Spartan 3E XC3S500E on a WaveShare board
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

entity NES_FPGA is
  port (
    clk_50MHz : in std_logic; 

		--VGA output
		VGA_VSYNC, VGA_HSYNC : out std_logic;
		VGA_RED, VGA_GREEN, VGA_BLUE : out std_logic_vector(2 downto 0);
		
		--Audio output, mixer is analog, 4 --> 0 : p1, p2, noise, tri, dmc
		Audio : out std_logic_vector(4 downto 0);
		
		--Controllers
		Ctrl_D1, Ctrl_D2 : in std_logic;
		Ctrl_Clk1, Ctrl_Clk2, Ctrl_Rst : out std_logic;		

      --PS2 Keyboard
      PS2_CLK : in std_logic;
      PS2_DATA : in std_logic;

		led : out std_logic_vector(3 downto 0);
		BLed : out std_logic_vector(3 downto 0);
		BOUTON_RST : in std_logic;
	 
     -- SPI Flash for cartridge
     spi_clk  : buffer STD_LOGIC;
     spi_cs   : out STD_LOGIC;
     spi_din  : in STD_LOGIC;
     spi_dout : out STD_LOGIC;	
	
		--External_SRAM1 (AS6C4008, 512K x 8)
		SRAM1_add : out std_logic_vector(18 downto 0);
		SRAM1_data : inout std_logic_vector(7 downto 0);
		SRAM1_nCS1 : out std_logic;
		SRAM1_nOE : out std_logic;
		SRAM1_nWE : out std_logic;

		--External_SRAM2 (AS6C4008, 512K x 8)
		SRAM2_add : out std_logic_vector(18 downto 0);
		SRAM2_data : inout std_logic_vector(7 downto 0);
		SRAM2_nCS1 : out std_logic;
		SRAM2_nOE : out std_logic;
		SRAM2_nWE : out std_logic;
		
		--External bus
		Extb : out std_logic_vector(9 downto 0);
		
		--LCD Char SPI (74HC595)
		lcd_clk, lcd_rst, lcd_data : out std_logic;
		
		switch : in std_logic_vector(3 downto 0)

    );
end NES_FPGA;

architecture Behavioral of NES_FPGA is

--RAM boot-loading / Cartdrige emulator
	component INES_reader is
    Port ( clk, clk_50MHz, M2 : in  STD_LOGIC;
           reset : in  STD_LOGIC;
			  test : out std_logic_vector(7 downto 0);
           done : out  STD_LOGIC;
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
			  spi_busy : out STD_LOGIC);
	end component;

	COMPONENT NES_2C02
	PORT(
		clk : IN std_logic;
		rstn : IN std_logic;
		ChipSelect_n : IN std_logic;
		ReadWrite : IN std_logic;
		Address : IN std_logic_vector(2 downto 0);
		Data_in : IN std_logic_vector(7 downto 0);
		CHR_Data_in : IN std_logic_vector(7 downto 0);
		CHR_Data_out   : out std_logic_vector(7 downto 0);		
		Data_out  : OUT std_logic_vector(7 downto 0);
		CHR_Address : OUT std_logic_vector(13 downto 0);
		CHR_wr, CHR_ena : out std_logic;
		VRAM_mirror : in std_logic_vector(1 downto 0);
		VBlank_n : OUT std_logic;
		Mode : in std_logic;
		Edge_cut : in std_logic;
		VGA_VSYNC, VGA_HSYNC : out std_logic;
		color : out std_logic_vector(5 downto 0);
		data_6502_out : in std_logic_vector(7 downto 0);
		ena_ROM_4K_n1, wr_ROM_4K_n1 : in std_logic;
		test_hit : out std_logic
		);
	END COMPONENT;

	COMPONENT APU
	PORT(
		clk : IN std_logic;
		rstn : IN std_logic;
		phi2_ce : IN std_logic;
		ChipSelect : IN std_logic;
		ReadWrite : IN std_logic;
		Address : IN std_logic_vector(4 downto 0);
		Data_in : IN std_logic_vector(7 downto 0);          
		Data_out : OUT std_logic_vector(7 downto 0);
	   dmc_DmaOn : OUT std_logic_vector(2 downto 0);
	   dmc_address : OUT std_logic_vector(15 downto 0);
		irq : OUT std_logic;
		DMA_off : IN std_logic;
		PCM_out : OUT std_logic_vector(9 downto 0); test : out std_logic_vector(7 downto 0)
		);
	END COMPONENT;
	
	COMPONENT r6502_tc
	PORT(
		clk_clk_i : IN std_logic;
		d_i : IN std_logic_vector(7 downto 0);
		irq_n_i : IN std_logic;
		nmi_n_i : IN std_logic;
		rdy_i : IN std_logic;
		rst_rst_n_i : IN std_logic;
		so_n_i : IN std_logic;          
		a_o : OUT std_logic_vector(15 downto 0);
		d_o : OUT std_logic_vector(7 downto 0);
		rd_o : OUT std_logic;
		sync_o : OUT std_logic;
		wr_n_o : OUT std_logic;
		wr_o : OUT std_logic
		);
	END COMPONENT;

	COMPONENT CPU_Ram_2k
	  PORT (
		 clka : IN STD_LOGIC;
		 ena : IN STD_LOGIC;
		 wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
		 addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;

	COMPONENT clock_85MHz
	PORT(
		CLKIN_IN : IN std_logic;          
		CLKFX_OUT : OUT std_logic;
		CLKIN_IBUFG_OUT : OUT std_logic;
		CLK0_OUT : OUT std_logic
		);
	END COMPONENT;

	COMPONENT palette
	PORT(
		clk : IN std_logic;
		vs_tmp : IN std_logic;
		hs_tmp : IN std_logic;
		color : IN std_logic_vector(5 downto 0);          
		vsync : OUT std_logic;
		hsync : OUT std_logic;
		VGA_RED : OUT std_logic_vector(2 downto 0);
		VGA_GREEN : OUT std_logic_vector(2 downto 0);
		VGA_BLUE : OUT std_logic_vector(2 downto 0)
		);
	END COMPONENT;

	COMPONENT LCD_SPI_Ctrl
	PORT(
		clk_50MHz : IN std_logic;
		reset : IN std_logic;
		adresse : IN std_logic_vector(15 downto 0);
		data_1 : IN std_logic_vector(7 downto 0);
		data_2 : IN std_logic_vector(7 downto 0);
		txt_in : IN std_logic_vector(6 downto 0);
		latch_enable : IN std_logic;          
		lcd_clk : OUT std_logic;
		lcd_rst : OUT std_logic;
		lcd_data : OUT std_logic;
		txt_address : OUT std_logic_vector(4 downto 0)
		);
	END COMPONENT;

	COMPONENT ROM_Palette
	  PORT (
		 clka : IN STD_LOGIC;
		 addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(8 DOWNTO 0)
	  );
	END COMPONENT;

	signal clk, clk_50MHz_b, clk_85, clk_tmp, clk_cpu, clk_2cpu : std_logic;
	signal RSTN : std_logic;

	--signaux INES reader
	signal done, rst_init, PRG_ena : std_logic;
	
	--CPU Bus signals
	signal Addr_bus, DMA_addr, CPU_addr : std_logic_vector(15 downto 0);
	signal DMA_dout, DMA_reg, CPU_dout, CPU_din, RAM_dout, PRG_Data : std_logic_vector(7 downto 0);
	signal DMA_off, DMA_CS, we_sync, DMA_wrn, RAM_ena, CPU_wr, CPU_wrn, CPU_nmi, CPU_irq, CPU_rdy : std_logic;
	signal Ctrl1_ena, Ctrl2_ena : std_logic;
	
	--PPU signals
	signal PPU_CS, PPU_rw, VBl_flag, sync, vs_tmp, hs_tmp : std_logic;
	signal PPU_din, PPU_dout, CHR_Data_in, CHR_Data_out : std_logic_vector(7 downto 0);
	signal CHR_Addr : std_logic_vector(13 downto 0);
	signal color : std_logic_vector(5 downto 0);
	signal CHR_ena, CHR_wr : std_logic;
	signal VRAM_mirror : std_logic_vector(1 downto 0);
	
	--APU signals
	signal APU_dout, APU_din : std_logic_vector(7 downto 0);
	signal dmc_address : std_logic_vector(15 downto 0);
	signal PCM_signal : std_logic_vector(9 downto 0);
	signal dmc_DmaOn : std_logic_vector(2 downto 0);
	signal APU_CS : std_logic;
	
	--WRAM signals
	signal WRAM_ena : std_logic;
	signal WRAM_dout : std_logic_vector(7 downto 0);
	
	--Tests
	signal clk_slow : std_logic;
	signal data_test, test : std_logic_vector(7 downto 0);
	signal apu_ena, slow_clk : std_logic;
	signal counter : std_logic_vector(15 downto 0);
	signal test_hit2 : std_logic;
	signal test_hit : std_logic;

	COMPONENT T65
	PORT(
		Mode : IN std_logic_vector(1 downto 0);
		Res_n : IN std_logic;
		Enable : IN std_logic;
		Clk : IN std_logic;
		Rdy : IN std_logic;
		IRQ_n : IN std_logic;
		NMI_n : IN std_logic;
		SO_n : IN std_logic;
		T65DataIn : IN std_logic_vector(7 downto 0);          
		R_W_n : OUT std_logic;
		Sync : OUT std_logic;
		EF : OUT std_logic;
		MF : OUT std_logic;
		XF : OUT std_logic;
		ML_n : OUT std_logic;
		VP_n : OUT std_logic;
		VDA : OUT std_logic;
		VPA : OUT std_logic;
		T65Address : OUT std_logic_vector(15 downto 0);
		T65DataOut : OUT std_logic_vector(7 downto 0)
		);
	END COMPONENT;

	COMPONENT NES_2A03
	PORT(
		clk_cpu : IN std_logic;
		clk_2cpu : IN std_logic;
		rst_n : IN std_logic;
		nmi_n : IN std_logic;
		irq_n : IN std_logic;
		data_in : IN std_logic_vector(7 downto 0);
		ctrl1_data : IN std_logic;
		ctrl2_data : IN std_logic;          
		address : OUT std_logic_vector(15 downto 0);
		data_out : OUT std_logic_vector(7 downto 0);
		audio : OUT std_logic_vector(9 downto 0);
		wr_n : OUT std_logic;
		ctrl_rst : OUT std_logic;
		ctrl1_clk : OUT std_logic;
		ctrl2_clk : OUT std_logic
		);
	END COMPONENT;

begin

	-- Reset
	process(clk_50MHz_b)
	variable count : integer range 0 to 10000 :=0;
	begin
	if clk_50MHz_b'event and clk_50MHz_b='1' then
		if BOUTON_RST = '0' then
			count := 0;
			RSTN <= '0';
			rst_init <= '1';
		elsif done = '0' and count < 8192 then
			RSTN <= '0';
			rst_init <= '1';
			count := count + 1;
		elsif done = '0' then
			rst_init <= '0';
		else
			RSTN <= '1';
		end if;
	end if;
	end process;

	Cartridge : INES_reader Port map (
			  clk => clk,
			  clk_50MHz => clk,
			  M2 => clk_cpu,
           reset => rst_init,
			  test => open,
           done => done,
			  save_error => BLed(1),
			  irq_n => CPU_irq,
			  VRAM_mirror => VRAM_mirror,
			  -- SRAM1 ctrl
           address1 => Addr_bus(14 downto 0),
           data_out1 => PRG_Data,
			  data_in1 => CPU_dout,
			  ena1 => PRG_ena,
			  wr1 => CPU_wr,
			  WRAM_dout => WRAM_dout,
			  WRAM_ena => WRAM_ena,
			  -- SRAM1 port
			  SRAM1_add => SRAM1_add,	-- RAM 128k : uses only 17 bits
--			  SRAM1_add(18 downto 17) => open,
			  SRAM1_data => SRAM1_data,
			  SRAM1_nCS1 => SRAM1_nCS1,
			  SRAM1_nOE => SRAM1_nOE,
			  SRAM1_nWE => SRAM1_nWE,
			  -- SRAM2 ctrl
           address2 => CHR_Addr(12 downto 0),
           data_out2 => CHR_Data_in,
			  data_in2 => CHR_Data_out,
			  ena2 => CHR_ena,
			  wr2 => CHR_wr,
			  -- SRAM2 port
			  SRAM2_add => SRAM2_add,	-- RAM 128k : uses only 17 bits
--			  SRAM2_add(18 downto 17) => open,
			  SRAM2_data => SRAM2_data,
			  SRAM2_nCS1 => SRAM2_nCS1,
			  SRAM2_nOE => SRAM2_nOE,
			  SRAM2_nWE => SRAM2_nWE,
			  -- SPI Flash
			  spi_clk => spi_clk,
			  spi_cs => spi_cs,
			  spi_din => spi_din,
			  spi_dout => spi_dout,
			  spi_busy => BLed(0));

-- Only for using RAM 128k
--SRAM1_add(18 downto 17) <= "11";
--SRAM2_add(18 downto 17) <= "11";

--Global Clock, clk = 21.477272 MHz, cpu_clk = 1.789773 MHz for NTSC NES
	Inst_clock: clock_85MHz PORT MAP(
		CLKIN_IN => clk_50MHz,
		CLKFX_OUT => clk_85,
		CLKIN_IBUFG_OUT => clk_50MHz_b,
		CLK0_OUT => open
	);

----Slow clock
--process(clk_85)
--variable count : integer range 0 to 10000000;
--begin
--if clk_85'event and clk_85='1' then
--	count := count +1;
--	if count > 1000 then
--		count := 0;
--		clk_slow <= not clk_slow;
--	end if;
--end if;
--end process;
--clk_tmp <= clk_85 when (switch(3) = '1' or rst_init = '1') else clk_slow;
clk_tmp <= clk_85;

process(clk_tmp)
variable count : integer range 0 to 24;
begin
if clk_tmp'event and clk_tmp='1' then
	count := count +1;
	if count > 23 then
		count := 0;
		clk_cpu <= not clk_cpu;
		clk_2cpu <= '0';
	end if;
	if count = 12 then
		clk_2cpu <= '1';
	end if;
	if count mod 2 = 0 then
		clk <= not clk;
	end if;
end if;
end process;

--Cpu clock = 1.789773 MHz for NTSC NES

	Inst_NES_2C02: NES_2C02 PORT MAP(
		clk => clk,
		rstn => rstn,
		ChipSelect_n => PPU_CS,
		ReadWrite => PPU_rw,
		Address => Addr_bus(2 downto 0),
		Data_in => PPU_din,
		Data_out => PPU_dout,
		CHR_Address => CHR_Addr,
		CHR_Data_in => CHR_Data_in,
		CHR_Data_out => CHR_Data_out,
		CHR_ena => CHR_ena,
		CHR_wr => CHR_wr,
		VRAM_mirror  => VRAM_mirror,
		VBlank_n => VBl_flag,
		Mode => switch(0),
		Edge_cut => switch(1),
		VGA_VSYNC => vs_tmp,
		VGA_HSYNC => hs_tmp,
		color => color,
		data_6502_out => x"FF",
		ena_ROM_4K_n1 => '0',
		wr_ROM_4K_n1 => '0',
		test_hit => test_hit);

	Inst_Palette : ROM_Palette
	  PORT MAP (
		 clka => clk,
		 addra => color,
		 douta(8 downto 6) => VGA_RED,
		 douta(5 downto 3) => VGA_GREEN,
		 douta(2 downto 0) => VGA_BLUE
	  );

process(clk)
begin
if clk'event and clk = '1' then
	VGA_VSYNC <= vs_tmp;
	VGA_HSYNC <= hs_tmp;
end if;
end process;


	CPU_Ram : CPU_Ram_2k	--Ram 2k is mirrored to 8k
	  PORT MAP (
		 clka => not clk_cpu,
		 ena => RAM_ena,
		 wea(0) => CPU_wr,
		 addra => Addr_bus(10 downto 0),
		 dina => CPU_dout,
		 douta => RAM_dout
	  );

--	Inst_cpu: r6502_tc PORT MAP(
--		clk_clk_i => clk_cpu,
--		d_i => CPU_din,
--		irq_n_i => CPU_irq,
--		nmi_n_i => CPU_nmi,
--		rdy_i => CPU_rdy,
--		rst_rst_n_i => rstn,
--		so_n_i => '1',
--		a_o => CPU_addr,
--		d_o => CPU_dout,
--		rd_o => open,
--		sync_o => open,
--		wr_n_o => CPU_wrn,
--		wr_o => CPU_wr
--	);

--	Inst_NES_2A03: NES_2A03 PORT MAP(
--		clk_cpu => clk_cpu,
--		clk_2cpu => clk_2cpu,
--		rst_n => rstn,
--		nmi_n => CPU_nmi,
--		irq_n => CPU_irq,
--		address => CPU_addr,
--		data_in => CPU_din,
--		data_out => CPU_dout,
--		audio => open,
--		wr_n => CPU_wrn,
--		ctrl_rst => open,
--		ctrl1_clk => open,
--		ctrl2_clk => open,
--		ctrl1_data => '1',
--		ctrl2_data => '1'
--	);

--Inst_T65: T65 PORT MAP(
--		Mode => "00",
--		Res_n => rstn,
--		Enable => '1',
--		Clk => clk_cpu,
--		Rdy => CPU_rdy,
--		IRQ_n => CPU_irq,
--		NMI_n => CPU_nmi,
--		SO_n => '1',
--		R_W_n => CPU_wrn,
--		Sync => open,
--		EF => open,
--		MF => open,
--		XF => open,
--		ML_n => open,
--		VP_n => open,
--		VDA => open,
--		VPA => open,
--		T65Address => CPU_addr,
--		T65DataIn => CPU_din,
--		T65DataOut => CPU_dout
--	);
--CPU_wr <= not CPU_wrn;

--DMA control
Addr_bus <= dmc_address when dmc_DmaOn(2) = '1' else
					CPU_addr when DMA_off = '1' else DMA_addr;
PPU_din <= CPU_dout when DMA_off = '1' else DMA_dout;
PPU_rw <= (not CPU_wr) when DMA_off = '1' else (DMA_wrn);
CPU_rdy <= DMA_off and (not dmc_DmaOn(0));

--Address decoder
RAM_ena <= '1' when Addr_bus(15 downto 13) = "000" else '0';	--$0000 to $1FFF
PPU_CS <= '0' when Addr_bus(15 downto 13) = "001" else '1';		--$2000 to $3FFF
DMA_CS <= '1' when Addr_bus(15 downto 0) = x"4014" else '0';	--$4014
APU_CS <= '1' when Addr_bus(15 downto 5) = "01000000000" else '0';	--$4000 to $401F
Ctrl1_ena <= '1' when Addr_bus = x"4016" else '0';					--$4016
Ctrl2_ena <= '1' when Addr_bus = x"4017" else '0';					--$4017
WRAM_ena <= '1' when Addr_bus(15 downto 13) = "011" else '0';	--$6000 to $7FFF
PRG_ena <= Addr_bus(15);													--$8000 to $FFFF

CPU_din <= 	CPU_dout when CPU_wrn = '0' else
				RAM_dout when RAM_ena = '1' else
				PPU_dout when PPU_CS = '0' else
				WRAM_dout when WRAM_ena = '1' else
				Prg_data when PRG_ena = '1' else
				("0000000" & not Ctrl_D1) when Ctrl1_ena = '1' else
				("0000000" & not Ctrl_D2) when Ctrl2_ena = '1' else
				(others => '-');
APU_din <= CPU_din when dmc_DmaOn(2) = '1' else CPU_dout;

--Others CPU logic
--we_sync <= CPU_wr and (not clk_cpu) and (not clk_2cpu) and DMA_off;	
CPU_nmi <= Vbl_Flag;

--DMA
process(clk_cpu)
variable count : integer range 0 to 256;
variable step, DMA_ready : std_logic;
begin
if rstn = '0' then
	DMA_off <= '1';
	DMA_addr <= (others => '-');
	DMA_wrn <= '1';
	DMA_ready := '0';
elsif clk_cpu'event and clk_cpu = '1' then
	--DMA transfer
	if DMA_off = '0' then
		--Read step
		if step = '0' or dmc_DmaOn(1) = '1' then	-- DMC DMA has priority, keep this state till DMC has fetched his byte
			DMA_addr <= DMA_reg & std_logic_vector(to_unsigned(count,8));
			DMA_wrn <= '1'; 		--Read mode
			step := '1';
			if count = 256 then
				DMA_off <= '1';			--End of transfer
			end if;
		else
		--Write step
			DMA_addr <= x"2004";	--Address of PPU OAM data port
			DMA_dout <= CPU_din;	--Transfers data from memory to OAM reg
			DMA_wrn <= '0';		--Write active
			step := '0';
			count := count + 1;
		end if;
	--Wrinting on DMA register, and starts transfer
	elsif (DMA_CS = '1' and CPU_wr = '1') then
		DMA_reg <= CPU_dout;
		DMA_wrn <= '1';
		DMA_addr <= x"4014";
		count := 0;
		step := '0';
		DMA_ready := '1';
--	elsif DMA_ready = '1' then
		DMA_off <= '0';
		DMA_ready := '0';
	else
		DMA_wrn <= '1';
	end if;
end if;
end process;

--Controllers command
process(clk_2cpu)	--Ctrl_Rst reset both controllers
begin
if clk_2cpu'event and clk_2cpu='1' then
	if Ctrl1_ena = '1' then
		if CPU_wr = '1' then
			Ctrl_Rst <= CPU_dout(0);
		else
			Ctrl_Clk1 <= '1';
		end if;
	else
		Ctrl_Clk1 <= '0';
	end if;
	if Ctrl2_ena = '1' and CPU_wr = '0' then
		Ctrl_Clk2 <= '1';
	else
		Ctrl_Clk2 <= '0';
	end if;
end if;
end process;

	
	Inst_APU: APU PORT MAP(
		clk => clk_2cpu,
		rstn => rstn,
		phi2_ce => clk_cpu,
		ChipSelect => APU_CS,
		ReadWrite => CPU_wr,
		Address => Addr_bus(4 downto 0),
		Data_out => APU_dout,
		Data_in => APU_din,
		dmc_DmaOn => dmc_DmaOn,
		dmc_address => dmc_address,
		irq => open, --CPU_irq,
		DMA_off => DMA_off,
		PCM_out => PCM_signal, test => test
	);

--Audio delta-sigma conversion
process(clk)
	variable PWM_accumulator : std_logic_vector(10 downto 0);
begin
if clk'event and clk='1' then
	PWM_accumulator := std_logic_vector(unsigned('0' & PWM_accumulator(9 downto 0)) + unsigned('0' & PCM_signal));
end if;
	Audio <= (others => PWM_accumulator(10));
end process;
--Audio <= '0';

--	Inst_LCD_test: LCD_SPI_Ctrl PORT MAP(
--		clk_50MHz => clk_50MHz_b,
--		reset => rst_init,
--		lcd_clk => lcd_clk,
--		lcd_rst => lcd_rst,
--		lcd_data => lcd_data,
--		adresse => x"FFFF",
----		adresse(15) => '0',
--		data_1 => test,
--		data_2 => x"FF",
--		txt_in => "0000000",
--		txt_address => open,
--		latch_enable => '1'
--	);

--apu_ena <= '1' when Addr_bus(15 downto 4)= "010000000000" else '0';
--Extb <= apu_ena & clk_cpu & CPU_wrn & Addr_bus(3 downto 0) & "000";
--led <= (not test(1 downto 0)) & done & CPU_irq;
led <= "1111";
--Extb <= Vbl_Flag & CHR_Addr(12) & CPU_irq & test(2 downto 0) & (Vbl_Flag and test_hit) & "---";
Extb <= (others => '0');
--test_hit2 <= PRG_ena and we_sync;
--Extb <= test & "00";
BLed(3 downto 2) <= (others => '0');

--process(clk)
--variable count : integer;
--variable c2 : std_logic_vector(17 downto 0);
--variable cnt_on : std_logic;
--begin
--if clk'event and clk='1' then
--	if Vbl_Flag='0' then
--		count :=0;
--		counter <= c2(17 downto 2);
--	elsif test_hit='0' then
--		count := count + 1;
--	else
--		c2 := std_logic_vector(to_unsigned(count,18));
--	end if;
--end if;
--end process;


end Behavioral;
