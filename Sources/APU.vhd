----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:38:01 07/31/2020 
-- Design Name: 
-- Module Name:    APU - Behavioral 
-- Project Name: 
-- Target Devices: Spartan 3E
-- Tool versions: ISE
-- Description: Audio Processor Unit for NES, output PCM 10 bits
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
use IEEE.NUMERIC_STD.ALL;


entity APU is
    Port ( clk : in  STD_LOGIC;
           rstn : in  STD_LOGIC;
           phi2_ce : in  STD_LOGIC;
			  ChipSelect : in  STD_LOGIC;
           ReadWrite : in  STD_LOGIC;
           Address : in  STD_LOGIC_VECTOR (4 downto 0);
           Data_out : out  STD_LOGIC_VECTOR (7 downto 0);
           Data_in : in  STD_LOGIC_VECTOR (7 downto 0);
			  dmc_DmaOn : out STD_LOGIC_VECTOR (2 downto 0);	-- 0: CPU / DMA wait, 1 Data Fetch
			  dmc_address : out STD_LOGIC_VECTOR (15 downto 0);
           irq : out  STD_LOGIC;
			  DMA_off : in STD_LOGIC;
           PCM_out : out  STD_LOGIC_VECTOR (9 downto 0);
			  test : out std_logic_vector(7 downto 0)
			  );
end APU;

architecture Behavioral of APU is

	COMPONENT tnd_table
	  PORT (
		 clka : IN STD_LOGIC;
		 addra : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(9 DOWNTO 0)
	  );
	END COMPONENT;

	COMPONENT pulse_table
	  PORT (
		 clka : IN STD_LOGIC;
		 addra : IN STD_LOGIC_VECTOR(4 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	  );
	END COMPONENT;

	--signals
	signal APU_CE : std_logic;
	signal HalfFrame_CE : std_logic;
	signal QuarterFrame_CE : std_logic;
	signal frameCounter_timer : integer range 0 to 32767;
	signal PCM_pulse1 : integer range 0 to 15;
	signal PCM_pulse2 : integer range 0 to 15;
	signal PCM_noise : integer range 0 to 15;
	signal PCM_triangle : integer range 0 to 45;
	signal triangle_linearCounter : integer range 0 to 127;
	signal triangle_linearCounterLoad : integer range 0 to 127;
	signal triangle_linearCounterFlag : std_logic;
	signal triangle_TimerLoad : std_logic_vector(10 downto 0);
	signal triangle_timer : integer range 0 to 2047;
	signal triangle_LengthCounterHalt : std_logic;
	signal triangle_sequencer : integer range 0 to 31;
	signal triangle_LengthCounterLoadFlag : std_logic;
	signal triangle_LengthCounterLoad : integer range 0 to 255;
	signal triangle_lengthCounter_LengthCounter : integer range 0 to 255;
	signal triangle_LengthCounterGate : std_logic;
	signal noise_EnvelopeDecay : integer range 0 to 15;
	signal noise_lfsr : std_logic_vector(14 downto 0);
	signal noise_EnvelopeVolume : integer range 0 to 15;
	signal noise_TimerLoad : std_logic_vector(3 downto 0);
	signal noise_LengthCounterHalt : std_logic;
	signal noise_EnvelopeStartFlag : std_logic;
	signal noise_LFSRMode : std_logic;
	signal noise_timer : integer range 0 to 4095;
	signal noise_LengthCounterLoad : integer range 0 to 255;
	signal noise_LengthCounterLoadFlag : std_logic;
	signal noise_EnvelopeConstantFlag : std_logic;
	signal noise_envelope_volume : integer range 0 to 15;
	signal noise_envelope_divider : integer range 0 to 15;
	signal noise_lengthCounter_LengthCounter : integer range 0 to 255;
	signal noise_LengthCounterGate : std_logic;
	signal noise_envelope_LoopFlag : std_logic; 
	signal pulse2_envelope_LoopFlag : std_logic;
	signal pulse2_EnvelopeDecay : integer range 0 to 15;
	signal pulse2_sequencer : std_logic_vector(7 downto 0);
	signal pulse2_EnvelopeVolume : integer range 0 to 15;
	signal pulse2_TimerLoad : std_logic_vector(10 downto 0);
	signal pulse2_EnvelopeStartFlag : std_logic;
	signal pulse2_timer : integer range 0 to 2047;
	signal pulse2_LengthCounterLoad : integer range 0 to 255;
	signal pulse2_LengthCounterLoadFlag : std_logic;
	signal pulse2_EnvelopeConstantFlag : std_logic;
	signal pulse2_envelope_volume : integer range 0 to 15;
	signal pulse2_envelope_divider : integer range 0 to 15;
	signal pulse2_lengthCounter_LengthCounter : integer range 0 to 255;
	signal pulse2_LengthCounterGate : std_logic;
	signal pulse2_LengthCounterHalt : std_logic;
	signal pulse2_ShiftCount : integer range 0 to 7;
	signal pulse2_NegateFlag : std_logic;
	signal pulse2_DividerPeriod : integer range 0 to 7;
	signal pulse2_SweepEnable : std_logic;
	signal pulse1_EnvelopeDecay : integer range 0 to 15;
	signal pulse1_sequencer : std_logic_vector(7 downto 0);
	signal pulse1_EnvelopeVolume : integer range 0 to 15;
	signal pulse1_TimerLoad : std_logic_vector(10 downto 0);
	signal pulse1_EnvelopeStartFlag : std_logic;
	signal pulse1_timer : integer range 0 to 2047;
	signal pulse1_LengthCounterLoad : integer range 0 to 255;
	signal pulse1_LengthCounterLoadFlag : std_logic;
	signal pulse1_EnvelopeConstantFlag : std_logic;
	signal pulse1_envelope_volume : integer range 0 to 15;
	signal pulse1_envelope_divider : integer range 0 to 15;
	signal pulse1_lengthCounter_LengthCounter : integer range 0 to 255;
	signal pulse1_LengthCounterHalt : std_logic;
	signal pulse1_envelope_LoopFlag : std_logic;
	signal pulse1_LengthCounterGate : std_logic;
	signal pulse1_ShiftCount : integer range 0 to 7;
	signal pulse1_NegateFlag : std_logic;
	signal pulse1_DividerPeriod : integer range 0 to 7;
	signal pulse1_SweepEnable : std_logic;
	signal frameCounter_InterruptInhibit : std_logic;
	signal frameCounter_Mode : std_logic;	
	signal dmc_IrqEnableFlag : std_logic;
	signal dmc_LoopFlag : std_logic;
	signal dmc_RateIndex : std_logic_vector(3 downto 0);
	signal dmc_SampleAddress : std_logic_vector(7 downto 0);
	signal dmc_SampleLength : std_logic_vector(7 downto 0);
	signal dmc_SampleBuffer : std_logic_vector(7 downto 0);
	signal dmc_out : integer range 0 to 127;
	signal dmc_SampleRemain : integer range 0 to 4095;
	signal dmc_SampleGate : std_logic;
	signal dmc_SilenceBuffer : std_logic;
	signal pulse_table_in : std_logic_vector(4 downto 0);
	signal tnd_table_in : std_logic_vector(7 downto 0);
	signal pulse_table_out : std_logic_vector(7 downto 0);
	signal tnd_table_out : std_logic_vector(9 downto 0);

begin


--Assignments
noise_envelope_LoopFlag <= '0';
pulse2_envelope_LoopFlag <= '0';
pulse1_envelope_LoopFlag <= '0';
--PCM_out <= std_logic_vector(to_unsigned((4* (PCM_pulse1 + PCM_pulse2) + 2* PCM_noise + 4* PCM_triangle + 2* dmc_out),10));
noise_EnvelopeVolume <= noise_EnvelopeDecay when noise_EnvelopeConstantFlag = '1'
								else noise_envelope_volume;
noise_LengthCounterGate <= '1' when noise_lengthCounter_LengthCounter > 0 else '0';
triangle_LengthCounterGate <= '1' when triangle_lengthCounter_LengthCounter > 0 else '0';
pulse2_LengthCounterGate <= '1' when pulse2_lengthCounter_LengthCounter > 0 else '0';
pulse2_EnvelopeVolume <= pulse2_EnvelopeDecay when pulse2_EnvelopeConstantFlag = '1'
				else pulse2_envelope_volume;
pulse1_LengthCounterGate <= '1' when pulse1_lengthCounter_LengthCounter > 0 else '0';
pulse1_EnvelopeVolume <= pulse1_EnvelopeDecay when pulse1_EnvelopeConstantFlag = '1'
				else pulse1_envelope_volume;
dmc_SampleGate <= '1' when dmc_SampleRemain > 0 else '0';


process(clk)
variable APU_CE_cnt : std_logic_vector(3 downto 0) := "0001";
begin
if clk'event and clk='1' then
	APU_CE_cnt := APU_CE_cnt(2 downto 0) & APU_CE_cnt(3);
end if;
APU_CE <= APU_CE_cnt(0);
end process;


process(clk)
	variable set_interrupt, interrupt_event : std_logic;
	variable dmc_enable, noise_enable, triangle_enable, pulse2_enable, pulse1_enable : std_logic := '0';
	variable dmc_TotalCount, dmc_count : integer range 0 to 511;
	variable dmc_shift : integer range 0 to 8;
	variable dmc_SampleShift : std_logic_vector(7 downto 0);
	variable dmc_address_tmp : std_logic_vector(14 downto 0);
	variable dmc_SampleEmpty, dmc_SampleSilence : std_logic;
	variable dmc_interrupt : std_logic;
	variable dmc_state : integer range 0 to 5;
	variable pulse2_SweepCount : integer range 0 to 8;
	variable pulse2_SrlCount : integer range 0 to 7;
	variable pulse2_ChangeAmount : std_logic_vector(10 downto 0);
	variable pulse1_SweepCount : integer range 0 to 8;
	variable pulse1_SrlCount : integer range 0 to 7;
	variable pulse1_ChangeAmount : std_logic_vector(10 downto 0);
	variable fb_bit : std_logic;
	variable pulse1_count, pulse2_count : integer range 0 to 7;
	variable lut_lengthCounter : integer range 0 to 255;
	variable lut_DutyCycle : std_logic_vector(7 downto 0);
begin
if clk'event and clk='1' then

--Framecounter
	QuarterFrame_CE <= '0';
	HalfFrame_CE <= '0';
	interrupt_event := '0';
	if APU_CE = '1' then
		frameCounter_timer <= frameCounter_timer + 1;
		if frameCounter_timer = 3728 then
			QuarterFrame_CE <= '1';
		elsif frameCounter_timer = 7456 then
			HalfFrame_CE <= '1';
			QuarterFrame_CE <= '1';
		elsif frameCounter_timer = 11186 then
			QuarterFrame_CE <= '1';
		elsif frameCounter_timer = 14914 and frameCounter_Mode = '0' then
			HalfFrame_CE <= '1';
			QuarterFrame_CE <= '1';
			frameCounter_timer <= 0;
			if frameCounter_InterruptInhibit = '0' then
				set_interrupt := '1';
				interrupt_event := '1';
			end if;
		elsif frameCounter_timer >= 18640 then
			HalfFrame_CE <= '1';
			QuarterFrame_CE <= '1';
			frameCounter_timer <= 0;			
		end if;
	end if;

--Noise generator
	if rstn = '0' then
		noise_lfsr <= (others => '1');
		noise_LengthCounterLoadFlag <= '0';
		noise_enable := '0';
	else
		if APU_CE = '1' then
			if noise_timer = 0 then
				if noise_LFSRMode = '1' then
					fb_bit := noise_lfsr(6);
				else
					fb_bit := noise_lfsr(1);
				end if;
				noise_lfsr <= (fb_bit xor noise_lfsr(0)) & noise_lfsr(14 downto 1);
				if noise_lfsr(0) = '0' and noise_LengthCounterGate = '1' and noise_enable = '1' then
					PCM_noise <= noise_EnvelopeVolume;
				else
					PCM_noise <= 0;
				end if;
				case noise_TimerLoad is
					when x"0" => noise_timer <= 4;
					when x"1" => noise_timer <= 8;
					when x"2" => noise_timer <= 16;
					when x"3" => noise_timer <= 32;
					when x"4" => noise_timer <= 64;
					when x"5" => noise_timer <= 96;
					when x"6" => noise_timer <= 128;
					when x"7" => noise_timer <= 160;
					when x"8" => noise_timer <= 202;
					when x"9" => noise_timer <= 254;
					when x"A" => noise_timer <= 380;
					when x"B" => noise_timer <= 508;
					when x"C" => noise_timer <= 762;
					when x"D" => noise_timer <= 1016;
					when x"E" => noise_timer <= 2034;
					when others => noise_timer <= 4068;
				end case;
			else
				noise_timer <= noise_timer - 1;
			end if;
		end if;
	end if;

--Noise envelope
	if QuarterFrame_CE = '1' then
		if noise_EnvelopeStartFlag = '1' then
			noise_EnvelopeStartFlag <= '0';
			noise_envelope_volume <= 15;
			noise_envelope_divider <= noise_EnvelopeDecay;
		else
			if noise_envelope_divider = 0 then
				noise_envelope_divider <= noise_EnvelopeDecay;
				if noise_envelope_volume > 0 then
					noise_envelope_volume <= noise_envelope_volume -1 ;
				else
					if noise_envelope_LoopFlag = '1' then
						noise_envelope_volume <= 15;
					end if;
				end if;
			else
				noise_envelope_divider <= noise_envelope_divider - 1;
			end if;
		end if;
	end if;

--Noise lenghcounter
	if HalfFrame_CE = '1' then
		if noise_lengthCounter_LengthCounter > 0 and noise_LengthCounterHalt = '0' then
			noise_lengthCounter_LengthCounter <= noise_lengthCounter_LengthCounter - 1;
		end if;
	end if;
	if noise_LengthCounterLoadFlag = '1' then
		noise_LengthCounterLoadFlag <= '0';
		noise_lengthCounter_LengthCounter <= noise_LengthCounterLoad;
	end if;

--Triangle
	if rstn = '0' then
		triangle_enable := '0';
	elsif QuarterFrame_CE = '1' then
		if triangle_linearCounterFlag = '1' then
			triangle_linearCounter <= triangle_linearCounterLoad;
		elsif triangle_linearCounter > 0 then
			triangle_linearCounter <= triangle_linearCounter - 1;
		end if;
		if triangle_LengthCounterHalt = '0' then
			triangle_linearCounterFlag <= '0';
		end if;
	end if;

--	if APU_CE = '1' then
	if phi2_ce = '1' then
		if triangle_timer = 0 then
			if (triangle_LengthCounterGate = '1' and triangle_linearCounter > 0 and triangle_enable = '1') or triangle_sequencer /= 0 then
				triangle_sequencer <= triangle_sequencer + 1;
				case triangle_sequencer is
					when 0 | 31 => PCM_triangle <= 3*0;
					when 1 | 30 => PCM_triangle <= 3*1;
					when 2 | 29 => PCM_triangle <= 3*2;
					when 3 | 28 => PCM_triangle <= 3*3;
					when 4 | 27 => PCM_triangle <= 3*4;
					when 5 | 26 => PCM_triangle <= 3*5;
					when 6 | 25 => PCM_triangle <= 3*6;
					when 7 | 24 => PCM_triangle <= 3*7;
					when 8 | 23 => PCM_triangle <= 3*8;
					when 9 | 22 => PCM_triangle <= 3*9;
					when 10 | 21 => PCM_triangle <= 3*10;
					when 11 | 20 => PCM_triangle <= 3*11;
					when 12 | 19 => PCM_triangle <= 3*12;
					when 13 | 18 => PCM_triangle <= 3*13;
					when 14 | 17 => PCM_triangle <= 3*14;
					when 15 | 16 => PCM_triangle <= 3*15;
				end case;
			else
				PCM_triangle <= 0;
			end if;
			triangle_timer <= to_integer(unsigned(triangle_TimerLoad));
		else
			triangle_timer <= triangle_timer - 1;
		end if;
	end if;

--Triangle lenghcounter
	if HalfFrame_CE = '1' then
		if triangle_lengthCounter_LengthCounter > 0 and triangle_LengthCounterHalt = '0' then
			triangle_lengthCounter_LengthCounter <= triangle_lengthCounter_LengthCounter - 1;
		end if;
	end if;
	if triangle_LengthCounterLoadFlag = '1' then
		triangle_LengthCounterLoadFlag <= '0';
		triangle_lengthCounter_LengthCounter <= triangle_LengthCounterLoad;
	end if;

--Pulse2
	if rstn = '0' then
		pulse2_sequencer <= x"0F";
		pulse2_enable := '0';
	else
		if APU_CE = '1' then
			if pulse2_timer = 0 then
				pulse2_count := (pulse2_count + 1) mod 8;
				if pulse2_sequencer(pulse2_count) = '1' and pulse2_enable = '1' then
					PCM_pulse2 <= pulse2_EnvelopeVolume;
				else
					PCM_pulse2 <= 0;
				end if;
				if pulse2_LengthCounterGate = '0' then
					PCM_pulse2 <= 0;
				end if;
				pulse2_timer <= to_integer(unsigned(pulse2_TimerLoad));
			else
				pulse2_timer <= pulse2_timer - 1;
			end if;
		end if;
	end if;

--Pulse2 envelope
	if QuarterFrame_CE = '1' then
		if pulse2_EnvelopeStartFlag = '1' then
			pulse2_EnvelopeStartFlag <= '0';
			pulse2_envelope_volume <= 15;
			pulse2_envelope_divider <= pulse2_EnvelopeDecay;
		else
			if pulse2_envelope_divider = 0 then
				pulse2_envelope_divider <= pulse2_EnvelopeDecay;
				if pulse2_envelope_volume > 0 then
					pulse2_envelope_volume <= pulse2_envelope_volume - 1;
				elsif pulse2_envelope_LoopFlag = '1' then
					pulse2_envelope_volume <= 15;
				end if;
			else
				pulse2_envelope_divider <= pulse2_envelope_divider - 1;
			end if;
		end if;
	end if;

--Pulse2 lenghcounter
	if HalfFrame_CE = '1' then
		if pulse2_lengthCounter_LengthCounter > 0 and pulse2_LengthCounterHalt = '0' then
			pulse2_lengthCounter_LengthCounter <= pulse2_lengthCounter_LengthCounter - 1;
		end if;
	end if;
	if pulse2_LengthCounterLoadFlag = '1' then
		pulse2_LengthCounterLoadFlag <= '0';
		pulse2_lengthCounter_LengthCounter <= pulse2_LengthCounterLoad;
	end if;

	--Pulse2 Sweep
	if pulse2_SweepEnable = '1' then
		if pulse2_SweepCount > pulse2_DividerPeriod then
			pulse2_ChangeAmount := '0' & pulse2_ChangeAmount(10 downto 1);
			if pulse2_SrlCount mod 8 = pulse2_ShiftCount then
				if pulse2_NegateFlag = '1' then
					pulse2_TimerLoad <= std_logic_vector(unsigned(pulse2_TimerLoad) - unsigned(pulse2_ChangeAmount));
				else
					pulse2_TimerLoad <= std_logic_vector(unsigned(pulse2_TimerLoad) + unsigned(pulse2_ChangeAmount));
				end if;
				pulse2_SweepCount := 0;
			else
				pulse2_SrlCount := pulse2_SrlCount + 1;
			end if;
		elsif HalfFrame_CE = '1' then	--Increase each half frame
			pulse2_SweepCount := pulse2_SweepCount + 1;
			pulse2_SrlCount := 1;	-- Start to 1 because when pulse2_ShiftCount = 0 means 8 shifts
			pulse2_ChangeAmount := pulse2_TimerLoad;
		end if;
	else
		pulse2_SweepCount := 0;
	end if;

--Pulse1
	if rstn = '0' then
		pulse1_sequencer <= x"0F";
		pulse1_enable := '0';
	else
		if APU_CE = '1' then
			if pulse1_timer = 0 then
				pulse1_count := (pulse1_count + 1) mod 8;
				if pulse1_sequencer(pulse1_count) = '1' and pulse1_enable = '1' then
					PCM_pulse1 <= pulse1_EnvelopeVolume;
				else
					PCM_pulse1 <= 0;
				end if;
				if pulse1_LengthCounterGate = '0' then
					PCM_pulse1 <= 0;
				end if;
				pulse1_timer <= to_integer(unsigned(pulse1_TimerLoad));
			else
				pulse1_timer <= pulse1_timer - 1;
			end if;
		end if;
	end if;

--Pulse1 envelope
	if QuarterFrame_CE = '1' then
		if pulse1_EnvelopeStartFlag = '1' then
			pulse1_EnvelopeStartFlag <= '0';
			pulse1_envelope_volume <= 15;
			pulse1_envelope_divider <= pulse1_EnvelopeDecay;
		else
			if pulse1_envelope_divider = 0 then
				pulse1_envelope_divider <= pulse1_EnvelopeDecay;
				if pulse1_envelope_volume > 0 then
					pulse1_envelope_volume <= pulse1_envelope_volume - 1;
				elsif pulse1_envelope_LoopFlag = '1' then
					pulse1_envelope_volume <= 15;
				end if;
			else
				pulse1_envelope_divider <= pulse1_envelope_divider - 1;
			end if;
		end if;
	end if;

--Pulse1 lenghcounter
	if HalfFrame_CE = '1' then
		if pulse1_lengthCounter_LengthCounter > 0 and pulse1_LengthCounterHalt = '0' then
			pulse1_lengthCounter_LengthCounter <= pulse1_lengthCounter_LengthCounter - 1;
		end if;
	end if;
	if pulse1_LengthCounterLoadFlag = '1' then
		pulse1_LengthCounterLoadFlag <= '0';
		pulse1_lengthCounter_LengthCounter <= pulse1_LengthCounterLoad;	
	end if;

	--Pulse1 Sweep
	if pulse1_SweepEnable = '1' then
		if pulse1_SweepCount > pulse1_DividerPeriod then
			pulse1_ChangeAmount := '0' & pulse1_ChangeAmount(10 downto 1);			
			if pulse1_SrlCount mod 8 = pulse1_ShiftCount then
				if pulse1_NegateFlag = '1' then
					pulse1_TimerLoad <= std_logic_vector(unsigned(pulse1_TimerLoad) + unsigned(not pulse1_ChangeAmount));	-- 1' complement
				else
					pulse1_TimerLoad <= std_logic_vector(unsigned(pulse1_TimerLoad) + unsigned(pulse1_ChangeAmount));
				end if;
				pulse1_SweepCount := 0;
			else
				pulse1_SrlCount := pulse1_SrlCount + 1;
			end if;
		elsif HalfFrame_CE = '1' then	--Increase each half frame
			pulse1_SweepCount := pulse1_SweepCount + 1;
			pulse1_SrlCount := 1;
			pulse1_ChangeAmount := pulse1_TimerLoad;
		end if;
	else
		pulse1_SweepCount := 0;
	end if;


--DMC timer
--	 DMC Rate LUT (values for NTSC NES)
	case dmc_RateIndex is 
		when x"0" => dmc_TotalCount := 428/2;
		when x"1" => dmc_TotalCount := 380/2;
		when x"2" => dmc_TotalCount := 340/2;
		when x"3" => dmc_TotalCount := 320/2;
		when x"4" => dmc_TotalCount := 286/2;
		when x"5" => dmc_TotalCount := 254/2;
		when x"6" => dmc_TotalCount := 226/2;
		when x"7" => dmc_TotalCount := 214/2;
		when x"8" => dmc_TotalCount := 190/2;
		when x"9" => dmc_TotalCount := 160/2;
		when x"A" => dmc_TotalCount := 142/2;
		when x"B" => dmc_TotalCount := 128/2;
		when x"C" => dmc_TotalCount := 106/2;
		when x"D" => dmc_TotalCount := 84/2;
		when x"E" => dmc_TotalCount := 72/2;
		when others => dmc_TotalCount := 54/2;
	end case;

--	DMC output process
	if APU_CE = '1' then
		dmc_count := dmc_count + 1;
		-- Output data at the data rate
		if dmc_count >= dmc_TotalCount then
			dmc_count := 0;
			-- Output delta-sigma generator
			if dmc_SampleSilence = '0' then
				if dmc_SampleShift(0) = '0' then
					if dmc_out > 1 then
						dmc_out <= dmc_out - 2;
					end if;
				else
					if dmc_out < 126 then
						dmc_out <= dmc_out + 2;
					end if;			
				end if;
			end if;
			-- When outputs 8 bits a new byte should be fetched
			if dmc_shift = 7 then
				dmc_shift := 0;
				dmc_SampleShift := dmc_SampleBuffer;
				dmc_SampleEmpty := '1';
				dmc_SampleSilence := dmc_SilenceBuffer or (not dmc_enable);
				dmc_SilenceBuffer <= '1';	-- No more sample data
			else
				dmc_SampleShift := '-' & dmc_SampleShift(7 downto 1);
				dmc_shift := dmc_shift + 1;
			end if;
		end if;
	end if;
	
	-- DMC DMA
	if rstn = '0' then
		dmc_enable := '0';
		dmc_state := 0;
	elsif phi2_ce = '1' then
		if dmc_state = 0 and dmc_SampleEmpty = '1' and (dmc_LoopFlag = '1' or dmc_SampleGate = '1') then
			-- Prepare timing depending CPU and DMA cycles
			if DMA_off = '0' then
				dmc_state := 3;	-- DMA is on, no need to wait for RDY CPU
			elsif ReadWrite = '1' then
				dmc_state := 2;	-- CPU is writing, no need to wait 3 RDY cycles
			else
				dmc_state := 1;
			end if;
		end if;
	-- FSM for DMC DMA
		if dmc_state > 0 then
			case dmc_state is
				when 1 =>
					dmc_state := 2;
					dmc_DmaOn(1 downto 0) <= "11";	-- Ask CPU / DMA to stop
				when 2 =>
					dmc_state := 3;
					dmc_DmaOn(1 downto 0) <= "11";	-- Ask CPU / DMA to stop
				when 3 =>
					if ReadWrite = '0' then	-- if CPU is writing, wait another cycle
						dmc_state := 4;
						dmc_DmaOn(1 downto 0) <= "11";	-- Ask CPU / DMA to stop
						dmc_DmaOn(2) <= '1';	-- DMC take control of busses
					end if;
				when 4 =>
				dmc_SampleBuffer <= Data_in(7 downto 0);
				dmc_SampleEmpty := '0';
				dmc_DmaOn <= (others => '0');
				dmc_state := 0;
				dmc_SilenceBuffer <= '0';
				-- Prepare the next sample fetch
				if dmc_SampleGate = '1' then
					dmc_SampleRemain <= dmc_SampleRemain - 1;
					dmc_address_tmp := std_logic_vector(to_unsigned(to_integer(unsigned(dmc_address_tmp)) + 1, 15));
				elsif dmc_LoopFlag = '1' then
					-- Restart the Sample Loop
					dmc_address_tmp := '1' & dmc_SampleAddress & "000000";
					dmc_SampleRemain <= to_integer(unsigned(dmc_SampleLength & "0001"));
				end if;
				when others =>
					dmc_DmaOn <= (others => '0');
			end case;
		end if;
	end if;

	-- Do reads and writes on phi2_ce = 0
	if ChipSelect = '1' and ReadWrite = '1' and phi2_ce = '0' then
		
		-- LUT for the length counter and DutyCycle
		case Address is
			when "00011"|"00111"|"01011"|"01111" =>
				case Data_in(7 downto 3) is
					when "00000" => lut_lengthCounter := 10;
					when "00001" => lut_lengthCounter := 254;
					when "00010" => lut_lengthCounter := 20;
					when "00011" => lut_lengthCounter := 2;
					when "00100" => lut_lengthCounter := 40;
					when "00101" => lut_lengthCounter := 4;
					when "00110" => lut_lengthCounter := 80;
					when "00111" => lut_lengthCounter := 6;
					when "01000" => lut_lengthCounter := 160;
					when "01001" => lut_lengthCounter := 8;
					when "01010" => lut_lengthCounter := 60;
					when "01011" => lut_lengthCounter := 10;
					when "01100" => lut_lengthCounter := 14;
					when "01101" => lut_lengthCounter := 12;
					when "01110" => lut_lengthCounter := 26;
					when "01111" => lut_lengthCounter := 14;
					when "10000" => lut_lengthCounter := 12;
					when "10001" => lut_lengthCounter := 16;
					when "10010" => lut_lengthCounter := 24;
					when "10011" => lut_lengthCounter := 18;
					when "10100" => lut_lengthCounter := 48;
					when "10101" => lut_lengthCounter := 20;
					when "10110" => lut_lengthCounter := 96;
					when "10111" => lut_lengthCounter := 22;
					when "11000" => lut_lengthCounter := 192;
					when "11001" => lut_lengthCounter := 24;
					when "11010" => lut_lengthCounter := 72;
					when "11011" => lut_lengthCounter := 26;
					when "11100" => lut_lengthCounter := 16;
					when "11101" => lut_lengthCounter := 28;
					when "11110" => lut_lengthCounter := 32;
					when others => lut_lengthCounter := 30;
				end case;
			when "00000"|"00100" =>
				case Data_in(7 downto 6) is
					when "00" => lut_DutyCycle := "01000000";
					when "01" => lut_DutyCycle := "01100000";
					when "10" => lut_DutyCycle := "01111000";
					when others => lut_DutyCycle := "10011111";
				end case;
			when others =>
		end case;
		-- Select register to write
		case Address is
			when "00000" =>	--$4000
				pulse1_sequencer <= lut_DutyCycle;
				pulse1_LengthCounterHalt <= Data_in(5);
				pulse1_EnvelopeConstantFlag <= Data_in(4);
				pulse1_EnvelopeDecay <= to_integer(unsigned(Data_in(3 downto 0)));
				pulse1_LengthCounterLoadFlag <= '1';
			when "00001" =>	--$4001
test <= Data_in;
				pulse1_SweepEnable <= Data_in(7);
				pulse1_DividerPeriod <= to_integer(unsigned(Data_in(6 downto 4)));
				pulse1_NegateFlag <= Data_in(3);
				pulse1_ShiftCount <= to_integer(unsigned(Data_in(2 downto 0)));
				pulse1_LengthCounterLoadFlag <= '1';
			when "00010" =>	--$4002
				pulse1_TimerLoad(7 downto 0) <= Data_in;
				pulse1_LengthCounterLoadFlag <= '1';
			when "00011" =>	--$4003
				pulse1_EnvelopeStartFlag <= '1';		--Write starts envelope
				pulse1_TimerLoad(10 downto 8) <= Data_in(2 downto 0);
				pulse1_LengthCounterLoad <= lut_lengthCounter;
				pulse1_count := 0;
				pulse1_LengthCounterLoadFlag <= '1';
			when "00100" =>	--$4004
				pulse2_sequencer <= lut_DutyCycle;
				pulse2_LengthCounterHalt <= Data_in(5);
				pulse2_EnvelopeConstantFlag <= Data_in(4);
				pulse2_EnvelopeDecay <= to_integer(unsigned(Data_in(3 downto 0)));
				pulse2_LengthCounterLoadFlag <= '1';
			when "00101" =>	--$4005
				pulse2_SweepEnable <= Data_in(7);
				pulse2_DividerPeriod <= to_integer(unsigned(Data_in(6 downto 4)));
				pulse2_NegateFlag <= Data_in(3);
				pulse2_ShiftCount <= to_integer(unsigned(Data_in(2 downto 0)));
				pulse2_LengthCounterLoadFlag <= '1';
			when "00110" =>	--$4006
				pulse2_TimerLoad(7 downto 0) <= Data_in;
				pulse2_LengthCounterLoadFlag <= '1';
			when "00111" =>	--$4007
				pulse2_EnvelopeStartFlag <= '1';
				pulse2_TimerLoad(10 downto 8) <= Data_in(2 downto 0);
				pulse2_LengthCounterLoad <= lut_lengthCounter;
				pulse2_count := 0;
				pulse2_LengthCounterLoadFlag <= '1';
			when "01000" =>	--$4008
				triangle_LengthCounterHalt <= Data_in(7);
				triangle_linearCounterLoad <= to_integer(unsigned(Data_in(6 downto 0)));
			when "01001" =>	--$4009
			when "01010" =>	--$400A
				triangle_TimerLoad(7 downto 0) <= Data_in;
			when "01011" =>	--$400B
				triangle_LengthCounterLoad <= lut_lengthCounter;
				triangle_TimerLoad(10 downto 8) <= Data_in(2 downto 0);
				triangle_linearCounterFlag <= '1';
				triangle_LengthCounterLoadFlag <= '1';
			when "01100" =>	--$400C
				noise_LengthCounterHalt <= Data_in(5);
				noise_EnvelopeConstantFlag <= Data_in(4);
				noise_EnvelopeDecay <= to_integer(unsigned(Data_in(3 downto 0)));
			when "01101" =>	--$400D
			when "01110" =>	--$400E
				noise_LFSRMode <= Data_in(7);
				noise_TimerLoad <= Data_in(3 downto 0);
			when "01111" =>	--$400F
				noise_EnvelopeStartFlag <= '1';
				noise_LengthCounterLoad <= lut_lengthCounter;
				noise_LengthCounterLoadFlag <= '1';
			when "10000" =>	--$4010
				dmc_IrqEnableFlag <= Data_in(7);
				dmc_LoopFlag <= Data_in(6);
				dmc_RateIndex <= Data_in(3 downto 0);
			when "10001" =>	--$4011
				dmc_out <= to_integer(unsigned(Data_in(6 downto 0)));
			when "10010" =>	--$4012
				dmc_address_tmp := "1" & Data_in & "000000";
				dmc_SampleAddress <= Data_in;
				dmc_SampleRemain <= 0;
			when "10011" =>	--$4013
				dmc_SampleLength <= Data_in;
				dmc_SampleRemain <= to_integer(unsigned(Data_in & "0001"));
			when "10100" =>	--$4014
			when "10101" =>	--$4015
				dmc_enable := Data_in(4);
				noise_enable := Data_in(3);
				triangle_enable := Data_in(2);
				pulse2_enable := Data_in(1);
				pulse1_enable := Data_in(0);
				set_interrupt := interrupt_event; --The interrupt flag is not cleared if set at the same clock
			when "10110" =>	--$4016
			when "10111" =>	--$4017		
				frameCounter_Mode <= Data_in(7);
				frameCounter_InterruptInhibit <= Data_in(6);
			when others =>
		end case;
		-- LengthCounters are tied to 0 when a channel is disabled
		if noise_enable = '0' then
			noise_lengthCounter_LengthCounter <= 0;
		end if;
		if triangle_enable = '0' then
			triangle_lengthCounter_LengthCounter <= 0;
		end if;
		if pulse2_enable = '0' then
			pulse2_lengthCounter_LengthCounter <= 0;
		end if;
		if pulse1_enable = '0' then
			pulse1_lengthCounter_LengthCounter <= 0;
		end if;
	end if;
	if ChipSelect = '1' and ReadWrite = '0' and phi2_ce = '0' then
		 case Address is
			when "10101" =>	--$4015
				Data_out <= dmc_interrupt & set_interrupt & '-' & dmc_SampleGate & noise_LengthCounterGate & triangle_LengthCounterGate
								& pulse2_LengthCounterGate & pulse1_LengthCounterGate;
				set_interrupt := interrupt_event; --The interrupt flag is not cleared if set at the same clock
			when others =>
		 end case;
	end if;	
	
end if;
dmc_interrupt := dmc_SampleGate and dmc_IrqEnableFlag;
irq <= set_interrupt nand dmc_interrupt;
dmc_address <= '1' & dmc_address_tmp;
end process;

-- Output Mixer
pulse_table_in <= std_logic_vector(to_unsigned(PCM_pulse1 + PCM_pulse2, 5));
tnd_table_in <= std_logic_vector(to_unsigned(PCM_triangle + 2* PCM_noise + dmc_out, 8));

	Inst_pulse_table : pulse_table
  PORT MAP (
    clka => phi2_ce,
    addra => pulse_table_in,
    douta => pulse_table_out
  );
  
	Inst_tnd_table : tnd_table
	  PORT MAP (
		 clka => phi2_ce,
		 addra => tnd_table_in,
		 douta => tnd_table_out
	  );

PCM_out <= std_logic_vector(unsigned(pulse_table_out) + unsigned(tnd_table_out));


end Behavioral;
