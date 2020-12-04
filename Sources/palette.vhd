library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity palette is
	port(	clk, vs_tmp, hs_tmp : in std_logic;
			color : in std_logic_vector(5 downto 0);
			vsync, hsync : out std_logic;
			VGA_RED, VGA_GREEN, VGA_BLUE : out std_logic_vector(2 downto 0));

end palette;

architecture Behavioral of palette is

begin

process(clk)
variable r, g, b : integer range 0 to 7;
variable color_tmp : integer range 0 to 63;
begin
if clk'event and clk='1' then
	color_tmp := to_integer(unsigned(color));
	case color_tmp is
                    when 0 => r := 3; g := 3; b := 3;
                    when 1 => r := 0; g := 1; b := 4;
                    when 2 => r := 0; g := 1; b := 5;
                    when 3 => r := 2; g := 0; b := 4;
                    when 4 => r := 2; g := 0; b := 3;
                    when 5 => r := 3; g := 0; b := 2;
                    when 6 => r := 3; g := 0; b := 0;
                    when 7 => r := 2; g := 1; b := 0;
                    when 8 => r := 1; g := 1; b := 0;
                    when 9 => r := 0; g := 2; b := 0;
                    when 10 => r := 0; g := 2; b := 0;
                    when 11 => r := 0; g := 2; b := 0;
                    when 12 => r := 0; g := 2; b := 2;
                    when 13 => r := 0; g := 0; b := 0;
                    when 14 => r := 0; g := 0; b := 0;
                    when 15 => r := 0; g := 0; b := 0;
                    when 16 => r := 5; g := 5; b := 5;
                    when 17 => r := 0; g := 2; b := 6;
                    when 18 => r := 2; g := 2; b := 7;
                    when 19 => r := 3; g := 1; b := 7;
                    when 20 => r := 4; g := 1; b := 6;
                    when 21 => r := 5; g := 1; b := 3;
                    when 22 => r := 5; g := 1; b := 1;
                    when 23 => r := 4; g := 2; b := 0;
                    when 24 => r := 3; g := 3; b := 0;
                    when 25 => r := 1; g := 4; b := 0;
                    when 26 => r := 0; g := 4; b := 0;
                    when 27 => r := 0; g := 4; b := 1;
                    when 28 => r := 0; g := 3; b := 4;
                    when 29 => r := 0; g := 0; b := 0;
                    when 30 => r := 0; g := 0; b := 0;
                    when 31 => r := 0; g := 0; b := 0;
                    when 32 => r := 7; g := 7; b := 7;
                    when 33 => r := 2; g := 5; b := 7;
                    when 34 => r := 4; g := 4; b := 7;
                    when 35 => r := 6; g := 3; b := 7;
                    when 36 => r := 7; g := 3; b := 7;
                    when 37 => r := 7; g := 3; b := 6;
                    when 38 => r := 7; g := 3; b := 3;
                    when 39 => r := 7; g := 4; b := 1;
                    when 40 => r := 5; g := 5; b := 0;
                    when 41 => r := 4; g := 6; b := 0;
                    when 42 => r := 2; g := 7; b := 1;
                    when 43 => r := 2; g := 6; b := 3;
                    when 44 => r := 2; g := 6; b := 6;
                    when 45 => r := 2; g := 2; b := 2;
                    when 46 => r := 0; g := 0; b := 0;
                    when 47 => r := 0; g := 0; b := 0;
                    when 48 => r := 7; g := 7; b := 7;
                    when 49 => r := 5; g := 6; b := 7;
                    when 50 => r := 6; g := 6; b := 7;
                    when 51 => r := 7; g := 6; b := 7;
                    when 52 => r := 7; g := 5; b := 7;
                    when 53 => r := 7; g := 5; b := 7;
                    when 54 => r := 7; g := 6; b := 6;
                    when 55 => r := 7; g := 6; b := 5;
                    when 56 => r := 6; g := 7; b := 4;
                    when 57 => r := 6; g := 7; b := 4;
                    when 58 => r := 5; g := 7; b := 5;
                    when 59 => r := 5; g := 7; b := 6;
                    when 60 => r := 5; g := 7; b := 7;
                    when 61 => r := 5; g := 5; b := 5;
                    when 62 => r := 0; g := 0; b := 0;
                    when 63 => r := 0; g := 0; b := 0;
		when others => r := 0; g := 0; b := 0;
	end case;
	vsync <= vs_tmp;
	hsync <= hs_tmp;
	VGA_RED <= std_logic_vector(to_unsigned(r,VGA_RED'length));
	VGA_GREEN <= std_logic_vector(to_unsigned(g,VGA_GREEN'length));
	VGA_BLUE <= std_logic_vector(to_unsigned(b,VGA_BLUE'length));
end if;
end process;

end Behavioral;
