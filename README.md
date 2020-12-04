# NES_FPGA_WS
A NES impletemented on a FPGA Waveshare Core3S500E

This is a NES in VHLD that runs on a Waveshare Core3S500E board (Xilinx Spartan XC3S500E FPGA), mounted on a homemade board.
It needs : 2 SRAM of 512kB (depending on games to be run), a 3 bits RGB DAC for the VGA / SCART output, and a 6 pins connector to connect the game board.

The game board is a PCB whith a SPI Flash memory (M25P40 or equivalent, depending of game size) containting a .nes file starting at adresse $000000.
If a saving state memory is needed, it will be saved at the next 64kB free sector of the SPI Flash.
