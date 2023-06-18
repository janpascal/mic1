all:	micropro MICRO.MIC MACRO.MAC

micropro: *.pas
	fpc micropro.pas

MICRO.MIC: micro.mat micasm
	./micasm micro.mat MICRO.MIC

MACRO.MAC: macro.asm macasm
	./macasm macro.asm MACRO.MAC

micasm: micasm.pas
	fpc micasm.pas

macasm: macasm.pas
	fpc macasm.pas

clean:
	rm -f *.o *.ppu *.mac *.mic *.MAC *.MIC micropro micasm macasm

run:	micropro MICRO.MIC MACRO.MAC
	clear ; ./micropro

