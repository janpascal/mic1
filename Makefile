SVN_REVISION := $(shell svn info | grep ^Revision: | grep -o [0-9]*)
SVN_ROOT := $(shell svn info | grep Root: | grep -o svn.*\\\|http.*)

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
	clear ; make ; sleep 1; ./micropro; sleep 1; touch micropro.pas ; sleep 1; make ; ./micropro

dist:
	TMPDIR=$$( mktemp -d ); \
	svn export . $${TMPDIR}/mic1-svn$(SVN_REVISION); \
	tar czf ../mic1-svn$(SVN_REVISION).tar.gz -C $${TMPDIR} mic1-svn$(SVN_REVISION); \
        rm -rf $${TMPDIR}
