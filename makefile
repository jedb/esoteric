
OUTPUTDIR = bin
SOURCEDIR = src

EXECUTABLES  =  ${OUTPUTDIR}/test \
                ${OUTPUTDIR}/fractran \
                ${OUTPUTDIR}/thue \
                ${OUTPUTDIR}/unlambda \
                ${OUTPUTDIR}/brainfuck

SWITCHES = -XDeriveDataTypeable



all: testprog fractranprog thueprog unlambdaprog brainfuckprog


clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

distclean:
	rm ${EXECUTABLES}



testprog:
	cd ${SOURCEDIR}; \
	ghc ${SWITCHES} --make test.hs -o ../${OUTPUTDIR}/test

fractranprog:
	cd ${SOURCEDIR}; \
	ghc ${SWITCHES} --make fractran.hs -o ../${OUTPUTDIR}/fractran

thueprog:
	cd ${SOURCEDIR}; \
	ghc ${SWITCHES} --make thue.hs -o ../${OUTPUTDIR}/thue

unlambdaprog:
	cd ${SOURCEDIR}; \
	ghc ${SWITCHES} --make unlambda.hs -o ../${OUTPUTDIR}/unlambda

brainfuckprog:
	cd ${SOURCEDIR}; \
	ghc ${SWITCHES} --make brainfuck.hs -o ../${OUTPUTDIR}/brainfuck
