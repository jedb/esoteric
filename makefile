
OUTPUTDIR = bin

EXECUTABLES = ${OUTPUTDIR}/test ${OUTPUTDIR}/fractran ${OUTPUTDIR}/thue



all: testprog fractranprog thueprog


clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

distclean:
	rm ${EXECUTABLES}



testprog:
	ghc --make test.hs -o ${OUTPUTDIR}/test

fractranprog:
	ghc --make fractran.hs -o ${OUTPUTDIR}/fractran

thueprog:
	ghc --make thue.hs -o ${OUTPUTDIR}/thue

