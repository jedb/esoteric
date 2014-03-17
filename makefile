
OUTPUTDIR = bin

EXECUTABLES = ${OUTPUTDIR}/test ${OUTPUTDIR}/fractran ${OUTPUTDIR}/thue ${OUTPUTDIR}/unlambda

SWITCHES = -XDeriveDataTypeable



all: testprog fractranprog thueprog unlambdaprog


clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

distclean:
	rm ${EXECUTABLES}



testprog:
	ghc ${SWITCHES} --make test.hs -o ${OUTPUTDIR}/test

fractranprog:
	ghc ${SWITCHES} --make fractran.hs -o ${OUTPUTDIR}/fractran

thueprog:
	ghc ${SWITCHES} --make thue.hs -o ${OUTPUTDIR}/thue

unlambdaprog:
	ghc ${SWITCHES} --make unlambda.hs -o ${OUTPUTDIR}/unlambda

