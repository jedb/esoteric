
OUTPUTDIR = bin

EXECUTABLES = ${OUTPUTDIR}/test ${OUTPUTDIR}/fractran ${OUTPUTDIR}/thue ${OUTPUTDIR}/unlambda



all: testprog fractranprog thueprog unlambdaprog


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

unlambdaprog:
	ghc -XDeriveDataTypeable --make unlambda.hs -o ${OUTPUTDIR}/unlambda

