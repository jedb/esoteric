
OUTPUTDIR = ./bin

EXECUTABLES = ${OUTPUTDIR}/test


all: test

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

distclean:
	rm ${EXECUTABLES}


test:
	ghc --make ./test.hs -o ${OUTPUTDIR}/test

