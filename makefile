
OUTPUTDIR = ./bin



all: test

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete


test:
	ghc --make ./test.hs -o ${OUTPUTDIR}/test

