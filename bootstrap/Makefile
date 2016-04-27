GHCI=ghci
RUN=runghc

.PHONY: help
all:
	@echo "make dependency # install dependency item with aptitude"
	@echo "make build      # make executable file of Main"
	@echo "make test       # run all test"
	@echo "make clean      # remove all temporary files"

.PHONY: dependency
dependency:
	sudo aptitude install ghc-ghci libghc-parsec3-dev libghc-hunit-dev libghc-readline-dev

.PHONY: clean
clean:
	rm -f *.hi *.o bin/*

.PHONY : build
build:
	mkdir -p bin
	ghc Main.hs -o vl && mv vl ./bin/

.PHONY : test
test:
	${RUN} test/ParseTest.hs
	${RUN} test/EvalTest.hs
