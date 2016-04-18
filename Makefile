GHCI=ghci
RUN=runghc

.PHONY: help
all:
	@echo "make dependency # install dependency item with aptitude"
	@echo "make run        # run on runghc"
	@echo "make build      # make executable file of Main"
	@echo "make test       # run all test"
	@echo "make clean      # remove all temporary files"

.PHONY: dependency
dependency:
	sudo aptitude install ghc-ghci libghc-parsec3-dev libghc-hunit-dev libghc-readline-dev

.PHONY: run
run:
	(cd src; ${RUN} Main.hs)

.PHONY: clean
clean:
	rm -f src/*.hi src/*.o bin/*

.PHONY : build
build:
	mkdir -p bin
	(cd src; ghc Main.hs -o vl && mv vl ../bin/)

.PHONY : test
test:
	(cd src; ${RUN} test/ParseTest.hs)
	(cd src; ${RUN} test/EvalTest.hs)
