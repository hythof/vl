GHCI=ghci
RUN=runghc
CC=gcc -Wall -Wshadow -Wpointer-arith -Wcast-qual -Werror -g

.PHONY: all
all:
	@echo "make run   # run on runghc"
	@echo "make build # make executable file of Main"
	@echo "make test  # run all test"
	@echo "make vm    # create vl.out from vl.vl"
	@echo "make clean # remove all temporary files and all build files"

.PHONY: clean
clean:
	rm *.hi *.o Main tmp/*

.PHONY: run
run:
	${RUN} Main.hs

.PHONY: shell
shell:
	${GHCI} AST.hs Parser.hs Eval.hs

# developping
.PHONY : build
build:
	ghc Main.hs && ./Main

# developping
.PHONY : test
test:
	${RUN} test/ParserTest.hs
	${RUN} test/EvalTest.hs
