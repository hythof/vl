HOST_CC=ghci
HOST_HS=test.hs

all:
	clear
	make test_parse

clean:
	rm *.hi *.o Main

shell:
	clear
	${HOST_CC} *.hs

build:
	ghc Main.hs && ./Main demo.vl

test_parse:
	echo "import Main\ntestAll" | ${HOST_CC} test/ParserTest.hs
