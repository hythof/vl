run:
	@cd src/bootstrap; cat ../*.vl | runghc Main.hs

metrics:
	@echo `cat src/bootstrap/*.hs | wc -l` "lines bootstrap"
	@echo `cat src/bootstrap/test/*.hs | wc -l` "lines bootstrap/test"

test:
	@cd src/bootstrap; runghc test/ParserTest.hs && runghc test/EvalTest.hs
