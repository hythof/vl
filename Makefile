all:
	@echo "# metrics"
	@make -s metrics
	@echo
	@echo "# test"
	@make -s test

metrics:
	@echo `cat src/bootstrap/*.hs | wc -l` "lines bootstrap"
	@echo `cat src/bootstrap/test/*.hs | wc -l` "lines bootstrap/test"

test:
	@cd src/bootstrap; runghc test/ParserTest.hs && runghc test/EvalTest.hs
