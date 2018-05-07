all:
	@echo "# Metrics"
	@make -s metrics
	@echo
	@echo "# Tests"
	@make -s test

metrics:
	@echo `cat src/bootstrap/*.hs | wc -l` "lines bootstrap"
	@echo `cat src/bootstrap/test/*.hs | wc -l` "lines bootstrap/test"

test:
	@echo "parse syntax"
	@cd src/bootstrap; runghc test/ParserTest.hs
	@echo "eval ast"
	@cd src/bootstrap; runghc test/EvalTest.hs
