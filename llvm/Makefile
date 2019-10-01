reset:
	vi add.c
	/usr/bin/clang -S -emit-llvm -O0 add.c
	/usr/local/Cellar/llvm/8.0.1/bin/llc add.ll
	clang add.s -o add
	./add
	cat add.ll

check:
	vi add.ll && lli add.ll
