reset:
	vi add.c
	clang -S -emit-llvm -O3 add.c
	llc add.ll
	as -o add.o add.s
	ld64.lld -lSystem -o add add.o
	./add
	vi add.ll

check:
	vi add.ll && lli add.ll

clean:
	rm -rf add add.dSYM add.ll add.s add.o
