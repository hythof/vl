run: lib.ll
	runghc Main

lib.ll: lib.c
	clang -S -emit-llvm -O3 lib.c -o lib.ll
