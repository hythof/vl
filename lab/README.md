Project hpp
===========

Goal
====
The program language all acheve productivity, performance and portability.

For productivity
- shorter
- easy to change
- understandable

For performance
- less than 1.1 times slower than C
- less than 1.1 times use memory than C

For portability
- Linux
- BSD and macOS
- Windows
- other languages such as JavaScript without IO library

Background
==========
High performance programing language is normaly low productivity.
The opposit is also true.
Sometime making almost same code in difference languages.

High level architecture
=======================
Only for first version.
1. compile compiler.vl to compiler.c by compiler.vl run on Haskell runtime
2. compile compiler.vl by before step generated binary

Later versions.
1. get previous version compiler.c
2. build compiler.c by clang and get compiler executable file
3. compile compiler.vl to compiler by compiler

Development dependency
- clang
- pthread
- epoll | queue | async/await
- ICU for utf8

Features
========
Language level
- int64, float64, string(utf8), function
- exception
- ad-hock polymorphism
- parametric polymorphism
- multi dispatch
- aldgebraic data type
- GC
- software transactional memory

Runtime level
- epoll + multi process

Major functions
===============
```
enum ast:
  int int
  float float
  string string
	function name string, args []string, body ast
	call name []ast
  op2 name string, left ast, right ast
  list []ast
  struct [](string, ast)
	dict [](string, ast)
parse string ast
optimize ast ast
compile ast [(string, ast)] string
```

Compiling flow
==============
For main program
1. open main.vl and imported files
2. parse all files to ASTs
3. find `main` function from ASTs
4. compile all function and generic them to AST functions
5. compile AST functions to C source codes
6. link to executable file by lld

Examples
========
```
TBD
```

Security
========
TBD

Well known problems
===================
TBD

Tests
=====
- feature tests
- function tests for primitive types
- performance tests

Repository
==========
git@github.com:hythof/vl.git

History
=======
2019/07/09 write README.md
