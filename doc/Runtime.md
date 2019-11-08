# Runtime design

Runtime consists the following three parts.

- core
- standard
- library



## core

It provides primitive value with fixed memory layout.

- i1, i8..i64
- f32, f64
- struct
- array : { len i64, nodes [T] }
- string : { len i64, chars [utf8] }
- dict : k v { contains k -> bool, get k -> maybe v, set k -> v -> void }



## standard

It provides primitive methods, C bindings for system call.
User can use mixed version `standard` with latest `core`.

Primitive methods:

- array
- string
- hash dict
- weak dict
- transaction

System call:

- File IO
- Network IO



## library

It consists 3rd party libraries.
User can manage dependencies.

Examples:

```
scope mail

dependencies {
  protocol.imap
  protocol.pop3 https://example.com/protocol/pop3.git
}
```
