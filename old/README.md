# About 
This document describe programming language especially design and implements.

# Syntax

```
top:
  exp*

exp:
  "(" exp ")"
  val
  ref arg* : exp  # type
  ref arg* = exp  # function
  ref exp*        # call
  exp (, exp)+    # tuple
  op1 exp?
  exp? op2 exp?
  # [^¥n]*        # comment
  [;\n]

id:
  [a-z][a-z0-9]*

ref:
  id (. id)*

arg:
  "(" arg ")"
  exp (: type)?
  arg [*+]  # repeatable

val:
  [0-9]+ (. [0-9]+)? # number
  'c'         # utf8 char
  "string"    # string
  [exp*]      # list
  {exp*}      # struct
  arg+ => exp # anonymous

op2:
  [+-*/><]
  <= >= == <=>
  >> <<
  || &&

op1:
  [+-*!~]
```

# Dependency
- ICU 64.2: for utf8 operations

