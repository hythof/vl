# Syntax

## primitive value

```
1
1.2
true, false
"string"
[1 2 3]
assign = "value"
add a b = a + b
call = add 1 10
```

## user definition type

```
data vector2:
  x i64
  y i64
enum option a:
  some a
  none
```

## syntax sugger

```
method vector2:
  show = "($x,$y)"
```


## build-in functions

```
interface:
  min a : a a a
  max a : a a a
  range i64 i64 list(i64)

interface bool:
  if a : a a

interface string:
  count i64
  append string string
  slice i64 i64 string

interface list a:
  nth i8..64 a
  count i64
  append list(a) list(a)
  slice i64 i64 list(a)
  map b : (a b) list(b)
  filter : (a bool) list(a)
  min : a
  max : a
```

## the order of operators

```
~ ! # only single operators
* / %
& | << >>
+ -
> >= < <=  == !=
|| &&
:= += /= *= /= %=
```
