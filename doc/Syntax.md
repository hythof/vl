# Syntax

```
root = top+
top = package? | import* | export* | defines
package = "package" name
import = "import" name+
export = "export" id+
defines = define (BR define)*
define = func | declear
func =
| id id* "::" type+
| id arg* "=" body
var = id type value?
declear =
| "struct" id+ ":" BR? members?
| "enum" id+ ":" BR? id type? (SEP id type)*
body = closure | exp | matches | (BR INDENT (func | exp))+ with?
exp = unit (OP2 exp)?
match = (PIPE matcher+ "=>" exp)+
matcher = value | "." name | closure
with = BR "with: " (BR INDENT (func | var))+

id = [a-zA-Z_] [a-zA-Z0-9_]
name = id ("." id)*
arg = id
type = (id | ("[" type+ "]")) "?"?
unit = bottom ("." id ("(" unit* ")")?)*
bottom = "(" (bottom | closure) ")" | value | id
member = INDENT var | func
members = member (SEP member)*
value =
| [0-9]+(. [0-9]+)*
| "true" | "false"
| '"' [^"] '"'
| "[" unit* "]"
| "(" id arg* = exp ")"
closure = (id | (" id+ ")") "->" exp

BR = "\n"
PIPE = "\n|"
SEP = BR | ","
INDENT = "  "
OP2 = [; + - * / % & | << >> + - > >= < <=  == != || && := += /= *= /= %=]
```

## Values

```
1               # integer 64bit
1.2             # float 64bit
"string"        # string
true, false     # bool
(a,b) -> a + b  # function
```

## Containers

```
[1 2 3]          # array
(a = 1, b = "s") # struct
("a": 1, "b": 2) # dictionary
```

## Function

```
one = 1
inc x = x + one
pair x = (x = x, y = inc(x))
```


## Types

```
struct vector2 {
  x i64
  y i64
}
interface printable {
  to_string string
}
struct vector3 {
  inherit(vector2)
  inherit(printable) {
    to_string = "($x, $y, $z)"
  }
  z i64
}
enum option a {
  some a
  none
}
inc :: i64 i64
inc x = x + 1
add num(n) :: n n n # accept i8 to i64 and f32 to f64
add x y = x + y
hello :: string io
hello name = print("Hello" name)
trace x :: x io(x)
trace x = print(x); x
```

## Sequence

```
hi = println("Hi."); println("How's going?")
reply =
  println("Oh, Hi.")
  println("Pretty good.")
```


## Branch

```
gcd a b = b == 0
| a
| gcd(b a % b)
fib x =
| (< 0) = 0
| 1 = one
| _ = fib(x) + fib(x - 1)
```

## Loop

```
for n = n.items(x -> print(x))
while n = (m > 0).while(m -= 1; print("decrement" i))
with:
  m n
```

## Coroutine

```
nat n = coroutine(f -> f(n); nat(n + 1))
ten = nat(1).take(10) # it's consisted 1 to 10
```

## Error handling

```
first a :: [a] ? # same as nullable(a)
first xs =
| [x _] = x
| _ = nil
guard :: param void! # same as failable(void)
guard param =
| (param.name == "") = fail("name should be not empty")
| _ = pass(void)
to_i64 :: string i64! # same as failable(i64)
to_i64 s = calc(s 0)
with:
  calc s n = s
  | ["0" xs] = calc(xs (n * 10))
  | ["1" xs] = (1 * n) + calc(xs (n * 10))
  # ...
  | ["9" xs] = (9 * n) + calc(xs (n * 10))
  | _ = fail("There is not a number about " s)
```


## Namespace

```
package util
import protocol
export y2j
y2j s = protocol.yaml.parse(s).(protocol.json.dump)
```


## Order of operation

Binary operations

```
* / %
& | << >>
+ -
> >= < <=  == !=
|| &&
:= += /= *= /= %=
```

## Reserved keyword

```
true, false
nil, fail(any...)
```
