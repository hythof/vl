# Syntax

```
root = top+
top = package? | import* | export* | defines
package = "package" name
import = "import" name+
export = "export" id+
defines = define (BR define)*
define =
| "struct" id+ "{" members? "}"
| "enum" id+ "{" id type? (SEP id type)* "}"
| id id* "::" type+
| id arg* "=" body
body = func | exp | matches | (BR INDENT (define | exp))+
exp =
| "do" block
| "while" id? exp block
| "for" id "=" exp ("," id "=" exp)* block
| "if" exp block ("else" block)?
| "if" exp (BR "|" exp){2}
| "when" exp+ matches
| "return" exp ("if" exp)?
| (("next" | "break") id?) ("if" exp)?
| "var" id exp
| unit (OP2 exp)? with?
matches = (BR "|" match+ "=" exp)+ with?
match = value | "." name
block = "{}" | "{" line (SEP line)* "}" with?
with = "with {" BR defines "}"

id = [a-zA-Z_] [a-zA-Z0-9_]
name = id ("." id)*
arg = id
type = (id | ("[" type+ "]")) "?"?
unit = bottom ("." id ("(" unit* ")")?)*
bottom = "(" (bottom | func) ")" | value | id
member = (id type value?) | define
members = member (SEP member)*
value =
| [0-9]+(. [0-9]+)*
| "true" | "false"
| '"' [^"] '"'
| "[" unit* "]"
| "(" id arg* = exp ")"
func = (id | "(" id+ ")") "->" exp

BR = "\n"
SEP = BR | ","
INDENT = "  "
OP2 = [; + - * / % & | << >> + - > >= < <=  == != || && := += /= *= /= %=]
```


## Primitive values

```
1                # integer 64bit
1.2              # float 64bit
true, false      # bool
"string"         # string
[1 2 3]          # array
["a": 1, "b": 2] # dictionary
(a = 1, b = "s"} # struct
```


## Function definition

```
one = 1
inc x = x + one
call = inc(2)
add = a -> b -> a + b
main = {
  print("ok")
}
```


## Type definition

```
struct vector2 {
  x i64
  y i64
}
enum option a {
  some a
  none
}
```


## Branch

```
gcd a b = if b == 0
| a
| gcd b (a % b)
fib x =
| (< 1) = 0
| 1 = 1
| _ = fib(x) + fib(x - 1)
op1 op val = when op
| "-" = -1 * val
| "!" = if val false true
| "~" = xor(val)
op2 op l r = when op
| "+" = l + r
| "-" = l - r
```


## Statement

```
prompt mark = do {
  var count 0
  while true {
    print(mark)
    cmd = readline
    next if cmd.empty
    break if cmd == "exit"
    if cmd == "show" {
      puts("count $count")
      next
    } else {
      count += 1
    }
    when cmd
    | "help" = puts("some messages")
    | "9x9" = show99
    | _ = puts("unknown command: $cmd")
  }
  return count
}
show99 = for i = 1..9, j = 1..9 {
  puts("$i x $j = ${i * j}")
}
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


## Namespace

```
package util
import protocol
export y2j
y2j s = protocol.yaml.parse(s).(protocol.json.dump)
```
