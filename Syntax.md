# Syntax

```
root = top | define | stmt
top = [struct enum data scope] id+ "{" members? "}"
define = id arg* "=" body
body = (define BR)* stmt
exp = op1 | op2
op1 = OP1 ref
op2 = ref OP2 op2
stmt =
| "do " lines
| "if" exp lines ("else" lines)?
| "for" exp ("," exp) lines
| "next" ("if" exp)?
| "break" ("if" exp)?
| "return" ("if" exp)?
| exp (BR stmt)*
lines = exp | ("{" (exp | stmt) (BR (exp | stmt))* "}")

id = [a-zA-Z_] [a-zA-Z0-9_]
arg = id
type = id | ("[]" id)
ref = bottom ("." id ("(" ref* ")")?)*
bottom = "(" bottom ")" | value | id
member = (id (type | value)) | define
members = member ("," member)*
value = [0-9]+(. [0-9]+)*
| "true" | "false"
| '"' [^"] '"'
| "[" ref* "]"
| "{" members? "}"
| (id | "(" id+ ")") "->" body

BR = "\n"
OP1 = [- ! ~]
OP2 = [+ - * / % & | << >> + - > >= < <=  == != || && := += /= *= /= %=]
```


## Primitive values

```
1              # integer 64bit
1.2            # float 64bit
true, false    # bool
"string"       # string
[1 2 3]        # array
{key 1, val 2} # struct
arg -> body    # function
```


## Function definition

```
one = 1
inc x = x + one
call = inc(2)
```


## Type definition

```
struct vector2:
  x i64
  y i64
enum option a:
  some a
  none
```


## Branch

```
abs x = if (x > 0) x (-x)
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
op2 op l r = when op {
  "+" = l + r
  "-" = l - r
}
```


## Statement

```
prompt mark = do {
  var count 0
  while {
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
show99 = for i = 1 to 9, j = 1 to 9 {
  puts "$i x $j = ${i * j}"
}
```


## Order of operation

Unary operations

```
- ~ !
```

Binary operations

```
* / %
& | << >>
+ -
> >= < <=  == !=
|| &&
:= += /= *= /= %=
```


## Syntax sugger

```
data vector2:
  x i64
  y i64

method addable(vector2):
  + a b =
    x = a.x + b.x
    y = b.y + b.y
    vector2(x y)

main = do {
  v1 = vector2(1 1)
  v2 = vector2(2 3)
  result = v1 + v2
  puts(result)
}
```


## Namespace

```
scope const
status = 200
message = "Hello"


scope httpd:
  common status message
  net.tcp listen
  protocol http

start param:":80" = listen(param).http.each req, res -> do {
  res.status(status)
  _make_body(res)
}
_make_body res = res.body(message)


scope main:
  httpd start

main = do {
  listen = argv(0 ":8080")
  start(listen)
}
```
