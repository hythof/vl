#!/usr/bin/ruby

require "strscan"
require "pp"

class VL
  attr_reader :scope
  def initialize src
    @src   = src
    @scan  = StringScanner.new src + "\n"
    @scope = Scope.new "___main___"
  end

  R_VAR   = /[a-z][0-9a-z_]*/
  R_CONST = /[A-Z][0-9a-z_]*/
  R_LOCAL = /_[A-Z][0-9a-z_]+/
  R_ID    = /#{ R_VAR }|#{ R_CONST }|#{ R_LOCAL }/o
  def parse
    s = @scan
    while !s.eos?
      case
      when s.scan(/\s+/)
        # nothing
      when s.scan(/(#{ R_VAR }) *= */o)
        @scope.apply += ["=", s[1], parse_expr]
      else
        raise "Parse Error. rest=#{ s.rest }"
      end
    end
  end

  def parse_expr
    s = @scan
    expr = []
    while !s.scan(/\n/)
      case
      when s.scan(/ +/)
        # nothing
      when s.scan(/\d+\.\d+/)
        expr += [:double, s[0]]
      when s.scan(/\d+/)
        expr += [:int, s[0]]
      when s.scan(/[\*\/]/)
        expr += [:expr, s[0]]
      when s.scan(/[\+\-]/)
        expr += [:term, s[0]]
      else
        raise "Parse Error. rest=#{ s.rest }"
      end
    end
    return expr
  end

  class Scope
    attr_accessor :name, :arg, :ret, :param, :apply
    def initialize name
      @name  = name
      @arg   = []
      @ret   = []
      @param = []
      @apply = []
    end
  end

  class Var
    attr_accessor :name, :value, :typed
    def initialize name, value, typed
      @name  = name
      @value = value
      @typed = typed
    end
  end

  class Ref
    attr_accessor :name, :need, :typed
    def initialize name, need, typed
      @name  = name
      @need  = need
      @typed = typed
    end
  end
end

if $0 == __FILE__
  require "stringio"
  $count = 0
  $error = []
  def apply code, expect
    $count += 1
    vl = VL.new code
    vl.parse
    fact = vl.scope.apply
    if expect.inspect == fact.inspect
      puts "."
    else
      puts "E"
      $error.push ["-- code", code, "-- expect", expect.inspect, "-- fact", fact.inspect].join("\n")
    end
  end

  apply "a = 1 + 2", [["def", "a"], ["+", 1, 2]]

  puts $error
end

__END__
stdout = env.stdout
msg    = "hello world"
stdout.puts msg

test : {
  stdout : io
  mmap   : io.memory
  app    : load.main stdout=mmap random={ .seed = 5} time={ .at(2011 5 6 sec=2) } # fixed randomize
#  app(msg="no output message")
#  stdout.puts mmap.stdout
}

__
std fmt cgi smtp : load.*
stderr stdout : env.*
request : [{k:"" v:""}]

stdout(cgi(request).response.string)
smtp("localhost").send(app.sendmail)
app.log.each {
  msg : arg.msg
  stderr msg
}
# or stderr app.log.array

# app.vl
CRLF : "\r\n"
request : {
  path         : "/"
  query_string : ""
}
env : {
  cookie : {
    domain : ""
    expire = Time.now
  }
}
response : {
  status   : 200
  type     : "text/html"
  charset  : "utf-8"
  body     : ""
  cookies  : [{
    name   : ""
    value  : ""
    path   : ""
    expire : env.cookie.expire
    domain : env.cookie.domain
    secure : F
    string : " ".join(
      "%s=%s".format name value
      domain ? "Domain=%s".format(domain)
      path   ? "Path=%s".format(path)
      expire ? "Expire=%d".foramt(expire.utc)
      secure ? "secure=1"
    )
  }]
  cookies.set_cookie : [{"Set-Cookie: %s.string"}].join(";" + CRLF)
  headers            : [{k:"" v:""}]
  headers.string     : [{ "%s.k: %s.v" }].join(CRLF)
  string             : CRLF.join(
    "Status: %d".format(status)
    headers.string
    "Content-Type: %s;%s".format(type, charset)
    "Content-Length: %d".format(body.size)
    cookies.set_cookie
    ""
    body
  )
}
sendmail : [{
  from    : ""
  to      : ""
  subject : ""
  body    : ""
  headers : [{k:"", v:""}]
  files   : [file]
}]


# contact.vl
email : string + {
  domain   : .right "@"
  username : .left "@"
  invalid  : not .valid
  valid    : .match("^\w+@[\w.]+")
  freemail : .end("@hotmail.co.jp")
}
name       : ""
email      : email("")
address    : ""
address_id : ""
remark     : ""
confirm    : ""
confirmed  : confirm != ""
valid      : error.size == 0
invalid    : !valid
error      : [
    !name            ? "名前を入力してください"
    <
      email.empty    ? "メールを入力してください"
      email.invalid  ? "メールアドレスの形式が正しくありません"
      email.freemail ? "フリーメールアドレスはお使いになれません"
    >
    !address         ? "住所を入力してください"
    !address_id      ? "県を選択してください"
  ]
html = <
    cond : [valid confirmd]
    cond == [T T] ? "contact/thanks.html"
    cond == [T _] ? "contact/confirm.html"
    cond == [_ _] ? "contact/index.html"
  >

##################################################
if/loop/this
-- if / unless / else / else if / switch
if
- a == b { ... }
- a == c and d > 0 { ... }
- { ... }
if a
- b { ... }
- c { ... }
- { ... }
cond = if 1
cond 2 { ... }
cond(3){ ... }
cond_b = if 2.mod(args[0])
cond(2) & cond_b(2) { ... }
cond . cond_b 2 { }
-- for / while
loop 1..10 { i = var.count; ... }
loop 1..10 step=2 { i = var.count; ...; }
loop label=exit { loop{ if ... loop.last.exit } } # or loop.last(-1)
each = loop { n:int Callback() }
each 19 { ... }
-- variable
a:100
b:100:readonly
c : 100 : double : readonly
d : 100.0 : decimal
e : "msg"
f : string
-- type
a : "" : string
b : 0 : int
c : 0.0 : double
d : <1 2 3> # compiler valid on (1 or 2 or 3)
d : <0 :string> # compiler valid on 0 or string type
e : [:int]
f : [<1 2 3 :str :double>]
g : [{key:string val:string}]:hash
h : { name:string age:int(< 100) }
-- define(no override)
a = 100 # const
a = { ... } # const
-- function
a = {
  arg1 : string
  arg2 : int
  msg = (arg1 + "") * arg2
}
a("hello", 3) # => hello hello
a.arg1 = "hello"
a.arg2 = 2
a.msg # => hello hello
b = a.copy(arg1="say")
b.arg2 = 1
b.msg, a.msg # => say, hello hello hello
a.arg1 = "hello"
a(arg1="")    # compile error
a(arg2=2).msg # => hello hello
a = { a:string b:int = a*b }
-- other
a = 1
b = {
  a = 2
  print a           # => 2
  print var.a       # => 2
  print var.outer.a # => 1
  print var.outer.outer.a # => Error
}

type Tree  = Empty
| Leaf Int
| Node Tree Tree


depth = Tree -> int
depth Empty = 0
depth Leaf  = 1
depth Node l r = 1 + max(depth 1, depth r)

