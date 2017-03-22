#/usr/bin/perl

use strict;
use warnings;
use utf8;
use Parse::RecDescent;
use Data::Dumper;

$::RD_ERRORS = 1;
$::RD_WARN   = 1;
$::RD_HINT   = 1;
$::RD_TRACE  = 0 if $ARGV[0];
#$Parse::RecDescent::skip = " *";

my @stack;
my $current;

my $grammer = q!
  start     : statement(s)

# statement
  statement : comment(s)
            | id "=" expr ##
            | option
            | ref

# expression
  cond      : expr '==' expr
            | expr '<>' expr
            | expr '<'  expr
            | expr '>'  expr
            | expr '<=' expr
            | expr '>=' expr
            | expr '<<' expr
            | expr '>>' expr
  expr      : term "+" expr ##
            | term "-" expr ##
            | term ##
  term      : fact "*" term ##
			| fact "/" term ##
			| fact ##
  fact      : literal
            | ref
            | array
            | struct
            | "(" expr ")" ## 2
# primitive
  id        : /[A-Za-z_][0-9A-Za-z_]*/
  literal   : double | int | string | regexp
    double  : /-?[0-9]+\.[0-9]+/ ##
    int     : /-?[0-9]+/ { int($item[1]) }
    string  : '"' <skip: ''> /[^"]*/ '"' ## 3
            | "'" <skip: ''> /[^']*/ "'" ## 3
    regexp  : "/" <skip: ''> /[^\/]+/ "/" /[imxoesun]?/ ## 2 4
  option    : id ":" expr ##*
  struct    : "{" struct_kv(s?) "}" ## 0 2
  struct_kv : id ":" expr ##
  array     : "[" expr(s?) "]" ## 0 2
  ref       : id expr(s?) ##*

# etc
  comment   : "#" /.+/ ## 2
!;
$grammer =~ s! ###!    { ::puts(::Dumper(\@item)); \@item }!g;
$grammer =~ s! ## +(\d+(?: \d+)+)!     { [\@item[qw($1)]] }!g;
$grammer =~ s! ## +(\d+)!     { \$item[$1] }!g;
$grammer =~ s! ##\*! { [\@item] }!g;
$grammer =~ s! ##! { \$#item == 1 ? \$item[1] : [\@item[1 .. \$#item]] }!g;
$grammer =~ s! ##([a-z]\w+) +(.+)! { ::def_$1(\@item[qw($2)]) }!g;
#print $grammer;

sub parse {
  my($vl_code) = @_;
  @stack = ();
  $current = {};
  def_block("main");
  my $puts   = def_block("puts");
  my $parser = new Parse::RecDescent($grammer);
  my $result = $parser->start($vl_code) || die "parse error $@";
return $result;
  return $current;
}
sub def_block {
  my($name, @flags) = @_;
  error("$name redefined function") if exists $current->{$name};
  my $new = {
	arg => [],
	ret => [],
    var => {},
    run => [],
  };
  $current->{$name} = $new;
  push @stack, $current;
  $current = $new;
}
sub def_block_end {
  $current = pop @stack;
}
sub def_var {
  my($name, $value, @flags) = @_;
  my $type = "undef";
  error("$name redefined variable") if exists $current->{$name};
  $current->{var}->{$name} = {
    type  => $type,
    value => $value,
  };
}
sub def_assign {
  my($name, $value, @flags) = @_;
  error("$name not defined variable") unless exists $current->{$name};
  push @{$current->{run}}, ["assign", $name, $value];
}
sub def_run {
  my($type, @value) = @_;
  push @{$current->{run}}, [$type, @value];
}

# utility
sub puts(@_) {
  print join " " => @_, "\n";
}
sub error {
  my($msg) = @_;
  die $msg;
}
sub fmt {
  my($fmt, @item) = @_;
  for(0 .. $#item) {
    $fmt =~ s!$_!$item[$_]!;
  }
  return $fmt;
}

sub t {
	my($a, $b)
}

# test
if($0 eq __FILE__) {
  my $tt = sub { print Dumper(parse(shift)); exit; };
#  $tt->("puts 1 + 2 (3) 4+(5-6)");

  my $t_n  = 0;
  my $t_ok = 0;
  my @error;
  my $t = sub {
	my($vl_code, @expect) = @_;
	++$t_n;
    my $result = parse($vl_code);
    if(Dumper(\@expect) eq Dumper($result)) {
	  ++$t_ok;
	  print ".";
    } else {
	  print "E";
	  push @error, "---( $t_n )---" . "\n" . Dumper(\@expect) . "\n" . Dumper($result) . "\n--\n" . $vl_code
    }
  };
  # primitive / string
  $t->('name = "foo"', ['name', '=', "foo"]);
  $t->("name = 'bar'", ['name', '=', "bar"]);
  $t->("name = 'foo' + 'bar'", ['name', '=', ["foo", '+', "bar"]]);
  # primitive / number
  $t->("n=100", ['n', '=', 100]);
  $t->("n=53.87", ['n', '=', 53.87]);
  # primitive / ()
  $t->("n = 1 + 2 * 3 - 4 / 5",
    ['n', '=', [1, '+', [[2, '*', 3], '-', [4, '/', 5]]]]);
  # primitive / array
  $t->("n = [1 2 3]",
    ['n', '=', ['array', [1,2,3]]]);
  # primitive / struct
  $t->("p = { name:'foo' age:27 }",
    ['p', '=', ['struct', [['name', ':', 'foo'], ['age', ':', 27]]]]);
  # primitive / variable
  $t->("a=1\nb=2\nc=a+b",
    ['a', '=', 1], ['b', '=', 2], ['c', '=', [['ref', 'a', []], '+', ['ref', 'b', []]]]);
  # function
  $t->("puts 'hello' ' ' 'world' 1+2",
    ['ref', 'puts', ["hello", " ", "world", [1,'+',2]]]);
  $t->("puts 1 + 2 (3) 4+(5-6)",
    ['ref', 'puts', [[1, '+', 2], 3, [4, '+', [5, '-', 6]]]]);

  print "\n$t_ok/$t_n\n";
  foreach(@error) {
	print $_, "\n\n";
  }
}


__END__
#puts 1 + (2 - 5)
n = 1 + 2 * 3 - 4 / 5
...
# run arguments
name : false
# def struct
:personal {
	name : "nobody"
	age  : 0
}

...

add a b : a + b
inc a : add 1 a
...
db       : "test"
host     : "localhost"
username : "test"
password : "test"
port     : 3306

_con = lib.tcp.open host port {
	# extend, hook, override
	# implements map high level API to low level API
	read_fetch = rows:[[string]] {
		
	}
}
parse sql   = sql.format prefix:'@'
execute sql = _con.write sql
query table = {
	field  : "*"
	limit  : 0
	offset : 0
	order  : []
	where  : []
	sql = "SELECT :field FROM :table"
	+ " WHERE (" + (where.join ") AND (") + ")" if where
	+ order.join "," if order
	+ " LIMIT :limit" if limit
	+ " OFFSET :offset" if offset
}
fetch q:query = rows:[[]] {
	execute q.sql
	rows = _con.read_fetch
}
fetch_admin  = :fetch { limit:10 order:"modified_at DESC" }
fetch_common = :fetch_admin { where:"hide=0" }
rows = fetch_common "user"


# primitive
a = 1
a = 1.0
a = "string"
a = 'string'
:a = [string] ; a += "hello"
a = [1 2 3] 
:a = [string => string]
:a = { name:string age:int } ; b = a "foo" 27; puts b.name
a = { name="foo" age=27 }   ; b = a name="bar"; puts b.name b.age
:a = { group:[{name:string age:num}] show:{ puts group } }
a.group += "bar" 28
a.group += name="hoge" age=29
a.group.push name="hoge" age=29
a = fmt:"1970/1/1 00:00:00" {
	y :: year    : 1970
	m :: month   : 1
	d :: day     : 1
	h :: hour    : 0
	i :: minutes : 0
	s :: second  : 0
    fmt := "%04d/%02d/%02d %02d:%02d:%02d" y m d h i s
}
a = (1 + 2 + 3)
a = <1 2 3> # a = 1 or 2 or 3
a = { age:<1..150> }
a = { level:<debug error crit> } # ={ level:<debug=0 error=1 crit=2> } ; a.level=<error>; puts a.level # 1
# expression
if BOOLEAN {}
else {}
else if BOOLEAN {}
if a 1:"even" 2:"odd" "ippai" # a==1 ? "even" : a==2 ? "odd" : "ippai"
for a = ARRAY {}
for a = NUM {}
for a = NUM .. NUM {}
for NUM {}
for NUM .. NUM {}
STRUCT => {
	.age = 28
}
STRUCT => a {
	a.age = 28
}
loop = {
	start : 1
	end   : 1
	for n = start .. end {
		block n
	}
}
loop2 = for 1 .. 10
loop2 {
	count : num
	puts count
}
loop2 . puts # eval
loop2 . puts . { ... } # loop2{ n : num; puts n; ... }
loop3 = loop2 . puts
loop3 { ... } # loop2{ n : num; puts n; ... }
# yoyakugo
if else for block next last _ and or F T
# enzainshi
+
-
/
*
**
%
. # 関数合成
A & { 1:B 2:C _:D } # switch-case
nums = [:int | :double] # type switch

in  = ym:string => code:string => [0]
out = ym:string => code:string => {num=0, total_price=0}
for line = io.lines {
  if line == "-- out --" ... {
    if line == "-- out --" {
      next
    }
    ymd _ code num _ price = line.split "\t"
    price.sub /\D/
    y m d = ymd.split "\t"
    ym    = "%04d:y/%02d:m"
    out:ym:code {
      .num         += num
      .total_price += price * num
    }
  } else {
    ymd _ _ code num _ cost = line.split "\t"
    cost.sub /\D/
    y m d = ymd.split "/"
    ym    = "%04d:y/%02d:m"
    in:ym:code += cost * num
  }
}
stock = code::s => [price=0]
for ym = in {
  for code price_list = in:ym {
    stock:code += price_list
  }
  sold_count = 0
  benefit    = 0
  for code v = out:ym {
    num total_price = v.*
    unless stock:code {
      warn "%{code} has no stock bat sold on %{ym}"
    }
    sold_count += num
    total_cost  = fold + stock[code].shift(num)
    benefit    += total_price - total_cost
    if benefit < 0 {
      puts "%{code} : %{total_price} - %{total_cost} = %{benefit}"
    }
  }
  stock_sum = fold + stock.price
  puts "%{ym} : stock = %{stock_sum} : benefit = %{benefit}"
}
# AST



-----------------------------------------------------
<!doctype html>
<html>
<head>
	<meta http-equiv="content-type" content="text/html; charset=UTF-8">
	<title>Google</title>
	<script>
	</script>
	<style>
	* {
		margin: 0;
		padding: 0;
	}
	body, html {
		height: 100%;
	}
	#layout_header {
		width: 100%;
		height: 50px;
		position: absolute;
		top: 0;
		left: 0;
		z-index: 1;
		background: #ff00ff;
	}
	#layout_navi {
		float: left;
		width: 200px;
		height: 100%;
		background: #ffaacc;
	}
	#layout_content {
		position: relative;
		margin-left: 200px;
		height: 100%;
		background: #ffffaa;
	}
	#preview {
		height: 100%;
		width: 100%;
		border: none;
		background: #aaaaaa;
	}
	</style>
</head>
<body>
	<div id="layout_header">
		RSS Reader	
	</div>
	<div id="layout_navi">
		<div class="wrap">
			navi
		</div>
	</div>
	<div id="layout_content">
		<div class="wrap">
			<iframe id="preview" frameborder="0" src="http://google.co.jp/"></iframe>
		</div>
	</div>
</body>
</html>
----------
var      = value | struct | array | function
value    = [0-9\.]+ | ".+?" | '.+?'
struct   = [ ... ]
# point = { x=0 y=0 }; p = point(1, 10); print p.x
array    = [ ... ]
# member = ["foo" "bar" "hoge"]; print member[0]
function = @arg1 arg2( ... )
# add = @a b (a+b); print (add 1 2)
--------------------------------------------------------------------------
in  = ym => code => [price=0]
out = ym => code => {num=0 total_price=0}
for line = io.lines {
  if line == "-- out --" ... {
    if line == "-- out --" {
      next
    }
    ymd _ code num _ price = line.split \t
    parse.sub! \D
    y m d = ymd.split \t
    ym    = "%04d/%02d" y m
    out[ym code].num         += num
    out[ym code].total_price += price * num
  } else {
    ymd _ _ code num _ cost = line.split \t
    cost.sub! \D
    y m d = ymd.split "/"
    ym    = "%04d/%02d" y m
    io[ym code] += cost * num
  }
}
stock = code => [price=0]
ym_list = in:keys
for ym = in {
  for code price_list = in[ym] {
    stock[code] += price_list
  }
  sold_count = 0
  benefit    = 0
  for code v = out[yum] {
    num total_price = v.*
    unless stock[code] {
      warn "$code has no stock bat sold on $ym"
    }
    sold_count += num
    total_cost  = stock[code].shift(num).fold +
    benefit    += total_price - total_cost
    if benefit < 0 {
      puts "$code : $total_price - $total_cost = $benefit"
    }
  }
  stock_sum = stock.fold +
  puts "$ym : stock = $stock_num : benefit = $benefit"
}








#!/usr/bin/vl -lang=perl

# global : os, lib, type, if, for, yield, return
# op   : + - / * % ** = == ? ++ --
# os   : time, random, fileIO, netIO, process, thread, sytemcall
# lib  : http, imap, hash.md5, bufio
# type : string, integer, array, list, table, regexp

trace(messages:s ...) = messages.each => (msg) os.stdout("({time_delta()}) {msg}")

time_delta() = {
	delta = os.time() - os.time.start_at
	day   = int(delta / 24 / 60 / 60)
	hour  = (delta / 60 / 60) % 24
	min   = (delta / 60) % 60
	sec   = delta % 60
	return (
		day  > 0 ? "(%d)%02d:%02d:%02d".format(day hour min sec)
		hour > 0 ? "%02d:%02d".format(hour min sec)
		min  > 0 ? "%02d:%02d".format(min sec)
		_        ? "%02d".format(sec)
	)
}
crawl = {
  # all fieldes readonly
	count      = 0
	retry      = 5
	wait_limit = 600
	timeout    = 60
	ua         = lib.use("LWP::UserAgent").new() # readonly
  cache      = [url:s={status:i, head:s, body:s}](index=url)

  wait(sec:i) = {
    limited_sec = sec > wait_limit ? wait_limit : sec
    yield(limited_sec)
    os.sleep(limited_sec)
  }

  get(url:s) {
		ua.timeout(timeout)
		++count
		trace("get({count}) {url}")
		for(1, retry) => (try) {
			rand_wait = os.rand(1, 10)
			trace("random_wait({rand_wait})")
			os.sleep(rand_wait)

			r = ua.get(url)
			r.code.left("4") ? {
        trace("not found {r.status_line}")
        try.last()
      }
			r.is_success() ? {
        return(response = Response(
            status = r.code()
            head   = r.header()
            body   = r.decoded_content()
          ))
      }
      wait(try ** 2) => (sec) {
        trace("retry({try}) wait({sec}) reason({r.status_line})")
      }
		}
	}

	store(url, path) = {
		file = os.path.file(path)
		file.exists => return file.read()
		return get(url) => (r) file.write(r.body)
	}
}
Run() = {
	dir  = 0
	book = [:s](uniq=T)
	env.data.readlines() => (line){
		group_name = line.chomp()
		group_name == "" ? next()
		++dir
		url = "http://www.example.com/{group_name}/index.html"
		os.mkdir(dir)
		html = getstore(url "{dir}/index.html") || next()
		html.match(r!href="(/contents/{group_name}/)(index.html)"!g) => (dir, file) {
			book_base = "http://www.example.com{dir}"
			book_url  = "{book_base}{file}"
			book.set(book_url) ? next()
			book_html = getstore(book_url "{dir}/{book.count}/index.html") || next()
			book_html.match(r!<a href="(\d+)\.html">\d+</a>!) => (num) {
				jpg_url = "{book_base}img/{num}.jpg"
				getstore(jpg_url "{dir}/{book.count}/{num}.jpg")
			}
		}
	}
}
main() = {
  for(10) { # 0 .. 9
    p = os.process => Run()
    p.wait()
    p.exit_code == 0 ? return
	}
}

________________________________________________________________
# maintenance.vl
statistics = os.atomic + {
  accept    = 0
  connect   = 0
  request   = 0
  response  = 0
  response_status {
    x200  = 0
    x300  = 0
    x400  = 0
    x500  = 0
    other = 0
  }
  client_close = 0
  thread_timeout = 0
}
Hook(app) = {
  lib.hook_in(
    app.accept       => ++statistics.accept
    app.connect      => ++statistics.connect
    app.http_process => ++statistics.request
  )
  lib.hook_out(
    lib.fiber.timeout => ++statistics.thread_timeout
    app.http_process  => ++statistics.response
    app.connect       => ++statistics.client_close
    app.run           => (status:i) {
      status >= 500 ? ++statistics.response_status.x500
      status >= 400 ? ++statistics.response_status.x400
      status >= 300 ? ++statistics.response_status.x300
      status >= 200 ? ++statistics.response_status.x200
      _             ? ++statistics.response_status.other
    }
  )
}
command = {
  statistics() = statistics.inspect() + os.systemcall.statistics.inspect()
  quit() = os.exit(0)
  exit() = os.exit(0)
}
Accept(port=8080) = {
  os.bind(port)
  os.accept() => (sock) {
    buf = lib.bufio(os.stdin)
    puts(msg)  = buf.write("{msg}\n")
    print(msg) = buf.write(msg)
    print("> ")
    buf.readlines() => (line) {
      cmd = line.strip()
      if(command[cmd]) {
        print(command[cmd]())
      } else {
        puts("cmd notfound '{cmd}'")
      }
      print("\n> ")
      buf.flush()
    }
  }
}
# main.vl
param = {
  thread_timeout = 30
  read_buffer    = 10 * 1024 * 1024
}
lib.macro(
  os.thread = lib.fiber
  os.fork   = lib.fiber
)
main() = {
  app = lib.load("app")
  mt  = lib.load("maintenance")
  mt.Hook(app)
  os.thread => accept()
  os.thread => mt.Accept()
}
accept() = {
  os.bind(port=80)
  for() {
    os.accept() => (sock) os.thread(timeout=param.thread_timeout) => connect(sock)
  }
}
connect(sock) = {
  buf = lib.bufio(sock, in_size=param.read_buffer)
  for() {
    ret = http_process(buf))
    if(ret) {
      buf.flush()
    } else {
      sock.close()
      break;
    }
  }
}
http_process(buf:lib.bufio) = {
  head = app.Header()
  buf.readlines() => (line) {
    last if line == ""
    line.split(/(.+): *(.+)/) => (k, v) head.push(k, v)
  }
  len  = head.get(key="Content-Length", 0)
  body = buf.read(len)
  if(body.len != len) {
    return
  }
  status:i, response_head:app.Header, response_body:s = app.Run(head, body)
  return buf.write("HTTP/1.1\nStatus: {status}\n{response_head.join("{k|ascii}: {v|ascii}\n")}\n\n{http_body}")
}
# app.vl
Header = [{key:s value:s}](index=key)
Run(head:Header, body) = {
  # user application implements
}



_______________________________________________________________________
#!/usr/bin/vl -lang=perl

trace(msg:String ...) = os.stdout("(" time_delta() ")" msg)
time_delta():String = {
	delta = os.time() - os.time.start_at
	day   = int(delta / 24 / 60 / 60)
	hour  = (delta / 60 / 60) % 24
	min   = (delta / 60) % 60
	sec   = delta % 60
	return(
		day  > 0 ? "(%d)%02d:%02d:%02d".format(day hour min sec)
		hour > 0 ? "%02d:%02d".format(hour min sec)
		min  > 0 ? "%02d:%02d".format(min sec)
		_        ? "%02d".format(sec)
	)
}
crawl = {
	count      = 0
	retry      = 5
	wait_limit = 600
	timeout    = 60
	ua         : lib.use("LWP::UserAgent").new() # readonly
	status     : 200 # readonly
	head       : ""  # readonly
	body       : ""  # readonly
	(num:Int)  = crawl(retry=num)
	get(url:String) = {
		ua.timeout(timeout)
		++count
		trace("get({count}) {url}")
		1 .. retry : try {
			rand_wait = 1 + os.rand(10)
			trace("random_wait({rand_wait})")
			os.sleep(wait)

			r = ua.get(url)
			status = r.code()
			head   = r.header()
			body   = r.decoded_content()

			r.is_success()        ? return(T)
			r.code.left(1) == "4" ? trace("not found {r.status_line}") try.last()

			_wait = 2 ** try
			wait  = _wait > wait_limit ? wait_limit : _wait
			trace("retry({try}) wait({wait}) reason({r.status_line})")
			os.sleep(wait)
		}
		return(F)
	}
	store(url:String path:String) = {
		file = os.path.file(path)
		file.exists ? return file.read()
		return get(url) && file.write(content)
	}
}
run() = {
	dir  = 0
	book = [:String spec=uniq]
	env.data.readlines(:line) {
		group_name = line.chomp()
		group_name == "" ? next()
		++dir
		url = "http://www.example.com/{group_name}/index.html"
		os.mkdir(dir)
		html = getstore(url "{dir}/index.html") || next()
		html.match(r!href="(/contents/{group_name}/)(index.html)"!g : dir file) {
			book_base = "http://www.example.com{dir}"
			book_url  = "{book_base}{file}"
			book.set(book_url) ? next()
			book_html = getstore(book_url "{dir}/{book.count}/index.html") || next()
			book_html.match(r!<a href="(\d+)\.html">\d+</a>! : num) {
				jpg_url = "{book_base}img/{num}.jpg"
				getstore(jpg_url "{dir}/{book.count}/{num}.jpg")
			}
		}
	}
}
main() = {
	1 .. 10 {
		ret = os.eval() {
			run()
		}
		ret ? return()
	}
}
