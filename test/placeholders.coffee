# Placeholder Syntax
# ------------------

# * Array with Placeholders
# * Property Access of Placeholders
# * Scope of Variables
# * Assignment with Placeholders
# * Indexing with Placeholders
# * Class with Placeholders
# * Comprehensions with Placeholders
# * If with Placeholders
# * While with Placeholders
# * Switch with Placeholders
# * Throw with Placeholders
# * Try/Catch/Finally with Placeholders
# * Function Call with Placeholders
# * Funciton Definition with Placeholders
# * Interpolation with Placeholders
# * JavaScript Literal with Placeholders
# * Object with Placeholders
# * Operator with Placeholders
# * Range with Placeholders
# * Slicing with Placeholders
# * Splicing with Placeholders
# * Parenthetical Expression with Placeholders
# * Return with Placeholders

test "placeholders in simple array", ->
  f = ([_, _ + _, _])
  array = f(1, 2, 3, 4)

  ok array[0] is 1
  ok array[1] is 5
  ok array[2] is 4

test "placeholders in array with splat", ->
  f = ([_, _..., _])
  array = f(0, [1, 2, 3], 10)

  arrayEq [0, 1, 2, 3, 10], array

test "property subject should expanded", ->
  f = (_.x = 1)
  obj = {}
  f(obj)

  ok obj.x is 1

test "property name of this access should not expanded", ->
  f = (x) ->
    @_ = x

  obj = {}

  f.call(obj, 1)

  ok obj['_'] is 1

test "property name should not expanded", ->
  x = {}
  x._ = 2
  f = ->
  f::_ = 2

  ok x['_'] is 2
  ok f::['_'] is 2

test "bound variable should not expanded", ->
  (_ = 1)

  ok _ is 1

test "scope of variables is entire function", ->
  x = (not _)
  y = ([_, _ = 1, _])
  z = (_ + _)

  ok x
  arrayEq [undefined, 1, 1], y
  ok z is 2

test "catch clause introduce parameter", ->
  x = try
        throw 1
      catch _
        (_ + _)
  ok x is 2

test "lhs of an assignment may have expression", ->
  obj = {}
  ((_.a)({a:obj})).x = 1

  ok obj.x is 1

test "destructuring assignment: simple array", ->
  [_] = [1]
  x = (_ + _)

  ok x is 2

test "destructuring assignment: array with splat", ->
  [_...] = [1]
  x = (_[0] + _[0])

  ok x is 2

test "destructuring assignment: array in array", ->
  [[_]] = [[1]]
  x = (_ + _)

  ok x is 2

test "destructuring assignment: object in array, shorthand", ->
  [{_}] = [{_:1}]
  x = (_ + _)

  ok x is 2

test "destructuring assignment: object in array", ->
  [{x:_}] = [{x:1}]
  x = (_ + _)

  ok x is 2

test "destructuring assignment: array in object in array", ->
  [{x:[_]}] = [{x:[1]}]
  x = (_ + _)

  ok x is 2

test "indexing", ->
  obj = {x: {x: 1}}
  f = (_['x']['x'])
  g = (obj[_])

  ok f(obj) is 1
  ok g('x')['x'] is 1

test "class name can contain placeholder", ->
  obj = {}
  (class _.Foo
     foo: 1)(obj)

  ok (new obj.Foo).foo is 1

test "class name itself cannot be a placeholder", ->
  class _
    foo: 1
  ok (new _).foo is 1

test "super class can be placeholder", ->
  class Base
    foo: 1

  sub = (class Sub extends _)(Base)

  ok (new sub).foo is 1

test "class body has own scope", ->
  class Foo
    _ = 1
  new Foo
  f = (_ + _)
  ok f(1, 2) is 3

test "placeholders in class body", ->
  foo = (class Foo
           @x = _)(1)
  ok foo.x is 1

test "placeholders in method body", ->
  foo = (class Foo
           foo: -> _)(1)
  ok (new foo).foo() is 1

test "placeholders in constructor body", ->
  foo = (class Foo
           constructor: -> @foo = _)(1)
  ok (new foo).foo is 1

test "constructor can be placeholders", ->
  foo = (class Foo
           constructor: _)(-> @foo = 1)
  ok (new foo).foo is 1

test "defining bound method with placeholders", ->
  class Foo
    plus: (_ + _)
    getProperty: (2; @[_]) # @ should be bound to instance of Foo, not Foo.
    foo: 1

  ok (new Foo()).plus(1, 2) is 3
  ok (new Foo()).getProperty('foo') is 1

test "placeholders with extends", ->
  class Super
    foo: -> 1

  Sub = ->
  (_ extends _)(Sub, Super)

  Sub::foo = -> super + 2

  ok (new Sub).foo() is 3

test "placeholders with new", ->
  class Foo
    constructor: (@foo) ->
    bar: -> @foo

  ok (new _ _)(Foo, 1).bar() is 1

test "array comprehensions with placeholders", ->
  array = (x + i + _ for x, i in _)(2, [3, 2, 1])
  arrayEq [5, 5, 5], array

test "array comprehensions with placeholders and postfixed body", ->
  # Note: this is ((a, b) => for x, i in b then x + i + a),
  #       not ((a, b) => for x, i in a then x + i + b),
  #       since AST don't distinguish postfix form from normal form.
  array = (for x, i in _ then x + i + _)(2, [3, 2, 1])
  arrayEq [5, 5, 5], array


test "array comprehensions with placeholders and guard", ->
  array = (x + i for x, i in [3, 2, 1] when x % _ == 0)(2)
  arrayEq [3], array

test "array comprehensions declare variable, value", ->
  array = ((_ + i) for _, i in [3, 2, 1])
  arrayEq [3, 3, 3], array
  ok (_ + _) is 2

test "array comprehensions do not introduce scopes", ->
  array = (_ = x for x, i in [3, 2, 1])
  ok (_ + _) is 2

test "array comprehensions declare variable, index", ->
  array = ((x + _) for x, _ in [3, 2, 1])
  arrayEq [3, 3, 3], array
  ok (_ + _) is 6

test "object comprehensions with placeholders", ->
  array = (prop + value for prop, value of _)({x: 'X'})
  arrayEq ['xX'], array

test "object comprehensions declare variable, property name", ->
  array = ((_ + i) for _, i of {x: 'X'})
  arrayEq ['xX'], array
  ok (_ + _) is 'xx'

test "object comprehensions declare variable, value", ->
  array = ((x + _) for x, _ of {x: 'X'})
  arrayEq ['xX'], array
  ok (_ + _) is 'XX'

test "range comprehensions with placeholders", ->
  array = (x + 1 for x in _)([1..10])
  arrayEq (x + 1 for x in [1..10]), array

test "range comprehensions with placeholders and step", ->
  array = (x + 1 for x in [1..10] by _)(2)
  arrayEq (x + 1 for x in [1..10] by 2), array

test "range comprehensions declare variable", ->
  array = (_ + _ for _ in [1..10])
  arrayEq (x + x for x in [1..10]), array
  ok (_ + _) is 22

test "if with placeholders", ->
  f = (if _ then _ else _)

  ok f(true, 1, 2) is 1
  ok f(false, 1, 2) is 2

test "postfix if with placeholders", ->
  # Note: this is ((x, y) => y if x), not ((x, y) => x if y),
  #       since AST don't distinguish postfix form from normal form.
  f = (_ if _)

  ok f(true, 1) is 1

test "while with placeholders", ->
  i = 0
  (while i < _ then i += _)(3, 1)

  ok i is 3
test "postfix while with placeholders", ->
  # Note: this is ((a, b) => i += b while i < a),
  #       not (a, b) => i += a while i < b),
  #       since AST don't distinguish postfix form from normal form.

  i = 0
  (i += _ while i < _)(3, 1)

  ok i is 3

test "switch with placeholders", ->
  f = (switch _
       when _ then _
       when _, _ then _
       else _)

  ok f(1, 1, 2, 3, 4, 5, 6) is 2
  ok f(2, 1, 2, 3, 4, 5, 6) is 6
  ok f(3, 1, 2, 3, 4, 5, 6) is 5
  ok f(4, 1, 2, 3, 4, 5, 6) is 5

test "switch with placeholders but subject", ->
  f = (switch
       when _ then _
       when _, _ then _
       else _)

  ok f(true, 1, true, true, 2, 3) is 1
  ok f(false, 1, true, true, 2, 3) is 2
  ok f(false, 1, false, true, 2, 3) is 2
  ok f(false, 1, false, false, 2, 3) is 3


test "throw with placeholders", ->
  f = (throw _)

  throws (-> f "a"), "a"

test "try catch finally with placeholders", ->
  f = (try
         _()
       catch e
         _
       finally
         _())

  x = 0
  y = f((-> 1), 2, (-> x = 3))
  ok x is 3
  ok y is 1

  x = 0
  y = f((-> throw 1), 2, (-> x = 3))
  ok x is 3
  ok y = 2

test "function call delimits scope of placeholders", ->
  f = (x) -> x

  g = f _ + _

  ok g(1, 2) is 3

test "function call with splat and placeholders", ->
  f = (x, y, z) -> x + y + z
  g = (f _...)
  ok g([1, 2, 3]) is 6

test "function parameters defines variable", ->
  f = (_) -> _ + _

  ok f(1) is 2

test "default value and placeholders", ->
  f = (g = (_ + _)) -> g(1, 2)

  ok f() is 3

test "code has own scope", ->
  (-> _ = 1)()
  f = (_ + _)

  ok f(1, 2) is 3


test "string interpolation with placeholders", ->
  # converted to "abc" + (_ + _) + "def" by lexer,
  # so that this is "abc" + ((a, b) -> a + b) + "def".
  f = "abc#{_ + _}def"

  ok typeof f is 'string'

test "regex interpolation with placeholders", ->
  # converted to Regexp("abc" + (_ + _) + "def") by lexer,
  # so that this is Regexp("abc" + ((a, b) -> a + b) + "def").
  f = ///abc#{_ + _}def///

  ok f.exec

test "placeholders are ignored in JavaScript literal", ->
  ok (CoffeeScript.compile "`_ + _`", bare: yes) is "_ + _;"

test "object with placeholders", ->
  f = (
    x: _
    y: {x: _}
    _: _
  )

  object = f(1, 2, 3)

  ok object.x is 1
  ok object.y.x is 2
  ok object._ is 3

test "shorthand object with placeholders", ->
  f = {_} # (_a) -> {_a: _a}
  # FIXME: should be (_a) -> {_: _a} ?

  object = f(1)

  ok not(object._)

test "operators with placeholders", ->
  ok (-_)(1) is -1
  ok (_ + _)(1, 2) is 3
  ok (_ instanceof _)({}, Object)
  ok (_ in _)(1, [1, 2, 3])
  ok (_ of _)('x', {x: 1})
  ok (_?)(1)
  ok (_ ? _)(1, 2) is 1
  ok (_ < _ < _)(1, 2, 3)

test "ranges with placeholders", ->
  array = ([_+_..._*_])(1, 2, 3, 4)

  arrayEq [3...12], array

test "slicing with placeholders", ->
  array = [1, 2, 3]

  f = (array[_.._])
  g = (array[_..])
  h = (array[.._])

  arrayEq [2, 3], f(1, 2)
  arrayEq [2, 3], g(1)
  arrayEq [1, 2], h(1)


test "splicing with placeholders", ->
  f = (array = [1, 2, 3]; array[_.._] = []; array)
  g = (array = [1, 2, 3]; array[_..] = []; array)
  h = (array = [1, 2, 3]; array[.._] = []; array)

  arrayEq [1], f(1, 2)
  arrayEq [1], g(1)
  arrayEq [3], h(1)

test "identify function by placeholder", ->
  ok ((_))(1) is 1

test "explicit return with placeholders", ->
  ok (-> return _)(1)() is 1
