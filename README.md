Scala-like placeholder syntax for CoffeeScript.
===============================================

This compiler extension adds Scala-like placeholder syntax to the CoffeeScript compiler.

Expression contains unbound variables named “_” are converted to anonymous bound function.

For example, `array.foldLeft(_ + _)` is converted to `array.foldLeft((a, b) => a + b)`.

Note that bound variables are not placeholders, so that `(_) -> _` remains `(_) -> _`.  Your existing code using `_` as a variable should not be affected.

Getting Started
---------------

Get extension:

    git clone https://github.com/taku0/placeholder_syntax_for_coffeescript.git

Run compiler with --require option:

    cd placeholder_syntax_for_coffeescript
    coffee --require ./src/placeholder_ext.coffee -ep '_ + _'

If you would get something like the following, the extension works:

    (function() {
      var __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };
      __bind(function(_a, _b) {
        return _a + _b;
      }, this);
    }).call(this);

Scopes of Placeholders
----------------------

Is `foo(_ + _, _)` converted to `(a, b, c) => foo(a + b, c)` or `(a) => foo(((b, c) => b + c), a)`?  The answer is `(a) => foo(((b, c) => b + c), a)`, according to the definition of scopes described in this section.

Scopes are expressions converted to the body of the generated anonymous functions.  A scope of a placeholder is defined as the smallest expression which satisfies all of the following conditions:

* It is not a placeholder itself.
* It contains placeholders.
* It is one of followings:
  * An argument of a function call.
  * A parenthetical expression.

Examples:

    console.log(_) == (a) => console.log a
    array.foldLeft(_ + _) == array.foldLeft((a, b) => a + b)
    (x = _ + _) == ((a, b) => x = a + b)
    (x = (_ + _)) == (x = ((a, b) => a + b))

Note the rules are much looser than the rules adopted in Scala.

Special treatment for method definition
---------------------------------------

Bound methods can be defined with placeholders.

Example:

    class Foo
      constructor: (@foo) ->
      bar: (@foo + _)
      # is equivalent to
      #
      #   bar: (a) => @foo + a
      #
      # Note @ referes to an instance of Foo, not Foo.

Notes on postfix form
---------------------

Since current CoffeeScript compiler does not distinguish postfix form from normal form, parameter order is not intuitive:

    (_ if _) == ((a, b) => b if a)
    (_ while _) == ((a, b) => b while a)
    (_ for x in _) == ((a, b) => b for x in a)

This should be fixed on some future occasion.

Notes on scope of bound variables
---------------------------------

Since variable scope in CoffeeScript (and JavaScript) is entire function, all variables in following code are bound, so that they are not placeholders:

    ->
      x = (not _)
      y = [_, _ = 1, _]
      z = (_ + _)
  
      # x is true, y is [1, 1, 1], z is 2

Notes on interpolations
-----------------------

`"abc#{_ + _}def"` is converted to `"abc" + (_ + _) + "def"` by lexer, so that this is converted to `"abc" + ((a, b) -> a + b) + "def"`.  This should be fixed on some future occasion.
