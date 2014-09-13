# Myopia, a µ-recursive programming language

*Myopia* is a programming language based on [µ-recursive functions][murec].
It's an entry for [PLT Games December 2012][pltgames] competition "Into the
Turing Tarpit".

## Installation and usage

    git clone git://github.com/miikka/myopia.git
    cd myopia
    cabal sandbox init
    cabal install
    myopia --help
    myopia example.myop fib
    myopia example.myop hw --io

## How does it work?

Myopia deals with functions from tuples of natural numbers to natural numbers (N^n
-> N). The functions are constructed by composing the following primitives:

* `Z`, the zero function.
* `S`, the successor function.
* `I[i,k]`, the family of identity functions.
* `C`, the composition operator.
* `P`, the primitive recursive operator.
* `M`, the minimisation operator.

The programs consist of function definitions. Optionally you can also declare
the type of the function.

    -- this is a comment
    name = body

    -- Here's is the unary identity function with a type signature
    id : N -> N
    id = I[1,1]

    -- plus2(x) = x + 2. Here's how you compose functions:
    plus2 : N -> N
    plus2 = C(S, S)

You can use the functions you have declared in other functions. It's equivalent
to replacing the function name with function body. Note that if you call
functions recursively, bad things will happen. Better settle for `P`.

    -- plus(x,y) = x + y
    plus : N x N -> N
    plus = P(id, C(S, I[2,3]))

    -- Don't do this, it won't even typecheck:
    -- bottom = bottom

Use C to compose the functions.

    -- You can use C to duplicate parameters, like this:
    double : N -> N
    double = C(plus, id, id)

## Is it Turing-complete?

µ-recursive functions are Turing-complete, so yes. Also, if you do not use `M`
operator in your program, your functions are primitive recursive and guaranteed
to halt.

It's does not look like the usual Turing tarpits, though. Still, constructing
for example the function for n-th Fibonacci number takes some work, as seen
  below.

## Example programs, please!

Multiplication:

    mult = P(Z, C(plus, I[2,3], I[3,3]))

Using multiplication, we can calculate factorials.

    c1 = C(S, Z)

    fac : N -> N
    fac = C(P(c1, C(mult, I[2,3], C(S, I[1,3]))), id, Z)

Calculating n-th Fibonacci number is the Hello World equivalent of the
functional programming world, so let's do that.

Because the n-th Fibonacci number is defined in terms of the two previous
numbers, primitive recursion is not directly applicable. We need a way to pass
around two numbers at the same time. This can be done by encoding the numbers
as a powers of primes: `pair(x,y) = 2^x 3^y`.

    -- exp(x,y) = x^y
    exp : N x N -> N
    exp = C(P(c1, C(mult, I[2,3], I[3,3])), I[2,2], I[1,2])

    -- pair(x,y) = 2^x 3^y
    pair : N x N -> N
    pair = C(mult, C(exp, C(c2, I[1,2]), I[1,2]), C(exp, C(c3, I[2,2]), I[2,2]))

We also need a way to extract the elements of the pair. This can be done with
the function `lo` defined below.  For a proper explanation of how these
functions are derived, see [examples][pm-examples] and
[more examples of primitive recursive functions][pm-more] on 
PlanetMath.

    -- rem(x,y) is the remainder of the integer division x/y.
    rem = P(Z, C(mult,C(sgn, I[3,3]), C(mult, C(S, I[2,3]), C(sgn, C(diff, C(S, I[2,3]), I[3,3])))))

    -- div(x,y)=1 if y divides x, otherwise div(x,y)=0.
    div = C(minus, C(c1, I[1,2]), C(sgn, C(rem, I[1,2], I[2,2])))

    -- if lo(x,y)=z, z is the largest number for which x^z divides y.
    lo : N x N -> N
    lo = M(C(and, C(div, I[3,3], C(exp, I[2,3], I[1,3])), C(div, I[3,3], C(exp, I[2,3], C(S, I[1,3])))))

    -- projection functions for pairs. p1(pair(x,y)) = x, p2(pair(x,y)) = y.
    p1 = C(lo, c2, id)
    p2 = C(lo, c3, id)

Finally we can the define the Fibonacci function:

    c6 = C(double, c3)

    -- fib(n) calculates the n-th Fibonacci number
    fib = C(p1, C(P(c6, C(pair, C(p2, I[2,3]), C(plus, C(p2, I[2,3]), C(p1, I[2,3])))), id, id))

Beware: this is very, very slow. Implementing the closed-form solution is left
as an exercise for the reader.

[pm-examples]: http://planetmath.org/?op=getobj&id=11973&from=objects
[pm-more]: http://planetmath.org/encyclopedia/MoreExamplesOfPrimitiveRecursiveFunctions.html


## Input and output

Myopia has an IO mode, which can be enabled with the switch `--io`. It can be
used with functions of type `N -> N`. The result of function `f(n)` is
considered to be the n-th output byte. You also get access to primitive
function `ioChar : N -> N`, which returns you the n-th input byte.

For example, copying input to output can be implemented like this:

    cat : N -> N
    cat = ioChar

Implementing interactive programs might be tricky, because there are no
guarantees for evaluation order.

## Reading material

These two pages show how many basic functions can be implemented using
primitive recursion and bounded minimalisation. (Myopia does not have a
primitive for bounded minimalisation, but it can be implemented in terms of
primitive recursion or minimalisation.)

* Chi Woo: [examples of primitive recursive functions][pm-examples]. PlanetMath.org
* Chi Woo: [more examples of primitive recursive functions][pm-more]. PlanetMath.org

## Further work

I have a feeling that by golfing around the complexity of the language could be
decreased. Some random ideas to think about:

* Could M and P operators be combined?
* According to Kleene's normal form theorem, all µ-recursive functions can be
  written using two primitive recursive functions g, h in the form g(µh(...),
  ...). Could the language be based on this?
* Could the amount of I operators be limited?

Feature ideas:

* Type inference for `I[i,k]` operators so you don't need to explicitly mention
  `k` (or even `i` in the case of `I[1,1]`).
* Unicode syntax. Who wouldn't love `plus : א → א × א`?

[pltgames]: http://www.pltgames.com/competition/2012/12
[murec]: http://en.wikipedia.org/wiki/Μ-recursive_function
