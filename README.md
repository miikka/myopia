# Myopia, a µ-recursive programming language

*Myopia* is a programming language based on [µ-recursive functions][murec].
It's an entry for [PLT Games December 2012][pltgames] competition "Into the
Turing Tarpit".

## How does it work?

Myopia deals with functions from tuples of natural numbers to natural numbers (N^n
-> N). The functions are constructed by composing the following primitives:

* `Z`, the zero function.
* `S`, the successor function.
* `I[i,k]`, the family of identity functions.
* `C`, the composition operator.
* `P`, the primitive reursive operator.
* `M`, the minimisation operator.

The programs consists of function definitions. Optionally you can also declare
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

    fac : N -> N
    fac = C(P(c1, C(mult, I[2,3], C(S, I[1,3]))), id, Z)

*TODO* construct fib.

## Further work

I have a feeling that by golfing around the complexity of the language could be
decreased. Some random ideas to think about:

* Could M and P operators be combined?
* According to Kleene's normal form theorem, all µ-recursive functions can be
  written using two primitive recursive functions g, h in the form g(µh(...),
  ...). Could the language be based on this?
* Could the amount of I operators be limited?


[pltgames]: http://www.pltgames.com/competition/2012/12
[murec]: http://en.wikipedia.org/wiki/Μ-recursive_function
