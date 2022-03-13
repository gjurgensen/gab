Let's create a cutting-edge[^ce] language! It is going to be a pure[^pure] dependently typed function language.

[^ce]: Only cutting-edge feature-wise. We won't pursue optimal performance.

[^pure]: We will eventualy add an IO monad for specifying effects. Our language however will still be pure in the usual sense in that terms will be referentially transparent.

# Design Goals

Ultimately, we want a featureful language with strong dependent types and IO and custom notation and all sorts of cool stuff. But as with any other software project, it is always a good idea to start small and get an end-to-end working prototyping before expanding.

We are going to start with an untyped lambda calculus. We'll implement just parsing and interpretation.

# Grammar

We won't be excessively formal. We are roughly aiming for the following grammar:

```
TERM ::= VAR | λ VAR. TERM | TERM TERM
VAR  ::= ...
```

As usual, function application associates leftward and the bodies of lambdas extend as far as possible. Arbitrary parenthesization is permitted.

Our grammar shows the core language, but we'll also add some syntactic sugar for convenience. Instead of using the unicode `λ`, you can use the ascii keyword `fn`. We also allow an abbreviation for nested functions, e.g. allowing `λ x y. z` to parse to `λ x. λ y. z`.

# Semantics

We'll implement a call-by-value semantics. I.e. arguments to functions are fully evaluated before function application.

# Running the Interpreter

Running the interpreter (with `stack run`) will put you into a REPL, where you can evaluate concrete lambda calculus expressions.