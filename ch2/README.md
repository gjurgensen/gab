Next, we are going to add types. But we only have one sort of thing at the moment. Type-checking a unityped language is about the dullest thing I can imagine, so let's add booleans to our language.

We wish to avoid type inference for now, so we will require explicit type annotations in our lambda binders.

Introducing types will actually kill the Turing-completeness we had, since our old fixpoint combinator for recursion is ill-typed. We solve this by simply adding a primitive fixpoint operator.

Oh, and while we're making improvements, let's throw in a decent pretty printer instead of printing ASTs.

# Grammar

```
TERM ::= true | false | VAR | λ VAR : TYPE. TERM | fix TERM | TERM TERM | if TERM then TERM else TERM
VAR  ::= ...

TYPE ::= Bool | TYPE -> TYPE
```

Function application associates rightward.

Multi-variable lambda require parentheses around each variable-type pair. I.e. `λ(x: t)(y: t'). z`.

# Typing

Again, we won't bother specifying things formally. If you'd like an introduction to formal type theory, I'd highly recommend Pierce's *Types and Programming Languages*.

The types here are straightforward. `true` and `false` obviously have type `Bool`. The type of a variable is given at its binding cite. `fix t` has type `A` if `t` has type `A -> A`. `if c then t else e` has type `A` if `c` has type `Bool`, and both `t` and `e` are of type `A`.

# Running the Interpreter

We add commands to the REPL. Type `:h` to see them!