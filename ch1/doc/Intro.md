Let's create a cutting-edge[^ce] language! It is going to be a pure dependently typed function language.

[^ce]: Only cutting-edge feature-wise. We won't purse optimal performance.

# Design Goals

Ultimately, we want a featureful language with strong dependent types and IO and custom notation and all sorts of cool stuff. But as with any other software project, it is always a good idea to start small and get an end-to-end working prototyping before expanding.

We are going to start with an untyped lambda calculus. We'll implement just parsing and interpretation.

# Grammar

```
term ::= var | λ var. term  | term term
var  ::= ...
```

We'll also add some syntactic sugar, allowing `λ x y. z` to parse to `λ x. λ y. z`.

# Semantics

We'll implement a call-by-value semantics. I.e. arguments to functions are fully evaluated before function application.