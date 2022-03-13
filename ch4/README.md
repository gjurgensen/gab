At the moment, our language is entirely expression-based. Let's add some statements, including variable bindings and ADTs. For both of these constructs, we are just going to be taking baby steps. We start with their most basic versions.

We can also now remove our primitive booleans, since these can be represented by an ADT.

# Grammar

Our grammar is getting very busy. I'll now use single quotes to represent strings to match, x* to represent 0 or more instances of x, and x+ to represent 1 or more instances of x.

It is also necessary now to differentiate between variable identifiers, which we will say begin with a lowercase character, and constructor identifiers, which begin with an uppercase character.

```
PROGRAM ::= (STMT ';')*
STMT ::= def VAR VAR* ':=' TERM
       | data VAR ':=' VAR TYPE* ('|' VAR TYPE*)*

TERM ::= VAR | CONSTR | 'λ' VAR '.' TERM | 'fix' TERM | TERM TERM
       | 'case' TERM 'of' CONSTR VAR* '=>' TERM ('|' CONSTR VAR* '=>' TERM)* 'end'

VAR    ::= [a-z]...
CONSTR ::= [A-Z]...

TYPE ::= VAR | TYPE -> TYPE
```

Note that we may define function bindings such as `def id := fn x. x;`, but we also provide the alternate sugared alternative `def id x := x;`.

# Typing

Before considering ADTs and constructors, we must address the introduction of `def` statements. These are perhaps subtly disruptive. Consider the statement `def id x := x;`. We might give it the type `?0 -> ?0`, as we did in the last chapter. But this is in a sense monomorphic, despite the appearance of flexibility. If you later include `id True`, this will lead to the `id` type being constrained to `Bool -> Bool`, and it will now no longer accomodate any other instantiation. For instance, `id not` would require `id` to have type `(Bool -> Bool) -> Bool -> Bool`, which is incompatible with the earlier concrete type.

We solve this by introducing proper polymorphism. Instead of type `?0 -> ?0`, `id` will have type `∀ 'a. 'a -> 'a`. This will have the same behavior, but it may be repeatedly instantiated with different variables `'a` without disruption.

Typing of constructors is suprisingly straightforward. Given a declaration `data A := ... | B x y ... z | ...` we give constructor `B` the type `x -> y -> ... -> z -> A`. Note that we do not currently allow the ADT to take type arguments, although it may be recursive.

Typing of the `case` construct is less friendly. We must ensure that each pattern is compatible (unifiable) with the scrutinee. Then the type is the shared type of each arm of, typed in a context extended with the variables appearing in their respective patterns.

# Semantics

The `case` construct works by unification, which motivates our abstraction of unification away from types and into a generic process over unification trees. We attempt to unify the normalized scrutinee with each pattern. The first match is accepted and we enter this arm. We do not at the moment check for totality of patterns, so it is possible that no pattern matches even in a well-typed case expression, and that our resulting program will diverge.

# Running the interpreter

We add a "load" command to the REPL to process gab files. Try `:l test/prelude.gab` to load in some definitions!