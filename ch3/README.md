The explicit type annotations on function arguments is, well, annoying. For one, they are inflexible. In our untyped lambda calculus, `λ x. x` served as the identity function on all types. But now, we are forced to give it a singular concrete type. Monomorphic frustrations aside, it is just a hassle writing all of these annotations. As I'm sure you have seen in other languages, most types can be inferred.

# Typing

We implement inferred typing via a proces of unification. `?n` represents a unification variable, where `n` is a natural number.
When we unify two or more types, we attempt to find an instantiation of unification variables which makes the types equal. For instance, unifying the types `Bool -> ?0` with `?1 -> Bool -> Bool` yields the solution `?0 ↦ (Bool -> Bool), ?1 ↦ Bool`.

To infer types, we give functions the type `?i -> ?j`, where `i` and `j` are fresh variables. We then continue to refine via unification as we go forward. For instance, if the function is applied to something of type `?k`, then we would discover the constraint that `?i = ?k`.