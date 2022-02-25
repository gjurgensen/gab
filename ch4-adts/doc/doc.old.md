At the moment, our language is entirely expression-based. We'd like to make room for statements, including variable bindings and type bindings.

We'll also greatly expand our types. Eventually, we'd like full ADT-style inductive types and pattern matching, but that is pretty complex. For now, 

We can also now remove our primitive booleans, since these can be represented by an ADT.

# Grammar

Our grammar is getting very busy. I'll now use single quotes to represent strings to match, x* to represent 0 or more instances of x, and x+ to represent 1 or more instances of x.

It is also necessary now to differentiate between variable identifiers, which we will say begin with a lowercase character, and constructor identifiers, which begin with an uppercase character.

```
PROGRAM ::= (STMT ';')*
STMT ::= VAR ':=' TERM
       | data VAR VAR* ':=' VAR TYPE* ('|' VAR TYPE*)*

TERM ::= VAR | CONSTR | 'λ' VAR '.' TERM | 'fix' TERM | TERM TERM
       | 'case' TERM 'of' CONSTR VAR* '=>' TERM ('|' CONSTR VAR* '=>' TERM)* 'end'

VAR    ::= [a-z]...
CONSTR ::= [A-Z]...

TYPE ::= VAR | TYPE -> TYPE
```
