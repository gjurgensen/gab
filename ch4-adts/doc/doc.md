At the moment, our language is entirely expression-based. Let's add some statements, including variable bindings and ADTs. For both of these constructs, we are just going to be taking baby steps. We start with their most basic versions.

We can also now remove our primitive booleans, since these can be represented by an ADT.

# Grammar

Our grammar is getting very busy. I'll now use single quotes to represent strings to match, x* to represent 0 or more instances of x, and x+ to represent 1 or more instances of x.

It is also necessary now to differentiate between variable identifiers, which we will say begin with a lowercase character, and constructor identifiers, which begin with an uppercase character.

```
PROGRAM ::= (STMT ';')*
STMT ::= VAR ':=' TERM
       | data VAR ':=' VAR TYPE* ('|' VAR TYPE*)*

TERM ::= VAR | CONSTR | 'λ' VAR '.' TERM | 'fix' TERM | TERM TERM
       | 'case' TERM 'of' CONSTR VAR* '=>' TERM ('|' CONSTR VAR* '=>' TERM)* 'end'

VAR    ::= [a-z]...
CONSTR ::= [A-Z]...

TYPE ::= VAR | TYPE -> TYPE
```
