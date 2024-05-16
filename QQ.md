see also [[FlatLike]] [[ZMLike]] [[TopLike]]


# Equivalences 
Value = Pattern = Type = Function 

bool = false | true

- value: either false or true
- pattern: 
- type: the data type bool = false | true
- a function that returns false | true
    in verse a type is a function that applied to a value either fails (if the argument is not the right type) or return the value, bit like a filter
    
Operators: 
-- or, alternative 
|

-- and, unification
& 

-- variable introduction
(a) a+1 

-- type declaration
val : type

-- serialised data
($..hex..$) : type 

(++) append binary sequences

#11 ++ #011

#0# : Bool ~~ false

-- subtypes/filters
> >= == = <>
>= ~~ > | = 
<> ~~ < | >  
><

.. other combinations?

a |& b ~~ (a | b) | (a&b) ~~ true 

-- ordering of values 
succ +1 prec -1 



## non-parametric types

bool = false | true

## parametric types (variable introduction)

list = \a -> cons a (list a) | nil 

## symbolyc constructors

[a]  = \a -> h : t | []

## context manupulation functions

-- import from remote module

import url

-- push to context

def definitions

-- evaluate expression with expanded context
with : definitions -> exp -> r

## pattern matching functions

Pattern matching is broken into branches that can be combined with | as usual:

Type classes as open ended, value/type pattern matching:

Most recently added branches have precedence

Every name is implicitly set to the value = fail "<name> is undefined"

add |= (a:string,b:string) -> append a b

add |= (0:int,b:int) -> b

add |= (0:int,b:int) -> b

add |= (a:int,b:int) -> plus a b

# Type classes are type function + context expanding

list a = cons a (list a) | nil

-- type class is a type
monoid a = {
      zero : a
    , plus : a -> a -> a
}

-- with defaults 
signedNumber a = {
    plus : a -> a -> a
    
    minus : a -> a -> a = \a b -> plus a (negate b)

    negate : a -> a 
}

signedNumber int = {
    plus = plusInt
    negate = negateInt
}

-- type class instance and definition?
monoid int = {
      zero = 0
    , plus = add 
} : monoid int

-- cue style
-- defined the general pattern, that specifies only types
monoid a = {
      zero : a
    , plus : a -> a -> a
}

-- and merge with the specific one, it will fail if types do not unify
def $ monoid int & {
      zero = 0
    , plus = add 
}

-- equivalent to:

zero |= _:int -> 0

plus |= a:int -> b:int -> a+b

def $ monoid string

with (monoid int) (zero + zero) 

zero _:int = 0

plus a:int b:int = a+b 

zero (monoid int)

branch1 | branch2

if branch1 fails then try branch2 in sequential order

and works not just on values but also on types (and kinds?)

and maybe can perform any check because they are just tests

?verse: test either fails or return its left operand:
x > 4 == x | fail

Patterns/Functions:
  int i ->  "int!"   -- Fails if `i` not an `int`    
| s:string -> "str!" 
| o -> type o

i=2 -> 

Functional patterns 
odd i -> "odd"
even i -> "even"

-- one or more 'a's
many a = cons a (nil | many a)

[a] [a a] [a a a] ..

-- zero or more 
some a = nil | cons a (some a)

-- starts and ends with `a`
startEnd a = cons a (
    len n = cons a nil | cons _ n)
)

-- These can be interpreted as
-- generators of values / functions

generate$ startEnd a 
[aa] | [a_a] | [a__a]

-- or as types
verify$ [a] : startEnd a
false

verify$ [a] : many a
true

## A context is a term with a hole in it

## Functions == Values == Types

## Unification

## Sequences of operations

a;b;c
~~
a
b
c

## No Magic

Minimum language, all functionality provided by possibly effectful interfaces.

Virtual-Machine just another interface.

data State s o where
    Get :: State s
    Put : s -> State ()

aState Get = case q of
    Get 

## Transparent 

As in Forth, the execution mechanism (stack,mem, etc) is available for inspection. 

## Self describing format

``quid2

-- Let me show you what a self-describing format looks like.
-- we define a model
-- 

bit = v0 | v1

bool = false | true

list = \a -> cons a (list a) | nil 

-- a decoding function
unflat v0 = false
unflat v1 = true

unflat : 
unflat [#0] = nil
unflat (#1:t) = cons (unflat t)  

flat false = [#0]
flat true = [#1]

flat nil = [#0]
flat (cons h t) = #1 ++ flat h ++ flat t 
``

## Meta-level.
Enough power to encode a generic flat/unflat function

## As a document format

This whole web site is a quid2 value.

