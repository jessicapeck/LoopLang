# LoopLang Syntax

## Stitch types
Implemented stitch types include:
- chain: `ch`
- single crochet: `sc`
- double crochet: `dc`
- increase stitch: `inc`
- decrease stitch: `dec`

## Row identifiers
`r1: ...`
`R1: ...`
`row 1: ...`
`ROW 1: ...`
- the start of a new row can be indicated using any of the above forms
- a space between the row identifier and the row number is optional for all forms when the row number is a literal
- when the row number is a variable, if the variable is in parentheses then the space is optional, otherwise the space is mandatory

## Multipliers
`R1: sc 3, inc, sc 3` 
`R1: 3 sc, inc, 3 sc` 
- multiple consecutive stitches can be denoted by specifying the stitch type and then the number of consecutive stitches of that type or vice versa
- a lack of stitch multiplier after the stitch type indicates that there should only be one stitch
- commas are used to separate different stitches

`R8: (sc 13, inc) x3`
- parentheses are used to define a local stitch sequence
- the `x<INT>` form is used to specify that the sequence in the preceeding parentheses should be repeated

## Row count
`R21: sc 30 [30]`
- the row count can optionally be specified at the end of the line using square brackets

## Comments
```
<<Starting with red yarn>>
row 1: ch 10
row 2: sc 10
row 3: sc 5 <<changing to green yarn in the last st>>, sc 5
row 4: sc 10 <<FO the green yarn>>
```
- comments can be written within double angled brackets
## Variables
```
let seq = dec 2, sc 5, dec 2

...
R10: sc 10, seq, sc 10
R11: sc 8, seq, sc 8
...
```

```
let i = 5
R4: (sc i, inc, sc i) x(i) 
```
- `let` can be used to define a variable (e.g. a sequence of stitches or an integer) that can be used throughout the pattern as shown
- where a variable is used is a stitch sequence multiplier, it must be contained within parentheses and placed after the `x` symbol
- identify end of assignment with newline

Here are examples of the types that can be stored in variables:
```
let a = 3
let b = true
let c = ch 10
let d = (sc 2, inc) x6
let e = sc 5, dc 2, sc 5
let f = foo()
let g = goo(1, e)
let h = row 1: ch 30
let i = (
    row 2: sc 30
    row 3: (sc 13, inc 2) x2
)
```
In `ast.ml` we have:
```
type t = 
  | TInt 
  | TBool 
  | TStitch
  | TStitchSeqItem
  | TStitchSeq
  | TFunc of t list * t
  | TStmtExpr
  | TStmtExprList
```

## For-loops
```
...
R2: ...
for i = 1 to 5:
    R(2+i): sc i, inc
... 
```
-  a `for`-loop can be used to represent multiple rows with a numerical pattern
- identify body of loop with newline and indentation

## If-else statements
```
...
if <BOOL>:
    ROW 3: sc 30
else:
    ROW 3: dc 30
...
```

## Functions
```
def foo(i):
    return (sc i, inc, sc i) 

foo(3)
```
- functions can be defined using `def`
- functions can be called using the function name
- the function body is defined by indentation
- `return` is used to specify what is returned from the function
- the function return is specified by the contents of the parentheses
- the types that can be returned from functions are the same as the types that can be stored in variables

```
def foo(i):
    return (
        row i: sc 2, inc, sc 2
        row (i+1): dc 6
    ) 

foo(5)
```

- to return multiple lines, they must be indented within the parentheses, starting on a new line


