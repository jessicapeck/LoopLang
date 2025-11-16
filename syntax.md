# LoopLang Syntax

`r1: ...`
`R1: ...`
`row 1: ...`
`ROW 1: ...`
- the start of a new row can be indicated using any of the above forms
- a space between the row identifier and the row number is optional for all forms when the row number is a literal
- when the row number is a variable, if the variable is in parentheses then the space is optional, otherwise the space is mandatory

`R1: sc 3, inc, sc 3` 
`R1: 3 sc, inc, 3 sc` 
- multiple consecutive stitches can be denoted by specifying the stitch type and then the number of consecutive stitches of that type or vice versa
- a lack of stitch multiplier after the stitch type indicates that there should only be one stitch
- commas are used to separate different stitches

`R8: (sc 13, inc) x3`
- parentheses are used to define a local stitch sequence
- the `x<INT>` form is used to specify that the sequence in the preceeding parentheses should be repeated

`R21: sc 30 [30]`
- the row count can optionally be specified at the end of the line using square brackets

```
let SEQ = dec 2, sc 5, dec 2

...
R10: sc 10, SEQ, sc 10
R11: sc 8, SEQ, sc 8
...
```

```
let i = 5
R4: (sc i, inc, sc i) x(i) 
```
- `let` can be used to define a variable (e.g. a sequence of stitches or an integer) that can be used throughout the pattern as shown
- where a variable is used is a stitch sequence multiplier, it must be contained within parentheses and placed after the `x` symbol
- identify end of assignment with newline

```
...
R2: ...
for i = 1 to 5:
    R(2+i): sc i, inc
... 
```
-  a `for`-loop can be used to represent multiple rows with a numerical pattern
- identify body of loop with newline and indentation

```
def foo():
    ...
```
- functions can be defined using `def`

