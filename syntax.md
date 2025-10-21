# LoopLang Syntax

`R1: 3 sc, inc, 3 sc` 
- rows are delimetered using `R<INT>:` form
- multiple consecutive stitches can be denoted by specifying the number of conesecutive stitches and then the stitch type `<INT> <STITCH>`
- commas are used to separate different stitches

`R8: (13 sc, inc)x3`
- parentheses are used to define a local stitch sequence
- the `x<INT>` form is used to specify that the sequence in the preceeding parentheses should be repeated

`R21: 30sc [30]`
- the row count can optionally be specified at the end of the line using square brackets

```
let SEQ = dec 2, 5 sc, dec 2

...
R10: 10sc, SEQ, 10sc
R11: 8sc, SEQ, 8 sc
...
```
- `let` can be used to define a sequence of stitches under a shorter name which can be used throughout the pattern as shown
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

