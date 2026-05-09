# Documentation

## Crochet-Specific Syntax and Semantics

### Stitch Types
LoopLang supports the following stitch types:

```
mr       << magic ring >>
ch       << chain >>  
sc       << single crochet >>
dc       << double crochet >>
hdc      << half-double crochet >>
tr       << treble crochet >>
inc      << increase stitch >>
dec      << decrease stitch >>
slst     << slip stitch >>
```

### Row Identifiers
LoopLang supports the following ways of identifying the start of a new row:

```
row 1: ...
ROW 2: ...
r3: ...
R4: ...
```

Each row should be on a newline and begin with one of the four row identifiers specified above. If the row number is an integer literal, then the space following the row identifier is optional. For variable row numbers, the space is required unless the variable is enclosed within parentheses.

### Row Ranges
The same stitch sequence can be repeated across multiple rows using the following notation:

```
R1: ch 30
R2-5: sc 30
R6: dc 30
```

The upper and lower bounds of the row range are inclusive.

### Multipliers
Multiple consecutive stitches of the same type are represented by specifying the stitch type and the number of repetitions, arranged in any order. A stitch type without a multiplier represents a single instance of that stitch.

```
R6: sc 9, inc
  
<< ... or alternatively ... >>
  
R6: 9 sc, inc
```

To indicate the repetition of a sequence of stitches within a single row, the sequence should be enclosed within parentheses and this should be followed by the number of times the sequence should be performed.

```
R8: (sc 2, inc) x5
```

This repetition can used be used within a larger sequence of stitches.

R10: sc, (dc, inc 2) x3, dec

### Stitch Count
The stitch count can optionally be specified at the end of the row using square brackets.

```
R20: sc 30 [30]
R21: (sc 14, inc) x2 [32]
```

### Comments
Comments can be used to provide additional instructions within the pattern. Comments are enclosed within double angle brackets, and can be placed on their own line or within the items of a stitch sequence.

```
<<starting with red yarn>>
R1: ch 10
R2: sc 10
R3: sc 5 <<changing to green yarn in the last st>>, sc 5
R4: dc 10 <<FO>>
```

Comments are preserved during compilation, and so will be present in the output crochet pattern.

## Additional Features and Programming Constructs

### Sequence Mirroring
The `mirror( main_seq | middle_seq )` syntax can be used to represent a symmetrical sequence. This will result in the main sequence followed by the middle sequence followed by the reverse of the main sequence.

```
R1: mr 15
R2: mirror(sc 4, dc 3 | inc)
R3: sc 16
```

Row 2 of the example above will compile to `R2: sc 4, dc 3, inc, dc 3, sc 4 [16]` .

### Variables
Variables are defined using the `let` keyword. Variables can be used to store integers, booleans, sequences of stitches, rows, the results of function calls, and values from other variables. Where multiple rows are assigned to a variable, they should be enclosed within parentheses, indented, and each row should be on its own line.

```
<<integer and boolean definitions>>
let a = 3
let b = true
<<stitch sequence definitions>>
let c = ch 10
let d = (sc 2, inc) x6
let e = sc 5, dc 2, sc 5
<<row definitions>>
let f = R1: ch 30
let g = (
    R2: sc 30
    R3: (sc 13, inc 2) x2
)  
<<function results and variable definitions>>
let h = foo()
let i = a
```

These variables can then be used within the LoopLang program, and will be converted to their assigned values during compilation.

### Arithmetic and Boolean Operations
LoopLang supports the following arithemtic operations:

```
n1 + n2       <<addition>>
n1 - n2       <<subtraction>>
n1 * n2       <<multiplication>>
n1 / n2       <<integer division>>
```

LoopLang supports the following boolean operations:

```
<<boolean relational operators>>
n1 > n1       <<greater than>>
n1 < n2       <<less than>>
n1 == n2      <<equality>>
<<boolean logical operators>>
b1 and b2     <<logical AND>>
b1 or b2      <<logical OR>>
not b         <<logical NOT>>
```

### If-Else Statements
If-else statements are control flow structures that determine which block of code to execute based on a boolean condition.

```
if boolean_condition:
    ...
else:
    ...
```

If-else statements can also be written without the `else` component.

### For-Loops
For-loops are used to repeatedly execute a block of code a specific number of times by iterating over a counter. The syntax for a for-loop requires the definition of the counter variable that can be referenced within the for-loop body, and the specification of the lower and upper bounds of the values that the variable iterates over. The lower and upper bounds are inclusive, and the upper bound must be greater than or equal to the lower bound.

```
R1: ch 30
for i = 2 to 5:
    R(i): sc 30
R6: (sc 14, inc) x2
```

### Functions
Functions are defined used the `def` keyword, followed by the name of the function, and a list of its paramaters within parentheses. Within the function body, whatever is to be returned from the function should be written within the `return(...)` construct.

```
def foo(i):
    return(sc i, inc 2, sc i)
R1: ch 10
R2: foo(4)
R3: foo(5)
```

If multiple rows are being returned from a function, then each row should be on its own line and they should be indented.

```
def foo(row_num, seq1, seq2):
    return(
        R(row_num): seq1
        R(row_num + 1): seq2
    )
```

## Debugging

### Common Error Messages

`ERROR SyntaxError: syntax error at line 3, column 11`

This will mean that there is something wrong with the keywords you have written. The line number (number of lines down the file) and the column number (number of character across that line) should direct you to the location of the error (possibly just after the incorrect keyword). Then you should check the documentation to make sure you have written everything correctly.

`ERROR TypeError: undefined variable: 'seq'`

This means that you have used a variable before defining it. The name of the undefined variable will be specified in the error message. Make sure that you have written `let seq = ...` before using seq later on in the program.

`ERROR TypeError: stitch sequence multiplier expression expects an Integer multiplier`

When using variables, make sure that the value assigned to that variable makes sense in the location it is being used. For example, make sure that variables that hold integers are only used where you would normally expect to see an integer.

`ERROR StitchError: R1 of the pattern can only contain chain stitches or a magic ring`

This error message means that a stitch has been used somewhere that it is not allowed to be used. Either there are stitch types other than chain stitches or a magic ring in the first row, or a stitch such as a magic ring has been used someone other than the first row.

`ERROR RowNumberError: expected row number 6, but found row number 7 in its place`

This type of error message is produced when a row is missing from the pattern. Row numbers should start from 1 and increment by 1 with each new row. This example message above is telling you that row 6 is missing.

`ERROR StitchCountError: row number 5 is built on top of 10 stitches which is inconsistent with the previous stitch count of 12`

This means that the compiler has detected an inconsistent stitch count relationship. If you encounter this message, make sure that each row uses the same number of stitches as the stitch count from the previous row. The error message will tell you which row number contains the wrong number of stitches, how many stitches it currently uses, and how many stitches it should be using.

`ERROR ForLoopError: the for-loop expects the lower bound to be less than or equal to the upper bound, but found a lower bound of 5 and an upper bound of 2 being used`

When writing a for-loop, ensure that the `n1` and `n2` in `for i = n1 to n2:` are such that `n2` is greater than or equal to `n1`.

`ERROR DivideByZeroError: division by zero encountered during expression evaluation`

This means that a calculation within the program contains a division by zero, which is not allowed.

### Common Warning Messages

`WARNING the function foo(Integer->StitchSequence) contains rows that are not returned`

This warning means that there are rows within the function body that are not placed within the `return(...)` construct. These rows will be ignored, and have no effect on the final pattern.

`WARNING the given stitch count for row number 2 was incorrect, this has been corrected in the result`

This warning means that a stitch count annotation provided by the user in the LoopLang program is incorrect, but the pattern itself and its stitch count relationships are still valid. In this scenario, the compiler will calculate the correct stitch count, and write this instead of the incorrect stitch count in the output pattern.