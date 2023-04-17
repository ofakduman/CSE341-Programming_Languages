# G++ Programming Language Homework

This repository contains the implementation of a simple G++ programming language interpreter, designed as a homework assignment. The G++ language is inspired by Lisp and includes basic arithmetic operations, function definitions, control structures, and lexical scoping. The project's main file is helloworld.g++, which demonstrates a sumup function that calculates the sum of all integers up to a given number.
## Language Overview
### Lexical Syntax

    Keywords: and, or, not, eq, gt, nil, set, defvar, deffun, while, if, load, disp, true, false
    Operators: +, -, /, *, (, ), ,
    Comments: Lines or part of the lines starting with ;;
    Terminals: Keywords, Operators, Literals
    Literals: Predefined types include unsigned fractions, represented by two unsigned integers separated by the character "f" (e.g., 123f12 for the fraction 123/12)
    Identifiers: Combinations of alphabetical characters, digits, and "_" with leading alphabetical characters only

### Concrete Syntax

    Non-terminals: $START, $INPUT, $EXPLIST, $EXP, ...
    Expressions: An expression always returns a fraction, and an expression list returns the value of the last expression
    Functions: Function definitions and calls, supporting up to 3 parameters passed by value
    Control Statements: if, while, and, or, not, eq, gt
    Variables: Strong typing, static binding, lexical scoping

## Example Code

The gpp_interpreter.lisp file includes an example implementation of a sumup function, which calculates the sum of all integers up to a given number:

``` lisp
;; helloworld.g++
(deffun sumup (x) 
(if (eq x 1f1) (1) 
(+ x (sumup (- x 1f1)))) 
```


## To use the G++ interpreter with the example code, run the following commands:
```sh
    $ g++
    > clisp gpp_interpreter.lisp
    > (sumup 4 2)
    6
    > (exit)
    $ _
```

## Usage

Make sure you have the G++ interpreter installed on your system.
    Clone this repository and navigate to its directory.
    Run the G++ interpreter, load the example code, and test the sumup function as shown in the example above.
    To exit the interpreter, type (exit).

## Contributing

Feel free to open issues or submit pull requests if you have any suggestions or improvements for the project.
