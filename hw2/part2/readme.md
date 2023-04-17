# G++ Programming Language Lexical Analyzer

This is a Lexical Analyzer for the G++ programming language, implemented in Lisp. The purpose of this project is to tokenize and analyze the syntax of a given G++ source code, detecting any lexical errors and providing a clear output of the token types.
## Features

    Read G++ source code from a file or user input
    Tokenize the source code into various types of tokens, such as keywords, identifiers, and operators
    Detect and report any lexical errors found in the source code
    Provide an output of the tokens and their types

## Usage

To use this lexical analyzer, you can either provide the G++ source code file as an argument when running the script or interactively input the code during runtime.
### Running with a source code file

To run the lexical analyzer with a G++ source code file, provide the filename as a command-line argument:

bash

sbcl --script gpp_lexer.lisp <source_code_file>

The output will show the tokens and their types in the order they appear in the source code.
## Interactive mode

To run the lexical analyzer interactively, simply execute the script without any arguments:

bash

sbcl --script gpp_lexer.lisp

You will be prompted to enter G++ source code lines. Type the code and press Enter to see the tokenized output for each line. To exit the interactive mode, type :exit and press Enter.
## Example



``` lisp
(("KW_FOR" "for") ("IDENTIFIER" "i") ("KW_IN" "in") ("KW_RANGE" "range") ("OP_OP" "(") ("VALUEI" "0") ("OP_COMMA" ",") ("VALUEI" "10") ("OP_CP" ")") ("OP_COLON" ":"))
(("KW_PRINT" "print") ("OP_OP" "(") ("IDENTIFIER" "i") ("OP_CP" ")"))
```
## Contributing

If you'd like to contribute to this project, feel free to fork the repository and submit a pull request with your changes. All contributions are welcome!