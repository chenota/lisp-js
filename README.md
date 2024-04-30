# LispJS

LispJS is a JavaScript interpreter written entirely in Common Lisp, that includes both a CLI and file functionality.

## How to Run

This project is formulated as an ASDF-based system (i.e. package) for Common Lisp. In order to run it, you must have SBCL and Quicklisp installed. If so, you can clone this repository into

```
~/.quicklisp/local-projects
```
or any other location that stores Quicklisp systems.

If you want to use any function available in the package, you can manually load the system in SBCL like so:

```
* (ql:quickload :lisp-js)
* (in-package :lisp-js)
```

If you don't include the last line, you can use any of the exported functions (found in package.lisp) in SBCL like so:
```
(lisp-js:<function> <args>)
```

To use the program outside of SBCL, you must compile it into a binary, which can be done using the included makefile with
```
make build
```

The compiled binary can be ran like so:
```
./lispjs [file-name]
```

## How Does LispJS Work?

LispJS was built entirely from scratch (by me!), and follows a typical interpreter software pipeline.

### Lexer

The lexer generates a list of tokens from the input file or string. The lexer generates tokens by relating regular expressions to token types, and capturing the longest match at the beginning of the input string. If a match is found, a token is generated and associated with the string that matched it, and if a match is not found, the program throws a lexing error and exits. Finally, if a specific token type, like NUMBER or BOOLEAN, needs to be associated with a non-string, the lexer converts the associated string into the correct data type.

Related Files:
<ul>
    <li>tokenizer.lisp</li>
    <li>js-convert.lisp</li>
</ul>

Example:

The lexer converts this input:
```
let a = 1 + 2;
print(a)
```

into this sequence of tokens:
```
(LET "let") (IDENTIFIER "a") (ASSIGN "=") (NUMBER 1) (PLUS "+") (NUMBER 2) (SEMICOLON ";") (PRINT "print") (LPAREN "(") (IDENTIFIER "a") (RPAREN ")")
```

### Parser

The parser follows up the lexer by structuring a given sequence of tokens into a tree structure which represents an order of operations that is meaningful to the program. The specific type of parser I implemented is a Pratt Parser, which I chose because Pratt Parsers are very easy to hack and add on to, unlike other popular parsing methods like LALR which need a formal grammar. Furthermore, Pratt Parsers are top-down but can parse matching parenthesis and left-recursion unlike the more popular ll(n) parsing method, which I find super interesting! In short, Pratt Parsers associate a binding power with operators, and group operators with higher binding powers closer to each other. My particular parser implementation uses parselets, which are functions that tell the parser how to parse tokens it encounters. Pratt Parsers were designed with basic arithmetic expressions in mind, so I had to do quite a bit of syntax directed translation (SDT) to make a clean abstract syntax tree (AST) out of JavaScript code.

Related Files:
<ul>
    <li>pratt-parser.lisp</li>
    <li>pratt-parselets.lisp</li>
</ul>

Example:



## Development Information

This project is being developed on Ubuntu 20.04.6 LTS.

## Credits

Quicklisp Packages:
- Alexandria
- CL-PPCRE
- parse-number

Pratt Parsers:
- https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- https://www.youtube.com/watch?v=qyZQPJYvsGw

JavaScript Precedence:
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence

ECMAScript Abstract Operations:
- https://tc39.es/ecma262/multipage/abstract-operations.html