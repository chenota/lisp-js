# Lisp-JS

Lisp-JS is a JavaScript interpreter written entirely in Common Lisp. This project is very much in progress and won't encapsulate all of JavaScript, but it will eventually have much of the language's major functionality (you can think of it like a mini NodeJS). 

## Finished

### Lexer

The lexer converts a string into a list of tokens that will be used by the parser. The main lexer functionality is contained in src/tokenizer.lisp, although it also uses functions from src/js-convert.lisp.

### Parser

I've implemented a Pratt parser that can successfully parse a great deal of JS code. At this point, the parser may need some refinement down the line, but it's more than good enough to start implementing a nontrivial evaluator.

### Evaluator

This piece of the program evaluates the abstract syntax tree generated by the parser using a big-step evalution technique.

## TODO

### User-Friendly Program

I have implemented a super primitive user-friendly command line program, however there's still a lot of work to be done so not moving to finished yet.

## How to use

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

I'm working on a command-line program that must be compiled into a binary before use, which can be done with 
```
make build
```

The compiled binary can be ran like so:
```
./ljs.o
```
## Development Information

This project is being developed on Ubuntu 20.04.6 LTS.

## Credits

Quicklisp Packages:
- Alexandria
- CL-PPCRE

Pratt Parsers:
- https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- https://www.youtube.com/watch?v=qyZQPJYvsGw

JavaScript Precedence:
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence

ECMAScript Abstract Operations:
- https://tc39.es/ecma262/multipage/abstract-operations.html