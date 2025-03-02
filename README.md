# LBTS2023-2024_trusted_interpreter

## Overview
LBTS2023-2024_trusted_interpreter is a programming language interpreter with built-in security features for handling trusted and untrusted data. The language focuses on information flow control and taint analysis to prevent leakage of sensitive information and ensure secure execution of code.

## Key Features

- **Taint Tracking**: Automatically tracks and propagates taint information across operations
- **Trust Blocks**: Secure execution environments for handling sensitive data
- **Secret Values**: Special data type for representing confidential information
- **Handle Mechanism**: Secure evaluation of trusted closures
- **Plugin System**: Controlled execution of external code with security checks

## Language Semantics

The language is expression-based and includes:

- Basic data types (integers, booleans, strings, lists)
- First-class functions
- Let-binding for variable declaration
- Conditional expressions
- Binary operations with taint propagation
- Trust regions for secure computation

## Security Model

LBTS2023-2024_trusted_interpreter implements a security model based on:

1. **Information Flow Control**: Tracks how information flows through the program to prevent leaks
2. **Taint Analysis**: Marks values that may contain sensitive information as "tainted"
3. **Trust Boundaries**: Provides mechanisms to explicitly trust and handle data
4. **Execution Control**: Restricts certain operations in trusted contexts

## Usage Examples

### Basic Operations

```ocaml
Let("x", CstInt(5),
    Let("y", CstInt(10),
        BinOp("+", Var("x"), Var("y"))))
```

### Working with Trust Blocks

```ocaml
Let("secret_value", Secret("password123"),
    Trust(CstBool(true),
          Var("secret_value")))
```

### Handling Tainted Data

```ocaml
Let("sensitive", Secret("sensitive_data"),
    Let("tainted", Var("sensitive"),
        Trust(CstBool(true),
              Handle(Var("tainted")))))
```

## Project Structure

- `lib/ast.ml`: Abstract Syntax Tree definitions
- `lib/env.ml`: Environment handling and variable lookup
- `lib/interpreter.ml`: Core evaluation logic
- `lib/taint.ml`: Taint propagation and operations
- `lib/utilities.ml`: Helper functions for value handling and display

## Security Restrictions

The interpreter enforces several security restrictions:

- No negative number results in arithmetic operations
- No division by zero
- Tainted values cannot be accessed in trust blocks
- Trust blocks cannot be nested
- Handle blocks can only be used inside trust blocks
- Plugin execution is not allowed in trust contexts
- Secret data cannot be leaked through plugin execution

## Installation

### Prerequisites
- OCaml (recommended version 4.14 or higher)
- Dune build system

### Build Instructions
```bash
# Clone the repository
git clone https://github.com/contisimone99/LBTS2023-2024_trusted_interpreter.git
cd PROGETTO_LBTS

# Build the project
dune build

# Run tests
dune test
```

## Running Programs

Programs can be executed by:

1. Creating an expression using the AST constructors
2. Evaluating it with the `eval` function from the interpreter
3. Providing an initial environment and trust context

Example:
```ocaml
open PROGETTO_LBTS.Ast
open PROGETTO_LBTS.Env
open PROGETTO_LBTS.Interpreter

let expr = Let("x", CstInt(5),
               BinOp("+", Var("x"), CstInt(3)))

let result = eval empty_env false expr
```


## License
This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

This project was developed as part of [your course/research/etc.] focused on language-based security mechanisms.
