# Generation Z Interpreter (ZPP)

Welcome to the Generation Z Interpreter! This project is an interpreter for a fictional programming language designed to be simple and educational.

## Features

- **Simple Syntax**: Easy to learn and use.
- **Educational**: Great for learning the basics of programming and interpreters.
- **Extensible**: Easily add new features and functionality.

## Installation

To install the Generation Z Interpreter, clone the repository and build the project:

```sh
git clone https://github.com/Colinster327/zpp.git
cd zpp
opam install . --deps-only
dune build
```

## Usage

To run a Generation Z program, use the following command:

```sh
dune exec zpp path/to/your/program.zpp
```

## Basic Syntax

Here are some examples to help you understand the syntax of the zpp programming language:

### Variable Declaration and Conditional Statements

```zpp
vibecheck x = 5;
lowkey (x < 7) {
  6 * 4
} cap {
  x / 5
}
```

### Arithmetic Operations

```zpp
vibecheck a = 2;
vibecheck b = 3;
4 + a * b
```

### Looping

```zpp
vibecheck x = 0;
grind x < 6 {
  x <= x + 1;
}
```

### Functions

```zpp
vibecheck add = [x, y] => {
  x + y
};
add [5, 5]
```

### Recursion

```zpp
vibecheck fib = [n] => {
  lowkey n < 2 { n }
  cap {fib [n - 1] + fib [n - 2]}
};
fib [5]
```

### Standard Output

```zpp
slay "Hello World!";
```

To run a program, save it to a file (e.g., `hello.zpp`) and execute:

## Contributing

Contributions are welcome! Please fork the repository and submit a pull request with your changes.

## Contact

For any questions or suggestions, please open an issue on GitHub.

Happy coding!
