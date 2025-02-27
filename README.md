# BBDReduction

BBDReduction is a command-line utility tool for **Binary Decision Diagram (BDD) reduction**, written in Haskell. It allows users to input a Boolean formula and apply transformations to simplify the corresponding decision tree.

## Features
- Parses Boolean formulas with logical operators.
- Supports various reduction operations on the BDD.
- Outputs a minimized decision tree representation.
- Written in Haskell, ensuring functional and efficient computations.

## Installation

### Using Prebuilt Executable (Windows)
A precompiled executable (`bdd.exe`) is available for Windows users. Simply download and run it from the command line.

### Compiling from Source
If you prefer to compile the program yourself or are using a non-Windows OS, you can build it using the [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/):

```sh
ghc bdd.hs -o bdd
```

This will generate an executable (`bdd` on Linux/macOS or `bdd.exe` on Windows) which you can run from the terminal.

## Usage

### Input Format
- Boolean formulas are written using the following notation:
  - `xy` → **AND** (`x ∧ y`)
  - `x + y` → **OR** (`x ∨ y`)
  - `!x` → **NOT** (`¬x`)
- Example: `xy + x!yz`

### Commands
After entering a formula, a decision tree representation will be generated. You can then apply transformations:

- `redundant n` → Removes a redundant node **n**.
- `sameTree x y` → Merges nodes **x** and **y** if they represent the same computation tree.
- `done` → Exits when no further reductions are possible.
- `donef` → Forces exit even if further reductions remain.

### Example Execution
```sh
$ ./bdd
Enter formula:
-> xy + x!yz
```

Output (Initial BDD):

```sh
1. if "x" then 2 else 3
2. if "y" then 4 else 5
3. if "y" then 6 else 7
4. if "z" then 9 else 9
5. if "z" then 9 else 8
6. if "z" then 8 else 8
7. if "z" then 8 else 8
8. "0"
9. "1"
```
Applying a transformation:

```sh
Which transformation to apply?
-> redundant 4
```

Output:

```sh
1. if "x" then 2 else 3
2. if "y" then 9 else 5
3. if "y" then 6 else 7
5. if "z" then 9 else 8
6. if "z" then 8 else 8
7. if "z" then 8 else 8
8. "0"
9. "1"
```

```sh
Which transformation to apply?
-> sametree 6 7
```

Output:

```sh
Current BDD:
1. if "x" then 2 else 3
2. if "y" then 9 else 5
3. if "y" then 6 else 6
5. if "z" then 9 else 8
6. if "z" then 8 else 8
8. "0"
9. "1"
```

...

Final reduction:

```sh
Which transformation to apply?
-> done
Reduced BDD has 5 nodes.
```

## Video Demonstration
A video demonstration of BBDReduction is available [here](https://blox-dev.github.io/demos/binary_decision_diagram_haskell.mp4).

## License
BBDReduction is open-source and released under the [MIT License](LICENSE).
