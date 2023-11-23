# AoC

## Templates, utilitites and download-script

Download today's puzzle input from [adventofcode](https://adventofcode.com), create a directory for this day and some template files to solve the puzzle in a given language.
Includes templates and utility libraries for the following languages:
- python
- julia
- haskell
- C++

### Requirements
 - python3
 - requests (`pip3 install requests`)
 - if you want to get your puzzle input downloaded automatically,
    - log into [adventofcode](https://adventofcode.com) via your browser and get the content of the session cookie
    - in your terminal, do `export AOC_SESSION=your-session-id`

### How to use
This repository can either be used as a template repository or as a submodule.

```bash
$ python3 init.py --help
usage: init.py [-h] [-d DAY] [-y YEAR] [-l LANGUAGE [LANGUAGE ...]]
               [--no-download]

optional arguments:
  -h, --help            show this help message and exit
  -d DAY, --day DAY
  -y YEAR, --year YEAR
  -l LANGUAGE [LANGUAGE ...], --language LANGUAGE [LANGUAGE ...]
  --no-download
```

If no day / year is given, the current date is used. `language` defaults to `py`.

The script creates a folder for the day in the current working directory and copies all files for the given language from `templates/<language>/` into it.

For example, to solve today's problem in Julia, do
```bash
$ export AOC_SESSION=your-session-id
$ python3 init.py -l jl
```

## Notes on languages

### Python
Templates are in `templates/py`. Do `pip install -e utils/py` to install the utility module `aoc.utility`.

```bash
$ python init.py
$ cd dayXX
$ pytest   # pip install pytest
$ python solver.py
```

### Julia
Templates are in `templates/jl`. Do `] dev utils/jl` to make the utility module `AoC` available in your environment.

```bash
$ python init.py -l jl
$ cd dayXX
$ julia
julia> push!(LOAD_PATH, pwd())
julia> using Revise
julia> using Solver
julia> test()  # or test(1) or test(2)
julia> main()  # or main(1) or test(2)
```
Revise hot reloads code in `Solver.jl`, so code along and just keep your REPL running.

### Haskell
Templates are in `templates/hs`

dependencies: `cabal install split optparse-applicative raw-strings-qq vector parsec`

```bash
$ python init.py -l hs
$ cd dayXX
$ ghc -i../utils/hs solver.hs
$ ./solver
```

Run tests with `./solver --test` or in the repl:
```
$ ghci -i../utils/hs solver.hs
λ> test1
λ> test2
```

You can also run only part one or two with `./solver --part 1`.

Utilities are in `utils/hs`

### C++
requires `cxxtest`: `sudo apt-get install cxxtest`

Templates are in `templates/cpp`

```bash
$ python init.py -l cpp
$ cd dayXX
$ make test
$ make run
```

Utilities are in `utils/cpp`. `cd utils/cpp; make` to test Utils.cpp
