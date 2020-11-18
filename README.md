# AoC 2020

## Templates and download script

Download today's puzzle input from [adventofcode](https://adventofcode.com), create a directory for this day and some template files to solve the puzzle in a given language.

### Requirements
 - python3
 - requests (`pip3 install requests`)
 - pytest (`pip3 install pytest`) (only if you solve puzzles in python)
 - if you want to get your puzzle input downloaded automatically,
    - log into [adventofcode](https://adventofcode.com) via your browser and get the content of the session cookie
    - in your terminal, do `export AOC_SESSION=your-session-id`

### How to use

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
```bash
$ export AOC_SESSION=your-session-id
$ python3 init.py -l cpp
```
