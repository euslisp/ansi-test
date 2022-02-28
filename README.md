## Introduction

This repository contains code for test, as well as some documentation on test results, related to Common Lisp compliance of the [EusLisp](https://github.com/euslisp/EusLisp) language.

Those tests are based on the [Ansi-Test](https://common-lisp.net/project/ansi-test/) test suite, which was made to test the Common Lisp compliance of different Common Lisp implementations.

To run the tests (assuming you have installed euslisp, perhaps with the irt extension), issue either `eus run-tests.l`, `irteusgl run-tests.l`, `roseus run-tests.l` or `./run-tests.l` (assuming the file has execution permissions).

In order to get precise coverage of the tests (so as to have an idea of how much "Common" is EusLisp), you might want to visit the file `REPORT.org`, preferentially in emacs.

Run `./run-tests.l --help` for more options.

```bash
usage: run-tests.l [-h] [-cl] [-aux] [-e=INFILE] [-u=OUTFILE]

ANSI test suite for EusLisp

optional arguments:
  -h, --help	show this help message and exit
  -cl, --cl-compatible	Set when using EusLisp cl-compatible branch
  -aux, --auxiliary	Set to load cl-compatible libraries on 'auxiliary/cl_comp/'
  -r=WHITE-LIST, --run-test=WHITE-LIST	Run a certain test or type of test
  -s=BLACK-LIST, --skip-test=BLACK-LIST	Skip a certain test or type of test
  -e=INFILE, --ensure-pass=INFILE	Only execute tests marked as successful in the designated log file
  -u=OUTFILE, --update-log=OUTFILE	Print the test results to the designated log file
  -i, --interactive	Set to start the euslisp interpreter with the loaded test suite
  -q, --quiet	Set to inhibit printing failed tests in the end
```

## Results


        | Tests-version           | Test-num | Passed | Failure | Skip |
        |-------------------------+----------+--------+---------|   -  |
        | 1                       |     5557 |   2983 |    2574 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 2 - Add Arrays tests    |     6297 |   3087 |    3210 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 3                       |     7676 |   3489 |    4187 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 4 - Add misc tests      |     8187 |   3506 |    4681 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 5 - Add numbers tests   |     9582 |   3988 |    5594 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 6 - Add sequences tests |    11249 |   4659 |    6590 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 7                       |    15420 |   6601 |    8819 |   -  |
        |-------------------------+----------+--------+---------|   -  |
        | 8 - Track skips (master)|    16203 |   5960 |    9660 |  583 |
        |-------------------------+----------+--------+---------|   -  |
        | 8b - With -cl -aux      |    16203 |   7068 |    8559 |  576 |
