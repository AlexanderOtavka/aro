[![Build Status](https://circleci.com/gh/AlexanderOtavka/aro.svg?style=shield&circle-token=84e6e5347cf534a856e4b176fe4d5742c05b623f)](https://circleci.com/gh/AlexanderOtavka/aro)

# aro

> Maintained by Zander Otavka &lt;otavkaal@grinnell.edu&gt;

The aro compiler.

## Building and Running

Running `make` will use cargo to create an optimized executable named `aro`.
`make docker-run args="foo bar"` will build and run the docker image,
passing `foo` and `bar` as arguments to the aro executable. `make docker-test`
will run all unit tests inside the docker container.

## Changelog

### Assignment 1 (1/29/17)

#### New Features

* Build process added with Makefile
* Basic test suite using rust's built in testing framework
* Simple starter program that just parses and prints command line args

#### Changed Features

None

#### Known Bugs

None
