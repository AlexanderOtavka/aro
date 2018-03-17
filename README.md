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

### Assignment 6 (3/16/18)

There aren't many new tests because I implemented most of this stuff using
existing structures like functions and generics. The integration tests on the
examples should suffice to test most of the changes.

#### New Features

* **Ref cells**, and global functions `ref`, `get!` and `set!` for creating and
  interacting with then.
* **While loops** as a global function, implemented with recursion.
* The **sequencing operator**.
* The updated grammar is as follows:

  ```
  // Expression
  e ::= n                       // integer
      | f                       // float
      | #true() | #false()      // boolean
      | x                       // identifier
      | e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2   // arithmetic
      | e1 <= e2                // comparison
      | if e1 then e2 else e3   // if
      | let p <== e1 e2         // recursive let binding with pattern
      | let X <== t e           // type alias
      | x: t1 -t2-> e           // function declaration shorthand
      | fn p -t2-> e            // function declaration with pattern
      | X: t1 -t2-> e           // generic function
      | e1 <| e2 | e1 |> e2     // function call
      | e <| type t | type t |> e   // generic call
      | (e1 e2 ... en)          // tuple
      | [e1 e2 ... en]          // list
      | e1; e2                  // sequencing
      | (e)                     // parenthesis

  // Pattern
  p ::= x: t            // identifier
      | (p1 p2 ... pn)  // destructed tuple

  // Type
  t ::= Int             // integer
      | Num             // float (is a supertype of Int)
      | Bool            // boolean
      | Any             // universal supertype
      | Empty           // universal subtype
      | X               // type identifier
      | t -> t          // function
      | X: t1 -> t2     // generic function (t1 is a subtyping restriction on X)
      | (t1 t2 ... tn)  // tuple
      | [t..]           // list (homogenous, the `..` is part of the syntax)
      | (Ref <| t)      // ref cell
  ```

#### Changed Features

None.

#### Known Bugs

None.

### Assignment 5 (3/14/18)

I don't think I missed anything, but I did a lot here. Generics are meant to
count towards the final project. Also, I implemented this all without a single
thought to efficiency. I can optimize later if needed.

#### New Features

* **Type system** with subtyping relationships. `Any` is the universal
  supertype, `Empty` is the universal subtype.

* **Tuples** of up to `n` elements, along with tuple destructing in functions
  and let expressions using patterns.

* **Heterogenous lists**. Elements of the empty list have the `Empty` type
  (that's why we need it).

* **Generics**, which are expressed as functions that take a type parameter
  with a subtyping restriction. Also, tuples can be destructed in functions and
  let expressions using patterns. "unit" is just an empty tuple.

* **Type aliases**, which are basically just let bindings for types.

* The updated grammar is as follows:

  ```
  // Expression
  e ::= n                       // integer
      | f                       // float
      | #true() | #false()      // boolean
      | x                       // identifier
      | e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2   // arithmetic
      | e1 <= e2                // comparison
      | if e1 then e2 else e3   // if
      | let p <== e1 e2         // recursive let binding with pattern
      | let X <== t e           // type alias
      | x: t1 -t2-> e           // function declaration shorthand
      | fn p -t2-> e            // function declaration with pattern
      | X: t1 -t2-> e           // generic function
      | e1 <| e2 | e1 |> e2     // function call
      | e <| type t | type t |> e   // generic call
      | (e1 e2 ... en)          // tuple
      | [e1 e2 ... en]          // list
      | (e)                     // parenthesis

  // Pattern
  p ::= x: t            // identifier
      | (p1 p2 ... pn)  // destructed tuple

  // Type
  t ::= Int             // integer
      | Num             // float (is a supertype of Int)
      | Bool            // boolean
      | Any             // universal supertype
      | Empty           // universal subtype
      | X               // type identifier
      | t -> t          // function
      | X: t1 -> t2     // generic function (t1 is a subtyping restriction on X)
      | (t1 t2 ... tn)  // tuple
      | [t..]           // list (homogenous, the `..` is part of the syntax)
  ```

* The typing of `if`s is a little bit special. Instead of forcing them to be the
  same, it tries to find a common supertype, defaulting to `Any` if neither
  branch is a subtype of the other. This solves annoying issues where you
  might return an `Int` in one branch and a `Float` in another and have the
  expression not typecheck.

* Added an ugly "hook" syntax. This is only meant to be used internally, but it
  is technically exposed to the user. Hooks allow us to call into the runtime.
  Hooks aren't automatically typesafe, so they need to be annotated inline.
  I use hooks to implement the following globally bound functions:
  ```
  push: T: Any -> T -> [T..] -> [T..]   // cons by a different name
  is_empty: T: Any -> [T..] -> Bool
  head: T: Any -> [T..] -> T
  tail: T: Any -> [T..] -> [T..]
  floordiv: Num -> Num -> Int   // perform division and floor the result
  ```

#### Changed Features

* `a / b` for integers `a` and `b` now returns a float. Integer division is now
  done with `floordiv`. I did this because I wanted `Int` and `Num` aka.
  Float to have a subtyping relationship, and I didn't want people to have to
  worry about the possibility that dividing two `Num`s might sometimes floor
  the result. Python 3 works this way too.

#### Known Bugs

None.

### Assignment 4 (2/25/18)

#### New Features

* Added functions and let bindings. Let bindings are recursive, so there is
  no explicit fixed-point construct. The updated grammar is as follows:
  ```
  e ::= n                       // integer
      | f                       // float
      | #true() | #false()      // boolean
      | x                       // identifier
      | e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2   // arithmetic
      | e1 <= e2                // comparison
      | if e1 then e2 else e3   // if
      | let x <== e1 e2         // recursive let binding
      | x -> e                  // function declaration
      | e1 <| e2 | e1 |> e2     // function call
      | (e)                     // parenthesis
  ```
* Added small-step evaluation. Use the `-s` flag to see it in action. It's fun
  to run it on `examples/recursion.aro` and watch it explode and then collapse
  again.

#### Changed Features

* `nan` and `inf` are now globally bound values, and can be shadowed.

#### Known Bugs

None.

### Assignment 3 (2/23/18)

#### New Features

* Integrated LALRPOP lexer/parser generator.
* Added pretty error messages with line/column information that underline the
  problem. Oooh, shiny. Try `cargo run -- examples/parse_error.aro` to see an example.

#### Changed Features

* Changed syntax to use infix operators. The new language grammar is:
  ```
  e ::= n | f | (e) | e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2
        | true | false | e1 <= e2 | if e1 then e2 else e3
  ```

#### Known Bugs

Still none. I still have hella tests, but not as many now since a library does
most of the work now.

### Assignment 2 (2/7/18)

#### New Features

A basic S-expression language has been added with the following grammar:

```
e ::= n | (+ e1 e2) | (- e1 e2) | (* e1 e2) | (/ e1 e2)
    | true | false | (<= e1 e2) | (if e1 e2 e3)
    | f | NaN | inf
```

where `n` is an integer constant and `f` is a floating point constant.

The executable takes a file lexes, parses, and evaluates the code, and spits
out the result to stdout.

#### Changed Features

The executable no longer takes a `--length` argument. Instead, it now only
accepts a single file name to evaluate. Use `--help` for details.

#### Known Bugs

None. And I have hella tests.

### Assignment 1 (1/29/18)

#### New Features

* Build process added with Makefile
* Basic test suite using rust's built in testing framework
* Simple starter program that just parses and prints command line args

#### Changed Features

None

#### Known Bugs

None
