# Mini-Scheme spec tests

`test/spec` contains executable specification files for Mini-Scheme.

## Layout

- `core/*.scm`
  - Required Mini-Scheme behavior.
  - Loaded by `cabal test` through `SchemeSpecRunner`.
  - Top-level forms run in file order with a shared environment.
  - A form with `;; => expected` is compared against the interpreter output.
  - A form without `;; =>` is treated as setup and must still evaluate successfully.

- `errors/*.scm`
  - One malformed source fragment per file.
  - Checked directly with the parser, not through the interpreter.
  - These are separated from `core` because a single parse error prevents parsing
    the rest of a normal Scheme source file.

- `r5rs/*.scm`
  - R5RS-portable subset used for differential testing.
  - The same file is always checked against Mini-Scheme.
  - If `guile` or `racket` is available, `cabal test` also compares the outputs
    against that implementation.
  - `MINI_SCHEME_R5RS=/path/to/scheme` can be used to select an executable
    explicitly. The generated Scheme file path is passed as the final argument.

- `options/*.scm`
  - Optional features such as TCO and macros.
  - These files document the intended behavior but are not part of the default
    required test suite yet.

## Expectation Syntax

```scheme
(+ 1 2) ;; => 3
```

`;; => ERROR` means the form must fail, but the exact error message is not part
of the spec. For future cases that need a message check, use:

```scheme
(some-form) ;; => ERROR contains "needle"
```
