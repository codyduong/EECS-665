# t1

```
make clean # dragonlex does not overwrite lexer.hs, so remove it first
make build
make run
make test
```

`dragonlex.hs` compiles to `dragonlex` which we use to generate a `lexer.hs` from a `*.spec`
`lexer.hs` compiles to `lexer` which we use create our tokens

## Other

As a retrospective I wanted to use `Rust` but there were way too many headaches, too burdened by type system. Plus I couldn't
really figure out a good way to generate the rust intermediatery code and then the final compilation in a satisfying manner.
