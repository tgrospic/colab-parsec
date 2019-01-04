# Collaborative Learning :: Parsec

This project is created as a playground to learn [Parsec](https://wiki.haskell.org/Parsec) library.

## Short info about __Cabal__ and __Stack__

The easiest way to start with Haskell is to install [Haskell Platform](https://www.haskell.org/platform/). It contains GHC, Cabal and also Stack.

Haskell package manager is [Cabal](https://www.haskell.org/cabal/) and it's currently in transformation to `V2`. To use Cabal V2 all commands should be prefixed with `new-` like `cabal new-build`. V1 manages dependencies globally and sandboxed mode does not share the same packages so V2 is preferred and it will soon be default (V3). It uses `<project_name>.cabal` file for specification.

Another option to manage dependencies is with [Stack](https://docs.haskellstack.org/en/stable/README/). It's created to solve the Cabal V1 problems. It's a wrapper around Cabal and can use `package.yaml` file as specification from which `*.cabal` file is generated (use as read-only).  
It uses [Stackage](https://www.stackage.org/) server to store _Snapshots_ of libraries that are checked to work together. Stack configuration is written in `stack.yaml` file. Stack can also manage and run different independent versions of GHC. `resolver` option specify which snapshot and GHC version will be used.

\* Stack can use globally installed GHC (e.g. installed with Haskell Platform) so in this project resolver is set to use snapshot with GHC 8.4.3.

`colab-parsec.cabal` file is generated and it's not included in this project. _I'm not sure is this is the right thing but it's easier to edit package.yaml than .cabal file._  
But the main reason why I'm using Stack is because [Haskell IDE engine](https://github.com/alanz/vscode-hie-server) currently works with Stack but not with Cabal V2.

## Build (install dependencies)

Stack commands will generate `colab-parsec.cabal` file.

```sh
stack build
# or
cabal new-build
```

## Start repl

```sh
stack repl
# or
cabal new-repl
```
