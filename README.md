# haskell-rolodex [![Stories in Ready](https://badge.waffle.io/lexi-lambda/haskell-rolodex.svg?label=ready&title=Ready)](http://waffle.io/lexi-lambda/haskell-rolodex)

This is a tiny Haskell web service that provides a REST API to manage contacts in a SQLite database. It is built using [servant][servant].

## Installing and running the server

This project uses [stack][stack] for build management. Clone this repository, then run `stack build` to install and compile the server.

The `SQLITE_FILENAME` environment variable must be set in order to run the server, and it will be used to determine where the SQLite database should be on the filesystem. If the file does not exist, it will be created.

```
$ stack build
$ env SQLITE_FILENAME=data.sqlite stack exec haskell-rolodex
```

## Running the test suite

This project includes a small, incomplete test suite. To run it, just run `stack test`.

[servant]: https://github.com/haskell-servant/servant
[stack]: https://github.com/commercialhaskell/stack
