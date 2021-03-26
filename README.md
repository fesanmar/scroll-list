# scroll-list

[![Build Status](https://travis-ci.org/fesanmar/scroll-list.svg?branch=main)](https://travis-ci.org/fesanmar/scroll-list)

Haskell package that provides functions for relocate an item within a list.

## Usage

Add scroll-list to your `package.yml` and import `Data.List.Scroll` module.

Some examples are given below:

```Haskell
  >>> up 2 3 ["one", "two", "three"]
  ["three", "one", "two"]

  >>> up 4 1 ["one", "two", "three"]
  ["one", "two", "three"]

  >>> up 2 3 ["one", "two", "three"]
  ["three", "one", "two"]

  >>> down 0 1 ["one", "two", "three"]
  ["two", "one", "three"]

  >>> up 4 1 ["one", "two", "three"]
  ["one", "two", "three"]

  >>> down 0 4 ["one", "two", "three"]
  ["two", "three", "one"]
```

Read the documentation in [hackage](https://hackage.haskell.org/package/scroll-list-1.0.0.0).
