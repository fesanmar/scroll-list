# scroll-list

[![Build Status](https://travis-ci.org/fesanmar/scroll-list.svg?branch=main)](https://travis-ci.org/fesanmar/scroll-list)

Haskell package that provides functions for relocate an item within a list.

## Usage

Add scroll-list to your `package.yml` and import `Data.List.Scroll` module.
The following functions are available in the module:

### Up function

The `up` function moves an element 'n' positions to the beginning of a list.

```Haskell
>>> up 2 2 ["one", "two", "three"]
["three", "one", "two"]
```

### Down function

The `down` function moves an element `n` positions to the end of a list.

```Haskell
>>> down 0 1 ["one", "two", "three"]
["two", "one", "three"]
```

### Remove by idenx function

The `deleteByIndex` function removes an element from a list by its  within it.

```Haskell
>>> deleteByIndex 1 ["one", "two", "three"]
["one", "three"]
```

Read the documentation in [hackage](https://hackage.haskell.org/package/scroll-list-1.1.0.0).
