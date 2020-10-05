# eldritch

algebraic data types and pattern matching

## Release

`[eldritch "0.0.0-alpha"]`

## WIP
eldritch is still in development and in alpha, and is subject to breakages without notice.

## Overview

Eldritch is an experimental library to provide Clojure language features available to other modern languages. Abstract data types, and compatible pattern matching come together creating a small monad framework.

### Match

Eldritch is equipped with both runtime and compile time match functions. In addition, the `define` macro has brought pattern matching to function definitions.

```clojure
(require '[eldritch.match :refer [match matches? define]])

; match creates compile-time matching algorithms
; Has both a predicate form, and a switch forms

(match 1 1) ;=> true
(match 1
  1 :yes!
  _ :no!)  ;=> :yes!

; matches? works at runtime, allowing for dynamic patterns

(matches? 1 1) ;=> true
```

`match` and `matches?` have the same semantics, so for the remainder of this readme we will use `matches?` in place of both.

#### Objects

Matchability is defined by the pattern. By default, all objects define match as equality.

```
(matches? 1 1) ;=> true
(matches? 1 2) ;=> false
```

#### Collections

Matching against collections works similar to Clojure destructuring. It is strict, requiring all map keys to be present, lengths to be equal, and types to be the same.

```clojure
(matches? {:a 1} {:a 1}) ;=> true
(matches? [1 2] [1 2]) ;=> true
(matches? (list 1 2) (list 1 2))  ;=> true

(matches? [1 2] (list 1 2)) ;=> false
(matches? [1 2] [1 2 3]) ;=> false
(matches? [1 2 3] [1 2]) ;=> false
(matches? {:a 1 :b 2} {:a 1}) ;=> false
```

#### Variables

Variables, represented by symbols, can be provided as symbols. Variables represented by the same symbol must also match themselves.

```clojure
(matches? 'x 1) ;=> true
(matches? '[x x] [1 1]) ;=> true
(matches? '[x x] [1 2]) ;=> false
```

In the case of `match` (and `matches?`), we solve the ambiguity of variables in the pattern and variables in the current environment with the use of Clojure's unquote (`~`).

```clojure
(let [x 1]
  (match [1 2] [~x x])) ;=> true
```

### Algebraic Data Types

Eldritch provides access to algebraic data types via the `data` macro.

```clojure
(data List (Cons head tail) Nil)
```

Data types can be extend using Java interfaces, or Clojure protocols at definition time by wrapping the definition in a vector.

```clojure
(data
  [Maybe clojure.lang.IDeref
   (deref [this] (second this))]
  (Some x)
  None)
```

The protocol extensions can be added to the parent type (in this case `Maybe`), or on the child definitions (in this case `Some` or `None`).

### Define Macro

In addition to match, we have the `define` macro which adds pattern matching to function signatures.


```clojure
(data Maybe (Some x) None)

(data List (Cons h t) Nil)

(define head
  ([~Nil]
    None)
  ([(~List h _)]
   (Some h)))

(define tail
  ([~Nil]
    Nil)
  ([(~List _ t)]
    t))
```
