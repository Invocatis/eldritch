(ns examples.core
  (:require
   [eldritch.ardent :refer [data]]
   [eldritch.match :refer [define]]
   [examples.monad :refer [Some None]])
  (:refer-clojure :exclude [empty? pop peek]))

(define empty?)

(data Bool True False)

(data Stack Empty (Top x y))

(define pop
  [(~Top _ T)]
  T)

(define pop
  [~Empty]
  Empty)

(define peek
  [(~Top H _)]
  (Some H))

(define peek
  [~Empty]
  None)

(define empty?
  [(~Top _ _)]
  False)

(define empty?
  [~Empty]
  True)

(defn adapt
  [s]
  (if-not (seq s)
    Empty
    (Top (first s) (adapt (rest s)))))

(data List (Cons head tail) Nil)

(define head
  [(~Cons x _)]
  (Some x))

(define head
  [~Nil]
  None)

(define tail
  [(~Cons _ y)]
  y)

(define tail
  [~Nil]
  Nil)
