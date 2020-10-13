(ns examples.church
  (:require
   [eldritch.ardent :refer [data]]
   [eldritch.match :refer [define]])
  (:refer-clojure :exclude [+ - > < >= <=]))

(data Church (S x) Zero)

(define church)

(define church
  [0] Zero
  [n] (S (church (dec n))))

(define unchurch
  [Zero] 0
  [(~S n)] (clojure.core/+ (unchurch n) 1))

(define +
  [~Zero n] n
  [n ~Zero] n
  [(~S m) (~S n)] (S (S (+ m n))))

(define >
  [~Zero _] false
  [_ ~Zero] true
  [(~S m) (~S n)] (> m n))

(define asdf
  [(~S n)] (println S))
