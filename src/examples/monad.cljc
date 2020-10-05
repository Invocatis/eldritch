(ns examples.monad
  (:require
   [eldritch.ardent :refer [data]]
   [eldritch.match :refer [define]]))

(define fmap
  [f value]
  (f value))

(define bind
  [value f]
  (f value))

(define extract
  [x]
  x)

(define fapply
  [f & vals]
  (apply f vals))

(data Maybe (Some x) None)

(define fmap
  [f (~Some value)]
  (Some (f value)))

(define fmap
  [_ ~None]
  None)

(define bind
  [(~Some value) f]
  (f value))

(define bind
  [~None _]
  None)

(define extract
  [(~Some value)]
  value)

(define extract
  [~None]
  nil)

(define fapply
  [f & vals]
  (fmap #(apply % (map extract vals)) f))

(define fapply
  [~None]
  None)

(data Either (Right x) (Left x))

(define extract
  [(~Right value)]
  value)

(define fmap
  [f (~Right value)]
  (Right (f value)))

(define fmap
  [_ (~Left x)]
  (Left x))

(define bind
  [(~Right x) f]
  (f x))

(define bind
  [(~Left x) _]
  (Left x))

(define fapply
  [(~Left x)]
  (Left x))

(def >>= bind)

(def <$> fmap)

(def <*> fapply)

(def P partial)

(defmacro infix
  [& body]
  (if (= (count body ) 1)
    (first body)
    (let [[val f & rest] body]
      (if (empty? rest)
        `(~f ~val)
        `(~f ~val (infix ~@rest))))))

(defmacro -- [& body] `(infix ~@body))
