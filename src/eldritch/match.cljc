(ns eldritch.match
  (:require
    [eldritch.util :refer [distinct-by]]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]
    [eldritch.walk :as ewalk]))

(defn variable?
  [any]
  (and (symbol? any)
       (not= '& any)))
       ; (not (uppercase-letter? (first (name any)))) #_(= \? (first (name any)))))

(defn invoke?
  [any]
  (and (symbol? any) (= \! (first (name any)))))

(defprotocol IMatchable
  (runtime-match [pattern target continuation environment])
  (compile-match [pattern target]))

(def ^:dynamic join)

(extend-type clojure.lang.APersistentMap
  IMatchable
  (runtime-match [pattern target continuation environment]
                 (if (empty? pattern)
                   (empty? target)
                   (let [[pk pv] (first pattern)]
                     (and
                      (map? target)
                      (runtime-match pv (get target pk) [(dissoc pattern pk) (dissoc target pk) continuation] environment)))))

  (compile-match [pattern target]
                 (if (empty? pattern)
                   `(empty? ~target)
                   (let [[pk pv] (first pattern)]
                     `(and
                       (map? ~target)
                       ~(compile-match pv `(get ~target ~pk))
                       ~(compile-match (dissoc pattern pk) `(dissoc ~target ~pk)))))))

(extend-type clojure.lang.PersistentVector
  IMatchable
  (runtime-match [pattern target continuation environment]
                 (and
                  (vector? target)
                  (runtime-match (seq pattern) (seq target) continuation environment)))
  (compile-match [pattern target]
                 `(and
                   (vector? ~target)
                   ~(compile-match (seq pattern) `(seq ~target)))))


(let [runtime (fn [pattern target continuation environment]
                (cond
                  (empty? pattern)
                  (and (empty? target)
                       (if-not (empty? continuation)
                         (apply runtime-match (conj continuation environment))
                         true))

                  (= `unquote (first pattern))
                  (runtime-match (eval (second pattern)) target continuation environment)

                  (-> (first pattern) meta ::...)
                  (if-not (empty? continuation)
                    (and
                     (if (contains? environment (first pattern))
                       (runtime-match (get environment (first pattern)) target continuation environment)
                       true)
                     (apply runtime-match (conj continuation (assoc environment (first pattern) target))))
                    true)

                  (and (= 'and (first pattern)))
                  (and
                   (loop [pattern (rest pattern)
                          acc ()]
                     (if (empty? pattern)
                       (every? boolean acc)
                       (recur
                         (rest pattern)
                         (cons (runtime-match (first pattern) target continuation environment) acc))))
                   (apply runtime-match (conj continuation environment)))

                  :else
                  (and
                   (seq? target)
                   (runtime-match (first pattern) (first target)
                                  [(rest pattern) (rest target) continuation]
                                  environment))))

      compile (fn [pattern target]
                (cond
                  (empty? pattern)
                  `(empty? ~target)

                  (= `unquote (first pattern))
                  `(runtime-match ~(second pattern) ~target nil nil)

                  (-> (first pattern) meta ::...)
                  true

                  ; (and (= 'and (first pattern)))
                  ; `(and
                  ;   (loop [pattern (rest pattern)
                  ;          acc ()]
                  ;     (if (empty? pattern)
                  ;       (cons `and acc)
                  ;       (recur
                  ;         (rest pattern)
                  ;         (cons (compile-match ~(first pattern) ~target nil) acc))))
                  ;   (apply compile-match continuation))

                  :else
                  `(and
                    (seq? ~target)
                    ~(compile-match (first pattern) `(first ~target))
                    ~(compile-match (rest pattern) `(rest ~target)))))]

  (extend-type clojure.lang.PersistentVector$ChunkedSeq
    IMatchable
    (runtime-match [pattern target continuation environment]
                   (runtime pattern target continuation environment))

    (compile-match [pattern target]
                   (compile pattern target)))

  (extend-type clojure.lang.PersistentList
    IMatchable
    (runtime-match [pattern target continuation environment]
                   (runtime pattern target continuation environment))

    (compile-match [pattern target]
                   (compile pattern target)))

  (extend-type clojure.lang.LazySeq
    IMatchable
    (runtime-match [pattern target continuation environment]
                   (runtime pattern target continuation environment))

    (compile-match [pattern target]
                   (compile pattern target))))

(extend-type clojure.lang.Symbol
  IMatchable
  (runtime-match [pattern target continuation environment]
                 (cond
                   (= pattern '_)
                   (apply runtime-match (conj continuation environment))

                   (contains? environment pattern)
                   (runtime-match (get environment pattern) target continuation environment)

                   :else
                   (or
                    (empty? continuation)
                    (apply runtime-match (conj continuation (assoc environment pattern target))))))

  (compile-match [pattern target] true))

(extend-type nil
  IMatchable
  (runtime-match [pattern target continuation environment]
                 (nil? target))

  (compile-match [pattern target]
                 `(nil? ~target)))

(extend-type java.lang.Object
  IMatchable
  (runtime-match [pattern target continuation environment]
                 (and
                  (= pattern target)
                  (or
                   (empty? continuation)
                   (apply runtime-match (conj continuation environment)))))
  (compile-match [pattern target]
                 `(and
                   (= ~pattern ~target))))

(defn precompile
  [pattern]
  (condp apply [pattern]
    seq?
    (let [i (.indexOf pattern '&)
          pattern (if-not (neg? i)
                    (concat (take i pattern)
                            (cons
                             (with-meta (nth pattern (inc i)) {::... true})
                             (drop (+ i 2) pattern)))
                    pattern)]
      (map precompile pattern))

    vector?
    (let [i (.indexOf pattern '&)
          pattern (if-not (neg? i)
                    (concat (take i pattern)
                            (cons
                             (with-meta (nth pattern (inc i)) {::... true})
                             (drop (+ i 2) pattern)))
                    pattern)]
      (mapv precompile pattern))

    map?
    (into {} (map (fn [[k v]] [(precompile k) (precompile v)]) pattern))

    pattern))

(defn matches?
  [pattern target]
  (runtime-match (precompile pattern) target nil {}))

(defn paths
  [v]
  (mapv
   vec
   (cond
     (map? v)
     (mapcat
      (fn [[k1 v1]]
        (->> (paths v1)
             (map (fn [p]
                    (cons k1 p)))))
      v)

     (sequential? v)
     (if (= (first v) 'and)
       (paths (last v))
       (mapcat
        (fn [i v1]
          (->> (paths v1)
               (map (fn [p]
                      (cons i p)))))
        (range)
        v))

     :else
     [[]])))

(defn resolve-path
  [form path]
  (loop [sentinel (Object.)
         m form
         ks (seq path)]
    (if ks
      (let [m (if (seq? m)
                (nth m (first ks))
                (get m (first ks) sentinel))]
        (if (identical? sentinel m)
          nil
          (recur sentinel m (next ks))))
      m)))

(defn resolve-variable-path
  [form path]
  (loop [sentinel (Object.)
         m form
         ks (seq path)]
    (if (and (seq? m) (= 'and (first m)))
      (recur sentinel (last m) ks)
      (if ks
        (if (fn? (first ks))
          (recur sentinel (apply (first ks) [m]) (next ks))
          (cond
            (seq? m)
            (cond
              (= 'and (first m))
              (recur sentinel (last m) ks)
              (#{'quote `unquote} (first m))
              nil
              :else
              (let [m (nth m (first ks))]
                (if (identical? sentinel m)
                  nil
                  (recur sentinel m (next ks)))))

            :else
            (let [m (get m (first ks) sentinel)]
              (if (identical? sentinel m)
                nil
                (recur sentinel m (next ks))))))
        m))))

(defn variables
  [form]
  (let [paths (paths form)
        var-paths (->> paths
                       (map
                        (fn [p]
                          (let [leaf (resolve-variable-path form p)]
                            (if (variable? leaf)
                              [leaf
                               (if (-> leaf meta ::...)
                                 (conj (pop p) `(partial drop ~(peek p)))
                                 p)]
                              nil))))
                       (remove nil?))]
    var-paths))

(defn destructure-variables
  [form value-symbol]
  (vec
   (mapcat
    (fn [[v path]]
      [v `(resolve-variable-path ~value-symbol ~path)])
    (variables form))))

(declare optimize)

(defmacro match
  [expr & branches]
  (let [optimize identity #_(if (bound? #'optimize) optimize identity)
        emit (fn emit [& branches]
               (cond
                 (empty? branches)
                 `(throw (IllegalArgumentException. (str "No matching clause: " ~expr)))

                 (= 1 (count branches))
                 `(match ~expr ~(first branches) true ~'_ false)

                 :else
                 (let [[form then & branches] branches
                       form (precompile form)
                       expr-sym (gensym)]
                   `(let [~expr-sym ~expr]
                      ~(let [vars (destructure-variables form expr-sym)]
                         `(if ~(optimize (compile-match form expr-sym))
                            ~(if-not (empty? vars)
                               `(and
                                 ~@(map
                                    (fn [[_ vals]] `(= ~@(map (fn [[_ path]] path) vals)))
                                    (group-by first (partition 2 vars)))
                                 (let ~(vec (mapcat identity (distinct-by first (partition 2 vars))))
                                   ~then))
                               then)
                            ~(apply emit branches)))))))]
    (apply emit branches)))

(defn optimize-0
  [form]
  form
  (walk/postwalk
   (fn [form]
     (match form
       (`resolve-path x []) x
       ('if true x y)  x
       ('if false x y) y
       (`let [] x) x
       (`rest (`drop n x)) `(drop ~(inc n) ~x)
       (`rest x) `(drop 1 ~x)
       (`empty? (`drop n x)) `(= (count ~x) ~n)
       (`pop (`subvec x start end)) `(subvec ~x ~start (dec ~end))
       (`pop (`pop x)) `(subvec ~x 0 (- (count ~x) 3))
       (`vector? (`subvec _ _ _)) true
       (`vector? (`pop _)) true
       (`seq? (`drop _ _)) true
       _ form))
   form))

(defn optimize-1
  [form]
  (walk/postwalk
   (fn [form]
     (match form
       (`empty? (`subvec x start end)) `(= (dec ~start) ~end)
       (`peek (`subvec x 0 end)) `(nth ~x (dec ~end))
       _ form))
   form))

(defn optimize-2
  [form]
  (walk/postwalk
   (fn [form]
     (match form
       (`dec (`- x n)) `(- ~x ~(inc n))
       (`dec x) `(- ~x 1)
       (`= x (`- y z)) `(= (+ ~x ~z) ~y)
       (`and & x) `(and ~@(remove true? x))
       (`+ (and !int? x) (and !int? y)) (+ x y)
       _ form))
   form))

(defn optimize-3
  "Some optimizations at certain levels result in performance drops, not increases
   For example,
    (pop (pop [1 2 3 4]))

   is faster than
    (let [x [1 2 3 4]]
      (subvec x 0 (- (count x) x)))

  This step reverts these forms"
  [form]
  (walk/postwalk
   (fn [form]
     (match form

       _ form))
   form))

(defn optimize
  [form]
  (let [opt (comp optimize-3 optimize-2 optimize-1 optimize-0)]
    (opt form)
    #_(loop [last form
             form (opt form)]
        (if (= last form)
          form
          (recur form (opt form))))))

(defprotocol IDefine
  (add-impl [this pattern f])
  (impls [this]))

(defmacro ^:private invoke
  [f & args]
  `(if (nil? ~f)
     (throw (Exception. (str "No matching impl " ~@(interpose \space args))))
     (.invoke ~f ~@args)))

(defn specificity
  [pattern]
  (cond
    (variable? pattern) 0
    (coll? pattern) (apply + 1 (map specificity pattern))
    :else 1))

(defn define-fn
  [name]
  (let [impls (atom [])
        get-fn (fn [& args]
                 (let [args (vec args)]
                   (second (first (filter (fn [[p f]] (matches? p args))
                                          (reverse (sort-by (fn [[p f]] (specificity p)) @impls)))))))]
    (reify
      IDefine
      (add-impl [this pattern f] (swap! impls conj [pattern f]))
      (impls [this] @impls)
      clojure.lang.IFn
      (invoke [this]
              (invoke
               (get-fn)))
      (invoke [this a]
              (invoke
               (get-fn a)
               a))
      (invoke [this a b]
              (invoke
               (get-fn a b)
               a b))
      (invoke [this a b c]
              (invoke
               (get-fn a b c)
               a b c))
      (invoke [this a b c d]
              (invoke
               (get-fn a b c d)
               a b c d))
      (invoke [this a b c d e]
              (invoke
               (get-fn a b c d e)
               a b c d e))
      (invoke [this a b c d e f]
              (invoke
               (get-fn a b c d e f)
               a b c d e f))
      (invoke [this a b c d e f g]
              (invoke
               (get-fn a b c d e f g)
               a b c d e f g))
      (invoke [this a b c d e f g h]
              (invoke
               (get-fn a b c d e f g h)
               a b c d e f g h))
      (invoke [this a b c d e f g h i]
              (invoke
               (get-fn a b c d e f g h i)
               a b c d e f g h i))
      (invoke [this a b c d e f g h i j]
              (invoke
               (get-fn a b c d e f g h i j)
               a b c d e f g h i j))
      (invoke [this a b c d e f g h i j k]
              (invoke
               (get-fn a b c d e f g h i j k)
               a b c d e f g h i j k))
      (invoke [this a b c d e f g h i j k l]
              (invoke
               (get-fn a b c d e f g h i j k l)
               a b c d e f g h i j k l))
      (invoke [this a b c d e f g h i j k l m]
              (invoke
               (get-fn a b c d e f g h i j k l m)
               a b c d e f g h i j k l m))
      (invoke [this a b c d e f g h i j k l m n]
              (invoke
               (get-fn a b c d e f g h i j k l m n)
               a b c d e f g h i j k l m n))
      (invoke [this a b c d e f g h i j k l m n o]
              (invoke
               (get-fn a b c d e f g h i j k l m n o)
               a b c d e f g h i j k l m n o))
      (invoke [this a b c d e f g h i j k l m n o p]
              (invoke
               (get-fn a b c d e f g h i j k l m n o p)
               a b c d e f g h i j k l m n o p))
      (invoke [this a b c d e f g h i j k l m n o p q]
              (invoke
               (get-fn a b c d e f g h i j k l m n o q)
               a b c d e f g h i j k l m n o q))
      (invoke [this a b c d e f g h i j k l m n o p q r]
              (invoke
               (get-fn a b c d e f g h i j k l m n o q r)
               a b c d e f g h i j k l m n o q r))
      (invoke [this a b c d e f g h i j k l m n o p q r s]
              (invoke
               (get-fn a b c d e f g h i j k l m n o q r rest)
               a b c d e f g h i j k l m n o q r rest)))))

(def get-define-fn (memoize define-fn))

(defn qualify
  [sym]
  (symbol (name (ns-name *ns*)) (name sym)))

(defmacro define
  ([name] `(get-define-fn '~(qualify name)))
  ([name & df]
   (cond
     (every? seq? df)
     (cons
      `do
      (map
       (fn [df]
         `(define ~name ~@df))
       df))

     (vector? (first df))
     (let [formals (first df)
           body (rest df)
           actuals
           (if-not (neg? (.indexOf formals '&))
             (concat (repeatedly (- (count formals) 2) gensym) ['& (gensym)])
             (repeatedly (count formals) gensym))]
       `(let [f# (get-define-fn '~(qualify name))]
          (add-impl f# '~formals (fn [~@actuals] (match [~@(remove #{'&} actuals)] [~@(remove #{'&} formals)] (do ~@body))))
          (def ~name f#)))

     :else (throw (Exception. "Malformed define")))))



(define fibb [0] 1)
(define fibb [1] 1)
; (define fibb [!neg?] (throw (Exception. "Undefined fibbonacci for negative input")))
(define fibb [n] (+ (fibb (- n 1)) (fibb (- n 2))))
;
;
; (defn fizzbuzz
;   [n]
;   (doseq [n (range 1 n)]
;     (println
;      (match [(mod n 3) (mod n 5)]
;             [0 0] "FizzBuzz"
;             [0 _] "Fizz"
;             [_ 0] "Buzz"
;             [_ _] n))))


(require '[criterium.core :as crit])

(defn bench
  []
  (let [x (vec (range 10))]
    (crit/bench (nth x (- (count x) 2)))
    (crit/bench (peek (pop x)))))
