(ns eldritch.ardent
  (:require
   [eldritch.match :refer [define]])
  (:refer-clojure :exclude [resolve future]))

(defmacro ->ArgsType
  [parent protocols name vals]
  (let [meta-sym (gensym "meta_")]
    `(do
       (doseq [protocol# ~(vec (take-nth 2 protocols))]
         (prefer-method print-method clojure.lang.ISeq protocol#))
       (deftype ~(symbol (str name "$TYPE"))
         ~(vec (cons meta-sym vals))
         ~parent
         clojure.lang.ISeq
         (seq [_#] (cons ~name ~vals))
         (first [_#] (with-meta ~name ~meta-sym))
         (next [this#] (next (seq this#)))
         (more [this#] (next (seq this#)))
         (count [_#] ~(inc (count vals)))
         clojure.lang.Indexed
         (nth [_# index#] (if (= 0 index#) (with-meta ~name ~meta-sym) (nth ~vals (dec index#))))
         (nth [_# index# not-found#] (if (= 0 index#) ~name (nth ~vals (dec index#) not-found#)))
         clojure.lang.IObj
         (withMeta [_# meta#] (new ~(symbol (str name "$TYPE")) meta# ~@vals))
         (meta [_#] ~meta-sym)
         Object
         (equals [this# that#] (and (seq? that#) (= ~name (first that#)) (= ~vals (rest that#))))
         (equiv [this# that#] (and (seq? that#) (= ~name (first that#)) (= ~vals (rest that#))))
         (toString [_#] (str "(" ~name \space (apply str (interpose \space ~vals)) ")"))
         ~@protocols))))

(defmacro ->Type
  [name]
  (when-not (= name '_)
    `(defprotocol ~name)))

(def heritage
  (atom {}))

(defn inherits
  [parent child]
  (swap! heritage update parent (fn [children] (conj (or children #{}) child))))

(defn inherits?
  [parent child]
  (contains? (get @heritage parent) child))

(declare typeof)

(defonce constructor-protocol
  (defprotocol IConstructor))

(defmacro ->Constructor
  [parent name args]
  `(let [r# (reify
              clojure.lang.IFn
              ~(if (zero? (count args))
                 `(invoke [this#] this#)
                 `(invoke [this# ~@args] (with-meta (~(symbol (str name "$TYPE.")) nil ~@args) (meta this#))))
              IConstructor
              eldritch.match.IMatchable
              (runtime-match [pattern# target# continuation# environment#]
                (and
                 (or
                  (= pattern# target#)
                  (inherits? pattern# target#))
                 (if continuation#
                   (apply eldritch.match/runtime-match (conj continuation# environment#))
                   true)))
              (compile-match [pattern# target#]
                `(or
                  (= ~pattern# ~target#)
                  (inherits? ~pattern# ~target#)))
              Object
              (toString [_#] (str '~name))
              (equals [this# that#] (= (str this#) (str that#)))
              (hashCode [_#] (hash '~name)))]
     (defn ~(symbol (str name "?")) [any#] (= (typeof any#) r#))
     (defmethod clojure.core/print-method (type r#)
       [_# writer#]
       (print-simple (str ~name) writer#))
     r#))

(defn constructor?
  [any]
  (instance? eldritch.ardent.IConstructor any))

(defn typeof
  [val]
  (if (seq? val)
    (typeof (first val))
    val))

(defmacro data
  [typedef & definitions]
  (let [[type & type-protocols] (if (vector? typedef) typedef [typedef])]
    `(do
       (->Type ~type)
       ~@(for [definition definitions]
           (let [[constructor & heritage] (if (vector? definition) definition [definition])
                 [name args] (if (seq? constructor) [(first constructor) (rest constructor)] [constructor ()])
                 protocols (take-while (complement keyword?) (rest (drop-while #(not= :protocols %) heritage)))
                 constructors (take-while (complement keyword?) (rest (drop-while #(not= :inherits %) heritage)))]
             `(do
                (declare ~name)
                (->ArgsType ~type ~(concat type-protocols protocols) ~name [~@args])
                (def ~name (->Constructor ~type ~name ~args))
                (doseq [parent# ~(vec constructors)]
                  (inherits parent# ~name)))))
       nil)))


; (-- (P + 4)
;     <$> inc
;     <$> (Just 1)
;     >>= (fn [x] (Right (inc x))))

(comment
 (do*
  x <- (Just 1)
  y <- (+ x 1))
 (data Maybe (Just x) Nothing)

 (eldritch.ardent/->ArgsType Maybe (clojure.core/concat nil (clojure.core/remove eldritch.ardent/constructor? [])) None []))




(data Maybe (Some x) None)
(data AnotherMaybe
      [(Just x) :inherits Some]
      [Nothing :inherits None])
