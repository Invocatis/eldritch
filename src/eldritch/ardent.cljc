(ns eldritch.ardent
  (:require
   [eldritch.match :refer [define]])
  (:refer-clojure :exclude [resolve future]))

(defmacro ->NoArgsType
  [name]
  `(deftype ~(symbol (str name "$TYPE"))
     Object
     (equals [this# that#] (= this# that#))
     (toString [_#] ~name)))

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
         (nth [_# index#] (if (= 0 index#) ~name (nth ~vals (dec index#))))
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
              Object
              (toString [_#] (str '~name))
              (equals [this# that#] (= (str this#) (str that#))))]
     (defn ~(symbol (str name "?")) [any#] (= (typeof any#) r#))
     (defmethod clojure.core/print-method (type r#)
       [_# writer#]
       (print-simple (str ~name) writer#))
     r#))

(defmacro constructor?
  [any]
  `(instance? eldritch.ardent.IConstructor ~any))

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
           (let [[constructor & protocols] (if (vector? definition) definition [definition])
                 protocols (concat type-protocols protocols)
                 [name args] (if (seq? constructor) [(first constructor) (rest constructor)] [constructor ()])]
             `(do
                (declare ~name)
                (->ArgsType ~type ~protocols ~name [~@args])
                (def ~name (->Constructor ~type ~name ~args)))))
       nil)))


; (-- (P + 4)
;     <$> inc
;     <$> (Just 1)
;     >>= (fn [x] (Right (inc x))))

(comment
 (do*
  x <- (Just 1)
  y <- (+ x 1))
 (data Maybe (Just x) Nothing))
