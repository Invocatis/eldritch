(ns eldritch.walk
  (:require
   [clojure.walk :as walk])
  (:refer-clojure :exclude [find]))

(defn prewalk-with-accumulate
  [f acc form]
  (let [[acc form] (f acc form)]
    (walk/walk (partial prewalk-with-accumulate f acc)
               (fn [val] (if (reduced? val) @val val))
               form)))

(defn find
  [f form]
  (let [impl (fn impl [f form]
               (walk/walk (partial impl f)
                          (fn [val]
                            (if (and (seqable? val) (not (string? val)))
                              (if (some #(and (reduced? %) @%) val)
                                (reduced true)
                                (if (f val)
                                  (reduced true)
                                  val))
                              (if (f val)
                                (reduced true)
                                val)))
                          form))
        result (impl f form)]
    (if (reduced? result)
      @result
      nil)))

(defn any-match?
  [f form]
  (boolean (find f form)))

(defn walk-remove-leaves
  [pred form]
  (walk/walk
   (partial walk-remove-leaves pred)
   (fn [form]
     (cond
       (map-entry? form)
       form

       (map? form)
       (into (empty form) (remove (fn [[k v]] (or (pred k) (pred v))) form))

       (coll? form)
       (into (empty form) (remove pred form))

       :else
       form))
   form))
