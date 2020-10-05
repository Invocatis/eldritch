(ns eldritch.util)

(defn distinct-by
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (contains? @seen (f input))
            result
            (do (vswap! seen conj (f input))
                (rf result input))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen (f x))
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen (f x)))))))
                   xs seen)))]
     (step coll #{}))))
