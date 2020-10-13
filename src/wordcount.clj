(ns wordcount
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]))

(defn count-word
  [counts word]
  (update counts (string/lower-case word) (fn [count] (if count (inc count) 1))))

(defn count-line
  [acc line]
  (->>
   (string/split line #"\s+")
   (filter (partial re-matches #"[A-Za-z]+")) ; assuming english alphabet, no puncuation
   (reduce count-word acc)))

(defn wordcount
  [text]
  (reduce count-line {} (string/split-lines text)))

(defn output
  [counts]
  (->> counts
       (sort-by second >) ; descending
       (map
        (fn [[word count]]
          (println count word)))
       doall)
  nil)

(defn usage
  []
  (println "usage: wordcount <file>"))

(defn -main
  [& [path]]
  (if-let [text (and path (.exists (io/file path)) (slurp path))]
    (-> text wordcount output)
    (if-not path
      (usage)
      (println "File not found"))))

(defn shuffle*
  [coll]
  (if (empty? coll)
    ()
    (let [p (permute (rest coll))
          index (rand-int (count p))]
      (concat (take index p) (cons (first coll) (drop index p))))))

(defn random-integer-selection
  [m n]
  (->> (range n)
       rest           ; non-negative, drop 0
       shuffle*
       (take m)
       sort))

(defn sample
  [m coll]
  (take m (shuffle* coll)))

(defn substring?
  [s t]
  (let [t (seq t)]
    (loop [index 0
           s s]
      (cond
        (< (count s) (count t))
        -1

        (= (take (count t) s) t)
        index

        :else
        (recur (inc index) (rest s))))))

(defn substring?
  [s t]
  (let [tset (set t)]
    (loop [index (count t)]
      (println index (.charAt s index))
      (if-not (< index (count s))
        -1
        (if (contains? tset (.charAt s index))
          (if (= t (subs s (inc (- index (count t))) (inc index)))
            (inc (- index (count t)))
            (recur (inc index)))
          (recur (+ index (count t))))))))

(defn walk
  [graph from]
  (let [walkf (fn walkf [graph from seen]
                (let [paths (remove seen (get graph from))
                      seen (into seen paths)]
                  (if (empty? paths)
                    seen
                    (reduce into (map #(walkf graph % seen) paths)))))]
    (walkf graph from #{})))

(defn connected?
  [graph from val]
  (let [f (fn f [graph from val seen]
            (let [neighbors (remove seen (get graph from))
                  seen (into seen neighbors)]
              (if (some #{val} neighbors)
                true
                (if (empty? neighbors)
                  false
                  (some identity (map #(f graph % val seen) neighbors))))))]
    (boolean (f graph from val #{}))))

(defn node
  [graph n]
  (update graph n (fn [node] (if node node []))))

(defn assert-not-connected
  [graph n]
  (if (connected? graph n n)
    (throw (Exception. (str "Graph operation causes cyclic connection on [" n "]")))
    graph))

(defn parent
  [graph parent child]
  (if (= parent child)
    (throw (Exception. (str "Graph operation causes cyclic connection on [" parent "]")))
    (-> graph
        (node child)
        (node parent)
        (update parent conj child)
        (assert-not-connected parent)
        (assert-not-connected child))))

(defn print-graph
  [graph]
  (->> graph
       (sort-by key)
       (map (fn [[i c]] (apply str i " -> " (if (empty? c) "No children" (interpose ", " (sort c))))))
       (interpose \newline)
       (apply str)
       println))
