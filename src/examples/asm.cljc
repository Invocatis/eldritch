(ns examples.asm
  (:require
   [clojure.string :as string]
   [eldritch.ardent :refer [data]]
   [eldritch.match :refer [define match]]))

(data Expr
      (Mrk n)
      (Bne x y mrk)
      (Be x y mrk)
      (Mov reg value)
      (Add x y)
      (Mul x y)
      (Neg x)
      (Reg x)
      (Lit x))

(def ^:dynamic registers)
(def ^:dynamic marks)

(defn mov
  [r v]
  (vreset! (get registers r) v))

(defn $
  [r]
  @(get registers r))

(define evaluate
  ([(~Mrk n)])

  ([(~Bne (~Reg x) (~Lit y) (~Lit mrk))]
   (when-not (= ($ x) y)
     (mov "c" (get marks mrk))))

  ([(~Bne (~Reg x) (~Reg y) (~Lit mrk))]
   (when-not (= ($ x) ($ y))
     (mov "c" (get marks mrk))))

  ([(~Be (~Reg x) (~Lit y) (~Lit mrk))]
   (when (= ($ x) y)
     (mov "c" (get marks mrk))))

  ([(~Be (~Reg x) (~Reg y) (~Lit mrk))]
   (when (= ($ x) ($ y))
     (mov "c" (get marks mrk))))

  ([(~Mov (~Reg r) (~Lit v))]
   (mov r v))
  ([(~Mov (~Reg r0) (~Reg r1))]
   (mov r0 ($ r1)))

  ([(~Add (~Reg r) (~Lit v))]
   (mov r (+ ($ r) v)));
  ([(~Add (~Reg r0) (~Reg r1))]
   (mov r0 (+ ($ r0) ($ r1))))

  ([(~Mul (~Reg r) (~Lit v))]
   (mov r (* ($ r) v)));
  ([(~Mul (~Reg r0) (~Reg r1))]
   (mov r0 (* ($ r0) ($ r1))))

  ([(~Neg (~Reg x))]
   (mov x (- ($ x)))))

(defn registers!
  [n]
  (assoc
   (into {}
         (map vector
              (range 0 n)
              (repeatedly (fn [] (volatile! nil)))))
   "c" (volatile! 0)))

(defn run
  [marks body]
  (binding [registers (registers! 10)
            marks marks]
    (let [body (vec body)]
      (loop [value nil]
        (let [c ($ "c")]
          (mov "c" (inc c))
          (if (>= c (count body))
            value
            (recur (evaluate (nth body c)))))))))

(defn parse-arg
  [x]
  (if (= (first x) \$)
    (Reg
     (let [r (subs x 1)]
       (if (re-matches #"\d+" r)
         (Long. r)
         r)))
    (Lit
     (if (re-matches #"\d+" x)
       (Long. x)
       x))))

(define parse-line
  ([("bne" x y mrk)]
   (Bne x y mrk))
  ([("be" x y mrk)]
   (Be x y mrk))
  ([("add" x y)]
   (Add x y))
  ([("neg" x)]
   (Neg x))
  ([("mul" x y)]
   (Mul x))
  ([("mov" r v)]
   (Mov r v))
  ([("mrk" n)]
   (Mrk n)))

(defn compile-marks
  [code]
  (reduce
   (fn [acc [line no]]
     (match line
       ["mrk" name] (assoc acc name no)
       _ acc))
   {}
   (map vector code (range))))

(defn parse
  [text]
  (let [code (map #(string/split % #"\s+") (string/split-lines text))
        marks (compile-marks code)]
    (->> code
         (map (fn [[op & args]] (cons op (map parse-arg args))))
         (map parse-line)
         (vector marks))))

(def fibb
  (apply str
    (interpose \newline
               ["mov $0 1"
                "mov $1 1"
                "mov $3 0"
                "mrk fibb"
                "mov $2 $0"
                "add $0 $1"
                "mov $1 $2"
                "add $3 1"
                "bne $3 10 fibb"
                "mov $0 $0"])))
