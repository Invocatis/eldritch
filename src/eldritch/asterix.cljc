(ns eldritch.asterix
  (:require
   [clojure.walk :as walk]
   [eldritch.walk :refer [prewalk-with-accumulate any-match?]])
  (:refer-clojure :exclude [*]))

(defn *
  [dmap acc form]
  (prewalk-with-accumulate
    (fn [acc form]
      (if-let [dispatch (and (seq? form) (get dmap (first form)))]
        (dispatch acc form)
        (if-let [dispatch (get dmap nil)]
          (dispatch acc form)
          [acc form])))
    acc
    (walk/macroexpand-all form)))

(require '[eldritch.ardent :refer [Just fapply fmap bind]])

(defn unquoted? [x] (and (seq? x) (= 'clojure.core/unquote (first x))))

(defn quoted? [x] (and (seq? x) (= 'clojure.core/quote (first x))))

(defn has-unquote?
  [acc val]
  (any-match?
   (fn [form]
     (if (quoted? form)
       (reduced false)
       ((some-fn acc unquoted?) form)))
   val))

(defn do*-impl
  [form acc]
  (let [form (walk/macroexpand-all form)]
    (*
     {'let* (fn [acc [_ bindings :as let*]]
              [(reduce
                (fn [acc [sym val]]
                  (cond
                    (has-unquote? acc val)
                    (conj acc sym)
                    :else

                    (disj acc sym)))
                acc
                (partition 2 bindings))
               let*])

      'quote
      (fn [acc [_ form :as quoted]]
        [acc
         (if (contains? acc form)
           (reduced form)
           (reduced quoted))])

      'clojure.core/unquote
      (fn f [acc [_ form]]
        (cond

          (vector? form)
          (f acc (cons `vector form))

          (seq? form)
          [acc
           (let [{_consts false, args true}
                 (->> form
                      (group-by (comp boolean (partial has-unquote? acc))))
                 args-syms (into {} (map vector args (repeatedly gensym)))]
             (if-not (empty? args)
               (reduced
                (reduce
                 (fn [facc arg]
                   `(bind
                     ~(do*-impl arg acc)
                     (fn [~(get args-syms arg)]
                       ~facc)))
                 `(~@(map (fn [x] (get args-syms x x)) form))
                 args))
               form))]

          :else
          [(conj acc form) form]))


      nil (fn f [acc form]
            (cond
              (not (coll? form))
              [acc form]

              (not (has-unquote? acc form))
              [acc form]

              (vector? form)
              (f acc (cons `vector form))

              (seq? form)
              [acc
               (if (has-unquote? acc form)
                 (let [{_consts false, args true}
                       (->> form
                            (group-by (comp boolean (partial has-unquote? acc))))
                       args-syms (into {} (map vector args (repeatedly gensym)))]
                   (if (= (count args) 1)
                     `(fmap
                       (fn [~(first (vals args-syms))]
                         ~(map #(get args-syms % %) form))
                       ~(first (keys args-syms)))
                     `(fapply
                       (Just
                        ~(reduce
                          (fn [facc arg]
                            `(fn [~(get args-syms arg)]
                               ~facc))
                          (map #(get args-syms % %) form)
                          (reverse args)))
                       ~@args)))
                 form)]))}
     acc
     form)))

(defmacro do*
  [form]
  (do*-impl form #{}))
