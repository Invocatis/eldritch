(ns eldritch.core
  (:require
   [eldritch.ardent]
   ; [eldritch.asterix]
   [eldritch.match]))

(defmacro data
  [& args]
  `(eldritch.ardent/data ~@args))

(defmacro define
  [& args]
  `(eldritch.match/define ~@args))

(defmacro match
  [& args]
  `(eldritch.match/match ~@args))

(def matches? eldritch.match/matches?)
