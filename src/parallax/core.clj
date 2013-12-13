(ns parallax.core
  (:require [numeric.expresso.core :as e]
            [clojure.core.logic :as logic]))


(defrecord Variable [type shape])

(defrecord Function [ex args return])

(defn valid-shape? [shape]
  (or (logic/lvar? shape)
      (and (vector? shape) (every? integer? shape))))

(defn variable
  ([]
     (variable (logic/lvar))) ;; Replace with lvar
  ([type]
     (variable type (logic/lvar))) ;; Replace with lvar
  ([type shape]
      {:pre [(valid-shape? shape)]}
      (Variable. type shape)))

(defn variable? [var]
  (= Variable (class var)))

(defn function [ex args return]
  (Function. ex args return))

(declare collect-variables)

(defn add-symbol
  [item frame]
  (assoc frame item (variable)))

(defn parse-item
  [item frame]
  (cond
   (e/expression? item) (collect-variables item frame)
   (symbol? item) (add-symbol item frame)
   :else frame))

(defn collect-variables
  ([ex]
     (collect-variables (rest ex) {}))
  ([ex frame]
     (if-let [item (first ex)]
       (recur (rest ex) (parse-item item frame))
       frame)))

(defn make-expression-function [ex]
  (let [return-var (variable)
        args (collect-variables ex)]
    (function ex args return-var)))

(defmacro defex [& body]
  `(make-expression-function (e/ex ~@body)))
