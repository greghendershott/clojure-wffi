(ns wffi.core
  (:require [clojure.java.io :as io]
            [markdown.core :as md]
            [pl.danieljanus.tagsoup :as tagsoup]))

(defn split
  "FIXME: This is the conceptual, inefficient implementation. Should
  reimplement like Racket's split-at."
  [pred coll]
  [(take-while pred coll)
   (drop-while pred coll)])

(defn gather [pred? coll]
  (loop [result []
         coll coll]
    (cond (not (seq coll)) result

          (pred? (first coll))
          (let [[ys zs] (split (complement pred?) (rest coll))]
            (recur (concat result
                           (list (list* (first coll) ys)))
                   zs))

          :else nil))) ;better yet, raise exception

(defn md->bodies [fname]
  (let [[_ _ [_ _ & bodies]] ;skip :html {}. get contents of [:body {} contents]
        (-> (md/md-to-html-string (slurp fname))
            tagsoup/parse-string)]
    (gather #(= (tagsoup/tag %) :h1) bodies)))


(defn find-h2-and-pre [re coll]
  (->> coll
       (drop-while #(not (and (= (nth % 0) :h2)
                              (re-matches re (nth % 2)))))
       (drop-while #(not (= (nth % 0) :pre)))
       first
       tagsoup/children
       first))

(defn body->api-func [body]
  (let [[intro more] (split #(not= (tagsoup/tag %) :h2) body)
        [[_ _ name] & desc] intro]
    {:name name
     :desc desc
     :request/raw  (find-h2-and-pre #"Request:?" more)
     :request-method "TODO: Parse form request/raw"
     :request-path   "TODO: Parse form request/raw"
     :request-query  "TODO: Parse form request/raw"
     :request-head   "TODO: Parse form request/raw"
     :response/raw (find-h2-and-pre #"Response:?" more)
     :response-head  "TODO: Parse form request/raw"}))

(def bs (md->bodies "/Users/greg/src/clojure/wffi/src/wffi/example.md"))
(println (map body->api-func bs))
