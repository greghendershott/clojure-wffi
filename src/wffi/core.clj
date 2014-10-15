(ns wffi.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.string :as str]
            [markdown.core :as md]
            [pl.danieljanus.tagsoup :as tagsoup]
            [instaparse.core :as insta]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; request and response parsing

(def parse-request (insta/parser
;; request = junk start-line [head* [body]] junk
;; start-line = method ws+ path ['?' query+] '\n'

          "
start-line = junk method ws+ path [query] [header* [body]] junk

method = 'GET' | 'PUT' | 'POST' | 'DELETE' | 'OPTIONS'

path = segment+

<segment> = '/' (path-constant | variable)

path-constant = path-char*

<path-char> = unreserved
            | pct-encoded
            | sub-delims

<pct-encoded> = '%' HEXDIG HEXDIG

<unreserved>    = ALPHA / DIGIT / '-' / '.' / '_' / '~'
<reserved>      = gen-delims / sub-delims
<gen-delims>    = ':' / '/' / '?' / '#' / '[' / ']' / '@'
<sub-delims>    = '!' / '$' / '&' / '(' / ')'
                  / '*' / '+' / ',' / ';' / '='

HEXDIG = #'[0-9A-Fa-f]'
<ALPHA> = #'[A-Za-z]'
DIGIT = #'[0-9]'

query = ws* '?' (ws* query-segment)+

query-segment = query-key '=' query-value
              | '&' query-segment
              | '[' query-segment ']'
query-key = (ALPHA | DIGIT | '-' | '_' | '.')+
query-value = variable | query-value-constant
query-value-constant = (ALPHA | DIGIT | '-' | '_' | '.')+

header = '\n' (bracketed-header | unbracketed-header)
bracketed-header = '[' header ']'
unbracketed-header = header-key ':' ws* header-value

header-key = (ALPHA | DIGIT | '-' | '_' | '.')+
header-value = variable | #'[^\n]+'

body = #'\\n{2}.*$'

variable = '{' #'[^{}]+' '}'
         | '{' ws* '}'

ws = #'\\s'
junk = (ws | '\n')* 
"))

(defn parse-request-transform [x]
  (->> (parse-request x)
       (insta/transform {:query-key str
                         :path-constant str})))

(pprint (parse-request "GET /users/{user}/item/{item}?pqr=0&q={q}&[r=2]"))
;; (pprint
;;  (parse-request
;;   "
;; GET /user/{user}/items/{item}
;;     ?query-param={}
;;     &[optional-query-param={}]
;;     &constant-query-param=1
;;     &[optional-constant-query-param=1]
;; Header: {}
;; Header-With-Alias: {alias}
;; Header-With-Constant-Value: Constant Value
;; "))
;; [Optional-Header-With-Variable-Value: {}]
;; [Optional-Header-With-Constant-Value: 10000]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       first
       str/trim))

(defn body->api-func [body]
  (let [[intro more] (split #(not= (tagsoup/tag %) :h2) body)
        [[_ _ name] & desc] intro
        m {:name name
           :desc desc
           :request/raw  (find-h2-and-pre #"Request:?" more)
           :request-method "TODO"
           :request-path   "TODO"
           :request-query  "TODO"
           :request-head   "TODO"
           :response/raw (find-h2-and-pre #"Response:?" more)
           :response-head  "TODO: Parse form request/raw"}]
    ;;(assoc m :request/tpl (parse-request (:request/raw m)))
    m))

;; (def bs (md->bodies "/Users/greg/src/clojure/wffi/src/wffi/example.md"))
;; (pprint (map body->api-func bs))
