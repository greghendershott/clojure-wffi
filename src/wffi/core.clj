(ns wffi.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.string :as str]
            [markdown.core :as md]
            [pl.danieljanus.tagsoup :as tagsoup]
            [instaparse.core :as insta]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; request and response parsing

;; Note: Instaparse grammar rules in <angle-brackets> are omitted
;; from the tree -- i.e. they are intermediate rules that are helpful
;; for parsing, but usless noise in the produced tree. Some additional
;; simplification of the tree is done using insta/transform in
;; `parse-request-transform` below.
(def parse-request (insta/parser
                    "
request = <junk> method <ws>+ path [query] [headers [body]] <junk>

method = 'GET' | 'PUT' | 'POST' | 'DELETE' | 'OPTIONS'

path = path-segment+

<path-segment> = '/' (path-constant | variable)

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

<HEXDIG> = #'[0-9A-Fa-f]'
<ALPHA> = #'[A-Za-z]'
<DIGIT> = #'[0-9]'

query = <ws>* <question-mark> (<ws>* query-segment)+

query-segment = <ampersand>* (optional-query-segment | required-query-segment)
optional-query-segment = <open-bracket> query-segment-core <close-bracket>
required-query-segment = query-segment-core
query-segment-core = query-key <equal-sign> query-value

query-key = (ALPHA | DIGIT | '-' | '_' | '.')+
query-value = variable | query-value-constant
query-value-constant = (ALPHA | DIGIT | '-' | '_' | '.')+

headers = header*
header = <newline> (optional-header | required-header)
header-core = header-key <colon> <ws>* header-value
optional-header = <open-bracket> header-core <close-bracket>
required-header = header-core

header-key = (ALPHA | DIGIT | '-' | '_' | '.')+
header-value = variable | header-value-constant
header-value-constant = #'[^\n\\]]+'

body = #'\\n{2}.*$'

variable = <open-brace> #'[^{}]*' <close-brace>

newline = '\\n'
colon = ':'
equal-sign = '='
question-mark = '?'
ampersand = '&'
open-bracket = '['
close-bracket = ']'
open-brace = '{'
close-brace = '}'

<ws> = #'\\s'
<junk> = (ws | '\n')* 
"))

(defn parse-request-transform [x]
  (->> (parse-request x)
       (insta/transform {:query-key str
                         :path-constant str
                         :header-key str
                         ;; "lift" a few nested things
                         :header identity
                         :header-value identity
                         :query-segment identity
                         :query-segment-core (fn [& more] (seq more))
                         :header-core (fn [& more] (seq more))
                         })))

;; (pprint (parse-request "GET /users/{user}/item/{item}?pqr=0&q={q}&[r=2]"))

(pprint
 (parse-request-transform
  "
GET /user/{user}/items/{item}
    ?query-param={}
    &[optional-query-param={}]
    &constant-query-param=1
    &query-param-with-alias={alias}
    &[optional-constant-query-param=1]
Header: {}
Header-With-Alias: {alias}
Header-With-Constant-Value: Constant Value
[Optional-Header-With-Variable-Value: {}]
[Optional-Header-With-Constant-Value: 10000]

Entity"))

;;(pprint (parse-request "GET /\n[Header: {Value}]\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gather [pred? coll]
  (loop [result []
         coll coll]
    (cond (not (seq coll)) result

          (pred? (first coll))
          (let [[ys zs] (split-with (complement pred?) (rest coll))]
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
  (let [[intro more] (split-with #(not= (tagsoup/tag %) :h2) body)
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
