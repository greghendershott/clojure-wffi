(ns wffi.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [clojure.string :as str]
            [markdown.core :as md]
            [pl.danieljanus.tagsoup :as tagsoup]
            [instaparse.core :as insta]
            [clj-http.client :as client]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; request parsing

;; Note: Instaparse grammar rules in <angle-brackets> are omitted
;; from the tree -- i.e. they are intermediate rules that are helpful
;; for parsing, but usless noise in the produced tree. Some additional
;; simplification of the parse output is done using insta/transform in
;; `simplify-parsed-request` below.
(def raw-parse-request (insta/parser
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
header-core = header-key <colon> <ws>+ header-value
optional-header = <open-bracket> header-core <close-bracket>
required-header = header-core

header-key = (ALPHA | DIGIT | '-' | '_' | '.')+
header-value = header-value-constant | variable
header-value-constant = #'[^\n\\]]+'

body = #'\\n{2}.*$'

variable = <open-brace> #'[^ {}]*' <close-brace>

newline = '\\n'
colon = ':'
equal-sign = '='
question-mark = '?'
ampersand = '&'
open-bracket = '['
close-bracket = ']'
open-brace = '{'
close-brace = '}'

ws = #'\\s'
junk = (ws | '\n')*
"))

(defn- simplify-parsed-request
  [tree]
  (insta/transform
   {;; xform top level from hiccup vector to map
    :request (fn [& more]
               (reduce (fn [m [k & v]] (assoc m k v))
                       {}
                       more))
    ;; map-ize query segments
    :required-query-segment
    (fn [[k [_ v]]] {:required? true,:key k,:val v})
    :optional-query-segment
    (fn [[k [_ v]]] {:required? false,:key k,:val v})
    ;; map-ize headers
    :required-header
    (fn [[k v]] {:required? true, :key k, :val v})
    :optional-header
    (fn [[k v]] {:required? false, :key k, :val v})
    ;; map-ize variables
    :variable (fn [x] {:var (if (= x "") nil x)})
    ;; concat some chars to strings
    :query-key str
    :path-constant str
    :header-key str
    ;; "lift" a few nested things
    :header identity
    :header-value identity
    :query-segment identity
    :query-segment-core (fn [& more] (seq more))
    :header-core (fn [& more] (seq more))
    :query-value-constant identity
    :header-value-constant identity
    }
   tree))

(def parse-request (comp simplify-parsed-request raw-parse-request))

(comment
  (pprint (parse-request "GET /users/{user}/item/{item}?pqr=0&q={q}&[r=2]")))

(comment
  (pprint
   (parse-request
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

Request entity. Blah blah blah.")))

(comment
  (pprint (parse-request "GET /foo\nHeader: Value\nHeader: {Value}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- req-param-err [name]
  (throw
   (Exception.
    (str "required parameter `" name "' not supplied"))))

(defn- do-request
  "Takes an API request map as produced by parse-request, a map of
  parameters and their values, and an endpoint for the web service.
  Makes an HTTP request using clj-http."
  [a m endpoint]
  (let [[method] (:method a)
        paths (map (fn [x]
                     (if-let [k (:var x)]
                       (if-let [v (get m (keyword k))]
                         v
                         (req-param-err k))
                       x))
                   (:path a))
        queries (map (fn [x]
                       ;; If value is a non-nil variable, use as the
                       ;; key (i.e. it's an "alias") else use :key.
                       (let [k (or (get-in x [:val :var]) (get x :key))
                             v (get m (keyword k))]
                         (when-not v
                           (req-param-err k))
                         (str k "=" v)))
                     (:query a))
        heads (map (fn [x]
                     ;; If the value is a non-nil variable, use it
                     ;; as the key, otherwise use the key.
                     (let [k (or (get-in x [:val :var]) (get x :key))
                           v (get m (keyword k))]
                       (when-not v
                         (req-param-err k))
                       (str k ": " v "\n")))
                   (:headers a))
        body  ""] ;;TO-DO
    ;; TO-DO: Instead of forming the raw HTTP request bytes here, use
    ;; clj-http.
    (str method
         " "
         endpoint
         (apply str paths) ;FIXME: url-encoding
         (if (seq queries) "?" "") ;FIXME: Can nil pun here with `when`?
         (apply str (interpose "&" queries)) ;FIXME: url-encoding
         "\n"
         (apply str heads)
         "\n"
         body)))

;; Example
(comment
  (println
   (do-request
    {:method "GET"
     :path ["/"
            "user"
            "/"
            {:var "user"}
            "/"
            "items"
            "/"
            [:variable "item"]]
     :query [{:required? true, :key "query-param", :val {:var nil}}
             {:required? false, :key "optional-query-param", :val {:var nil}}
             {:required? true, :key "constant-query-param", :val "1"}
             {:required? true, :key "query-param-with-alias", :val {:var "alias"}}
             {:required? false, :key "optional-constant-query-param", :val "1"}]
     :headers [{:required? true, :key "Header", :val {:var nil}}
               {:required? true, :key "Header-With-Alias", :val {:var "alias"}}
               {:required? true,
                :key "Header-With-Constant-Value",
                :val "Constant Value"}
               {:required? false,
                :key "Optional-Header-With-Variable-Value",
                :val {:var nil}}
               {:required? false,
                :key "Optional-Header-With-Constant-Value",
                :val "10000"}]}
    {:user "Greg"
     :item "1"
     :query-param "42"
     :optional-query-param "52"
     :constant-query-param "62"
     :alias "72"
     :optional-constant-query-param "99"
     :Header "10"
     :Header-With-Alias "20"
     :Header-With-Constant-Value "30"
     :Optional-Header-With-Variable-Value "40"
     :Optional-Header-With-Constant-Value "50"}
    "endpoint")))

(defn make-api-fn
  "Takes a :request hiccup vector as produced by
  parse-request, and returns a function. The function takes
  a map of parameter names and values."
  [api-map endpoint]
  (fn [param-map]
    (do-request api-map param-map endpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gather [pred? coll]
  (loop [result []
         coll coll]
    (cond (not (seq coll)) result

          (pred? (first coll))
          (let [[ys zs] (split-with (complement pred?) (rest coll))]
            (recur (concat result
                           (list (list* (first coll) ys)))
                   zs))

          :else nil))) ;better yet, raise exception

(defn- md->bodies [fname]
  (let [[_ _ [_ _ & bodies]] ;FIXME: skip :html {}. get contents of [:body {} contents]
        (-> (md/md-to-html-string (slurp fname))
            tagsoup/parse-string)]
    (gather #(= (tagsoup/tag %) :h1) bodies)))


(defn- find-h2-and-pre [re coll]
  (->> coll
       (drop-while #(not (and (= (nth % 0) :h2)
                              (re-matches re (nth % 2)))))
       (drop-while #(not (= (nth % 0) :pre)))
       first
       tagsoup/children
       first
       str/trim))

(defn- body->api-func [endpoint body]
  (let [[intro more] (split-with #(not= (tagsoup/tag %) :h2) body)
        [[_ _ name] & desc] intro
        request-template (find-h2-and-pre #"Request:?" more)
        request-map (parse-request request-template)
        request-func (make-api-fn request-map endpoint)]
    {:name name
     :desc desc
     :request-template request-template ;just for debugging
     :request-map request-map           ;just for debuggin
     :request-func request-func
     :response/raw (find-h2-and-pre #"Response:?" more)}))

(def bs (md->bodies "/Users/greg/src/clojure/wffi/src/wffi/example.md"))
(def apis (map (partial body->api-func "ENDPOINT-TODO")
               bs))
;; (pprint apis)
;; (println)
;; (println (:request-func (first apis)))
(println ((:request-func (first apis))
          {:user "joe"
           :item 42
           :qp1 52
           :qp2 62
           :Header1 10
           :alias "alias-value"}))

;; TODO: Use a macro to actually `defn` a named function for each of
;; the web API :request-funcs.
