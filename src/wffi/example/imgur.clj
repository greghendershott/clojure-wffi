(ns wffi.example.imgur
  (require wffi.core
           [clojure.pprint :refer :all]))

;; Try <kbd>C-c C-d d</kbd> with point on `stats`. Should see a doc string.

;; ;; Runtime example

;; (let [service (wffi.core/markdown->service "/Users/greg/src/webapi-markdown/imgur.md")
;;       stats (get-in service [:apis 'stats :request-function])]
;;   (pprint (stats {:view "today"})))

;; ;; Compile-time example

;; (wffi.core/defwrappers "/Users/greg/src/webapi-markdown/imgur.md")
;; (pprint (stats {:view "today"}))
