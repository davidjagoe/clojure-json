(ns test
  (:require (org.danlarkin [json :as json]))
  (:use (clojure test)))

;; setup JSON encoder-decoder checker test
(defmethod assert-expr :json=
  [msg form]
  `(let [values# (list ~@(next form))
         json-form# (first values#)
         json-string# (json/encode-to-str json-form#)
         decoded-string# (json/decode-from-str json-string#)
         result# (= json-form# decoded-string#)]
     (if result#
       (report {:type :pass
                :message ~msg
                :expected '~form
                :actual `(~'~'= ~json-form# ~decoded-string#)})
       (report {:type :fail
                :message ~msg
                :expected `(~'~'= ~json-form# ~decoded-string#)
                :actual (list '~'not= json-form# decoded-string#)}))
     result#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Basics         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest array
  (is (:json= [1 2 3 4 5])))

(deftest empty-object
  (is (:json= {})))

(deftest single-object
  (is (:json= {:foo 1})))

(deftest double-object
  (is (:json= {:bar 2 :foo 1})))

(deftest nested-array
  (is (:json= [[[[[[1 2 3 4 5]]]]]])))

(deftest nested-object
  (is (:json= {:bam {:foo 1} :baz {:bar 2}})))

(deftest array-of-objects
  (is (:json= [{:foo 1} {:bar 2}])))

(deftest empty-string
  (is (= (json/decode-from-str "")
         "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Numbers         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest single-object-int-key
  (is (= "{\"1\":1}"
         (json/encode-to-str {1 1}))))

(deftest single-object-bignum-key
  ;; Should be 1.0E25 but those idiots at microsoft don't think they
  ;; need to adhere to mere IEEE standards.
  (is (= "{\"1E+25\":1}"
         (json/encode-to-str {1e25 1}))))

(deftest easy-number
  (is (:json= 10)))

(deftest float-number
  (is (:json= 10.01)))

(deftest big-integer
  (is (:json= 100000000000000000000000000000)))

(deftest big-float
  (is (:json= 100000000000000000000000000000.01)))

(deftest small-exponent
  (is (:json= 1e2))
  (is (= (json/decode-from-str "1e3") 1000))
  (is (= (json/decode-from-str "-3.2e4") -32000)))

(deftest big-exponent
  (is (:json= 1e25)))

(deftest negative-exponent
  (is (:json= 1e-2))
  (is (= (json/decode-from-str "1e-2") 0.01))
  (is (= (json/decode-from-str "-1.04E-3") -0.00104)))

(deftest plus-sign-exponent
  (is (:json= 1e+2))
  (is (= (json/decode-from-str "1e+2") 100))
  (is (= (json/decode-from-str "1.49E+7") 14900000))
  (is (= (json/decode-from-str "-9.3e+0") -9.3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Collections      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest collections
  (is (:json= ["1 2 3"]))
  (is (:json= {:a 1 :b 2}))
  (is (:json= {:a "a" :b "b"}))
  ;; default set behaviour:
  (is (= ["a" "b"]
           (vec (sort (json/decode (json/encode #{"a" "b"}))))))
  ;; constant-time-lookup-preserving behaviour:
  (is (= {:a :a :b :b})
      (binding [org.danlarkin.json.encoder/*sets-as-maps* true]
        (json/decode (json/encode #{"a" "b"})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Unicode         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest unicode-chars
  (is (:json= "ٯ✈")))

(deftest escaped-unicode-chars
  (is (:json= "\u066f\u2708"))
  (is (= (json/decode-from-str "\"\\u00E5\"") "\u00e5")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    String escaping    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest string-escaping
  (is (:json= {:key "\""}))
  (is (= (json/encode-to-str "\"") "\"\\\"\"")) ; single double-quote gets escaped
  ;; make sure hash-map keys get string-escaped when encoding:
  (is (= (json/encode-to-str {(keyword "/\\\"") 42}) "{\"/\\\\\\\"\":42}"))
  (is (= (json/encode-to-str "\u009f\u0078\u0004\u003e\u001e\u0080\u0000")
         "\"\u009f\u0078\\u0004\u003e\\u001E\u0080\\u0000\""))
  (let [long-str (str "\u0000\u0007\u0008\u0009\u000A\u000B\u000C\u000D\u000E"
                      "\u001F\u0020\u0021\u0022\u0023\u005B\u005C\u005D\u2222")
        encoded-long-str (str "\"\\u0000\\u0007\\b\\t\\n\\u000B\\f\\r\\u000E\\u001F"
                              " !\\\"#\u005B\\\\\u005D\u2222\"")]
    (is (= (json/encode-to-str long-str) encoded-long-str))
    (is (= (json/decode-from-str encoded-long-str) long-str))
    ;; now run long-str through a round-trip test
    (is (:json= long-str))
    ;; don't loop infinitely if input has an unterminated string:
    (is (thrown? Exception (json/decode-from-str "\"\\\\\\\"")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Indenting       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest indenting
  (is (= (json/encode-to-str [{:foo 1},{:bar 2}] :indent 2)
         "[\n  {\n    \"foo\":1\n  },\n  {\n    \"bar\":2\n  }\n]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      All-in-one       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.json.org/JSON_checker/test/pass1.json
(deftest pass1
  (testing "file with unicode"
    (let [string (slurp "../test/pass1.json")
          decoded-json (json/decode-from-str string)
          encoded-json (json/encode-to-str decoded-json)
          re-decoded-json (json/decode-from-str encoded-json)]
      (is (= decoded-json re-decoded-json))))
  (testing "file with no unicode"
    (let [string (slurp "../test/no_unicode.json")
          decoded-json (json/decode-from-str string)
          encoded-json (json/encode-to-str decoded-json)
          re-decoded-json (json/decode-from-str encoded-json)]
      (is (= decoded-json re-decoded-json)))))

(run-tests)