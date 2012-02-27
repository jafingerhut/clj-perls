(ns com.fingerhutpress.clj-perls.test
  (:require [com.fingerhutpress.text.unicode :as uc])
  (:use [com.fingerhutpress.clj-perls])
  (:use [clojure.test]))

(set! *warn-on-reflection* true)


;; TBD: Possible to test die here?  Perhaps via creating temporary
;; files containing Clojure source code invoking die, making them
;; executable, and using clojure.java.shell/sh to run them and get
;; their exit status and *err* output.


;; TBD: Test while-<> here?  Should be possible with input from files,
;; but perhaps not from *in*.


;; TBD: tests for splice


;; TBD: I have written some tests for Clojure substr that do quite
;; exhaustive comparison to Perl's substr.  Perhaps do more exhaustive
;; testing in a separate shell script that invokes a Perl and Clojure
;; program?  Or maybe invoke them from here with
;; clojure.java.shell/sh?

(deftest test-substr
   (let [s "My loud parakeet is named Clarence"]
     (is (= (substr s 3 4)  "loud"))
     (is (= (substr s 3 -9) "loud parakeet is named"))
     (is (= (substr s 17)   "is named Clarence"))
     (is (= (substr s -4)   "ence"))
     (is (= (substr s -7 2) "la"))
     (is (= (substr s -7 -2) "laren"))
     (binding [*exception-on-surrogate-pair-splitting* true]
       (is (thrown? StringIndexOutOfBoundsException
                    (substr "\ud834\udd1e" 0 1)))
       (is (thrown? StringIndexOutOfBoundsException
                    (substr "\ud834\udd1e" -1))))
     (binding [*exception-on-surrogate-pair-splitting* false]
       (is (= (substr "\ud834\udd1e" 0 1) "\ud834"))
       (is (= (substr "\ud834\udd1e" -1) "\udd1e")))))


(deftest test-substr-replace
   (let [s "My loud parakeet is named Clarence"]
     (is (= (substr-replace s 3 4 "quiet")
            "My quiet parakeet is named Clarence"))
     (is (= (substr-replace s 3 -9 "cat")
            "My cat Clarence"))
     (is (= (substr-replace s 17 "tweeted")
            "My loud parakeet tweeted"))
     (is (= (substr-replace s -4 "issa")
            "My loud parakeet is named Clarissa"))
     (is (= (substr-replace s -7 2 "o")
            "My loud parakeet is named Corence"))
     (is (= (substr-replace s -7 -2 "andi")
            "My loud parakeet is named Candice"))
     (binding [*exception-on-surrogate-pair-splitting* true]
       (is (thrown? StringIndexOutOfBoundsException
                    (substr-replace "\ud834\udd1e" 0 1 "foo")))
       (is (thrown? StringIndexOutOfBoundsException
                    (substr-replace "\ud834\udd1e" -1 "foo"))))
     (binding [*exception-on-surrogate-pair-splitting* false]
       (is (= (substr-replace "\ud834\udd1e" 0 1 "foo") "foo\udd1e"))
       (is (= (substr-replace "\ud834\udd1e" -1 "foo") "\ud834foo")))))


(deftest test-split-on-space
  (doseq [f [split-on-space qw]]
    (is (= [] (f "")))
    (is (= ["Costs" "only" "$4.95"] (f "Costs only $4.95")))
    (is (= ["\ud834\udd1e" "foo"] (f "\t  \ud834\udd1e\nfoo")))
    (is (= ["nospaceshere"] (f "nospaceshere")))))


;; TBD: Test re-groups+

;; TBD: Perhaps extend re-groups+ to have an option to return (. m
;; (start)) and (. m (end)) numerical values, in addition to, or
;; instead of, substrings matched by parenthesized capture groups?


(deftest test-re-find+
  (is (= [ ""   ""     ""    ] (re-find+ #""    "")))
  (is (= [ ""   ""     ""    ] (re-find+ #"\s*" "")))
  (is (= [ ""   "   "  ""    ] (re-find+ #"\s*" "   ")))
  (is (= [ ""   ""     "a  " ] (re-find+ #"\s*" "a  ")))
  (is (= [ "a"  "  "   ""    ] (re-find+ #"\s+" "a  ")))
  (is (= ["" "The" " quick brown fox"] (re-find+ #"\w+" "The quick brown fox")))
  (is (= ["The" " quick" " brown"] (re-find+ #" \w+" "The quick brown")))
  (is (= ["T" "he quick brown fox" " quick brown fox" " fox" "!"]
           (re-find+ #"he(( \w+)+)" "The quick brown fox!")))
  )


;; TBD: test split-with-capture

;; TBD: test split


(def SMILING_FACE_WITH_OPEN_MOUTH_STR (uc/chr 0x1F603))

(deftest test-chomp
  (is (= "" (chomp "" \a)))
  (is (= "" (chomp "" "foo")))
  (is (= "" (chomp "" 0)))
  (is (= "b" (chomp "b" \a)))
  (is (= "b" (chomp "b" "foo")))
  (is (= "b" (chomp "b" 0)))
  (is (= "" (chomp "b" \b)))
  (is (= "" (chomp "b" "b")))
  (is (= "" (chomp "b" 98)))
  (is (= "Line ending with newline" (chomp "Line ending with newline\n" "\n")))
  (is (= "foo" (chomp (str "foo" SMILING_FACE_WITH_OPEN_MOUTH_STR)
                      SMILING_FACE_WITH_OPEN_MOUTH_STR)))
  (is (= "foo" (chomp (str "foo" SMILING_FACE_WITH_OPEN_MOUTH_STR)
                      0x1F603)))
  (is (= "foo" (chomp "foo\0" 0)))
  (is (= "A preposition is something you shouldn't end a sentence "
         (chomp "A preposition is something you shouldn't end a sentence with"
                "with"))))


;; TBD: test read-paragraph from a string reader, both true and false
;; cases of *auto-chomp*

;; TBD: test read-record, or perhaps only record-seq.  Do it from a
;; string reader.
