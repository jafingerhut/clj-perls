(ns com.fingerhutpress.clj-perls.test
  (:use [com.fingerhutpress.clj-perls])
  (:use [clojure.test]))

(set! *warn-on-reflection* true)


;; TBD: Possible to test die here?  Perhaps via creating temporary
;; files containing Clojure source code invoking die, making them
;; executable, and using clojure.java.shell/sh to run them and get
;; their exit status and *err* output.


(deftest test-chr
  (is (= "A" (chr 65)))
  (is (= '( "\n" "\r" "\t" ) (map chr [10 13 9])))
  (is (= "The quick brown fox jumped over the lazy dog"
         (apply str (map chr [84 104 101 32 113 117 105 99 107 32 98
                              114 111 119 110 32 102 111 120 32 106
                              117 109 112 101 100 32 111 118 101 114
                              32 116 104 101 32 108 97 122 121 32 100
                              111 103]))))
  (is (= "\ud834\udd1e" (chr 0x1d11e)))
  (is (= "\ud83d\ude03 smiling face"
         (apply str (map chr [0x1f603 32 115 109 105 108 105 110 103 32 102
                              97 99 101]))))
  (is (thrown? IllegalArgumentException (chr -1)))
  (is (thrown? IllegalArgumentException (chr (inc Character/MAX_CODE_POINT))))
  (is (thrown? IllegalArgumentException (chr (Integer/MAX_VALUE))))
  (is (thrown? IllegalArgumentException (chr (Integer/MIN_VALUE)))))


;; See com.fingerhutpress.unicode unit tests for more testing of chr
;; and ord functions.


;; TBD: Test while-<> here?  Should be possible with input from files,
;; but perhaps not from *in*.


;; TBD: tests for splice


;; TBD: tests for substr.  I have already written some that do quite
;; exhaustive comparison to Perl's substr.  Perhaps better to do a
;; handful of simple unit tests here, and leave the exhaustive testing
;; to a separate shell script that invokes a Perl and Clojure program.
;; Or maybe invoke them from here with clojure.java.shell/sh?


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


(def SMILING_FACE_WITH_OPEN_MOUTH_STR (chr 0x1F603))

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
