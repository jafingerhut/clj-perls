(ns com.fingerhutpress.clj-perls
  (:require [clojure.string :as str]
            [com.fingerhutpress.text.unicode :as uc]))

(set! *warn-on-reflection* true)

(def ^{:dynamic true} *exception-on-surrogate-pair-splitting* true)

(defn die
  "Print string msg to *err*, then exit.

   TBD: Document reasons for the way exit-status is converted to the
   System/exit argument.  I think I based it upon how Perl's die does
   it."
  ([msg]
     (die msg 0))
  ([msg exit-status]
     (binding [*out* *err*] 
       (printf "%s" msg)
       (flush))
     (let [shifted-status (bit-shift-right exit-status 8)]
       (System/exit (if (zero? shifted-status) 255 shifted-status)))))


(defmacro while-<>
  "Approximately like Perl's:

   while (<>) {
       # body here
   }

   Example to print all lines containing palindromes of length 5 or
   longer:

   (while-<> [file line]
     (when (and (= line (str/reverse line))
                (>= (count line) 5))
       (printf \"%s\n\" line)))

   It will read from a list of filenames on the command line, found in
   *command-line-args*, if there are any, or from *in* if there are
   not.  The symbol file is bound to the string containing the current
   file name, if *command-line-args* contained at least one file name,
   otherwise it is bound to *in*.

   Unlike Perl, each line never has the line terminator at the end.
   This does not implement the record separator feature $/ of Perl,
   and there is no line number automatically kept track of for you
   like Perl's $."
  [[file line] & body]
  `(doseq [~file (or *command-line-args* [*in*])]
     (with-open [rdr# (clojure.java.io/reader ~file)]
       (doseq [~line (line-seq rdr#)]
         ~@body))))


(defn ps-start-end
  "This helper function converts offset and length arguments as used
   in Perl's splice() and substr() function, into Clojure start and
   end arguments for subvec and subs.  It handles negative offset and
   length values as those Perl functions do, i.e. as offsets from the
   end of the string.

   It is a bit of a mess because of all of the conditions to check.
   There is likely code much like this buried inside of Perl's
   implementation of substr and splice."
  ([n offset]
     (cond (neg? offset) [(max 0 (+ n offset)) n]
           (> offset n) nil
           :else [offset n]))
  ([n offset c]
     (let [start (if (neg? offset)
                   (+ n offset)
                   offset)
           end (if (neg? c)
                 (+ n c)
                 (+ start c))]
       (cond (neg? start) (if (neg? end)
                            nil
                            [0 (min n end)])
             (> start n) nil
             :else [start (min n (max start end))]))))


(defn splice-helper [v start end coll-to-insert]
  (vec (concat (subvec v 0 start)
               coll-to-insert
               (subvec v end))))


;; Here we implement splice that takes a vector and either just an
;; offset, an offset and a length, or offset, length, and collection
;; of items to splice in.
(defn splice
  ([v offset]
     (when-let [[start end] (ps-start-end (count v) offset)]
       (splice-helper v start end [])))
  ([v offset length]
     (splice v offset length []))
  ([v offset length coll-to-insert]
     (when-let [[start end] (ps-start-end (count v) offset length)]
       (splice-helper v start end coll-to-insert))))


;; TBD: This version of substr works with offset and length given in
;; units of UTF-16 code units.  If you want one that works in units of
;; Unicode code points, this isn't it.

(defn substr
  "Like Perl's substr(s,offset) and substr(s,offset,length).

   Extracts a substring of s and returns it.  The first character is
   at offset 0.  If offset is negative, starts that many characters
   from the end of the string.  If length is omitted, returns
   everything to the end of the string.  If length is negative, leaves
   that many characters off the end of the string.

   Examples:

   (def s \"My loud parakeet is named Clarence\")
   (substr s 3 4)    ; \"loud\"
   (substr s 3 -9)   ; \"loud parakeet is named\"
   (substr s 17)     ; \"is named Clarence\"
   (substr s -4)     ; \"ence\"
   (substr s -7 2)   ; \"la\"
   (substr s -7 -2)  ; \"laren\"

   Note that offset and length are in units of UTF-16 code units,
   i.e. Java chars, not in units of Unicode code points.  If you
   specify extracting a substring that takes one char of a surrogate
   pair but not the other, that is probably a bug on your part.
   substr will throw an exception in this case if
   *exception-on-surrogate-pair-splitting* is true.

   See also: substr-replace if you want the ability to replace a
   substring, or cp-substr if you want offset and length in units of
   Unicode code points, not Java chars."
  ([s offset]
     (when-let [[start end] (ps-start-end (count s) offset)]
       (let [x (subs s start end)]
         (when (and *exception-on-surrogate-pair-splitting*
                    (uc/bad-surrogate-at-either-end? x))
           (throw (StringIndexOutOfBoundsException.)))
         x)))
  ([s offset length]
     (when-let [[start end] (ps-start-end (count s) offset length)]
       (let [x (subs s start end)]
         (when (and *exception-on-surrogate-pair-splitting*
                    (uc/bad-surrogate-at-either-end? x))
           (throw (StringIndexOutOfBoundsException.)))
         x))))


(defn substr-replace
  "Like Perl's \"substr(s,offset) = replacement\" or
   \"substr(s,offset,length) = replacement\", except that instead
   of modifying the (immutable) string s, it returns a string that
   is the same as s except with the specified substring replaced with
   the string replacement.

   Examples:

   (def s \"My loud parakeet is named Clarence\")
   (substr-replace s 3 4 \"quiet\")   ; \"My quiet parakeet is named Clarence\"
   (substr-replace s 3 -9 \"cat\")    ; \"My cat Clarence\"
   (substr-replace s 17 \"tweeted\")  ; \"My loud parakeet tweeted\"
   (substr-replace s -4 \"issa\")     ; \"My loud parakeet is named Clarissa\"
   (substr-replace s -7 2 \"o\")      ; \"My loud parakeet is named Corence\"
   (substr-replace s -7 -2 \"andi\")  ; \"My loud parakeet is named Candice\"

   Note that offset and length are in units of UTF-16 code units,
   i.e. Java chars, not in units of Unicode code points.  If you
   specify extracting a substring that takes one char of a surrogate
   pair but not the other, that is probably a bug on your part.
   substr will throw an exception in this case if
   *exception-on-surrogate-pair-splitting* is true.

   See also: cp-substr-replace if you want offset and length in units
   of Unicode code points, not Java chars."
  ([s offset replacement]
     (when-let [[start end] (ps-start-end (count s) offset)]
       (let [x (subs s 0 start)]
         (when (and *exception-on-surrogate-pair-splitting*
                    (uc/bad-surrogate-at-either-end? x))
           (throw (StringIndexOutOfBoundsException.)))
         (str x replacement))))
  ([s offset length replacement]
     (when-let [[start end] (ps-start-end (count s) offset length)]
       (let [x (subs s 0 start)
             y (subs s end)]
         (when (and *exception-on-surrogate-pair-splitting*
                    (or (uc/bad-surrogate-at-either-end? x)
                        (uc/bad-surrogate-at-either-end? y)))
           (throw (StringIndexOutOfBoundsException.)))
         (str x replacement y)))))


(defn split-on-space [s]
  "Behave as Perl's split(\" \", $s).

   Clojure's (str/split s #\"\\s+\") is almost the same, except when
   the string s has leading whitespace, in which case the Clojure
   version will return a list where the first string is empty, but the
   Perl version will not.  split-on-space handles this the same as
   Perl does, by first removing any leading whitespace before doing
   the split.  It also handles the corner case of returning an empty
   vector if s has length 0."
  (if (= s "")
    []
    (str/split (str/triml s) #"\s+")))


(defn qw
  "Split string on whitespace.  Returns a seq."
  [s]
  (split-on-space s))


;; The below is from PLEAC Section 6.7.  TBD: Check whether there is
;; any duplication or overlap with functions above.

;; Clojure's built-in split does not behave like Perl's split when
;; there are parenthesized capture groups in the regex pattern.
;; Perl's behavior of including these captured strings in the list
;; returned by split can be useful, so implementing that behavior in
;; Clojure would be nice.

;; Here is one implementation, called split-with-capture, that should
;; work like Perl's split, including for arbitrary values of the limit
;; parameter (-1, 0, and any positive value).  It is built on top of
;; re-groups+ first introduced in Section 1.7, and includes an
;; re-find+ function that can be useful on its own, because it returns
;; not only the string matched, but also the part of the string before
;; and after the match, like Perl's $` and $' special variables.

(defn re-groups+ [^java.util.regex.Matcher m s]
  "re-groups+ returns a vector like (re-groups) does,
   except it always returns a vector, even if there are no
   parenthesized subexpressions in the regexp, and it always returns
   the part of the string before the match (Perl's $`) as the first
   element, and the part of the string after the match (Perl's $') as
   the last element.

   See also replace-first+

   TBD: Consider writing replace+.  Anything else that uses re-groups
   that could take advantage of re-groups+?"
  (let [gc (. m (groupCount))
        pre (subs s 0 (. m (start)))
        post (subs s (. m (end)))]
    (loop [v [pre] c 0]
      (if (<= c gc)
        (recur (conj v (. m (group c))) (inc c))
        (conj v post)))))


(defn re-find+
  "Returns the next regex match, if any, of string to pattern, using
  java.util.regex.Matcher.find().  Uses re-groups+ to return the
  groups if a match was found, meaning that on a match the return
  value will always be a vector consisting of these strings:

  [ before-match match capture1 capture2 ... after-match ]

  Where capture1, capture2, etc. are strings that matched
  parenthesized capture groups in the pattern, if any."
  [^java.util.regex.Pattern re s]
  (let [m (re-matcher re s)]
    (when (. m (find))
      (re-groups+ m s))))


(defn- drop-trailing-empty-strings [v]
  (loop [max (count v)]
    (if (zero? max)
      []
      (if (= "" (v (dec max)))
        (recur (dec max))
        (subvec v 0 max)))))


(defn- split-with-capture-core [s re limit]
  (loop [result []
         s s
         c 1]
    (if (or (= s "") (= c limit))
      (conj result s)
      (if-let [matches (re-find+ re s)]
        (let [pre (matches 0)
              capture-groups (subvec matches 2 (dec (count matches)))
              post (peek matches)]
          (recur (apply conj result pre capture-groups) post (inc c)))
        ;; else we are done, and s is the last string to be returned
        (conj result s)))))


(defn split-with-capture
  ([s re]
     (drop-trailing-empty-strings (split-with-capture-core s re nil)))
  ([s re limit]
     (if (zero? limit)
       (split-with-capture s re)
       (split-with-capture-core s re (if (pos? limit) limit nil)))))


;; This is from Section 8.9.  TBD: It should be 'next to' the related
;; functions above that it relies upon.

;; If you want to use capture groups in the pattern that cause
;; additional matching items to be added to the created sequence, use
;; split-with-capture instead, defined in Section 6.7.

;; If you want the special-case Perl behavior when the pattern is a "
;; " (not a regex), use split-on-space defined in Section 1.6.

;; Here is split, that combines all of these behaviors into one like
;; Perl's split.
(defn split
  ([s re]
     (if (= s " ")
       (split-on-space s)
       (split-with-capture s re)))
  ([s re limit]
     (if (= s " ")
       ;; TBD: I don't think this behavior is implemented yet.
       (split-on-space s limit)
       (split-with-capture s re limit))))


(defn chomp
  "If s ends with the character, string, or Unicode code
   point (specified as an integer) c, return a string with c removed
   from the end.  Otherwise return s."
  [^CharSequence s c]
  (let [s (.toString s)]
    (cond
     (instance? Character c) (let [n (count s)]
                               (if (zero? n)
                                 s
                                 ;; Casts to int are not necessary for
                                 ;; correct behavior, but they do help
                                 ;; avoid a reflection warning for
                                 ;; equiv (i.e. =).
                                 (if (= (int c) (int (.charAt s (dec n))))
                                   (subs s 0 (dec n))
                                   s)))
     (instance? CharSequence c) (let [c (.toString ^CharSequence c)]
                                  (if (.endsWith s c)
                                    (subs s 0 (- (count s) (count c)))
                                    s))
     (instance? Number c) (chomp s (uc/chr c))
     :else (throw (IllegalArgumentException. (str "Invalid c arg: " c))))))


;; From Section 8.6 of PLEAC for Clojure.
;; TBD: Check whether there is any duplication or overlap with
;; functions above.

(def ^:dynamic *auto-chomp* true)

(defn read-paragraph-helper
  "Reads and returns a string containing the next 'paragraph' from the
  BufferedReader argument.  Paragraphs are taken to be consecutive
  sequences of non-empty lines separated by one or more empty lines."
  [^java.io.BufferedReader rdr]
  (loop [lines nil
         line (.readLine rdr)]
    (cond
     ;; If we reach end of file, return the lines we have found so
     ;; far, if any, otherwise nil.
     (nil? line) (if lines (apply str lines) nil)
     ;; Skip over empty lines before the paragraph begins
     (and (= line "") (nil? lines)) (recur nil (.readLine rdr))
     ;; Otherwise an empty line is a sign that we reached the end of
     ;; the paragraph we have been reading.
     (= line "") (apply str (conj lines "\n"))
     ;; We found a non-empty line.  Append it to the list of lines in
     ;; the paragraph.
     :else (recur (conj (or lines []) line "\n")
                  (.readLine rdr)))))


(defn read-paragraph
  [^java.io.BufferedReader rdr]
  (if-let [s (read-paragraph-helper rdr)]
    (if *auto-chomp*
      (str/trim-newline s)
      s)))


;; TBD: Can I use *in* as shown below, with fn read-record defined in
;; a separate namespace from where it is called, and *in* will refer
;; to the 'correct' *in*?  If so, why will that always work?  Because
;; *in* is dynamic and per-thread-bound?

;; TBD: Can I call this fn with sep=nil, or will ^CharSequence type
;; hint cause an error in that case?

;; TBD: Check over use of 16-bit chars to see if this is safe for
;; Unicode supplementary characters.  How does java.io.BufferedReader
;; method read work for supplementary characters?  Does it return
;; first a leading surrogate on one call, then a trailing surrogate on
;; the next?

(defn read-record
  "Like read-line, except it reads a 'record' from the stream that is
  the current value of *in*, where a record ends with the string sep.
  Like Perl, sep is an exact-match string, and the values \"\" and nil
  work like Perl's $/ equal to \"\" or undef, respectively, i.e. sep
  equal to \"\" treats two or more consecutive newlines as the
  separator, and sep equal to nil causes read-record to read the
  entire stream all at once.

  Returns nil if there are no more records to read.

  If the variable *auto-chomp* is true, the separator string will be
  removed from the end of the strings returned, otherwise it will be
  left on."
  ([^CharSequence sep]
     (read-record *in* sep))
  ([^java.io.BufferedReader rdr ^CharSequence sep]
     (cond
      (nil? sep) (slurp rdr)
      (= sep "") (read-paragraph rdr)
     ;; Note: The efficiency of this way of matching the sep string is
     ;; poor if it has repeated characters in it.  Creating a DFA to
     ;; match the string sep would be more efficient in the worst
     ;; case, but this should be efficient enough for the expected
     ;; common cases where characters in sep do not appear often in
     ;; the input stream.
      :else
      (let [sb (StringBuilder.)
            n-1 (dec (count sep))
            sep-last (int (.charAt sep n-1))
            sep-butlast (subs (str sep) 0 n-1)]
        (loop [c (.read rdr)]
          (if (neg? c)
            (if (zero? (.length sb))
              nil
              (str sb))
            (if (and (== c sep-last)
                     (>= (.length sb) n-1)
                     ;; Check whether other n-1 chars at end of sb
                     ;; match sep-butlast
                     (loop [i (int (dec n-1))
                            j (int (dec (.length sb)))]
                       (if (neg? i)
                         true
                         ;; Casts to int are not necessary for correct
                         ;; behavior, but they do help avoid a
                         ;; reflection warning for equiv (i.e. =).
                         (if (= (int (.charAt sep-butlast i))
                                (int (.charAt sb j)))
                           (recur (dec i) (dec j))
                           false))))
              (if *auto-chomp*
                ;; return all but sep at the end of sb
                (.substring sb 0 (- (.length sb) n-1))
                (do
                  (.append sb (char c))
                  (.toString sb)))
              (do
                (.append sb (char c))
                (recur (.read rdr))))))))))


(defn record-seq
  "Like line-seq, except it returns a lazy sequence of records
  separated by the string sep.  See read-record documentation for
  special case values of sep.  record-seq is affected by *auto-chomp*
  as read-record is."
  [^java.io.BufferedReader rdr ^CharSequence sep]
  (when-let [rec (read-record rdr sep)]
    (cons rec (lazy-seq (record-seq rdr sep)))))
