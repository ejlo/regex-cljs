(ns regex-cljs.core
  "A DSL for people who prefer verbose, maintenable regexes to terse
   now-you-have-two-problems ones. Clojurescript port."
  #_{:author "Christophe Grand, Erik Ouchterlony (cljs port)"
     :license "EPL"}
  (:use-macros [net.cgrand.regex-cljs-macros :only [letmap]])
  (:refer-clojure :exclude [repeat + * - resolve])
  (:require [clojure.string :as s]
            [regex-cljs.charset :as cs]))

;; Value-based DSL definition
(defprotocol RegexValue
  (pattern [this] "Returns the pattern represented by this value.")
  (groupnames [this] "Returns a seq of all group names used in this regex.")
  (match-empty? [this]))

(declare regex*)

(defrecord Regex [re groupnames spec]
  RegexValue
    (pattern [this] (.pattern re))
    (groupnames [this] groupnames)
    (match-empty? [this] (match-empty? spec)))

(defn exec [re s]
  (when-let [res (re-matches (:re re) s)]
    (if-let [ids (seq (:groupnames re))]
      (into {} (filter second (map vector (cons nil ids) res)))
      res)))

(defn regex* [spec]
  (Regex. (-> spec pattern re-pattern) (groupnames spec) spec))


(defn sequence [coll] (if (seq? coll) coll (or (seq coll) ())))

(defn- as-seq [v]
  (sequence (if (-> v rseq second (= :as))
              (-> v pop pop)
              v)))

(defn regex-quote [r]
  (s/replace r #"[.?*+^$\[\]\\\(\){}|-]"  #(str "\\" %)))

(extend-type string ; a String denotes a literal sequence of characters to match
  RegexValue
    (pattern [s]
      (regex-quote s))
    (groupnames [this] [])
    (match-empty? [s] (= "" s)))

(doseq [set-type [PersistentHashSet PersistentTreeSet]]
  (extend-type set-type ; a Set denotes an alternative
    RegexValue
    (pattern [set]
      (str "(?:" (s/join "|" (map pattern set)) ")"))
    (groupnames [set]
      (mapcat groupnames set))
    (match-empty? [set] (some match-empty? set))))


(doseq [seq-type [ChunkedSeq List LazySeq PersistentTreeMapSeq EmptyList Cons ChunkedCons]]
  (extend-type seq-type ; a seq denotes a non-capturing group
    RegexValue
    (pattern [v]
      (s/join (map pattern v)))
    (groupnames [v]
      (mapcat groupnames v))
    (match-empty? [v] (every? match-empty? v))))

(doseq [vec-type [Vector PersistentVector]]
  (extend-type vec-type ; a Vector denotes a group (capturing or not)
    RegexValue
    (pattern [v]
      (if (-> v rseq second (= :as))
        (str "(" (pattern (as-seq v)) ")")
        (pattern (as-seq v))))
    (groupnames [v]
      (if (-> v rseq second (= :as))
        (cons (peek v) (groupnames (as-seq v)))
        (groupnames (as-seq v))))
    (match-empty? [v]
      (match-empty? (as-seq v)))))

(doseq [map-type [ObjMap HashMap PersistentArrayMap PersistentHashMap PersistentTreeMap]]
  (extend-type map-type ; a map denotes a char range
    RegexValue
    (pattern [m] (pattern (cs/charset m)))
    (groupnames [v] [])
    (match-empty? [this] false)))

(extend-type cs/Charset
  RegexValue
    (pattern [cs]
      (let [reserved (set "[]&^-")
            esc #(if (or (not (< 0x1F (cs/char-code %) 0x7F)) (reserved %))
                   (format "\\u%04X" (cs/char-code %))
                   %)
            rs (-> cs cs/charset cs/ranges)]
        (apply str (concat ["["]
                           (mapcat (fn [[a b]]
                                     (if (and a (= a b))
                                       [(esc a)]
                                       [(esc (or a \u0000)) "-"
                                        (esc (or b \uFFFF))])) rs)
                           ["]"]))))
    (groupnames [v] [])
    (match-empty? [this] false))

(defn regex [& specs]
  (regex* (vec specs)))

(defrecord Repeat [frag min max]
  RegexValue
    (pattern [this]
      (let [s (pattern frag)
            max (or max "")]
        (str "(?:" s "){" min "," max "}")))
    (groupnames [this]
      (groupnames frag))
    (match-empty? [this] (or (zero? min) (match-empty? frag))))

(defn repeat
 ([spec] (Repeat. spec 0 nil))
 ([spec min] (Repeat. spec min nil))
 ([spec min max] (Repeat. spec min max)))

(defn *
 [& specs]
  (repeat (vec specs)))

(defn +
 [& specs]
  (repeat (vec specs) 1))

(defn ?
 [& specs]
  (repeat (vec specs) 0 1))

(def any cs/any-char)

(defrecord PositiveLookahead [frag]
  RegexValue
  (pattern [this]
    (str "(?=" (pattern frag) ")"))
  (groupnames [this]
    (groupnames frag)))

(defn ?= [frag] (PositiveLookahead. frag))

(defrecord NegativeLookahead [frag]
  RegexValue
  (pattern [this]
    (str "(?!" (pattern frag) ")"))
  (groupnames [this]
    nil))

(defn ?! [frag] (NegativeLookahead. frag))

;; predefined classes
(def digit {\0 \9})
(def !digit (cs/- digit))
(def space (cs/charset " \t\n\u000B\f\r"))
(def !space (cs/- space))
(def wordchar {\a \z \A \Z \_ \_ \0 \9})
(def !wordchar (cs/- wordchar))

(def posix
  (letmap
   Lower {\a \z}
   Upper {\A \Z}
   ASCII {\u0000 \u007F}
   Alpha (cs/+ Lower Upper)
   Digit {\0 \9}
   Alnum (cs/+ Alpha Digit)
   Punct (cs/charset "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
   Graph (cs/+ Alnum Punct)
   Print (cs/+ Graph \space)
   Blank (cs/charset " \t")
   Cntrl (cs/+ {\u0000 \u001F} \u007F)
   XDigit {\0 \9 \a \f \A \F}
   Space (cs/charset " \t\n\u000B\f\r")))


(comment
  regex=> (exec (regex [(repeat {\a \z}) :as :a] " " [(repeat {\a \z}) :as :b])
            "hello world")
  {:b "world", :a "hello", nil "hello world"}

  (def datestamp-re
    (let [d {\0 \9}]
      (regex [d d d d :as :year] \- [d d :as :month] \- [d d :as :day])))
  regex=> (exec datestamp-re "2007-10-23")
  {:day "23", :month "10", :year "2007", nil "2007-10-23"}
  regex=> (exec datestamp-re "20X7-10-23")
  nil
)