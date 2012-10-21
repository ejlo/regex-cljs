(ns regex-cljs.charset
  (:refer-clojure :exclude [complement * + - not contains?]))


;; ranges is a set of disjoint intervals.
;; nil represents (+ or -) infinity.
(defprotocol Rangeable
  (ranges [rs]))

(defprotocol Charsetable
  (charset [x]))


(defn char [n] (if (number? n) (String.fromCharCode n) n))
(defn char-code [c] (.charCodeAt c 0))

(extend-protocol Rangeable
  string             (ranges [s] (map vector s s))
  number             (ranges [n] [[(char n) (char n)]])

  ObjMap             (ranges [m] (map #(vec (map char %)) (seq m)))
  HashMap            (ranges [m] (map #(vec (map char %)) (seq m)))
  PersistentArrayMap (ranges [m] (map #(vec (map char %)) (seq m)))
  PersistentHashMap  (ranges [m] (map #(vec (map char %)) (seq m)))
  PersistentTreeMap  (ranges [m] (map #(vec (map char %)) (seq m)))

  PersistentHashSet  (ranges [s] (map (fn [x] [(char x) (char x)]) s))
  PersistentTreeSet  (ranges [s] (map (fn [x] [(char x) (char x)]) s))

  nil                (ranges [_] nil))

(defrecord Charset [rs]
  Rangeable
  (ranges [_] (seq rs))
  Charsetable
  (charset [this] this))

(defn left-of [[a b] c]
  (and b c (< (char-code b) (char-code c))))

(defn right-of [[a b] c]
  (and a c (< (char-code c) (char-code a))))

(defn range-compare [[a b] [c d]]
  (cond
   (left-of [a b] c) -1
   (right-of [a b] d) 1
   :default 0))

(def no-char (Charset. (sorted-set-by range-compare)))

(extend-protocol Charsetable
  nil
  (charset [_]
    no-char))

(defn- pred [c]
  (when (and c (pos? (char-code c))) (char (dec (char-code c)))))

(defn- succ [c]
  (when (and c (< (char-code c) 0xFFFF)) (char (inc (char-code c)))))

(defn- split
  "Splits ranges right after x."
  [rs x]
  (if-let [[a b :as r] (when x (get rs [x x]))]
    (if (or (= b x) (and b x (= (char-code b) (char-code x))))
      rs
      (-> rs (disj r) (conj [a x] [(succ x) b])))
    rs))

(defn- between [rs a b]
  (cond
    (and a b) (subseq rs >= [a a] <= [b b])
    a (subseq rs >= [a a])
    b (subseq rs <= [b b])
    :else (seq rs)))

(defn- subtract [cs [a b]]
  (let [rs (-> cs :rs (split (pred a)) (split b))]
    (Charset. (reduce disj rs (between rs a b)))))

(defn- add [cs [a b]]
  (let [rs (:rs cs)
        aa (pred a)
        bb (succ b)
        a (when a (first (get rs [aa aa] [a a])))
        b (when b (second (get rs [bb bb] [b b])))]
    (Charset. (conj (reduce disj rs (between rs a b))
                    [a b]))))

(def any-char (add no-char [nil nil]))

(extend-protocol Charsetable
  js/Object
  (charset [x]
    (reduce add no-char (ranges x))))

(defn + "union"
  ([] no-char)
  ([a] (charset a))
  ([a b]
    (reduce add (charset a) (ranges b)))
  ([a b & cs]
    (reduce + (+ a b) cs)))

(defn - "complement or asymetric difference"
  ([x] (reduce subtract any-char (ranges x)))
  ([x & xs]
    (reduce #(reduce subtract %1 (ranges %2)) x xs)))

(defn * "intersection"
  ([] any-char)
  ([a] (charset a))
  ([a b]
    (- (+ (- a) (- b))))
  ([a b & cs]
    (- (reduce + (+ (- a) (- b)) (map - cs)))))

(defn not [& xs]
  (- (reduce + xs)))

(defn pick
  "Returns a character contained in the charset or nil if the
   charset is empty."
  [cs]
  (when-let [[a b] (first (ranges cs))]
    (or a \u0000)))

(defn has? [cs c]
  (boolean ((:rs (charset cs)) [c c])))

(defn disjunctive-union
  "as and bs are collection of disjunct charsets, returns their union as a
   collection of smaller disjunct charsets."
  ([] nil)
  ([as] as)
  ([as bs]
    (let [A (reduce + as)
          B (reduce + bs)]
      (filter pick
        (concat
          (map #(- % B) as)
          (map #(- % A) bs)
          (for [a as b bs] (* a b)))))))

(defn disjunctive-intersection
  "as and bs are collection of disjunct charsets, returns their intersection
   as a collection of smaller disjunct charsets."
  ([] [any-char])
  ([as] as)
  ([as bs]
    (filter pick
      (for [a as b bs] (* a b)))))
