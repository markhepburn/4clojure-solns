(defn most [& args]
  (boolean (and (some true? args) (not (every? true? args)))))

(defn myrev [lst]
  (reduce (fn [val col] (cons col val)) nil lst))

(defn palindrom [sq]
  (let [sq (seq sq)]
    (= sq (reverse sq))))

(defn fibonacci [num]
  (loop [iter 0
         a 0
         b 1
         res []]
    (if (= iter num)
      res
      (recur (inc iter) b (+ a b) (conj res b)))))

(defn maximum [& args]
  (reduce (fn [v x] (if (> v x) v x)) args))

(defn get-caps [s]
  (apply str (re-seq #"[A-Z]+" s)))

(defn dup-seq [lst]
  (apply concat (map #(vec [% %]) lst)))

(defn range [start end]
  (take (- end start) (iterate inc start)))

(defn factorial [from]
  (reduce * (range 1 (inc from))))

;;; http://www.4clojure.com/problem/28
(defn my-flatten [col]
  (let [flattener
        (fn flattener [accum val]
          (if (coll? val) (reduce flattener accum val) (conj accum val)))]
   (reverse (reduce flattener '() col))))

;;; http://www.4clojure.com/problem/30
(defn compress-seq [lst]
  (reverse
   (reduce (fn [accum val]
             (if (= val (first accum)) accum (cons val accum)))
           (list (first lst)) (rest lst))))
;; (compress-seq "Leeeeeerrroyyy")

;;; http://www.4clojure.com/problem/39
(defn my-interleave [l1 l2]
  (loop [accum '()
         l1 l1
         l2 l2]
    (if (or (empty? l1) (empty? l2))
      accum
      (recur (concat accum (list (first l1) (first l2)))
             (rest l1) (rest l2)))))
;;; (my-interleave [1 2 3] [:a :b :c])
;;; (my-interleave [1 2] [3 4 5 6])
;;; (my-interleave [1 2 3 4] [5])

;;; http://www.4clojure.com/problem/33
(defn repl-seq [lst num]
  (apply concat (map #(repeat num %) lst)))
;;; (repl-seq [1 2 3] 2)

;;; http://www.4clojure.com/problem/40
(defn my-interpose [el lst]
  (->> lst
       (reduce (fn [accum val] (concat (list el val) accum)) '())
       rest
       reverse))
  ;; (reverse (rest (reduce (fn [accum val] (concat (list el val) accum)) '() lst)))
;; (my-interpose 0 [1 2 3])
;; (my-interpose :z [:a :b :c :d])

;;; http://www.4clojure.com/problem/31
(defn pack-seq [seq]
  (partition-by identity seq))

;;; http://www.4clojure.com/problem/41
(defn drop-n [seq n]
  (->> seq
       (partition-all n)
       (map (fn [grp]
              (if (= n (count grp)) (->> grp reverse rest reverse) grp)))
       (apply concat)))
;; (drop-n [1 2 3 4 5 6 7 8] 3)

;;; http://www.4clojure.com/problem/49
(defn my-split-at [n col]
  (loop [head '()
         rst col
         count n]
    (if (zero? count)
      (list (reverse head) rst)
      (recur (conj head (first rst)) (rest rst) (dec count)))))
;; (my-split-at 3 [1 2 3 4 5 6])
;; (my-split-at 2 [[1 2] [3 4] [5 6]])

;;; http://www.4clojure.com/problem/61
(defn my-zipmap [keys vals]
  (loop [ks keys
         vs vals
         res {}]
    (if (or (empty? ks) (empty? vs))
      res
      (recur (rest ks) (rest vs) (assoc res (first ks) (first vs))))))
;; (my-zipmap [1 2 3 4] ["one" "two" "three"])

;;; http://www.4clojure.com/problem/66
(defn gcd [x y]
  (loop [x (max x y) y (min x y)]
    (if (zero? y) x (recur y (mod x y)))))
;; (= (gcd 2 4) 2)
;; (= (gcd 10 5) 5)
;; (= (gcd 5 7) 1)
;; (= (gcd 1023 858) 33)

;;; http://www.4clojure.com/problem/81
(defn my-intersection [s1 s2]
  (loop [res #{}
         ss (seq s1)]
    (if-let [el (first ss)]
     (recur (if (contains? s2 el) (conj res el) res) (rest ss))
     res)))
;;; (my-intersection #{0 1 2 3} #{2 3 4 5})

;;; http://www.4clojure.com/problem/62
(defn my-iterate [f x]
  (cons x (lazy-seq (my-iterate f (f x)))))
;;; (take 5 (my-iterate #(* 2 %) 1))
;; (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16])
;; (= (take 100 (my-iterate inc 0)) (take 100 (range)))
;; (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

;;; http://www.4clojure.com/problem/107
;;; Given a positive integer n, return a function (f x) which computes x^n
(defn make-pow [n]
  (fn [x] (reduce * (repeat n x))))
;;; (= [1 8 27 64] (map (make-pow 3) [1 2 3 4]))

;;; http://www.4clojure.com/problem/99
(defn prod-dig [x y]
  (loop [res '() prod (* x y)]
    (if (> prod 0)
      (recur (conj res (rem prod 10)) (quot prod 10))
      res)))
;;; (= (prod-dig 99 9) [8 9 1])

;;; http://www.4clojure.com/problem/90
(defn cart-prod [s1 s2]
  (reduce (fn [accum val]
            (into accum (map #(vec [val %]) s2)))
          #{} s1))
;; (= (cart-prod #{1 2 3} #{4 5}) #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

;;; http://www.4clojure.com/problem/63
(defn my-groupby [f sq]
  (reduce
   (fn [accum val]
     (let [key (f val)
           grp (or (accum key) [])]
       (assoc accum key (conj grp val))))
   {} sq))
;;; (= (my-groupby #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

;;; http://www.4clojure.com/problem/88
(defn symmetric-diff [s1 s2]
  (let [only-s1 (reduce (fn [accum val] (if (contains? s2 val) accum (conj accum val))) #{} s1)]
    (reduce (fn [accum val] (if (contains? s1 val) accum (conj accum val))) only-s1 s2)))
;; (= (symmetric-diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})

;;; http://www.4clojure.com/problem/122
(defn from-binary-str [str]
  (reduce +
   (map (fn [d c] (if (= c \1) d 0)) (iterate #(* 2 %) 1) (reverse str))))
;;; (= 1365  (from-binary-str "10101010101"))

;;; http://www.4clojure.com/problem/97
(defn pascal [n]
  (let [nextrow
        (fn [row]
          (map + (cons 0 row) (concat row [0])))]
    (nth (iterate nextrow [1]) (dec n))))
;;; (= (pascal 11) [1 10 45 120 210 252 210 120 45 10 1])

;;; http://www.4clojure.com/problem/143
(defn dot-prod [xs ys]
  (reduce + (map * xs ys)))
;;; (= 32 (dot-prod [1 2 3] [4 5 6]))

;;; http://www.4clojure.com/problem/135
(defn from-infix [& exprs]
  (loop [[x op y & rest] exprs]
    (let [val (op x y)]
      (if (empty? rest) val (recur (cons val rest))))))
;;; (= 7  (from-infix 2 + 5))
;;; (= 42 (from-infix 38 + 48 - 2 / 2))

;;; http://www.4clojure.com/problem/95
;;; Each node in the tree must have a value, a left child, and a right child.
(defn binary-tree? [tree]
  (let [[val left right] tree]
    (and (= (count tree) 3)
         (or (nil? left) (and (coll? left) (binary-tree? left)))
         (or (nil? right) (and (coll? right) (binary-tree? right))))))
;; (= (binary-tree? '(:a (:b nil nil) nil)) true)
;; (= (binary-tree? '(:a (:b nil nil))) false)
;; (= (binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]]) true)
;; (= (binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false)
;; (= (binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil]) true)
;; (= (binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil]) false)
;; (= (binary-tree? '(:a nil ())) false)

;;; http://www.4clojure.com/problem/118
;;; Must return a LAZY sequence!
(defn my-map [f s]
  (when(not (empty? s))
    (cons (f (first s)) (lazy-seq (my-map f (rest s))))))
;; (= [3 4 5 6 7] (my-map inc [2 3 4 5 6]))
;; (= (repeat 10 nil) (my-map (fn [_] nil) (range 10)))
;; (= [1000000 1000001] (->> (my-map inc (range)) (drop (dec 1000000)) (take 2)))

;;; http://www.4clojure.com/problem/120
(defn sum-square-digits [nums]
  (reduce
   +
   (map (fn [num]
          (let [digits                  ; find digits of a number
                ; Clunky, but shorter: (map #(- (int %) (int \0)) (str num))
                (loop [res '() num num]
                  (if (> num 0)
                    (recur (conj res (rem num 10)) (quot num 10))
                    res))]
            ; Test if less than sum of squared digits; 1 or 0 on success/failure:
            (if (< num (reduce + (map #(* % %) digits))) 1 0)))
        nums)))
;; (= 8 (sum-square-digits (range 10)))
;; (= 19 (sum-square-digits (range 30)))
;; (= 50 (sum-square-digits (range 100)))
;; (= 50 (sum-square-digits (range 1000)))

;;; http://www.4clojure.com/problem/128
(defn parse-cards-spec [[suit rank]]
  (let [suits {\D :diamond, \S :spade, \H :heart, \C :club}
        ranks (into {} (map vector "23456789TJQKA" (range)))]
    {:suit (suits suit), :rank (ranks rank)}))
;; (= {:suit :diamond :rank 10} (parse-cards-spec "DQ"))
;; (= {:suit :heart :rank 3} (parse-cards-spec "H5"))
;; (= {:suit :club :rank 12} (parse-cards-spec "CA"))
;; (= (range 13) (map (comp :rank parse-cards-spec str) '[S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK SA]))

;;; http://www.4clojure.com/problem/100
(defn lcm [& nums]
  (reduce (fn [x y]
            (let [gcd (loop [[y x] (sort [x y])]
                        (if (zero? y) x (recur [(mod x y) y])))
                  prod (* x y)]
              (/ prod gcd)))
          nums))
;; (== (lcm 2 3) 6)
;; (== (lcm 5 3 7) 105)
;; (== (lcm 1/3 2/5) 2)
;; (== (lcm 3/4 1/6) 3/2)
;; (== (lcm 7 5/7 2 3/5) 210)

;;; http://www.4clojure.com/problem/96
(defn tree-mirror? [[v left right :as tree]]
  (let [mirror
        (fn mirror [[v l r :as t]]
          (if (nil? t) nil (list v (mirror r) (mirror l))))]
    (or (nil? tree)
        (= left (mirror right)))))
;; (= (tree-mirror? '(:a (:b nil nil) (:b nil nil))) true)
;; (= (tree-mirror? '(:a (:b nil nil) nil)) false)
;; (= (tree-mirror? '(:a (:b nil nil) (:c nil nil))) false)
;; (= (tree-mirror? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]] [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]) true)
;; (= (tree-mirror? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]] [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]]) false)
;; (= (tree-mirror? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]] [2 [3 nil [4 [6 nil nil] nil]] nil]]) false)

;;; http://www.4clojure.com/problem/157
(defn index-seq [s]
  (map vector s (range)))
;; (= (index-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
;; (= (index-seq [0 1 3]) '((0 0) (1 1) (3 2)))
;; (= (index-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])

;;; http://www.4clojure.com/problem/147
(defn pascal-trap [s]
  (let [nextrow
        (fn [row]
          (map +' (cons 0 row) (concat row [0])))]
    (iterate nextrow s)))
;; (= (second (pascal-trap [2 3 2])) [2 5 5 2])
;; (= (take 5 (pascal-trap [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
;; (= (take 2 (pascal-trap [3 1 2])) [[3 1 2] [3 4 3 2]])
;; (= (take 100 (pascal-trap [2 4 2])) (rest (take 101 (pascal-trap [2 2]))))

;;; http://www.4clojure.com/problem/146
(defn tree-map [tree]
  (into {}
    (for [[k1 m] tree [k2 v] m] [[k1 k2] v])))
;; (= (tree-map '{a {p 1, q 2} b {m 3, n 4}}) '{[a p] 1, [a q] 2 [b m] 3, [b n] 4})
;; (= (tree-map '{[1] {a b c d} [2] {q r s t u v w x}}) '{[[1] a] b, [[1] c] d,[[2] q] r, [[2] s] t,[[2] u] v, [[2] w] x})
;; (= (tree-map '{m {1 [a b c] 3 nil}}) '{[m 1] [a b c], [m 3] nil})

;;; http://www.4clojure.com/problem/153
(defn pairwise-disjoint [sets]
  (=
   (->> sets
        (map count)
        (reduce +))
   (count (reduce (fn [accum set] (into accum set)) #{} sets))))
;; (= (pairwise-disjoint #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}}) true)
;; (= (pairwise-disjoint #{#{:a :b :c :d :e} #{:a :b :c :d} #{:a :b :c} #{:a :b} #{:a}}) false)
;; (= (pairwise-disjoint #{#{[1 2 3] [4 5]} #{[1 2] [3 4 5]} #{[1] [2] 3 4 5} #{1 2 [3 4] [5]}}) true)
;; (= (pairwise-disjoint #{#{'a 'b} #{'c 'd 'e} #{'f 'g 'h 'i} #{''a ''c ''f}}) true)
;; (= (pairwise-disjoint #{#{'(:x :y :z) '(:x :y) '(:z) '()} #{#{:x :y :z} #{:x :y} #{:z} #{}} #{'[:x :y :z] [:x :y] [:z] [] {}}}) false)
;; (= (pairwise-disjoint #{#{(= "true") false} #{:yes :no} #{(class 1) 0} #{(symbol "true") 'false} #{(keyword "yes") ::no} #{(class '1) (int \0)}}) false)
;; (= (pairwise-disjoint #{#{distinct?} #{#(-> %) #(-> %)} #{#(-> %) #(-> %) #(-> %)} #{#(-> %) #(-> %) #(-> %)}}) true)
;; (= (pairwise-disjoint #{#{(#(-> *)) + (quote mapcat) #_ nil} #{'+ '* mapcat (comment mapcat)} #{(do) set contains? nil?} #{, , , #_, , empty?}}) false)


;;; http://www.4clojure.com/problem/46
(defn flip [f]
  (fn [& args] (apply f (reverse args))))
;; (= 3 ((flip nth) 2 [1 2 3 4 5]))
;; (= true ((flip >) 7 8))
;; (= 4 ((flip quot) 2 8))
;; (= [1 2 3] ((flip take) [1 2 3 4 5] 3))

;;; http://www.4clojure.com/problem/44
(defn rotate-seq [idx s]
  (let [split (mod idx (count s))
        [fst snd] (split-at split s)]
    (concat snd fst)))
;; (= (rotate-seq 2 [1 2 3 4 5]) '(3 4 5 1 2))
;; (= (rotate-seq -2 [1 2 3 4 5]) '(4 5 1 2 3))
;; (= (rotate-seq 6 [1 2 3 4 5]) '(2 3 4 5 1))
;; (= (rotate-seq 1 '(:a :b :c)) '(:b :c :a))
;; (= (rotate-seq -4 '(:a :b :c)) '(:c :a :b))

;;; http://www.4clojure.com/problem/43
(defn reverse-interleave [s n]
  (apply map list (partition n s)))
;; (= (reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
;; (= (reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
;; (= (reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

;;; http://www.4clojure.com/problem/50
(defn split-by-type [s]
  (let [typemap
        (reduce (fn [accum val]
                  (assoc accum (type val) (cons val (accum (type val)))))
                {} s)]
    (map reverse (into #{} (vals typemap)))))
;; (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
;; (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
;; (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

;;; http://www.4clojure.com/problem/55
(defn count-in-seq [s]
  (reduce (fn [m v] (update-in m [v] (fnil inc 0))) {} s))
;; (= (count-in-seq [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
;; (= (count-in-seq [:b :a :b :a :b]) {:a 2, :b 3})
;; (= (count-in-seq '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

;;; http://www.4clojure.com/problem/56
(defn find-distinct [s]
  (reduce (fn [accum x]
            (if (apply distinct? x accum) (conj accum x) accum))
          [] s))
;; (= (find-distinct [1 2 1 3 1 2 4]) [1 2 3 4])
;; (= (find-distinct [:a :a :b :b :c :c]) [:a :b :c])
;; (= (find-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
;; (= (find-distinct (range 50)) (range 50))

;;; http://www.4clojure.com/problem/58
(defn my-comp [& funcs]
  (let [funcs (reverse funcs)
        fun   (first funcs)
        funs  (rest funcs)]
    (fn [& args]
      (reduce (fn [x f] (f x)) (apply fun args) funs))))
;; (= [3 2 1] ((my-comp rest reverse) [1 2 3 4]))
;; (= 5 ((my-comp (partial + 3) second) [1 2 3 4]))
;; (= true ((my-comp zero? #(mod % 8) +) 3 5 7 9))
;; (= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

;;; http://www.4clojure.com/problem/59
(defn juxtapose [& funcs]
  (fn [& args]
    (map (fn [f] (apply f args)) funcs)))
;; (= [21 6 1] ((juxtapose + max min) 2 3 5 1 6 4))
;; (= ["HELLO" 5] ((juxtapose #(.toUpperCase %) count) "hello"))
;; (= [2 6 4] ((juxtapose :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;;; http://www.4clojure.com/problem/54
(defn my-partition [p s]
  (loop [acc []
         [part rest] (split-at p s)]
    (if (< (count part) p)
      acc
      (recur (conj acc part) (split-at p rest)))))
;; (= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
;; (= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
;; (= (my-partition 3 (range 8)) '((0 1 2) (3 4 5)))

;;; http://www.4clojure.com/problem/70
(defn word-sorting [s]
  (sort-by clojure.string/lower-case
           (clojure.string/split s #"[ .,!;]")))
;; (= (word-sorting  "Have a nice day.") ["a" "day" "Have" "nice"])
;; (= (word-sorting  "Clojure is a fun language!") ["a" "Clojure" "fun" "is" "language"])
;; (= (word-sorting  "Fools fall for foolish follies.") ["fall" "follies" "foolish" "Fools" "for"])

;;; http://www.4clojure.com/problem/67
(defn n-primes [n]
  (let [sieve
        (fn sieve [[x & xs]]
          (cons x (lazy-seq (sieve (filter #(pos? (rem % x)) xs)))))]
   (take n (cons 2 (sieve (iterate (partial + 2) 3))))))
;; (= (n-primes 2) [2 3])
;; (= (n-primes 5) [2 3 5 7 11])
;; (= (last (n-primes 100)) 541)

;;; http://www.4clojure.com/problem/74
(defn filter-squares [s]
  (let [nums (map #(Integer/valueOf %) (clojure.string/split s #","))
        fmt #(apply str (interpose "," (map (comp str int) %)))]
    (fmt (filter #(zero? (rem (Math/sqrt %) 1)) nums))))
;; (= (filter-squares "4,5,6,7,8,9") "4,9")
;; (= (filter-squares "15,16,25,36,37") "16,25,36")

;;; http://www.4clojure.com/problem/80
(defn perfect? [n]
  (let [divisors (filter #(zero? (rem (/ n %) 1)) (range 1 n))]
    (= n (apply + divisors))))
;; (= (perfect? 6) true)
;; (= (perfect? 7) false)
;; (= (perfect? 496) true)
;; (= (perfect? 500) false)
;; (= (perfect? 8128) true)

;;; http://www.4clojure.com/problem/65
(defn type-test [x]
  ;; (is this clearer like this, or as a (cond..) ?
  (if-not (ifn? x)
    :list                           ; :map,:set,:vec all implement IFn
    (if-not (associative? x)
      :set                   ; :map,:set don't equal seq of themselves
      (if (reversible? x)
        :vector               ; set from a set will be equal to itself
        :map))))
;; (= :map (type-test {:a 1, :b 2}))
;; (= :list (type-test (range (rand-int 20))))
;; (= :vector (type-test [1 2 3 4 5 6]))
;; (= :set (type-test #{10 (rand-int 5)}))
;; (= [:map :set :vector :list] (map type-test [{} #{} [] ()]))

;;; http://www.4clojure.com/problem/77
(defn anagrams [words]
  (let [sortw (fn [word] (str (sort word)))
        res (reduce (fn [res word]
                      (update-in res [(sortw word)] #((fnil conj #{}) % word)))
                    {} words)]
    (set (filter #(> (count %) 1) (vals res)))))
;; (= (anagrams ["meat" "mat" "team" "mate" "eat"]) #{#{"meat" "team" "mate"}})
;; (= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"]) #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;;; http://www.4clojure.com/problem/60
(def my-reductions
  (fn my-reductions
    ([op s] (my-reductions op (first s) (rest s)))
    ([op init s]
       (cons init
             (if-not (empty? s)
               (lazy-seq (my-reductions op (op init (first s)) (rest s))))))))
;; (= (take 5 (my-reductions + (range))) [0 1 3 6 10])
;; (= (my-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
;; (= (last (my-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;;; http://www.4clojure.com/problem/69
(def merge-map
  (fn [f mp & mps]
    (loop [m mp
           ms mps]
      (if (empty? ms)
        m
        (recur
         (reduce (fn [accum [k v]]
                   (if-let [val (get accum k)]
                     (conj accum [k (f val v)])
                     (conj accum [k v])))
                 m (first ms))
         (rest ms))))))
;; (= (merge-map * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}) {:a 4, :b 6, :c 20})
;; (= (merge-map - {1 10, 2 20} {1 3, 2 10, 3 15}) {1 7, 2 10, 3 15})
;; (= (merge-map concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}) {:a [3 4 5], :b [6 7], :c [8 9]})

;;; http://www.4clojure.com/problem/102
(def to-camel-case
  (fn [s]
    (let [[word & words] (clojure.string/split s #"-")]
      (apply str word (map #(clojure.string/capitalize %) words)))))
;; (= (to-camel-case "something") "something")
;; (= (to-camel-case "multi-word-key") "multiWordKey")
;; (= (to-camel-case "leaveMeAlone") "leaveMeAlone")

;;; http://www.4clojure.com/problem/86
(def happy-number?
  (fn [n]
    (let [step (fn [n] (loop [acc 0
                              n n]
                         (if (< n 1)
                           acc
                           (let [digit (rem n 10)]
                             (recur (+ acc (* digit digit)) (quot n 10))))))
          nums (iterate step n)]
      (loop [nums nums
             seen #{}]
        (let [num (first nums)]
          (cond
           (= num 1) true
           (contains? seen num) false
           :else (recur (rest nums) (conj seen num))))))))
;; (= (happy-number? 7) true)
;; (= (happy-number? 986543210) true)
;; (= (happy-number? 2) false)
;; (= (happy-number? 3) false)

;;; http://www.4clojure.com/problem/75
(def euler-tot
  (fn [n]
    (letfn [(gcd [x y]
              (loop [[y x] (sort [x y])]
                (if (zero? y) x (recur [(mod x y) y]))))]
      ;; coprime when gcd is 1; ignore special case of 1 by ranging
      ;; from 2 and incrementing result:
      (inc (count (filter #(= 1 (gcd % n)) (range 2 n)))))))
;; (= (euler-tot 1) 1)
;; (= (euler-tot 10) (count '(1 3 7 9)) 4)
;; (= (euler-tot 40) 16)
;; (= (euler-tot 99) 60)

;;; http://www.4clojure.com/problem/78
(def my-trampoline
  (fn [f & args]
    (loop [fun (apply f args)]
      (if-not (ifn? fun)
        fun
        (recur (fun))))))
(comment
  (= (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (my-trampoline triple 2))
  82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
          (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
    (map (partial my-trampoline my-even?) (range 6)))
  [true false true false true false])
)

;;; http://www.4clojure.com/problem/98
(def equiv-classes
  (fn [f domain]
    (let [by-mapping
          (reduce (fn [acc x]
                    (update-in acc [(f x)] #((fnil conj #{}) % x)))
                  {} domain)]
      (set (vals by-mapping)))))
;; (= (equiv-classes #(* % %) #{-2 -1 0 1 2}) #{#{0} #{1 -1} #{2 -2}})
;; (= (equiv-classes #(rem % 3) #{0 1 2 3 4 5 }) #{#{0 3} #{1 4} #{2 5}})
;; (= (equiv-classes identity #{0 1 2 3 4}) #{#{0} #{1} #{2} #{3} #{4}})
;; (= (equiv-classes (constantly true) #{0 1 2 3 4}) #{#{0 1 2 3 4}})

;;; http://www.4clojure.com/problem/85
(def power-set
  (fn [s]
    (letfn [(combine [acc x]
              (conj (into acc (map #(conj % x) acc)) #{x}))]
      (conj (reduce combine #{} s) #{}))))
;; (= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
;; (= (power-set #{}) #{#{}})
;; (= (power-set #{1 2 3}) #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
;; (= (count (power-set (into #{} (range 10)))) 1024)

;;; http://www.4clojure.com/problem/115
(def balanced-num?
  (fn [n]
    (let [digits (map #(- (int %) (int \0)) (str n))
          mid (int (/ (count digits) 2))
          [left right] (split-at mid digits)
          ;; maybe discard middle, if count was odd:
          right (if (= (count left) (count right)) right (rest right))]
      (= (reduce + left) (reduce + right)))))
;; (= true (balanced-num? 11))
;; (= true (balanced-num? 121))
;; (= false (balanced-num? 123))
;; (= true (balanced-num? 0))
;; (= false (balanced-num? 88099))
;; (= true (balanced-num? 89098))
;; (= true (balanced-num? 89089))
;; (= (take 20 (filter balanced-num? (range))) [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])

;;; http://www.4clojure.com/problem/105
(def key-seq
  (fn [s]
    (loop [k (first s)                  ; assuming here that it'll start with a keyword!
           s (rest s)
           m {}
           sq []]
      (let [next (first s)]
       (cond
        (empty? s) (if (nil? k) m (conj m [k sq]))
        (keyword? next) (recur next (rest s) (conj m [k sq]) [])
        (number? next) (recur k (rest s) m (conj sq next)))))))
;; (= {} (key-seq []))
;; (= {:a [1]} (key-seq [:a 1]))
;; (= {:a [1], :b [2]} (key-seq [:a 1, :b 2]))
;; (= {:a [1 2 3], :b [], :c [4]} (key-seq [:a 1 2 3 :b :c 4]))

;;; http://www.4clojure.com/problem/137
(def digits-for-base
  (fn [n b]
    (loop [digits (list (rem n b))      ; Initialised like this to handle the 0-case
           n (quot n b)]
      (if (< n 1)
        digits
        (recur (conj digits (rem n b)) (quot n b))))))
;; (= [1 2 3 4 5 0 1] (digits-for-base 1234501 10))
;; (= [0] (digits-for-base 0 11))
;; (= [1 0 0 1] (digits-for-base 9 2))
;; (= [1 0] (let [n (rand-int 100000)](digits-for-base n n)))
;; (= [16 18 5 24 15 1] (digits-for-base Integer/MAX_VALUE 42))

;;; http://www.4clojure.com/problem/110
(def pronounciation-seq
  (fn pronounciation-seq [ns]
    (let [groups (partition-by identity ns)
          pronounciation (flatten (map #(list (count %) (first %)) groups))]
      (cons pronounciation (lazy-seq (pronounciation-seq pronounciation))))))
;; (= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronounciation-seq [1])))
;; (= [3 1 2 4] (first (pronounciation-seq [1 1 1 4 4])))
;; (= [1 1 1 3 2 1 3 2 1 1] (nth (pronounciation-seq [1]) 6))
;; (= 338 (count (nth (pronounciation-seq [3 2]) 15)))

;;; http://www.4clojure.com/problem/93
(def partial-flatten
  (fn [s]
    (letfn [(rec [accum s]
              (if-not (coll? (first s))
                (conj accum s)
                (reduce rec accum s)))]
      (reduce rec [] s))))
;; (= (partial-flatten [["Do"] ["Nothing"]]) [["Do"] ["Nothing"]])
;; (= (partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]]) [[:a :b] [:c :d] [:e :f]])
;; (= (partial-flatten '((1 2)((3 4)((((5 6))))))) '((1 2)(3 4)(5 6)))

;;; http://www.4clojure.com/problem/108
(def least-common
  (fn [& ss]
    ;; take first element of each; if = then finished
    ;; else calc max of these, and drop-while < this for each seq, recurse
    (loop [ss ss]
      (let [hds (map first ss)]
        (if (apply = hds)
          (first hds)
          (recur (map (partial drop-while #(< % (apply max hds))) ss)))))))
(comment
(= 3 (least-common [3 4 5]))
(= 4 (least-common [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 7 (least-common (range) (range 0 100 7/6) [2 3 5 7 11 13]))
(= 64 (least-common (map #(* % % %) (range)) ;; perfect cubes
                    (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                    (iterate inc 20))) ;; at least as large as 20
)

;;; http://www.4clojure.com/problem/144
(def oscilrate
  (fn oscilrate [x f & fs]
    (cons x (lazy-seq (apply oscilrate (f x) (concat fs [f]))))))
;; (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0])
;; (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
;; (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])

;;; http://www.4clojure.com/problem/114
(def global-take-while
  (fn global-take-while [n p [s & ss]]
    (let [hit (p s)]
      (if-not (and hit (= 1 n))
       (cons s (lazy-seq (global-take-while (if hit (dec n) n) p ss)))))))
;; (= [2 3 5 7 11 13] (global-take-while 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23]))
;; (= ["this" "is" "a" "sentence"] (global-take-while 3 #(some #{\i} %) ["this" "is" "a" "sentence" "i" "wrote"]))
;; (= ["this" "is"] (global-take-while 1 #{"a"} ["this" "is" "a" "sentence" "i" "wrote"]))

;;; http://www.4clojure.com/problem/132
(def intersperse-when
  (fn intersperse-when [p v [c1 c2 & cs :as col]]
    (when-not (empty? col)
      (cons c1
            (lazy-seq
             (if c2
               (let [rst (intersperse-when p v (cons c2 cs))]
                 (if (p c1 c2) (cons v rst) rst))))))))
(comment
  (= '(1 :less 6 :less 7 4 3) (intersperse-when < :less [1 6 7 4 3]))
  (= '(2) (intersperse-when > :more [2]))
  (= [0 1 :x 2 :x 3 :x 4]  (intersperse-when #(and (pos? %) (< % %2)) :x (range 5)))
  (empty? (intersperse-when > :more ()))
  (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
     (take 12 (->> [0 1]
                   (iterate (fn [[a b]] [b (+ a b)]))
                   (map first)          ; fibonacci numbers
                   (intersperse-when (fn [a b]        ; both even or both odd
                         (= (mod a 2) (mod b 2)))
                       :same))))
)

;;; http://www.4clojure.com/problem/104
(def as-roman
  (fn [n]
    ;; singular, "5", and next-power (some redundancy, sure)
    (let [roman [["I" "V" "X"]
                 ["X" "L" "C"]
                 ["C" "D" "M"]
                 ["M" ""  "" ]]
          digits (reverse (map #(- (int %) (int \0)) (str n)))]
      (loop [[[r1 r5 r10] & rom] roman
             [d & dd] digits
             res ""]
        (if (nil? d)
          res
          (recur rom dd
                 (str
                  (cond
                   (= d 0)    ""
                   (<= 1 d 3) (apply str (repeat d r1))
                   (<= 4 d 5) (str (if (= 4 d) r1 "") r5)
                   (<= 6 d 8) (apply str r5 (repeat (- d 5) r1))
                   :else      (str r1 r10))
                  res)))))))
;; (= "I" (as-roman 1))
;; (= "XXX" (as-roman 30))
;; (= "IV" (as-roman 4))
;; (= "CXL" (as-roman 140))
;; (= "DCCCXXVII" (as-roman 827))
;; (= "MMMCMXCIX" (as-roman 3999))
;; (= "XLVIII" (as-roman 48))

;;; http://www.4clojure.com/problem/121
(def universal-comp
  (fn [formula]
    (letfn [(ev [vals form]
              (cond
               (number? form) form
               (symbol? form) (vals form)
               :else  (apply ({'* * '/ / '+ + '- -} (first form))
                             (map #(ev vals %) (rest form)))))]
      #(ev % formula))))
(comment
 (= 2 ((universal-comp '(/ a b))
       '{b 8 a 16}))

 (= 8 ((universal-comp '(+ a b 2))
       '{a 2 b 4}))

 (= [6 0 -4]
    (map (universal-comp '(* (+ 2 a)
                 (- 10 b)))
         '[{a 1 b 8}
           {b 5 a -2}
           {a 2 b 11}]))

 (= 1 ((universal-comp '(/ (+ x 2)
               (* 3 (+ y 1))))
       '{x 4 y 1})))

;;; http://www.4clojure.com/problem/116
(def balanced-prime?
  (fn [n]
    (let [sieve
          (fn sieve [[x & xs]]
            (cons x (lazy-seq (sieve (filter #(pos? (rem % x)) xs)))))
          primes (cons 2 (sieve (iterate (partial + 2) 3)))
          [[p1 p2 p3] restprimes] (split-at 3 primes)]
      (loop [p1 p1
             p2 p2
             p3 p3
             primes restprimes]
        (cond
         (= p2 n) (= n (/ (+ p1 p3) 2))
         (> p2 n) false
         :else (recur p2 p3 (first primes) (rest primes)))))))
;; (= false (balanced-prime? 4))
;; (= true (balanced-prime? 563))
;; (= 1103 (nth (filter balanced-prime? (range)) 15))

;;; http://www.4clojure.com/problem/103
(def k-combinations
  (fn [k s]
    (loop [acc #{#{}}
           cnt 1]
      (if (> cnt k)
        (->> acc
             (filter #(= k (count %))) set)
        (recur (apply clojure.set/union
                      (map (fn [a]
                             (map (fn [v] (conj a v)) s))
                           acc))
               (inc cnt))))))
(comment

(= (k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}})

(= (k-combinations 10 #{4 5 6}) #{})

(= (k-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})

(= (k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})

(= (k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})

(= (k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                    #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))

;;; http://www.4clojure.com/problem/148
(def big-divide
  (fn [n a b]
    (let [sum-below
          (fn [multiplier]
            (let [h (bigint (/ n multiplier))
                  h (if (= n (*' h multiplier)) (dec h) h)]
              (*' multiplier (/ (*' h (inc h)) 2))))
          as (sum-below a)
          bs (sum-below b)
          abs (sum-below (*' a b))]
      (-' (+' as bs) abs))))
;; (= 0 (big-divide 3 17 11))
;; (= 23 (big-divide 10 3 5))
;; (= 233168 (big-divide 1000 3 5))
;; (= "2333333316666668" (str (big-divide 100000000 3 5)))
;; (= "110389610389889610389610" (str (big-divide (* 10000 10000 10000) 7 11)))
;; (= "1277732511922987429116" (str (big-divide (* 10000 10000 10000) 757 809)))
;; (= "4530161696788274281" (str (big-divide (* 10000 10000 1000) 1597 3571)))

;;; http://www.4clojure.com/problem/158
(def decurry
  (fn [fs]
    (fn [& args]
      (reduce (fn [f v] (f v)) fs args))))
(comment
(= 10 ((decurry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))

(= 24 ((decurry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4))

(= 25 ((decurry (fn [a]
             (fn [b]
               (* a b))))
       5 5)))

;;; http://www.4clojure.com/problem/112
(def seq-horriblus
  (fn [max s]
    (letfn
        [(rec [node sum]
           (if (number? node)
             (let [tot (+ node sum)]
               [(if-not (> tot max) node) tot])
             (let [[hd & rst] node
                   [nd sm] (rec hd sum)
                   [tl smm] (if (or (nil? rst) (> sm max))
                              [nil sm]
                              (rec rst sm))]
               [(if (nil? nd) [] (cons nd tl)) smm])))]
      (first (rec s 0)))))
(comment
(=  (seq-horriblus 10 [1 2 [3 [4 5] 6] 7])
   '(1 2 (3 (4))))
(=  (seq-horriblus 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
   '(1 2 (3 (4 (5 (6 (7)))))))
(=  (seq-horriblus 9 (range))
   '(0 1 2 3))
(=  (seq-horriblus 1 [[[[[1]]]]])
   '(((((1))))))
(=  (seq-horriblus 0 [1 2 [3 [4 5] 6] 7])
   '())
(=  (seq-horriblus 0 [0 0 [0 [0]]])
   '(0 0 (0 (0))))
(=  (seq-horriblus 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
   '(-10 (1 (2 3 (4))))))
