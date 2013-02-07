(ns rosalind-clj.core
  (:require [clojure.string :as str]))

;;; DNA
(defn count-nuc [dna]
  "Count the number of occurances of each base in a str"
  (str/join " " (map #(get (frequencies dna) % 0) [\A \C \G \T])))

;;; RNA
(defn transcribe [dna]
  "Return the sequence transcribed into RNA"
  (str/replace dna \T \U))

;;; REVC
(defn rev-comp [dna]
  "Return the reverse complement of the sequence"
  (let [rc-map {\A \T, \T \A, \G \C, \C \G}]
    (str/reverse (str/join (for [base dna] (rc-map base))))))

;;; GC
(defn read-fa [file]
  "Read a fasta file into a map, where each key is the id of the sequence and each value is the sequence"
  (let [fa (slurp file)]
    (reduce (fn [map [id & dna-lines]] (assoc map id (str/join dna-lines))) ; map each id to it's dna sequence
            {} 
            (map #(str/split % #"\n") ; break apart the lines in each record
                 (rest (str/split fa #">")))))) ; separate the records 

(defn gc-count [dna]
  "Return the percentage of GC bases in DNA"
  (let [counts (frequencies dna)]
    (float (/ (+ (get counts \G 0) 
                 (get counts \C 0))
              (count dna)))))

(defn max-gc [fa]
  "Return the id and GC percentage of the sequence with the highest percentage of GC bases"
  (let [fa-map (read-fa fa)
        k (apply max-key #(gc-count (fa-map %)) (keys fa-map))]
    [k (gc-count (fa-map k))]))

(defn print-max-gc [fa]
  (let [[id gc] (max-gc fa)]
    (println id) 
    (println (str (* 100 gc) "%"))))

;;; HAMM
(defn hamming [x y]
  "Given two strings of equal length, return the number of positions at which the strings differ."
  (reduce + (map (fn [x y] (if (= x y) 0 1)) x y)))

;;; SUBS
(defn substr-pos [s t]
  "Return a sequence contains the positions of substring t in s"
  (loop [m (re-matcher (re-pattern (str/join ["(?=(" t ".*))"])) s)
         positions []]
    (if (.find m)
      (recur m (conj positions (inc (.start m))))
      positions)))

;;; CONS
(defn consensus [f]
  "Given a file containing DNA strings of equal length, return a consensus string map with a matrix containing the counts for each base at each postion."
  (let [seqs (str/split (slurp f) #"\n")
        len (count (first seqs))
        ;; Create a map keyed by base, with each val an empty array corresponding to a position in the sequence
        mat (reduce (fn [m k] (assoc m k (make-array Integer/TYPE len))) 
                  {} 
                  [\A \C \G \T])]
    (doseq [s seqs
            idx (range len)]
      (let [chr (.charAt s idx)
            val (aget (mat chr) idx)]
        (aset (mat chr) idx (inc val))))
    {:matrix mat, 
     :consensus (apply str (for [idx (range len)]
                             (apply max-key 
                                    (fn [base] (aget (mat base) idx))
                                    [\A \C \G \T])))}))

(defn consensus-soln [f]
  (let [c (consensus f)]
    (println (:consensus c))
    (doseq [[base cnt] (:matrix c)] 
      (print (str base ":") 
             (str/join " " (seq cnt))))))


;;; GRPH
(defn build-suffix-lists [seqs suff-len]
    "Given a map of strings keyed by id, and a prefix length, return a
map indexed by unique suffixes of that length, where each value is a
list of ids with that suffix"
  (reduce-kv (fn [m id dna] 
               (let [suff-start (- (count dna) suff-len)]
                 (update-in m [(subs dna suff-start)] conj id)))
             {} 
             seqs))

(defn build-prefix-lists [seqs pre-len]
  "Given a map of strings keyed by id, and a prefix length, return a
map indexed by unique prefixes of that length, where each value is a
list of ids with that prefix"
  (reduce-kv (fn [m id dna] 
               (let [prefix (subs dna 0 pre-len)]
                 (update-in m [prefix] conj id)))
             {} 
             seqs))

(defn overlap [f k]
  "Return the k-overlap graph of the DNAs in fasta file f"
  (let [seqs (read-fa f)
        pres (build-suffix-lists seqs k)
        suffs (build-prefix-lists seqs k)]
    (reduce-kv (fn [m fix parents]
                 (reduce (fn [m p]
                           (let [children  (remove #{p} (suffs fix))]
                             (assoc m p children)))
                         m
                         parents))
               {}
               pres)))

(defn print-adj-list [adj-list]
  "Print an adjacency list, with each vertex pair on a separate line"
  (doseq [[k v] adj-list 
          node v] 
    (when (seq v) 
      (println k node))))

;;; IPRB
(defn mendelian [hd hetero hr]
  "Return the probability that the random offspring of a population shows the dominant phenotype"
  (let [indv (+ hd hetero hr)]
     ;; Find the complement of the probability that a random offspring shows the recessive phenotype
    (float (- 1 (+ 
                 ;; Aa x Aa
                 (* (/ hetero indv)
                    (/ (dec hetero) (dec indv))
                    (/ 1 4))
                 ;; Aa x aa and aa x Aa
                 (* 2 
                    (/ hetero indv)
                    (/ hr (dec indv))
                    (/ 1 2))
                 ;; aa x aa
                 (* (/ hr indv)
                    (/ (dec hr) (dec indv))
                    1))))))

(def transl-map {"UUU" \F,      "CUU" \L,      "AUU" \I,      "GUU" \V
                 "UUC" \F,      "CUC" \L,      "AUC" \I,      "GUC" \V,
                 "UUA" \L,      "CUA" \L,      "AUA" \I,      "GUA" \V,
                 "UUG" \L,      "CUG" \L,      "AUG" \M,      "GUG" \V,
                 "UCU" \S,      "CCU" \P,      "ACU" \T,      "GCU" \A,
                 "UCC" \S,      "CCC" \P,      "ACC" \T,      "GCC" \A,
                 "UCA" \S,      "CCA" \P,      "ACA" \T,      "GCA" \A,
                 "UCG" \S,      "CCG" \P,      "ACG" \T,      "GCG" \A,
                 "UAU" \Y,      "CAU" \H,      "AAU" \N,      "GAU" \D,
                 "UAC" \Y,      "CAC" \H,      "AAC" \N,      "GAC" \D,
                 "UAA" nil,   "CAA" \Q,      "AAA" \K,      "GAA" \E,
                 "UAG" nil,   "CAG" \Q,      "AAG" \K,      "GAG" \E,
                 "UGU" \C,      "CGU" \R,      "AGU" \S,      "GGU" \G,
                 "UGC" \C,      "CGC" \R,      "AGC" \S,      "GGC" \G,
                 "UGA" nil,   "CGA" \R,      "AGA" \R,      "GGA" \G,
                 "UGG" \W,      "CGG" \R,      "AGG" \R,      "GGG" \G})

(defn translate [rna]
  "Returns a lazy-sequence of amino acids translated from RNA"
  (lazy-seq 
   (if-let [[x y z & xs] rna]
     (cons (transl-map (str/join (list x y z)))
           (translate xs))
     ())))
