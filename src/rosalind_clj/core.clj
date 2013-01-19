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

;;; HAMM
(defn hamming [x y]
  "Given two strings of equal length, return the number of positions at which the strings differ."
  (reduce + (map (fn [x y] (if (= x y) 0 1)) x y)))

;;; SUBS
(defn substr-pos [s t]
  (loop [m (re-matcher (re-pattern (str/join ["(?=(" t ".*))"])) s)
         positions []]
    (if (.find m)
      (recur m (conj positions (inc (.start m))))
      positions)))