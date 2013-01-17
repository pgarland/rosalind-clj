(ns com.obscureshapes.rosalind.core
  (:require [clojure.string :as string]))

;;; DNA
(defn count-nuc [dna]
  "Count the number of occurances of each base in a string"
  (string/join " " (map #(get (frequencies dna) % 0) [\A \T \C \G])))

;;; RNA
(defn transcribe [dna]
  "Return the sequence transcribed into RNA"
  (string/replace dna \T \U))

;;; REVC
(defn rev-comp [dna]
  "Return the reverse complement of the sequence"
  (let [rc-map {\A \T, \T \A, \G \C, \C \G}]
    (string/reverse (string/join (for [base dna] (rc-map base))))))
