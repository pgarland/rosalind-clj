(ns rosalind-clj.core-test
  (:use clojure.test
        rosalind-clj.core))

(deftest count-nuc-test
  (testing "count-nuc"
    (is (= (count-nuc "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
           "20 12 17 21"))))

(deftest transcribe-test
  (testing "transcribe"
    (is (=  (transcribe "GATGGAACTTGACTACGTAAATT")
            "GAUGGAACUUGACUACGUAAAUU"))))

(deftest rev-comp-test
  (testing "rev-comp"
    (is (= (rev-comp  "AAAACCCGGT")
           "ACCGGGTTTT"))))

;; (deftest max-gc-test
;;   (testing "max-gc"
;;     (let [[id gc]
;;           (max-gc "test/rosalind_clj/rosalind.fa")]
;;       (is (and (= id "Rosalind_0808")
;;                (<= (java.lang.Math/abs (- (* 100.0  gc) 60.919540))
;;                    0.001))))))

(deftest max-gc-test
  (testing "max-gc"
    (let [[id gc]
          (max-gc "test/rosalind_clj/rosalind.fa")]
      (is (= id "Rosalind_0808"))
      (is (<= (java.lang.Math/abs (- (* 100.0  gc) 60.919540))
              0.001)))))

(deftest hamming-test
  (testing "hamming"
    (is (= (hamming "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT")
           7))))

(deftest substr-test
  (testing "substr"
    (is (= (substr-pos "GATATATGCATATACTT" "ATAT")
           [2 4 10]))))

(deftest consensus-test
  (testing "consensus"
    (let [con (consensus "test/rosalind_clj/consensus.txt")]
      (is (= (:consensus con)
             "ATGCAACT"))
      (is (= (seq ((:matrix con) \A))
             [5 1 0 0 5 5 0 0]))
      (is (= (seq ((:matrix con) \C))
             [0 0 1 4 2 0 6 1]))
      (is (= (seq ((:matrix con) \G))
             [1 1 6 3 0 1 0 0]))
      (is (= (seq ((:matrix con) \T)) 
             [1 5 0 0 0 1 1 6])))))

(deftest overlap-test
  (testing "overlap"
    (let [adj-list (overlap "test/rosalind_clj/overlap.txt" 3)]
      (is (= (adj-list "Rosalind_5013") ()))
      (is (= (adj-list "Rosalind_0442") ()))
      (is (= (adj-list "Rosalind_2323") ()))
      (is (= (adj-list "Rosalind_2391") '("Rosalind_2323")))
      (is (= (adj-list "Rosalind_0498") '("Rosalind_2391" "Rosalind_0442"))))))