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

(deftest max-gc-test
  (testing "max-gc"
    (let [[id gc]
          (max-gc "test/rosalind_clj/rosalind.fa")]
      (is (and (= id "Rosalind_0808")
               (<= (java.lang.Math/abs (- (* 100.0  gc) 60.919540))
                   0.001))))))