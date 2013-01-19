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