(ns object-oriented-examples.core-test
  (:require [clojure.test :refer :all]
            [object-oriented-examples.core :refer :all]))

(defn book [n]
  (keyword (str "BOOK" n)))
(defn book-set [start end]
  (map 
   (fn [n] (book n)) 
   (range start (inc end))))
(defn compare-atoms [a1 a2 compare]
  (compare @a1 @a2))
(defn atom-lst [lst]
  (vec (map (fn [n] (atom n)) lst)))
(defn equal-atom-lst-compare [lst1 lst2]
  (let [index (range (count lst1))]
    (and
     (= (count lst1) (count lst2))
     (reduce 
     (fn [accume i] 
       (and (compare-atoms (nth lst1 i) (nth lst2 i) #(= %1 %2))))
     true
     index))))

(testing "checkout"
  (testing "one set of books"
    (is (= 15.2 (checkout (book-set 1 2))))
    (is (= 25.6 (checkout (book-set 1 4)))))
  (testing "repeated books"
    (is (= 32.0 (checkout (repeat 4 (book 1)))))
    (is (= 64.0 (checkout (repeat 8 (book 3))))))
  (testing "multiple sets of books"
    (is (= 63.6 (checkout (flatten [(book-set 1 5) (book-set 1 4) (book 1)]))))
    (is (= 51.6 (checkout (flatten [(repeat 2 (book-set 1 3)) (book-set 4 5)])))))
  (testing "sum up book set"
    (is (= 8.0 (sum-up #{:BOOK1})))
    (is (= 15.2 (sum-up #{:BOOK1 :BOOK2}))))
  (testing "grouping book sets"
    (is (equal-atom-lst-compare 
         (group-book-sets (flatten [(repeat 4 (book-set 1 4))])) 
         (flatten [(repeat 4 (atom (set (book-set 1 4)))) (atom #{})]))))
  (testing "merge books"
    (is (equal-atom-lst-compare (atom-lst [#{:BOOK1}]) (merge-book [(atom #{})] :BOOK1))))
  (testing "add last empty"
    (is (equal-atom-lst-compare (atom-lst [#{:a} #{}]) (add-last-empty (atom-lst [#{:a}]))))
    (is (equal-atom-lst-compare (atom-lst [#{}]) (add-last-empty [])))))
