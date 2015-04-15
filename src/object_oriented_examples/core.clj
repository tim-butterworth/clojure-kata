(ns object-oriented-examples.core
  (:gen-class))

;        To try and encourage more sales of the 5 different Harry
;        Potter books they sell, a bookshop has decided to offer
;        discounts of multiple-book purchases.
;
;        One copy of any of the five books costs 8 EUR.
;
;        If, however, you buy two different books, you get a 5%
;        discount on those two books.
;
;        If you buy 3 different books, you get a 10% discount.
;
;        If you buy 4 different books, you get a 20% discount.
;
;        If you go the whole hog, and buy all 5, you get a huge 25%
;        discount.
;
;        Note that if you buy, say, four books, of which 3 are
;        different titles, you get a 10% discount on the 3 that
;        form part of a set, but the fourth book still costs 8 EUR.
;
;        Your mission is to write a piece of code to calculate the
;        price of any conceivable shopping basket (containing only
;        Harry Potter books), giving as big a discount as possible.
;
;        For example, how much does this basket of books cost?
;
;        2 copies of the first book
;        2 copies of the second book
;        2 copies of the third book
;        1 copy of the fourth book
;        1 copy of the fifth book
;
;        Answer: 51.60 EUR

(def books-list [:BOOK1 :BOOK2 :BOOK3 :BOOK4 :BOOK5])
(def modify 
  (let [basis (range 0 (inc 5))] 
    (zipmap 
     basis 
     (map 
      (fn [n] 
        (let [b (* n 5)
              e (if (> n 3) 5 0)]
         (- 105 b e)))
      basis))))

(defn add-last-empty [sets]
  (if 
      (and 
       (not 
        (empty? sets))
       (empty? 
        (deref (last sets))))
    sets
    (conj sets (atom #{}))))

(defn merge-book [sets book]
  (let [result (add-last-empty sets)]
    (loop [remainder result ]
      (let [current (first remainder)]
        (if (contains? (deref current) book)
          (recur (rest remainder))
          (do
            (swap! current conj book)
            result))))))

(defn group-book-sets [books]
  (loop [accume [] remainder books]
    (if (not (empty? remainder))
      (let [current (first remainder)]
          (recur (merge-book accume current) (rest remainder)))
      accume)))

(defn sum-up [books]
  (* 
   (reduce 
    (fn [accume n] 
      (+ accume 8.0))
    0.0
    books) 
  (/ (modify (count books)) 100)))

(defn checkout [books]
  (reduce 
   (fn [accume book-set]
     (+ accume (sum-up @book-set)))
   0.0
   (group-book-sets books)))
