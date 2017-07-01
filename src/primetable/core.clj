(ns primetable.core)

(defn composite? [table i]
  (not (nil? (nth table i))))

(defn mark [table i f]
  (if (nil? (nth table i))
    (assoc table i f)
    table))

(defn mark-primes-sub [table f]
  (let [n (count table)]
    (reduce (fn [table i] (mark table i f))
            table
            (take-while
             #(< % n)
             (map #(* f %)
                  (iterate inc 2))))))

(defn mark-primes [table i]
  (if (or (< i 2) (composite? table i))
    table
    (mark-primes-sub table i)))

(defn make-first-factor-table
  "Create a vector where element i is nil if it is a prime number, 
  or the first prime factor if it is a composite number"
  [n]
  (reduce
   mark-primes
   (vec (repeat n nil))
   (range n)))
