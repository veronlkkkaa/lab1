(ns lab1.euler21-util)

(defn sum-proper-divisors [n]
  (if (<= n 1)
    0
    (->> (range 1 (inc (/ n 2)))
         (filter #(zero? (mod n %)))
         (reduce + 0))))