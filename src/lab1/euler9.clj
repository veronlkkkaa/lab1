(ns lab1.euler9)

(defn product [coll]
  (reduce *' coll))

 ;; 1. Хвостовая рекурсия
(defn tail-rec []
  (loop [a 1 b 2]
    (when-not (> a 333)
      (let [c (- 1000 a b)]
        (cond
          (> b c) (recur (inc a) (inc (inc a)))
          (= (+ (* a a) (* b b)) (* c c)) (* a b c)
          :else (recur a (inc b)))))))

(defn solve-rec []
  (loop [a 1 b 2]
    (when-not (> a 333)
      (let [c (- 1000 a b)]
        (cond
          (> b c) (recur (inc a) (inc a))  ; если b > c, увеличиваем a и сбрасываем b на a+1
          (= (+ (* a a) (* b b)) (* c c)) (* a b c)
          :else (recur a (inc b)))))))

;; 3. Модульный подход
(defn generate-triples []
  (for [a (range 1 334)
        b (range (inc a) 500)
        :let [c (- 1000 a b)]
        :when (and (> c b) (= (+ (* a a) (* b b)) (* c c)))]
    [a b c]))

(defn solve-modular []
  (->> (generate-triples)
       first
       (apply *)))

;; 4. map/flatten
(defn solve-with-map []
  (->> (range 1 334)
       (mapcat (fn [a]
                 (map (fn [b]
                        (let [c (- 1000 a b)]
                          (when (and (> c b)
                                     (= (+ (* a a) (* b b)) (* c c)))
                            (* a b c))))
                      (range (inc a) 500))))
       (remove nil?)
       first))

;; 5. Ленивый генератор
(defn lazy-triples []
  (letfn [(triples-gen [a b]
            (let [c (- 1000 a b)]
              (lazy-seq
               (cond
                 (> a 333) nil
                 (> b c) (triples-gen (inc a) (inc (inc a)))
                 (= (+ (* a a) (* b b)) (* c c))
                 (cons (* a b c) (triples-gen a (inc b)))
                 :else (triples-gen a (inc b))))))]
    (triples-gen 1 2)))

(defn solve-lazy []
  (first (lazy-triples)))