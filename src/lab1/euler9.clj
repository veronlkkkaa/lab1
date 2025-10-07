(ns lab1.euler9)

;; Вспомогательная функция product (умножение коллекции, безопасно для больших чисел)
(defn product
  [coll]
  (reduce *' coll))

; --- Решения ---

; 1. Монолитная реализация с хвостовой рекурсией (исправлено: один loop с a и b)
(defn tail-rec []
  (loop [a 1 b 2]
    (if (> a 333)
      nil
      (let [c (- 1000 a b)]
        (cond
          (> b c) (recur (inc a) (inc (inc a))) ; перейти к следующему a, установить b = a'+1
          (= (+ (* a a) (* b b)) (* c c)) (* a b c)
          :else (recur a (inc b)))))))

; 2. Монолитная реализация с обычной рекурсией.
(defn solve-rec []
  (loop [a 1 b 2]
    (if (> a 333)
      nil
      (let [c (- 1000 a b)]
        (cond
          (> b c) (recur (inc a) (inc (inc a))) ; следующий a, b = a'+1
          (= (+ (* a a) (* b b)) (* c c)) (* a b c)
          :else (recur a (inc b)))))))

; 3. Модульный подход (генерация тройок).
(defn generate-triples []
  (for [a (range 1 334)
        b (range (inc a) 500) ; b > a, и b < c => b < 500 примерно
        :let [c (- 1000 a b)]
        :when (and (> c b) (= (+ (* a a) (* b b)) (* c c)))]
    [a b c]))

(defn solve-modular []
  (->> (generate-triples)
       (first)
       (apply *)))

; 4. Использование map/flatten.
(defn solve-with-map []
  (->> (range 1 334)
       (mapcat (fn [a]
                 (map (fn [b]
                        (let [c (- 1000 a b)]
                          (if (and (> c b) (= (+ (* a a) (* b b)) (* c c)))
                            (* a b c)
                            nil)))
                      (range (inc a) 500))))
       (remove nil?)
       (first)))

; 5. Ленивый подход с генератором
(defn lazy-triples []
  (letfn [(triples-gen [a b]
            (let [c (- 1000 a b)]
              (lazy-seq
               (cond
                 (> a 333) nil
                 (> b c) (triples-gen (inc a) (inc (inc a)))
                 (= (+ (* a a) (* b b)) (* c c)) (cons (* a b c) (triples-gen a (inc b)))
                 :else (triples-gen a (inc b))))))]
    (triples-gen 1 2)))

(defn solve-lazy []
  (first (lazy-triples)))