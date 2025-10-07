(ns lab1.euler21)

;; Быстрая версия sum-proper-divisors: перебираем до sqrt(n), добавляем пары делителей.
(defn sum-proper-divisors [n]
  (cond
    (<= n 1) 0
    :else
    (let [sqrt-n (long (Math/floor (Math/sqrt n)))]
      (loop [i 2 sum 1]
        (if (> i sqrt-n)
          sum
          (if (zero? (mod n i))
            (let [other (quot n i)]
              (recur (inc i)
                     (if (= other i)
                       (+ sum i)
                       (+ sum i other))))
            (recur (inc i) sum)))))))

;; --- Решения ---

;; 1. Монолитная реализация с хвостовой рекурсией
(defn solve-tail-rec []
  (loop [n 1 total 0]
    (if (> n 9999)
      total
      (let [d-n (sum-proper-divisors n)
            d-d-n (sum-proper-divisors d-n)]
        (if (and (= d-d-n n)
                 (not= d-n n))
          (recur (inc n) (+ total n))
          (recur (inc n) total))))))

;; 2. Обычная рекурсивная версия (с сохранением интерфейса run-rec)
(defn solve-rec []
  (loop [n 1 total 0]
    (if (> n 9999)
      total
      (let [d-n (sum-proper-divisors n)
            d-d-n (sum-proper-divisors d-n)]
        (if (and (= d-d-n n)
                 (not= d-n n))
          (recur (inc n) (+ total n))
          (recur (inc n) total))))))

;; 3. Модульный/функциональный подход
(defn amicable? [n]
  (let [d-n (sum-proper-divisors n)
        d-d-n (sum-proper-divisors d-n)]
    (and (= d-d-n n)
         (not= d-n n))))

(defn solve-modular []
  (->> (range 1 10000)
       (filter amicable?)
       (reduce + 0)))

;; 4. Использование for
(defn solve-for []
  (->> (for [n (range 1 10000)
             :let [d-n (sum-proper-divisors n)
                   d-d-n (sum-proper-divisors d-n)]
             :when (and (= d-d-n n)
                        (not= d-n n))]
         n)
       (reduce + 0)))

;; 5. Ленивые последовательности
(defn lazy-amicables []
  (letfn [(amicable? [n]
            (let [d-n (sum-proper-divisors n)
                  d-d-n (sum-proper-divisors d-n)]
              (and (= d-d-n n)
                   (not= d-n n))))]
    (filter amicable? (range 1 10000))))

(defn solve-lazy []
  (reduce + 0 (lazy-amicables)))
