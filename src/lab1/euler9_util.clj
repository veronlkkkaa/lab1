(ns lab1.euler9-util
  "Вспомогательные функции для решения Задачи Эйлера 9.")

(defn product
  [coll]
  (reduce *' coll))
