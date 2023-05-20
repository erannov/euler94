(ns euler94.core
  (:gen-class))

(defn sq[s](* s s))
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn make-pythagorean
  [t s]
  (vector (min (* 2 s t)(- (sq s)(sq t))) (+ (sq s)(sq t))))

(defn valid-ans2?
  [[a c]]
  (or
    (= 1 (rem c (* 2 a)))
    (= 1 (rem (* 2 a) c))))

(defn perimeter-of-pythagorean
  [[a c]]
  (* 2 (+ a c)))

(defn solve
  [perimeter]
  (let [max-st (int (Math/sqrt (/ (inc perimeter) 3)))]
    (reduce
      (fn[r x](+ r
                 (reduce
                   (fn[r y]
                     (let [tup (make-pythagorean x y)]
                       (+ r
                          (if (valid-ans2? tup)
                            (perimeter-of-pythagorean tup)
                            0))))
                   0
                   (range (inc x)  (min max-st (inc (* x 4))) 2))))
      0
      (range 1 (/ max-st 6)))))

(defn -main
  [& args]
  (println(time(solve 1000000000))))
