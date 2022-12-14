(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (var input [])
  (var current [])
  (each [line (io.lines :./solutions/inputs/13.txt)]
    (match line
      "" (do
           (table.insert input current)
           (set current []))
      _ (let [(list _) (string.gsub line "," " ")]
          (table.insert current (fennel.eval list)))))
  input)

(fn compare [a b]
  (if ;; Both numbers
      (and (= :number (type a)) (= :number (type b)))
      (if (< a b) :valid
          (< b a) :invalid
          nil)
      ;; Exactly one is a number
      (and (= :number (type a)) (= :table (type b)))
      (compare [a] b)
      (and (= :table (type a)) (= :number (type b)))
      (compare a [b])
      ;; Both lists
      (and (= 0 (length a)) (= 0 (length b)))
      nil
      (= 0 (length a))
      :valid
      (= 0 (length b))
      :invalid
      (let [result (compare (. a 1) (. b 1))]
        (if result result (compare (lume.slice a 2) (lume.slice b 2))))))

(fn compare-packets [input]
  (icollect [_ [left right] (ipairs input)]
    (compare left right)))

(fn count-valid [results]
  (accumulate [sum 0 i v (ipairs results)]
    (if (= v :valid) (+ sum i) sum)))

(fn add-divider-packets [input]
  (accumulate [all-packets [[[2]] [[6]]] _ [first second] (ipairs input)]
    [first second (table.unpack all-packets)]))

(fn comparator [a b]
  (= (compare a b) :valid))

(fn find-dividers [sorted-packets]
  (accumulate [product 1 i v (ipairs sorted-packets)]
    (if (or (= (fennel.view v) (fennel.view [[2]]))
            (= (fennel.view v) (fennel.view [[6]])))
        (* product i)
        product)))

(fn part-1 []
  (fennel.view (-> (read-input) (compare-packets) (count-valid))))

(fn part-2 []
  (fennel.view (-> (read-input) (add-divider-packets) (lume.sort comparator)
                   (find-dividers))))

{: part-1 : part-2}
