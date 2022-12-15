(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn monkeys []
  [{:items [74 73 57 77 74]
    :history []
    :operation (fn [old]
                 (* old 11))
    :test (fn [item]
            (if (= 0 (% item 19)) 6 7))}
   {:items [99 77 79]
    :history []
    :operation (fn [old]
                 (+ old 8))
    :test (fn [item]
            (if (= 0 (% item 2)) 6 0))}
   {:items [64 67 50 96 89 82 82]
    :history []
    :operation (fn [old]
                 (+ old 1))
    :test (fn [item]
            (if (= 0 (% item 3)) 5 3))}
   {:items [88]
    :history []
    :operation (fn [old]
                 (* old 7))
    :test (fn [item]
            (if (= 0 (% item 17)) 5 4))}
   {:items [80 66 98 83 70 63 57 66]
    :history []
    :operation (fn [old]
                 (+ old 4))
    :test (fn [item]
            (if (= 0 (% item 13)) 0 1))}
   {:items [81 93 90 61 62 64]
    :history []
    :operation (fn [old]
                 (+ old 7))
    :test (fn [item]
            (if (= 0 (% item 7)) 1 4))}
   {:items [69 97 88 93]
    :history []
    :operation (fn [old]
                 (* old old))
    :test (fn [item]
            (if (= 0 (% item 5)) 7 2))}
   {:items [59 80]
    :history []
    :operation (fn [old]
                 (+ old 6))
    :test (fn [item]
            (if (= 0 (% item 11)) 2 3))}])

;; Modular arithmetic apparently
(local worry-number (* 19 2 3 17 13 7 5 11))

; (fn monkeys []
;   [{:items [79 98]
;     :history []
;     :operation (fn [old]
;                  (* old 19))
;     :test (fn [item]
;             (if (= 0 (% item 23)) 2 3))}
;    {:items [54 65 75 74]
;     :history []
;     :operation (fn [old]
;                  (+ old 6))
;     :test (fn [item]
;             (if (= 0 (% item 19)) 2 0))}
;    {:items [79 60 97]
;     :history []
;     :operation (fn [old]
;                  (* old old))
;     :test (fn [item]
;             (if (= 0 (% item 13)) 1 3))}
;    {:items [74]
;     :history []
;     :operation (fn [old]
;                  (+ old 3))
;     :test (fn [item]
;             (if (= 0 (% item 17)) 0 1))}
;             ])

; ;; Modular arithmetic apparently
; (local worry-number (* 23 19 13 17))

(fn keep-away [monkeys rounds]
  (for [n 1 rounds]
    (each [i monkey (ipairs monkeys)]
      (var item (table.remove (. monkey :items) 1))
      (while item
        (table.insert (. monkey :history) item)
        (let [;; Monkey inspect item
              inspected ((. monkey :operation) item)
              ;; You are relieved it's ok
              new (math.floor (/ inspected 3))
              ;; Monkey decides who to throw it to
              new-monkey (+ ((. monkey :test) new) 1)]
          ;; Throw to the new monkey
          (table.insert (. monkeys new-monkey :items) new))
        (set item (table.remove (. monkey :items) 1)))))
  monkeys)

(fn keep-away-2 [monkeys rounds]
  (for [n 1 rounds]
    (each [i monkey (ipairs monkeys)]
      (var item (table.remove (. monkey :items) 1))
      (while item
        (table.insert (. monkey :history) item)
        (let [;; Monkey inspect item
              inspected (% ((. monkey :operation) item) worry-number)
              ;; Monkey decides who to throw it to
              new-monkey (+ ((. monkey :test) inspected) 1)]
          ;; Throw to the new monkey
          (table.insert (. monkeys new-monkey :items) inspected))
        (set item (table.remove (. monkey :items) 1)))))
  monkeys)

(fn part-1 []
  (fennel.view (-> (monkeys) (keep-away 20)
                   (lume.map #(length (. $1 :history))) (lume.sort)
                   (lume.last 2) (lume.reduce #(* $1 $2)))))

(fn part-2 []
  (fennel.view (-> (monkeys) (keep-away-2 10000)
                   (lume.map #(length (. $1 :history))) (lume.sort)
                   (lume.last 2) (lume.reduce #(* $1 $2)))))

{: part-1 : part-2}
