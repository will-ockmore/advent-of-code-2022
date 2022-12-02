(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/2.txt)]
    (icollect [turn _ (string.gmatch line "(%u)")]
      turn)))

(fn their-turn [choice]
  (match choice
    :A :rock
    :B :paper
    :C :scissors))

(fn our-turn [choice]
  (match choice
    :X :rock
    :Y :paper
    :Z :scissors))

(fn outcome [turn]
  (match turn
    [:rock :rock] :draw
    [:rock :paper] :loss
    [:rock :scissors] :win
    [:paper :paper] :draw
    [:paper :rock] :win
    [:paper :scissors] :loss
    [:scissors :scissors] :draw
    [:scissors :rock] :loss
    [:scissors :paper] :win))

(fn score [turn]
  (let [(first second) (table.unpack turn)
        theirs (their-turn first)
        ours (our-turn second)
        result (outcome [ours theirs])
        score-for-choice (match ours
                           :rock 1
                           :paper 2
                           :scissors 3)
        score-for-result (match result
                           :loss 0
                           :draw 3
                           :win 6)]
    (+ score-for-choice score-for-result)))

(fn count-score [score-fn turns]
  (accumulate [sum 0 _ turn (ipairs turns)]
    (if (> (length turn) 0)
        (+ sum (score-fn turn))
        sum)))

(fn get-turn [theirs required-result]
  (do (print theirs required-result) (match [theirs required-result]
     [first :draw] first
     [:rock :win] :paper
     [:rock :loss] :scissors
     [:paper :win] :scissors
     [:paper :loss] :rock
     [:scissors :win] :rock
     [:scissors :loss] :paper)))

(fn score-2 [turn]
  (let [(first second) (table.unpack turn)
        theirs (their-turn first)
        required-result (match second :X :loss :Y :draw :Z :win)
        ours (get-turn theirs required-result)
        result (outcome [ours theirs])
        score-for-choice (match ours
                           :rock 1
                           :paper 2
                           :scissors 3)
        score-for-result (match result
                           :loss 0
                           :draw 3
                           :win 6)]
    (+ score-for-choice score-for-result)))

(fn part-1 []
  (fennel.view (->> (read-input) (count-score score))))

(fn part-2 []
  (fennel.view (->> (read-input) (count-score score-2))))

{: part-1 : part-2}
