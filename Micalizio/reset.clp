;;; ##############
;;; RESET MODULE
;;; ##############

(defmodule RESET (import MAIN ?ALL)(import QUESTIONS ?ALL)(import RULES ?ALL)(import TRIP ?ALL)(export ?ALL))

(defrule resetRanks
  (or(resetAll)(resetModify))
  ?rank <- (rank $?)
  ?pathRank <- (pathRank $?)
=>
  (retract ?rank ?pathRank)
)

(defrule resetOAVs
  (or(resetAll)(resetModify))
  ?oav <- (oav)
=>
  (retract ?oav)
)

(defrule resetPaths
  (or(resetAll)(resetModify))
  ?path <- (path(pid ?pid))
=>
  (retract ?path)
)

(defrule resetStages
  (or(resetAll)(resetModify))
  ?stage <- (stage(path ?pid))
  (not(reservation(path ?pid)))
=>
  (retract ?stage)
)

(defrule resetQuestions (declare(salience -1))
  (resetAll)
  ?question <- (question(already-asked TRUE))
=>
  (modify ?question(already-asked FALSE))
)

(defrule deleteResetAll (declare(salience -10))
  ?reset <- (resetAll)
=>
  (retract ?reset)
  (bind ?year (nth$ 1 (local-time)))
  (assert(request(date 1 1 (+ 1 ?year))))
  (focus QUESTIONS RULES TRIP)
)
(defrule deleteResetModify (declare(salience -10))
  ?reset <- (resetModify)
=>
  (retract ?reset)
  (focus QUESTIONS RULES TRIP)
)
