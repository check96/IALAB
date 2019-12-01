
;;; ###############
;;  RULES MODULE
;;; ###############

(defmodule RULES (import MAIN ?ALL)(import QUESTIONS ?ALL)(export ?ALL))

(deftemplate rule
  (slot if)
  (slot then)
  (slot not)
  (slot type)
  (slot coefficient (default 2))
)

(deftemplate oav
  (slot object)
  (slot type)
  (slot cf)
  (slot ranked (default no))
)

(deftemplate stage
  (slot path)
  (slot numPeople)
  (slot hotel)
  (slot location)
  (multislot arrivalDate)
  (multislot awayDate)
  (slot numNights (default 2))
)

(deffacts experience_rules
  (rule(if balneare) (not montano)(type tourism)(coefficient 2))
  (rule(if balneare) (then naturalistico )(type tourism)(coefficient 3))
  (rule(if balneare) (then enogastronomico)(type tourism)(coefficient 3))
  (rule(if balneare) (then sportivo)(not lacustre)(type tourism)(coefficient 4))
  (rule(if montano) (then naturalistico)(type tourism)(coefficient 2))
  (rule(if montano) (then sportivo)(type tourism)(coefficient 3))
  (rule(if montano) (then lacustre)(type tourism)(coefficient 3))
  (rule(if montano) (then religioso)(type tourism)(coefficient 4))
  (rule(if montano) (then enogastronomico)(type tourism)(coefficient 4))
  (rule(if montano) (then culturale)(type tourism)(coefficient 5))
  (rule(if lacustre) (then termale)(type tourism)(coefficient 2))
  (rule(if lacustre) (then naturalistico)(not sportivo)(type tourism)(coefficient 3))
  (rule(if naturalistico) (not religioso)(type tourism)(coefficient 3))
  (rule(if naturalistico) (not culturale)(type tourism)(coefficient 3))
  (rule(if naturalistico) (then sportivo)(type tourism)(coefficient 4))
  (rule(if naturalistico) (then enogastronomico)(type tourism)(coefficient 4))
  (rule(if naturalistico) (then termale)(type tourism)(coefficient 5))
  (rule(if termale)(not sportivo)(type tourism)(coefficient 2))
  (rule(if termale)(not culturale)(type tourism)(coefficient 3))
  (rule(if termale)(not religioso)(type tourism)(coefficient 3))
  (rule(if termale)(not enogastronomico)(type tourism)(coefficient 3))
  (rule(if culturale) (then religioso)(not sportivo)(type tourism)(coefficient 2))
  (rule(if religioso) (not sportivo)(type tourism)(coefficient 2))
  (rule(if sportivo) (not enogastronomico)(type tourism)(coefficient 3))
  (rule(if trentino)(then montano)(not balneare)(type region)(coefficient 0.1))
  (rule(if balneare) (then 6)(type date)(coefficient 0.05))
  (rule(if balneare) (then 7)(type date)(coefficient 0.1))
  (rule(if balneare) (then 8)(type date)(coefficient 0.1))
  (rule(if balneare) (then 9)(type date)(coefficient 0.05))
  (rule(if montano) (then 1)(type date)(coefficient 0.1))
  (rule(if montano) (then 2)(type date)(coefficient 0.08))
  (rule(if montano) (then 7)(type date)(coefficient 0.02))
  (rule(if montano) (then 8)(type date)(coefficient 0.02))
  (rule(if montano) (then 11)(type date)(coefficient 0.08))
  (rule(if montano) (then 12)(type date)(coefficient 0.1))
  (rule(if naturalistico) (then 3)(type date)(coefficient 0.02))
  (rule(if naturalistico) (then 4)(type date)(coefficient 0.05))
  (rule(if naturalistico) (then 5)(type date)(coefficient 0.1))
  (rule(if naturalistico) (then 9)(type date)(coefficient 0.03))
  (rule(if naturalistico) (then 9)(type date)(coefficient 0.1))
  (rule(if sportivo) (then 1)(type date)(coefficient 0.1))
  (rule(if sportivo) (then 2)(type date)(coefficient 0.06))
  (rule(if sportivo) (then 7)(type date)(coefficient 0.07))
  (rule(if sportivo) (then 8)(type date)(coefficient 0.07))
  (rule(if sportivo) (then 11)(type date)(coefficient 0.06))
  (rule(if sportivo) (then 12)(type date)(coefficient 0.1))
  (rule(if lacustre) (then 4)(type date)(coefficient 0.08))
  (rule(if lacustre) (then 5)(type date)(coefficient 0.1))
  (rule(if lacustre) (then 6)(type date)(coefficient 0.08))
  (rule(if lacustre) (then 9)(type date)(coefficient 0.08))
)

(defrule simmetricalRules(declare(salience 1000))
  (rule(if ?type) (then ?type2 & ~nil)(type ?t&~date)(coefficient ?div))
=>
  (assert(rule(if ?type2)(then ?type)(type ?t)(coefficient ?div)))
)

(defrule simmetricalRulesNot(declare(salience 1000))
  (rule(if ?type) (not ?type2 & ~nil)(type ?t&~date)(coefficient ?div))
  (test(neq (type ?type2) INTEGER))
=>
  (assert(rule(if ?type2)(not ?type)(type ?t)(coefficient ?div)))
)

(defrule cf_stars
  (request(numPeople ?numPeople)(stars ?stars))
  (hotel(name ?hotel)(stars ?numStars&:(>= ?numStars ?stars))(numRooms ?numRooms&:(>= ?numRooms (div (+ ?numPeople 1) 2))))
=>
  (assert(oav(object ?hotel)(type stars)(cf (/ ?numStars 4))))
)

(defrule CF_regions
  (request(regions $?regions)(tourismTypes $?tourismTypes)(notRegions $?notRegions))
  (hotel(name ?hotel)(location ?location))
  (location(name ?location)(region ?region))
=>
  (bind ?cf 0.5)
  (if(member$ ?region $?regions)
    then (bind ?cf 0.75))

  (if(member$ ?region $?notRegions)
      then (bind ?cf 0.25))

  (do-for-all-facts ((?r rule)) (and(eq ?r:type region) (eq ?r:then ?region) (member$ ?r:if $?tourismTypes))
    (bind ?cf (+ ?cf ?r:coefficient)))

  (assert(oav(object ?hotel)(type region)(cf ?cf)))
)

(defglobal ?*value* = 0.5)
(defrule CF_tourismType
  (request(tourismTypes $?ttypes)(notTourismTypes $?notTypes))
  (hotel(name ?hotel)(location ?location))
  (location(name ?location)(region ?region))
=>
  (do-for-all-facts ((?s score)) (eq ?s:location ?location)
    (if(member$ ?s:tourismType $?ttypes) then
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes)))))
    (if(member$ ?s:tourismType $?notTypes) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?notTypes)))))
  )
  (do-for-all-facts ((?s score)(?r rule)) (and(eq ?s:location ?location)(eq ?r:type tourism))
    (if(and(member$ ?r:if $?ttypes)(eq ?r:then ?s:tourismType)) then
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes) ?r:coefficient ))))
    (if(and(member$ ?r:if $?ttypes)(eq ?r:not ?s:tourismType)) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?ttypes) ?r:coefficient))))
  )

  (do-for-all-facts ((?s score)(?r rule)) (and(eq ?s:location ?location)(eq ?r:type tourism)(eq ?r:if ?region)(eq ?s:tourismType ?r:then))
    (bind ?*value* (+ ?*value* ?r:coefficient)))

  (assert(oav(object ?hotel)(type tourismType)(cf ?*value*)))
  (bind ?*value* 0.5)
)

(defrule CF_Used
  (request)
  (hotel (name ?hotel))
=>
  (bind ?value 1)
  (do-for-all-facts ((?s stage)) (eq ?s:hotel ?hotel)
    (bind ?value (* ?value 0.95)))
  (assert(oav(object ?hotel)(type used) (cf ?value)))
)

(defrule combineCF (declare(salience -10))
  ?oavR <- (oav(object ?hotel)(type region)(cf ?cfR))
  ?oavT <- (oav(object ?hotel)(type tourismType)(cf ?cfT))
  ?oavS <- (oav(object ?hotel)(type stars)(cf ?cfS))
  ?oavU <- (oav(object ?hotel)(type used)(cf ?cfU))
  =>
    (retract ?oavR ?oavT ?oavS)
    (bind ?cf (* ?cfU (+ (* 0.45 ?cfR) (* 0.45 ?cfT) (* 0.1 ?cfS))))
;    (printout t ?hotel " (R: " ?cfR ", T: " ?cfT ", U: "?cfU ", S: "?cfS ")  ---> " ?cf crlf)
    (modify ?oavU(type all)(cf ?cf))
)

(defrule createRanking (declare (salience -50))
  ?oav <- (oav(object ?hotel)(type all)(cf ?cf&:(> ?cf 0.5))(ranked no))
  (not(oav(object ~?hotel)(type all)(cf ?cf1&:(> ?cf1 ?cf))(ranked no)))
  ?rank <- (rank $?ranks)
  =>
  (retract ?rank)
  (modify ?oav (ranked yes))
  (assert(rank $?ranks ?hotel))
)

(defrule print-results (declare(salience -100))

  (rank $? ?hotel $?)
  (oav(object ?hotel)(type all)(cf ?cf))
  (hotel(name ?hotel)(location ?location))
=>
  ;(printout t ?hotel "  in  " ?location "   -->  " ?cf crlf)
)
