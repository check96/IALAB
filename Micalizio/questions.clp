;;; #################
;;; QUESTIONS MODULE
;;; #################

(defmodule QUESTIONS (import MAIN ?ALL)(export ?ALL))

(deftemplate question
   (slot attribute)
   (slot the-question)
   (slot already-asked (default FALSE))
   (slot type)
   (multislot valid-answers)
)

(deftemplate attribute
  (slot name)
  (multislot value)
)

(deffacts question-attributes
  (question (attribute price)
            (the-question "Budget?")
            (type INTEGER))
  (question (attribute stars)
            (the-question "Qualità dell'albergo? inserire numero minimo di stelle")
            (type INTEGER)
            (valid-answers 1 2 3 4 5))
  (question (attribute numPeople)
            (the-question "Quante persone siete? inserire numero ")
            (type INTEGER))
  (question (attribute date)
            (the-question "In che data vorresti partire? (dd mm yyyy)"))
  (question (attribute spostamenti)
            (the-question "Durante la vacanza, preferisci spostarti molto o poco?")
            (type STRING)
            (valid-answers "molto" "poco"))
  (question (attribute nights)
            (the-question "Quanti giorni preferisti che durasse la vacanza? inserire numero dei giorni")
            (type INTEGER))
  (question (attribute notRegions)
            (the-question "Regioni che non vuoi visitare? inserire nome delle regioni ")
            (type STRING)
            (valid-answers "calabria" "puglia" "basilicata" "campania" "piemonte" "veneto" "liguria" "trentino" "toscana" "emiliaromagna"))
  (question (attribute regions)
            (the-question "Preferenze sulle regioni da visitare? inserire nome delle regioni ")
            (type STRING)
            (valid-answers "calabria" "puglia" "basilicata" "campania" "piemonte" "veneto" "liguria" "toscana" "trentino" "emiliaromagna"))
  (question (attribute notTourismTypes)
            (the-question "Ci sono tipi di località che preferiresti evitare?")
            (type STRING)
            (valid-answers "balneare" "montano" "lacustre" "naturalistico" "termale" "culturale" "religioso" "sportivo" "enogastronomico"))
  (question (attribute tourismTypes)
            (the-question "Che tipo di località vuoi visitare? ")
            (type STRING)
            (valid-answers "balneare" "montano" "lacustre" "naturalistico" "termale" "culturale" "religioso" "sportivo" "enogastronomico"))
)

(deffunction ask-question (?question ?type ?valid-answers)
 	(printout t ?question crlf)
 	(bind $?a (readline))

  (while TRUE do
      (if (eq $?a "") then
        (return $?a))

      (bind $?answer (explode$ $?a))
      (bind ?wrong FALSE)
      (loop-for-count (?i 1 (length$ $?answer)) do
        (bind ?answ (nth$ ?i $?answer))
        (if(neq (type ?answ) INTEGER) then
          (bind ?answ (lowcase (str-cat ?answ))))
        (if(or (neq (type ?answ) ?type) (and (<> (length$ ?valid-answers) 0) (not(member$ ?answ ?valid-answers)))) then
            (bind ?wrong TRUE)
            (break))
      )

      (if (eq ?wrong TRUE)
        then
          (printout t "Hai inserito un valore non valido. Riprova." crlf)
          (bind $?a (readline))
        else
          (return (lowcase $?a)))
  )
)

(defrule ask-a-question
   	?question <- (question (already-asked FALSE)
                   	(the-question ?the-question)
                   	(attribute ?the-attribute)
                    (type ?type)
                    (valid-answers $?valid-answers))
   	=>

    (bind $?value (explode$ (ask-question ?the-question ?type ?valid-answers)))
    (bind $?daysForMonth (create$ 31 28 31 30 31 30 31 31 30 31 30 31))
    (bind ?error FALSE)
    (if (and(> (length$ $?value) 0) (eq ?the-attribute date))
      then
       (bind ?day (integer(nth$ 1 $?value)))
       (bind ?month (integer(nth$ 2 $?value)))

       (if (or(< ?month 0)(> ?month 12))
          then
            (bind ?error TRUE))

       (if (and(eq ?error FALSE)(or(< ?day 0)(> ?day (nth$ ?month $?daysForMonth))))
          then
            (bind ?error TRUE))
    )
    (if (eq ?error FALSE)
      then
        (modify ?question (already-asked TRUE))
       	(assert (attribute (name ?the-attribute) (value $?value))))
)

(defrule compile
    ?attr <- (attribute (name ?n) (value $?v))
    ?r <- (request)
    (test(<>(length$ ?v) 0))
    =>
    (retract ?attr)

    (switch ?n
      (case numPeople then (modify ?r (numPeople (nth$ 1 ?v))))
      (case regions then (modify ?r (regions ?v)))
      (case notRegions then (modify ?r (notRegions ?v)))
      (case tourismTypes then (modify ?r (tourismTypes ?v)))
      (case notTourismTypes then (modify ?r (notTourismTypes ?v)))
      (case stars then (modify ?r (stars (nth$ 1 ?v))))
      (case nights then (modify ?r (nights (nth$ 1 ?v))))
      (case spostamenti then (modify ?r (spostamenti (nth$ 1 ?v))))
      (case price then (modify ?r (price (nth$ 1 ?v))))
      (case date then (modify ?r (date ?v)))
    )
)

(defrule lastChecks (declare(salience -10))
  ?req <- (request(numPeople ?numPeople)(spostamenti ?value)(nights ?nights)(price ?budget))
=>
  (if (eq ?value molto)
      then
        (modify ?req(minLocations (+ 2 (div ?nights 8))) (maxLocations (+ 3 (div ?nights 8))))
      else
        (modify ?req(minLocations (+ 1 (div ?nights 8))) (maxLocations (+ 2 (div ?nights 8))))
  )

  (if(= ?budget 0)
    then
      (modify ?req(price (* ?nights 125 (div (+ ?numPeople 1) 2))))
    else
      (if (< ?budget (* ?nights 75 (div (+ ?numPeople 1) 2)))
        then
          (modify ?req (nights (div ?budget 125))))
  )

  (assert(rank (create$ ))(pathRank (create$ )))
)
