(defmodule MAIN (export ?ALL))

(deftemplate location
  (slot name)
  (slot region)
  (multislot tourismTypes (allowed-values Balneare Montano Lacustre Naturalistico Termale Culturale Religioso Sportivo Enogastronomico))
)

(deftemplate hotel
  (slot name)
  (slot stars (type INTEGER) (range 1 4))
  (slot numRooms (type INTEGER))
  (slot location)
)

(deftemplate request
  (slot name (default Person))
  (slot numPeople (default 1)(type INTEGER))
  (multislot regions)
  (multislot notRegions)
  (multislot tourismTypes)
  (multislot notTourismTypes)
  (slot stars (default 1)(type INTEGER))
  (slot minLocations (default 1) (type INTEGER))
  (slot maxLocations (default 5) (type INTEGER))
  (multislot arrivalDate)
  (multislot awayDate)
  (slot nights (default 1) (type INTEGER))
  (slot price (default 0) (type INTEGER))
)

(deftemplate reservation
  (slot name)
  (multislot hotels)
;; DA COMPLETARE
)

(deftemplate distance
  (slot from)
  (slot to)
  (slot distance)
)

(deftemplate score
  (slot location)
  (slot tourismType)
  (slot score (type INTEGER) (default 0) (range 0 5))
)

(deffacts locations
  (location(name Schiavonea)(region Calabria)(tourismTypes Balneare Naturalistico))
  (location(name Camigliatello)(region Calabria)(tourismTypes Montano Naturalistico Lacustre))
  (location(name Castrovillari)(region Calabria)(tourismTypes Culturale Enogastronomico Naturalistico))
  (location(name Bari)(region Puglia)(tourismTypes Balneare Enogastronomico))
  (location(name Polignano)(region Puglia)(tourismTypes Balneare Enogastronomico))
  (location(name Alberobello)(region Puglia)(tourismTypes Culturale Enogastronomico))
  (location(name Matera)(region Basilicata)(tourismTypes Culturale))
  (location(name Lauria)(region Basilicata)(tourismTypes Termale Lacustre))
  (location(name Napoli)(region Campania)(tourismTypes Balneare Culturale Enogastronomico Sportivo))
  (location(name Caserta)(region Campania)(tourismTypes Culturale))
)

(deffacts hotels
  (hotel(name CastroHotel)(stars 3)(numRooms 20)(location Castrovillari))
  (hotel(name SeaHotel)(stars 4)(numRooms 20)(location Schiavonea))
  (hotel(name SilaHotel)(stars 3)(numRooms 30)(location Camigliatello))
  (hotel(name TermeHotel)(stars 4)(numRooms 25)(location Lauria))
  (hotel(name MateHotel)(stars 3)(numRooms 30)(location Matera))
  (hotel(name BariHotel)(stars 3)(numRooms 30)(location Bari))
  (hotel(name PoliHotel)(stars 4)(numRooms 35)(location Polignano))
  (hotel(name Trulli)(stars 2)(numRooms 15)(location Alberobello))
  (hotel(name VesuvioHotel)(stars 3)(numRooms 45)(location Napoli))
  (hotel(name Reggia)(stars 4)(numRooms 50)(location Caserta))
)

(deffacts distances
  (distance(from Castrovillari)(to Schiavonea)(distance 44))
  (distance(from Castrovillari)(to Camigliatello)(distance 97))
  (distance(from Castrovillari)(to Lauria)(distance 58))
  (distance(from Castrovillari)(to Matera)(distance 153))
  (distance(from Castrovillari)(to Bari)(distance 208))
  (distance(from Castrovillari)(to Alberobello)(distance 180))
  (distance(from Castrovillari)(to Polignano)(distance 200))
  (distance(from Castrovillari)(to Napoli)(distance 246))
  (distance(from Castrovillari)(to Caserta)(distance 264))
  (distance(from Schiavonea)(to Camigliatello)(distance 60))
  (distance(from Schiavonea)(to Bari)(distance 197))
  (distance(from Schiavonea)(to Alberobello)(distance 126))
  (distance(from Schiavonea)(to Polignano)(distance 186))
  (distance(from Schiavonea)(to Matera)(distance 142))
  (distance(from Schiavonea)(to Lauria)(distance 99))
  (distance(from Schiavonea)(to Napoli)(distance 290))
  (distance(from Schiavonea)(to Caserta)(distance 307))
  (distance(from Lauria)(to Camigliatello)(distance 159))
  (distance(from Lauria)(to Bari)(distance 211))
  (distance(from Lauria)(to Alberobello)(distance 194))
  (distance(from Lauria)(to Polignano)(distance 211))
  (distance(from Lauria)(to Matera)(distance 145))
  (distance(from Lauria)(to Napoli)(distance 198))
  (distance(from Lauria)(to Caserta)(distance 215))
  (distance(from Camigliatello)(to Bari)(distance 294))
  (distance(from Camigliatello)(to Alberobello)(distance 266))
  (distance(from Camigliatello)(to Polignano)(distance 283))
  (distance(from Camigliatello)(to Matera)(distance 236))
  (distance(from Camigliatello)(to Napoli)(distance 342))
  (distance(from Camigliatello)(to Caserta)(distance 361))
  (distance(from Bari)(to Alberobello)(distance 55))
  (distance(from Bari)(to Polignano)(distance 36))
  (distance(from Bari)(to Matera)(distance 65))
  (distance(from Bari)(to Napoli)(distance 266))
  (distance(from Bari)(to Caserta)(distance 260))
  (distance(from Alberobello)(to Polignano)(distance 29))
  (distance(from Alberobello)(to Matera)(distance 68))
  (distance(from Alberobello)(to Napoli)(distance 316))
  (distance(from Alberobello)(to Caserta)(distance 318))
  (distance(from Polignano)(to Matera)(distance 73))
  (distance(from Polignano)(to Napoli)(distance 298))
  (distance(from Polignano)(to Caserta)(distance 292))
  (distance(from Matera)(to Napoli)(distance 251))
  (distance(from Matera)(to Caserta)(distance 256))
  (distance(from Napoli)(to Caserta)(distance 33))

)

(deffacts scores
  (score(location Schiavonea) (tourismType Balneare) (score 3))
  (score(location Schiavonea) (tourismType Naturalistico) (score 4))
  (score(location Schiavonea) (tourismType Culturale) (score 1))
  (score(location Schiavonea) (tourismType Religioso) (score 2))
  (score(location Schiavonea) (tourismType Enogastronomico) (score 2))
  (score(location Camigliatello) (tourismType Montano) (score 5))
  (score(location Camigliatello) (tourismType Lacustre) (score 3))
  (score(location Camigliatello) (tourismType Naturalistico) (score 4))
  (score(location Camigliatello) (tourismType Enogastronomico) (score 3))
  (score(location Castrovillari) (tourismType Naturalistico) (score 5))
  (score(location Castrovillari) (tourismType Culturale) (score 2))
  (score(location Castrovillari) (tourismType Sportivo) (score 3))
  (score(location Castrovillari) (tourismType Enogastronomico) (score 4))
  (score(location Matera) (tourismType Naturalistico) (score 3))
  (score(location Matera) (tourismType Culturale) (score 4))
  (score(location Matera) (tourismType Enogastronomico) (score 2))
  (score(location Lauria) (tourismType Montano) (score 2))
  (score(location Lauria) (tourismType Lacustre) (score 4))
  (score(location Lauria) (tourismType Naturalistico) (score 3))
  (score(location Lauria) (tourismType Termale) (score 2))
  (score(location Bari) (tourismType Balneare) (score 5))
  (score(location Bari) (tourismType Naturalistico) (score 1))
  (score(location Bari) (tourismType Culturale) (score 2))
  (score(location Bari) (tourismType Sportivo) (score 3))
  (score(location Bari) (tourismType Enogastronomico) (score 4))
  (score(location Polignano) (tourismType Balneare) (score 5))
  (score(location Polignano) (tourismType Enogastronomico) (score 3))
  (score(location Alberobello) (tourismType Naturalistico) (score 3))
  (score(location Alberobello) (tourismType Culturale) (score 3))
  (score(location Alberobello) (tourismType Enogastronomico) (score 2))
  (score(location Caserta) (tourismType Naturalistico) (score 2))
  (score(location Caserta) (tourismType Culturale) (score 5))
  (score(location Caserta) (tourismType Enogastronomico) (score 1))
  (score(location Napoli) (tourismType Balneare) (score 2))
  (score(location Napoli) (tourismType Naturalistico) (score 1))
  (score(location Napoli) (tourismType Culturale) (score 4))
  (score(location Napoli) (tourismType Religioso) (score 2))
  (score(location Napoli) (tourismType Sportivo) (score 4))
  (score(location Napoli) (tourismType Enogastronomico) (score 4))
)

;############################
;##    EXPERIENCE RULES    ##
;############################

(deftemplate rule
  (slot if)
  (slot then)
  (slot not)
  (slot divisor (default 2))
)

(deffacts experience_rules
  (rule(if Balneare) (not Montano)(divisor 2))
  (rule(if Balneare) (then Naturalistico )(divisor 3))
  (rule(if Balneare) (then Enogastronomico)(divisor 3))
  (rule(if Balneare) (then Sportivo)(not Lacustre)(divisor 4))
  (rule(if Montano) (then Naturalistico)(divisor 2))
  (rule(if Montano) (then Sportivo)(divisor 3))
  (rule(if Montano) (then Lacustre)(divisor 3))
  (rule(if Montano) (then Religioso)(divisor 4))
  (rule(if Montano) (then Enogastronomico)(divisor 4))
  (rule(if Montano) (then Culturale)(divisor 5))
  (rule(if Lacustre) (then Termale) (divisor 2))
  (rule(if Lacustre) (then Naturalistico) (not Sportivo)(divisor 3))
  (rule(if Naturalistico) (not Religioso) (divisor 3))
  (rule(if Naturalistico) (not Culturale) (divisor 3))
  (rule(if Naturalistico) (then Sportivo) (divisor 4))
  (rule(if Naturalistico) (then Enogastronomico) (divisor 4))
  (rule(if Naturalistico) (then Termale) (divisor 5))
  (rule(if Termale)(not Sportivo)(divisor 2))
  (rule(if Termale)(not Culturale)(divisor 3))
  (rule(if Termale)(not Religioso)(divisor 3))
  (rule(if Termale)(not Enogastronomico)(divisor 3))
  (rule(if Culturale) (then Religioso)(not Sportivo)(divisor 2))
  (rule(if Religioso) (not Sportivo)(divisor 2))
  (rule(if Sportivo) (not Enogastronomico)(divisor 3))
)

(defrule simmetricalRules(declare(salience 1000))
  (rule(if ?type) (then ?type2)(divisor ?div))
=>
  (assert(rule(if ?type2)(then ?type)(divisor ?div)))
)

(defrule simmetricalRulesNot(declare(salience 1000))
  (rule(if ?type) (not ?type2)(divisor ?div))
=>
  (assert(rule(if ?type2)(not ?type)(divisor ?div)))
)

(defrule simmetricalDistance (declare(salience 1000))
  (distance(from ?a)(to ?b)(distance ?d))
=>
  (assert(distance(from ?b)(to ?a)(distance ?d)))
)

(defrule start (declare(salience 100))
  =>

  (assert (request))
  (focus QUESTIONS)
)

;;******************
;; The QUESTION module
;;******************

(defmodule QUESTIONS (import MAIN ?ALL)(export ?ALL))

(deftemplate question
   (slot attribute)
   (slot the-question)
   (slot already-asked (default FALSE))
   (slot type)
   (multislot valid-answers)
)

(deffacts question-attributes
  (question (attribute name)
            (the-question "Nome del prenotante?")
            (type STRING))
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
  (question (attribute nights)
            (the-question "Durata massima della vacanza? inserire numero dei giorni")
            (type INTEGER))
  (question (attribute date)
            (the-question "In che data vorresti partire? (dd mm yyyy)"))
  (question (attribute minLocations)
            (the-question "C'è un numero minimo di località da visitare? inserire numero ")
            (type INTEGER))
  (question (attribute maxLocations)
            (the-question "C'è un numero massimo di località da visitare? inserire numero ")
            (type INTEGER))
  (question (attribute notRegions)
            (the-question "Regioni che non vuoi visitare? inserire nome delle regioni ")
            (type STRING)
            (valid-answers "Calabria" "Puglia" "Basilicata" "Campania"))
  (question (attribute regions)
            (the-question "Preferenze sulle regioni da visitare? inserire nome delle regioni ")
            (type STRING)
            (valid-answers "Calabria" "Puglia" "Basilicata" "Campania"))
  (question (attribute notTourismTypes)
            (the-question "Ci sono tipi di località che preferiresti evitare?")
            (type STRING)
            (valid-answers "Balneare" "Montano" "Lacustre" "Naturalistico" "Termale" "Culturale" "Religioso" "Sportivo" "Enogastronomico"))
  (question (attribute tourismTypes)
            (the-question "Che tipo di località vuoi visitare? ")
            (type STRING)
            (valid-answers "Balneare" "Montano" "Lacustre" "Naturalistico" "Termale" "Culturale" "Religioso" "Sportivo" "Enogastronomico"))
)

(deftemplate attribute
  (slot name)
  (multislot value)
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
          (bind ?answ (str-cat ?answ)))

        (if(and (<> (length$ ?valid-answers) 0) (or(not(member$ ?answ ?valid-answers))(neq (type ?answ) ?type))) then
            (bind ?wrong TRUE)
            (break))
      )

      (if (eq ?wrong TRUE)
        then
          (printout t ?question crlf)
          (bind $?a (readline))
        else
          (return $?a))
  )
)

(defrule ask-a-question
   	?question <- (question (already-asked FALSE)
                   	(the-question ?the-question)
                   	(attribute ?the-attribute)
                    (type ?type)
                    (valid-answers $?valid-answers))
   	=>
   	(modify ?question (already-asked TRUE))
   	(assert (attribute (name ?the-attribute) (value (explode$ (ask-question ?the-question ?type ?valid-answers)))))
)

(defrule compile
    ?attr <- (attribute (name ?n) (value $?v))
    ?r <- (request)
    (test(<>(length$ ?v) 0))
    =>
    (retract ?attr)

    (switch ?n
      (case name then (modify ?r (name (implode$ ?v))))
      (case numPeople then (modify ?r (numPeople (nth$ 1 ?v))))
      (case regions then (modify ?r (regions ?v)))
      (case notRegions then (modify ?r (notRegions ?v)))
      (case tourismTypes then (modify ?r (tourismTypes ?v)))
      (case notTourismTypes then (modify ?r (notTourismTypes ?v)))
      (case stars then (modify ?r (stars (nth$ 1 ?v))))
      (case minLocations then (modify ?r (minLocations (nth$ 1 ?v))))
      (case maxLocations then (modify ?r (maxLocations (nth$ 1 ?v))))
      (case nights then (modify ?r (nights (nth$ 1 ?v))))
      (case date then (modify ?r (arrivalDate ?v)))
    )
)

;;******************
;; The CF_RULE module
;;******************

(defmodule CF_RULE (import MAIN ?ALL)(export ?ALL))

(deftemplate MAIN::oav
  (slot hotel)
  (slot type)
  (slot cf)
)

(defrule assertOAV
  (request(numPeople ?numPeople))
  (hotel(name ?hotel)(stars ?numStars)(numRooms ?numRooms&:(>= ?numRooms (div (+ ?numPeople 1) 2))))
=>
  (assert(oav(hotel ?hotel)(type region)(cf 0.5)))
  (assert(oav(hotel ?hotel)(type used)(cf 1)))
  (assert(oav(hotel ?hotel)(type stars)(cf (/ ?numStars 5))))
  (assert(oav(hotel ?hotel)(type price)(cf 0.5)))
)

(defrule CF_regions (declare (salience 100))
  ?oav <- (oav(hotel ?hotel)(type region))
  (request(regions $?regions)(notRegions $?notRegions))
  (hotel(name ?hotel)(location ?location))
  (location(name ?location)(region ?region))
=>
  (if(member$ ?region $?regions) then
    (modify ?oav(cf 0.75)))

  (if(member$ ?region $?notRegions) then
    (modify ?oav(cf 0.25)))
)

(defglobal ?*value* = 0.5)
(defrule CF_tourismType (declare (salience 100))
  (request(tourismTypes $?ttypes)(notTourismTypes $?notTypes))
  (hotel(name ?hotel)(location ?location))
=>
  (do-for-all-facts ((?s score)) (eq ?s:location ?location)
    (if(member$ ?s:tourismType $?ttypes) then
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes)))))
    (if(member$ ?s:tourismType $?notTypes) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?notTypes)))))
  )
  (do-for-all-facts ((?s score)(?r rule)) (eq ?s:location ?location)
    (if(and(member$ ?r:if $?ttypes)(eq ?r:then ?s:tourismType)) then
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes) ?r:divisor ))))
    (if(and(member$ ?r:if $?ttypes)(eq ?r:not ?s:tourismType)) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?ttypes) ?r:divisor))))
  )
  (assert(oav(hotel ?hotel)(type tourismType)(cf ?*value*)))
  (bind ?*value* 0.5)
)

(defrule CF_Used (declare (salience 100))
  (reservation(hotels $?hotels))
  (hotel(name ?hotel))
  ?oav <- (oav(hotel ?hotel)(type used)(cf ?cf))
  (test(member$ ?hotel $?hotels))
=>
  (modify ?oav(cf (* ?cf 0.9)))
)

(defrule combineCF
  ?oavR <- (oav(hotel ?hotel)(type region)(cf ?cfR))
  ?oavT <- (oav(hotel ?hotel)(type tourismType)(cf ?cfT))
  ?oavU <- (oav(hotel ?hotel)(type used)(cf ?cfU))

  =>
    (printout t ?cfR "  " ?cfT "  " ?cfU crlf)
    (retract ?oavR ?oavT)
    (bind ?cf (* ?cfU (/ (+ ?cfR ?cfT) 2)))
    (modify ?oavU(type all)(cf ?cf))
    (printout t ?hotel " ---> " ?cf crlf )
)
