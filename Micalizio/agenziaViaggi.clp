
(defmodule MAIN (export ?ALL))

(deftemplate location
  (slot name)
  (slot region)
  (multislot tourismTypes (allowed-values Balneare Montano Lacustre Naturalistico Termale Culturale Religioso Sportivo Enogastronomico))
)

(deftemplate hotel
  (slot name)
  (slot stars (type INTEGER) (allowed-values 1 2 3 4))
  (slot numRooms (type INTEGER))
  (slot location)
)

(deftemplate request
  (slot name (default Person))
  (slot numPeople (default 1)(type INTEGER))
  (multislot regions)
  (multislot tourismTypes)
  (slot stars (default 1)(type INTEGER))
  (slot minLocations (default 1) (type INTEGER))
  (slot maxLocations (default 5) (type INTEGER))
  (slot nights (default 1) (type INTEGER))
  (slot price (type FLOAT))
)

(deftemplate solution
  (slot name)
  (slot hotel)
  (multislot locations)
  (slot price (type FLOAT))
)

(deftemplate distance
  (slot from)
  (slot to)
  (slot distance)
)

(deftemplate score
  (slot location)
  (slot tourismType)
  (slot score (type INTEGER) (default 0))
)

(deftemplate answer
	(multislot request))


(deffacts locations
  (location(name Schiavonea)(region Calabria)(tourismTypes Balneare Naturalistico))
  (location(name Camigliatello)(region Calabria)(tourismTypes Montano Naturalistico Lacustre))
  (location(name Castrovillari)(region Calabria)(tourismTypes Culturale Enogastronomico Naturalistico))
  (location(name Bari)(region Puglia)(tourismTypes Balneare Enogastronomico))
  (location(name Polignano)(region Puglia)(tourismTypes Balneare Enogastronomico))
  (location(name Alberobello)(region Puglia)(tourismTypes Culturale Enogastronomico))
  (location(name Matera)(region Basilicata)(tourismTypes Culturale))
  (location(name Lauria)(region Basilicata)(tourismTypes Termale Lacustre))
  (location(name Napoli)(region Campania)(tourismTypes Culturale Enogastronomico Sportivo))
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
  (score(location Castrovillari) (tourismType Naturalistico) (score 4))
  (score(location Castrovillari) (tourismType Culturale) (score 2))
  (score(location Lauria) (tourismType Lacustre) (score 1))
  (score(location Lauria) (tourismType Termale) (score 3))
  (score(location Camigliatello) (tourismType Lacustre) (score 3))
  (score(location Schiavonea) (tourismType Balneare) (score 4))
  (score(location Schiavonea) (tourismType Naturalistico) (score 2))
  (score(location Polignano) (tourismType Balneare) (score 5))
  (score(location Polignano) (tourismType Enogastronomico) (score 3))
  (score(location Matera) (tourismType Culturale) (score 5))
  (score(location Alberobello) (tourismType Culturale) (score 4))
  (score(location Alberobello) (tourismType Enogastronomico) (score 2))
  (score(location Bari) (tourismType Balneare) (score 3))
  (score(location Bari) (tourismType Enogastronomico) (score 5))
  (score(location Napoli) (tourismType Enogastronomico) (score 5))
  (score(location Caserta) (tourismType Culturale) (score 3))
)

(defrule simmetricalDistance (declare(salience 1000))
  (distance(from ?a)(to ?b)(distance ?d))
=>
  (assert(distance(from ?b)(to ?a)(distance ?d)))
)

(defrule start (declare(salience 100))
=>
  (focus CHOOSE)
)

  ;prende la richiesta dell'utente e ne scompone le parti
(defmodule REQUEST)

;deve avere priorità più alta
;   (defrule read (declare(salience 1000))
;=>
;   (printout t "inserisci richiesta")
;   (bind ?line (readline))
;   (assert (answer (request ?line)))
;)


    ; regola o funzione che prese le parti dell'input va a riempire gli slot della richiesta
;(defrule::REQUEST fillRequest)

  ;dalle richieste si scelgono delle possiblili soluzioni
(defmodule CHOOSE (import MAIN ?ALL)(export ?ALL))

(deffunction division (?val)

  (if (>(mod ?val 2) 0) then
     (+(div ?val 2)1)
  else (div ?val 2))
)

(defrule defineHotel (declare (salience 100))
  (request(name ?name)(numPeople ?numPeople)(nights ?nights)(regions $?regions)(stars ?stars)(price ?price))
  (hotel(name ?nameHotel) (numRooms ?numRooms&:(>= ?numRooms (division ?numPeople))) (stars ?numStars&:(>= ?numStars ?stars)) (location ?loc) )
  (location (name ?loc) (region ?region))
  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0)))
  (test(or (= ?price 0) (<= (* ?nights 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)) ?price)))

=>
  (assert(solution(name ?name) (hotel ?nameHotel) (locations ?loc) (price (* ?nights 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)))))
)

(defrule defineLocation (declare (salience 90))
  (request(name ?name)(regions $?regions)(maxLocations ?num)(tourismTypes $?tourismTypes))
  (solution(name ?name) (hotel ?hotel) (locations $?locations)(price ?price))
  (hotel(name ?hotel)(location ?location))
  (location (name ?loc&:(neq ?loc ?location)) (region ?region)(tourismTypes $? ?ttype $?))
  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0)))
  (test(or(member$ ?ttype $?tourismTypes)(=(length$ $?tourismTypes) 0)))
  (distance(from ?loc)(to ?location)(distance ?d))
  (test(and(<= ?d 100) (< (length$ $?locations) ?num)))
  (test(not(member$ ?loc $?locations)))
  =>

  (assert(solution(name ?name)(hotel ?hotel) (locations $?locations ?loc) (price ?price)))

)

(defrule deleteDuplicates (declare (salience 80))
  ?sol <- (solution(hotel ?hotel)(locations $?loc1))
  (solution(hotel ?hotel)(locations $?loc2&~$?loc1))
  (test(=(length$ $?loc1) (length$ $?loc2)))
  (test(subsetp $?loc1 $?loc2))

  =>

  (retract ?sol)

)

(defrule deleteForNum (declare(salience 60))
  (request(name ?name)(minLocations ?min)(maxLocations ?max))
  ?sol <- (solution(name ?name) (locations $?locations&:(or(< (length$ $?locations) ?min) (> (length$ $?locations) ?max) )))

  =>
  (retract ?sol)
)

;si stampano i risultati
(defmodule PRINT-RESULT)
