
(defmodule MAIN (export ?ALL))

(deftemplate location
  (slot name)
  (slot region)
  (multislot tourismTypes (allowed-values Balneare Montano Lacustre Naturalistico Termale Culturale Religioso Sportivo Enogastronomico))
)

(deftemplate hotel
  (slot name)
  (slot stars (allowed-values 1 2 3 4))
  (slot numRooms)
  (slot location)
)

(deftemplate request
  (slot name (default Person))
  (slot numPeople (default 1))
  (multislot regions)
  (multislot tourismTypes)
  (slot stars (default 1))
  (slot numLocations (default 1))
  (slot nights (default 1))
  (slot price)
)

(deftemplate solution
  (slot name)
  (slot hotel)
  (multislot locations)
  (slot price (default 0))
)

(deftemplate distance
  (slot from)
  (slot to)
  (slot distance)
)

(deftemplate answer
	(multislot request))


(deffacts locations
  (location(name Schiavonea)(region Calabria)(tourismTypes Balneare Naturalistico))
  (location(name Camigliatello)(region Calabria)(tourismTypes Montano Naturalistico Lacustre))
  (location(name Castrovillari)(region Calabria)(tourismTypes Culturale Enogastronomico Naturalistico))
  (location(name Bari)(region Puglia)(tourismTypes Enogastronomico))
  (location(name Polignano)(region Puglia)(tourismTypes Balneare Enogastronomico))
  (location(name Alberobello)(region Puglia)(tourismTypes Culturale Enogastronomico))
  (location(name Matera)(region Basilicata)(tourismTypes Culturale))
  (location(name Lauria)(region Basilicata)(tourismTypes Termale Lacustre))
  (location(name Napoli)(region Campania)(tourismTypes Culturale Enogastronomico Sportivo))
  (location(name Caserta)(region Campania)(tourismTypes Culturale))
)

(deffacts hotels
  (hotel(name CastroHotel)(stars 3)(numRooms 15)(location Castrovillari))
  (hotel(name SeaHotel)(stars 4)(numRooms 20)(location Schiavonea))
  (hotel(name SilaHotel)(stars 3)(numRooms 30)(location Camigliatello))
  (hotel(name TermeHotel)(stars 4)(numRooms 25)(location Lauria))
  (hotel(name MateHotel)(stars 3)(numRooms 25)(location Matera))
  (hotel(name BariHotel)(stars 3)(numRooms 30)(location Bari))
  (hotel(name PoliHotel)(stars 4)(numRooms 35)(location Polignano))
  (hotel(name Trulli)(stars 2)(numRooms 15)(location Alberobello))
  (hotel(name VesuvioHotel)(stars 3)(numRooms 45)(location Napoli))
  (hotel(name Reggia)(stars 4)(numRooms 50)(location Caserta))

)

(deffacts distances
  (distance(from Castrovillari)(to Schiavonea)(distance 44))
  (distance(from Castrovillari)(to Camigliatello)(distance 97))
  (distance(from Castrovillari)(to Bari)(distance 208))
  (distance(from Castrovillari)(to Alberobello)(distance 180))
  (distance(from Castrovillari)(to Polignano)(distance 200))
  (distance(from Castrovillari)(to Matera)(distance 153))
  (distance(from Castrovillari)(to Napoli)(distance 246))
  (distance(from Castrovillari)(to Caserta)(distance 264))
  (distance(from Schiavonea)(to Camigliatello)(distance 60))
  (distance(from Schiavonea)(to Bari)(distance 208))
  (distance(from Schiavonea)(to Alberobello)(distance 180))
  (distance(from Schiavonea)(to Polignano)(distance 200))
  (distance(from Schiavonea)(to Matera)(distance 153))
  (distance(from Schiavonea)(to Napoli)(distance 246))
  (distance(from Schiavonea)(to Caserta)(distance 264))
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

(defrule defineHotel (declare (salience 100))
  (request(name ?name)(numPeople ?numPeople)(stars ?stars))
  (hotel(name ?nameHotel) (numRooms ?numRooms&:(>= ?numRooms ?numPeople)) (stars ?numStars&:(>= ?numStars ?stars)) (location ?loc) )
  (location (name ?loc))
=>
  (assert(solution(name ?name) (hotel ?nameHotel) (locations ?loc)))
)

(defrule defineLocation (declare (salience 90))
  (request(name ?name)(regions $?regions)(numLocations ?num)(tourismTypes $?tourismTypes))
  (solution(name ?name) (hotel ?hotel) (locations $?locations))
  (hotel(name ?hotel)(location ?location))
  (location (name ?loc&:(neq ?loc ?location)) (region ?region)(tourismTypes $? ?ttype $?))
  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0)))
  (test(or(member$ ?ttype $?tourismTypes)(=(length$ $?tourismTypes) 0)))
  (distance(from ?loc)(to ?location)(distance ?d))
  (test(and(<= ?d 100) (< (length$ $?locations) ?num)))
  (test(not(member$ ?loc $?locations)))
  =>

  (assert(solution(name ?name)(hotel ?hotel) (locations $?locations ?loc)))

)

;si stampano i risultati
(defmodule PRINT-RESULT)
