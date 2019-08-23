
(defmodule MAIN (export ?ALL))

(deftemplate location
  (slot name)
  (slot region)
  (multislot tourismType (allowed-values Balneare Montano Lacustre Naturalistico Termale Culturale Religioso Sportivo Enogastronomico))
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
  (slot price (default 0))
)

(deftemplate subRequest
  (slot name (default Person))
  (slot numPeople (default 1))
  (slot region)
  (slot tourismType)
  (slot stars (default 1))
  (slot nights (default 1))
  (slot price (default 0))
)

(deftemplate option
  (slot name)
  (slot hotel)
  (slot nights (default 1))
  (slot location)
  (slot price (default 0))
)

(deftemplate solution
  (slot name)
  (multislot locations)
  (slot price (default 0))
)

(deftemplate distance
  (slot from)
  (slot to)
  (slot distance)
)

(deffacts locations
  (location(name Schiavonea)(region Calabria)(tourismType Balneare Naturalistico))
  (location(name Camigliatello)(region Calabria)(tourismType Montano Naturalistico Lacustre))
  (location(name Castrovillari)(region Calabria)(tourismType Culturale Enogastronomico Naturalistico))
  (location(name Bari)(region Puglia)(tourismType Enogastronomico))
  (location(name Polignano)(region Puglia)(tourismType Balneare Enogastronomico))
  (location(name Alberobello)(region Puglia)(tourismType Culturale Enogastronomico))
  (location(name Matera)(region Basilicata)(tourismType Culturale))
  (location(name Lauria)(region Basilicata)(tourismType Termale Lacustre))
  (location(name Napoli)(region Campania)(tourismType Culturale Enogastronomico Sportivo))
  (location(name Caserta)(region Campania)(tourismType Culturale))
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

(defrule simmetricalDistance (salience 10)
  (distance(from ?a)(to ?b)(distance ?d))
=>
  (assert(distance(from ?b)(to ?a)(distance ?d)))
)

(defrule start
  (request(name ?name))
=>
  (assert(solution(name ?name)))
  (focus CHOOSE)
)

(defmodule CHOOSE (import MAIN ?ALL)(export ?ALL))

(defrule divide
  (request(name ?name)(regions $? ?region $?)(tourismTypes $? ?tourismType $?)(nights ?nights))

  =>

  (assert(subRequest(name ?name)(region ?region)(tourismType ?tourismType)))
)

(defrule findOptions
  ?subR <- (subRequest(name ?name)(numPeople ?numPeople)(nights ?nightsReq) (region ?region) (stars ?numStars) (price ?priceReq)(tourismType ?ttypeReq))
  (hotel (name ?nameHotel)(numRooms ?numRooms&:(>= ?numRooms ?numPeople)) (stars ?starHotel&:(>= ?starHotel ?numStars)) (location ?loc))
  (location (name ?nameLoc&:(eq ?nameLoc ?loc)) (region ?reg&:(eq ?reg ?region)) (tourismType $? ?ttype&:(eq $?ttype ?ttypeReq) $?))
  (test(or (<= (* ?nightsReq 25 ?numPeople (+ ?starHotel 1)) ?priceReq)(= ?priceReq 0)))

  =>

  (assert(option(name ?name)(hotel ?nameHotel) (location ?loc) (price (* ?nightsReq 25 ?numPeople (+ ?starHotel 1)))))
  (retract ?subR)
)

;(assert(request(regions Calabria Puglia)(tourismTypes Montano Culturale)))
(defrule findSolution
  ?opt <- (option(name ?name)(location ?loc)(price ?price))
  ?sol <- (solution(name ?nameSol&:(eq ?name ?nameSol))(locations $?locations)(price ?priceSol))
  (test (not (member$ ?loc $?locations)))
  =>
  (modify ?sol(locations $?locations ?loc)(price (+ ?price ?priceSol)))
  (retract ?opt)
)

(defrule checkPrice
  (request(name ?nameReq)(price ?priceReq))
  (solution(name ?name&:(eq ?name ?nameReq))(price ?price&:(> ?price ?priceReq)))

  =>


)
