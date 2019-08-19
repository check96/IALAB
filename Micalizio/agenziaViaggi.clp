; (assert(request(numPeople 15)(time 3))
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
  (slot name (default Ciccio))
  (slot numPeople (default 1))
  (multislot regions)
  (multislot tourismTypes)
  (slot stars)
  (slot numLocations)
  (slot time (default 1))
)

(deftemplate solution
  (slot name)
  (multislot hotels)
  (multislot locations)
  (slot price)
)

(deffacts locations
  (location(name Villapiana)(region Calabria)(tourismType Balneare))
  (location(name Schiavonea)(region Calabria)(tourismType Balneare Naturalistico))
  (location(name Camigliatello)(region Calabria)(tourismType Montano Naturalistico))
  (location(name Castrovillari)(region Calabria)(tourismType Culturale Enogastronomico Naturalistico))
  (location(name Roma)(region Lazio)(tourismType Religioso Sportivo))
  (location(name Montecatini)(region Toscana)(tourismType Termale))
)

(deffacts hotels
  (hotel(name VillaHotel1)(stars 2)(numRooms 10)(location Villapiana))
  (hotel(name VillaHotel2)(stars 1)(numRooms 10)(location Villapiana))
  (hotel(name CastroHotel)(stars 3)(numRooms 15)(location Castrovillari))
  (hotel(name Hotel)(stars 4)(numRooms 20)(location Schiavonea))
  (hotel(name RomaHotel)(stars 4)(numRooms 40)(location Roma))
  (hotel(name SilaHotel)(stars 3)(numRooms 30)(location Camigliatello))
  (hotel(name TermeHotel)(stars 4)(numRooms 25)(location Montecatini))
)

(defrule findSolution
  (request(name ?name)(numPeople ?numPeople)(time ?timeReq)(tourismTypes ?ttypeReq))
  (hotel (name ?nameHotel) (numRooms ?numRooms&:(>= ?numRooms ?numPeople)) (stars ?numStars) (location ?loc))
  (location (name ?nameLoc&:(eq ?loc ?nameLoc)) (tourismType $? ?ttype&:(eq $?ttype ?ttypeReq) $?))

  =>

  (assert(solution (name ?name)(hotels ?nameHotel) (locations ?loc) (price (* ?timeReq 25 ?numPeople (+ ?numStars 1)))))
)
