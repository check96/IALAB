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
  (multislot notregions)
  (multislot tourismTypes)
  (multislot nottourismTypes)
  (slot stars (default 1)(type INTEGER))
  (slot minLocations (default 1) (type INTEGER))
  (slot maxLocations (default 5) (type INTEGER))
  (multislot arrivalDate)
  (multislot awayDate)
  (slot nights (default 1) (type INTEGER))
  (slot price (default 0) (type INTEGER))
)

(deftemplate option
  (slot name)
  (slot hotel)
  (slot nights (default 0))
  (multislot locations)
  (multislot arrivalDate)
  (multislot awayDate)
  (slot price (type FLOAT))
)

(deftemplate reservation
  (slot name)
  (slot hotel)
  (slot nights (default 1))
  (multislot locations)
  (multislot arrivalDate)
  (multislot awayDate)
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
  (slot score (type INTEGER) (default 0) (range 0 5))
)

(deftemplate oav
  (slot option)
  (slot attribute)
  (slot value)
  (slot certain (default 1))
)

(deftemplate region
  (slot name)
  (multislot visited)
)

(deftemplate user
  (slot name)
  (slot numPeople (type INTEGER))
  (multislot regions)
  (multislot notregions)
  (multislot tourismTypes)
  (multislot nottourismTypes)
  (slot stars (type INTEGER) (range 1 4))
  (slot minLocations (type INTEGER))
  (slot maxLocations (type INTEGER))
  (slot nights (type INTEGER) (default 1))
  (slot price (type INTEGER))
)

(deffacts regions
  (region(name Calabria))
  (region(name Puglia))
  (region(name Basilicata))
  (region(name Campania))
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
  (distance(from Castrovillari)(to Castrovillari)(distance 1))
  (distance(from Castrovillari)(to Schiavonea)(distance 44))
  (distance(from Castrovillari)(to Camigliatello)(distance 97))
  (distance(from Castrovillari)(to Lauria)(distance 58))
  (distance(from Castrovillari)(to Matera)(distance 153))
  (distance(from Castrovillari)(to Bari)(distance 208))
  (distance(from Castrovillari)(to Alberobello)(distance 180))
  (distance(from Castrovillari)(to Polignano)(distance 200))
  (distance(from Castrovillari)(to Napoli)(distance 246))
  (distance(from Castrovillari)(to Caserta)(distance 264))
  (distance(from Schiavonea)(to Schiavonea)(distance 1))
  (distance(from Schiavonea)(to Camigliatello)(distance 60))
  (distance(from Schiavonea)(to Bari)(distance 197))
  (distance(from Schiavonea)(to Alberobello)(distance 126))
  (distance(from Schiavonea)(to Polignano)(distance 186))
  (distance(from Schiavonea)(to Matera)(distance 142))
  (distance(from Schiavonea)(to Lauria)(distance 99))
  (distance(from Schiavonea)(to Napoli)(distance 290))
  (distance(from Schiavonea)(to Caserta)(distance 307))
  (distance(from Lauria)(to Lauria)(distance 1))
  (distance(from Lauria)(to Camigliatello)(distance 159))
  (distance(from Lauria)(to Bari)(distance 211))
  (distance(from Lauria)(to Alberobello)(distance 194))
  (distance(from Lauria)(to Polignano)(distance 211))
  (distance(from Lauria)(to Matera)(distance 145))
  (distance(from Lauria)(to Napoli)(distance 198))
  (distance(from Lauria)(to Caserta)(distance 215))
  (distance(from Camigliatello)(to Camigliatello)(distance 1))
  (distance(from Camigliatello)(to Bari)(distance 294))
  (distance(from Camigliatello)(to Alberobello)(distance 266))
  (distance(from Camigliatello)(to Polignano)(distance 283))
  (distance(from Camigliatello)(to Matera)(distance 236))
  (distance(from Camigliatello)(to Napoli)(distance 342))
  (distance(from Camigliatello)(to Caserta)(distance 361))
  (distance(from Bari)(to Bari)(distance 1))
  (distance(from Bari)(to Alberobello)(distance 55))
  (distance(from Bari)(to Polignano)(distance 36))
  (distance(from Bari)(to Matera)(distance 65))
  (distance(from Bari)(to Napoli)(distance 266))
  (distance(from Bari)(to Caserta)(distance 260))
  (distance(from Alberobello)(to Alberobello)(distance 1))
  (distance(from Alberobello)(to Polignano)(distance 29))
  (distance(from Alberobello)(to Matera)(distance 68))
  (distance(from Alberobello)(to Napoli)(distance 316))
  (distance(from Alberobello)(to Caserta)(distance 318))
  (distance(from Polignano)(to Polignano)(distance 1))
  (distance(from Polignano)(to Matera)(distance 73))
  (distance(from Polignano)(to Napoli)(distance 298))
  (distance(from Polignano)(to Caserta)(distance 292))
  (distance(from Matera)(to Matera)(distance 1))
  (distance(from Matera)(to Napoli)(distance 251))
  (distance(from Matera)(to Caserta)(distance 256))
  (distance(from Napoli)(to Napoli)(distance 1))
  (distance(from Napoli)(to Caserta)(distance 33))
  (distance(from Caserta)(to Caserta)(distance 1))
)

(deffacts scores
  (score(location Schiavonea) (tourismType Balneare) (score 3))
  (score(location Schiavonea) (tourismType Naturalistico) (score 4))
  (score(location Schiavonea) (tourismType Culturale) (score 1))
  (score(location Schiavonea) (tourismType Religioso) (score 1))
  (score(location Schiavonea) (tourismType Enogastronomico) (score 1))
  (score(location Camigliatello) (tourismType Montano) (score 5))
  (score(location Camigliatello) (tourismType Lacustre) (score 3))
  (score(location Camigliatello) (tourismType Naturalistico) (score 4))
  (score(location Camigliatello) (tourismType Enogastronomico) (score 3))
  (score(location Castrovillari) (tourismType Naturalistico) (score 5))
  (score(location Castrovillari) (tourismType Culturale) (score 3))
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
  (score(location Bari) (tourismType Sportivo) (score 2))
  (score(location Bari) (tourismType Enogastronomico) (score 4))
  (score(location Polignano) (tourismType Balneare) (score 5))
  (score(location Polignano) (tourismType Enogastronomico) (score 3))
  (score(location Alberobello) (tourismType Naturalistico) (score 3))
  (score(location Alberobello) (tourismType Culturale) (score 3))
  (score(location Alberobello) (tourismType Enogastronomico) (score 2))
  (score(location Caserta) (tourismType Naturalistico) (score 2))
  (score(location Caserta) (tourismType Culturale) (score 5))
  (score(location Caserta) (tourismType Enogastronomico) (score 1))
  (score(location Napoli) (tourismType Naturalistico) (score 1))
  (score(location Napoli) (tourismType Culturale) (score 4))
  (score(location Napoli) (tourismType Religioso) (score 2))
  (score(location Napoli) (tourismType Sportivo) (score 4))
  (score(location Napoli) (tourismType Enogastronomico) (score 4))
)

(deftemplate bestOptions
  (multislot options)
)

(defrule simmetricalDistance (declare(salience 1000))
  (distance(from ?a)(to ?b)(distance ?d))
=>
  (assert(distance(from ?b)(to ?a)(distance ?d)))
)

(deffunction ask-question (?question)

 	(printout t ?question crlf)
   	(bind $?answer (readline))
   		$?answer)

(defrule start (declare(salience 100))
  =>
  (assert (request) (print-sorted) (bestOptions))
  (focus QUESTIONS REQUEST CHOOSE PRINT)
)

(defmodule QUESTIONS (import MAIN ?ALL) (export ?ALL))

(deftemplate attribute
  (slot name)
  (multislot value)
)

(deftemplate question
   (slot attribute (default ?NONE))
   (slot the-question (default ?NONE))
   (slot already-asked (default FALSE))
)

(deffacts question-attributes
  (question (attribute name)
            (the-question "Nome del prenotante?"))
  (question (attribute minLocations)
            (the-question "E numero minimo? inserire numero "))
  (question (attribute maxLocations)
            (the-question "C'è un numero massimo di località da visitare? inserire numero "))
  (question (attribute stars)
            (the-question "Qualità dell'albergo? inserire numero minimo di stelle"))
  (question (attribute price)
            (the-question "Budget?"))
  (question (attribute notregions)
            (the-question "Mentre regioni che non vuoi visitare? inserire nome delle regioni "))
  (question (attribute regions)
            (the-question "Preferenze sulle regioni da visitare? inserire nome delle regioni "))
  (question (attribute nottourismTypes)
            (the-question "Ci sono invece tipi di località che preferiresti evitare?"))
  (question (attribute tourismTypes)
            (the-question "Che tipo di località vuoi visitare? "))
  (question (attribute date)
            (the-question "In che data vorresti partire? (d m y)"))
  (question (attribute nights)
            (the-question "Durata della vacanza? inserire numero dei giorni"))
  (question (attribute numPeople)
            (the-question "Quante persone siete? inserire numero "))
)


(defrule resetOptions (declare(salience 100))
?opt <- (option)
(reset)
=>
(retract ?opt)
)

(defrule resetOav (declare(salience 100))
  ?oav <- (oav)
  (reset)
=>
  (retract ?oav)
)

(defrule resetVisited (declare(salience 100))
  ?reg <- (region)
  (reset)
=>
  (modify ?reg (visited (create$ )))
)

(defrule deleteReset (declare(salience 95))
  ?reset <- (reset)
  (not(option))
  (not(oav))

  =>
  (retract ?reset)
)

(defrule ask-a-question	(declare (auto-focus TRUE))
   	?f <- (question (already-asked FALSE)
                   	(the-question ?the-question)
                   	(attribute ?the-attribute))

   	=>
   	(modify ?f (already-asked TRUE))
   	(assert (attribute (name ?the-attribute)
                      	(value (explode$ (ask-question ?the-question)))))
)

  ;prende la richiesta dell'utente e ne scompone le parti e riempie i valori di request
(defmodule REQUEST (import QUESTIONS ?ALL)(export ?ALL))

(defrule initRequest (declare(salience 10))
  ?user <- (user (name ?name)(numPeople ?numPeople)(regions $?regions)(notregions $?banned)(tourismTypes $?tourismTypes)(stars ?numStars)(price ?price)(maxLocations ?max)
                  (minLocations ?min)(nights ?nights))
  ?r <- (request(name ?name))
  (attribute (name ?n) (value ?v&:(or(eq ?v no)(eq ?v \n))))

  =>
  (switch ?n
    (case numPeople then (modify ?r (numPeople ?numPeople)))
    (case regions then (modify ?r (regions ?regions)))
    (case notregions then (modify ?r (notregions $?banned)))
    (case tourismTypes then (modify ?r (tourismTypes $?tourismTypes)))
    (case stars then (modify ?r (stars ?numStars)))
    (case minLocations then (modify ?r (minLocations ?min)))
    (case maxLocations then (modify ?r (maxLocations ?max)))
    (case nights then (modify ?r (nights ?nights)))
    (case price then (modify ?r (price ?price)))
  )
)

(deffunction convertDate ($?date)
  (bind ?daysForMonth (create$ 31 28 31 30 31 30 31 31 30 31 30 31))
  (bind ?days 0)
  (loop-for-count (?i 1 (- (nth$ 2 $?date) 1)) do
    (bind ?days (+ ?days (nth$ ?i ?daysForMonth)))

  (bind ?days (*(+ ?days (nth$ 1 $?date)) (-(nth$ 3 $?date) 2019 -1))))
  ?days)

(deffunction calculateAwayDate (?nights $?date)
  (bind ?awayDate (create$ 01 01 2019))
  (bind ?day (integer (nth$ 1 $?date)))
  (bind ?month (integer (nth$ 2 $?date)))
  (bind ?year (integer (nth$ 3 $?date)))

  (if(>(+ ?day ?nights) 28)
    then
      (if(or(= ?month 04)(= ?month 06)(= ?month 09) (= ?month 11))
        then
          (bind ?awayDate 1 3 (-(+ ?day ?nights) 30) (+ ?month 1) ?year)
        else
          (bind ?awayDate (-(+ ?day ?nights) 31) (+ ?month 1) ?year)
      )
      (if(= ?month 02)
        then
          (bind ?awayDate (-(+ ?day ?nights) 28) 03 ?year)
      )
      (if(= ?month 12)
        then
          (bind ?awayDate (-(+ ?day ?nights) 31) 01 (+ ?year 1))
      )
    else
      (bind ?awayDate (+ ?day ?nights) ?month ?year)
  )
  ?awayDate)

(defrule compile (declare(auto-focus TRUE))
  ?attr <- (attribute(name ?n)(value $?v))
  ?r <- (request(nights ?nights))
  =>
  (retract ?attr)
  (switch ?n
    (case name then (modify ?r (name (implode$ ?v))))
    (case numPeople then (if (eq (type (nth$ 1 ?v)) INTEGER) then (modify ?r (numPeople (nth$ 1 ?v)))))
    (case regions then (modify ?r (regions ?v)))
    (case notregions then (modify ?r (notregions ?v)))
    (case tourismTypes then (modify ?r (tourismTypes ?v)))
    (case nottourismTypes then (modify ?r (nottourismTypes ?v)))
    (case stars then (if (eq (type (nth$ 1 ?v)) INTEGER ) then (modify ?r (stars (nth$ 1 ?v)))))
    (case minLocations then (if (eq (type (nth$ 1 ?v)) INTEGER ) then (modify ?r (minLocations (nth$ 1 ?v)))))
    (case maxLocations then (if (eq (type (nth$ 1 ?v)) INTEGER ) then (modify ?r (maxLocations (nth$ 1 ?v)))))
    (case nights then (if (eq (type (nth$ 1 ?v)) INTEGER) then (modify ?r (nights (nth$ 1 ?v)))))
    (case price then (if (eq (type (nth$ 1 ?v)) INTEGER) then (modify ?r (price (nth$ 1 ?v)))))
    (case date then
          (if (=(length$ ?v) 0) then
              (bind ?day (+ (mod (random) 28) 1))
              (bind ?month (+ (mod (random) 12) 1))
              (modify ?r (arrivalDate (create$ ?day ?month 2020)) (awayDate (calculateAwayDate ?nights (create$ ?day ?month 2020))))
            else
             (modify ?r (arrivalDate ?v) (awayDate (calculateAwayDate ?nights ?v)))))
  )
)

  ;dalle richieste si scelgono delle possiblili opzioni
(defmodule CHOOSE (import MAIN ?ALL )(export ?ALL))

(deffunction verifyDate(?dateArr ?dateAw ?date)
  (bind ?arrive convertDate ?dateArr)
  (bind ?away convertDate ?dateAw)
  (bind ?d convertDate ?date)

  (return(and(> ?date ?arrive)(< ?date ?away)))
)

(deffunction verifyRooms(?date ?away ?numPeople ?name ?numRooms)
    (bind ?sum 0)
    (do-for-all-facts((?book reservation))
      (and(eq ?book:hotel ?name) (not(verifyDate ?book:arrivalDate ?book:awayDate ?date)) (not(verifyDate ?book:arrivalDate ?book:awayDate ?away)))
      (bind ?sum (+ ?sum ?book:numPeople))
    )
    (return (>= (- ?numRooms ?sum) (div (+ ?numPeople 1) 2)))
)

(defrule defineHotel (declare (salience 100))
  (request(name ?name)(numPeople ?numPeople)(regions $?regions)(notregions $?banned)(stars ?stars)(arrivalDate $?date)(awayDate $?away)(price ?priceReq))
  ?hotel <- (hotel(name ?nameHotel) (numRooms ?numRooms) (stars ?numStars&:(>= ?numStars ?stars)) (location ?loc))
  (location (name ?loc) (region ?region))

  (test(verifyRooms ?date ?away ?numPeople ?nameHotel ?numRooms))
  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0)))
  (test(or(not(member$ ?region $?banned)) (=(length$ $?banned) 0)))
  (test(or (= ?priceReq 0) (<= (* 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)) ?priceReq)))

=>
  (assert(option(name ?name) (hotel ?nameHotel) (locations ?loc)(arrivalDate $?date) (awayDate $?away) (nights 1)(price (* 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)))))
)

(defrule defineLocations (declare (salience 90))
  (option(name ?name) (hotel ?hotel)(nights ?nights) (locations $?locations))
  (request(name ?name)(numPeople ?numPeople)(regions $?regions)(notregions $?banned)(nights ?nightsReq)(arrivalDate $?date) (awayDate $?away) (maxLocations ?num)
          (tourismTypes $?tourismTypes)(price ?priceReq))
  (hotel(name ?hotel)(location ?location)(stars ?numStars))
  (location (name ?loc&:(neq ?loc ?location)) (region ?region)(tourismTypes $? ?ttype $?))
  (distance(from ?loc)(to ?location)(distance ?d))

  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0) (<= ?d 100)))
  (test(or(not(member$ ?region $?banned)) (=(length$ $?banned) 0)))
  (test(or(member$ ?ttype $?tourismTypes)(=(length$ $?tourismTypes) 0)))
  (test(< (length$ $?locations) ?num))
  (test(not(member$ ?loc $?locations)))
  =>
  (bind ?n (+ ?nights (+ (mod (random) 2) 1)))

  (if(or (= ?priceReq 0) (<= (* ?n 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)) ?priceReq))
    then
      (assert(option(name ?name)(hotel ?hotel)(nights ?n) (locations $?locations ?loc)(arrivalDate $?date) (awayDate $?away)
                    (price (* ?n 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)))))
  )
)

(defrule deleteDuplicates (declare (salience 80))
  ?opt <- (option(hotel ?hotel)(locations $?loc1))
  (option(hotel ?hotel)(locations $?loc2& ~$?loc1))
  (test(=(length$ $?loc1) (length$ $?loc2)))
  (test(subsetp $?loc1 $?loc2))

  =>
  (retract ?opt)
)

(defrule deleteForNum (declare(salience 70))
  (request(name ?name)(minLocations ?min)(maxLocations ?max))
  ?opt <- (option(name ?name) (locations $?locations&:(or(< (length$ $?locations) ?min) (> (length$ $?locations) ?max) )))

  =>
  (retract ?opt)
)

(defrule findVisitedRegions (declare (salience 65))
  ?opt <- (option(name ?name) (locations $? ?location $?))
  (location(name ?location)(region ?region))
  ?reg <- (region (name ?region) (visited $?visited))
  (test(not(member$ ?opt $?visited)))
  =>
  (modify ?reg (visited $?visited ?opt))
)

(defrule deleteIncompleteByRegion (declare (salience 60))
  (request(name ?name) (regions $? ?region $?))
  ?opt <- (option(name ?name))
  (region (name ?region) (visited $?visited))
  (test(not(member$ ?opt $?visited)))
  =>
  (retract ?opt)
)

(defrule CF_Price (declare(salience 50))
  (request(name ?name)(numPeople ?numPeople)(price ?priceReq))
  ?opt <- (option (name ?name)(price ?price)(nights ?nights))

=>
  (if(> ?priceReq 0)
    then
      (bind ?c (mod(*(- ?price ?priceReq -1) 0.01) 1))
      (assert(oav(option ?opt) (attribute price) (value ?price)(certain (abs ?c))))
    else
      (bind ?c (/ 100 (/ ?price (* ?nights (div (+ ?numPeople 1) 2)))))
      (assert(oav(option ?opt) (attribute price) (value ?price) (certain (abs ?c))))
  )
)

(defrule CF_Used (declare(salience 40))
  ?opt <- (option(name ?name)(hotel ?hotel))
  (reservation(name ~?name) (hotel ?hotel))
=>
  (assert(oav(option ?opt) (attribute yetUsed) (value yes)(certain 0.4)))
)

(defglobal ?*value* = 0)
(defrule CF_score (declare(salience 30))

  (request(name ?name)(tourismTypes $?tourismTypes))
  ?opt <- (option (name ?name) (locations $?locations))

=>
    (do-for-all-facts
      ((?s score)) (and(member$ ?s:location $?locations) (or(member$ ?s:tourismType $?tourismTypes)(= (length$ $?tourismTypes) 0)))
      (bind ?*value* (+ ?*value* ?s:score))
    )
    (assert(oav(option ?opt) (attribute score)(value ?*value*)(certain (- 1(/ ?*value* 100)))))
    (bind ?*value* 0)
)

(defrule CF_distances (declare(salience 20))
  ?opt <- (option(name ?name)(hotel ?hotel)(locations $? ?loc $?))
  (hotel(name ?hotel)(location ?loc1))
  (distance(from ?loc)(to ?loc1) (distance ?d))
  =>
  (assert(oav(option ?opt) (attribute distances) (value ?loc) (certain (- 100 ?d))))
)

(defrule combineCF_distances (declare(salience 15))
  ?oav1 <- (oav(option ?opt)(attribute distances) (value ?v) (certain ?c))
  ?oav2 <- (oav(option ?opt)(attribute distances) (value ?v1& ~?v)(certain ?c1))
  =>
  (retract ?oav1)
  (modify ?oav2 (value (str-cat ?v ?v1))(certain (+ ?c ?c1)))
)

(defrule deleteForDistance (declare(salience 13))
  ?opt <- (option(nights ?nights))
  ?oavD <- (oav(option ?opt)(attribute distances)(certain ?c&:(< ?c -150)))
  ?oavP <- (oav(option ?opt)(attribute price))
  ?oavS <- (oav(option ?opt)(attribute score))
  =>
  (retract ?opt ?oavS ?oavP ?oavD)
)

(defrule CF_regions (declare(salience 11))
	(request(name ?name) (regions $?reg))
  	?opt <- (option (name ?name) (locations $?locations))
  	?l <- (location (region ?r))
  	(test (member$ ?l $?locations))
  	=>
	(if (not(member$ ?r $?reg))
		then
			(assert(oav(option ?opt) (attribute region) (value ?r) (certain 0.5)))
		else
			(assert(oav(option ?opt) (attribute region) (value ?r) (certain 1)))
	)
)


(defrule combineCF (declare(salience 10))
  ?oavP <- (oav(option ?opt) (attribute price) (certain ?CP))
  ?oavD <- (oav(option ?opt) (attribute distances) (certain ?CD))
  ?oavS <- (oav(option ?opt) (attribute score) (certain ?CS))
  ?oavR <- (oav(option ?opt) (attribute region) (certain ?CR))
  =>
  (retract ?oavP ?oavD)
  (bind ?CT (* ?CP ?CD ?CS ?CR))
  (printout t "option " ?opt "  score " ?CS "  price " ?CP "  distances " ?CD "  totale " ?CT crlf)
  (modify ?oavS (attribute all)(certain (abs ?CT)))
)

;si stampano i risultati
(defmodule PRINT (import MAIN ?ALL) (import QUESTIONS ?ALL))

(defrule assert-unprinted "Asserts each item that needs to be printed." (declare(salience 20))
  (print-sorted)
  ?opt <- (option)
  =>
  (assert (unprinted ?opt))
)

(defglobal ?*count* = 1)

(defrule retract-print-sorted "Retract print-sorted after all items enumerated."
  (declare (salience 5))
  ?f <- (print-sorted)
  =>
  (retract ?f)
  (bind ?*count* 1)
  (assert(reset))
)

(defrule print-sorted (declare(salience 10))
  (print-sorted)
  ?opt <- (option (hotel ?hotel) (nights ?nights) (locations $?locations) (price ?price))
  ?u <- (unprinted ?opt)
  ?best <- (bestOptions (options $?opts))
  ?oav <- (oav(option ?opt) (attribute all) (certain ?certain))

  (forall (unprinted ?opt1) (oav (option ?opt1) (attribute all)(certain ?c))
      (test(>= ?certain ?c)))
  =>

  (retract ?u)
  (if(<= ?*count* 5)
    then
      (modify ?best (options $?opts ?opt))
      (printout t ?*count* ":=>  hotel " ?hotel " nights " ?nights " locations " $?locations "  price " ?price  "  cf " ?certain crlf)
      (retract ?oav)
    else
      (retract ?oav ?opt)
  )
  (bind ?*count* (+ ?*count* 1))
)

(deftemplate selectedId
	(slot index (type INTEGER))
	(slot attribute))

(defrule chooseOption (declare(salience -5))
	(not(print-sorted))
	(bestOptions (options $?options))

	(not(selected))
	=>

	(printout t "Quale soluzione scegli? inserisci il numero dell'indice, altrimenti 0 per modificare la richiesta" crlf)
	(bind ?in (read))

	(if (and(> ?in 0)(<= ?in 5))
		then
	    (assert(selected(fact-index(nth$ ?in $?options)))))

	(if (eq ?in 0)
		then
			(printout t "cosa vuoi cambiare? inserisci il numero corrispondente"crlf
			 "1 nome" crlf
			 "2 numero di persone" crlf
			 "3 regioni preferite" crlf
			 "4 regioni da evitare" crlf
			 "5 tipi di turismo preferiti" crlf
			 "6 tipi di turismo da evitare" crlf
			 "7 numero di stelle dell'albergo" crlf
			 "8 numero minimo di posti da visitare" crlf
			 "9 numero massimo di posti da visitare" crlf
			 "10 numero di notti" crlf
			 "11 prezzo" crlf)

			(bind ?input (read))

			(switch ?input
				(case 1 then (assert (selectedId (index ?input) (attribute name))))
				(case 2 then (assert (selectedId (index ?input) (attribute numPeople))))
				(case 3 then (assert (selectedId (index ?input) (attribute regions))))
				(case 4 then (assert (selectedId (index ?input) (attribute notregions))))
				(case 5 then (assert (selectedId (index ?input) (attribute tourismTypesame))))
				(case 6 then (assert (selectedId (index ?input) (attribute nottourismTypes))))
				(case 7 then (assert (selectedId (index ?input) (attribute stars))))
				(case 8 then (assert (selectedId (index ?input) (attribute minLocations))))
				(case 9 then (assert (selectedId (index ?input) (attribute maxLocations))))
				(case 10 then (assert (selectedId (index ?input) (attribute nights))))
				(case 11 then (assert (selectedId (index ?input) (attribute price))))
			)
	)
)

(defrule checkSelected

	?sel <- (selectedId (attribute ?at))
	?quest <- (question (attribute ?at))
  ?best <- (bestOptions)
	=>
  (modify ?best (options (create$ )))
	(modify ?quest (already-asked FALSE))
	(retract ?sel)
  (assert(print-sorted) (reset))
  (focus QUESTIONS REQUEST CHOOSE PRINT)
)

(defrule createReservation (declare(salience -10))
  ?sel <- (selected ?num)
  ?req <- (request(name ?name) (numPeople ?numPeople)(regions $?regions)(notregions $?banned)(tourismTypes $?tourismTypes))
  ?opt <- (option(name ?name)(hotel ?hotel) (nights ?nights) (locations $?locations) (price ?price) (arrivalDate $?date) (awayDate $?away))
  (test(=(fact-index ?opt) ?num))

  =>
  (retract ?sel)
  (assert(user(name ?name)(numPeople ?numPeople) (nights ?nights) (regions $?regions)(notregions $?banned)(tourismTypes $?tourismTypes)(price ?price))
         (reservation(name ?name)(hotel ?hotel) (nights ?nights) (locations $?locations) (price ?price) (arrivalDate $?date) (awayDate $?away)))
  (printout t "Prenotazione effettuata!" crlf)
)


;scelta della soluzione
;modificare richiesta
;cf su regionalità
