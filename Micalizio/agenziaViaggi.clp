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
  (slot numPeople (default 2)(type INTEGER))
  (multislot regions (default Calabria Puglia))
  (multislot notregions)
  (multislot tourismTypes (default Balneare))
  (multislot nottourismTypes)
  (slot stars (default 3)(type INTEGER))
  (slot minLocations (default 1) (type INTEGER))
  (slot maxLocations (default 5) (type INTEGER))
  (slot nights (default 1) (type INTEGER))
  (slot price (default 600) (type INTEGER))
)

(deftemplate option
  (slot name)
  (slot hotel)
  (slot nights (default 0))
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
  (slot score (type INTEGER) (default 1) (range 1 5))
)

(deftemplate oav
  (slot option)
  (slot attribute)
  (slot value)
  (slot certain (default 1))
)

(deftemplate solution
  (slot name)
  (slot hotel)
  (slot nights (default 1))
  (multislot locations)
  (slot price (type FLOAT))
)

(deftemplate region
  (slot name)
  (multislot visited)
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
  (score(location Schiavonea) (tourismType Montano) (score 1))
  (score(location Schiavonea) (tourismType Lacustre) (score 1))
  (score(location Schiavonea) (tourismType Naturalistico) (score 4))
  (score(location Schiavonea) (tourismType Termale) (score 1))
  (score(location Schiavonea) (tourismType Culturale) (score 1))
  (score(location Schiavonea) (tourismType Religioso) (score 1))
  (score(location Schiavonea) (tourismType Sportivo) (score 1))
  (score(location Schiavonea) (tourismType Enogastronomico) (score 1))
  (score(location Camigliatello) (tourismType Balneare) (score 1))
  (score(location Camigliatello) (tourismType Montano) (score 5))
  (score(location Camigliatello) (tourismType Lacustre) (score 3))
  (score(location Camigliatello) (tourismType Naturalistico) (score 4))
  (score(location Camigliatello) (tourismType Termale) (score 1))
  (score(location Camigliatello) (tourismType Culturale) (score 1))
  (score(location Camigliatello) (tourismType Religioso) (score 1))
  (score(location Camigliatello) (tourismType Sportivo) (score 1))
  (score(location Camigliatello) (tourismType Enogastronomico) (score 1))
  (score(location Castrovillari) (tourismType Balneare) (score 1))
  (score(location Castrovillari) (tourismType Montano) (score 1))
  (score(location Castrovillari) (tourismType Lacustre) (score 1))
  (score(location Castrovillari) (tourismType Naturalistico) (score 5))
  (score(location Castrovillari) (tourismType Termale) (score 1))
  (score(location Castrovillari) (tourismType Culturale) (score 3))
  (score(location Castrovillari) (tourismType Religioso) (score 1))
  (score(location Castrovillari) (tourismType Sportivo) (score 1))
  (score(location Castrovillari) (tourismType Enogastronomico) (score 4))
  (score(location Matera) (tourismType Balneare) (score 1))
  (score(location Matera) (tourismType Montano) (score 1))
  (score(location Matera) (tourismType Lacustre) (score 1))
  (score(location Matera) (tourismType Naturalistico) (score 1))
  (score(location Matera) (tourismType Termale) (score 1))
  (score(location Matera) (tourismType Culturale) (score 4))
  (score(location Matera) (tourismType Religioso) (score 1))
  (score(location Matera) (tourismType Sportivo) (score 1))
  (score(location Matera) (tourismType Enogastronomico) (score 1))
  (score(location Lauria) (tourismType Balneare) (score 1))
  (score(location Lauria) (tourismType Montano) (score 1))
  (score(location Lauria) (tourismType Lacustre) (score 4))
  (score(location Lauria) (tourismType Naturalistico) (score 1))
  (score(location Lauria) (tourismType Termale) (score 2))
  (score(location Lauria) (tourismType Culturale) (score 1))
  (score(location Lauria) (tourismType Religioso) (score 1))
  (score(location Lauria) (tourismType Sportivo) (score 1))
  (score(location Lauria) (tourismType Enogastronomico) (score 1))
  (score(location Bari) (tourismType Balneare) (score 5))
  (score(location Bari) (tourismType Montano) (score 1))
  (score(location Bari) (tourismType Lacustre) (score 1))
  (score(location Bari) (tourismType Naturalistico) (score 1))
  (score(location Bari) (tourismType Termale) (score 1))
  (score(location Bari) (tourismType Culturale) (score 1))
  (score(location Bari) (tourismType Religioso) (score 1))
  (score(location Bari) (tourismType Sportivo) (score 1))
  (score(location Bari) (tourismType Enogastronomico) (score 4))
  (score(location Polignano) (tourismType Balneare) (score 5))
  (score(location Polignano) (tourismType Montano) (score 1))
  (score(location Polignano) (tourismType Lacustre) (score 1))
  (score(location Polignano) (tourismType Naturalistico) (score 1))
  (score(location Polignano) (tourismType Termale) (score 1))
  (score(location Polignano) (tourismType Culturale) (score 1))
  (score(location Polignano) (tourismType Religioso) (score 1))
  (score(location Polignano) (tourismType Sportivo) (score 1))
  (score(location Polignano) (tourismType Enogastronomico) (score 3))
  (score(location Alberobello) (tourismType Balneare) (score 1))
  (score(location Alberobello) (tourismType Montano) (score 1))
  (score(location Alberobello) (tourismType Lacustre) (score 1))
  (score(location Alberobello) (tourismType Naturalistico) (score 1))
  (score(location Alberobello) (tourismType Termale) (score 1))
  (score(location Alberobello) (tourismType Culturale) (score 3))
  (score(location Alberobello) (tourismType Religioso) (score 1))
  (score(location Alberobello) (tourismType Sportivo) (score 1))
  (score(location Alberobello) (tourismType Enogastronomico) (score 2))
  (score(location Caserta) (tourismType Balneare) (score 1))
  (score(location Caserta) (tourismType Montano) (score 1))
  (score(location Caserta) (tourismType Lacustre) (score 1))
  (score(location Caserta) (tourismType Naturalistico) (score 1))
  (score(location Caserta) (tourismType Termale) (score 1))
  (score(location Caserta) (tourismType Culturale) (score 5))
  (score(location Caserta) (tourismType Religioso) (score 1))
  (score(location Caserta) (tourismType Sportivo) (score 1))
  (score(location Caserta) (tourismType Enogastronomico) (score 1))
  (score(location Napoli) (tourismType Balneare) (score 1))
  (score(location Napoli) (tourismType Montano) (score 1))
  (score(location Napoli) (tourismType Lacustre) (score 1))
  (score(location Napoli) (tourismType Naturalistico) (score 1))
  (score(location Napoli) (tourismType Termale) (score 1))
  (score(location Napoli) (tourismType Culturale) (score 4))
  (score(location Napoli) (tourismType Religioso) (score 1))
  (score(location Napoli) (tourismType Sportivo) (score 4))
  (score(location Napoli) (tourismType Enogastronomico) (score 4))
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
  (assert (request) (print-sorted))
  (focus QUESTIONS REQUEST CHOOSE)
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
  (question (attribute numPeople)
            (the-question "quante persone siete? inserire numero "))
  (question (attribute nights)
            (the-question "durata della vacanza? inserire numero "))
  (question (attribute minLocations)
            (the-question "numero minimo di località da visitare? inserire numero "))
  (question (attribute maxLocations)
            (the-question "numero massimo di località da visitare? inserire numero "))
  (question (attribute stars)
            (the-question "quante stelle deve avere l'albergo dove pernotterete? inserire numero "))
  (question (attribute price)
            (the-question "quanto vuole spendere? inserire valore "))
  (question (attribute regions)
            (the-question "preferenze su regioni da visitare? inserire nome delle regioni "))
  (question (attribute notregions)
            (the-question "ci sono delle regioni che non vuole visitare? inserire nome delle regioni "))
  (question (attribute tourismTypes)
            (the-question "località da visitare? inserire tipo delle località "))
  (question (attribute nottourismTypes)
            (the-question "ci sono delle località che non vuole visitare? inserire tipo delle località "))
)

(defrule ask-a-question
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

(defrule compile
  (attribute(name ?n)(value $?v))
  (test(or(neq ?v no)(neq ?v \n)))
  ?r <- (request)

  =>

  (switch ?n
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
  )

)

  ;dalle richieste si scelgono delle possiblili opzioni
(defmodule CHOOSE (import MAIN ?ALL )(export ?ALL))

(defrule defineHotel (declare (salience 100))
  (request(name ?name)(numPeople ?numPeople)(regions $?regions)(notregions $?banned)(stars ?minStars)(price ?price))
  (hotel(name ?nameHotel) (numRooms ?numRooms&:(>= ?numRooms (div (+ ?numPeople 1) 2))) (stars ?numStars&:(>= ?numStars ?minStars)) (location ?loc))
  (location (name ?loc) (region ?region))

  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0)))
  (test(or(not(member$ ?region $?regions)) (=(length$ $?banned) 0)))
  (test(or (= ?price 0) (<= (* 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)) ?price)))

=>
  (assert(option(name ?name) (hotel ?nameHotel) (locations ?loc) (nights 1)(price (* 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)))))
)

(defrule defineLocations (declare (salience 90))
  (option(name ?name) (hotel ?hotel)(nights ?nights) (locations $?locations))
  (request(name ?name)(numPeople ?numPeople)(regions $?regions)(nights ?nightsReq) (maxLocations ?num)(tourismTypes $?tourismTypes) (price ?priceReq))
  (hotel(name ?hotel)(location ?location)(stars ?numStars))
  (location (name ?loc&:(neq ?loc ?location)) (region ?region)(tourismTypes $? ?ttype $?))
  (distance(from ?loc)(to ?location)(distance ?d))
  (test(or (= ?priceReq 0) (<= (* (+ ?nights 1) 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)) ?priceReq)))
  (test(or(member$ ?region $?regions)(=(length$ $?regions) 0) (<= ?d 100)))
  (test(or(member$ ?ttype $?tourismTypes)(=(length$ $?tourismTypes) 0)))
  (test(< (length$ $?locations) ?num))
  (test(not(member$ ?loc $?locations)))
  =>

  (assert(option(name ?name)(hotel ?hotel)(nights (+ ?nights 1)) (locations $?locations ?loc) (price (* (+ ?nights 1) 25 (+ ?numStars 1) (div (+ ?numPeople 1) 2)))))
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

;(defrule resetVisited (declare(salience -5))
;  ?reg <- (region)
;  =>
;  (modify ?reg (visited (create$ )))
;)

(defrule CF_Price (declare(salience 50))

  (request(name ?name)(numPeople ?numPeople)(price ?priceReq))
  ?opt <- (option (name ?name)(price ?price)(nights ?nights))

=>
  (if(> ?priceReq 0)
    then
      (bind ?c (mod(*(- ?price ?priceReq -1) 0.01) 1))
      (assert(oav(option ?opt) (attribute price) (value ?price)(certain (abs ?c))))
    else
      (bind ?c (/ (/ 100 ?price) (* ?nights (div (+ ?numPeople 1) 2))))
      (assert(oav(option ?opt) (attribute price) (value ?price) (certain (abs ?c))))
  )
)

(defrule CF_Used (declare(salience 40))
  ?opt <- (option(name ?name)(hotel ?hotel))
  (solution(name ~?name) (hotel ?hotel))
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
  ?opt <- (option)
  ?oavD <- (oav(option ?opt)(attribute distances)(certain ?c&:(< ?c -150)))
  ?oavP <- (oav(option ?opt)(attribute price))
  ?oavS <- (oav(option ?opt)(attribute score))
  =>
  (retract ?opt ?oavS ?oavP ?oavD)
)

(defrule combineCF (declare(salience 10))
  ?oavP <- (oav(option ?opt) (attribute price) (certain ?CP))
  ?oavD <- (oav(option ?opt) (attribute distances) (certain ?CD))
  ?oavS <- (oav(option ?opt) (attribute score) (certain ?CS))
  =>
  (retract ?oavP ?oavD)
  (bind ?CT (* ?CP ?CD ?CS))
;  (printout t "option " ?opt "  score " ?CS "  price " ?CP "  distances " ?CD "  totale " ?CT crlf)
  (modify ?oavS (attribute all)(certain (abs ?CT)))
  (focus PRINT)
)

;si stampano i risultati
(defmodule PRINT (import MAIN ?ALL))

(defrule assert-unprinted "Asserts each item that needs to be printed." (declare(salience 20))
  ?opt <- (option)
  =>
  (assert (unprinted ?opt))
)

(defrule retract-print-sorted "Retract print-sorted after all items enumerated."
  (declare (salience -10))
  ?f <- (print-sorted)
  =>
  ;(retract ?f)
)

(defglobal ?*count* = 0)

(defrule print-sorted (declare(salience 10))

  ?opt <- (option (hotel ?hotel) (locations $?locations) (price ?price))
  ?u <- (unprinted ?opt)
  (oav(option ?opt) (attribute all) (certain ?certain))

  (forall(and (unprinted ?opt1) (oav (option ?opt1) (attribute all)(certain ?c)))
      (test(>= ?certain ?c)))
  =>
  (retract ?u)
;  (if(< ?*count* 5)
;    then
      (printout t "hotel " ?hotel " locations " $?locations "  price " ?price  "  cf " ?certain crlf)
;  )
  (bind ?*count* (+ ?*count* 1))

)

;input
;aggiornamento request
;certan factor con valore sullo score
;aggiornare fatti dopo la richiesta
;consigliare posti vicini alla regione
