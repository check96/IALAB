(defmodule MAIN (export ?ALL))

(deftemplate location
  (slot name)
  (slot region)
)

(deftemplate hotel
  (slot name)
  (slot stars (type INTEGER) (range 1 5))
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
  (slot maxLocations (default 1) (type INTEGER))
  (multislot date)
  (slot nights (default 7) (type INTEGER))
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
  (slot tourismType (allowed-values Balneare Montano Lacustre Naturalistico Termale Culturale Religioso Sportivo Enogastronomico))
  (slot score (type INTEGER) (default 0) (range 0 5))
)

(deffacts locations
  (location(name Schiavonea)(region Calabria))
  (location(name Camigliatello)(region Calabria))
  (location(name Castrovillari)(region Calabria))
  (location(name Bari)(region Puglia))
  (location(name Polignano)(region Puglia))
  (location(name Alberobello)(region Puglia))
  (location(name Matera)(region Basilicata))
  (location(name Lauria)(region Basilicata))
  (location(name Napoli)(region Campania))
  (location(name Caserta)(region Campania))
  (location(name Susa)(region Piemonte))
  (location(name Montecatini)(region Toscana))
  (location(name Garda)(region Veneto))
  (location(name Firenze)(region Toscana))
  (location(name Bologna)(region EmiliaRomagna))
  (location(name Torino)(region Piemonte))
  (location(name Genova)(region Liguria))
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
  (hotel(name Hotel_Napoleon_Susa)(stars 3)(numRooms 13)(location Susa))
  (hotel(name Alpi_Hotel)(stars 4)(numRooms 38)(location Susa))
  (hotel(name Grand_Hotel_Tettuccio)(stars 4)(numRooms 44)(location Montecatini))
  (hotel(name Hotel_Roma)(stars 3)(numRooms 21)(location Garda))
  (hotel(name Hotel_Bonciani)(stars 3)(numRooms 55)(location Firenze))
  (hotel(name Vinaio)(stars 4)(numRooms 45)(location Firenze))
  (hotel(name Hotel_Cosmopolitan)(stars 4)(numRooms 139)(location Bologna))
  (hotel(name Hotel_Alpi_Resort)(stars 3)(numRooms 31)(location Torino))
  (hotel(name King_Hotel)(stars 4)(numRooms 310)(location Torino))
  (hotel(name Starhotels_President)(stars 4)(numRooms 237)(location Genova))
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
  (distance(from Susa)(to Montecatini)(distance 408))
  (distance(from Susa)(to Garda)(distance 340))
  (distance(from Susa)(to Firenze)(distance 459))
  (distance(from Susa)(to Bologna)(distance 369))
  (distance(from Susa)(to Torino)(distance 57))
  (distance(from Susa)(to Genova)(distance 220))
  (distance(from Montecatini)(to Garda)(distance 265))
  (distance(from Montecatini)(to Firenze)(distance 48))
  (distance(from Montecatini)(to Bologna)(distance 107))
  (distance(from Montecatini)(to Torino)(distance 360))
  (distance(from Montecatini)(to Genova)(distance 180))
  (distance(from Garda)(to Firenze)(distance 258))
  (distance(from Garda)(to Bologna)(distance 165))
  (distance(from Garda)(to Torino)(distance 298))
  (distance(from Garda)(to Genova)(distance 280))
  (distance(from Firenze)(to Bologna)(distance 97))
  (distance(from Firenze)(to Torino)(distance 420))
  (distance(from Firenze)(to Genova)(distance 230))
  (distance(from Bologna)(to Torino)(distance 331))
  (distance(from Bologna)(to Genova)(distance 296))
  (distance(from Torino)(to Genova)(distance 171))
)

;;;Balneare Montano Lacustre Naturalistico Termale Culturale Religioso Sportivo Enogastronomico

(deffacts scores
  (score(location Schiavonea) (tourismType Balneare) (score 3))
  (score(location Schiavonea) (tourismType Naturalistico) (score 3))
  (score(location Schiavonea) (tourismType Culturale) (score 2))
  (score(location Schiavonea) (tourismType Religioso) (score 3))
  (score(location Schiavonea) (tourismType Enogastronomico) (score 4))
  (score(location Camigliatello) (tourismType Montano) (score 4))
  (score(location Camigliatello) (tourismType Lacustre) (score 3))
  (score(location Camigliatello) (tourismType Naturalistico) (score 4))
  (score(location Camigliatello) (tourismType Enogastronomico) (score 3))
  (score(location Castrovillari) (tourismType Naturalistico) (score 5))
  (score(location Castrovillari) (tourismType Culturale) (score 2))
  (score(location Castrovillari) (tourismType Sportivo) (score 3))
  (score(location Castrovillari) (tourismType Enogastronomico) (score 4))
  (score(location Matera) (tourismType Naturalistico) (score 3))
  (score(location Matera) (tourismType Culturale) (score 4))
  (score(location Matera) (tourismType Enogastronomico) (score 3))
  (score(location Lauria) (tourismType Montano) (score 2))
  (score(location Lauria) (tourismType Lacustre) (score 3))
  (score(location Lauria) (tourismType Naturalistico) (score 3))
  (score(location Lauria) (tourismType Termale) (score 2))
  (score(location Bari) (tourismType Balneare) (score 5))
  (score(location Bari) (tourismType Naturalistico) (score 2))
  (score(location Bari) (tourismType Culturale) (score 3))
  (score(location Bari) (tourismType Sportivo) (score 3))
  (score(location Bari) (tourismType Enogastronomico) (score 4))
  (score(location Polignano) (tourismType Balneare) (score 5))
  (score(location Polignano) (tourismType Enogastronomico) (score 3))
  (score(location Alberobello) (tourismType Naturalistico) (score 3))
  (score(location Alberobello) (tourismType Culturale) (score 4))
  (score(location Alberobello) (tourismType Enogastronomico) (score 3))
  (score(location Caserta) (tourismType Naturalistico) (score 2))
  (score(location Caserta) (tourismType Culturale) (score 5))
  (score(location Caserta) (tourismType Enogastronomico) (score 2))
  (score(location Napoli) (tourismType Balneare) (score 2))
  (score(location Napoli) (tourismType Naturalistico) (score 2))
  (score(location Napoli) (tourismType Culturale) (score 4))
  (score(location Napoli) (tourismType Religioso) (score 4))
  (score(location Napoli) (tourismType Sportivo) (score 4))
  (score(location Napoli) (tourismType Enogastronomico) (score 5))
  (score(location Susa) (tourismType Montano) (score 5))
  (score(location Susa) (tourismType Naturalistico) (score 5))
  (score(location Susa) (tourismType Enogastronomico) (score 3))
  (score(location Susa) (tourismType Lacustre) (score 3))
  (score(location Susa) (tourismType Sportivo) (score 4))
  (score(location Montecatini) (tourismType Termale) (score 4))
  (score(location Montecatini) (tourismType Culturale) (score 3))
  (score(location Garda) (tourismType Lacustre) (score 5))
  (score(location Garda) (tourismType Culturale) (score 2))
  (score(location Garda) (tourismType Naturalistico) (score 3))
  (score(location Firenze) (tourismType Culturale) (score 5))
  (score(location Firenze) (tourismType Religioso) (score 4))
  (score(location Firenze) (tourismType Enogastronomico) (score 4))
  (score(location Firenze) (tourismType Sportivo) (score 3))
  (score(location Bologna) (tourismType Enogastronomico) (score 4))
  (score(location Bologna) (tourismType Culturale) (score 4))
  (score(location Bologna) (tourismType Religioso) (score 3))
  (score(location Bologna) (tourismType Sportivo) (score 3))
  (score(location Torino) (tourismType Culturale) (score 5))
  (score(location Torino) (tourismType Religioso) (score 4))
  (score(location Torino) (tourismType Sportivo) (score 4))
  (score(location Torino) (tourismType Enogastronomico) (score 4))
  (score(location Genova) (tourismType Enogastronomico) (score 3))
  (score(location Genova) (tourismType Balneare) (score 4))
  (score(location Genova) (tourismType Culturale) (score 3))

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
  (rule(if ?type&~nil) (then ?type2&~nil)(divisor ?div))
=>
  (assert(rule(if ?type2)(then ?type)(divisor ?div)))
)

(defrule simmetricalRulesNot(declare(salience 1000))
  (rule(if ?type&~nil) (not ?type2&~nil)(divisor ?div))
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
  (bind ?year (nth$ 1 (local-time)))
  (assert (request(date 1 1 (+ 1 ?year))))
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
  (question (attribute date)
            (the-question "In che data vorresti partire? (dd mm yyyy)"))
  (question (attribute numLocations)
            (the-question "Durante la vacanza, preferisci spostarti molto o poco?")
            (type STRING)
            (valid-answers molto poco))
  (question (attribute nights)
            (the-question "Quanti giorni preferisti che durasse la vacanza? inserire numero dei giorni")
            (type INTEGER))
  (question (attribute notRegions)
            (the-question "Regioni che non vuoi visitare? inserire nome delle regioni ")
            (type STRING)
            (valid-answers "calabria" "puglia" "basilicata" "campania" "piemonte" "veneto" "liguria" "toscana" "emiliaromagna"))
  (question (attribute regions)
            (the-question "Preferenze sulle regioni da visitare? inserire nome delle regioni ")
            (type STRING)
            (valid-answers "calabria" "puglia" "basilicata" "campania" "piemonte" "veneto" "liguria" "toscana" "emiliaromagna"))
  (question (attribute notTourismTypes)
            (the-question "Ci sono tipi di località che preferiresti evitare?")
            (type STRING)
            (valid-answers "balneare" "montano" "lacustre" "naturalistico" "termale" "culturale" "religioso" "sportivo" "enogastronomico"))
  (question (attribute tourismTypes)
            (the-question "Che tipo di località vuoi visitare? ")
            (type STRING)
            (valid-answers "balneare" "montano" "lacustre" "naturalistico" "termale" "culturale" "religioso" "sportivo" "enogastronomico"))
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
          (bind ?answ (lowcase(str-cat ?answ))))

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
      (case nights then (modify ?r (nights (nth$ 1 ?v))))
      (case date then (modify ?r (date ?v)))
      (case numLocations then
              (if (eq(nth$ 1 ?v) molto)
                then
                  (modify ?r (minLocations (* 3 (+ 1 (mod ?n 7)))))
                else
                  (modify ?r (maxLocations (* 3 (+ 1 (mod ?n 7)))))
              )
      )
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
    (retract ?oavR ?oavT)
    (bind ?cf (* ?cfU (/ (+ ?cfR ?cfT) 2)))
    (modify ?oavU(type all)(cf ?cf))
)

(defrule focus-on-path
  (not(or(oav(type region)) (oav(type tourismType))))
=>
  (focus PATH)
)


;;; #############
;;; PATH MODULE
;;; #############

(defmodule PATH (import MAIN ?ALL)(import QUESTIONS ?ALL))

(deffunction calculateAwayDate (?nights $?date)

  (bind ?day (integer (nth$ 1 $?date)))
  (bind ?month (integer (nth$ 2 $?date)))
  (bind ?year (integer (nth$ 3 $?date)))

  (if(and(>(+ ?day ?nights) 30)(or(= ?month 04)(= ?month 06)(= ?month 09) (= ?month 11)))
    then
      (bind ?awayDate 1 3 (-(+ ?day ?nights) 30) (+ ?month 1) ?year)
      (return ?awayDate))

  (if(and(>(+ ?day ?nights) 31)(or(= ?month 01)(= ?month 03)(= ?month 05)(= ?month 07)(= ?month 08)(= ?month 10)))
    then
      (bind ?awayDate (-(+ ?day ?nights) 31) (+ ?month 1) ?year)
      (return ?awayDate))

  (if(and(>(+ ?day ?nights) 28)(= ?month 02))
    then
      (bind ?awayDate (-(+ ?day ?nights) 28) 03 ?year)
      (return ?awayDate))

  (if(and(>(+ ?day ?nights) 31)(= ?month 12))
    then
      (bind ?awayDate (-(+ ?day ?nights) 31) 01 (+ ?year 1))
      (return ?awayDate))

  (bind ?awayDate (+ ?day ?nights) ?month ?year)
  (return ?awayDate)
)

(deffunction convertDate ($?date)
  (bind ?daysForMonth (create$ 31 28 31 30 31 30 31 31 30 31 30 31))
  (bind ?days 0)
  (loop-for-count (?i 1 (- (nth$ 2 $?date) 1)) do
    (bind ?days (+ ?days (nth$ ?i ?daysForMonth)))

  (bind ?days (*(+ ?days (nth$ 1 $?date)) (-(nth$ 3 $?date) 2019 -1))))
  ?days)

(deftemplate MAIN::path
  (slot pid)
  (slot hotel)
  (multislot arrivalDate)
  (multislot awayDate)
  (slot price (type FLOAT))
)

(defglobal ?*path_id* = 1)
(defrule first-location(declare(salience 100))
  (request(nights ?nights)(date ?date))
  (oav(hotel ?hotel)(type all)(cf ?cf))
  ;(not(oav(hotel ?hotel1&~?hotel)(type all)(cf ?cf1&:(> ?cf1 ?cf))))
  (test(<= ?*path_id* 5))
  =>
  (assert(path(pid ?*path_id*)(hotel ?hotel) (arrivalDate ?date) (awayDate calculateAwayDate ?nights ?date)))
  (bind ?*path_id* (+ 1 ?*path_id*))
)

(defrule make-path (declare(salience 90))
  (request(nights ?n)(date ?date))
  (oav(hotel ?hotel)(type all)(cf ?cf))
  =>
  (assert(path(pid ?*path_id*)(hotel ?hotel)))
)


(defrule focus-on-print (declare(salience -100))
  =>
  (focus PRINT)
)
;;; ##############
;;; PRINT MODULE
;;; ##############

(defmodule PRINT (import MAIN ?ALL)(import QUESTIONS ?ALL))
(defrule assert-unprinted
  (hotel (name ?hotel))
=>
  (assert(unprinted ?hotel))
)

(defrule print-results
  ?oav <- (oav(hotel ?hotel)(type all)(cf ?cf))
  (hotel(name ?hotel)(location ?loc))
  ?unpr <- (unprinted ?hotel)
  (not(oav(hotel ?hotel1&~?hotel)(type all)(cf ?cf1&:(> ?cf1 ?cf))))

  =>
  (retract ?oav ?unpr)
  (printout t ?hotel " in " ?loc " ---> " ?cf crlf)
)
