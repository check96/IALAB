(defmodule MAIN (export ?ALL))
;;; #############
;;; DEFTEMPLATE
;;; #############

(deftemplate location
  (slot name)
  (slot region)
)

(deftemplate hotel
  (slot name)
  (slot stars (type INTEGER) (range 1 4))
  (slot numRooms (type INTEGER))
  (slot location)
)

(deftemplate request
  (slot name (default (gensym)))
  (slot numPeople (default 1)(type INTEGER))
  (multislot regions)
  (multislot notRegions)
  (multislot tourismTypes)
  (multislot notTourismTypes)
  (slot stars (default 1)(type INTEGER))
  (slot spostamenti (default poco))
  (slot minLocations(type INTEGER))
  (slot maxLocations(type INTEGER))
  (multislot date)
  (slot nights (default 7) (type INTEGER))
  (slot price (default 0) (type INTEGER))
)

(deftemplate distance
  (slot from)
  (slot to)
  (slot distance)
)

(deftemplate score
  (slot location)
  (slot tourismType (allowed-values balneare montano lacustre naturalistico termale culturale religioso sportivo enogastronomico))
  (slot score (type INTEGER) (default 0) (range 0 5))
)

(deftemplate oav
  (slot object)
  (slot type)
  (slot cf)
  (slot ranked (default no))
)

(deftemplate rule
  (slot if)
  (slot then)
  (slot not)
  (slot coefficient (default 2))
)

(deftemplate path
  (slot pid)
  (multislot hotels)
  (multislot locations)
  (multislot cfHotels)
  (multislot arrivalDate)
  (multislot awayDate)
  (slot price)
  (slot cfPath (type FLOAT))
  (slot ranked (default no))
)

(deftemplate stage
  (slot path)
  (slot hotel)
  (slot location)
  (multislot arrivalDate)
  (multislot awayDate)
  (slot numNights (default 2))
)


(deftemplate reservation
  (slot name)
  (slot path)
  (slot numPeople)
  (slot price)
;; DA COMPLETARE
)

;;; ##########
;;; DEFFACTS
;;; ##########

(deffacts locations
  (location(name Schiavonea)(region calabria))
  (location(name Camigliatello)(region calabria))
  (location(name Castrovillari)(region calabria))
  (location(name Bari)(region puglia))
  (location(name Polignano)(region puglia))
  (location(name Alberobello)(region puglia))
  (location(name Matera)(region basilicata))
  (location(name Lauria)(region basilicata))
  (location(name Napoli)(region campania))
  (location(name Caserta)(region campania))
  (location(name Susa)(region piemonte))
  (location(name Torino)(region piemonte))
  (location(name Montecatini)(region toscana))
  (location(name Brenta)(region trentino))
  (location(name Garda)(region veneto))
  (location(name Firenze)(region toscana))
  (location(name Bologna)(region emiliaromagna))
  (location(name Genova)(region liguria))
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
  (hotel(name Hotel_Garda)(stars 3)(numRooms 21)(location Garda))
  (hotel(name Brenta_Hotel)(stars 4)(numRooms 71)(location Brenta))
  (hotel(name Hotel_Bonciani)(stars 3)(numRooms 55)(location Firenze))
  (hotel(name Vinaio)(stars 4)(numRooms 45)(location Firenze))
  (hotel(name Hotel_Cosmopolitan)(stars 4)(numRooms 139)(location Bologna))
  (hotel(name Granata_Resort)(stars 3)(numRooms 31)(location Torino))
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
  (distance(from Susa)(to Brenta)(distance 400))
  (distance(from Montecatini)(to Garda)(distance 265))
  (distance(from Montecatini)(to Firenze)(distance 48))
  (distance(from Montecatini)(to Bologna)(distance 107))
  (distance(from Montecatini)(to Torino)(distance 360))
  (distance(from Montecatini)(to Genova)(distance 180))
  (distance(from Montecatini)(to Brenta)(distance 387))
  (distance(from Garda)(to Firenze)(distance 258))
  (distance(from Garda)(to Bologna)(distance 165))
  (distance(from Garda)(to Torino)(distance 298))
  (distance(from Garda)(to Genova)(distance 280))
  (distance(from Garda)(to Brenta)(distance 93))
  (distance(from Firenze)(to Bologna)(distance 97))
  (distance(from Firenze)(to Torino)(distance 420))
  (distance(from Firenze)(to Genova)(distance 230))
  (distance(from Firenze)(to Brenta)(distance 367))
  (distance(from Bologna)(to Torino)(distance 331))
  (distance(from Bologna)(to Genova)(distance 296))
  (distance(from Bologna)(to Brenta)(distance 280))
  (distance(from Torino)(to Genova)(distance 171))
  (distance(from Torino)(to Brenta)(distance 349))
  (distance(from Genova)(to Brenta)(distance 343))
)

(deffacts scores
  (score(location Schiavonea) (tourismType balneare) (score 3))
  (score(location Schiavonea) (tourismType naturalistico) (score 3))
  (score(location Schiavonea) (tourismType culturale) (score 2))
  (score(location Schiavonea) (tourismType religioso) (score 3))
  (score(location Schiavonea) (tourismType enogastronomico) (score 4))
  (score(location Camigliatello) (tourismType montano) (score 4))
  (score(location Camigliatello) (tourismType lacustre) (score 3))
  (score(location Camigliatello) (tourismType naturalistico) (score 4))
  (score(location Camigliatello) (tourismType enogastronomico) (score 3))
  (score(location Castrovillari) (tourismType naturalistico) (score 5))
  (score(location Castrovillari) (tourismType culturale) (score 2))
  (score(location Castrovillari) (tourismType sportivo) (score 3))
  (score(location Castrovillari) (tourismType enogastronomico) (score 4))
  (score(location Matera) (tourismType naturalistico) (score 3))
  (score(location Matera) (tourismType culturale) (score 4))
  (score(location Matera) (tourismType enogastronomico) (score 3))
  (score(location Lauria) (tourismType montano) (score 2))
  (score(location Lauria) (tourismType lacustre) (score 3))
  (score(location Lauria) (tourismType naturalistico) (score 3))
  (score(location Lauria) (tourismType termale) (score 2))
  (score(location Bari) (tourismType balneare) (score 5))
  (score(location Bari) (tourismType naturalistico) (score 2))
  (score(location Bari) (tourismType culturale) (score 3))
  (score(location Bari) (tourismType sportivo) (score 3))
  (score(location Bari) (tourismType enogastronomico) (score 4))
  (score(location Polignano) (tourismType balneare) (score 5))
  (score(location Polignano) (tourismType enogastronomico) (score 3))
  (score(location Alberobello) (tourismType naturalistico) (score 3))
  (score(location Alberobello) (tourismType culturale) (score 4))
  (score(location Alberobello) (tourismType enogastronomico) (score 3))
  (score(location Caserta) (tourismType naturalistico) (score 2))
  (score(location Caserta) (tourismType culturale) (score 5))
  (score(location Caserta) (tourismType enogastronomico) (score 2))
  (score(location Napoli) (tourismType balneare) (score 2))
  (score(location Napoli) (tourismType naturalistico) (score 2))
  (score(location Napoli) (tourismType culturale) (score 4))
  (score(location Napoli) (tourismType religioso) (score 4))
  (score(location Napoli) (tourismType sportivo) (score 4))
  (score(location Napoli) (tourismType enogastronomico) (score 5))
  (score(location Susa) (tourismType montano) (score 5))
  (score(location Susa) (tourismType naturalistico) (score 5))
  (score(location Susa) (tourismType enogastronomico) (score 3))
  (score(location Susa) (tourismType lacustre) (score 3))
  (score(location Susa) (tourismType sportivo) (score 4))
  (score(location Montecatini) (tourismType termale) (score 4))
  (score(location Montecatini) (tourismType culturale) (score 3))
  (score(location Garda) (tourismType lacustre) (score 5))
  (score(location Garda) (tourismType culturale) (score 2))
  (score(location Garda) (tourismType naturalistico) (score 3))
  (score(location Firenze) (tourismType culturale) (score 5))
  (score(location Firenze) (tourismType religioso) (score 4))
  (score(location Firenze) (tourismType enogastronomico) (score 4))
  (score(location Firenze) (tourismType sportivo) (score 3))
  (score(location Bologna) (tourismType enogastronomico) (score 5))
  (score(location Bologna) (tourismType culturale) (score 4))
  (score(location Bologna) (tourismType religioso) (score 3))
  (score(location Bologna) (tourismType sportivo) (score 3))
  (score(location Torino) (tourismType culturale) (score 5))
  (score(location Torino) (tourismType religioso) (score 4))
  (score(location Torino) (tourismType sportivo) (score 4))
  (score(location Torino) (tourismType enogastronomico) (score 4))
  (score(location Genova) (tourismType enogastronomico) (score 3))
  (score(location Genova) (tourismType balneare) (score 3))
  (score(location Genova) (tourismType culturale) (score 3))
  (score(location Brenta) (tourismType enogastronomico) (score 3))
  (score(location Brenta) (tourismType montano) (score 4))
  (score(location Brenta) (tourismType sportivo) (score 3))
  (score(location Brenta) (tourismType naturalistico) (score 5))
  (score(location Brenta) (tourismType lacustre) (score 3))
  (score(location Brenta) (tourismType culturale) (score 2))
)

(deffacts experience_rules
  (rule(if balneare) (not montano)(coefficient 2))
  (rule(if balneare) (then naturalistico )(coefficient 3))
  (rule(if balneare) (then enogastronomico)(coefficient 3))
  (rule(if balneare) (then sportivo)(not lacustre)(coefficient 4))
  (rule(if montano) (then naturalistico)(coefficient 2))
  (rule(if montano) (then sportivo)(coefficient 3))
  (rule(if montano) (then lacustre)(coefficient 3))
  (rule(if montano) (then religioso)(coefficient 4))
  (rule(if montano) (then enogastronomico)(coefficient 4))
  (rule(if montano) (then culturale)(coefficient 5))
  (rule(if lacustre) (then termale) (coefficient 2))
  (rule(if lacustre) (then naturalistico) (not sportivo)(coefficient 3))
  (rule(if naturalistico) (not religioso) (coefficient 3))
  (rule(if naturalistico) (not culturale) (coefficient 3))
  (rule(if naturalistico) (then sportivo) (coefficient 4))
  (rule(if naturalistico) (then enogastronomico) (coefficient 4))
  (rule(if naturalistico) (then termale) (coefficient 5))
  (rule(if termale)(not sportivo)(coefficient 2))
  (rule(if termale)(not culturale)(coefficient 3))
  (rule(if termale)(not religioso)(coefficient 3))
  (rule(if termale)(not enogastronomico)(coefficient 3))
  (rule(if culturale) (then religioso)(not sportivo)(coefficient 2))
  (rule(if religioso) (not sportivo)(coefficient 2))
  (rule(if sportivo) (not enogastronomico)(coefficient 3))
)

(defrule simmetricalRules(declare(salience 1000))
  (rule(if ?type) (then ?type2 & ~nil)(coefficient ?div))
=>
  (assert(rule(if ?type2)(then ?type)(coefficient ?div)))
)

(defrule simmetricalRulesNot(declare(salience 1000))
  (rule(if ?type) (not ?type2 & ~nil)(coefficient ?div))
=>
  (assert(rule(if ?type2)(not ?type)(coefficient ?div)))
)

(defrule simmetricalDistance (declare(salience 1000))
  (distance(from ?a)(to ?b)(distance ?d))
=>
  (assert(distance(from ?b)(to ?a)(distance ?d)))
)

(defrule start
  =>
  (bind ?year (nth$ 1 (local-time)))
  (assert(request(date 1 1 (+ 1 ?year))))
  (focus QUESTIONS RULES PATH)
)

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
      (case spostamenti then (modify ?r (spostamenti (nth$ 1 ?v))))
      (case price then (modify ?r (price (nth$ 1 ?v))))
      (case date then
          (bind ?daysForMonth (create$ 31 28 31 30 31 30 31 31 30 31 30 31))
            ;;;; AGGIUNGERE CONTROLLI SULLA DATA
          (modify ?r (date ?v)))
    )
)

(defrule lastChecks (declare(salience -10))
  ?req <- (request(spostamenti ?value)(nights ?nights))
=>
  (if (eq ?value molto)
      then
        (modify ?req(minLocations (+ 2 (div ?nights 8))) (maxLocations (+ 3 (div ?nights 8))))
      else
        (modify ?req(minLocations (+ 1 (div ?nights 8))) (maxLocations (+ 2 (div ?nights 8))))
  )
  (assert(rank (create$ ))(pathRank (create$ )))
)

;;; ###############
;;  RULES MODULE
;;; ###############

(defmodule RULES (import MAIN ?ALL)(import QUESTIONS ?ALL)(export ?ALL))

(defrule assertOAV
  (request(numPeople ?numPeople)(stars ?stars))
  (hotel(name ?hotel)(stars ?numStars&:(>= ?numStars ?stars))(numRooms ?numRooms&:(>= ?numRooms (div (+ ?numPeople 1) 2))))
=>
  (assert(oav(object ?hotel)(type region)(cf 0.5)))
  (assert(oav(object ?hotel)(type stars)(cf (/ ?numStars 5))))
)

(defrule CF_regions
  ?oav <- (oav(object ?hotel)(type region))
  (request(regions $?regions)(notRegions $?notRegions))
  (hotel(name ?hotel)(location ?location))
  (location(name ?location)(region ?region))
=>
  (if(member$ ?region $?regions) then
    (modify ?oav(cf 0.85)))

  (if(member$ ?region $?notRegions) then
    (modify ?oav(cf 0.15)))
)

(defglobal ?*value* = 0.5)
(defrule CF_tourismType
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
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes) ?r:coefficient ))))
    (if(and(member$ ?r:if $?ttypes)(eq ?r:not ?s:tourismType)) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?ttypes) ?r:coefficient))))
  )
  (assert(oav(object ?hotel)(type tourismType)(cf ?*value*)))
  (bind ?*value* 0.5)
)

(defrule CF_Used
  (request)
  (hotel (name ?hotel))
=>
  (bind ?value 1)
  (do-for-all-facts ((?s stage)) (eq ?s:hotel ?hotel)
    (bind ?value (* ?value 0.9)))
  (assert(oav(object ?hotel)(type used) (cf ?value)))
)

(defrule combineCF (declare(salience -10))
  ?oavR <- (oav(object ?hotel)(type region)(cf ?cfR))
  ?oavT <- (oav(object ?hotel)(type tourismType)(cf ?cfT))
  ?oavS <- (oav(object ?hotel)(type stars)(cf ?cfS))
  ?oavU <- (oav(object ?hotel)(type used)(cf ?cfU))
  =>
    (retract ?oavR ?oavT ?oavS)
    (bind ?cf (* ?cfU (+ (* 0.5 ?cfR) (* 0.4 ?cfT) (* 0.1 ?cfS))))
    ;(printout t ?hotel " (R: " ?cfR ", T: " ?cfT ", U: "?cfU ", S: "?cfS ")  ---> " ?cf crlf)
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

;;; #############
;;; PATH MODULE
;;; #############

(defmodule PATH (import MAIN ?ALL)(import RULES ?ALL)(import QUESTIONS ?ALL)(export ?ALL))

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

(defrule first-location (declare(salience 100))
  (rank $? ?hotel $?)
  (hotel(name ?hotel)(location ?location))
  (oav(object ?hotel)(type all)(cf ?cf))
=>
  (bind ?pid (gensym))
  (assert(path(pid ?pid)(hotels ?hotel)(locations ?location)(cfHotels ?cf)(cfPath ?cf)))
)

(defrule make-path (declare(salience 90))
  (request(maxLocations ?max))
  ?path <- (path(pid ?pid)(hotels $?hotels)(locations $?locations)(cfHotels $?cfHotels)(cfPath ?cfPath))
  (rank $? ?hotel1 $?)
  (hotel(name ?hotel&:(eq  ?hotel (nth$ (length$ $?hotels) $?hotels))) (location ?loc))
  (hotel(name ?hotel1)(location ?loc1))
  (oav(object ?hotel1)(type all)(cf ?cfHotel))
  (test(< (length$ $?hotels) ?max))
  (test(not(member$ ?hotel1 $?hotels)))
  (test(not(member$ ?loc1 $?locations)))

  (distance(from ?loc)(to ?loc1)(distance ?d&:(<= ?d 200)))
=>
  (bind ?cf (/ (+ (* ?cfPath (length$ $?hotels)) ?cfHotel) (+ 1 (length$ $?hotels))))
  (modify ?path(hotels $?hotels ?hotel1)(locations $?locations ?loc1) (cfHotels $?cfHotels ?cfHotel) (cfPath ?cf))
)

(defrule cf_Distances (declare(salience 70))
  (path(pid ?pid)(locations $?locations)(cfPath ?cf)(ranked no))
  (not(calculated ?pid))
=>
  (bind ?value 0)
  (loop-for-count (?i 1 (- (length$ $?locations) 1)) do
    (do-for-all-facts ((?d distance)) (and(eq ?d:from (nth$ ?i $?locations))(eq ?d:to (nth$ (+ 1 ?i) $?locations)))
      (bind ?value (+ ?value ?d:distance)))
  )

  (bind ?cfD (/ (- ?value (* 100 (length$ $?locations))) 1000))
  (if (< ?cfD 0) then
    (bind ?cfD 0))

  (assert(oav(object ?pid)(type distances)(cf ?cfD)))
)

(deffunction dayForHotel (?totalDays $?cf)
  (bind ?total 0.0)

  (loop-for-count (?i 1 (length$ $?cf))
    (bind ?total (+ ?total (nth$ ?i $?cf))))
  (bind $?days (create$))
  (bind ?sum 0)

  (loop-for-count (?i 1 (length$ $?cf))
    (bind ?day (round(/ (* ?totalDays (nth$ ?i $?cf)) ?total)))
    (bind $?days (create$ $?days ?day))
    (bind ?sum (+ ?sum ?day))
  )

  (bind ?totalDays (- ?totalDays ?sum))
  (loop-for-count (?i 1 ?totalDays)
    (bind ?d (+ 1(nth$ ?i $?days)))
    (replace$ $?days ?i ?i ?d))

$?days)

(defrule make-stages (declare(salience 60))
  (request(nights ?nights)(date $?date))
  (path (pid ?pid)(hotels $?hotels)(locations $?locations)(cfHotels $?cfs)(ranked no))

=>
  (bind $?days (dayForHotel ?nights $?cfs))

  (loop-for-count (?i 1 (length$ $?hotels))
    (assert(stage(path ?pid)(hotel (nth$ ?i $?hotels))(location (nth$ ?i $?locations))(arrivalDate $?date)(awayDate (calculateAwayDate (nth$ ?i $?days) $?date)) (numNights (nth$ ?i $?days))))
    (bind $?date (calculateAwayDate (nth$ ?i $?days) $?date)))
)

(deffunction verifyRooms(?numPeople ?hotel ?numRooms $?dates)
    (bind ?sum 0)
    (do-for-all-facts((?s stage))  (eq ?s:hotel ?hotel)
      (bind ?arrive (convertDate ?s:arrivalDate))
      (bind ?away (convertDate ?s:awayDate))
      (bind ?arrivalDate (convertDate (create$ (nth$ 1 $?dates)(nth$ 2 $?dates)(nth$ 3 $?dates))))
      (bind ?awayDate (convertDate (create$ (nth$ 4 $?dates)(nth$ 5 $?dates)(nth$ 6 $?dates))))
      (if(and (or (< ?arrivalDate ?arrive)(> ?arrivalDate ?away)) (or(< ?awayDate ?arrive)(> ?awayDate ?away)))
        then
          (bind ?sum (+ ?sum ?s:numPeople)))
    )
    (return (< (- ?numRooms ?sum) (div (+ ?numPeople 1) 2)))
)

(defrule verifyAvailable (declare(salience 50))
  (request(numPeople ?numPeople))
  ?path <- (path (pid ?pid)(ranked no))
  ?stage <- (stage (path ?pid)(hotel ?hotel)(arrivalDate $?date)(awayDate $?away))
  (hotel(name ?hotel)(numRooms ?numRooms))
  (not(calculated ?pid))
=>
  (if(verifyRooms ?numPeople ?hotel ?numRooms (create$ $?date $?away)) then
    (retract ?stage ?path))
)

(defrule pruning-samePath (declare(salience 40))
  (path(pid ?pid)(hotels $?hotels)(ranked no))
  ?path <- (path(pid ?pid1 & ~?pid)(hotels $?hotels1)(ranked no))

  (test(=(length$ $?hotels) (length$ $?hotels1)))
  (test(subsetp $?hotels $?hotels1))
  =>
  (retract ?path)
  (do-for-all-facts((?s stage))  (eq ?s:path ?pid1)
    (retract ?s))
)

(defrule cf_Price (declare(salience 30))
  (request(numPeople ?numPeople)(price ?priceReq)(nights ?nights))
  ?path <- (path(pid ?pid)(ranked no)(price nil))
  (not(calculated ?pid))
=>
  (bind ?price 0)
  (do-for-all-facts ((?s stage) (?h hotel)) (and(eq ?s:path ?pid)(eq ?s:hotel ?h:name))
    (bind ?price (+ ?price (* ?s:numNights 25 (+ ?h:stars 1) (div (+ ?numPeople 1) 2)))))

  (if (or(< ?price ?priceReq)(= ?priceReq 0))
    then
      (assert(oav(object ?pid)(type price)(cf (/ ?price ?nights 125))))
      (modify ?path(price ?price))
    else
      (retract ?path)
  )
)

(defrule combineOAV
  ?oavD <- (oav(object ?pid)(type distances)(cf ?cfD))
  ?oavP <- (oav(object ?pid)(type price)(cf ?cfP))
  ?path <- (path(pid ?pid)(cfPath ?cfPath)(ranked no))
=>
  (retract ?oavD ?oavP)
  (modify ?path(cfPath (- (+ (* 0.6 ?cfPath) (* 0.4 ?cfP)) ?cfD)))
  (assert(calculated ?pid))
)

(defrule rankPath (declare(salience -50))
  ?path <- (path(pid ?pid)(cfPath ?cf&:(> ?cf 0.5))(ranked no))
  (not(path(pid ~?pid)(cfPath ?cf1&:(> ?cf1 ?cf))(ranked no)))
  ?rank <- (pathRank $?ranks)
  ?c <- (calculated ?pid)
  =>
  (retract ?rank ?c)
  (modify ?path (ranked yes))
  (assert(pathRank $?ranks ?pid))
)

(defglobal ?*count* = 1)

(defrule print-path (declare(salience -100))
  (pathRank $? ?path $?)
  ?p <- (path(pid ?path)(cfPath ?cfPath)(price ?price))
=>
  (if (<= ?*count* 5)
    then
      (printout t ?*count* " -   " ?path "  -->  " ?price "€.  --> " ?cfPath crlf)
      (do-for-all-facts ((?s stage)) (eq ?s:path ?path)
        (printout t "    " ?s:hotel " in " ?s:location "  " ?s:arrivalDate "     " ?s:awayDate crlf))
      (bind ?*count* (+ 1 ?*count*))
    else
      (retract ?p))
)

(defrule choosePath (declare(salience -110))
  (pathRank $?paths)
  =>
    (bind ?in -1)
  	(while(or(< ?in 0)(> ?in 5)) do
      (printout t "Quale soluzione scegli? inserisci il numero dell'indice, altrimenti 0 per modificare la richiesta" crlf)
      (bind ?in (read)))

    (if (<> ?in 0)
      then
       (assert(selected ?in))
      else
  			(printout t "cosa vuoi cambiare? inserisci i numeri corrispondenti"crlf
  			 "1 nome" crlf
  			 "2 numero di persone" crlf
  			 "3 regioni preferite" crlf
  			 "4 regioni da evitare" crlf
  			 "5 tipi di turismo preferiti" crlf
  			 "6 tipi di turismo da evitare" crlf
  			 "7 numero di stelle dell'albergo" crlf
  			 "8 numero di notti" crlf
  			 "9 prezzo" crlf)

  			(bind $?input (explode$(readline)))

        (loop-for-count (?i 1 (length$ $?input))
          (bind ?inp (nth$ ?i $?input))
        	(switch ?inp
    				(case 1 then (assert (toModify name)))
    				(case 2 then (assert (toModify numPeople)))
    				(case 3 then (assert (toModify regions)))
    				(case 4 then (assert (toModify notRegions)))
    				(case 5 then (assert (toModify tourismTypes)))
    				(case 6 then (assert (toModify notTourismTypes)))
    				(case 7 then (assert (toModify stars)))
    				(case 8 then (assert (toModify nights)))
    				(case 9 then (assert (toModify price)))
    			)
        )
  	)
)

(defrule modify-request
	?mod <- (toModify ?attr)
	?quest <- (question(attribute ?attr))
=>
	(modify ?quest (already-asked FALSE))
	(retract ?mod)
  (assert(resetModify))
  (bind ?*count* 1)
  (focus RESET)
)

(defrule createReservation (declare(salience -10))
  ?sel <- (selected ?num)
  (pathRank $?ranks)
  (path(pid ?pid)(price ?price))
  ?req <- (request(name ?name) (numPeople ?numPeople))
  (test(= (member$ ?pid $?ranks) ?num))
  =>
  (assert(reservation(name ?name)(numPeople ?numPeople)(path ?pid)(price ?price)))
  (assert(resetAll))

  (retract ?sel ?req)
  (printout t "Prenotazione effettuata!" crlf)
  (bind ?*count* 1)
  (focus RESET)
)

;;; ##############
;;; RESET MODULE
;;; ##############

(defmodule RESET (import MAIN ?ALL)(import QUESTIONS ?ALL)(import RULES ?ALL)(import PATH ?ALL)(export ?ALL))

(defrule resetRanks
  (or(resetAll)(resetModify))
  ?rank <- (rank $?)
  ?pathRank <- (pathRank $?)
=>
  (retract ?rank ?pathRank)
)

(defrule resetOAVs
  (or(resetAll)(resetModify))
  ?oav <- (oav)
=>
  (retract ?oav)
)

(defrule resetPaths
  (or(resetAll)(resetModify))
  ?path <- (path(pid ?pid))
  (not(reservation(path ?pid)))
=>
  (retract ?path)
)

(defrule resetStages
  (or(resetAll)(resetModify))
  ?stage <- (stage(path ?pid))
  (not(reservation(path ?pid)))
=>
  (retract ?stage)
)

(defrule resetQuestions (declare(salience -1))
  (resetAll)
  ?question <- (question(already-asked TRUE))
=>
  (modify ?question(already-asked FALSE))
)

(defrule deleteResetAll (declare(salience -10))
  ?reset <- (resetAll)
=>
  (retract ?reset)
  (bind ?year (nth$ 1 (local-time)))
  (assert(request(date 1 1 (+ 1 ?year))))
  (focus QUESTIONS RULES PATH)
)
(defrule deleteResetModify (declare(salience -10))
  ?reset <- (resetModify)
=>
  (retract ?reset)
  (focus QUESTIONS RULES PATH)
)

;1) oavPrice
;2) reset all
