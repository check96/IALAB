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
  (location(name Milano)(region lombardia))
  (location(name Novara)(region piemonte))
  (location(name Viareggio)(region toscana))
  (location(name Verona)(region veneto))
  (location(name Parma)(region emiliaromagna))
  (location(name Amalfi)(region campania))
)

(deffacts hotels
  (hotel(name CastroHotel)(stars 3)(numRooms 20)(location Castrovillari))
  (hotel(name SeaHotel)(stars 4)(numRooms 20)(location Schiavonea))
  (hotel(name SilaHotel)(stars 3)(numRooms 30)(location Camigliatello))
  (hotel(name TermeHotel)(stars 3)(numRooms 25)(location Lauria))
  (hotel(name MateHotel)(stars 3)(numRooms 30)(location Matera))
  (hotel(name BariHotel)(stars 4)(numRooms 30)(location Bari))
  (hotel(name PoliHotel)(stars 3)(numRooms 35)(location Polignano))
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
  (hotel(name queen_hotel)(stars 4)(numRooms 72)(location Milano))
  (hotel(name nuova_hotel)(stars 3)(numRooms 45)(location Novara))
  (hotel(name carnival)(stars 4)(numRooms 34)(location Viareggio))
  (hotel(name hotel_arena)(stars 3)(numRooms 45)(location Verona))
  (hotel(name prosciutto_hotel)(stars 2)(numRooms 16)(location Parma))
  (hotel(name costa_dorata)(stars 4)(numRooms 38)(location Amalfi))
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
  (distance(from Susa)(to Brenta)(distance 400))
  (distance(from Montecatini)(to Garda)(distance 265))
  (distance(from Montecatini)(to Firenze)(distance 48))
  (distance(from Montecatini)(to Bologna)(distance 107))
  (distance(from Montecatini)(to Torino)(distance 360))
  (distance(from Montecatini)(to Brenta)(distance 387))
  (distance(from Garda)(to Firenze)(distance 258))
  (distance(from Garda)(to Bologna)(distance 165))
  (distance(from Garda)(to Torino)(distance 298))
  (distance(from Garda)(to Brenta)(distance 93))
  (distance(from Firenze)(to Bologna)(distance 97))
  (distance(from Firenze)(to Torino)(distance 420))
  (distance(from Firenze)(to Brenta)(distance 367))
  (distance(from Bologna)(to Torino)(distance 331))
  (distance(from Bologna)(to Brenta)(distance 280))
  (distance(from Torino)(to Brenta)(distance 349))
  (distance(from Milano)(to Novara)(distance 50))
  (distance(from Novara)(to Torino)(distance 92))
  (distance(from Milano)(to Parma)(distance 120))
  (distance(from Viareggio)(to Firenze)(distance 95))
  (distance(from Viareggio)(to Montecatini)(distance 50))
  (distance(from Verona)(to Brenta)(distance 119))
  (distance(from Verona)(to Garda)(distance 35))
  (distance(from Verona)(to Parma)(distance 105))
  (distance(from Parma)(to Bologna)(distance 100))
  (distance(from Amalfi)(to Napoli )(distance 61))
  (distance(from Amalfi)(to Caserta)(distance 85))
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
  (score(location Lauria) (tourismType naturalistico) (score 2))
  (score(location Lauria) (tourismType termale) (score 2))
  (score(location Bari) (tourismType balneare) (score 5))
  (score(location Bari) (tourismType naturalistico) (score 2))
  (score(location Bari) (tourismType culturale) (score 3))
  (score(location Bari) (tourismType sportivo) (score 3))
  (score(location Bari) (tourismType enogastronomico) (score 4))
  (score(location Polignano) (tourismType balneare) (score 5))
  (score(location Polignano) (tourismType sportivo) (score 3))
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
  (score(location Susa) (tourismType montano) (score 4))
  (score(location Susa) (tourismType naturalistico) (score 5))
  (score(location Susa) (tourismType enogastronomico) (score 3))
  (score(location Susa) (tourismType lacustre) (score 3))
  (score(location Susa) (tourismType sportivo) (score 4))
  (score(location Montecatini) (tourismType termale) (score 5))
  (score(location Montecatini) (tourismType enogastronomico) (score 3))
  (score(location Montecatini) (tourismType culturale) (score 3))
  (score(location Garda) (tourismType lacustre) (score 5))
  (score(location Garda) (tourismType culturale) (score 2))
  (score(location Garda) (tourismType religioso) (score 2))
  (score(location Garda) (tourismType montano) (score 3))
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
  (score(location Torino) (tourismType sportivo) (score 3))
  (score(location Torino) (tourismType enogastronomico) (score 4))
  (score(location Brenta) (tourismType enogastronomico) (score 3))
  (score(location Brenta) (tourismType montano) (score 4))
  (score(location Brenta) (tourismType sportivo) (score 3))
  (score(location Brenta) (tourismType naturalistico) (score 5))
  (score(location Brenta) (tourismType lacustre) (score 3))
  (score(location Brenta) (tourismType culturale) (score 2))
  (score(location Milano) (tourismType culturale) (score 4))
  (score(location Milano) (tourismType religioso) (score 3))
  (score(location Milano) (tourismType sportivo) (score 4))
  (score(location Milano) (tourismType enogastronomico) (score 3))
  (score(location Milano) (tourismType naturalistico) (score 2))
  (score(location Novara) (tourismType culturale) (score 3))
  (score(location Novara) (tourismType religioso) (score 3))
  (score(location Novara) (tourismType enogastronomico) (score 2))
  (score(location Novara) (tourismType lacustre) (score 3))
  (score(location Viareggio) (tourismType balneare) (score 3))
  (score(location Viareggio) (tourismType culturale) (score 3))
  (score(location Viareggio) (tourismType naturalistico) (score 2))
  (score(location Viareggio) (tourismType lacustre) (score 2))
  (score(location Verona) (tourismType culturale) (score 4))
  (score(location Verona) (tourismType religioso) (score 3))
  (score(location Parma) (tourismType culturale) (score 3))
  (score(location Parma) (tourismType enogastronomico) (score 5))
  (score(location Parma) (tourismType religioso) (score 3))
  (score(location Parma) (tourismType sportivo) (score 2))
  (score(location Amalfi) (tourismType balneare) (score 5))
  (score(location Amalfi) (tourismType culturale) (score 3))
  (score(location Amalfi) (tourismType naturalistico) (score 4))
)

(defrule simmetricalDistance (declare(salience 1000))
  (distance(from ?a)(to ?b)(distance ?d))
=>
  (assert(distance(from ?b)(to ?a)(distance ?d)))
)

(defrule start
  =>
  (assert(request))
  (focus QUESTIONS RULES TRIP)
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
  (question (attribute price)
            (the-question "Budget?")
            (type INTEGER))
  (question (attribute stars)
            (the-question "Qualità dell'albergo? inserire numero minimo di stelle")
            (type INTEGER)
            (valid-answers 1 2 3 4))
  (question (attribute numPeople)
            (the-question "Quante persone siete? inserire numero ")
            (type INTEGER))
  (question (attribute date)
            (the-question "In che data vorresti partire? (dd mm yyyy)")
            (type INTEGER))
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

    (while TRUE do
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
          (assert (attribute (name ?the-attribute) (value $?value)))
          (break)
        else 
          (printout t "Hai inserito un valore non valido. Riprova." crlf)
      )
    )
)

(defrule compile
    ?attr <- (attribute (name ?n) (value $?v))
    ?r <- (request)
    (test(<>(length$ ?v) 0))
  =>
    (retract ?attr)

    (switch ?n
      (case numPeople then (modify ?r (numPeople (nth$ 1 ?v))(price 0)))
      (case regions then (modify ?r (regions ?v)))
      (case notRegions then (modify ?r (notRegions ?v)))
      (case tourismTypes then (modify ?r (tourismTypes ?v)))
      (case notTourismTypes then (modify ?r (notTourismTypes ?v)))
      (case stars then (modify ?r (stars (nth$ 1 ?v))))
      (case nights then (modify ?r (nights (nth$ 1 ?v)) (price 0)))
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
      (if (< ?budget (* ?nights 125 (div (+ ?numPeople 1) 2)))
        then
          (modify ?req (nights (div ?budget (* 125 (div (+ ?numPeople 1) 2))))))
  )

  (assert(rank (create$ ))(pathRank (create$ )))
)


;;; ###############
;;  RULES MODULE
;;; ###############

(defmodule RULES (import MAIN ?ALL)(import QUESTIONS ?ALL)(export ?ALL))

(deftemplate rule
  (slot if)
  (slot then)
  (slot not)
  (slot type)
  (slot coefficient (default 2))
)

(deftemplate oav
  (slot object)
  (slot type)
  (slot cf)
  (slot ranked (default no))
)

(deftemplate stage
  (slot path)
  (slot numPeople)
  (slot hotel)
  (slot location)
  (multislot arrivalDate)
  (multislot awayDate)
  (slot numNights (default 1))
)

(deffacts experience_rules

  (rule(if balneare) (not montano)(type tourism)(coefficient 2))
  (rule(if balneare) (then naturalistico )(type tourism)(coefficient 3))
  (rule(if balneare) (then enogastronomico)(type tourism)(coefficient 3))
  (rule(if balneare) (then sportivo)(not lacustre)(type tourism)(coefficient 4))
  (rule(if montano) (then naturalistico)(type tourism)(coefficient 2))
  (rule(if montano) (then sportivo)(type tourism)(coefficient 3))
  (rule(if montano) (then lacustre)(type tourism)(coefficient 3))
  (rule(if montano) (then religioso)(type tourism)(coefficient 4))
  (rule(if montano) (then enogastronomico)(type tourism)(coefficient 4))
  (rule(if montano) (then culturale)(type tourism)(coefficient 5))
  (rule(if lacustre) (then termale)(type tourism)(coefficient 2))
  (rule(if lacustre) (then naturalistico)(not sportivo)(type tourism)(coefficient 3))
  (rule(if naturalistico) (not religioso)(type tourism)(coefficient 3))
  (rule(if naturalistico) (not culturale)(type tourism)(coefficient 3))
  (rule(if naturalistico) (then sportivo)(type tourism)(coefficient 4))
  (rule(if naturalistico) (then enogastronomico)(type tourism)(coefficient 4))
  (rule(if naturalistico) (then termale)(type tourism)(coefficient 5))
  (rule(if termale)(not sportivo)(type tourism)(coefficient 2))
  (rule(if termale)(not culturale)(type tourism)(coefficient 3))
  (rule(if termale)(not religioso)(type tourism)(coefficient 3))
  (rule(if termale)(not enogastronomico)(type tourism)(coefficient 3))
  (rule(if culturale) (then religioso)(not sportivo)(type tourism)(coefficient 2))
  (rule(if religioso) (not sportivo)(type tourism)(coefficient 2))
  (rule(if sportivo) (not enogastronomico)(type tourism)(coefficient 3))


  (rule(if trentino)(then montano) (not balneare)(type region)(coefficient 0.1))
  (rule(if trentino) (then lacustre) (type region)(coefficient 0.04))
  (rule(if calabria) (then balneare) (type region)(coefficient 0.08))
  (rule(if calabria) (then montano) (type region)(coefficient 0.04))
  (rule(if puglia) (then balneare)(type region)(coefficient 0.1))
  (rule(if puglia) (then balneare)(type region)(coefficient 0.07))
  (rule(if campania) (then balneare)(type region)(coefficient 0.05))
  (rule(if campania) (then enogastronomico)(type region)(coefficient 0.1))
  (rule(if emiliaromagna) (then enogastronomico)(type region)(coefficient 0.1))
  (rule(if toscana) (then enogastronomico)(type region)(coefficient 0.1))
  (rule(if piemonte) (then montano) (not balneare) (type region)(coefficient 0.06))
  (rule(if piemonte) (then lacustre) (type region)(coefficient 0.03))
  (rule(if lombardia) (then lacustre) (type region)(coefficient 0.04))
  (rule(if lombardia) (then culturale)(not balneare) (type region)(coefficient 0.04))


  (rule(if balneare) (then 6)(type date)(coefficient 0.05))
  (rule(if balneare) (then 7)(type date)(coefficient 0.1))
  (rule(if balneare) (then 8)(type date)(coefficient 0.1))
  (rule(if balneare) (then 9)(type date)(coefficient 0.05))
  (rule(if montano) (then 1)(type date)(coefficient 0.1))
  (rule(if montano) (then 2)(type date)(coefficient 0.08))
  (rule(if montano) (then 7)(type date)(coefficient 0.02))
  (rule(if montano) (then 8)(type date)(coefficient 0.02))
  (rule(if montano) (then 11)(type date)(coefficient 0.08))
  (rule(if montano) (then 12)(type date)(coefficient 0.1))
  (rule(if naturalistico) (then 3)(type date)(coefficient 0.02))
  (rule(if naturalistico) (then 4)(type date)(coefficient 0.05))
  (rule(if naturalistico) (then 5)(type date)(coefficient 0.1))
  (rule(if naturalistico) (then 9)(type date)(coefficient 0.03))
  (rule(if naturalistico) (then 9)(type date)(coefficient 0.1))
  (rule(if sportivo) (then 1)(type date)(coefficient 0.1))
  (rule(if sportivo) (then 2)(type date)(coefficient 0.06))
  (rule(if sportivo) (then 7)(type date)(coefficient 0.07))
  (rule(if sportivo) (then 8)(type date)(coefficient 0.07))
  (rule(if sportivo) (then 11)(type date)(coefficient 0.06))
  (rule(if sportivo) (then 12)(type date)(coefficient 0.1))
  (rule(if lacustre) (then 4)(type date)(coefficient 0.08))
  (rule(if lacustre) (then 5)(type date)(coefficient 0.1))
  (rule(if lacustre) (then 6)(type date)(coefficient 0.08))
  (rule(if lacustre) (then 9)(type date)(coefficient 0.08))

)

(defrule simmetricalRules(declare(salience 1000))
  (rule(if ?type) (then ?type2 & ~nil)(type ?t&~date)(coefficient ?div))
=>
  (assert(rule(if ?type2)(then ?type)(type ?t)(coefficient ?div)))
)

(defrule simmetricalRulesNot(declare(salience 1000))
  (rule(if ?type) (not ?type2 & ~nil)(type ?t&~date)(coefficient ?div))
  (test(neq (type ?type2) INTEGER))
=>
  (assert(rule(if ?type2)(not ?type)(type ?t)(coefficient ?div)))
)

(defrule cf_stars
  (request(numPeople ?numPeople)(stars ?stars))
  (hotel(name ?hotel)(stars ?numStars&:(>= ?numStars ?stars))(numRooms ?numRooms&:(>= ?numRooms (div (+ ?numPeople 1) 2))))
=>
  (assert(oav(object ?hotel)(type stars)(cf (/ ?numStars 4))))
)

(defrule CF_regions
  (request(regions $?regions)(tourismTypes $?tourismTypes)(notRegions $?notRegions))
  (hotel(name ?hotel)(location ?location))
  (location(name ?location)(region ?region))
=>
  (bind ?cf 0.5)
  (if(member$ ?region $?regions)
    then (bind ?cf 0.75))

  (if(member$ ?region $?notRegions)
      then (bind ?cf 0.25))

  (do-for-all-facts ((?r rule)) (and(eq ?r:type region) (eq ?r:then ?region) (member$ ?r:if $?tourismTypes))
    (bind ?cf (+ ?cf ?r:coefficient)))

  (assert(oav(object ?hotel)(type region)(cf ?cf)))
)

(defglobal ?*value* = 0.5)
(defrule CF_tourismType
  (request(tourismTypes $?ttypes)(notTourismTypes $?notTypes))
  (hotel(name ?hotel)(location ?location))
  (location(name ?location)(region ?region))
=>
  (do-for-all-facts ((?s score)) (eq ?s:location ?location)
    (if(member$ ?s:tourismType $?ttypes) then
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes)))))
    (if(member$ ?s:tourismType $?notTypes) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?notTypes)))))
  )
  (do-for-all-facts ((?s score)(?r rule)) (and(eq ?s:location ?location)(eq ?r:type tourism))
    (if(and(member$ ?r:if $?ttypes)(eq ?r:then ?s:tourismType)) then
      (bind ?*value* (+ ?*value* (/ (* 0.1 ?s:score) (length$ $?ttypes) ?r:coefficient ))))
    (if(and(member$ ?r:if $?ttypes)(eq ?r:not ?s:tourismType)) then
      (bind ?*value* (- ?*value* (/ (* 0.1 (- 6 ?s:score)) (length$ $?ttypes) ?r:coefficient))))
  )

  (do-for-all-facts ((?s score)(?r rule)) (and(eq ?s:location ?location)(eq ?r:type tourism)(eq ?r:if ?region)(eq ?s:tourismType ?r:then))
    (bind ?*value* (+ ?*value* ?r:coefficient)))

  (assert(oav(object ?hotel)(type tourismType)(cf ?*value*)))
  (bind ?*value* 0.5)
)

(defrule CF_Used
  (request)
  (hotel (name ?hotel))
=>
  (bind ?value 1)
  (do-for-all-facts ((?s stage)) (eq ?s:hotel ?hotel)
    (bind ?value (* ?value 0.95)))
  (assert(oav(object ?hotel)(type used) (cf ?value)))
)

(defrule combineCF (declare(salience -10))
  ?oavR <- (oav(object ?hotel)(type region)(cf ?cfR))
  ?oavT <- (oav(object ?hotel)(type tourismType)(cf ?cfT))
  ?oavS <- (oav(object ?hotel)(type stars)(cf ?cfS))
  ?oavU <- (oav(object ?hotel)(type used)(cf ?cfU))
  =>
    (retract ?oavR ?oavT ?oavS)
    (bind ?cf (* ?cfU (+ (* 0.45 ?cfR) (* 0.45 ?cfT) (* 0.1 ?cfS))))
;    (printout t ?hotel " (R: " ?cfR ", T: " ?cfT ", U: "?cfU ", S: "?cfS ")  ---> " ?cf crlf)
    (modify ?oavU(type all)(cf ?cf))
)

(defrule createRanking (declare (salience -50))
  ?oav <- (oav(object ?hotel)(type all)(cf ?cf)(ranked no))
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
;;; TRIP MODULE
;;; #############

(defmodule TRIP (import MAIN ?ALL)(import QUESTIONS ?ALL)(import RULES ?ALL)(export ?ALL))

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

(deftemplate reservation
  (slot path)
  (slot numPeople)
  (slot price)
)

(deffacts reservations  
  (reservation(path 97) (numPeople 15) (price 6400))
    (stage (path 97) (numPeople 15) (hotel BariHotel) (location Bari) (arrivalDate 20 2 2020) (awayDate 24 2 2020) (numNights 4))
    (stage (path 97) (numPeople 15) (hotel PoliHotel) (location Polignano) (arrivalDate 24 2 2020) (awayDate 27 2 2020) (numNights 3))
  (reservation(path 98) (numPeople 1) (price 875))
    (stage (path 98) (numPeople 1) (hotel BariHotel) (location Bari) (arrivalDate 24 2 2020) (awayDate 27 2 2020) (numNights 4))
    (stage (path 98) (numPeople 1) (hotel PoliHotel) (location Polignano) (arrivalDate 27 2 2020) (awayDate 29 2 2020) (numNights 3))
  (reservation (path 2) (numPeople 6) (price 2250))
    (stage (path 2) (numPeople 6) (hotel Grand_Hotel_Tettuccio) (location Montecatini) (arrivalDate 12 12 2020) (awayDate 14 12 2020) (numNights 2))
    (stage (path 2) (numPeople 6) (hotel Vinaio) (location Firenze) (arrivalDate 14 12 2020) (awayDate 16 12 2020) (numNights 2))
    (stage (path 2) (numPeople 6) (hotel Hotel_Cosmopolitan) (location Bologna) (arrivalDate 16 12 2020) (awayDate 18 12 2020) (numNights 2))
)

(deffunction getMonths ($?months)
  (bind $?month (create$ 1))
  (bind ?max (nth$ 1 ?months))

  (loop-for-count (?i 2 12) do
    (if (> (nth$ ?i ?months) ?max)
      then
        (bind $?month (create$ ?i))
        (bind ?max (nth$ ?i ?months))
      else (if (= (nth$ ?i ?months) ?max)
        then
          (bind $?month (create$ $?month ?i)))))
$?month)

(deffunction chooseDate($?months)
  (bind ?day (+ (mod (random) 28) 1))
  (bind $?m (getMonths $?months))
  (bind ?index (+ (mod (random) (length$ $?m)) 1))
  (return (create$ ?day (nth$ ?index $?m) 2020))
)

(deffunction replace(?index ?value $?array)
  (bind $?new (create$))
  (loop-for-count (?i 1 (- ?index 1)) do
    (bind $?new (create$ $?new (nth$ ?i $?array))))
  (bind $?new (create$ $?new ?value))
  (loop-for-count (?i (+ ?index 1) 12) do
    (bind $?new (create$ $?new (nth$ ?i $?array))))
  (return $?new)
)

(defglobal ?*months* = (create$ 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5))

(defrule CF_date (declare(salience 510))
  (request(tourismTypes $? ?ttype $?))
  (rule(if ?ttype)(then ?month)(coefficient ?c))
  (test(eq (type ?month) INTEGER))
=>
  (bind ?value (+ ?c (nth$ ?month ?*months*)))
  (bind ?*months* (replace ?month ?value ?*months*))
)

(defrule date (declare(salience 500))
  ?req <- (request(date $?date))
  (test(= (length$ $?date) 0))
=>
  (modify ?req(date (chooseDate ?*months*)))
  (bind ?*months* (create$ 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5))
)

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
  (assert(path(pid ?pid)(hotels ?hotel)(locations ?location)(cfHotels ?cf)))
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

  (distance(from ?loc)(to ?loc1)(distance ?d&:(<= ?d 120)))
=>
 ; (bind ?cf (/ (+ (* ?cfPath (length$ $?hotels)) ?cfHotel) (+ 1 (length$ $?hotels))))
  (modify ?path(hotels $?hotels ?hotel1)(locations $?locations ?loc1) (cfHotels $?cfHotels ?cfHotel))
)

(defrule normalize (declare(salience 89))
  (rank $?ranks)
  ?path <- (path (pid ?pid)(hotels $?hotels)(cfHotels $?cfHotels))
=>
  (bind ?cf 0)
  (bind ?den 0)
  (do-for-all-facts((?h hotel)) (member$ ?h:name $?hotels)
      (bind ?pos (member$ ?h:name $?ranks))
      (bind ?norm (- 1 (/ (- ?pos 1) (length$ $?ranks))))
      (bind ?cf (+ ?cf (* ?norm (nth$ (member$ ?h:name $?hotels) $?cfHotels))))
      (bind ?den (+ ?den ?norm))    
    )

    (modify ?path(cfPath (/ ?cf ?den)))
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

(defrule make-stages (declare(salience 70))
  (request(numPeople ?numPeople)(nights ?nights)(date $?date))
  (path (pid ?pid)(hotels $?hotels)(locations $?locations)(cfHotels $?cfs)(ranked no))

=>
  (bind $?days (dayForHotel ?nights $?cfs))

  (loop-for-count (?i 1 (length$ $?hotels))
    (assert(stage(numPeople ?numPeople)(path ?pid)(hotel (nth$ ?i $?hotels))(location (nth$ ?i $?locations))(arrivalDate $?date)(awayDate (calculateAwayDate (nth$ ?i $?days) $?date)) (numNights (nth$ ?i $?days))))
    (bind $?date (calculateAwayDate (nth$ ?i $?days) $?date)))
)

(deffunction verifyRooms(?numPeople ?hotel ?numRooms $?dates)
    (bind ?sum 0)
    (do-for-all-facts((?s stage)(?r reservation))(and(eq ?s:hotel ?hotel)(eq ?r:path ?s:path))
      (bind ?arrive (convertDate ?s:arrivalDate))
      (bind ?away (convertDate ?s:awayDate))
      (bind ?arrivalDate (convertDate (create$ (nth$ 1 $?dates)(nth$ 2 $?dates)(nth$ 3 $?dates))))
      (bind ?awayDate (convertDate (create$ (nth$ 4 $?dates)(nth$ 5 $?dates)(nth$ 6 $?dates))))
   
      (if(not(or(and(<= ?arrivalDate ?arrive)(<= ?awayDate ?arrive)) (and(>= ?arrivalDate ?away)(> ?awayDate ?away))))
        then
          (bind ?sum (+ ?sum ?s:numPeople)))
    )
    (return (< (- ?numRooms ?sum) (div (+ ?numPeople 1) 2)))
)

(defrule verifyAvailable (declare(salience 60))
  (request(numPeople ?numPeople))
  ?path <- (path (pid ?pid)(ranked no))
  ?stage <- (stage (path ?pid)(hotel ?hotel)(arrivalDate $?date)(awayDate $?away))
  (hotel(name ?hotel)(numRooms ?numRooms))
  (not(calculated ?pid))
=>
  (if(verifyRooms ?numPeople ?hotel ?numRooms (create$ $?date $?away)) then
    (retract ?stage ?path))
)

(defrule pruning-samePath (declare(salience 50))
  (path(pid ?pid)(hotels $?hotels)(ranked no))
  ?path <- (path(pid ?pid1 & ~?pid)(hotels $?hotels1)(ranked no))

  (test(=(length$ $?hotels) (length$ $?hotels1)))
  (test(subsetp $?hotels $?hotels1))
  =>
  (retract ?path)
  (do-for-all-facts((?s stage))  (eq ?s:path ?pid1)
    (retract ?s))
)


(defrule delete-for-num (declare(salience 55))
  (request (spostamenti molto) (minLocations ?min) (nights ?nights&:(>= ?nights 7)))
  ?path <- (path (locations $?loc))
  (test (< (length$ $?loc) ?min ))

  =>

  (retract ?path)
)


(defrule cf_Distances (declare(salience 40))
  (path(pid ?pid)(locations $?locations)(cfPath ?cf)(ranked no))
  (not(calculated ?pid))
=>
  (bind ?value 0)
  (loop-for-count (?i 1 (- (length$ $?locations) 1)) do
    (do-for-all-facts ((?d distance)) (and(eq ?d:from (nth$ ?i $?locations))(eq ?d:to (nth$ (+ 1 ?i) $?locations)))
      (bind ?value (+ ?value (- 100 ?d:distance))))
  )

  (bind ?cfD (/ (- 100 (/ ?value (length$ $?locations))) 1000))
  (assert(oav(object ?pid)(type distances)(cf ?cfD)))
)

(defrule cf_Price (declare(salience 30))
  (request(numPeople ?numPeople)(price ?budget)(nights ?nights))
  ?path <- (path(pid ?pid)(ranked no)(price nil))
  (not(calculated ?pid))
=>
  (bind ?price 0)
  (do-for-all-facts ((?s stage) (?h hotel)) (and(eq ?s:path ?pid)(eq ?s:hotel ?h:name))
    (bind ?price (+ ?price (* ?s:numNights 25 (+ ?h:stars 1) (div (+ ?numPeople 1) 2)))))


  (if (<= ?price ?budget)
    then
      (assert(oav(object ?pid)(type price)(cf (/ ?price ?budget))))
      (modify ?path(price ?price))
    else
      (if (<= ?price (+ ?budget(* 30 ?numPeople)))
        then
          (assert(oav(object ?pid)(type price)(cf (- (/ ?price ?budget) (/ (- ?price ?budget) (* ?numPeople 200))))))
        else
          (retract ?path)))
)

(defrule combineOAV
  ?oavD <- (oav(object ?pid)(type distances)(cf ?cfD))
  ?oavP <- (oav(object ?pid)(type price)(cf ?cfP))
  ?path <- (path(pid ?pid)(cfPath ?cfPath)(ranked no))
=>
  (retract ?oavD ?oavP)
  (bind ?cf (- (+ (* 0.6 ?cfPath) (* 0.4 ?cfP)) ?cfD))
;  (printout t "path " ?pid "  cfD: " ?cfD "   ?cfPath: " ?cfPath "  cfPrice: " ?cfP "  ---> " ?cf crlf)
  (modify ?path(cfPath ?cf))
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
      (printout t ?*count* " -   " ?path "  -->  " ?price "€." crlf)
      (do-for-all-facts ((?s stage)) (eq ?s:path ?path)
        (printout t "    " ?s:hotel " in " ?s:location "  " ?s:arrivalDate "     " ?s:awayDate crlf))
      (bind ?*count* (+ 1 ?*count*))
    else
      (retract ?p))
)

(defrule choosePath (declare(salience -110))
  (pathRank $?paths)
  =>
    (if (= ?*count* 1)
      then
        (printout t "Non è stato possibile creare un itinerario di viaggio con le informazioni a disposizione. Premere 0 per modificare la richiesta" crlf)
      else
        (printout t "Quale soluzione scegli? inserisci il numero dell'indice, altrimenti 0 per modificare la richiesta" crlf)
    )
   
    (bind ?in (read))
    (while(or(neq (type ?in) INTEGER)(< ?in 0)(> ?in 5)) do
      (printout t "Hai inserito un valore errato. Riprova." crlf)
      (bind ?in (read)))

    (if (<> ?in 0)
      then
       (assert(selected ?in))
      else
        (printout t "cosa vuoi cambiare? inserisci i numeri corrispondenti" crlf
         "1 numero di persone" crlf
         "2 regioni preferite" crlf
         "3 regioni da evitare" crlf
         "4 tipi di turismo preferiti" crlf
         "5 tipi di turismo da evitare" crlf
         "6 numero di stelle dell'albergo" crlf
         "7 numero di notti" crlf
         "8 budget" crlf
         "9 data di partenza" crlf
         "10 preferenza sugli spostamenti" crlf)

        (while TRUE do
            (bind $?input (explode$(readline)))
            (bind ?wrong FALSE)
            (loop-for-count (?i 1 (length$ $?input))
              (bind ?inp (nth$ ?i $?input))
              (if(or(neq (type ?inp) INTEGER) (< ?inp 1)(> ?inp 10))
                then
                  (bind ?wrong TRUE)
                  (break))
            )
            (if (eq ?wrong TRUE)
              then
                (printout t "Hai inserito dei valori errati. Riprova" crlf)
              else
                (break))
        )

        (loop-for-count (?i 1 (length$ $?input))
            (bind ?inp (nth$ ?i $?input))
        (switch ?inp
        (case 1 then (assert (toModify numPeople)))
        (case 2 then (assert (toModify regions)))
        (case 3 then (assert (toModify notRegions)))
        (case 4 then (assert (toModify tourismTypes)))
        (case 5 then (assert (toModify notTourismTypes)))
        (case 6 then (assert (toModify stars)))
        (case 7 then (assert (toModify nights)))
        (case 8 then (assert (toModify price)))
        (case 9 then (assert (toModify date)))
        (case 10 then (assert (toModify spostamenti)))
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

(defrule createReservation (declare(salience -120))
  ?sel <- (selected ?num)
  (pathRank $?ranks)
  ?req <- (request(numPeople ?numPeople))

=>
  (assert(reservation(numPeople ?numPeople)(path (nth$ ?num $?ranks)))
       (resetAll))

  (retract ?sel ?req)
  (printout t "Prenotazione effettuata!" crlf)
  (bind ?*count* 1)
  (focus RESET)
)

;;; ##############
;;; RESET MODULE
;;; ##############

(defmodule RESET (import MAIN ?ALL)(import QUESTIONS ?ALL)(import RULES ?ALL)(import TRIP ?ALL)(export ?ALL))

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

(defrule deleteCalculated
  ?c <- (calculated)
  =>
  (retract ?c)
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
  (focus QUESTIONS RULES TRIP)
)

(defrule deleteResetModify (declare(salience -10))
  ?reset <- (resetModify)
=>
  (retract ?reset)
  (focus QUESTIONS RULES TRIP)
)

