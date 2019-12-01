;;; #############
;;; TRIP MODULE
;;; #############

(defmodule TRIP (import MAIN ?ALL)(import RULES ?ALL)(export ?ALL))

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
  (slot name)
  (slot path)
  (slot numPeople)
  (slot price)
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

  (distance(from ?loc)(to ?loc1)(distance ?d&:(<= ?d 120)))
=>
  (bind ?cf (/ (+ (* ?cfPath (length$ $?hotels)) ?cfHotel) (+ 1 (length$ $?hotels))))
  (modify ?path(hotels $?hotels ?hotel1)(locations $?locations ?loc1) (cfHotels $?cfHotels ?cfHotel) (cfPath ?cf))
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
    (printout t "Quale soluzione scegli? inserisci il numero dell'indice, altrimenti 0 per modificare la richiesta" crlf)
    (bind ?in (read))
  	(while(or(< ?in 0)(> ?in 5)) do
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
  			 "8 budget" crlf)

        (while TRUE do
            (bind $?input (explode$(readline)))
            (bind ?wrong FALSE)
            (loop-for-count (?i 1 (length$ $?input))
              (bind ?inp (nth$ ?i $?input))
              (if(or(< ?inp 1)(> ?inp 8))
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
  ?req <- (request(numPeople ?numPeople))
  (test(= (member$ ?pid $?ranks) ?num))
=>
  (assert(reservation(numPeople ?numPeople)(path ?pid)(price ?price)))
  (assert(resetAll))

  (retract ?sel ?req)
  (printout t "Prenotazione effettuata!" crlf)
  (bind ?*count* 1)
  (focus RESET)
)
