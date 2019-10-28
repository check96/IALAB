#show partite/4.

% squadra(Nome,Nazione,Città).
squadra("Club Brugge","Belgio","Bruges").
squadra("Monaco","Francia","Monaco").
squadra("Paris Saint Germain","Francia","Parigi").
squadra("Olympic Lione","Francia","Lione").
squadra("Borussia Dortmund","Germania","Dortmund").
squadra("Schalke 04","Germania","Gelsenkirchen").
squadra("Bayern Monaco","Germania","Monaco di Baviera").
squadra("Hoffenheim","Germania","Hoffenheim Sinsheim").
squadra("AEK","Grecia","Atene").
squadra("Tottenham","Inghilterra","Londra").
squadra("Liverpool","Inghilterra","Liverpool").
squadra("Manchester United","Inghilterra","Manchester").
squadra("Manchester City","Inghilterra","Manchester").
squadra("Inter","Italia","Milano").
squadra("Roma","Italia","Roma").
squadra("Napoli","Italia","Napoli").
squadra("Juventus","Italia","Torino").
squadra("PSV Eindhoven","Olanda","Eindhoven").
squadra("Ajax","Olanda","Amsterdam").
squadra("Porto","Portogallo","Porto").
squadra("Benfica","Portogallo","Lisbona").
squadra("Viktoria Plzen","Repubblica Ceca","Plzen").
squadra("Lokomotiv Mosca","Russia","Mosca").
squadra("CSKA Mosca","Russia","Mosca").
squadra("Stella Rossa","Serbia","Belgrado").
squadra("Atletico Madrid","Spagna","Madrid").
squadra("Real Madrid","Spagna","Madrid").
squadra("Barcellona","Spagna","Barcellona").
squadra("Valencia","Spagna","Valencia").
squadra("Young Boys","Svizzera","Berna").
squadra("Galatasaray","Turchia","Instanbul").
squadra("Shaktar Donetsk","Ucraina","Donetsk").

% giornate
giornata(1..6).

% gironi
girone("A"; "B"; "C"; "D"; "E"; "F"; "G"; "H").

% creazione Gironi 	appartiene(Squadra,Girone).
4{appartiene(S,G): squadra(S,_,_)}4 :- girone(G).

% non è possibile che due squadre della stessa nazione siano nello stesso girone
:- appartiene(S1,G), appartiene(S2,G), squadra(S1,N,_), squadra(S2,N,_), S1 != S2.

% non è possibile che una squadra sia in due gironi diversi
:- appartiene(S,G1), appartiene(S,G2), G1 != G2.

% serve solo per debug
partite(S1,S2,G,Num):- partita(S1,S2,Num), appartiene(S1,G).

% creazione calendario		partita(SquadraCasa,SquadraTrasferta,Giornata).
1{ partita(S1,S2,Num) : giornata(Num) }1 :- appartiene(S1,G), appartiene(S2,G), S1 != S2.

% non è possibile che una squadra giochi più di una partita alla stessa Giornata
:- squadra(S,_,_), giornata(G), #count{ST : partita(S,ST,G)}=N, #count{SC : partita(SC,S,G)}=M, N+M != 1.

% una partita si può disputare solo una volta <==> non è possibile che la stessa partita si disputi in giornata diverse
:- partita(S1,S2,G1), partita(S1,S2,G2), G1 != G2.

% non è possibile che ogni squadra non affronti in casa un'altra squadra del girone. La partita in trasferta è calcolata in automatico.
:- appartiene(S1,G), #count{S2 : partita(S1,S2,_), appartiene(S2,G)} != 3.

% non è possibile che due squadre della stessa città giochino una partita in casa alla stessa giornata
:- partita(S1,_,G), partita(S2,_,G), S1 != S2, squadra(S1,_,C), squadra(S2,_,C).

% una squadra non può giocare più di due partite consecutive in casa
:- partita(S,_,G), partita(S,_,G+1), partita(S,_,G+2).

% una squadra non può giocare più di due partite consecutive in trasferta
:- partita(_,S,G), partita(_,S,G+1), partita(_,S,G+2).

% le partite di andata sono giocate nelle prime 3 giornate, quelle di ritorno nelle ultime 3.
% <=> non è possibile che una squadra affronti due volte un'altra squadra nelle prime 3 giornate
:- partita(S1,S2,G), partita(S2,S1,G1), G <= 3, G1 <= 3.
