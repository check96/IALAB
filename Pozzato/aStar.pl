% nodo(posizione,valore,azioni)

search(Soluzione) :-
  iniziale(S),
  aStar([nodo(S,0,[])],[],Sol), reverse(Sol,Soluzione),
  length(Soluzione, L),write(L).

% aStar(open(nodo(posizione,valore,azioni)), closed, soluzione)
aStar([nodo(S,_,Azioni)|_],_,Azioni) :- finale(S), !.                                     % restituisco la soluzione (le azioni), se il prossimo nodo da espandere è un nodo finale S
aStar([nodo(NodeMin,ValueMin,Actions)|Open], Close,Soluzione):-
    findall(Azione,applicabile(Azione,NodeMin),ListaApplicabili),                       % cerco tutte le possibili azioni applicabili al nodo NodeMin
    generaFigli(nodo(NodeMin,ValueMin,Actions),ListaApplicabili,List,Open,Close),       % eseguo le azioni, aggiungendo i nodi alla lista Open
    aStar(List,[NodeMin|Close],Soluzione).                                % aggiungo NodeMin alla lista dei visitati e continuo l'esplorazione


% generaFigli(Nodo(nodo,valore,azioni),ListaApplicabili,NewOpen,Open,Close).
generaFigli(_,[],Open,Open,_).

% se tutti i controlli vanno a buon fine nella chiusura della chiamata ricorsiva aggiunge SNuovo a newOpen
generaFigli(nodo(S,Value,AzioniPerS),[Azione|AltreAzioni],NewOpen,Open,Close):-
    trasforma(Azione,S,SNuovo,Costo),                                        % eseguo Azione e creo un nodo in una nuova posizione SNuovo
    \+member(SNuovo,Close),                                                  % controllo se SNuovo fa parte dei nodi già visitati
    NewValue is Costo + Value,                                               % calcolo il valore del costo di cammino del nuovo nodo
    inserisci(nodo(SNuovo,NewValue,[Azione|AzioniPerS]),Open,List),
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,NewOpen,List,Close).    % eseguo le azioni rimanenti

% se uno dei controlli è andato male, scarto l'azione ed eseguo le rimanenti azioni
generaFigli(nodo(S,Value,AzioniPerS),[_|AltreAzioni],NewOpen,Open,Close):-
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,NewOpen,Open,Close).

%inserisci(nodo da inserire, lista di nodi ordinati, nuova coda ordinata)
inserisci(Nodo,[],[Nodo]).

inserisci(nodo(S,Value,AzioniPerS),[nodo(S,PosValue,_)|Tail], Queue):-
	Value < PosValue,!,
	append([nodo(S,Value,AzioniPerS)],Tail,Queue).

inserisci(nodo(S,_,_),[nodo(S,PosValue,AzioniPerPos)|Tail], [nodo(S,PosValue,AzioniPerPos)|Tail]).
	

inserisci(nodo(S,Value,AzioniPerS),[nodo(Pos,PosValue,AzioniPerPos)|Tail], Queue):-
	distanza(S,D), F is D + Value,
	distanza(Pos,DPos), FPos is DPos + PosValue,
	F =< FPos, !,
	rimuoviDuplicati(S,[nodo(Pos,PosValue,AzioniPerPos)|Tail],NuovaCoda),
	append([nodo(S,Value,AzioniPerS)],NuovaCoda,Queue).

inserisci(nodo(S,Value,AzioniPerS),[nodo(Pos,PosValue,AzioniPerPos)|Tail],[nodo(Pos,PosValue,AzioniPerPos)|Open]) :-
	inserisci(nodo(S,Value,AzioniPerS),Tail,Open).


rimuoviDuplicati(_,[],[]).

rimuoviDuplicati(Pos,[nodo(Pos,_,_)|Tail],Tail):- !.

rimuoviDuplicati(Pos,[Head|Tail],[Head|Queue]):-
	rimuoviDuplicati(Pos,Tail,Queue).