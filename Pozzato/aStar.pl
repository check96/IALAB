% nodo(posizione,F_valore,G_valore,azioni)

search(Soluzione) :-
  iniziale(S),
  distanza(S,D),
  aStar([nodo(S,D,0,[])],[],Sol), reverse(Sol,Soluzione),
  length(Soluzione, L),write(L).

% aStar(open(nodo(posizione,F_valore, G_valore,azioni)), closed, soluzione)
% open è una coda ordinata in ordine decrescente sul valore di F.
aStar([nodo(S,_,_,Azioni)|_],_,Azioni) :- finale(S), !.                               % restituisco la soluzione (le azioni), se il prossimo nodo da espandere è un nodo finale S
aStar([nodo(NodeMin,F,G,Actions)|Open], Close,Soluzione):-                     % il prossimo nodo da espandere è il primo della lista Open
    findall(Azione,applicabile(Azione,NodeMin),ListaApplicabili),                       % cerco tutte le possibili azioni applicabili al nodo NodeMin
    generaFigli(nodo(NodeMin,F,G,Actions),ListaApplicabili,List,Open,Close),       % eseguo le azioni, generando i figli. Ora la lista dei nodi Open è List.
    aStar(List,[NodeMin|Close],Soluzione).                                % aggiungo NodeMin alla lista dei visitati e continuo l'esplorazione


% generaFigli(Nodo(nodo,F_valore, G_valore,azioni),ListaApplicabili,NewOpen,Open,Close).
generaFigli(_,[],Open,Open,_).

generaFigli(nodo(S,F,G,AzioniPerS),[Azione|AltreAzioni],NewOpen,Open,Close):-
    trasforma(Azione,S,SNuovo,Costo),                                        % eseguo Azione e creo un nuovo nodo in posizione SNuovo
    \+member(SNuovo,Close),                                                  % controllo se SNuovo fa parte dei nodi già visitati
    NewG is Costo + G,                                                       % calcolo il valore del costo di cammino del nuovo nodo
    distanza(SNuovo,D),                                                      % calcolo la distanza tra SNuovo e un nodo finale
    NewF is NewG + D,
    inserisci(nodo(SNuovo,NewF,NewG,[Azione|AzioniPerS]),Open,List),!,       % inserisco il nuovo nodo nella lista Open
    generaFigli(nodo(S,F,G,AzioniPerS),AltreAzioni,NewOpen,List,Close).      % eseguo le azioni rimanenti

% se uno dei controlli è andato male, scarto l'azione ed eseguo le azioni rimanenti
generaFigli(nodo(S,F,G,AzioniPerS),[_|AltreAzioni],NewOpen,Open,Close):-
    generaFigli(nodo(S,F,G,AzioniPerS),AltreAzioni,NewOpen,Open,Close).

%  inserisci(nodo(Pos,F,G,Azioni), Open, newOpen)
% se la lista Open è vuota, il Nodo viene inserito in prima poszione
inserisci(Nodo,[],[Nodo]).
% se il nodo in poszione S è già presente nella coda, si controlla se il nuovo valore F è minore di quello del nodo già in lista e, in caso, si sostituisce.
inserisci(nodo(S,F,G,AzioniPerS),[nodo(S,F_Pos,_,_)|Tail], Queue):-
	F < F_Pos,!,
	append([nodo(S,F,G,AzioniPerS)],Tail,Queue).
% se, invece F è maggiore di F_Pos, si scarta il nodo.
inserisci(nodo(S,_,_,_),[nodo(S,F_Pos,G_Pos,AzioniPerPos)|Tail], [nodo(S,F_Pos,G_Pos,AzioniPerPos)|Tail]).
% per ogni nodo in Open, si controlla se F <= F_Nodo
inserisci(nodo(S,F,G,AzioniPerS),[nodo(Pos,F_Pos,G_Pos,AzioniPerPos)|Tail], Queue):-
	F =< F_Pos, !,
	rimuoviDuplicati(S,[nodo(Pos,F_Pos,G_Pos,AzioniPerPos)|Tail],NuovaCoda),      % si controlla che il nodo non sia presente nel resto della lista con valori più alti.
	append([nodo(S,F,G,AzioniPerS)],NuovaCoda,Queue).                             % si aggiunge il nodo in testa alla lista.

% altrimenti si continua a scorrere la lista.
inserisci(nodo(S,F,G,AzioniPerS),[nodo(Pos,F_Pos,G_Pos,AzioniPerPos)|Tail],[nodo(Pos,F_Pos,G_Pos,AzioniPerPos)|Open]) :-
	inserisci(nodo(S,F,G,AzioniPerS),Tail,Open).

% rimuoviDuplicati(Pos,Open,NewOpen)
% se la lista è vuota, si restituisce.
rimuoviDuplicati(_,[],[]).
% se Pos è presente in Open, si restuisce la lista senza Pos.
rimuoviDuplicati(Pos,[nodo(Pos,_,_,_)|Tail],Tail):- !.
%altrimenti si scorre la lista.
rimuoviDuplicati(Pos,[Head|Tail],[Head|Queue]):-
	rimuoviDuplicati(Pos,Tail,Queue).
