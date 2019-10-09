% nodo(posizione,valore,azioni)

search(Soluzione) :-
  iniziale(S),
  aStar([nodo(S,0,[])],nodo(S,0,[]),[],Sol), reverse(Sol,Soluzione).

% aStar(open(nodo(posizione,valore,azioni)), nodeMin, closed, soluzione)
aStar(_,nodo(S,_,Azioni),_,Azioni) :- finale(S), !.                                          % restituisco la soluzione (le azioni), se il prossimo nodo da espandere è un nodo finale S
aStar(Open,nodo(NodeMin,ValueMin,Actions), Close,Soluzione):-
    findall(Azione,applicabile(Azione,NodeMin),ListaApplicabili),                               % cerco tutte le possibili azioni applicabili al nodo NodeMin
    generaFigli(nodo(NodeMin,ValueMin,Actions),ListaApplicabili,ListaFigli,Open,Close),         % eseguo le azioni
    select(nodo(NodeMin,ValueMin,Actions),Open,NewOpen),                                        % elimino NodeMin dalla lista Open
    append(NewOpen,ListaFigli,NuovaCoda),                                                       % aggiungo a open la lista dei figli generati precedentemente
    min(NuovaCoda,NewNodeMin),                                                                  % calcolo il nuovo nodeMin, cioè il nodo con l'euristica migliore (distanza minore), tra quelli presenti in NuovaCoda
    aStar(NuovaCoda,NewNodeMin,[NodeMin|Close],Soluzione).                                      % aggiungo NodeMin alla lista dei visitati e continuo l'esplorazione


% generaFigli(Nodo(nodo,valore,azioni),ListaApplicabili,ListaFigli,Open,Close).
generaFigli(_,[],[],_,_).

generaFigli(nodo(S,Value,AzioniPerS),[Azione|AltreAzioni],[nodo(SNuovo,NewValue,[Azione|AzioniPerS])|FigliTail],Open,Close):-
    trasforma(Azione,S,SNuovo,Costo),                                             % eseguo Azione e creo un nodo in una nuova posizione SNuovo
    NewValue is Costo + Value,                                                    % calcolo il valore dell'euristica del nuovo nodo
    checkOpen(nodo(SNuovo,NewValue,[Azione|AzioniPerS]),Open, NewOpen),           % controllo se il nodo S è presente nella lista open. vedi sotto
    \+member(SNuovo,Close),!,                                                     % controllo se SNuovo non fa parte dei nodi già visitati
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,FigliTail,NewOpen,Close).    % eseguo le azioni rimanenti

% se uno dei controlli è andato male, scarto l'azione ed eseguo le rimanenti azioni
generaFigli(nodo(S,Value,AzioniPerS),[_|AltreAzioni],FigliTail,Open,Close):-
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,FigliTail,Open,Close).

/* controlla se il nodo S è presente nella lista open.
   restiuisce:
   - la lista open se il nodo non è presente in essa
   - una nuova lista identica a open ma con il nodo aggiornato, se il nodo è presente nella lista ma con un valore più alto
   - false altrimenti.

   checkOpen(Nodo,Open,NewOpen)
*/
% se la lista è vuota, restituisce la lista vuota. CASO BASE
checkOpen(_,[],[]) :- !.
% se il nodo è presente nella lista ma con un valore maggiore di quello del nuovo nodo, aggiorno il nodo e restituisco la lista aggiornata.
checkOpen(nodo(S,Value,Azioni), [nodo(S,ListValue,_)|Tail], [nodo(S,Value,Azioni)|Tail]) :-
  Value < ListValue, !.
% altrimenti se è presente ma con un valore minore o uguale, restituisco false.
checkOpen(nodo(S,_,_), [nodo(S,Value,Actions)|Tail], [nodo(S,Value,Actions)|Tail]) :- !, false.
% continuo a scorrere la lista open, spostando la head nella nuova lista.
checkOpen(Nodo,[Head|Tail],[Head|NewList]) :- checkOpen(Nodo,Tail,NewList).

/*
    controlla se il nodo S è presente nella lista close.
    Se è presente con un valore maggiore rispetto a quello di S, S viene rimosso da close e spostato in Open con il valore aggiornato

    checkClose(Nodo,Open,Close)
*/
checkClose(_,[],[]) :- !.




/* calcolo del valore minimo dei nodi nella lista open
  min(lista,minimo)

  aux inizia con (listaSenzaPrimoELemento, primoElemento, EuristicaPrimoELemento, nodoMinimo)
  nodoMinimo è l'output.
*/

min([nodo(S,Value,Azioni)|Tail],Minimo) :-
  distanza(S,D), Euristica is D + Value, aux(Tail,nodo(S,Value,Azioni),Euristica,Minimo).

% aux(lista,nodoMinimoTemporaneo,EuristicaMinima, nodoMinimo)
% se la lista è vuota, restituisco il nodoMinimoTemporaneo
aux([],TmpNodeMin,_,TmpNodeMin).

% se nella lista c'è un solo elemento, restituisco l'elemento se la sua euristica è minore dell'EuristicaMinima.
aux([nodo(S,Value,Azioni)], _, EuristicaMinima, nodo(S,Value,Azioni)) :-
  distanza(S,D), EuristicaS is D + Value, EuristicaS < EuristicaMinima, !.

% cerco il minimo della sottosequenza a destra e lo valuto rispetto a S. Se l'euristica di S è minore dell'euristica trovata nella sottosequenza a destra restituisco S.
aux([nodo(S,Value,Azioni)|Tail], TmpNodeMin , EuristicaMinima , nodo(S,Value,Azioni)) :-
  aux(Tail, TmpNodeMin, EuristicaMinima, _),
  distanza(S,D), EuristicaS is D + Value, EuristicaS < EuristicaMinima, !.

% altrimenti continuo a scorre la lista
aux([_|Tail],TmpNodeMin,EuristicaMinima,NodeMin) :-
  aux(Tail,TmpNodeMin,EuristicaMinima,NodeMin).

aux(_,NodeMin,_,NodeMin).
