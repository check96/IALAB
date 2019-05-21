% nodo(nodo,valore,azioni)

search(Soluzione) :-
    iniziale(S),
    aStar([nodo(S,0,[])],[],Sol), reverse(Sol,Soluzione).

% aStar(open(nodo(posizione,valore,azioni)),closed,soluzione)
aStar(Open,_,Azioni) :- finale(S), member(nodo(S,_,Azioni),Open), !.        % restituisco la soluzione (le azioni), se un nodo finale S è presente in open
aStar(Open,Close,Soluzione):-
    min(Open,nodo(NodeMin,ValueMin,Actions)),                                                   % calcolo il nodo con l'euristica migliore (distanza minore), tra quelli presenti in Open
    findall(Azione,applicabile(Azione,NodeMin),ListaApplicabili),                               % cerco tutte le possibili azioni applicabili al nodo NodeMin (il nodo calcolato al punto precedente)
    generaFigli(nodo(NodeMin,ValueMin,Actions),ListaApplicabili,ListaFigli,Open,Close),         % eseguo le azioni
    delete(Open,nodo(NodeMin,ValueMin,Actions),NewOpen),                                        % elimino NodeMin dalla lista Open
    append(NewOpen,ListaFigli,NuovaCoda),                                                       % aggiungo a open la lista dei figli generati precedentemente
    aStar(NuovaCoda,[NodeMin|Close],Soluzione).                                                 % aggiungo NodeMin alla lista dei visitati e continuo l'esplorazione

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
*/
% se la lista è vuota, torna la lista vuota. CASO BASE
checkOpen(Nodo,[],[]).
% se il nodo è presente nella lista ma con un valore maggiore di quello del nuovo nodo, aggiorno il nodo e restituisco la lista aggiornata.
checkOpen(nodo(S,Value,Azioni), [nodo(S,ListValue,ListActions)|Tail], [nodo(S,Value,Azioni)|Tail]) :-
  Value < ListValue, !.
% altrimenti se è presente ma con un valore minore o uguale, restituisco false.
checkOpen(nodo(S,_,_), [nodo(S,Value,Actions)|Tail], [nodo(S,Value,Actions)|Tail]) :- false.
% continuo a scorrere la lista open, spostando la head nella nuova lista.
checkOpen(Nodo,[Head|Tail],NewList) :- checkOpen(Nodo,Tail,[Head|NewList]).

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
