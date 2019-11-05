% nodo(posizione,valore,azioni)

search(Soluzione) :-
  iniziale(S),
  aStar([nodo(S,0,[])],nodo(S,0,[]),[],Sol), reverse(Sol,Soluzione).

% aStar(open(nodo(posizione,valore,azioni)), nodeMin, closed, soluzione)
aStar(_,nodo(S,_,Azioni),_,Azioni) :- finale(S), !.                                     % restituisco la soluzione (le azioni), se il prossimo nodo da espandere è un nodo finale S
aStar(Open,nodo(NodeMin,ValueMin,Actions), Close,Soluzione):-
    findall(Azione,applicabile(Azione,NodeMin),ListaApplicabili),                       % cerco tutte le possibili azioni applicabili al nodo NodeMin
    generaFigli(nodo(NodeMin,ValueMin,Actions),ListaApplicabili,List,Open,Close),       % eseguo le azioni, aggiungendo i nodi alla lista Open
    select(nodo(NodeMin,ValueMin,Actions),List,NewOpen),                                % elimino NodeMin dalla lista Open
    min(NewOpen,NewNodeMin),                                                            % calcolo il nuovo nodeMin, cioè il nodo con l'euristica migliore (distanza minore), tra quelli presenti in NuovaCoda
    aStar(NewOpen,NewNodeMin,[NodeMin|Close],Soluzione).                                % aggiungo NodeMin alla lista dei visitati e continuo l'esplorazione


% generaFigli(Nodo(nodo,valore,azioni),ListaApplicabili,NewOpen,Open,Close).
generaFigli(_,[],Open,Open,_).

% se tutti i controlli vanno a buon fine nella chiusura della chiamata ricorsiva aggiunge SNuovo a newOpen
generaFigli(nodo(S,Value,AzioniPerS),[Azione|AltreAzioni],[nodo(SNuovo,NewValue,[Azione|AzioniPerS])|NewOpen],Open,Close):-
    trasforma(Azione,S,SNuovo,Costo),                                        % eseguo Azione e creo un nodo in una nuova posizione SNuovo
    NewValue is Costo + Value,                                               % calcolo il valore del costo di cammino del nuovo nodo
    \+member(SNuovo,Close),                                                  % controllo se SNuovo fa parte dei nodi già visitati
    checkOpen(nodo(SNuovo,NewValue,[Azione|AzioniPerS]),Open, List),!,       % controllo se il nodo S è presente nella lista open. vedi sotto
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,NewOpen,List,Close).    % eseguo le azioni rimanenti

% se uno dei controlli è andato male, scarto l'azione ed eseguo le rimanenti azioni
generaFigli(nodo(S,Value,AzioniPerS),[_|AltreAzioni],NewOpen,Open,Close):-
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,NewOpen,Open,Close).

/* controlla se il nodo S è presente nella lista open.
   restiuisce:
   - la lista open se il nodo non è presente in essa
   - la lista open senza il nodo, se questo è presente nella lista ma con un valore più alto
   - false altrimenti.

   checkOpen(Nodo,Open,NewOpen)
*/
% se la lista è vuota, restituisce la lista vuota. CASO BASE
checkOpen(_,[],[]) :- !.
% se il nodo è presente nella lista ma con un valore maggiore di quello del nuovo nodo, non lo aggiungo alla nuova lista (cosi facendo di fatto lo eliminiamo dalla lista Open)
checkOpen(nodo(S,Value,_), [nodo(S,ListValue,_)|Tail], Tail) :-
  Value < ListValue, !.
% altrimenti se è presente ma con un valore minore o uguale, restituisco false.
checkOpen(nodo(S,_,_), [nodo(S,Value,Actions)|Tail], [nodo(S,Value,Actions)|Tail]) :- !, false.
% continuo a scorrere la lista open, spostando la head nella nuova lista.
checkOpen(Nodo,[Head|Tail],[Head|NewList]) :- checkOpen(Nodo,Tail,NewList).


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
aux([nodo(S,Value,Azioni)|Tail], TmpNodeMin , EuristicaMinima, nodo(S,Value,Azioni)) :-
  aux(Tail, TmpNodeMin, EuristicaMinima, _),
  distanza(S,D), EuristicaS is D + Value, EuristicaS < EuristicaMinima, !.

aux(_,NodeMin,_,NodeMin).
