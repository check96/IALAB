search(Soluzione) :- iniziale(S), distanza(S,D), dfs([nodo(S,0,[])],Soluzione,[],D,[]).

% dfs(ListaNodi,Soluzione,Visitati,Soglia,OverBound)
dfs([],Soluzione,_,_,OverBound) :-
  iniziale(S),                                          % cerca il nodo iniziale
  min_list(OverBound,NuovaSoglia),                      % calcola la nuova soglia, che è uguale al valore del minimo tra gli OverBound
  dfs([nodo(S,0,[])],Soluzione,[],NuovaSoglia,[]).      % inizia la ricerca

% se il nodo S è finale, restiuisce le azioni che hanno portato a S
dfs([nodo(S,_,Azioni)|_],Azioni,_,_,_) :- finale(S).

% se il valore dell'euristica del nodo S è minore della soglia, lo espando
dfs([nodo(S,Value,AzioniPerS)|FigliTail],Soluzione,Visitati,Soglia,OverBound) :-
  distanza(S,D), Tmp is D + Value, Tmp =< Soglia,!,                               % controlla se il valore dell'euristica del nodo S è minore della soglia
  findall(Azione,applicabile(Azione,S),ListaApplicabili),                         % cerco tutte le possibili azioni applicabili al nodo S
  generaFigli(nodo(S,Value,AzioniPerS),ListaApplicabili,ListaFigli,Visitati),     % eseguo le azioni (espando il nodo S)
  unisci(ListaFigli,FigliTail,Figli),!,                                           % metto la lista dei figli in testa alla lista dei nodi da visitare
  dfs(Figli,Soluzione,[S|Visitati],Soglia,OverBound).                             % metto S nei visitati e continuo la ricerca

% altrimenti, metto il suo valore negli OverBound e espando il nodo successivo
dfs([nodo(S,Value,_)|FigliTail],Soluzione,Visitati,Soglia,OverBound) :-
    distanza(S,D), Tmp is D + Value,
    dfs(FigliTail,Soluzione,Visitati,Soglia,[Tmp|OverBound]).

% generaFigli(nodo(Nodo,Value,Azioni), Azioni, ListaFigli, Visitati)
generaFigli(_,[],[],_).

generaFigli(nodo(S,Value,AzioniPerS),[Azione|AltreAzioni],[nodo(SNuovo,NewValue,[Azione|AzioniPerS])|FigliTail],Visitati):-
    trasforma(Azione,S,SNuovo,Costo),                                         % eseguo Azione e creo un nodo in una nuova posizione SNuovo
    NewValue is Costo + Value,                                                % calcolo il valore dell'euristica del nuovo nodo
    \+member(SNuovo,Visitati),!,                                              % controllo se il nodo non è già stato visitato
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,FigliTail,Visitati).     % eseguo le azioni rimanenti

% se uno dei controlli è andato male, scarto l'azione ed eseguo le rimanenti azioni
generaFigli(nodo(S,Value,AzioniPerS),[_|AltreAzioni],FigliTail,Visitati):-
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,FigliTail,Visitati).

% unisci(ListaA, ListaB, ListaUnione)
unisci([],B,B).
unisci(A,[],A).
unisci([X|TailA],B,Unione) :-
   member(X,B),!,
   unisci(TailA,B,Unione).
unisci([X|TailA],B,[X|Unione]) :- unisci(TailA,B,Unione).
