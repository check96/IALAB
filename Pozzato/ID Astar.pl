search(Soluzione) :- statistics(walltime, [_ | [_]]),
   iniziale(S), iterativeDeepening(nodo(S,0),Soluzione,0),
   statistics(walltime, [_ | [ExecutionTime]]),
   write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

valuta(nodo(S,G),F) :- distanza(S,H), F is G + H.

iterativeDeepening(nodo(S,G),Soluzione,Soglia) :-
    dfsLimitata(nodo(S,G),Soluzione,[S],Soglia), !.

iterativeDeepening(S,Soluzione,_):-
    retract(overbound(X)),
    min_list(X,NuovaSoglia),
    iterativeDeepening(S,Soluzione,NuovaSoglia).

dfsLimitata(nodo(S,_),[],_,_) :- finale(S).
dfsLimitata(nodo(S,G),[Azione|AzioniTail],Visitati,Soglia):-
    valuta(nodo(S,G),F),
    \+ check(F,Soglia), !,
    applicabile(Azione,S),
    trasforma(Azione,S,SNuovo,Costo),
    \+member(SNuovo,Visitati),
    GValue is G + Costo,
    dfsLimitata(nodo(SNuovo,GValue),AzioniTail,[SNuovo|Visitati],Soglia).

check(F,Soglia):-
    F > Soglia,
    retract(overbound(X)),!,
    assert(overbound([F|X])).

check(F,Soglia):-
    F > Soglia,
    assert(overbound([F])).
