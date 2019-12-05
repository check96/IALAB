search(Soluzione) :-
   iniziale(S), iterativeDeepening(nodo(S,0),Soluzione,1),length(Soluzione,L),write(L).

iterativeDeepening(nodo(S,G),Soluzione,Soglia) :-
    assert(overbound(999999999)),
    dfsLimitata(nodo(S,G),Soluzione,[S],Soglia), !.

iterativeDeepening(S,Soluzione,_):-
    retract(overbound(X)),
    X \== 999999999,
    iterativeDeepening(S,Soluzione,X).

dfsLimitata(nodo(S,_),[],_,_) :- finale(S),!.
dfsLimitata(nodo(S,G),[Azione|AzioniTail],Visitati,Soglia):-
%  write(Soglia),write("  "),write(S),write("  "),
    valuta(nodo(S,G),F),
    \+check(F,Soglia),!,
    applicabile(Azione,S),
    trasforma(Azione,S,SNuovo,Costo),
%    write(Azione),write("  ==>   "),write(SNuovo),nl,
    \+member(SNuovo,Visitati),
    GValue is G + Costo,
    dfsLimitata(nodo(SNuovo,GValue),AzioniTail,[SNuovo|Visitati],Soglia).

valuta(nodo(S,G),F) :- distanza(S,H), F is G + H,!.

check(F,Soglia):-
    F > Soglia,
    overbound(X),
    F < X,!,
    retract(overbound(X)),
    assert(overbound(F)).

check(F,Soglia) :- F > Soglia.
