search(Soluzione) :-
  iniziale(S), iterativeDeepening(S,Soluzione,1),length(Soluzione,L),write(L),nl.

iterativeDeepening(S,[],_) :- finale(S).
iterativeDeepening(S,Soluzione,Soglia) :-
    dfsLimitata(S,Soluzione,[S],Soglia),!.

iterativeDeepening(S,Soluzione,Soglia):-
    NuovaSoglia is Soglia+1,
    write(NuovaSoglia),nl,
    numRighe(R), numColonne(C), NuovaSoglia =< R*C,   % limite imposto per evitare che vada in un loop infinito quando non esistono soluzioni
    iterativeDeepening(S,Soluzione,NuovaSoglia).

dfsLimitata(S,[],_,_):-finale(S).
dfsLimitata(S,[Azione|AzioniTail],Visitati,Soglia):-
    Soglia>0,
    applicabile(Azione,S),
    trasforma(Azione,S,SNuovo,_),
    \+member(SNuovo,Visitati),
    NuovaSoglia is Soglia-1,
    dfsLimitata(SNuovo,AzioniTail,[SNuovo|Visitati],NuovaSoglia).
