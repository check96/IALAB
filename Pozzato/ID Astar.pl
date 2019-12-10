search(Soluzione) :-
   iniziale(S), valuta(nodo(S,0),F),iterativeDeepening(nodo(S,0),Soluzione,F).      % inizia la ricerca con soglia F del nodo iniziale

iterativeDeepening(nodo(S,G),Soluzione,Soglia) :-
    assert(overbound(999999999)),                             % inizializza overbound a infinito
    dfsLimitata(nodo(S,G),Soluzione,[S],Soglia).

% se la dfsLimitata fallisce, si aumenta la soglia e si ricominicia
iterativeDeepening(S,Soluzione,_):-
    retract(overbound(X)),
    X \== 999999999,                        % si controlla l'esistenza di qualche nodo che abbia superato la soglia all'iterazione precedente
    iterativeDeepening(S,Soluzione,X).

dfsLimitata(nodo(S,_),[],_,_) :- finale(S).
dfsLimitata(nodo(S,G),[Azione|AzioniTail],Visitati,Soglia):-
    applicabile(Azione,S),
    trasforma(Azione,S,SNuovo,Costo),     % si trasforma il nodo
    \+member(SNuovo,Visitati),            % si controlla che il nodo non sia già stato visitato
    GValue is G + Costo,                  % si calcola il costo di cammino di SNuovo
    valuta(nodo(SNuovo,GValue),F),        % si calcola f(SNuovo)
    \+check(F,Soglia),                    % si controlla che f(SNuovo) non superi la soglia
    dfsLimitata(nodo(SNuovo,GValue),AzioniTail,[SNuovo|Visitati],Soglia).

valuta(nodo(S,G),F) :- distanza(S,H), F is G + H.

% Si aggiorna overbound in caso f > Soglia e f < overbound
check(F,Soglia):-
    F > Soglia,
    overbound(X),
    F < X,!,
    retract(overbound(X)),
    assert(overbound(F)).

check(F,Soglia) :-
   F > Soglia.
