search(Soluzione) :- iniziale(S), distanza(S,D), dfs([nodo(S,0,[])],Soluzione,[],D,[]).

% dfs(ListaNodi,Soluzione,Visitati,Soglia,OverBound)
dfs([],Soluzione,_,_,OverBound) :-
  iniziale(S), distanza(S,D),
  min(OverBound,NuovaSoglia),
  dfs([nodo(S,D,[])],Soluzione,[],NuovaSoglia,[]).

dfs([nodo(S,_,Azioni)|_],Azioni,_,_,_) :- finale(S).

dfs([nodo(S,Value,AzioniPerS)|FigliTail],Soluzione,Visitati,Soglia,OverBound) :-
  distanza(S,D), Tmp is D + Value, Tmp =< Soglia,!,
  findall(Azione,applicabile(Azione,S),ListaApplicabili),
  generaFigli(nodo(S,Value,AzioniPerS),ListaApplicabili,ListaFigli,Visitati),
  unisci(ListaFigli,FigliTail,Figli),!,
  dfs(Figli,Soluzione,[S|Visitati],Soglia,OverBound).

dfs([nodo(S,Value,_)|FigliTail],Soluzione,Visitati,Soglia,OverBound) :-
    distanza(S,D), Tmp is D + Value,
    dfs(FigliTail,Soluzione,Visitati,Soglia,[Tmp|OverBound]).

generaFigli(_,[],[],_).

generaFigli(nodo(S,Value,AzioniPerS),[Azione|AltreAzioni],[nodo(SNuovo,NewValue,[Azione|AzioniPerS])|FigliTail],Visitati):-
    trasforma(Azione,S,SNuovo,Costo),
    \+member(SNuovo,Visitati),!,
    distanza(SNuovo,D), NewValue is D + Value + Costo,
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,FigliTail,Visitati).

generaFigli(nodo(S,Value,AzioniPerS),[_|AltreAzioni],FigliTail,Visitati):-
    generaFigli(nodo(S,Value,AzioniPerS),AltreAzioni,FigliTail,Visitati).

unisci([],B,B).
unisci(A,[],A).
unisci([X|TailA],B,Unione) :-
   member(X,B),!,
   unisci(TailA,B,Unione).

unisci([X|TailA],B,[X|Unione]) :- unisci(TailA,B,Unione).



min([Head|Tail],Minimo) :- aux(Tail,Head,Minimo).

aux([S],Temp,S) :- S < Temp, !.
aux([_],Temp,Temp).
aux([Head|Tail],Temp,Head) :- aux(Tail,Temp,Minimo), Head < Minimo,!.
aux([_|Tail],Temp,Minimo) :- aux(Tail,Temp,Minimo).
aux(_,Temp,Temp).
