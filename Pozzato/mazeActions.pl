% applicabile(Azione,Stato).

applicabile(est,pos(R,C)) :- numColonne(NC), C < NC, Colonna is C+1, \+occupata(pos(R,Colonna)).
applicabile(ovest,pos(R,C)) :- C>1, Colonna is C-1, \+occupata(pos(R,Colonna)).
applicabile(nord,pos(R,C)) :- R>1, Riga is R-1, \+occupata(pos(Riga,C)).
applicabile(sud,pos(R,C)) :- numRighe(NR), R < NR, Riga is R+1, \+occupata(pos(Riga,C)).

% trasforma(Azione,Stato,NuovoStato).

trasforma(est,pos(Riga,Colonna),pos(Riga,ColonnaAccanto),1) :- ColonnaAccanto is Colonna+1.
trasforma(ovest,pos(Riga,Colonna),pos(Riga,ColonnaAccanto),1) :- ColonnaAccanto is Colonna-1.
trasforma(nord,pos(Riga,Colonna),pos(RigaSopra,Colonna),1) :- RigaSopra is Riga-1.
trasforma(sud,pos(Riga,Colonna),pos(RigaSotto,Colonna),1) :- RigaSotto is Riga+1.


% distanza tra un nodo e un nodo target più vicino
distanza(S,D):-
  findall(Target,finale(Target),ListaTarget),
  findDistances(S,ListaTarget,Distances),
  min_list(Distances, D),!.

findDistances(_,[],[]) :- !.

findDistances(pos(X1,Y1),[pos(X2,Y2)|Tail],[D|Distances]) :-
    D is ((X2-X1)**2 + (Y2-Y1)**2),     % distanza cartesiana
    %abs(X2-X1, X), abs(Y2-Y1, Y), D is X + Y,   % distanza di Manhattan
    findDistances(pos(X1,Y1),Tail,Distances).

abs(X,X) :- X >= 0, !.
abs(X,-X).
