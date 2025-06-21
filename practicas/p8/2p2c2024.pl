%! subsecuenciaCreciente(+L,-S)

subsecuenciaCreciente(L,S) :- sublista(L, S), creciente(S).

%! prefijo(+L, ?P)
prefijo(L, P) :- append(P, _, L).

sufijo(L, S) :- append(_, S, L).

%! sublista(+L, ?S).
sublista([], []).
sublista([X|Tx], [X|Ty]) :- sublista(Tx, Ty).
sublista([_|Tx], Ty) :- sublista(Tx, Ty).


%! creciente(+L)
creciente([]).
creciente([_]).
creciente([X,Y|T]) :- X < Y, creciente([Y|T]).

hayMasLarga(L, S, N1) :- subsecuenciaCreciente(L, S), length(S, N), N > N1.
unaSub(L,S,N) :- subsecuenciaCreciente(L, S), length(S, N).

%! subsecuenciaCrecienteMasLarga(+L, -S)
subsecuenciaCrecienteMasLarga(L, S) :- unaSub(L, S, N1), not(hayMasLarga(L, _, N1)).


%! fibonacci(-X)
fibonacci(1).
fibonacci(X) :- fibonacci(X1), fibonacci(X2), X is X1 + X2. 