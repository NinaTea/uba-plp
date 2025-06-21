mayorA2(X) :- X > 2.
esPar(X) :- mod(X, 2)  =:= 0.
menor(X, ?Y) :- X < Y.


%! insertar(?X, +L, ?LX)
insertar(X, L, LX) :- append(P, S, L), append(P, [X|S], LX).

%! permutacion(+L, ?PS)
permutacion([], []).
permutacion([L|LS], PS) :- permutacion(LS, P), insertar(L, P, PS).


