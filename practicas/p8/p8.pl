%ej 1
%el padre de y es x

padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

%el hijo de Y es X
hijo(X, Y) :- padre(Y, X).

hermano(X, Y) :- X \= Y, padre(Z, X), padre(Z, Y).

%Y es descendiente de X si el padre de Y es X
descendiente(X, Y) :- padre(X, Y).
descendiente(X, Y) :- abuelo(X, Y).

nieto(juan, Y) :- padre(juan, Z), padre(Z, Y).

ancestro(X, X).
ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% es ancestro de x la persona juan?
% para ancestro(juan, x) -> x, cumple el primer caso y lo devuelve
% vemos el resto, ancestro(juan, x) :- ancestro(z, x), padre(juan, z)
% x es un valor que cumple que cumple ancestro y donde juan es su padre

% ej 2
vecino(X, Y, [X | [Y | _]]).
vecino(X, Y, [_|_]) :- vecino(X, Y, _).

% ej 3
natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

% Si ejecuto menorOIgual(0, X) se cuelga.
% Un programa en prolog se cuelga cuando no termina el proceso de aplicación
% de la regla de resolución

% el problema de menorOIgual era el orden de las reglas


% ej 4
% juntar(?Lista1, ?Lista2, ?Lista3)
juntar([], [], []).
juntar([], L2, L2).
juntar(L1, [], L1).
juntar([H1|T1], L2, [H1|T3]) :- juntar(T1, L2, T3).

% ej 5

ultimo([X], X). 
ultimo([H1|T1], U) :- ultimo(T1, U).

capicua([]).
capicua([ _ ]).
capicua([ H | T ]):- juntar(M, [H] , T ), capicua( M ).
reverse(L1, L2):- juntar(L1, L2, L3), capicua(L3).

pertenece(X, [X|T]).
pertenece(X, [Y|T]) :- X \= Y, pertenece(X, T).


% ej 6
%! aplanar(+Xs, -Ys)
aplanar([], []).
aplanar([H|T], Ys) :- aplanar(H, L),
                      aplanar(T, R),
                      append(L, R, Ys),
                     is_list(H).
aplanar([H|T], [H|Ty]) :- aplanar(T, Ty), not(is_list(H)).

% ej 7
%! intersección(+L1, +L2, -L3)
intersección([X], [X], [X]).
intersección([], _, []).
intersección([H1|T1], L2, [H1|T3]):- member(H1, L2), intersección(T1, L2, T3).
intersección([H1|T1], L2, T3):- not(member(H1, L2)), intersección(T1, L2, T3).

%! L1 tiene los N primeros elementos de L, |L| = m
%! L2 tiene el resto
%! Si L tiene menos de N elementos el predicado debe fallar.

%! Reversibilidad> que params pueden estar indefinidos al momento de la invocacion?

partir(N, L, L1, L2).
