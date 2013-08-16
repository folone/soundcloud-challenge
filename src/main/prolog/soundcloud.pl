% initial data
friend(davidbowie,  omid).
friend(davidbowie,  kim).
friend(kim,         torsten).
friend(torsten,     omid).
friend(brendan,     torsten).
friend(ziggy,       davidbowie).
friend(mick,        ziggy).

% friendship works both way.
friend(X, Y) :-
    compare(>, X, Y), % without this line we'll drop into an infinite loop
    friend(Y, X).

% a path P between X and Y of length N
path(X, Y, P, A) :-
    path(X, Y, P, [], A),
    not(X = Y).

path(X, Y, [X,Y], _, 1) :- friend(X, Y).
path(X, Y, [X|P], Visited, A) :- friend(X, Z),
                                 not(member(Z, Visited)),
                                 path(Z, Y, P, [Z|Visited], A1),
                                 A is A1 + 1.

% now to find all pathes of length 2:
result(List) :- findall((X, Path), path(X, Y, Path, 2), List).
