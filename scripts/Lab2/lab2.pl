% Question 1

correspond(E1, [E1|_], E2, [E2|_]).
correspond(E1, [_|Tail1], E2, [_|Tail2]) :-
    correspond(E1, Tail1, E2, Tail2).


% Question 2

interleave(Ls, L) :-
    interleave1(Ls, L, []),
    samelength(Ls).


interleave1([], [], []).
interleave1([], [Head|Tail], [L|Ls]) :-
    interleave1([L|Ls], [Head|Tail], []).
interleave1([[Head|[]]|Lists], [Head|Tail], Lists1) :-
    interleave1(Lists, Tail, Lists1).
interleave1([[Head|[E|Elts]]|Lists], [Head|Tail], Lists1) :-
    append(Lists1, [[E|Elts]], Lists2),
    interleave1(Lists, Tail, Lists2).


samelength([]).
samelength([_]).
samelength([E1,E2|Tail]) :-
    length(E1,N),
    length(E2,N),
    samelength([E1|Tail]).
    

% Question 3

partial_eval(X, _, _, X) :-
    number(X).
partial_eval(X, Y, Val0, Val1) :-
    atom(X),
    ( X = Y ->
        Val1 = Val0
    ;
        Val1 = X
    ).
partial_eval(X+Y, Atom, Num, E) :-
    partial_eval(X, Atom, Num, E0),
    partial_eval(Y, Atom, Num, E1),
    ( number(E0), number(E1) ->
        E is E0 + E1
    ;
        E = E0 + E1
    ).
partial_eval(X-Y, Atom, Num, E) :-
    partial_eval(X, Atom, Num, E0),
    partial_eval(Y, Atom, Num, E1),
    ( number(E0), number(E1) ->
        E is E0 - E1
    ;
        E = E0 - E1
    ).
partial_eval(X*Y, Atom, Num, E) :-
    partial_eval(X, Atom, Num, E0),
    partial_eval(Y, Atom, Num, E1),
    ( number(E0), number(E1) ->
        E is E0 * E1
    ;
        E = E0 * E1
    ).
partial_eval(X/Y, Atom, Num, E) :-
    partial_eval(X, Atom, Num, E0),
    partial_eval(Y, Atom, Num, E1),
    ( number(E0), number(E1) ->
        E is E0 / E1
    ;
        E = E0 / E1
    ).
partial_eval(X//Y, Atom, Num, E) :-
    partial_eval(X, Atom, Num, E0),
    partial_eval(Y, Atom, Num, E1),
    ( number(E0), number(E1) ->
        E is E0 // E1
    ;
        E = E0 // E1
    ).