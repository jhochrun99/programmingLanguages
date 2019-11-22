% p6
inboth(A,B,X) :- member(X,A), member(X,B).

% p7
increment_all([],[]).
increment_all([H1|T1], [H2|T2]) :- increment_all(T1,T2), H2 is H1+1.

% p8
foldup([], _).
foldup(L1, []) :- foldup(L1, L1).
foldup([H|[H2|T]], [H|[H2|T]]) :- L is H+H2, foldup(T, [L|T]).
foldup([H|T], [H2|_]) :- L is H+H2, foldup(T, [L|T]).
