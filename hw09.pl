% problem 2.6
inboth(A,B,X) :- member(X,A), member(X,B).

% problem 3.7
increment_all([],[]).
increment_all([H1|T1], [H2|T2]) :- increment_all(T1,T2), H2 is H1+1.

% problem 3.8 < doesnt work
foldup([],[]).
foldup([H|[H2|T]], L) :- foldup([L|T],[L]), L is H+H2.
