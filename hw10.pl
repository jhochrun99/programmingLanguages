% 1
/*
T1 = [[a,f(b),c,d],g(X1,[e|X2]),X3,X4|X5]
T2 = [Y1,g(d,Y2)|Y1]

Y1 = [a,f(b),c,d]
g(X1,[e|X2]) = g(d,Y2)
X3,X4|X5 = Y1 = [a,f(b),c,d]

T1 will unify with T2:
X1 - d
X3 - a
X4 - f(b)
X5 - [c,d]
Y1 - [a,f(b),c,d]
*/

% 2
all_diff([]). % case of empty list
all_diff([_]). % case with only one element
all_diff([H|T]) :- not(member(H,T)), all_diff(T).

% 3
t(V, Left, Right).

flipNode(nil, nil).
flipNode(t(V,L,R), t(V,R,L).

someTree(X) :-
    X = t("t1",
        t("t2a",
            t("t3a", nil, nil),
            t("t3b", nil, nil)),
        t("t2b",
            t("tc3", nil, nil),
            t("t3d", nil, nil))).

flipTree(nil, nil).
flipTree(t(V,L,R), t(V,R2,L2)) :- flipTree(L,L2), flipTree(R,R2).

% 4
inorderTraversalSpatial(nil, []).
inorderTraversalSpatial(t(V,L,R), Lst) :-
        inorderTraversalSpatial(L,L2),
        inorderTraversalSpatial(R,R2),
        append(L2,[V],L2V),
        append(L2V,R2,Lst).

% 5
inorderTraversalTemporal(t(_,L,_), X) :- inorderTraversalTemporal(L, X). % left
inorderTraversalTemporal(t(V,_,_), V). % parent
inorderTraversalTemporal(t(_,_,R), X) :- inorderTraversalTemporal(R, X). % right

% 6
sublist([],_).
sublist([H|T], [H|T2]) :- checkNext(T,T2). % found start, checks next value
sublist([H|T], [_|T2]) :- sublist([H|T], T2). % continues looking for start of list

checkNext([],_).
checkNext([H|T], [H|T2]) :- checkNext(T,T2).

% 7
sublist_cut([], _).
sublist_cut([H|T], [H|T2]) :- checkNext(T, T2), !.
sublist_cut([H|T], [_|T2]) :- sublist_cut([H|T], T2).

% 8
bad_sort(X,Y) :- permutation(Y, X), sorted(Y), !.

sorted([]).
sorted([_]).
sorted([H,H2|T]) :- H =< H2, sorted([H2|T]).
