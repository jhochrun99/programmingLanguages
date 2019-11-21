male(bob).
male(john).
male(ben).
male(martin).
male(edmund).
male(david).
male(isidore).
male(william).
male(ferdinand).
male(morris).
male(alphonse).
male(jiri).

female(kathryn).
female(beatrice).
female(rachel).
female(lillian).
female(alice).
female(rosa).
female(marjorie).
female(emma).
female(nellie).
female(eva).
female(bertha).
female(fergie).

% A is the child of B
child(bob, john).
child(bob, kathryn).

child(beatrice, john).
child(beatrice, kathryn).

child(john, ben).
child(john, rachel).
child(lillian, ben).
child(lillian, rachel).

child(kathryn, rosa).
child(kathryn, martin).
child(alice, martin).
child(alice, rosa).
child(ferdinand, martin).
child(ferdinand, fergie).

child(marjorie, edmund).
child(marjorie, lillian).
child(david, lillian).
child(david, edmund).

child(ben, isidore).
child(ben, bertha).
child(william, isidore).
child(william, bertha).
child(emma, isidore).
child(emma, bertha).

child(morris, alphonse).
child(morris, emma).
child(nellie, alphonse).
child(nellie, emma).
child(eva, alphonse).
child(eva, emma).
child(jiri, alphonse).
child(jiri, emma).

parent(A,B) :- child(B,A).

father(A,B) :- parent(A,B), male(A).
mother(A,B) :- parent(A,B), female(A).

sibling(A,B) :- parent(C,A), parent(C,B), A\=B.

uncle(A,B) :- male(A), parent(C,B), sibling(A,C).
aunt(A,B) :- female(A), parent(C,B), sibling(A,C).

ancestor(A,B) :- parent(A,B).
ancestor(A,B) :- parent(A,C), ancestor(C,B).

grandparent(A,B) :- parent(A,C), parent(C,B).

first_cousin(A,B) :- parent(C,A), parent(D,B), sibling(C,D).
