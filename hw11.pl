:- use_module(library(clpfd)).

% 3
nth(0,[X|_],X).
nth(N,[_|R],X) :- N1 #= N-1, nth(N1, R, X).

% 4
sisters(X) :-
        X = [A,B,C,D,E],
        D #= D2*2,
        E #= E3*3,
        A+B+C+(D2*2)+(E3*3) #= 200,
        A #>= 8, B #>= 8, C #>= 8, (D2*2) #>= 8, (E3*3) #>= 8,
        (A*12) + (B*3) + C + D2 + E3 #= 200,

        label([A,B,C,D2,E3]).

% 6
seating :-
        People = [Bob, Sandra, Jiri, Zoltan, Jana, Bogdana],
        People ins 1..6,
        all_different(People),

        Zoltan in 1 \/ 6,
        Sandra in 2..5,
        Bogdana #= 3,
        1 #= abs(Bob - Jiri), %Bob must be 1 away from Jiri
        1 #\= abs(Zoltan - Bob), %Zoltan can't be 1 away from Bob or Jana
        1 #\= abs(Zoltan - Jana),

        label(People),
        writeln(people=People).

% 7
profs(Hs) :-
        % each professor in the list Hs of offices is represented as:
        % h(Name, Pet, Laptop , Class)
        length(Hs, 5),
        member(h(abel,_,_,distsys), Hs),
        member(h(biggs,camel,_,_), Hs),
        member(h(_,_,thinkPad,introProg), Hs),
        member(h(daniels,_,chromeBook,_), Hs),
        member(h(_,snake,macBook,algo), Hs),

        append([h(eucalpytus,_,_,_),_|_],_,Hs),
        append(_,[_,h(_,bird,Acer,_)|[]],Hs), Acer \= acer,

        neighbor(h(daniels,_,_,_), h(_,_,acer,_), Hs),
        neighbor(h(_,ferret,_,_), h(_,camel,_,_),Hs),
        neighbor(h(_,_,toshiba,ai), h(_,capybara,_,_), Hs),

        member(h(cadeau,_,_,_), Hs),
        member(h(abel,NoFer,_,_), Hs), NoFer \= ferret,
        member(h(_,_,_,os), Hs).

neighbor(X,Y,Z) :- append(_, [X,Y|_], Z).
neighbor(X,Y,Z) :- append(_, [Y,X|_], Z).

whoHasFerret(X) :-
        profs(Hs),
        member(h(X,ferret,_,_), Hs), !.

whoTeachesOs(X) :-
        profs(Hs),
        member(h(X,_,_,os), Hs), !.
