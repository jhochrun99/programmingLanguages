%%%%%%%%%%%%%%%%%%%%
% MAZE DATA
%%%%%%%%%%%%%%%%%%%%

% The raw map data. Each mazeWall(R,C) indicates the presence of a wall
% at row R, column C.
mazeWall(0,0).
mazeWall(0,1).
mazeWall(0,2).
mazeWall(0,3).
mazeWall(0,4).
mazeWall(0,5).
mazeWall(0,6).
mazeWall(0,7).
mazeWall(0,8).
mazeWall(0,9).
mazeWall(0,11).
mazeWall(1,0).
mazeWall(1,5).
mazeWall(1,11).
mazeWall(2,0).
mazeWall(2,2).
mazeWall(2,3).
mazeWall(2,4).
mazeWall(2,5).
mazeWall(2,7).
mazeWall(2,9).
mazeWall(2,10).
mazeWall(2,11).
mazeWall(3,0).
mazeWall(3,7).
mazeWall(3,9).
mazeWall(3,11).
mazeWall(4,0).
mazeWall(4,1).
mazeWall(4,2).
mazeWall(4,3).
mazeWall(4,4).
mazeWall(4,5).
mazeWall(4,6).
mazeWall(4,7).
mazeWall(4,9).
mazeWall(4,11).
mazeWall(5,0).
mazeWall(5,9).
mazeWall(5,11).
mazeWall(6,0).
mazeWall(6,2).
mazeWall(6,4).
mazeWall(6,5).
mazeWall(6,6).
mazeWall(6,7).
mazeWall(6,9).
mazeWall(6,11).
mazeWall(7,0).
mazeWall(7,2).
mazeWall(7,7).
mazeWall(7,11).
mazeWall(8,0).
mazeWall(8,2).
mazeWall(8,3).
mazeWall(8,4).
mazeWall(8,5).
mazeWall(8,6).
mazeWall(8,7).
mazeWall(8,8).
mazeWall(8,9).
mazeWall(8,11).
mazeWall(9,0).
mazeWall(9,2).
mazeWall(9,9).
mazeWall(9,11).
mazeWall(10,0).
mazeWall(10,2).
mazeWall(10,3).
mazeWall(10,5).
mazeWall(10,6).
mazeWall(10,7).
mazeWall(10,9).
mazeWall(10,11).
mazeWall(11,0).
mazeWall(11,5).
mazeWall(11,9).
mazeWall(11,11).
mazeWall(12,0).
mazeWall(12,2).
mazeWall(12,3).
mazeWall(12,4).
mazeWall(12,5).
mazeWall(12,7).
mazeWall(12,8).
mazeWall(12,9).
mazeWall(12,11).
mazeWall(13,0).
mazeWall(13,5).
mazeWall(13,7).
mazeWall(13,11).
mazeWall(14,0).
mazeWall(14,1).
mazeWall(14,2).
mazeWall(14,3).
mazeWall(14,4).
mazeWall(14,5).
mazeWall(14,6).
mazeWall(14,7).
mazeWall(14,8).
mazeWall(14,9).
mazeWall(14,10).
mazeWall(14,11).

% The maximum dimensions of the maze grid
mazeDimension(15,12).

% The start and end position of the maze.
% That is, when solving the maze, you start
% at mazeStartPos and you finish and mazeEndPos.
mazeStartPos(13,6).
mazeEndPos(0,10).

%%%%%%%%%%%%%%%%%%%%
% MAZE PRINTING
%%%%%%%%%%%%%%%%%%%%

% Helper predicates determine which character
% to print for a particular maze position.
% We use S to signify the starting position,
% and E to signify the ending position.
% Stars are used for maze walls, space for
% paths. When drawing the solution, periods
% show the winning path.
mazeElement(R,C,'S',_) :- mazeStartPos(R,C), !.
mazeElement(R,C,'E',_) :- mazeEndPos(R,C), !.
mazeElement(R,C,'*',_) :- mazeWall(R,C), !.
mazeElement(R,C,'.',V) :- member([R,C],V), !.
mazeElement(_,_,' ',_).

% Newline at the end of each row.
mazeNewLine(C) :- mazeDimension(_,C), nl.

% Print the maze, with a winning path, maybe.
% We just enumerate all positions within the
% maze dimensions, call mazeElement to figure
% out what to print there, and then print it.
printMaze(WinningPath) :-
    mazeDimension(Rows, Cols),
    between(0, Rows, Row),
    between(0, Cols, Col),
    mazeElement(Row, Col, Appearance, WinningPath),
    write(Appearance),
    mazeNewLine(Col),
    fail.

% Shortcut function to print the maze
% if you don't have the solution yet.
printUnsolvedMaze :-
    printMaze([]).

% Shortcut function to find the solution
% and print it.
printSolvedMaze :-
    mazeStartPos(StartR,StartC),
    solve(StartR,StartC,_,_,[[StartR,StartC]],Visited,[],_),
    printMaze(Visited).

% Print out the winning path by calling solve.
winningPath(Path) :-
    mazeStartPos(StartR,StartC),
    solve(StartR,StartC,_,_,[[StartR,StartC]],_,[],Path1),
    reverse(Path1,Path).

%%%%%%%%%%%%%%%%%%%%
% MAZE SOLVING
%%%%%%%%%%%%%%%%%%%%

% The four cardinal directions that we can move in: north,
% south, east, west.
direction(n).
direction(s).
direction(e).
direction(w).

% Given a current position in the maze, and a direction of travel, calculate
% a new position.
% For example, to move east, from position (4,7), I would do:
%    ?- newPos(4,7,e,NewRow,NewCol).
%    NewRow = 4,
%    NewCol = 8 ;
%    false.
% This predicate should fail if the proposed position is outside the bounds of the map,
% or if it's a wall. For example:
% Can't do this: new position (15,6) would be outside the map.
%    ?- newPos(14,6,s,NewRow,NewCol).
%    false.
%
% Can't do this either: new position (14,7) is a wall
%    ?- newPos(14,6,e,NewRow,NewCol).
%    false.

newPos(OldRow,OldCol,e,OldRow,NewCol) :-
    NewCol is OldCol + 1,
    not(mazeWall(OldRow, NewCol)),
    mazeDimension(Row, Col), Row > OldRow, Col > NewCol, OldRow > -1, NewCol > -1. 
newPos(OldRow,OldCol,w,OldRow,NewCol) :-
    NewCol is OldCol - 1,
    not(mazeWall(OldRow, NewCol)),
    mazeDimension(Row, Col), Row > OldRow, Col > NewCol, OldRow > -1, NewCol > -1.
newPos(OldRow,OldCol,s,NewRow,OldCol) :-
    NewRow is OldRow + 1,
    not(mazeWall(NewRow, OldCol)),
    mazeDimension(Row, Col), Row > NewRow, Col > OldCol, NewRow > -1, OldCol > -1.
newPos(OldRow,OldCol,n,NewRow,OldCol) :-
    NewRow is OldRow - 1,
    not(mazeWall(NewRow, OldCol)),
    mazeDimension(Row, Col), Row > NewRow, Col > OldCol, NewRow > -1, OldCol > -1.

% Generate all possible moves from given position.
% 
% Parameters are as follows:
% move(CurrentR,     % Current row and column position
%      CurrentC, 
% 
%      NewR,         % new row and column position
%      NewC, 
% 
%      PosVisitedIn,  % List of [R,C] positions visited prior to this move
%                     % for example: [[1, 9], [1, 8], [2, 8]]
%      PosVisitedOut, % List of [R,C] positions visited including this move.
%                     % New moves are attached to the beginning of the list.
%                     % for example: [[1, 10], [1, 9], [1, 8], [2, 8]]
%      MoveListIn,    % List of moves made before this move.
%                     % For example: [n,n,w,w,n,e]
%      MoveListOut)   % List of moves made including this move.
%                     % New moves added at beginning of the list.
%                     % For example: [n,n,n,w,w,n,e]
move(CurrentR, CurrentC, NewR, NewC, PosVisitedIn, [[NewR, NewC] | PosVisitedIn], MoveListIn, [Direction | MoveListIn]) :-
    direction(Direction),
    newPos(CurrentR, CurrentC, Direction, NewR, NewC),
    not(member([NewR, NewC], PosVisitedIn)).

% Solve the maze by repeatedly calling move. Stops when
% we reach the maze ending position.
solve(R,C,_,_,Visited,Visited,Moves,Moves) :-
    mazeEndPos(R,C),!.
solve(R,C,NextR,NextC,VisitedIn,VisitedOut2,MoveListIn,MoveListOut2) :-
    move(R,C,NewR,NewC, VisitedIn, VisitedOut, MoveListIn, MoveListOut),
    solve(NewR,NewC,NextR,NextC,VisitedOut,VisitedOut2, MoveListOut, MoveListOut2).
