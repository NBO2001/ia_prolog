% generate_positions(Block, X_1, Y, Positions) :-
%     size(Block, Size),
%     Size_1 is X_1 + Size - 1,
%     findall(at(X, Y), (between(X_1, Size_1, I), X is I + 1), Positions).


busy(X,Y, State) :-
    member( on(_, at(X, Y)), State );
    member( dirty(X,Y), State).

stabilize(State,Block, X, Y) :-
    Y = 1.

stabilize(State,Block, X, Y) :-
    Y_1 is Y - 1,
    busy(X,Y_1, State).

can(State, move(Block, at(X,Y))) :-
    place(X), place(Y),
    block(Block),
    \+ busy( X,Y, State),
    member( on(Block, at(X_1, Y_1)), State ),
    Y_b is Y_1+1,
    \+ busy( X_1, Y_b, State), % Don't can have nothing above. 
    stabilize(State, Block, X,Y).


at(X,Y) :- 
    place(X),
    place(Y).

% Blocks definition
block(a).
block(b).
block(c).
block(d).

% Sizes
size(a, 1).
size(b, 1).
size(c, 2).
size(d, 3).

% Places definition
place(1).
place(2).
place(3).
place(4).
place(5).
place(6).


% Representation
% inital = [ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ]
% final  = [ on(d, at(4, 1)), on(a, at(5, 2)), on(b, at(6, 2)), on(c, at(5, 3))]


% teste
% can([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ], Action).
% member( on(_, at(1, 1)), [ on(c, 1, 1), on(a, 4, 1), on(b, 6, 1), on(d, 4, 2) ] ).


 