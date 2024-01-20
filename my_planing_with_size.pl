

substitute(_, [], _, []).

substitute(A, [A|As], B, [B|Bs]) :-
    substitute(A, As, B, Bs), !.

substitute(A, [X|As], B, [X|Bs]) :-
    substitute(A, As, B, Bs).

reversed([], []).

reversed([X | Tail], Reversed) :- 
    reversed(Tail, ReversedTail),
    append(ReversedTail, [X], Reversed).

first_anchor([Ac | _], Coords) :-
    (Ac = dirty(X, Y) ; Ac = on(_, at(X, Y))),
    Coords = (X-Y), !.

first_anchor([_ | Tail], Coords) :-
    first_anchor(Tail, Coords).

is_possible(Block, (Xr-_), (Xa-_)) :-
    size(Block, Size),
    Med is Size/2,
    E is abs(Xa-Xr),
    E < Med.

generate_positions(Block, X_1, Y, Positions) :-
    size(Block, Size),
    Size_1 is X_1 + Size - 2,
    X_2 is X_1 - 1,
    findall((X-Y), (between(X_2, Size_1, I), X is I + 1), Positions).

look_positions(State, [], []).

look_positions(State, [X-Y | Positions], [on(Block, at(X, Y)) | Situation]) :-
    member(on(Block, at(X, Y)), State),
    look_positions(State, Positions, Situation).

look_positions(State, [X-Y | Positions], [dirty(X, Y) | Situation]) :-
    member(dirty(X, Y), State),
    look_positions(State, Positions, Situation).

look_positions(State, [X-Y | Positions], [empty(X, Y) | Situation]) :-
    \+ member(on(_, at(X, Y)), State), % change of Block to _
    \+ member(dirty(X, Y), State),
    look_positions(State, Positions, Situation).

busy_all(State, []).

busy_all(State, [Look | Stack ]) :-
    \+ member( Look, State),
    busy_all(State, Stack).

busy(X,Y, State) :-
    member( on(_, at(X, Y)), State );
    member( dirty(X,Y), State).

find_block(State, Block, X, Y) :-
    member( on(Block, at(X,Y) ), State).

stabilize(State,Block, X, Y) :-
    Y = 1,
    \+ busy(X,Y, State).

stabilize(State,Block, X, Y) :-
    Y_1 is Y - 1,
    busy(X,Y_1, State),
    find_block(State,To, X, Y_1),
    \+ To == Block.

can(Block, [], []).

can(Block,Situation,Positions) :-
    nth0(0, Positions, First), last(Positions, Last),
    first_anchor(Situation, Coords_1), % Busy or Block more left
    reversed(Situation, SituationRev),
    first_anchor(SituationRev, Coords_2), %Busy or Block more rigth
    is_possible(Block,First, Coords_1),
    is_possible(Block,Last, Coords_2).

have_space(Block, X) :-
    size(Block, Size),
    F is X + Size - 1,
    place(F).

goal(State, move(Block, at(X,Y))) :-
    place(X), place(Y),
    block(Block),
    have_space(Block, X),
    generate_positions(Block, X, Y, Range), % Position at the block will occuped
    look_positions(State, Range, Situation),
    busy_all(State, Situation),
    member( on(Block, at(X_1, Y_1)), State ),
    Y_b is Y_1+1,
    \+ busy( X_1, Y_b, State), % Don't can have nothing above. 
    stabilize(State, Block, X,Y).
    can(Block,Situation, Range).


at(X,Y) :- 
    place(X),
    place(Y).


perform_r([],[],Source, Source).

perform_r( [X-Y | List ], [X_1-Y_1 | List_2 ], Source, Target) :-
    substitute(dirty(X, Y), Source, dirty(X_1, Y_1), Target1),
    perform_r(List, List_2, Target1, Target).

perform_r1([_ | Tail1], [_ | Tail2], Source, Target) :-
    perform_r(Tail1, Tail2, Source, Target).

% End for working
perform(Source, move(Block, at(X,Y)), Target) :-
    substitute(on(Block, at(X_F, Y_F)), Source, on(Block, at(X,Y)), Target1),
    generate_positions(Block,X,Y, Pos),
    generate_positions(Block,X_F,Y_F, Pos_2),
    perform_r1( Pos_2, Pos, Target1, Target).
    
strips(Initial, Final, Plan) :- strips(Initial, Final, [Initial], Plan).

strips(Initial, Final, Visited, Plan) :- 
    deepening_strips(1, Initial, Final, Visited, Plan).

deepening_strips(Bound, Initial, Final, Visited, Plan) :-
    bounded_strips(Bound, Initial, Final, Visited, Plan).

deepening_strips(Bound, Initial, Final, Visited, Plan) :-
    succ(Bound, Successor),
    % write(Bound), write("\n"),
    deepening_strips(Successor, Initial, Final, Visited, Plan).

bounded_strips(_, Final, Final, _, []).

bounded_strips(Bound, Initial, Final, Visited, [Action|Actions]) :-
    succ(Predecessor, Bound),
    goal(Initial, Action),
    perform(Initial, Action, Intermediate),
    % write(Intermediate), write("\n"),
    \+ member(Intermediate, Visited),
    bounded_strips(Predecessor, Intermediate, Final, [Intermediate|Visited], Actions).



plan(Initial, Final, Plan) :- strips(Initial, Final, Plan).

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


state1(    [ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ]).
state2(    [ on(c, at(1, 1)), dirty(2,1), on(a, at(1, 2)), on(b, at(2, 2)), on(d, at(4, 1)), dirty(5,1), dirty(6,1) ] ).

finalState([ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2,2), dirty(3,2) ]).
finalState2([ on(c, at(5, 2)), dirty(6,2), on(a, at(5, 3)), on(b, at(6, 3)), on(d, at(4, 1)), dirty(5,1), dirty(6,1) ]).

% state2(State), finalState2(Final), plan(State, Final, Plan).

% perform([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ], move(d, at(1, 2)), Target).
% Representation
% inital = [ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ]

% final  = [ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2,2), dirty(3,2) ]
% plan([ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ], [ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2,2), dirty(3,2) ], Plan).

% plan([ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ], [ on(c, at(1, 1)), dirty(2,1), on(a, at(1, 3)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2,2), dirty(3,2) ], Plan).

% plan( [ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ], [ on(c, at(5, 3)), dirty(6,3), on(a, at(5, 2)), on(b, at(6, 2)), on(d, at(4, 1)), dirty(5,1), dirty(6,1) ], Plan).


% teste

% goal([ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ], Action).
% perform([ on(c, at(1, 1)), dirty(2,1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(5,2), dirty(6,2) ],  move(d, at(1, 2)), Intermediate).

% goal([on(c, at(1, 1)), dirty(2, 1), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2, 2), dirty(3, 2)], Action).

% member( on(_, at(1, 1)), [ on(c, 1, 1), on(a, 4, 1), on(b, 6, 1), on(d, 4, 2) ] ).

% generate_positions(d,1,1, Pos).

% find_block([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ], Block, 1,2).


% Action = move(d, at(1, 2))

% look_positions([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4, 2), dirty(5,2), dirty(6,2) ],[1-2, 2-2, 3-2, 4-2], Situation).
% look_positions([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ],[1-1, 2-1, 3-1, 4-1], Situation).

% busy_all([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ], [empty(1, 2), empty(2, 2), empty(3, 2) ]).

% Tests Stablity
% generate_positions(d,1,1, Pos), look_positions([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ],Pos, Situation).



% Situation = [on(c, at(1, 1)), dirty(2, 1), empty(3, 1)] ;
% reversed([on(c, at(1, 1)), dirty(2, 1), empty(3, 1)], ListRv).
% first_anchor([empty(3, 1), dirty(2, 1), on(c, at(1, 1))], Coords).
% first_anchor([on(c, at(1, 1)), dirty(2, 1), empty(3, 1)], Coords).
% generate_positions(d, 1, 1, Pos), nth0(0, Pos, First), last(Pos, Last).


% Block, where is the maximun extreme of the block


% first_anchor([empty(3, 1), dirty(2, 1), on(c, at(1, 1))], Coords),  generate_positions(d, 1, 1, Pos), nth0(0, Pos, First), is_possible(d, First, Coords).

% can(d,[on(c, at(1, 1)), dirty(2, 1), empty(3, 1)],[1-1, 2-1, 3-1]).
% can(d,[on(c, at(1, 1)), empty(2, 1), empty(3, 1)],[1-1, 2-1, 3-1]).
% can(d,[empty(1, 1), on(c, at(2, 1)), empty(3, 1)],[1-1, 2-1, 3-1]).
% can(d,[empty(1, 1), empty(2, 1),  on(c, at(3, 1))],[1-1, 2-1, 3-1]).
% can(d,[on(a, at(1, 1)), empty(2, 1),  on(c, at(3, 1))],[1-1, 2-1, 3-1]).


% 1 ?- perform([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ], move(d, at(1, 2)), Target).
% Target = [on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2, 1), dirty(4, 2), dirty(5, 2), dirty(6, 2)].

% perform([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(4,2), dirty(5,2), dirty(6,2) ], move(dirty(1, 2)), Target).


% perform([ on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(4, 2)), dirty(2,1), dirty(5,2), dirty(6,2) ], move(d, at(1, 2)), Target).

% perform_r([4-2, 5-2, 6-2], [1-2, 2-2, 3-2], [on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2, 1), dirty(4, 2), dirty(5, 2), dirty(..., ...)], _386)


% perform_r([5-2, 6-2], [2-2, 3-2], [on(c, at(1, 1)), on(a, at(4, 1)), on(b, at(6, 1)), on(d, at(1, 2)), dirty(2, 1), dirty(5, 2), dirty(6, 2)], Target).

% substitute(dirty(6, 2), [dirty(5, 2), dirty(6, 2)], dirty(3, 2), Target).


% move(d,at(1,2))
% move(c,at(4,3))