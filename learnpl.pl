
substitute(_, [], _, []).

substitute(A, [A|As], B, [B|Bs]) :-
    substitute(A, As, B, Bs), !.

substitute(A, [X|As], B, [X|Bs]) :-
    substitute(A, As, B, Bs).

perform(Source, move(Block, Destination), Target) :-
    substitute(on(Block, _), Source, on(Block, Destination), Target).

is_object(Object) :-
    block(Object); place(Object).

on(Block, Object) :-
    block(Block), is_object(Object).

clear(Object) :-
    is_object(Object).


% Append rules
append([], L, L).
append([X | Y], Z, [X | W]) :- append(Y, Z, W).

delete(A, [A|B], B).
delete(A, [B, C|D], [B|E]) :-
    delete(A, [C|D], E).

action(State, move(Block, Destination)) :-
    is_object(Block),
    \+ Block == Destination,
    free(State, Block),
    free(State, Destination).

free(State, Thing) :-
    is_object(Thing),
    \+ member(on(_, Thing), State).

can(Block, Object, State) :-
    Block \== Object,
    member(clear(Block), State),
    is_object(Object).

% Define the move action for moving a block from one place to another
move(Block, From, To, InitialState, FinalState) :-
    can(Block, To, InitialState),
    Block \== To,
    delete(clear(To), InitialState, TempState),
    delete(on(Block, _), TempState, State1),
    append(State1, [ on(Block, To), clear(From) ], FinalState).



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
    action(Initial, Action),
    perform(Initial, Action, Intermediate),
    \+ member(Intermediate, Visited),
    bounded_strips(Predecessor, Intermediate, Final, [Intermediate|Visited], Actions).



solve(Initial, Final, Plan) :- strips(Initial, Final, Plan).

% Blocks definition
block(a).
block(b).
block(c).

% Places definition
place(1).
place(2).
place(3).
place(4).

% move(Block, From, To, InitialState, FinalState) 
% solve([on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(b)], [on(a, b), on(b, c), on(c,2), clear(3), clear(4),clear(1)], Plan).
% solve([on(a, b), on(b, 1), on(c, 2)], [on(a, b), on(b, c), on(c,2)], Plan).

% move(a, c, 4, [on(a, b), on(b, c), on(c,2), clear(4), clear(a)], FinalState).

% can(a, 2, [clear(2), clear(4), on(b,3), clear(b), on(c,1), on(a,c), clear(a)]).
% can(c, 2, [clear(2), clear(4), on(b,3), clear(b), on(c,1), on(a,c), clear(a)]).
% can(Block, 2, [clear(2), clear(4), on(b,3), clear(b), on(c,1), on(a,c), clear(a)]). 
% move(a,c,2, [clear(2), clear(4), on(b,3), clear(b), on(c,1), on(a,c), clear(a)], Final).
% move(a,c,Destine, [clear(2), clear(4), on(b,3), clear(b), on(c,1), on(a,c), clear(a)], Final).
% move(a,c,a, [clear(2), clear(4), on(b,3), clear(b), on(c,1), on(a,c), clear(a)], Final).
% delete(on(a, _),  [clear(4), on(b, 3), clear(b), on(c, 1), on(a, c), clear(a), on(a, 2), clear(c)], FinalState)

%action([ on(b, p), on(a, b), on(c, r)], Destine).

% bounded_strips(1, [on(a, b), on(b, c), on(c,2)],[on(a, b), on(b, c), on(c,2)], [[on(a, b), on(b, c), on(c,2)],[on(a, b), on(b, c), on(c,2)]],Plan).
%  bounded_strips(1, [on(a, b), on(b, 1), on(c, 2)], [on(a, b), on(b, c), on(c, 2)], [[on(a, b), on(b, 1), on(c, 2)]], Plan).