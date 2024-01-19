
substitute(_, [], _, []).

substitute(A, [A|As], B, [B|Bs]) :-
    substitute(A, As, B, Bs), !.

substitute(A, [X|As], B, [X|Bs]) :-
    substitute(A, As, B, Bs).

perform(Source, move(Block, Destination), Target) :-
    substitute(on(Block, From), Source, on(Block, Destination), Target1),
    append(Target1, [ clear(From) ], Target2),
    delete(clear(Destination), Target2, Target ).

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


free(State, Thing) :-
    is_object(Thing),
    \+ member(on(_, Thing), State).


action(State, move(Block, Destination)) :-
    is_object(Block),
    \+ Block == Destination,
    free(State, Block),
    free(State, Destination).


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
    can(Initial, Action),
    
    perform(Initial, Action, Intermediate),
    \+ member(Intermediate, Visited),
    bounded_strips(Predecessor, Intermediate, Final, [Intermediate|Visited], Actions).



solve(Initial, Final, Plan) :- strips(Initial, Final, Plan).


can(State, move(Block, Object)) :-
    is_object(Object),
    block(Block),
    Block \== Object.

at(X,Y) :- 
    place(X),
    place(Y).

% Blocks definition
block(a).
block(b).
block(c).

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
% inital = [ on(c, 1, 1), on(a, 4, 1), on(b, 6, 1), on(d, 4, 2) ]
% final  = [ on(d, 4, 1), on(a, 5, 2), on(b, 6, 2), on(c,5, 3)]


% can(Block, Object, [on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)]).
% can(Block, Object, [on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a)]).
% can([on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)], Action).

% perform([on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)], move(c, a), Intermediate)
% Intermediate = [on(a, b), on(b, 1), on(c, a), clear(3), clear(4), clear(a), clear(c)].

% solve([on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)], [on(a, b), on(b, c), on(c, 2), clear(1), clear(a), clear(3), clear(4)], Plan).
% solve([on(a, b), on(b, 1), on(c, 2)], [on(a, b), on(b, c), on(c, 2)], Plan).

% perform([on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)], move(c, a), Intermediate).
% Intermediate = [on(a, b), on(b, 1), on(c, a), clear(3), clear(4), clear(a), clear(c)].

