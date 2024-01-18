
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


can(Block, move(Block, Object), State) :-
    is_object(Object),
    block(Block),
    Block \== Object,
    member(clear(Block), State),
    member(clear(Object), State).


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
    can(Initial, Action, Initial),
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

% can(Block, Object, [on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)]).
% can(Block, Object, [on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a)]).
% solve([on(a, b), on(b, 1), on(c, 2), clear(3), clear(4), clear(a), clear(c)], [on(a, b), on(b, c), on(c, 2), clear(1), clear(a), clear(3), clear(4)], Plan).