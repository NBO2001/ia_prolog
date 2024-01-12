
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

can(Block, Object, State) :-
    member(clear(Object), State),
    member(on(_, Block), State) =:= false,
    is_object(Object).


% Define the move action for moving a block from one place to another
move(Block, From, To, InitialState, FinalState) :-
    can(Block, To, InitialState),
    delete(clear(From), InitialState, TempState),
    delete(on(Block, _), TempState, TempState2),
    append(TempState2, [clear(To), on(Block, To)], FinalState).

% Blocks definition
block(a).
block(b).
block(c).

% Places definition
place(1).
place(2).
place(3).
place(4).