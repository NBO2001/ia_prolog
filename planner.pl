
plan(State, Goals, []):-
    satisfied(State, Goals).

plan(State, Goals, Plan):-
    append(PrePlan, [Action],Plan),
    select(State, Goals, Goal),
    achieves(Action, Goal),
    can(Action, Condition),
    preserves(Action, Goals),
    regress(Goals, Action, RegressedGoals),

plan(State, RegressedGoals, PrePlan).
    satisfied(State, []).

satisfied(State, [Goal|Goals]):-
    member(Goal, State),
    satisfied(State, Goals).

satisfied(State, [Goal | Goals]):-
    holds(Goal),
    satisfied(State,Goals).
    
holds(dif(X,Y)).

select(State, Goals, Goal):-
    member(Goal, Goals).
    achieves(Action, Goal):-
    adds(Action, Goals),
    member(Goal, Goals).

preserves(Action, Goals):-
    deletes(Action, Relations),
    member(Goal, Relations),
    not_member(Goal,Goals).
    not_member(_, []) :- !.

not_member(X, [Head|Tail]) :-
    dif(X,Head),
    not_member(X, Tail).
    regress(Goals, Action, RegressedGoals):-
    adds(Action, NewRelations),

delete_all(Goals, NewRelations, RestGoals),
    can(Action, Condition),
    addnew(Condition, RestGoals, RegressedGoals).
    addnew([], L, L).

addnew([Goal | _], Goals, _):-
    impossible(Goal, Goals),!, fail.

addnew([X|L1], L2, L3):-
    member(X,L2), ! ,
    addnew(L1, L2, L3).
    addnew([X|L1], L2, [X|L3]):-
    addnew(L1,L2,L3).


delete_all([],_,[]).
    delete_all([X|L1], L2, Diff):-
    member(X,L2),!,
    delete_all(L1, L2, Diff).

delete_all([X|L1], L2, [X|Diff]):-
    delete_all(L1, L2, Diff).


% Blocks definition
block(a).
block(b).
block(c).

% Places definition
place(1).
place(2).
place(3).
place(4).

% plan([on(a, b), on(b, 1), on(c, 2)], [on(a, b), on(b, c), on(c,2)], Plan).