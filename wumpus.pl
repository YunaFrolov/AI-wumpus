%%% dynamic methods %%%

:- dynamic ([
	     agent_location/1,
	     gold_location/1,
	     pit_location/1,
	     time/1,
	     score/1,
	     visited/1,
	     visited_cells/1,
	     world_size/1,
	     wumpus_location/1
	    ]).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initializing

init :-
    init_game,
    init_land,
    init_agent,
    init_wumpus.

init_game :-
    retractall( time(_) ),
    assert( time(0) ),

    retractall( score(_) ),
    assert( score(0) ),
    
    retractall( visited(_) ),
    assert( visited(1) ),

    retractall( isWumpus(_,_) ),
    retractall( isGold(_,_) ),
    
    retractall( visited_cells(_) ),
    assert( visited_cells([]) ).

	
init_land :-
    retractall( world_size(_) ),
    assert( world_size(4) ),

    retractall( gold_location(_) ),
    assert( gold_location([3,2]) ),

    retractall( pit_location(_) ),
    assert( pit_location([4,4]) ),
    assert( pit_location([3,3]) ),
    assert( pit_location([1,3]) ).

init_agent :-
    retractall( agent_location(_) ),
    assert( agent_location([1,1]) ),
    
    visit([1,1]).

init_wumpus :-
    retractall( wumpus_location(_) ),
    assert( wumpus_location([4,1]) ).

visit(Xs) :-
    visited_cells(Ys),
    retractall( visited_cells(_) ),
    assert( visited_cells([Ys|Xs]) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% States

standing :-
    wumpus_location(WL),
    gold_location(GL),
    agent_location(AL),

    ( is_pit(yes, AL) -> format("Agent has fallen into a pit ~n", []),
      fail
    ; stnd(AL, GL, WL) 
      %\+ pit_location(yes, Al),
    ).

stnd(AL, GL, WL) :-
    format("Agent continues to search.~n", []).

stnd(AL, _, AL) :-
    format("Agent was eaten", []),
    fail.

stnd(AL, AL, _) :-
    format("Agent found the gold.", []),
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Perceptors

adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).

adjacent( [X1, Y1], [X2, Y2] ) :-
    ( X1 = X2, adj( Y1, Y2 )
    ; Y1 = Y2, adj( X1, X2 )
    ).

isSmelly(Ls1) :-
    wumpus_location( Ls2 ),
    adjacent( Ls1, Ls2 ).

isBleezy(Ls1) :-
    pit_location( Ls2 ),
    adjacent( Ls1, Ls2 ).

isGlittering( [X1, Y1] ) :-
    gold_location( [X2, Y2] ),
    X1 = X2,
    Y1 = Y2.

bleezy(yes) :-
    agent_location(AL),
    isBleezy(AL).
bleezy(no).

smelly(yes) :-
    agent_location(AL),
    isSmelly(AL).
smelly(no).

glittering(yes) :-
    agent_location(AL),
    isGlittering(AL).
glittering(no).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Perceptotion

make_perception([_Stench,_Bleeze,_Glitter]) :-
    agent_location(AL),
    isStinky(AL),
    isBleezie(AL),
    isGlittering(AL).

make_percept_sentence([Stench,Bleeze,Glitter]) :-
	smelly(Stench),
	bleezy(Bleeze),
	glittering(Glitter).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Knowledge Base:

update_KB( [Stench,Bleeze,Glitter] ) :-
    add_wumpus_KB(Stench),
    add_pit_KB(Bleeze),
    add_gold_KB(Glitter).


add_wumpus_KB(no) :-
    agent_location([X,Y]),
    world_size(WS),
    
    Z1 is Y+1, assume_wumpus(no,[X,Z1]),
    Z2 is Y-1, assume_wumpus(no,[X,Z2]),
    Z3 is X+1, assume_wumpus(no,[Z3,Y]),
    Z4 is X-1, assume_wumpus(no,[Z4,Y]).

add_pit_KB(no) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).

add_pit_KB(yes) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).

add_gold_KB(no) :-
    gold_location(GL),
    assume_gold(no, GL).
    
add_gold_KB(yes) :-
    gold_location([X1,Y1]),
    agent_location([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).

assume_wumpus(no, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(no, L) ),
    format("add to KB ~p - no Wumpus~n", [L]).

assume_wumpus(yes, L) :-
    %wumpus_healthy, % Will be included ...
    retractall( isWumpus(_, L) ),
    assert( isWumpus(yes, L) ),
    format("add to KB ~p - Wumpus can be there~n", [L]).

assume_pit(no, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(no, L) ),
    format("add to KB ~p - no Pit~n", [L]).

assume_pit(yes, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(yes, L) ),
    format("add to KB ~p - a Pit can be there~n", [L]).

assume_gold(no, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(no, L) ),
    format("add to KB ~p - no Gold~n", [L]).

assume_gold(yes, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(yes, L) ),
    format("add to KB ~p - Gold Found~n", [L]).

permitted([X,Y]) :-
    world_size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.

ask_KB(VisitedList, Action) :-
    isWumpus(no, L),
    isPit(no, L),
    permitted(L),
    not_member(L, VisitedList),
    update_agent_location(L),
    Action = L.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Updates

update_time :-
    time(T),
    NewTime is T+1,
    retractall( time(_) ),
    assert( time(NewTime) ).

update_score :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    update_score(AL, GL, WL).

update_score(P) :-
    score(S),
    NewScore is S+P,
    retractall( score(_) ),
    assert( score(NewScore) ).

update_score(AL, AL, _) :-
    update_score(1000).

update_score(_,_,_) :-
    update_score(-1).

update_agent_location(NewAL) :-
    agent_location(AL),
    retractall( agent_location(_) ),
    assert( agent_location(NewAL) ).

is_pit(no,  X).
is_pit(yes, X) :-
    pit_location(X).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_member(X, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start of the game

start :-
    format("Initialization... ~n", []),
    init,
    
    format("Start on [1, 1] ~n", []),
    step([[1,1]]).

% Scheduling simulation:

step_pre(VisitedList) :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),

    score(S),
    time(T),

    ( AL=GL -> format("Agent Won~n", []), format("Score: ~p,~n Time: ~p", [S,T])
    ; AL=WL -> format("Agent Lost: Eaten ~n", []),
               format("Score: ~p,~n Time: ~p", [S,T])
    ; step(VisitedList)
    ).

step(VisitedList) :-
    make_percept_sentence(Perception),
    agent_location(AL),
    format("Agent in ~p, seeing: ~p~n", [AL,Perception]),
    
    update_KB(Perception),
    ask_KB(VisitedList, Action),
    format("Agent going to: ~p~n", [Action]),

    update_time,
    update_score,
    
    agent_location(Aloc),
    VL = [Aloc|VisitedList],
    standing,
    step_pre(VL).