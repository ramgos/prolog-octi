:- use_module(library(clpfd)).	% Finite domain constraints
:- use_module(library(dif)).	% Sound inequality

width(5).
height(6).
next_team(red, green).
next_team(green, red).
winpos(red, [(1, 1), (2, 1), (3, 1), (4, 1)]).
winpos(green, [(1, 5), (2, 5), (3, 5), (4, 5)]).

base_game(game(
              red,
              [
		red - 12,
                green - 12
              ],
              [
		octi(green, (1, 1), []), octi(green, (2, 1), []), octi(green, (3, 1), []), octi(green, (4, 1), []),
		octi(red, (1, 5), []), octi(red, (1, 4), []), octi(red, (3, 5), []), octi(red, (4, 5), [])
              ]
              )).

label_game(game(_, _, Board)) :-
    term_variables(Board, Vars),
    labeling([ff], Vars).

add_vectors((X0, Y0), (X1, Y1), (X, Y)) :-
    X #= X0 + X1,
    Y #= Y0 + Y1.

map_get([Key - Value | _], Key - Value).
map_get([Key0 - _| Rest], Key - Value) :-
    dif(Key0, Key),
    map_get(Rest, Key - Value).

map_set([], [], _ - _).
map_set([Key - _ | Rest], [Key - Value | Cont], Key - Value) :-
    map_set(Rest, Cont, Key - Value).
map_set([Key0 - Value0 | Rest], [Key0 - Value0 | Cont], Key - Value) :-
    dif(Key0, Key),
    map_set(Rest, Cont, Key - Value).


valid_pos((X, Y)) :-
    width(W), height(H),
    X in 0..W, Y in 0..H.

valid_vector((A, B)) :-
    A in -1..1, B in -1..1,
    abs(A) + abs(B) #\= 0.

% place arrow
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(place, (X, Y), (A, B))
     ) :-
    next_team(Team0, Team1),
    valid_pos((X, Y)),
    valid_vector((A, B)),

    % update arrow count
    map_get(Arrows0, Team0 - Team0Arrows),
    Team0Arrows #> 0,
    NextTeam0Arrows #= Team0Arrows - 1,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),

    % check octi doesn't already have arrow
    select(octi(Team0, (X, Y), Vectors), Board0, State0),
    maplist(dif((A, B)), Vectors),

    % update board
    append(Vectors, [(A, B)], NextVectors),
    append(State0, [octi(Team0, (X, Y), NextVectors)], Board1).

% move octigon in case no blocking
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1))
     ) :-
    next_team(Team0, Team1),
    valid_pos((X0, Y0)), valid_pos((X1, Y1)),

    % get octi
    select(octi(Team0, (X0, Y0), Vectors), Board0, State0),
    member(NeededVector, Vectors),

    add_vectors(NeededVector, (X0, Y0), (X1, Y1)),

    % check that nothing is blocking
    \+ select(octi(_, (X1, Y1), _), Board0, _),

    append(State0, [octi(Team0, (X1, Y1), Vectors)], Board1).

% move octigon in case blocking but of own team (jump no eat)
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(jump, (X0, Y0), (X1, Y1))
     ) :-
    next_team(Team0, Team1),
    valid_pos((X0, Y0)), valid_pos((X1, Y1)),

    % get octi
    select(octi(Team0, (X0, Y0), Vectors), Board0, State0),
    member(NeededVector, Vectors),

    % get blocking pos
    add_vectors(NeededVector, (X0, Y0), BlockingPos),
    add_vectors(BlockingPos, NeededVector, (X1, Y1)),

    % check that own team blocking and no octigon after jump
    select(octi(Team0, BlockingPos, _), Board0, _),
    \+ select(octi(_, (X1, Y1), _), Board0, _),

    append(State0, [octi(Team0, (X1, Y1), Vectors)], Board1).

% move octigon in case blocking but of other team (jump and eat)
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(jump, (X0, Y0), (X1, Y1))
     ) :-
    next_team(Team0, Team1),
    valid_pos((X0, Y0)), valid_pos((X1, Y1)),

    % get octi
    select(octi(Team0, (X0, Y0), Vectors), Board0, State0),
    member(NeededVector, Vectors),

    % get blocking pos
    add_vectors(NeededVector, (X0, Y0), BlockingPos),
    add_vectors(BlockingPos, NeededVector, (X1, Y1)),

    % get arrow count of octi at blocking pos
    select(octi(Team1, BlockingPos, OtherArrows), Board0, _),

    % update arrow count
    length(OtherArrows, AddedArrows),
    map_get(Arrows0, Team0 - Team0Arrows),
    NextTeam0Arrows #= Team0Arrows + AddedArrows,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),

    % check that no octigon after jump
    \+ select(octi(_, (X1, Y1), _), Board0, _),

    append(State0, [octi(Team0, (X1, Y1), Vectors)], State1),

    % remove eaten octi
    select(octi(_, BlockingPos, _), State1, Board1).

% chain move 2
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(chain, [(X0, Y0), (X1, Y1), (X2, Y2)])
     ) :-
    turn_1(
            game(Team0, Arrows0, Board0),
            game(Team1, Arrows0_1, Board0_1),
            move(jump, (X0, Y0), (X1, Y1))
        ),
    turn_1(
            game(Team0, Arrows0_1, Board0_1),
            game(Team1, Arrows1, Board1),
            move(jump, (X1, Y1), (X2, Y2))
        ).

% chain move N
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(chain, [(X0, Y0), (X1, Y1), (X2, Y2) | Cont])
     ) :-
    turn_1(
            game(Team0, Arrows0, Board0),
            game(Team1, Arrows0_1, Board0_1),
            move(jump, (X0, Y0), (X1, Y1))
        ),
    turn_1(
            game(Team0, Arrows0_1, Board0_1),
            game(Team1, Arrows1, Board1),
            move(chain, [(X1, Y1), (X2, Y2) | Cont])
         ).

% team0 wins if team1 doesn't have any moves
win(game(_, _, Board), Team0) :-
    next_team(Team0, Team1),
    \+ select(octi(Team1, _, _), Board, _).

% team wins if one of the octis are in winpos
win(game(_, _, Board), Team) :-
    winpos(Team, WinPos),
    member(Pos, WinPos),
    select(octi(Team, Pos, _), Board, _).

% team0 wins if team1 at their turn doesn't have any move
win(game(Team1, Arrows, Board), Team0) :-
    next_team(Team0, Team1),
    findall(game(Team1, Arrows, Board), turn_1(game(Team1, Arrows, Board), _, _), []).

any(Goal, [Arg | Rest]) :-
    call(Goal, Arg);
    any(Goal, Rest).

% Team wins the game if the game is already won
wins(Game, Team) :- win(Game, Team).

% Team wins the game if it is currently their turn, 
% and they have at least one move that results in a game where they win
wins(game(Team, Arrows, Board), Team) :-
    findall(Game1, turn_1(game(Team, Arrows, Board), Game1, _), Games),
    any(rwins(Team), Games).

% Team0 wins the game if it isn't their turn, 
% and all possible games that come after are won by Team0 
wins(game(Team1, Arrows, Board), Team0) :-
    next_team(Team0, Team1),
    findall(Game1, turn_1(game(Team1, Arrows, Board), Game1, _), Games),
    maplist(rwins(Team0), Games).

rwins(Team, Game) :- wins(Game, Team).

tabwritenl(X) :-
    write('  '), write(X), nl.

print_game(game(Team, Arrows, Board)) :-
    nl,
    write('turn: '), write(Team), nl,
    write('arrows: '), write(Arrows), nl,
    write('octis: '), nl,
    maplist(tabwritenl, Board), nl.

