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

gather_vectors([], []).
gather_vectors([(A, B) | Rest], Gathered) :-
    gather_vectors(Rest, PreGathered),
    append([A, B], PreGathered, Gathered).

gather_board([], []).
gather_board([octi(_, (X, Y), Vectors) | Rest], Gathered) :-
    gather_vectors(Vectors, GatheredVectors),
    gather_board(Rest, PreGathered),
    append([[X, Y], PreGathered, GatheredVectors], Gathered).

label_game(game(_, _, Board)) :-
    gather_board(Board, Gather),
    labeling([ff], Gather).

get_octi([octi(Team, (X, Y), Vectors) | _], octi(Team, (X, Y), Vectors)).
get_octi([octi(_, (X0, Y0), _) | Rest], octi(Team, (X, Y), Vectors)) :-
    dif((X0, Y0), (X, Y)),
    get_octi(Rest, octi(Team, (X, Y), Vectors)).

change_octi((X, Y), [octi(_, (X, Y), _) | Rest], [octi(Team, (X1, Y1), Vectors) | Rest], octi(Team, (X1, Y1), Vectors)).
change_octi((X, Y), [octi(Team0, (X0, Y0), Vectors0) | Rest], [octi(Team0, (X0, Y0), Vectors0) | Cont], octi(Team, (X1, Y1), Vectors)) :-
    dif((X0, Y0), (X, Y)),
    change_octi((X, Y), Rest, Cont, octi(Team, (X1, Y1), Vectors)).


add_vectors((X0, Y0), (X1, Y1), (X, Y)) :-
    X #= X0 + X1,
    Y #= Y0 + Y1.

sub_vectors((X0, Y0), (X1, Y1), (X, Y)) :-
    X #= X0 - X1,
    Y #= Y0 - Y1.

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
        move(place, (X, Y), (A, B)),
        meta([])
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
    get_octi(Board0, octi(Team0, (X, Y), Vectors)),
    maplist(dif((A, B)), Vectors),

    % update board
    append(Vectors, [(A, B)], NextVectors),
    change_octi((X, Y), Board0, Board1, octi(Team0, (X, Y), NextVectors)).

% move octigon in case no blocking
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1)),
        meta([])
     ) :-
    next_team(Team0, Team1),
    valid_pos((X0, Y0)), valid_pos((X1, Y1)),

    % get octi
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    member(NeededVector, Vectors),

    sub_vectors((X1, Y1), (X0, Y0), NeededVector),

    % check that nothing is blocking
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),

    change_octi((X0, Y0), Board0, Board1, octi(Team0, (X1, Y1), Vectors)).

% move octigon in case blocking but of own team (jump no eat)
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1)),
        meta([jump])
     ) :-
    next_team(Team0, Team1),
    valid_pos((X0, Y0)), valid_pos((X1, Y1)),

    % get octi
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    member(NeededVector, Vectors),

    % get blocking pos
    sub_vectors(BlockingPos, (X0, Y0), NeededVector),
    add_vectors(BlockingPos, NeededVector, (X1, Y1)),

    % check that own team blocking and no octigon after jump
    get_octi(Board0, octi(Team0, BlockingPos, _)),
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),

    change_octi((X0, Y0), Board0, Board1, octi(Team0, (X1, Y1), Vectors)).

% move octigon in case blocking but of other team (jump and eat)
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(move, (X0, Y0), (X1, Y1)),
        meta([jump, eat])
     ) :-
    next_team(Team0, Team1),
    valid_pos((X0, Y0)), valid_pos((X1, Y1)),

    % get octi
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    member(NeededVector, Vectors),

    % get blocking pos
    sub_vectors(BlockingPos, (X0, Y0), NeededVector),
    add_vectors(BlockingPos, NeededVector, (X1, Y1)),

    % get arrow count of octi at blocking pos
    get_octi(Board0, octi(Team1, BlockingPos, OtherArrows)),

    % update arrow count
    length(OtherArrows, AddedArrows),
    map_get(Arrows0, Team0 - Team0Arrows),
    NextTeam0Arrows #= Team0Arrows + AddedArrows,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),

    \+ get_octi(Board0, octi(_, (X1, Y1), _)),

    change_octi((X0, Y0), Board0, Board0_1, octi(Team0, (X1, Y1), Vectors)),

    % remove eaten octi
    select(octi(_, BlockingPos, _), Board0_1, Board1).

% chain move
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        [move(move, (X0, Y0), (X1, Y1)) | Rest],
        meta([chain])
     ) :-
    turn_1(
            game(Team0, Arrows0, Board0),
            game(Team1, Arrows0_1, Board0_1),
            move(move, (X0, Y0), (X1, Y1)),
            meta(Meta)
        ),
    member(jump, Meta),
    turn_1(
            game(Team0, Arrows0_1, Board0_1),
            game(Team1, Arrows1, Board1),
            Rest,
            meta(ChainMeta)
        ),
    member(chain, ChainMeta).

% chain end
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        [move(move, (X0, Y0), (X1, Y1))],
        meta([chain])
     ) :-
    turn_1(
            game(Team0, Arrows0, Board0),
            game(Team1, Arrows1, Board1),
            move(move, (X0, Y0), (X1, Y1)),
            meta(Meta)
        ),
    member(jump, Meta).

win(game(_, _, Octis), Team) :-
    next_team(Team, OtherTeam),
    \+ get_octi(Octis, octi(OtherTeam, _, _)).

win(game(_, _, Octis), Team) :-
    winpos(Team, WinPos),
    member(Pos, WinPos),
    get_octi(Octis, octi(Team, Pos, _)).

win(game(Team1, Arrows, Octis), Team0) :-
    next_team(Team0, Team1),
    findall([game(Team1, Arrows, Octis), _, _, _], turn_1(game(Team1, Arrows, Octis), _, _, _), []).

rwin(Team, Game) :- win(Game, Team).

any(Goal, [Arg | Rest]) :-
    call(Goal, Arg);
    any(Goal, Rest).

remove_from_list_1(_, [], []).
remove_from_list_1(X, [X | Rest], Cont) :-
    remove_from_list_1(X, Rest, Cont).
remove_from_list_1(X, [Y | Rest], [Y | Cont]) :-
    dif(X, Y),
    remove_from_list_1(X, Rest, Cont).

remove_from_list([], X, X).
remove_from_list([X | Rest], L0, L) :-
    remove_from_list_1(X, L0, L1),
    remove_from_list_1(Rest, L1, L).

wins_1(Game, Team, _, Depth) :-
    write(Depth), nl, nl,

    win(Game, Team).

wins_1(Game0, Team, _, Depth) :-
    write(Depth), nl, nl,
    
    findall(Game1, turn_1(Game0, Game1, _, _), Games),
    % maplist(label_game, Games),

    any(rwin(Team), Games).

wins_1(Game0, Team, CachedGames, Depth) :-
    write(Depth), nl, nl,
    NextDepth is Depth + 1,

    findall(Game1, turn_1(Game0, Game1, _, _), Games),
    % maplist(label_game, Games),

    remove_from_list(CachedGames, Games, FilteredGames),
    append(FilteredGames, Games, NewCachedGames),
    maplist(rwins_1(NextDepth, NewCachedGames, Team), Games).

rwins_1(Depth, CachedGames, Team, Game) :- wins_1(Game, Team, CachedGames, Depth).

wins(Game, Team) :-
    wins_1(Game, Team, [], 0).