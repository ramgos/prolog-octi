:- use_module(library(clpfd)).	% Finite domain constraints

base_game(game(
              red,
              [
              	red - 12,
                green - 12
              ],
              [
              	octi(green, (1, 1), []), octi(green, (2, 1), []), octi(green, (3, 1), []), octi(green, (4, 1), []),
              	octi(red, (1, 5), []), octi(red, (2, 5), []), octi(red, (3, 5), []), octi(red, (4, 5), [])
              ]
              )).

get_octi([octi(Team, (X, Y), Vectors) | _], octi(Team, (X, Y), Vectors)).
get_octi([octi(_, (X0, Y0), _) | Rest], octi(Team, (X, Y), Vectors)) :-
    (X, Y) \== (X0, Y0),
    get_octi(Rest, octi(Team, (X, Y), Vectors)).

change_octi((X, Y), [octi(_, (X, Y), _) | Rest], [octi(Team, (X1, Y1), Vectors) | Rest], octi(Team, (X1, Y1), Vectors)).
change_octi((X, Y), [octi(Team0, (X0, Y0), Vectors0) | Rest], [octi(Team0, (X0, Y0), Vectors0) | Cont], octi(Team, (X1, Y1), Vectors)) :-
	(X, Y) \== (X0, Y0),
    change_octi((X, Y), Rest, Cont, octi(Team, (X1, Y1), Vectors)).

add_vectors((X0, Y0), (X1, Y1), (X, Y)) :-
    X #= X0 + X1,
    Y #= Y0 + Y1.

sub_vectors((X0, Y0), (X1, Y1), (X, Y)) :-
    X #= X0 - X1,
    Y #= Y0 - Y1.

midpos((X0, Y0), (X1, Y1), (X, Y)) :-
    X #= (X0 + X1) / 2,
    Y #= (Y0 + Y1) / 2.

next_team(red, green).
next_team(green, red).

map_get([Key - Value | _], Key - Value).
map_get([Key0 - _| Rest], Key - Value) :-
    Key0 \== Key,
    map_get(Rest, Key - Value).

map_set([], [], _ - _).
map_set([Key - _ | Rest], [Key - Value | Cont], Key - Value) :-
    map_set(Rest, Cont, Key - Value).
map_set([Key0 - Value0 | Rest], [Key0 - Value0 | Cont], Key - Value) :-
    Key0 \== Key,
    map_set(Rest, Cont, Key - Value).

% place arrow
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(place, (X, Y), (A, B))
     ) :-
    next_team(Team0, Team1),
    
    % update arrow count
    map_get(Arrows0, Team0 - Team0Arrows),
    NextTeam0Arrows #= Team0Arrows - 1,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),
    
    % check octi doesn't already have arrow
    get_octi(Board0, octi(Team0, (X, Y), Vectors)),
    \+ member((A, B), Vectors),
    
    % update board
    append(Vectors, [(A, B)], NextVectors),
    change_octi((X, Y), Board0, Board1, octi(Team0, (X, Y), NextVectors)).

% move octigon in case no blocking
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1))
     ) :-
	next_team(Team0, Team1),
    
    % check that nothing is blocking
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),
    
    sub_vectors((X1, Y1), (X0, Y0), NeededVector),
    member(NeededVector, Vectors),
    change_octi((X0, Y0), Board0, Board1, octi(Team0, (X1, Y1), Vectors)).

% move octigon in case blocking but of own team (jump no eat)
turn_1(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1))
     ) :-
	next_team(Team0, Team1),
    
    % get position in middle
    midpos((X0, Y0), (X1, Y1), BlockingPos),
    
    % check that own team blocking and nothing ahead blocking
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
	get_octi(Board0, octi(Team0, BlockingPos, _)),
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),
    
    sub_vectors(BlockingPos, (X0, Y0), NeededVector),
    member(NeededVector, Vectors),
    change_octi((X0, Y0), Board0, Board1, octi(Team0, (X1, Y1), Vectors)).
    