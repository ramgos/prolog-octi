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

change_octi([octi(_, (X, Y), _) | Rest], [octi(Team, (X, Y), Vectors) | Rest], octi(Team, (X, Y), Vectors)).
change_octi([octi(Team0, (X0, Y0), Vectors0) | Rest], [octi(Team0, (X0, Y0), Vectors0) | Cont], octi(Team, (X, Y), Vectors)) :-
	(X, Y) \== (X0, Y0),
    change_octi(Rest, Cont, octi(Team, (X, Y), Vectors)).

add_pos((X0, Y0), (X1, Y1), (X, Y)) :-
    X is X0 + X1,
    Y is Y0 + Y1.

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

turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(place, (X, Y), (A, B))
     ) :-
    next_team(Team0, Team1),
    map_get(Arrows0, Team0 - Team0Arrows),
    NextTeam0Arrows is Team0Arrows - 1,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),
    get_octi(Board0, octi(Team0, (X, Y), Vectors)),
    \+ member((A, B), Vectors),
    append(Vectors, [(A, B)], NextVectors),
    change_octi(Board0, Board1, octi(Team0, (X, Y), NextVectors)).


turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(place, (X, Y), (A, B))
     ) :-