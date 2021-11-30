:- use_module(library(clpfd)).	% Finite domain constraints

width(6).
height(7).

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
turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(place, (X, Y), (A, B))
     ) :-
    next_team(Team0, Team1),
    
    % update arrow count
    map_get(Arrows0, Team0 - Team0Arrows),
    Team0Arrows #> 0,
    NextTeam0Arrows #= Team0Arrows - 1,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),
    
    % check octi doesn't already have arrow
    get_octi(Board0, octi(Team0, (X, Y), Vectors)),
    \+ member((A, B), Vectors),
    
    % update board
    append(Vectors, [(A, B)], NextVectors),
    change_octi((X, Y), Board0, Board1, octi(Team0, (X, Y), NextVectors)).

% move octigon in case no blocking
turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1))
     ) :-
	next_team(Team0, Team1),
    width(W), height(H),
    X1 #>= 0, X1 #< W,
    Y1 #>= 0, Y1 #< H,
    
    % check that nothing is blocking
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),
    
    sub_vectors((X1, Y1), (X0, Y0), NeededVector),
    member(NeededVector, Vectors),
    change_octi((X0, Y0), Board0, Board1, octi(Team0, (X1, Y1), Vectors)).

% move octigon in case blocking but of own team (jump no eat)
turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board1),
        move(move, (X0, Y0), (X1, Y1))
     ) :-
	next_team(Team0, Team1),
    width(W), height(H),
    X1 #>= 0, X1 #< W,
    Y1 #>= 0, Y1 #< H,
    
    % check that own team blocking and nothing ahead blocking
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    
    % get blocking pos
    sub_vectors(BlockingPos, (X0, Y0), NeededVector),
    add_vectors(BlockingPos, NeededVector, (X1, Y1)),
    
	get_octi(Board0, octi(Team0, BlockingPos, _)),
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),
    
    member(NeededVector, Vectors),
    change_octi((X0, Y0), Board0, Board1, octi(Team0, (X1, Y1), Vectors)).

% move octigon in case blocking but of other team (jump and eat)
turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        move(move, (X0, Y0), (X1, Y1))
     ) :-
	next_team(Team0, Team1),
    width(W), height(H),
    X1 #>= 0, X1 #< W,
    Y1 #>= 0, Y1 #< H,
    
    % check that own team blocking and nothing ahead blocking
    get_octi(Board0, octi(Team0, (X0, Y0), Vectors)),
    
    % get blocking pos
    sub_vectors(BlockingPos, (X0, Y0), NeededVector),
    add_vectors(BlockingPos, NeededVector, (X1, Y1)),
    
    % update arrow count
    get_octi(Board0, octi(Team1, BlockingPos, OtherArrows)),
    length(OtherArrows, AddedArrows),
    map_get(Arrows0, Team0 - Team0Arrows),
    NextTeam0Arrows #= Team0Arrows + AddedArrows,
    map_set(Arrows0, Arrows1, Team0 - NextTeam0Arrows),
    \+ get_octi(Board0, octi(_, (X1, Y1), _)),
    
    member(NeededVector, Vectors),
    change_octi((X0, Y0), Board0, Board0_1, octi(Team0, (X1, Y1), Vectors)),
    % remove eaten octi
    select(octi(_, BlockingPos, _), Board0_1, Board1).

% chain moves
turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows0, Board0),
        []
     ) :-
    next_team(Team0, Team1).

turn(
        game(Team0, Arrows0, Board0),
        game(Team1, Arrows1, Board1),
        [move(move, (X0, Y0), (X1, Y1)) | Rest]
     ) :-
    turn(
        	game(Team0, Arrows0, Board0),
        	game(Team1, Arrows0_1, Board0_1),
        	move(move, (X0, Y0), (X1, Y1))
        ),
    Game1_0 = game(Team0, Arrows0_1, Board0_1),
    turn(
        	Game1_0,
        	game(Team1, Arrows1, Board1),
        	Rest
        ).
    
    
    