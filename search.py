from pyswip import Prolog

prolog = Prolog()
prolog.consult("octi/octi10.pl")


def getrep(query_result):
    return query_result['Rep'].decode('ascii')


def base_game():
    return getrep(list(prolog.query(
        "base_game(Game), \
         term_string(Game, Rep)"
    ))[0])

seen = set()

def next_games(game):
    games = []
    print(game)
    q = prolog.query(
        f"turn_1({game}, Game, _),\
          label_game(Game), \
          term_string(Game, Rep)"
    )
    for index, sol in enumerate(q):
        rep = getrep(sol)
        if rep not in seen:
            games.append(rep)
            seen.add(rep)
        
    return games


def main():
    base = base_game()
    seen.add(base)

    nextgames = next_games(base)
    for game in nextgames:
        print(game)


if __name__ == "__main__":
    main()
