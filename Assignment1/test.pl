% FILEPATH: /code/concordia/SOEN-331/test.pl

% Facts
likes(john, pizza).
likes(mary, sushi).
likes(john, sushi).
likes(mary, chocolate).

% Rules
friend(X, Y) :- likes(X, Z), likes(Y, Z).

% Queries
?- friend(john, mary).
