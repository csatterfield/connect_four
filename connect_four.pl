% This library gives us access for functions like matrix transpose.
:- use_module(library(clpfd)).

% The initial empty 7 by 6 board represented as a list of columns.
empty_board([[-, -, -, -, -, -, -],
             [-, -, -, -, -, -, -],
             [-, -, -, -, -, -, -],
             [-, -, -, -, -, -, -],
             [-, -, -, -, -, -, -],
             [-, -, -, -, -, -, -],
             [-, -, -, -, -, -, -]]).

% To play the game, explain the rules and start from the empty board.
play :- explain_rules, empty_board(B), play_from(B).

% Play the game, alternating between the user x and the machine o.
play_from(Board) :- win(Board, x), write('Congratulations! You win!').
play_from(Board) :- win(Board, o), write('You lose! Better luck next time.').
play_from(Board) :- full(Board), write('It\'s a tie!').
play_from(Board) :-
    write('Your move:'),
    nl,
    read(N),
    user_moves(Board, N, Board1),
    display_board(Board1),
    write('Machine\'s move:'),
    nl,
    machine_moves(Board1, Board2),
    display_board(Board2),
    play_from(Board2).

% Insert a piece in the column specified by the user.
% TODO
user_moves(Board, N, Board1) :-
    empty_board(Board1).

% Insert a piece in the column by the machine.
% TODO
machine_moves(Board, Board1) :-
    empty_board(Board1).

% Explain how to play the game, and display the empty board.
explain_rules :-
    write('Welcome to Connect 4!'),
    nl,
    write('Enter 1., 2., 3., 4., 5., 6., 7. to insert a piece in that column.'),
    nl,
    empty_board(B),
    display_board(B).

% Display the game board, row by row.
display_board(Board) :-
    transpose(Board, Board1),
    display_board_helper(Board1),
    nl.

% A helper function to print out the matrix.
% TODO beautify
display_board_helper([]) :-
    write('[1,2,3,4,5,6,7]'),
    nl.
display_board_helper([A|R]) :-
    write(A),
    nl,
    display_board_helper(R).

% Winning conditions
win(Board, Player) :- row_win(Board, Player).
win(Board, Player) :- column_win(Board, Player).
win(Board, Player) :- diagonal_right_win(Board, Player).
win(Board, Player) :- diagonal_left_win(Board, Player).

% Player wins if there are 4 in a row of his color on the board
row_win(Board, Player) :-
    transpose(Board, Board1),
    column_win(Board1, Player).

% Player wins if there are 4 in a column of his color on the board.
column_win(Board, Player) :-
    append(_, [Column|_], Board),
    append(_, [Player, Player, Player, Player|_], Column).

% Player wins if there are 4 in a diagonal (\ direction) of his color on the
% board.
diagonal_right_win(Board, Player) :-
    append(_, [Column1, Column2, Column3, Column4|_], Board),
    append(Elements_above1, [Player|_], Column1),
    append(Elements_above2, [Player|_], Column2),
    append(Elements_above3, [Player|_], Column3),
    append(Elements_above4, [Player|_], Column4),
    length(Elements_above1, N1),
    length(Elements_above2, N2),
    length(Elements_above3, N3),
    length(Elements_above4, N4),
    N2 is N1 + 1,
    N3 is N2 + 1,
    N4 is N3 + 1.

% Player wins if there are 4 in a diagonal (/ direction) of his color on the
% board.
diagonal_left_win(Board, Player) :-
    append(_, [Column1, Column2, Column3, Column4|_], Board),
    append(Elements_above1, [Player|_], Column1),
    append(Elements_above2, [Player|_], Column2),
    append(Elements_above3, [Player|_], Column3),
    append(Elements_above4, [Player|_], Column4),
    length(Elements_above1, N1),
    length(Elements_above2, N2),
    length(Elements_above3, N3),
    length(Elements_above4, N4),
    N2 is N1 - 1,
    N3 is N2 - 1,
    N4 is N3 - 1.

% Return true if the board is full.
full(Board) :-
    \+ (append(_, [Column|_], Board),
        append(_, [-|_], Column)).
