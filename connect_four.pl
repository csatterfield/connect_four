% This library gives us access for functions like matrix transpose.
:- use_module(library(clpfd)).
% This library allows us to use list operations such as nth.
:- use_module(library(lists)).

% The initial empty 7 by 6 board represented as a list of columns.
empty_board([['-', '-', '-', '-', '-', '-'],
             ['-', '-', '-', '-', '-', '-'],
             ['-', '-', '-', '-', '-', '-'],
             ['-', '-', '-', '-', '-', '-'],
             ['-', '-', '-', '-', '-', '-'],
             ['-', '-', '-', '-', '-', '-'],
             ['-', '-', '-', '-', '-', '-']]).

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
	valid_move(Board, N),
	insert_into_board(Board, x, N, Board1).
user_moves(Board, N, Board1) :-
	write('Sorry, thats an invalid move. Please try again:'),
	nl,
    read(N1),
	user_moves(Board, N1, Board1).

% Check if inserting a piece in column N is a valid move
valid_move(Board, N) :-
	integer(N),
	N >= 1,
	N =< 7,
	nth1(N, Board, C),
	valid_move_column(C).
valid_move_column([H|_]) :- H = '-'.

% Insert a piece into a column of the board
% True if Board1 is Board with an additional entry of Colour in Column
insert_into_board(Board, Colour, Column, Board1) :-
	nth1(Column, Board, C), % gives us the desired Column of Board as C
	insert_into_column(C, Colour, C2),
	update_board(Board, Column, C2, Board1).

% insert_into_column(Column, Colour, Result)
% Insert a piece into a specificed column
% True if Result is Column with a new entry of type Colour inserted
insert_into_column(['-',X|T], Colour, [Colour,X|T]) :- \+X = '-'.
insert_into_column(['-'], Colour, [Colour]).
insert_into_column([H|T], Colour, [H|R]) :- insert_into_column(T, Colour, R).

% update_board(Board1, N, C2, Board2)
% Replaces the Nth column in Board1 with C2.
% True if Board2 is Board1 with the Nth column replaced by C2.
update_board([H|T], 1, C, [C|T]).
update_board([H|T], N, C, [H|R]) :- N2 is N-1, update_board(T, N2, C, R).

% Insert a piece in the column by the machine.
% TODO
:- use_module(library(random)). % temporary only
% picks a column at random
machine_moves(Board, Board1) :-
    random(1,8,N),
	valid_move(Board, N),
	insert_into_board(Board, o, N, Board1).
machine_moves(Board, Board1) :- machine_moves(Board,Board1). % tries again until it works.

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
