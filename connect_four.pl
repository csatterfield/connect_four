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

%
%/ Machine selects a move using the Negamax algorithm
%
% Insert a piece in the column by the machine.
% TODO
:- use_module(library(random)). % temporary only
% picks a column at random
machine_moves(Board, Board) :- win(Board, x).
machine_moves(Board, Board1) :-
	best_move(Board, 4, o, Move),
	insert_into_board(Board, o, Move, Board1).

opponent(x,o).
opponent(o,x).

% calculates the best possible Move for Player given Board
% Depth is the max depth to recursively search for possible moves
best_move(Board, Depth, Player, Move) :-
	best_score(Board, Player, Depth, 1, Score1),
	best_score(Board, Player, Depth, 2, Score2),
	best_score(Board, Player, Depth, 3, Score3),
	best_score(Board, Player, Depth, 4, Score4),
	best_score(Board, Player, Depth, 5, Score5),
	best_score(Board, Player, Depth, 6, Score6),
	best_score(Board, Player, Depth, 7, Score7),
	select_best_move([Score1, Score2, Score3, Score4, Score5, Score6, Score7], Board, Move).


% select the score that minimizes your opponents score (maximizes your score)	
select_best_move(ScoreList, _, Move) :-
	select_min_score(ScoreList, Score),
	print(ScoreList),
	nl,
	nth1(Move, ScoreList, Score).


% calculates the best possible score if the player chooses column N.
best_score(_,_,0,_,-42).
best_score(Board,_,_,_,0) :- full(Board).
best_score(Board, Player, Depth, N, -42) :- \+ valid_move(Board, N).
best_score(Board,o,_,_,Score) :- 
	can_win(Board, o),
	board_moves_left(Board,NM),
	Score is -1*((NM + 1)//2). 			  		  % Your score is the number of moves
												  %	before the board would be full at the time when you win.
best_score(Board,x,_,_,Score) :- 
	can_win(Board, x),
	board_moves_left(Board,NM),
	Score is -1*((NM + 1)//2). 			  		  % Your score is the number of moves
												  %	before the board would be full at the time when you win.												  

best_score(Board, Player, Depth, N, Score) :-
	valid_move(Board,N),
	insert_into_board(Board, Player, N, Board1),
	opponent(Player, P2),
	D2 is Depth - 1,
	best_score(Board1, P2, D2, 1, OS1),
	best_score(Board1, P2, D2, 2, OS2),
	best_score(Board1, P2, D2, 3, OS3),
	best_score(Board1, P2, D2, 4, OS4),
	best_score(Board1, P2, D2, 5, OS5),
	best_score(Board1, P2, D2, 6, OS6),
	best_score(Board1, P2, D2, 7, OS7),
	select_max_score([OS1,OS2,OS3,OS4,OS5,OS6,OS7], Score).

	
% Select the minimum score from a list
select_min_score([], 42).
select_min_score([H|T], Min) :- 
	select_min_score(T, M2),
	Min is min(H,M2).
	
% Select the max score from a list of scores
select_max_score([], -42).
select_max_score([H|T], Max) :- 
	select_max_score(T, M2),
	Max is max(H,M2).

% Get the number of moves until the board is full
board_moves_left([], 0).
board_moves_left([H|T], Moves) :-
	col_moves_left(H, M1),
	board_moves_left(T, M2),
	Moves is M1 + M2.
	
col_moves_left([], 0).
col_moves_left([X|T], 0) :- dif(X,'-').
col_moves_left(['-'|T], M) :-
	col_moves_left(T, M1),
	M is M1 + 1.
	
can_win(Board,Player) :- valid_move(Board, 1), insert_into_board(Board,Player,1,Board1), win(Board1, Player).
can_win(Board,Player) :- valid_move(Board, 2), insert_into_board(Board,Player,2,Board1), win(Board1, Player).
can_win(Board,Player) :- valid_move(Board, 3), insert_into_board(Board,Player,3,Board1), win(Board1, Player).
can_win(Board,Player) :- valid_move(Board, 4), insert_into_board(Board,Player,4,Board1), win(Board1, Player).
can_win(Board,Player) :- valid_move(Board, 5), insert_into_board(Board,Player,5,Board1), win(Board1, Player).
can_win(Board,Player) :- valid_move(Board, 6), insert_into_board(Board,Player,6,Board1), win(Board1, Player).
can_win(Board,Player) :- valid_move(Board, 7), insert_into_board(Board,Player,7,Board1), win(Board1, Player).