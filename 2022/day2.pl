:- use_module(library(clpfd)).

file_lines(File, Lines) :-
	setup_call_cleanup(open(File, read, In),
		stream_lines(In, Lines),
		close(In)
	).

stream_lines(Stream, Lines) :-
	read_line_to_string(Stream, Line),
	(Line = end_of_file -> Lines = [];
	 Lines = [Line|Rest], stream_lines(Stream, Rest)
	).

round_score("A X", 4).
round_score("A Y", 8).
round_score("A Z", 3).
round_score("B X", 1).
round_score("B Y", 5).
round_score("B Z", 9).
round_score("C X", 7).
round_score("C Y", 2).
round_score("C Z", 6).

round_2_score("A X", 3).
round_2_score("A Y", 4).
round_2_score("A Z", 8).
round_2_score("B X", 1).
round_2_score("B Y", 5).
round_2_score("B Z", 9).
round_2_score("C X", 2).
round_2_score("C Y", 6).
round_2_score("C Z", 7).

sum([], 0).
sum([First | Rest], Sum) :-
       Sum #= First + SumRest,
       sum(Rest, SumRest).

first_solution(Input, Solution) :-
	file_lines(Input, Lines),
	maplist(round_score, Lines, Scores),
	sum(Scores, Solution).

second_solution(Input, Solution) :-
	file_lines(Input, Lines),
	maplist(round_2_score, Lines, Scores),
	sum(Scores, Solution).
