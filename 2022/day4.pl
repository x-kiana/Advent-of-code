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

split_on(_, [], [], []).
split_on(Sep, [Sep | Rest], [], Rest).
split_on(Sep, [First | Rest], [First | RestBefore], After) :-
	dif(First, Sep),
	split_on(Sep, Rest, RestBefore, After).

fully_contained([], [], 0).
fully_contained([FirstFirst | FirstRest], [SecondFirst | SecondRest], Results) :-
	ResultsPrime #= Results - 1,
	((fdset_subset(FirstFirst, SecondFirst); fdset_subset(SecondFirst, FirstFirst)) -> fully_contained(FirstRest, SecondRest, ResultsPrime) ; fully_contained(FirstRest, SecondRest, Results)).

first_solution(File, Solution) :-
	file_lines(File, Lines),
	maplist(string_codes, Lines, LineCodes),
	maplist(split_on(44), LineCodes, FirstPairs, SecondPairs),
	maplist(split_on(45), FirstPairs, FirstLowers, FirstUppers),
	maplist(split_on(45), SecondPairs, SecondLowers, SecondUppers),
	maplist(number_codes, FirstLowerNums, FirstLowers),
	maplist(number_codes, SecondLowerNums, SecondLowers),
	maplist(number_codes, FirstUpperNums, FirstUppers),
	maplist(number_codes, SecondUpperNums, SecondUppers),
	maplist(fdset_interval, FirstIntervals, FirstLowerNums, FirstUpperNums),
	maplist(fdset_interval, SecondIntervals, SecondLowerNums, SecondUpperNums),
	fully_contained(FirstIntervals, SecondIntervals, Solution).

overlapping([], [], 0).
overlapping([FirstFirst | FirstRest], [SecondFirst | SecondRest], Results) :-
	ResultsPrime #= Results - 1,
	(fdset_intersect(FirstFirst, SecondFirst) -> overlapping(FirstRest, SecondRest, ResultsPrime) ; overlapping(FirstRest, SecondRest, Results)).

second_solution(File, Solution) :-
	file_lines(File, Lines),
	maplist(string_codes, Lines, LineCodes),
	maplist(split_on(44), LineCodes, FirstPairs, SecondPairs),
	maplist(split_on(45), FirstPairs, FirstLowers, FirstUppers),
	maplist(split_on(45), SecondPairs, SecondLowers, SecondUppers),
	maplist(number_codes, FirstLowerNums, FirstLowers),
	maplist(number_codes, SecondLowerNums, SecondLowers),
	maplist(number_codes, FirstUpperNums, FirstUppers),
	maplist(number_codes, SecondUpperNums, SecondUppers),
	maplist(fdset_interval, FirstIntervals, FirstLowerNums, FirstUpperNums),
	maplist(fdset_interval, SecondIntervals, SecondLowerNums, SecondUpperNums),
	overlapping(FirstIntervals, SecondIntervals, Solution).
