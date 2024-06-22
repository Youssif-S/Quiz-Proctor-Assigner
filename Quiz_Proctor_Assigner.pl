
assign_quiz(quiz(Course, Day, Slot, Count), [day(Day2, _)|T], AssignedTAs):-
												Day \= Day2,
												assign_quiz(quiz(Course, Day, Slot, Count), T, AssignedTAs).
	
assign_quiz(quiz(_, Day, Slot, Count), [day(Day, L)|_], AssignedTAs):-
										get_slot(L, Slot, LS, 1),
										length(LS, Length),
										Count =< Length,
										permutate(LS, AssignedTAs, Length, Count).
										
								
get_slot([H|_], A, H, A):- !.
get_slot([_|T], Slot, LS, A):-
							A1 is A + 1,
							get_slot(T, Slot, LS, A1).
					
permutate(LS, AssignedTAs1, C, C):-
							permutation(LS, AssignedTAs1).
permutate(LS, AssignedTAs, Length, Count):-
							Length \= Count,
							Length1 is Length - 1,
							permutate(LS, AssignedTAs1, Length1, Count),
							AssignedTAs1 = [_|AssignedTAs].
							
			




			
free_schedule(_,[],[]).
free_schedule(ALLTAs, [H|T], FreeSchedule):-
									is_off(ALLTAs,H,FT2), FreeSchedule = [FT2|D1],
									free_schedule(ALLTAs,T,D1).
									
is_off([],FT2,FT2).									
is_off([ta(_,Day_Off)|T],day(DayName,DaySche),Free):-Day_Off=DayName,is_off(T,day(DayName,DaySche),Free).
is_off([ta(Name,Day_Off)|T],day(DayName,DaySche),Free):-Day_Off\=DayName,free_tas_in_day(Name,DaySche,FreeTAs),
														is_off(T,day(DayName,FreeTAs),Free).

free_tas_in_day(Name,[H|T2],FreeTAs):-
									member(Name,H),delete(H,Name,FT),
									free_tas_in_day(Name,T2,FreeTAs1),FreeTAs=[FT|FreeTAs1].
									
											
free_tas_in_day(Name,[H|T2],FreeTAs):-
									\+ member(Name,H),append(H,[Name],FT),
									free_tas_in_day(Name,T2,FreeTAs1),FreeTAs=[FT|FreeTAs1].											

free_tas_in_day(_,[],[]).
							
							
		
			
			
assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule) :-
	Quizzes = [quiz(Course, Day, Slot, Count) | Quizzes1 ],
	free(Day, Slot, FreeSchedule, Proctors, Count, FreeSchedule1),
	assign_quizzes(Quizzes1, FreeSchedule1, ProctoringSchedule1),
	ProctoringSchedule = [proctors(quiz(Course, Day, Slot, Count),Proctors) | ProctoringSchedule1].
assign_quizzes([], _, []).

pick(_, 0,[]).
pick([H|L1], Count, [H|L2]) :-
	Count > 0,
	Count1 is Count - 1,
	pick(L1, Count1, L2).

free(Day, Slot, [day(Day1, [S1, S2, S3 ,S4 ,S5] ) | NextDay ], Proctors, Count, D):-
	Day=Day1,(
	Slot=1,Free=S1,D=[day(Day1, [Snew, S2, S3 ,S4 ,S5]) | NextDay ];
	Slot=2,Free=S2,D=[day(Day1, [S1, Snew, S3 ,S4 ,S5]) | NextDay ];
	Slot=3,Free=S3,D=[day(Day1, [S1, S2, Snew ,S4 ,S5]) | NextDay ];
	Slot=4,Free=S4,D=[day(Day1, [S1, S2, S3 ,Snew ,S5]) | NextDay ];
	Slot=5,Free=S5,D=[day(Day1, [S1, S2, S3 ,S4 ,Snew]) | NextDay ]),
	permutation(Free,Free1),
	pick(Free1, Count, Proctors),
	subtract(Free,Proctors,Snew).

free(Day, Slot, [day(Day1, [S1, S2, S3 ,S4 ,S5]) | NextDay], Proctors, Count, [day(Day1, [S1, S2, S3 ,S4 ,S5]) | FreeSchedule1]):-
	dif(Day, Day1),
	free(Day, Slot, NextDay, Proctors, Count, FreeSchedule1).





assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule) :-
		free_schedule(AllTAs, TeachingSchedule, FreeSchedule),
		assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).

