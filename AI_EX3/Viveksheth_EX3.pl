/* Facts  */
person(alice).
person(husband).
person(son).
person(daughter).
person(brother).

child(son).
child(daughter).

male(husband).
male(son).
male(brother).

female(alice).
female(daughter).

twin(alice,    brother).
twin(brother,  alice).
twin(son,      daughter).
twin(daughter, son).


/*Rules*/
istwin(X) :- twin(X, _).
older(alice,   son).
older(alice,   daughter).
older(husband, son).
older(husband, daughter).
inbar(M, N) :- person(M), person(N),
               male(M),   female(N).

together(S, T) :- S=alice, T=husband.
together(S, T) :- T=alice, S=husband.

alone(P) :- person(P), child(P).


/*Hints*/
solution(Killer, Victim, InBarA, InBarB, Alone) :-
    person(Killer), person(Victim), 

   /* A man and a woman were together in the 
       at the time of the murder. */
        inbar(InBarA, InBarB),
	InBarA \= Killer, InBarB \= Killer,
	InBarA \= Victim, InBarB \= Victim,

   /* One of the children was alone at the 
       time of the murder. */
	alone(Alone),
	Alone \= InBarA, Alone \= InBarB,
	Alone \= Killer, Alone \= Victim, 
    
    /* Alice and her husband were not together 
       at the time of the murder. */
        \+ together(Killer, Victim), Killer \= Victim,

    /* Alice and her husband were not together 
       at the time of the murder. */
        \+ together(InBarA, InBarB),
    
    /* The victim's twin was innocent. */
        istwin(Victim), \+ twin(Victim, Killer), 

    /* The killer was younger than the victim. */
	\+ older(Killer, Victim).
	   

/* Solution*/
print_solution :-
    
    solution(Killer, Victim, InBarA, InBarB, Alone),
	nl, write(Killer), write(' killed '), write(Victim), write('.'), nl,
	write(InBarA), write(' and '), write(InBarB),
	write(' were together in the bar.'), nl,
	write(Alone), write(' was alone.'), nl, nl.

?- print_solution.    