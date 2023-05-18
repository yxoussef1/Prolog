build_kb:- write('Please enter a word and its category on separate lines:'),nl,
    read(Word),((Word = done,write('Done building the words database...'),nl);(read(Category),
    assert(word(Word,Category)),build_kb)).
    
is_category(C):- word(_,C).

categories(L):-
setof(X,is_category(X),L).

available_length(Length):-
categories(X),exist(Length,X).

exist(Length,[H|T]):-
atom_chars(H,Newlist),(length(Newlist,Length);exist(Length,T)).

pick_word(W,L,C):-
word(W,C),
exist(L, [W]).
removedup([],[]).
removedup([H|T],L):-
member(H,T),removedup(T,L).
removedup([H|T],L):-
\+member(H,T),removedup(T,L1),L=[H|L1].
correct_letters(L1,L2,CL):-
intersection(L1,L2,L),removedup(L,CL).

correct_positions([],X,[]):-
!.
correct_positions(X,[],[]):-
!.
correct_positions([H|T],[H|T1],[H|T3]):-
correct_positions(T,T1,T3).

correct_positions([X|T],[H|T1],T3):-
X\==H,
correct_positions(T,T1,T3).

play:- write('The available categories are: '), 
categories(L),write(L),nl,write('Choose a category: '),
nl, read(X),check_cat(X,Z),write('Choose a length: '),nl,read(F),check_len(F,P,Z),write('Game started. You have '),P1 is P+1,write(P1),write(' guesses.'),nl,pick_word(W,P,Z),game(P1,W,P).



check_cat(X,X):-
is_category(X).
check_cat(X,Y):-
\+is_category(X),write('There are no words of this category.'),nl,read(Z),check_cat(Z,Y).

check_len(X,X,C):-
pick_word(_,X,C).
check_len(X,Y,C):-
\+pick_word(_,X,C),write('There are no words of this length.'),nl,read(Z),check_len(Z,Y,C).

game(Acc_G,Ref,Length):- nl,write('Enter a word composed of '),write(Length),write(' letters:'),nl,read(X),((exist(Length,[X]),X\==Ref , 
atom_chars(Ref,L1),atom_chars(X,L2),correct_letters(L1,L2,L3),Acc_G1 is Acc_G-1,Acc_G1 \==0,write('Correct letters are: '),write(L3),nl,
correct_positions(L1,L2,L4),write('Correct letters in correct positions are: '),write(L4),nl,write('Remaining Guesses are '),write(Acc_G1),nl,game(Acc_G1,Ref,Length));(\+exist(Length,[X]),write('Word is not composed of '),write(Length),write(' letters. Try again.'),nl,
write('Remaining Guesses are '),write(Acc_G),nl,game(Acc_G,Ref,Length));X=Ref,write('You Won!');(Acc_G1 is Acc_G-1,Acc_G1==0,write('You Lost!'))).




main:-
write('Welcome to Pro-Wordle!'),nl,write('----------------------'),nl,nl,build_kb,play.
