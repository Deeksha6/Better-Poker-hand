%Poker hand - to find the best poker hand among 2 given hands. Each hand here has 5 cards.

%substitute/4
substitute(_,_,[],[]).
substitute(X,Y,[X],[Y]).
substitute(X,Y,[X|T1],[Y|T2]) :- substitute(X,Y,T1,T2).
substitute(X,Y,[H|T1],[H|T2]) :- X\=H,substitute(X,Y,T1,T2).

if(P,Q,R) :- P,!,Q.
if(P,Q,R) :- R.

append([],Y,Y).
append([H|T],Y,[H|Z]) :- append(T,Y,Z).

append([],[diamonds,spades,clubs,heart],Slist).
append([],[1,2,3,4,5,6,7,8,9,10,ace,king,jack,queen],Rlist).

card(X,Y) :- member(X,Slist),member(Y,Rlist).

type_of_list(L,M) :- straight_flush(L),M is 1.
type_of_list(L,M) :- four_of_a_kind(L),M is 2.
type_of_list(L,M) :- has_full_house(L),M is 3.
type_of_list(L,M) :- has_straight(L),M is 4.
type_of_list(L,M) :- has_flush(L),M is 5.
type_of_list(L,M) :- three_of_a_kind(L),M is 6.
type_of_list(L,M) :- two_pairs(L),M is 7.
type_of_list(L,M) :- if(one_pair(L),M is 8,M is 9).


compare_type(X,Y,Handa,Handb,Hand) :- if(X<Y,append([],Handa,Hand),append([],Handb,Hand)).

sort_l([H|T],L) :-sort_p([H|T],L1),sort_p(L1,L2),if(L1 == L2,append([],L2,L),sort_l(L2,L)).
sort_p([X],[X]).
sort_p([card(A1,B1),card(A2,B2)|T],L) :- B1>B2,append([card(A2,B2)],L1,L),sort_p([card(A1,B1)|T],L1).
sort_p([card(A1,B1),card(A2,B2)|T],L) :- B1=<B2,append([card(A1,B1)],L1,L),sort_p([card(A2,B2)|T],L1).

suit(card(X,_),X).
rank(card(_,V),V).

has_flush([H|T]) :- suit(H,S),has_flush(T,S).
has_flush([],_).
has_flush([H|T],S) :- suit(H,S),has_flush(T,S).

same_rank([H|T]) :- rank(H,R),same_rank(T,R).
same_rank([],_).
same_rank([H|T],R) :- rank(H,R),same_rank(T,R).

has_full_house([H1,H2,H3,T1,T2]) :- same_rank([H1,H2,H3]),same_rank([T1,T2]).

has_straight([H|T]) :- rank(H,R),has_straight(T,R).
has_straight([],_).
has_straight([H|T],M) :- rank(H,R), M is R-1,has_straight(T,R).


one_pair([H1,H2|T]) :- if(same_rank([H1,H2]),true,one_pair([H2|T])).

two_pairs([[],[],[],[],[]]).
two_pairs([H1,H2,H3,H4,H5]) :-same_rank([H1,H2]),one_pair([H3,H4,H5]).
two_pairs([H1,H2,H3,H4,H5]) :-not(same_rank([H1,H2])),two_pairs([H2,H3,H4,H5,[]]).

three_of_a_kind([H1,H2,H3|T]) :- if(same_rank([H1,H2,H3]),true,three_of_a_kind([H2,H3|T])).

four_of_a_kind([H1,H2,H3,H4|T]) :- if(same_rank([H1,H2,H3,H4]),true,four_of_a_kind([H2,H3,H4|T])).

straight_flush([H|T]) :- has_flush([H|T]),has_straight([H|T]).

better_poker_hand(Hand1,Hand2,Hand) :-
 if(member(card(X,ace),Hand1),substitute(card(X,ace),card(X,14),Hand1,L1),append([],Hand1,L1)),
 if(member(card(X,jack),Hand1),substitute(card(X,jack),card(X,11),L1,L2),append([],L1,L2)),
 if(member(card(X,queen),Hand1),substitute(card(X,queen),card(X,12),L2,L3),append([],L2,L3)),
 if(member(card(X,king),Hand1),substitute(card(X,king),card(X,13),L3,L4),append([],L3,L4)),
 if(member(card(X,ace),Hand2),substitute(card(X,ace),card(X,14),Hand2,L5),append([],Hand2,L5)),
 if(member(card(X,jack),Hand2),substitute(card(X,jack),card(X,11),L5,L6),append([],L5,L6)),
 if(member(card(X,queen),Hand2),substitute(card(X,queen),card(X,12),L6,L7),append([],L6,L7)),
 if(member(card(X,king),Hand2),substitute(card(X,king),card(X,13),L7,L8),append([],L7,L8)),
 sort_l(L4,Handa),sort_l(L8,Handb),type_of_list(Handa,Type1),type_of_list(Handb,Type2),compare_type(Type1,Type2,Hand1,Hand2,Hand).
