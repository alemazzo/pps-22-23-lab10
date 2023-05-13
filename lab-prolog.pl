% PEANO LIB
sum(zero, N, N).
sum(s(N), M, s(O)) :- sum(N, M, O).
greater(s(_), zero).
greater(s(N), s(M)) :- greater(N, M).

% search (Elem , List )
search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).

% search2 (Elem , List )
% looks for two consecutive occurrences of Elem
search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).

% search_two (Elem , List )
% looks for two occurrences of Elem with any element in between !
search_two(X, cons(X, cons(Y, cons(X, Xs)))) :- Y \= X.
search_two(X, cons(Y, Xs)) :- search_two(X, Xs).

% search_anytwo (Elem , List )
% looks for any Elem that occurs two times , anywhere
search_anytwo(X, cons(X, Xs)) :- search(X, Xs).
search_anytwo(X, cons(_, Xs)) :- search_anytwo(X, Xs).

% size (List , Size )
% Size will contain the number of elements in List, written using notation zero , s( zero ), s(s( zero ))..
size(nil, zero).
size(cons(_, T), s(R)) :- size(T, R).

% sum_list (List , Sum )
sum_list(nil, zero).
sum_list(cons(H, T), R) :- sum_list(T, ST), sum(H, ST, R).

% count (List , Element , NOccurrences )
% it uses count (List , Element , NOccurrencesSoFar , NOccurrences )
count(List, E, N) :- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count (L, E, s(N) , M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E, N, M).

% max(List , Max )
% Max is the biggest element in List
% Suppose the list has at least one element
max(X, Y, X) :- greater(X, Y).
max(X, Y, Y) :- greater(Y, X).
max(X, X, X).

max_list(L, R) :- max_list(L, zero, R).
max_list(nil, B, B).
max_list(cons(H, T), B, R) :- max(H, B, G), max_list(T, G, R).

% min - max (List ,Min , Max )
% Min is the smallest element in List
% Max is the biggest element in List
% Suppose the list has at least one element
min(X, Y, X) :- max(X, Y, Y).
min(X, Y, Y) :- max(X, Y, X).
min(X, X, X).

min_max_list(cons(H, T), MN, MX) :- min_max_list(T, H, H, MN, MX).
min_max_list(nil, AMN, AMX, AMN, AMX).
min_max_list(cons(H, T), AMN, AMX, RMN, RMX) :- min(AMN, H, H), min_max_list(T, H, AMX, RMN, RMX).
min_max_list(cons(H, T), AMN, AMX, RMN, RMX) :- max(AMX, H, H), min_max_list(T, AMN, H, RMN, RMX).
min_max_list(cons(H, T), AMX, AMX, RMN, RMX) :- min_max_list(T, AMX, AMN, RMN, RMX).

% same (List1 , List2 )
% are the two lists exactly the same ?
same(nil, nil).
same(cons(H, T), cons(H2, T2)) :- H = H2, same(T, T2).

% all_bigger (List1 , List2 )
% all elements in List1 are bigger than those in List2 , 1 by 1
all_bigger(nil, nil).
all_bigger(cons(H, T), cons(H2, T2)) :- greater(H, H2), all_bigger(T, T2).

% sublist (List1 , List2 )
% List1 should contain elements all also in List2
sublist(nil, L).
sublist(cons(H, T), L) :- search(H, L), sublist(T, L).

% seq(N,E, List ) --> List is [E,E ,... ,E] with size N
% example : seq (s(s(s( zero ))), a, cons (a, cons (a, cons (a, nil )))).
seq(zero, _, nil).
seq(s(N), E, cons(E,T)) :- seq(N, E, T).

% seqR (N, List )
seqR(zero, nil).
seqR(s(N), cons(N, T)) :- seqR(N, T).

% seqR2 (N, List ) --> is [0 ,1 ,... ,N -1]
range(E, E, nil).
range(S, E, cons(S, T)) :- range(s(S), E, T).
seqR2(N, R) :- range(zero, N, R).
