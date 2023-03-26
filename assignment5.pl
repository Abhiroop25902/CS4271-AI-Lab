% append_two_list(L1, L2, L3). -> appends L1 and L2 and puts it in L3
append_two_list([], L2, L2). 
append_two_list([X|L1], L2, [X|L3]):-
    append_two_list(L1, L2, L3).


reverse_list_acc([], Res, Res).

reverse_list_acc([H|T], Curr, Res):-
    reverse_list_acc(T, [H|Curr], Res).

% reverse_list(L, Res) -> Res is the reversed List
reverse_list(L, Res):-
    reverse_list_acc(L, [], Res).

list_len_acc([], A, A).

list_len_acc([_|T], A, M):-
    A1 is A + 1,
    list_len_acc(T, A1, M).

% list_len(L, N) -> N is the lenght of the list L
list_len(L, N):-
    list_len_acc(L, 0, N).

% 1. Remove the K'th element from a list.

% remove_at(RemovedElem, List, Index, ResultantList)
% input -> List, Index to remove elem (1 indexed)
% output -> ResultantList, RemovedElem

remove_at_loop(H, [H|T], 1, ResList, VisitedElem):-
    reverse_list(VisitedElem, L1),
    append_two_list(L1, T, ResList).
    
remove_at_loop(RemElem, [H|T], Idx, ResList, VisitedElem):-
    Idx_minus_1 is Idx - 1,
    remove_at_loop(RemElem, T, Idx_minus_1, ResList, [H|VisitedElem]).

remove_at(RemElem, L, Idx, ResList):-
    remove_at_loop(RemElem, L, Idx, ResList, []).

% ?- remove_at(X, [a,b,c,d], 2, R).
% X = b,
% R = [a, c, d] .

% ?- remove_at(X, [a,b,c,d], 5, R).
% false.

% 2. Extract a given number of randomly selected elements from a list.
% The selected items shall be put into a result list.
% Hint: Use the built-in random number generator random/2 and the result of
% problem P1.

rnd_select_loop(_, 0, Res, Res).

rnd_select_loop(L, ResListLen, Res, SelectedElem):-
    list_len(L, L_Len),
    random_between(1, L_Len, RandomIdx),
    remove_at(RemovedElem, L, RandomIdx, RemainingList),
    ResListLen_Minus_1 is ResListLen - 1,
    rnd_select_loop(RemainingList, ResListLen_Minus_1, Res, [RemovedElem|SelectedElem]).
    

%rnd_select(GivenList, ResListLen, ResList) -> ResList will be
% from GivenList of length ResListLen
rnd_select(L, ResListLen, Res):-
    rnd_select_loop(L, ResListLen, Res, []).

% ?- rnd_select([a,b,c,d,e,f,g,h], 3, L).
% L = [c, b, f] .

% ?- rnd_select([a,b,c,d,e,f,g,h], 3, L).
% L = [g, c, e] .

% 3. Generate a random permutation of the elements of a list.
% Hint: Use the solution of problem P2.

rnd_permu(L, Res):-
    list_len(L, L_Len),
    rnd_select(L, L_Len, Res).

% ?- rnd_permu([a,b,c,d,e,f],L).
% {L = [b,a,d,c,e,f]}

% ?- rnd_permu([a,b,c,d,e,f],L).
% L = [e, b, c, f, d, a] .

% 4. Generate the combinations of K distinct objects chosen from the N
% elements of a list
% In how many ways can a committee of 3 be chosen from a group of 12
% people? We all know that there are C(12,3) = 220 possibilities (C(N,K)
% denotes the well-known binomial coefficients). For pure mathematicians,
% this result may be great. But we want to really generate all the possibilities
% (via backtracking).
% Example:
% ?- combination(3,[a,b,c,d,e,f],L).
% {L = [a,b,c]} ;
% {L = [a,b,d]} ;
% {L = [a,b,e]} ; ...

combination_loop(0, _, Res, SelectedElem):-
    !, reverse_list(SelectedElem, Res).

% we either pick the element
combination_loop(ResLen, [H|T], Res, SelectedElem):-
    ResLen_Minus_1 is ResLen - 1,
    combination_loop(ResLen_Minus_1, T, Res, [H|SelectedElem]).

% or we dont
combination_loop(ResLen, [_|T], Res, SelectedElem):-
    combination_loop(ResLen, T, Res, SelectedElem).

combination(ResLen, L, Res):-
    combination_loop(ResLen, L, Res, []).

    ?- combination(1,[a,b,c,d,e,f],L).
L = [a] ;
L = [b] ;
L = [c] ;
L = [d] ;
L = [e] ;
L = [f] ;
false.

% ?- combination(3,[a,b,c,d,e,f],L).
% L = [a, b, c] ;
% L = [a, b, d] ;
% L = [a, b, e] ;
% L = [a, b, f] ;
% L = [a, c, d] ;
% L = [a, c, e] ;
% L = [a, c, f] ;
% L = [a, d, e] ;
% L = [a, d, f] ;
% L = [a, e, f] ;
% L = [b, c, d] ;
% L = [b, c, e] ;
% L = [b, c, f] ;
% L = [b, d, e] ;
% L = [b, d, f] ;
% L = [b, e, f] ;
% L = [c, d, e] ;
% L = [c, d, f] ;
% L = [c, e, f] ;
% L = [d, e, f] ;
% false.

% ?- combination(5,[a,b,c,d,e,f],L).
% L = [a, b, c, d, e] ;
% L = [a, b, c, d, f] ;
% L = [a, b, c, e, f] ;
% L = [a, b, d, e, f] ;
% L = [a, c, d, e, f] ;
% L = [b, c, d, e, f] ;
% false.