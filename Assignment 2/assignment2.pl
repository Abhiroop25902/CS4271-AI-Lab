
/**
 *
 * is_member/2 predicate returns true if X is a member of list L
 *  
 * @param X the element to search
 * @param L the list to search in  */
is_member(X, [X]).
is_member(X, [X | _]).
is_member(X, [_ | Rest]):-
    is_member(X, Rest).

%% append_two_list(L1, L2, L3). -> appends L1 and L2 and puts it in L3
append_two_list([], L2, L2). 
append_two_list([X|L1], L2, [X|L3]):-
    append_two_list(L1, L2, L3).

lengthacc([], A, A).

lengthacc([_|T], A, M):-
    A1 is A + 1,
    lengthacc(T, A1, M).

% length of list L is N
list_len(L, N):-
    lengthacc(L, 0, N).

% range(Low, High, Res) -> Res is [Low, Low+1, ..., High]
range(High, High, [High]).
range(Low, High, [Low|L1]):-
    Low =< High,
    Low_plus_1 is Low + 1,
    range(Low_plus_1, High, L1).


% 1. To add an element to a list provided it is not present in the list.
% add_if_not_present(X, L, R) -> add X to L and return it in R,
add_if_not_present(X, [], [X]).
add_if_not_present(X, L, L):-
    is_member(X, L), !.
add_if_not_present(X, L, [X|L]).

% 2. To delete first occurrence of an element from a list.
% delete_first_occur(X, L, R) -> R is the result
delete_first_occur(_, [], []).
delete_first_occur(X, [X| L], L):- !.
delete_first_occur(X, [Y|L], [Y|L1]):-
    delete_first_occur(X, L, L1).

% 3. To delete all occurrences of an element from a list.
delete_all_occur(_, [], []).
delete_all_occur(X, [X|L], L1):-
    !, delete_all_occur(X, L, L1).
delete_all_occur(X, [Y|L], [Y|L1]):-
    delete_all_occur(X, L, L1).

% 4. To replace the first occurrence of an element X in L 
%  with Y giving the result in R.
% replace_X_with_Y(X, Y, L, R) -> R is the resultant
replace_X_with_Y(_, _, [], []).
replace_X_with_Y(X, Y, [X|Rest], [Y|L1]):- 
    !, replace_X_with_Y(X, Y, Rest, L1).
replace_X_with_Y(X, Y, [Z|Rest], [Z|L1]):-
    replace_X_with_Y(X, Y, Rest, L1).

% 5. has_duplicate(L), that determines whether list L has
% duplicate elements.
has_duplicate_loop([X|_], PrevElems):-
    is_member(X, PrevElems), !, not(fail).

has_duplicate_loop([X|L], PrevElems):-
    has_duplicate_loop(L, [X|PrevElems]).

has_duplicate(L):-
    has_duplicate_loop(L, []).

% 6. To duplicate the elements of a list.

%% duplicate(L, Res)  returns the result in Res
% @param L the original list
% @param Res resultant list

duplicate([], []).
duplicate([X|L], [X|[X|L1]]):-
    duplicate(L, L1).

% 7. To duplicate the elements of a list a given number of times.

%% generate_n(X, N, Res) Res is the a list with X duplicated N time
% @param X the elemtent to be duplicated
% @param N amount of time to duplicate
% @param Res the resultant list
generate_n(_, 0, []).
generate_n(X, N, [X|L]):-
    N_minus_1 is N - 1, 
    generate_n(X, N_minus_1, L).

%% duplicate_n(L, N,Res)  returns the result in Res
% @param L the original list
% @param N the amount of time to be deleted
% @param Res resultant list
duplicate_n([], _, []).
duplicate_n([X|L], N, Res):-
    generate_n(X, N, X_dupli_list),
    duplicate_n(L, N, NextRes),
    append_two_list(X_dupli_list, NextRes, Res).

% 8. To determine whether a list is a sub list of another list. 
% A list is a sub list of another list if it’s elements are 
% present in another list consecutively and in the same order.

prefix([], _).
prefix([X|L], [X|M]):-
    prefix(L, M).

%% is_sublist(L1, L2) return true if L1 is a sublist of L2
% NOTE: giving error
% is_sublist(L1, L2):-
%     append_two_list(_, L1, L3),
%     append_two_list(L3, _, L2).

% empty list is a sublist of any list
is_sublist([], _).

is_sublist([X|L], [X|M]):-
    prefix(L, M), !.

is_sublist([X|L], [_|M]):-
    is_sublist([X|L], M).

% 9. To determine whether a set is a subset of another set.
%% is_subset(S1, S2) return true if S1 is a subset of S2
is_subset([], _).
is_subset([X|L1], L2):-
    is_member(X, L2), is_subset(L1, L2).

% 10. To determine intersection of two sets.
%% set_intersection(S1, S2, Res) 
% Res is the intersection of Set S1 and S2
set_intersection([], _, []).
set_intersection([X|S1], S2, [X|S3]):-
    is_member(X, S2), !,
    set_intersection(S1, S2, S3).
set_intersection([_|S1], S2, S3):-
    set_intersection(S1, S2, S3).

% 12.To determine union of two sets.
%% set_union(S1, S2, Res) 
% Res is the union of Set S1 and S2
set_union([], S, S).
set_union([X|S1], S2, S3):-
    is_member(X, S2), !,
    set_union(S1, S2, S3).
set_union([X|S1], S2, [X|S3]):-
    set_union(S1, S2, S3).

% 13.To determine difference of two sets.
%% set_difference(S1, S2, Res) 
% Res is the S1 - S2
set_difference([], _,[]).
set_difference(S1, [], S1).
set_difference([X|S1], S2, S3):-
    is_member(X, S2), !,
    set_difference(S1, S2, S3).
set_difference([X|S1], S2, [X|S3]):-
    set_difference(S1, S2, S3).

% 14.To determine symmetric difference of two sets.
%% set_symmetric_difference(S1, S2, Res) 
% Res is the symmetric_difference of S1 and S2
% Res = (S1 Union S2) - (S1 Intersection S2)
set_symmetric_difference(S1, S2, Res):-
    set_union(S1, S2, S1_union_S2),
    set_intersection(S1, S2, S1_intersection_S2),
    set_difference(S1_union_S2, S1_intersection_S2, Res).

% 15. To find the last but one element of a list.
% last_but_one(L, X) -> X is the last but one element of list L
last_but_one(L, X):-
    append_two_list(_, [X|[_]], L).

% 16. To find the K'th element of a list.
% The first element in the list is number 1.
%% element_at(X,L,N).
% X is the Nth element at List L
element_at(X, [X|_], 1).
element_at(X, [_|L], N):-
    N_minus_1 is N - 1,
    element_at(X, L, N_minus_1).

% 17. To replace n th element by another element X in L, 
% leaving the resultant list in L1.
% replace_n(L, N, X, L1) -> L1 will have same elements as L,
% but every Nth term will be X

replace_n_loop([], _, _, [], _).

replace_n_loop([_|Rest], N, X, [X|L2],Curr):-
    Curr = N , !,
    replace_n_loop(Rest, N, X, L2, 1).

replace_n_loop([A|Rest], N, X, [A|L2],Curr):-
    NextCurr is Curr + 1,
    replace_n_loop(Rest, N, X, L2, NextCurr).

replace_n(L, N, X, L1):- 
    replace_n_loop(L, N, X, L1,1).

% 18. to remove every N'th element from a list.
% remove_n(L, N, L1) -> L1 will have elements of L but every 
% nth element removed

remove_n_loop([], _, [], _).

remove_n_loop([_|Rest], N, L2,Curr):-
    Curr = N , !,
    remove_n_loop(Rest, N, L2, 1).

remove_n_loop([A|Rest], N, [A|L2],Curr):-
    NextCurr is Curr + 1,
    remove_n_loop(Rest, N, L2, NextCurr).

remove_n(L, N, L1):-
    remove_n_loop(L,N, L1, 1).

% 19. Interleave alternate elements of L1 and L2 into L. 
% For example, if L1= [a, b, c] and L2= [1, 2], then L= [a, 1, b, 2, c].

% 0 means take from L1, else L2
interleave_two_list_loop([], L2, L2,_).
interleave_two_list_loop(L1, [], L1,_).

interleave_two_list_loop([X|L1], [Y|L2], [X|L3],0):-
    interleave_two_list_loop(L1, [Y|L2], L3,1).

interleave_two_list_loop([X|L1], [Y|L2], [Y|L3],1):-
    interleave_two_list_loop([X|L1], L2, L3,0).

% interleave_two_list(L1, L2, Res) -> Res is the result
interleave_two_list(L1, L2, Res):-
    interleave_two_list_loop(L1, L2, Res, 0).

%20.Transpose L1, L2 into L. That is, if L1= [a, b, c] and 
% L2= [1, 2, 3], then L= [(a, 1), (b, 2), (c,3)].

% transpose_two_list(L1, L2, Res) -> valid only if length of L1 = L2
transpose_two_list([], [], []).

transpose_two_list([A|L1], [B|L2], [(A, B)|Rest]):-
    list_len(L1, X),
    list_len(L2, X),
    transpose_two_list(L1, L2, Rest).

% 21. To split a list into two parts; the length of the first part is given.
% Do not use any predefined predicates.

% split_two_parts(L, N, Left, Right) -> 
% Left will be first N elements of L
% rest elements will be in Right

% N should be less than lenght of List
split_two_parts(L, 0, [], L).

split_two_parts([X|L], N, [X|Left], Right):-
    list_len([X|L], ListLen),
    N =< ListLen, 
    N_minus_1 is N - 1,
    split_two_parts(L, N_minus_1, Left, Right).

% 22. To extract a slice from a list.
% Given two indices, I and K, the slice is the list containing 
% the elements between the I'th and K'th element of the 
% original list (both limits included). 
% Start counting the elements with 1.
% Example:
% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
% {X = [c,d,e,f,g]}

slice(L1, LeftStart, RightEnd, Res):-
    LeftExtraLen is LeftStart - 1,
    split_two_parts(L1, LeftExtraLen, _, Res_plus_extra),
    LeftStart =< RightEnd,
    ResLen is RightEnd - LeftStart + 1,
    split_two_parts(Res_plus_extra, ResLen, Res, _).

% 23. To insert an element at a given position into a list.
% Example:
% ?- insert_at(alfa,[a,b,c,d],2,L).
% {L = [a,alfa,b,c,d]}

% insert_at(X, L, N, Res) -> N is 1 indexed
insert_at(X, [Head|Tail], 1, [X|[Head|Tail]]).

insert_at(X, [Head|Tail], N, [Head|Res]):-
    N_minus_1 is N - 1,
    insert_at(X, Tail, N_minus_1, Res).

% 24. To remove_every_other (L, L1). List L1 is just list L 
% with every other element removed
% (the two lists should have the same first element).

% 0 means keep, 1 means remove
remove_every_other_loop([X|L], [X|L1], 0):-
    remove_every_other_loop(L, L1, 1).

remove_every_other_loop([_|L], L1, 1):-
    remove_every_other_loop(L, L1, 0).

remove_every_other_loop([], [], _).

remove_every_other(L, L1):-
    remove_every_other_loop(L, L1, 0).


% 25. cutlast (L, L1) that defines L1 to be obtained from L 
% with last element removed.
cutlast(L, L1):-
    append_two_list(L1, [_], L).

% 26. trim (N, L, L1) that defines L1 to be obtained from L 
% with first N elements removed
trim(N, L, L1):-
    append_two_list(LeftExtra, L1, L),
    list_len(LeftExtra, N).

% 27.trimlast (N, L, L1) that defines L1 to be obtained from 
% L with last N elements removed.
trimlast(N, L, L1):-
    append_two_list(L1, RightExtra, L),
    list_len(RightExtra, N).

% 28.exchange_first_last(L, L1), defines that L1 to be obtained 
% from L with first and last elements exchanged.
% Example:
% ?-exchange_first_last([a, b, c, d, e], X).
% {X= [e, b, c, d, a]}
exchange_first_last([X|T], [Y|L1]):-
    append_two_list(Rest, [Y], T),
    append_two_list(Rest, [X], L1).

% 29 circular_left_shift(L, L1). That is, if L= [a, b, c, d, e, f] 
% then L1= [b, c, d, e, f, a]
circular_left_shift([X|L], L1):-
    append_two_list(L, [X], L1).

% 30. circular_right_shift(L, L1). That is, if L= [a, b, c, d, e, f] 
% then L1= [f, a, b, c, d, e]
circular_right_shift(L, [Y|L1]):-
    append_two_list(L1, [Y], L).

% [Try using circular_left_shift in 30 to implement circular_right_shift.]
circular_right_shift_2_loop(L, L, 0).

circular_right_shift_2_loop(L, L1, Curr):-
    circular_left_shift(L, L2),
    Curr_minus_1 is Curr - 1,
    circular_right_shift_2_loop(L2, L1, Curr_minus_1).

circular_right_shift_1(L, L1):-
    list_len(L, L_len),
    L_len_minus_1 is L_len - 1,
    circular_right_shift_2_loop(L, L1, L_len_minus_1).

% 31.To delete the middle element from an odd-numbered list 
% L into a list L1.

delete_middle_element_odd_length_loop([_|T], T, 0).

delete_middle_element_odd_length_loop([H|T], [H|L1], MiddleIndex):-
    MiddleIndex_minus_1 is MiddleIndex - 1,
    delete_middle_element_odd_length_loop(T, L1, MiddleIndex_minus_1).


delete_middle_element_odd_length(L, L1):-
    list_len(L, L_len),
    L_len_mod_2 is L_len mod 2,
    L_len_mod_2 = 1,
    MiddleIndex is (L_len-1)/2,
    delete_middle_element_odd_length_loop(L, L1, MiddleIndex).

% 32.To delete two middle elements from an even-numbered list 
% L into a list L1.

delete_middle_element_even_length(L, L1):-
    list_len(L, L_len),
    L_len_mod_2 is L_len mod 2,
    L_len_mod_2 = 0,
    MiddleIndex is L_len/2 - 1,
    delete_middle_element_even_length_loop(L, L1, MiddleIndex).

delete_middle_element_even_length_loop([_|[_|T]], T, 0).

delete_middle_element_even_length_loop([H|T], [H|L1], MiddleIndex):-
    MiddleIndex_minus_1 is MiddleIndex - 1,
    delete_middle_element_even_length_loop(T, L1, MiddleIndex_minus_1).