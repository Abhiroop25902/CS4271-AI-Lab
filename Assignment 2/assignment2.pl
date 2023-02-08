% 1. To add an element to a list provided it is not present in the list.

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

is_sublist([X|L], [Y|M]):-
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