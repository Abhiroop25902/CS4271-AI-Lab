% 1. To determine whether the first two elements of a list 
% are same
first_two_same([X|[X|_]]).

% 2. To determine whether a list is not a two-element list.
not_two_element_list([]).
not_two_element_list([_]).
% not_two_element_list([_, _, _]).
not_two_element_list([_, _, [_ | _]]).

% 3. To determine whether two lists are of same length.
equal_length([],[]).
equal_length([_ | L1], [_ | L2]):-
    equal_length(L1, L2).

% 4. To determine length of a list using your own number system.
% length 0 -> 0
% length 1 -> s(0)
% length 2 -> s(s(0))
my_list_length([], 0).
my_list_length([_| Rest], s(L)):-
    my_list_length(Rest, L).

% 5. To determine whether two lists are of same length using 
% the length predicate developed in 4 (previous problem).
equal_length1(L1, L2):-
    my_list_length(L1, X),
    my_list_length(L2, X).

% 6. To find the last element of a list. 
last_element([X], X).
last_element([_ | [B |L1]], X):-
    last_element([B | L1], X).

% 7. To find whether an element is a member of a list.
is_member(X, [X]).
is_member(X, [_ | Rest]):-
    is_member(X, Rest).

% 8. To find whether two elements are next to each other in 
% a list.
two_elem_next(A, B, [A | [B | _]]):- !.
two_elem_next(A, B, [_ | Rest]):-
    two_elem_next(A, B, Rest).

% 9. To append two lists in a third list.
% append_two_list(L1, L2, L3). -> appends L1 and L2 and puts it in L3
append_two_list([], L2, L2). 
append_two_list([X|L1], L2, [X|L3]):-
    append_two_list(L1, L2, L3).

% 10. To find the last element of a list using append 
% predicate developed in 9
last_element1(L1, X):-
    append_two_list(_, [X], L1).

% 11. To find whether an element is a member of a list using
% append predicate developed in 9
% is_member(X, L1). -> true if X is in L1
is_member1(X, L1):-
    append_two_list(_, [X|_], L1).
