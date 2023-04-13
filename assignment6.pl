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

%% set_union(S1, S2, Res) 
% Res is the union of Set S1 and S2
set_union([], S, S).
set_union([X|S1], S2, S3):-
    is_member(X, S2), !,
    set_union(S1, S2, S3).
set_union([X|S1], S2, [X|S3]):-
    set_union(S1, S2, S3).

% 1. Truth tables for logical expressions (1).
% Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
% logical equivalence) which succeed or fail according to the result of their
% respective operations; e.g. and(A,B) will succeed, if and only if both A and
% B succeed. Note that A and B can be Prolog goals (not only the constants
% true and fail).
% A logical expression in two variables can then be written in prefix notation,
% as in the following example: and(or(A,B),nand(A,B)).
and(P1, P2):- P1, P2.

or(P1, P2):- P1; P2.

nand(P1, P2):- not(and(P1, P2)).

nor(P1, P2):- not(or(P1, P2)).

xor(P1, P2):- and(P1, not(P2)); and(not(P1), P2).

impl(P1, P2):- not(P1); P2.

equ(P1, P2):- impl(P1, P2), impl(P2, P1).

% ?- and(or(true,true),nand(false,true)).
% true .

% ?- and(or(false,false),nand(true,true)).
% false.

% Now, write a predicate table/3 which prints the truth table of a given logical
% expression in two variables.

evaluate(E, true) :- E, !.
evaluate(_, false).

bool(true).
bool(false).

table(A,B,E) :-
  bool(A),
  bool(B),
  write(A),
  write(' \t '),
  write(B),
  write(' \t '),
  evaluate(E, Result),
  write(Result),nl, fail.

% ?- table(A,B,and(A,or(A,B))).
% true     true    true
% true     false   true
% false    true    false
% false    false   false
% false.

% ?- table(A,B,and(A,xor(A,B))).
% true     true    false
% true     false   true
% false    true    false
% false    false   false
% false.

% 2. Truth tables for logical expressions (2).
% Continue problem P1 by defining and/2, or/2, etc as being operators. This
% allows to write the logical expression in the more natural way, as in the
% example: A and (A or not B). Define operator precedence as usual;

% NOTE: operator precedence values are copied from swipl docs
% https://www.swi-prolog.org/pldoc/man?predicate=op/3
:- op(1100, xfx, or). % :- says "write it in console"
:- op(1100, xfx, nor). % :- says "write it in console"
:- op(1000, xfy, and). % :- says "write it in console"
:- op(1000, xfx, nand). % :- says "write it in console"
:- op(1100, yfx, xor). % :- says "write it in console"
:- op(1000, fx, not).  
:- op(1200, xfx, equ).
:- op(1200, xfx, impl).

% ?- table(A,B, A and (A or not B)).
% true     true    true
% true     false   true
% false    true    false
% false    false   false
% false.

% ?- table(A,B, A and (A xor not B)).
% true     true    true
% true     false   false
% false    true    false
% false    false   false
% false.

% 3. Truth tables for logical expressions (3).
% Generalize problem P2 in such a way that the logical expression may
% contain any number of logical variables. Define table/2 in a way that
% table(List,Expr) prints the truth table for the expression Expr, which
% contains the logical variables enumerated in List.
bool_loop([]).

bool_loop([H|T]):-
    bool(H),
    bool_loop(T).

write_L([]).

write_L([H|T]):-
    write(H),
    write(" \t "),
    write_L(T).

table(L,E) :-
    bool_loop(L),
    write_L(L),
    evaluate(E, Result),
    write(Result),nl, fail.

% ?- table([A,B,C], A and (B or C) equ A and B or A and C).
% true     true    true    true
% true     true    false   true
% true     false   true    true
% true     false   false   true
% false    true    true    true
% false    true    false   true
% false    false   true    true
% false    false   false   true
% false.

% ?- table([A,B,C], A and (B or C) impl A and B or A and C).
% true     true    true    true
% true     true    false   true
% true     false   true    true
% true     false   false   true
% false    true    true    true
% false    true    false   true
% false    false   true    true
% false    false   false   true
% false.

% 4. Write a PROLOG program for automated reasoning in Propositional Logic (PL)
% using Resolution-Refutation.
% Inputs: Sets of Axioms and Negation of Conclusion in Clause Form.
% Outputs: [1] (Conclusion is) True or False, i. e. , conclusion is derivable from the
% given sets of axioms.
% [2] The path through which the conclusion is found to be True , if it is
% valid.

% we create our clause structure as: clause(List, resolve(clauseX, clause))
% where List is the list of propositions (atoms) in the clause, and this 
% clause is obtained by resolving % two other clauses clauseX and clauseY.
% If clause is given as axiom, the second argument of our
% clause(arg1, arg2) structure is ‘none’.
create_clauses([],[]).
create_clauses([X|R],[clause(X,none)|RClauses]):-
    create_clauses(R,RClauses).

% negate all elements in the clause list
negation([],[]).
negation([not X|R],[X|NR]):-
    !,
    negation(R,NR).
negation([X|R],[not X|NR]):-
    negation(R,NR).

% resolve_pair(clause1, clause2, resolved_pair)
resolve_pair(
    clause(X,XPath),clause(Y,YPath),
    clause(Z,resolve(clause(X,XPath),clause(Y,YPath)))
):-
    negation(X,NX),
    negation(Y,NY),
    subtract(X,NY,CX),
    subtract(Y,NX,CY),
    union(CX,CY,Z).

% is X already present in Y?
is_present(clause(X,_),[clause(X,_)|_]):-!.
is_present(clause(X,P),[clause(_,_)|R]):-
    is_present(clause(X,P),R).

% resolve(C, Rem, Res) -> resolve C with all clauses of Rem and give them in Res
resolve(_,[],[]).
resolve(C,[X|R],RList):-
    resolve_pair(C,X,NC),
    resolve(C,R,NR),
    set_union([NC],NR,RList).

% generate new clause resolving a pair with each other
generate_clauses([],[]).
generate_clauses([Clause|R],NewClauses):-
    generate_clauses(R,NClauses),
    resolve(Clause,R,RClauses),
    set_union(NClauses,RClauses,NewClauses).

% end case
is_solved([clause([],Path)|_]):-
    write(solution:Path),!.
is_solved([clause(_,_)|R]):-
    is_solved(R).

% is_subset code but for clause
is_subset([],_).
is_subset([X|P],Q):-
    is_subset(P,Q),
    is_present(X,Q).

solve(Clauses):-
    generate_clauses(Clauses,NewClauses),
    is_solved(NewClauses),!.
solve(Clauses):-
    generate_clauses(Clauses,NewClauses),
    is_subset(NewClauses,Clauses),!,
    fail.
solve(Clauses):-
    generate_clauses(Clauses,NewClauses),
    set_union(NewClauses,Clauses,Combined),
    solve(Combined).
   

resolution_refutation(List):-
    create_clauses(List,Clauses),
    solve(Clauses).

% ?- resolution_refutation([[x, not y, not z], [y], [z], [not x]]).
% solution:resolve(clause([y,(not x)],resolve(clause([y],none),clause([(not x)],none))),clause([x,(not y)],resolve(clause([x,(not y),(not z)],none),clause([z],none))))
% true .

% ?- resolution_refutation([[x], [y]]).
% false.
   