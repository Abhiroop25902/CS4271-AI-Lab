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
