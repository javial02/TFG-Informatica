/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    Common predicates to several files                 */
/*                                                       */
/*                                                       */
/*                                                       */
/*                    Fernando Saenz-Perez (c) 2004-2021 */
/*                                      DISIA FADoSS UCM */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                Visit the Web site at: */
/*                           http://des.sourceforge.net/ */
/*                                                       */
/* This file is part of DES.                             */
/*                                                       */
/* DES is free software: you can redistribute it and/or  */
/* modify it under the terms of the GNU Lesser General   */
/* Public License as published by the Free Software      */
/* Foundation, either version 3 of the License, or (at   */
/* your option) any later version.                       */
/*                                                       */
/* DES is distributed in the hope that it will be useful,*/
/* but WITHOUT ANY WARRANTY; without even the implied    */
/* warranty of MERCHANTABILITY or FITNESS FOR A          */
/* PARTICULAR PURPOSE. See the GNU Lesser General Public */
/* License for more details.                             */
/*                                                       */
/* You should have received a copy of the GNU Lesser     */
/* General Public License and GNU General Public License */
/* along with this program. If not, see:                 */
/*                                                       */
/*            http://www.gnu.org/licenses/               */
/*********************************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List/Set processing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% concat_lists(+ListOfLists,-List) Appends a list of lists 
%   and returns the flattened list

concat_lists([],[]).
concat_lists([[]|R],S) :-
  concat_lists(R,S).
concat_lists([[C|R1]|R2],[C|S]) :-
  concat_lists([R1|R2],S).

list_to_list_of_lists([],[]).
list_to_list_of_lists([X|Xs],[[X]|Xss]) :-
  list_to_list_of_lists(Xs,Xss).

% % Appending two lists
% append([],X,X).
% append([X|Xs],Y,[X|Zs]) :-
%   append(Xs,Y,Zs).
% my_append(Xs,Ys,Zs) :-
%   append(Xs,Ys,Zs).

% Xs is either a variable or an open-ended list of the form [X1,...,Xn|_]
% Ys [Y1,...,Ym] is the list to be appended to Xs, so that Xs becomes [X1,...,Xn,Y1,...,Ym|_]
open_append(Xs,Ys) :-
  var(Xs),
  append(Ys,_,Xs),
  !.
open_append(Xs,Ys) :-
  append(Ys,_,OYs),
  append(_,OYs,Xs),
  !.


% Appending two lists for finding substrings
% my_appendfind([],X,X) :-
%   !.
% my_appendfind([X|Xs],Y,[X|Zs]) :-
%   my_appendfind(Xs,Y,Zs).

% Appending a VariableName=Variable to an input list
% append_NV('_',V,Vi,['='('_',V)|Vi]) :- 
%   !.
append_NV('_',V,Vi,Vo) :- 
  !,
  append(Vi,['='('_',V)],Vo).
append_NV(N,V,Vi,Vi) :- 
  member('='(N,V),Vi),
  !.
%append_NV(N,V,Vi,['='(N,V)|Vi]).
append_NV(N,V,Vi,Vo) :-
  append(Vi,['='(N,V)],Vo).

% append_new_vars(Vi,Vs,Vo). Append to Vi vars in Vs not in Vi, giving Vo as the result
append_new_vars(Vi,Vs,Vo) :-
  my_set_diff(Vs,Vi,Vn),
  append(Vi,Vn,Vo).

% Member of a list
my_member(X,L) :-
  member(X,L).
% my_member(X,[X|_Xs]).
% my_member(X,[_Y|Xs]) :-
%   my_member(X,Xs).
my_member_list([],_L).
my_member_list([X|Xs],L) :-
  member(X,L),
  my_member_list(Xs,L).
  
% my_member_chk(X,[X|_Xs]) :-
%   !.
% my_member_chk(X,[_Y|Xs]) :-
%   my_member_chk(X,Xs).
my_member_chk(X,Xs) :-
  memberchk(X,Xs).
  
% RDB predicate member check
% case-insensitive
rdb_pred_memberchk(X/A,Ps) :-
  to_uppercase(X,UX),
  to_uppercase_pred_list(Ps,UPs),
  memberchk(UX/A,UPs).
  
% Nth member of a list (first index is 0)
my_nth_member(X,N,Xs) :-
  my_nth_member(X,0,N,Xs).
% Nth member of a list (first index is 1)
my_nth1_member(X,N,Xs) :-
  my_nth_member(X,1,N,Xs).

my_nth1_member_list([],[],_Ys).
my_nth1_member_list([X|Xs],[N|Ns],Ys) :-
  my_nth1_member(X,N,Ys),
  my_nth1_member_list(Xs,Ns,Ys).

% my_nth1_member_var_list([],[],_Ys).
% my_nth1_member_var_list([X|Xs],[N|Ns],Ys) :-
%   my_nth1_member_var(X,N,Ys),
%   my_nth1_member_var_list(Xs,Ns,Ys).

my_nth_member(X,N,N,[X|_Xs]).
my_nth_member(X,CN,N,[_Y|Xs]) :-
  N1 is CN+1,
  my_nth_member(X,N1,N,Xs).

% % Nth member_var of a list
% % N starting from 0
% my_nth_member_var(X,N,Xs) :-
%   my_nth_member_var(X,0,N,Xs).
% N starting from 1
my_nth1_member_var(X,N,Xs) :-
  my_nth_member_var(X,1,N,Xs).

my_nth_member_var(X,N,N,[Y|_Xs]) :-
  X==Y.
my_nth_member_var(X,CN,N,[_Y|Xs]) :-
  N1 is CN+1,
  my_nth_member_var(X,N1,N,Xs).


% member_var of a list
my_member_var(X,[Y|_Ys]) :-
  X==Y.
% my_member_var(X,[Y|Ys]) :-
%   X\==Y,
%   my_member_var(X,Ys).
my_member_var(X,[_Y|Ys]) :-
  my_member_var(X,Ys).

my_member_var(X,P1,[P2|_Ys]) :-
  \+ \+ (( 
   make_ground(X),
   make_ground(P2),
   P1=P2)),
   !,
   P1=P2.
my_member_var(X,P,[_Y|Ys]) :-
  my_member_var(X,P,Ys).  
  
% Reversing a list  
my_reverse(L1, L2) :-
   my_reverse(L1, [], L2).
   
% reverse(+X, +Y, -Z)
% Z is X reversed, followed by Y
my_reverse([], Z, Z).
my_reverse([H|T], L0, L) :-
  my_reverse(T, [H|L0], L).
  
% list_between(+N1,+N2,-List)
% Create a list of integers between N1 and N2
list_between(N,N,[N]) :-
  !.
list_between(N1,N2,[N1|Ns]) :-
  NN1 is N1+1,
  list_between(NN1,N2,Ns).

% Replacing an element of a list
replace_list(_A,_B,[],[]).
replace_list(A,B,[A|Xs],[B|Ys]) :-
  !,
  replace_list(A,B,Xs,Ys).
replace_list(A,B,[X|Xs],[X|Ys]) :-
  replace_list(A,B,Xs,Ys).

% Take the tail list starting in the Nth element of the input list (elements are numbered from 1 on)  
% take_from_N(L,N,O) :-
%   take_from_N(L,1,N,O).

% take_from_N(L,N,N,L) :-
%   !.
% take_from_N([_|L],N,N2,O) :-
%   N1 is N+1,
%   take_from_N(L,N1,N2,O).

% Takes the first N elements from a list. 
% If there are no enough elements, return fresh variables
take_N(0,_L,[]) :-
  !.
take_N(N,[X|Xs],[X|Ys]) :-
  N1 is N-1,
  take_N(N1,Xs,Ys).
take_N(N,[],Xs) :-
  length(Xs,N).
  
% Takes up to N elements from a list. 
% If there are no enough elements, return fresh variables
take_up_to_N(_,[],[]) :-
  !.
take_up_to_N(0,_L,[]) :-
  !.
take_up_to_N(N,[X|Xs],[X|Ys]) :-
  N1 is N-1,
  take_up_to_N(N1,Xs,Ys).

% Split a list into two, the first one with N elements
split_list(0,L,[],L) :-
  !.
split_list(N,[X|Xs],[X|Ys],L) :-
  N1 is N-1,
  split_list(N1,Xs,Ys,L).
  
% Split a list into two, the first one with odd position elements, and the second one with even position elements
% [a,b,c,d,e,f] -> [a,c,e] , [b,d,f]
split_list_odd_even([],[],[]) :-
  !.
split_list_odd_even([O,E|Xs],[O|Os],[E|Es]) :-
  split_list_odd_even(Xs,Os,Es).
  
% Bidirectional list to tuple
my_list_to_tuple([L],T) :-
  nonvar(T),
  T\=(_H,_T),
  L=T.
my_list_to_tuple([L],T) :-
  var(T),
  L=T.
my_list_to_tuple([X,Y|Xs],(X,Ts)) :-
  my_list_to_tuple([Y|Xs],Ts).
  
my_list_to_tuple_list([],[]).
my_list_to_tuple_list([L|Ls],[T|Ts]) :-
  my_list_to_tuple(L,T),
  my_list_to_tuple_list(Ls,Ts).


% Bidirectional list to disjunction
my_list_to_disjunction([L],T) :-
  nonvar(T),
  T\=(_H;_T),
  L=T.
my_list_to_disjunction([L],T) :-
  var(T),
  L=T.
my_list_to_disjunction([X,Y|Xs],(X;Ts)) :-
  my_list_to_disjunction([Y|Xs],Ts).

%% Appending two tuples
%my_tuple_append((X,Xs),Y,(X,Zs)) :- 
%  !,
%  my_tuple_append(Xs,Y,Zs).
%my_tuple_append(X,Y,(X,Y)).

% Building a conjunctive term
%conjunctive_term([T],T) :-
%  !.
%conjunctive_term([T1,T2],CT) :- 
%  !, 
%  CT =.. [',',T1,T2].
%conjunctive_term([T|Ts],CT) :- 
%  CT =.. [',',T,CTT],
%  conjunctive_term(Ts,CTT).

% List to set
% Via unification
remove_duplicates(L,S) :-
  remove_duplicates(L,[],S).

% This reverses the input list:
% remove_duplicates([],L,L).
% remove_duplicates([X|Xs],AL,L) :-
%   my_member_chk(X,AL), 
%   !,
%   remove_duplicates(Xs,AL,L).
% remove_duplicates([X|Xs],AL,L) :-
%   remove_duplicates(Xs,[X|AL],L).

remove_duplicates([],_L,[]).
remove_duplicates([X|Xs],AL,L) :-
  my_member_chk(X,AL), 
  !,
  remove_duplicates(Xs,AL,L).
remove_duplicates([X|Xs],AL,[X|L]) :-
  remove_duplicates(Xs,[X|AL],L).

% List to set
% Variables are distinguished
remove_duplicates_var(L,S) :-
  extract_duplicates_var(L,[],S,_DS).

extract_duplicates_var(L,DS) :-
  extract_duplicates_var(L,[],_S,DS).
  
extract_duplicates_var([],_L,[],[]).
extract_duplicates_var([X|Xs],AL,L,[X|DL]) :-
  my_member_var(X,AL), 
  !,
  extract_duplicates_var(Xs,AL,L,DL).
extract_duplicates_var([X|Xs],AL,[X|L],DL) :-
  extract_duplicates_var(Xs,[X|AL],L,DL).

% % Remove contiguous duplicates
% remove_contiguous_duplicates([],[]).
% remove_contiguous_duplicates([T,T|L],S) :-
%   !,
%   remove_contiguous_duplicates([T|L],S).
% remove_contiguous_duplicates([T|L],[T|S]) :-
%   remove_contiguous_duplicates(L,S).
  
% Multiset difference
my_set_diff([], _, []).
my_set_diff([Element|Elements], Set, Difference) :-
  my_member_var(Element, Set),
  !,
  my_set_diff(Elements, Set, Difference).
my_set_diff([Element|Elements], Set, [Element|Difference]) :-
  my_set_diff(Elements, Set, Difference).

my_set_diff_list(_Elements, [], []).
my_set_diff_list(Elements, [Set|Sets], [Difference|Differences]) :-
  my_set_diff(Elements, Set, Difference),
  my_set_diff_list(Elements, Sets, Differences).

% Bag difference
my_bag_diff([], _, []).
my_bag_diff([Element|Elements], Set, Difference) :-
  remove_one_var_f(Element, Set, RSet),
  !,
  my_bag_diff(Elements, RSet, Difference).
my_bag_diff([Element|Elements], Set, [Element|Difference]) :-
  my_bag_diff(Elements, Set, Difference).

% my_set_nonvar_diff([], _, []).
% my_set_nonvar_diff([Element|Elements], Set, Difference) :-
%   member(Element, Set),
%   !,
%   my_set_nonvar_diff(Elements, Set, Difference).
% my_set_nonvar_diff([Element|Elements], Set, [Element|Difference]) :-
%   my_set_nonvar_diff(Elements, Set, Difference).

% Disjoint sets
% my_disjoint_sets(A,B) :-
%   my_set_diff(A,B,[]),
%   my_set_diff(B,A,[]).

% Multiset intersection
% Warning: not really multiset
my_set_inter([],_Ys,[]).
my_set_inter([X|Xs],Ys,[X|Zs]) :- 
  my_member_var(X,Ys), 
  my_set_inter(Xs,Ys,Zs).
my_set_inter([X|Xs],Ys,Zs) :- 
  \+ my_member_var(X,Ys),
  my_set_inter(Xs,Ys,Zs).

% Union
% my_set_union(X,Y,Z) :-
%   my_merge(X,Y,U),
%   my_mergesort(U,Z).
  
my_set_union(X,Y,Z) :-
  my_merge_var(X,Y,U),
  my_mergesort(U,Z).
  
my_set_union_list(Xss,Ys) :-
  concat_lists(Xss,Zs),
  my_remove_duplicates_sort(Zs,Ys).
  
  
duplicates_in_list(Cs,Ds) :-
  my_remove_duplicates_sort(Cs,SCs),
  my_bag_diff(Cs,SCs,DDs),
  my_remove_duplicates(DDs,Ds).
   
  
% Merging two lists; the first one contains no duplicates
my_merge_var(L,[],L).
my_merge_var(L,[A|As],Rs) :-
  my_member_var(A,L), 
  !,
  my_merge_var(L,As,Rs).
my_merge_var(L,[A|As],Rs) :-
  my_merge_var([A|L],As,Rs).

% Merging two lists; the first one contains no duplicates
% my_merge(L,[],L).
% my_merge(L,[A|As],Rs) :-
%   member(A,L), 
%   !,
%   my_merge(L,As,Rs).
% my_merge(L,[A|As],Rs) :-
%   my_merge([A|L],As,Rs).

  
% Mergesort
% Keep duplicates
% Stable
my_mergesort(L,OL) :-
  my_mergesort(L,'@=<',OL). 

my_mergesort([],_P,[]). 
my_mergesort([A],_P,[A]).
my_mergesort([A,B|Rest],P,S) :-
  ms_divide([A,B|Rest],L1,L2),
  my_mergesort(L1,P,S1),
  my_mergesort(L2,P,S2),
  ms_merge(S1,S2,P,S).
  
ms_divide([],[],[]).
ms_divide([A],[A],[]).
ms_divide([A,B|R],[A|Ra],[B|Rb]) :-
  ms_divide(R,Ra,Rb).

ms_merge(A,[],_P,A).
ms_merge([],B,_P,B).
ms_merge([A|Ra],[B|Rb],P,[A|M]) :-
%  A =< B,
  my_apply(my_apply(P,A),B),
  ms_merge(Ra,[B|Rb],P,M).
ms_merge([A|Ra],[B|Rb],P,[B|M]) :-
%  A > B,
  \+ my_apply(my_apply(P,A),B),
  ms_merge([A|Ra],Rb,P,M). 

% Multi-key comparison
% Ordering specs (a,d), Selection operator, Left, Right
my_multi_key_compare(Os,SelOp,L,R) :-
  my_add_tup_arg(SelOp,L,LT),
  my_apply(LT,Ls),
  my_add_tup_arg(SelOp,R,RT),
  my_apply(RT,Rs),
  my_multi_key_compare(Os,Ls,Rs).

my_multi_key_compare([],[],[]).
my_multi_key_compare([_O|Os],[L|Ls],[R|Rs]) :-
  L == R,
  my_multi_key_compare(Os,Ls,Rs).
my_multi_key_compare([a|_Os],[L|_Ls],[R|_Rs]) :-
  L @< R.
my_multi_key_compare([d|_Os],[L|_Ls],[R|_Rs]) :-
  L @> R.

% Selection operators:
% - First argument of a triple
n1_of_3_tuple_arg((A,_,_),A).
% - Identity
%id(A,A).
% % - Last argument of a term
% last_arg(T,[A]) :-
%   T=..[_|As],
%   append(_,[A],As).

% Comparison predicate for sort: Sorts descending on the second argument of a 2-tuple
second_tuple_arg_desc_order((_,L),(_,R)) :-
  L @=< R.
% Comparison predicate for sort: Sorts descending on
last_arg_desc_order(LT,RT) :-
  LT=..[_|LAs],
  append(_,[LA],LAs),
  RT=..[_|RAs],
  append(_,[RA],RAs),
  LA @>= RA.

% Stratum comparison predicate, to be used with my_mergesort for ordering strata in listings
stratum_compare((Pred1,Stratum1),(Pred2,Stratum2)) :-
  (Stratum1,Pred1) @=< (Stratum2,Pred2).

% PDG arc comparison predicate, to be used with my_mergesort for ordering arcs in listings
arc_compare(Arc1,Arc2) :-
  from_to_arc(Arc1,F1,T1),
  from_to_arc(Arc2,F2,T2),
  (T1,F1) @=< (T2,F2).

% Rule comparison (ascending) predicate, to be used with my_mergesort for ordering Datalog rules
% Rules are ordered by predicate name, then, for arity, then, first are facts (rules without RHS),
% then rules (with RHS) (both in lexicographic Prolog standard order)

dlrule_compare_asc(datalog(R1,NVs1,_,_,_,_,_),datalog(R2,NVs2,_,_,_,_,_)) :-
  rule_compare_asc((R1,NVs1),(R2,NVs2)).

rule_compare_asc(Pair1,Pair2) :-
  Pair1 = (Rule1,_V1s),
  Pair2 = (Rule2,_V2s),
  (Rule1 = ':-'(LHS1,RHS1) -> Kind1 = rule ; LHS1 = Rule1, Kind1 = fact),
  (Rule2 = ':-'(LHS2,RHS2) -> Kind2 = rule ; LHS2 = Rule2, Kind2 = fact),
  (functor(LHS1,F1,_A1), 
   functor(LHS2,F2,_A2),
   F1 @< F2,
   !
   ;
   functor(LHS1,F,A1), 
   functor(LHS2,F,A2),
   A1 < A2,
   !
   ;
   functor(LHS1,F,A), 
   functor(LHS2,F,A),
   Kind1 == fact,
   Kind2 == rule,
   !
   ;
   functor(LHS1,F,A), 
   functor(LHS2,F,A),
   Kind1 == fact,
   Kind2 == fact,
   !,
   LHS1 @< LHS2   
   ;
   functor(LHS1,F,A), 
   functor(LHS2,F,A),
   Kind1 == rule,
   Kind2 == rule,
   ':-'(LHS1,RHS1) @< ':-'(LHS2,RHS2)).
  

% my_intersect_var(+L1,+L2,-L3): L3 = L1 intersect L2
my_intersect_var([],_L,[]) :-
  !.
my_intersect_var(_L,[],[]) :-
  !.
my_intersect_var([X|Xs],L,[X|RXs]) :-
  my_member_var(X,L),
  !,
  my_intersect_var(Xs,L,RXs).
my_intersect_var([_X|Xs],L,RXs) :-
  !,
  my_intersect_var(Xs,L,RXs).

% my_union_var(+L1,+L2,-L3): L3 = L1 union L2
my_union_var([],Xs,Xs).
my_union_var([X|Xs],Zs,Ys) :-
  my_member_var(X,Zs),
  !,
  my_union_var(Xs,Zs,Ys).
my_union_var([X|Xs],Zs,[X|Ys]) :-
  my_union_var(Xs,Zs,Ys).
  
% my_subtract_var(L1,L2,L3): L3=L1-L2
my_subtract_var(L,[],L).
my_subtract_var(From,[X|Xs],L) :-
  my_remove_var(X,From,To),
  !,
  my_subtract_var(To,Xs,L).

% my_check_subset_var(L1,L2): check whether L1 is a subset of L2
my_check_subset_var(L1,L2) :-
  my_subtract_var(L1,L2,[]).

% my_remove_var(X,L1,L2): [X]+L1=L2.
my_remove_var(_X,[],[]).
my_remove_var(X,[Y|Ys],Zs) :-
  X==Y,
  !,
  my_remove_var(X,Ys,Zs).
my_remove_var(X,[Y|Ys],[Y|Zs]) :-
  my_remove_var(X,Ys,Zs).

% my_remove(X,L1,L2): [X]+L1=L2.
my_remove(_X,[],[]).
my_remove(X,[X|Ys],Zs) :-
  !,
  my_remove(X,Ys,Zs).
my_remove(X,[Y|Ys],[Y|Zs]) :-
  my_remove(X,Ys,Zs).
  
% case-insensitive predicate remove from list
my_rdb_pred_remove(_X,[],[]).
my_rdb_pred_remove(X/A,[Y/A|Ys],Zs) :-
  to_uppercase(X,UX),
  to_uppercase(Y,UX),
  !,
  my_rdb_pred_remove(X,Ys,Zs).
my_rdb_pred_remove(X,[Y|Ys],[Y|Zs]) :-
  my_rdb_pred_remove(X,Ys,Zs).

% my_remove_non_ground(L1,L2).
my_remove_non_ground([],[]).
my_remove_non_ground([Y|Ys],[Y|Zs]) :-
  my_ground(Y),
  !,
  my_remove_non_ground(Ys,Zs).
my_remove_non_ground([_X|Ys],Zs) :-
  my_remove_non_ground(Ys,Zs).

% my_flatten(Xs,Ys) is true if Ys is a list of the elements in Xs.
% e.g. my_flatten([[[3,c],5,[4,[]]],[1,b],a],[3,c,5,4,1,b,a]).    
my_flatten(Xs,Ys) :-
  my_flatten(Xs,[],Ys).

my_flatten(X,As,[X|As]) :-
  var(X),
  !.
my_flatten([X|Xs],As,Ys) :- 
  my_flatten(Xs,As,As1), 
  my_flatten(X,As1,Ys).
my_flatten(X,As,[X|As]) :-
  \+ my_is_list(X).
my_flatten([],Ys,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FD Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Build FD Constant Ranges:
%   From a list of integers i1...im, build (x1..x2)\/...\/(xn..xn),
%   where this range represents ii
build_fd_range([X|Xs],Range) :-
  build_fd_range(X,Xs,X,Range).

build_fd_range(Y,[],X,(X..Y)) :-
  !. % Help SICStus to not retry on redo
build_fd_range(X,[Y|Xs],Z,Range) :-
  Y is X+1,
  !,
  build_fd_range(Y,Xs,Z,Range).
build_fd_range(X,[Y|Xs],Z,(Z..X)\/Range) :-
  build_fd_range(Y,Xs,Y,Range).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Term to string my_term_to_string(+Term,+Quoted,-String,+NameVariables)
% NameVariables contains program variable names in Term as pairs Name=Variable
% If there are variables without names, they are assigned new names
% If Quoted=unquoted, then quotes surrounding upcase atoms are omitted

my_term_to_string(T,S) :- 
  my_term_to_string(T,[quoted(true)],S,[]).
  
my_term_to_string(T,S,NVs) :- 
  my_term_to_string(T,[quoted(true)],S,NVs).
  
my_term_to_string_unquoted(T,S) :- 
  my_term_to_string(T,[quoted(false)],S,[]).

my_term_to_string(T,Q,S,NVs) :- 
  assign_variable_names(T,NVs,CNVs),
  my_term_to_string_pl(T,Q,S,CNVs).
  
% Used by calls to my_term_to_string_pl for option portray(true).
% This quotes atoms for which quoted(true) does not quote, such as ***
portray(X) :-
  atom(X),
  write(''''),
  write(X),
  write('''').

  
% Do this directly with read_from_codes
% my_string_to_term(String,Term)
  
atom_to_date(A,D) :-
  atom_codes(A,As),
  string_to_date(As,D),
  !.
atom_to_date(A,_D) :-
  date_format([S|Ts]),
  atom_codes(S,Ss),
  findall(R,(member(T,Ts),member((T,R),[('YYYY',"[BC]Year"),('MM',"Month"),('DD',"Day")])),Rs),
  concat_strs_with(Rs,Ss,Fs),
  atom_codes(F,Fs),
  my_raise_exception(generic,syntax(['Invalid date: ',A,'. Valid format is ',F]),[]).
  
string_to_date(StrDate,Date) :-
  date_format([Separator|DateTokens]),
  string_to_date(DateTokens,Separator,Date,StrDate,"").

% Cast a string to a date w.r.t. the current date format
string_to_date(date(Y,M,D)) -->
  {date_format([S|Tokens])},
  string_to_date(Tokens,S,date(Y,M,D)),
  % Set optional fields
  {date_default_year(Y),
   date_default_month(M),
   date_default_day(D)}.
  
% Default date parts as of Oracle
date_default_year(Y) :-
  nonvar(Y),
  !.
date_default_year(Y) :-
  my_current_datetime((Y,_M,_D,_H,_Mi,_S)).
  
date_default_month(6) :-
  !.
date_default_month(_).

date_default_day(1) :-
  !.
date_default_day(_).

string_to_date(['YYYY'|Tokens],S,date(Y,M,D)) -->
  my_sql_blanks_star,
  my_kw("BC"),
  my_sql_blanks_star,
  my_positive_integer(Y1),
  my_sql_blanks_star,
  {Y is 1-Y1},
  date_time_separator(S),
  string_to_date(Tokens,S,date(Y,M,D)).
string_to_date(['YYYY'|Tokens],S,date(Y,M,D)) -->
  !,
  my_sql_blanks_star,
  my_integer(Y),
  my_sql_blanks_star,
  date_time_separator(S),
  string_to_date(Tokens,S,date(Y,M,D)).
string_to_date(['MM'|Tokens],S,date(Y,M,D)) -->
  !,
  my_sql_blanks_star,
  my_positive_integer(M),
  my_sql_blanks_star,
  date_time_separator(S),
  string_to_date(Tokens,S,date(Y,M,D)).
string_to_date(['DD'|Tokens],S,date(Y,M,D)) -->
  !,
  my_sql_blanks_star,
  my_positive_integer(D),
  my_sql_blanks_star,
  date_time_separator(S),
  string_to_date(Tokens,S,date(Y,M,D)).
string_to_date([],_S,date(Y,M,D)) -->
  [],
  {date_default_year(Y),
   date_default_month(M),
   date_default_day(D)}.
  
atom_to_time(A,D) :-
  atom_codes(A,As),
  string_to_time(As,UD),
  my_normalize_time(UD,D),
  !.
atom_to_time(A,_D) :-
%  my_raise_exception(generic,syntax(['Invalid time: ',A,'. Valid format is HH:Mi:SS']),[]).
  time_format([S|Ts]),
  atom_codes(S,Ss),
  findall(R,(member(T,Ts),member((T,R),[('HH',"Hour"),('Mi',"Minute"),('SS',"Second")])),Rs),
  concat_strs_with(Rs,Ss,Fs),
  atom_codes(F,Fs),
  my_raise_exception(generic,syntax(['Invalid time: ',A,'. Valid format is ',F]),[]).
  
string_to_time(As,D) :-
  string_to_time(D,As,"").
  
% Cast a string to a time w.r.t. the current time format
string_to_time(time(H,Mi,Se)) -->
  {time_format([S|Tokens])},
  string_to_time(Tokens,S,time(H,Mi,Se)),
  % Set optional fields
  {time_default_hour(H),
   time_default_minute(Mi),
   time_default_second(Se)}.

% Default time parts as of Oracle
time_default_hour(0) :-
  !.
time_default_hour(_).
  
time_default_minute(0) :-
  !.
time_default_minute(_).

time_default_second(0) :-
  !.
time_default_second(_).

string_to_time(['HH'|Tokens],S,time(H,Mi,Se)) -->
  !,
  my_sql_blanks_star,
  my_positive_integer(H),
  my_sql_blanks_star,
  date_time_separator(S),
  string_to_time(Tokens,S,time(H,Mi,Se)).
string_to_time(['Mi'|Tokens],S,time(H,Mi,Se)) -->
  !,
  my_sql_blanks_star,
  my_positive_integer(Mi),
  my_sql_blanks_star,
  date_time_separator(S),
  string_to_time(Tokens,S,time(H,Mi,Se)).
string_to_time(['SS'|Tokens],S,time(H,Mi,Se)) -->
  !,
  my_sql_blanks_star,
  my_number(Se),
  my_sql_blanks_star,
  date_time_separator(S),
  string_to_time(Tokens,S,time(H,Mi,Se)).
string_to_time([],_S,time(H,Mi,Se)) -->
  [],
  {time_default_hour(H),
   time_default_minute(Mi),
   time_default_second(Se)}.


atom_to_datetime(A,D) :-
  atom_codes(A,As),
  string_to_datetime(As,D),
  !.
atom_to_datetime(A,_D) :-
  my_raise_exception(generic,syntax(['Invalid datetime: ',A,'. Valid format is YYYY-MM-DD HH:Mi:SS']),[]).
  
string_to_datetime(As,D) :-
  string_to_datetime(D,As,"").
  
string_to_datetime(datetime(Y,M,D,H,Mi,S)) -->
  string_to_date(date(Y,M,D)),
  my_blanks,
  string_to_time(time(H,Mi,S)).
  
date_to_atom(date(Y,M,D),A) :-
  date_format([S|Fs]),
  date_to_atom(Fs,S,Y,M,D,'',A).
  
date_to_atom([],_S,_Y,_M,_D,A,A).
date_to_atom([F1,F2|Fs],S,Y,M,D,Ai,Ao) :-  
  date_to_atom([F1],S,Y,M,D,Ai,Ao1),
  atomic_concat(Ao1,S,Ao2),
  date_to_atom([F2|Fs],S,Y,M,D,Ao2,Ao).
date_to_atom(['YYYY'],_S,Y,_M,_D,Ai,Ao) :-  
  format_year(Y,FY),
  atomic_concat(Ai,FY,Ao).
date_to_atom(['MM'],_S,_Y,M,_D,Ai,Ao) :-  
  pad_zeroes(M,2,FM),
  atomic_concat(Ai,FM,Ao).
date_to_atom(['DD'],_S,_Y,_M,D,Ai,Ao) :-  
  pad_zeroes(D,2,FD),
  atomic_concat(Ai,FD,Ao).


format_year(Y,FY) :-
  Y>0,
  !,
  pad_zeroes(Y,4,FY).
format_year(Y,FY) :-
  AY is abs(Y)+1,
  pad_zeroes(AY,4,PY),
  atomic_concat('BC ',PY,FY).
  
  
time_to_atom(time(H,M,Se),A) :-
  time_format([S|Fs]),
  time_to_atom(Fs,S,H,M,Se,'',A).
  
time_to_atom([],_S,_H,_M,_Se,A,A).
time_to_atom([F1,F2|Fs],S,H,M,Se,Ai,Ao) :-  
  time_to_atom([F1],S,H,M,Se,Ai,Ao1),
  atomic_concat(Ao1,S,Ao2),
  time_to_atom([F2|Fs],S,H,M,Se,Ao2,Ao).
time_to_atom(['HH'],_S,H,_M,_Se,Ai,Ao) :-  
  pad_zeroes(H,2,FH),
  atomic_concat(Ai,FH,Ao).
time_to_atom(['Mi'],_S,_H,M,_Se,Ai,Ao) :-  
  pad_zeroes(M,2,FM),
  atomic_concat(Ai,FM,Ao).
time_to_atom(['SS'],_S,_H,_M,Se,Ai,Ao) :-  
  pad_zeroes(Se,2,FSe),
  atomic_concat(Ai,FSe,Ao).
  
datetime_to_atom(datetime(Y,M,D,H,Mi,S),A) :-
  date_to_atom(date(Y,M,D),A1),
  time_to_atom(time(H,Mi,S),A2),
  atomic_concat_list([A1,' ',A2],A). % ISO 8601 recommends 'T' instead of ' ' to delimit date and time, but it might appear to be less readable
  
  
% Check a date format. It can include YYYY, MM, DD (all numeric) for year, month and day separated by a character
% Parsed format is a list: [Separator, First token, ..., Nth token]
check_date_format(AtomFormat,[S|Format]) :-
  atom_codes(AtomFormat,StrFormat),
  parse_date_format([],Format,S,StrFormat,""),
  !.
check_date_format(_StrFormat,_Format) :-
  write_error_log(['Invalid date format string. It can include single numeric occurrences of YYYY, MM, DD for year, month and day separated by a character. Example: YYYY-MM-DD']),
  fail.
  
% Check a time format. It can include HH, Mi, SS (all numeric) for hour, minute and second separated by a character
% Parsed format is a list: [Separator, First token, ..., Nth token]
check_time_format(AtomFormat,[S|Format]) :-
  atom_codes(AtomFormat,StrFormat),
  parse_time_format([],Format,S,StrFormat,""),
  !.
check_time_format(_StrFormat,_Format) :-
  write_error_log(['Invalid time format string. It can include single numeric occurrences of HH, Mi, SS for hour, minute and second separated by a character. Example: HH:Mi:SS']),
  fail.
  
% Parse date format
parse_date_format(Ts,ATs,S) -->
  parse_date_time_format(Ts,ATs,S,date).

% Parse time format
parse_time_format(Ts,ATs,S) -->
  parse_date_time_format(Ts,ATs,S,time).

% Parse date or time format
parse_date_time_format(Ts,ATs,S,DateTime) -->
  {date_time_format_token(DateTime,Token),
   \+ memberchk(Token,Ts),
   name(Token,StrToken)},
  StrToken,
  {append(Ts,[Token],NTs)},
  date_time_separator(S),
  parse_date_time_format(NTs,ATs,S,DateTime).
parse_date_time_format(Ts,Ts,_S,_DT) -->
  [].
  
% Parse datetime format
parse_datetime_format(Ts,ATs) -->
  {date_time_format_token(DateTime,Token),
   \+ memberchk(Token,Ts),
   name(Token,StrToken)},
  StrToken,
  {append(Ts,[Token],NTs)},
  date_time_separator(S),
  parse_datetime_format(NTs,ATs,S,DateTime).
parse_datetime_format(Ts,Ts,_S,_DT) -->
  [].

date_time_separator(S) -->
  [C],
  {name(S,[C])}.
date_time_separator(_S) -->
  [].
  
date_time_format_token(date,'YYYY').
date_time_format_token(date,'MM').
date_time_format_token(date,'DD').
date_time_format_token(time,'HH').
date_time_format_token(time,'Mi').
date_time_format_token(time,'SS').
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File I/O
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Opening a File

try_open(F,CFN,St) :- 
  (my_file_exists(F) ->
   my_absolute_filename(F,CFN)
   ;
   atom_concat(F,'.dl',FP), 
   (my_file_exists(FP) ->
     my_absolute_filename(FP,CFN)
    ;
     my_working_directory(CD),
     atom_concat(CD,F,CDF),
     (my_file_exists(CDF) ->
       my_absolute_filename(CDF,CFN)
      ;
       atom_concat(CD,FP,CDFP),
       my_file_exists(CDFP),
       my_absolute_filename(CDFP,CFN)
     )
   )
  ),
  !, 
  (open(CFN,read,St),
   set_input(St) ->
   true
   ;
   write_error_log(['Stream cannot be opened.',nl]),
   fail). 
try_open(F,_,_) :-
  write_error_log(['File ''',F,''' not found.',nl]),
  fail.

try_close(Stream) :-
  catch(close(Stream),_,true).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User input
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_input_string(Str) :-
  flush_output,
  readln(Str,_),
  (current_batch_ID(ID) -> inc_lines(ID,1), write_string_log(Str), nl_log ; true),
  (\+ current_batch_ID(_), my_log([_|_]) -> write_only_to_log(Str), nl_only_to_log ; true).

% inc_line(ID) :-
%   retract(batch(ID,L,F,S)),
%   L1 is L+1,
%   assertz(batch(ID,L1,F,S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OS Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cat a file

cat_file(File) :-
  display_filename(File,DFN),
  my_absolute_filename(File,CFN),
  write_log_list(['%% BEGIN ',DFN,' %%',nl]),
  open(CFN,read,Hin),
  process_cat_file(Hin),
  write_log_list(['%% END   ',DFN,' %%']).
  
process_cat_file(Hin) :-  
  repeat,
    get_char(Hin,C),
    (
      C==end_of_file,
      try_close(Hin)
     ;
      write_log(C),
      fail
    ).

display_filename(File,File) :-
  host_safe(on),
  !.
display_filename(File,CFN) :-
  my_absolute_filename(File,CFN).

    
% Remove a file

rm_file(NFileName) :-
  ensure_atom(NFileName,FileName),
  (my_file_exists(FileName) ->
    my_remove_file(FileName)
   ;
    write_error_log(['File does not exist.'])
  ).


% Changing the Current Path

cd_path(NPath) :-
  ensure_atom(NPath,Path),
  (my_directory_exists(Path) ->
    my_change_directory(Path),
    (verbose(on) -> pwd_path ; true)
   ;
    write_error_log(['Cannot access the path ''', Path, '''.'])
  ).


% Displaying the Current Path

pwd_path :-
  my_working_directory(Path),
  (tapi(off) ->
   write_info_log(['Current directory is:',nl,'  ',Path]) 
  ;
   write_log_list([Path,nl])
  ).


% Listing Directory Contents

ls :-
  my_working_directory(WorkingPath),
  ls(WorkingPath).

ls(NPath) :-
  ensure_atom(NPath,Path),
  (\+ (my_directory_exists(Path)) ->
   write_warning_log(['Path ''', Path, ''' does not exist (wildcards are not allowed).'])
   ;
   (
    my_absolute_filename(Path, AbsolutePath),
    my_directory_files(AbsolutePath, Files),
    my_directory_directories(AbsolutePath, Directories),
    write_info_log(['Contents of ', AbsolutePath]), 
    nl_compact_log,
    write_log('Files:'), 
    write_dir_files(Files), 
    nl_log,
    write_log('Directories:'), 
    write_dir_directories(Directories), 
    nl_log)).


% Writing each File in a Directory. Path comes without final slash

write_dir_files([]).
write_dir_files([F|Fs]) :-
  nl_log, 
  write_log('  '), 
  write_log(F), 
  write_dir_files(Fs).


% Writing each Directory in a Directory

write_dir_directories([]).
write_dir_directories([F|Fs]) :-
  F \== '.',
  F \== '..', 
  !,
  nl_log,
  write_log('  '),
  write_log(F),
  write_dir_directories(Fs).
write_dir_directories([_F|Fs]) :-
  write_dir_directories(Fs).

% Copying a file
copy_file(InFile,OutFile) :-
  catch(
   (
    open(InFile,read,InStream),
    open(OutFile,write,OutStream),
    copy_stream(InStream,OutStream),
    close(OutStream),
    close(InStream)
   ),
   _M,
   write_error_log(['When copying "',InFile,'" to "',OutFile,'"'])).

% Copying from a stream to another
copy_stream(InStream,OutStream):-
  repeat,
    get_code(InStream,C),
    (end_of_file(C),
     flush_output(OutStream)
     ;
     put_code(OutStream,C),
     fail
    ).

% Include a first line in a file if it is not already
include_line_in_file(Line,File) :-
  is_line_in_file(Line,File),
  !.
include_line_in_file(Line,File) :-
  touch_file(File),
  temp_file(File,TempFile),
  open(File,read,InStream),
  open(TempFile,append,OutStream),
  include_line_in_stream(Line,InStream,OutStream),
  close(InStream),
  close(OutStream),
  copy_file(TempFile,File),
  rm_file(TempFile).

include_line_in_stream(Line,InStream,OutStream) :-
  write(OutStream,Line),
  nl(OutStream),
  copy_stream(InStream,OutStream).

is_line_in_file(Line,File) :-
  file_exists(File),
  open(File,read,Stream),
  call_cleanup(line_in_stream(Line,Stream,Found),close(Stream)),
  !,
  Found==true.
  
line_in_stream(Line,Stream,Found) :-
  atom_codes(Line,StrLine),
  repeat,
    readln(Stream,StrRead,E),
    (E==end_of_file
      ->
       Found=false,
       !
      ;
       StrLine==StrRead,
       Found=true
    ).
    

% Delete all lines Line in a file
delete_line_from_file(Line,File) :-
  file_exists(File),
  !,
  temp_file(File,TempFile),
  open(File,read,InStream),
  open(TempFile,append,OutStream),
  call_cleanup(delete_line_from_stream(Line,InStream,OutStream),(close(InStream),close(OutStream))),
  copy_file(TempFile,File),
  rm_file(TempFile).
delete_line_from_file(_Line,_File).

delete_line_from_stream(Line,InStream,OutStream) :-
  atom_codes(Line,StrLine),
  repeat,
    readln(InStream,StrRead,E),
    (E==end_of_file
      ->
       !
      ;
       (StrLine==StrRead
        ->
         fail
        ;
         atom_codes(LineRead,StrRead),
         write(OutStream,LineRead),
         nl(OutStream),
         fail
       )
    ).
    
    
% Delete a file if it exists
delete_file_if_exists(File) :-
  file_exists(File),
  !,
  rm_file(File).
delete_file_if_exists(_File).

% Delete a file if it is empty
delete_file_if_empty(File) :-
  file_exists(File),
  is_empty_file(File),
  !,
  rm_file(File).
delete_file_if_empty(_File).

is_empty_file(File) :-
  open(File,read,InStream),
  call_cleanup(stream_at_eof(InStream,EOF),close(InStream)),
  EOF==true.

stream_at_eof(InStream,EOF) :-
  my_get0(InStream,C),
  (end_of_file(C)
   ->
    EOF=true
   ;
    EOF=false),
  !.
      
:- dynamic(skipped_lines/1).

stream_find_label(Stream,Label,SkippedLines) :-
  retractall(skipped_lines(_)),
  assertz(skipped_lines(0)),
  stream_property(Stream, position(StreamPosition)),
  repeat,
    readln(Stream,String,E),
    ((E==end_of_file,
      !,
      write_error_log(['Label ',Label,' not found']),
      set_stream_position(Stream,StreamPosition)
      )
    ;
     retract(skipped_lines(L)),
     L1 is L+1,
     assertz(skipped_lines(L1)),
     is_label(String,Label)),
  skipped_lines(SkippedLines).
  

% Return a temporary file name based on the input file name and currente time
temp_file(File,TempFile) :-
  my_get_time(MS),
  atomic_concat_list([File,'_',MS],TempFile).
    
% Return the absolute file name of a file w.r.t. the system start path
my_start_path_absolute_file_name(File,AbsoluteFileName) :-
  start_path(RelativeToPath),
  my_absolute_file_name(File,RelativeToPath,AbsoluteFileName).
  
% my_relative_file_path(+FilePath,+RelativeTo,-RelFilePath)
% Return the relative file name of an absolute file w.r.t. the given reference path (RelativeTo, an absolute path)
% FilePath is an absolute file
% RelativeTo is an absolute path, and it must end in '/'
my_relative_file_path(FilePath,RelativeTo,RelFilePath) :-
  atom_codes(FilePath,StrFilePath),
  atom_codes(RelativeTo,StrRelativeTo),
  equal_up_to_last(StrFilePath,StrRelativeTo,"/",StrTailFilePath,StrTailRelativeTo),
  [Bar]="/",
  findall(_,member(Bar,StrTailRelativeTo),BarsTailFilePathList),
  length(BarsTailFilePathList,BarsTailFilePath),
  n_up_dir_levels(BarsTailFilePath,StrUpDirLevels),
  append(StrUpDirLevels,StrTailFilePath,StrRelFilePath),
  atom_codes(RelFilePath,StrRelFilePath).
  
equal_up_to_last([X|StrFilePath],[X|StrRelativeTo],[C],StrTailFilePath,StrTailRelativeTo) :-
  X\==C,
  !,
  equal_up_to_last(StrFilePath,StrRelativeTo,[C],StrTailFilePath,StrTailRelativeTo).
equal_up_to_last([C|StrFilePath],[C|StrRelativeTo],[C],StrFilePath,StrRelativeTo) :-
  \+ equal_up_to_last(StrFilePath,StrRelativeTo,[C],_StrTailFilePath,_StrTailRelativeTo),
  !.
equal_up_to_last([C|StrFilePath],[C|StrRelativeTo],[C],StrTailFilePath,StrTailRelativeTo) :-
  equal_up_to_last(StrFilePath,StrRelativeTo,[C],StrTailFilePath,StrTailRelativeTo).

n_up_dir_levels(0,[]) :-
  !.
n_up_dir_levels(N,[D,D,S|Levels]) :-
  [D,D,S]="../",
  N1 is N-1,
  n_up_dir_levels(N1,Levels).
  
  
% Touch file. If file does not exist, it is created  
touch_file(File) :-
  open(File,append,Stream),
  close(Stream).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec_if(Condition,Goal) :-
  (Condition -> Goal ; true).
  
% Verbose display

exec_if_verbose_on(Goal1,Goal2,Goal3,Goal4,Goal5) :-
  exec_if_verbose_on((Goal1,Goal2,Goal3,Goal4,Goal5)).

exec_if_verbose_on(Goal1,Goal2,Goal3,Goal4) :-
  exec_if_verbose_on((Goal1,Goal2,Goal3,Goal4)).

exec_if_verbose_on(Goal1,Goal2,Goal3) :-
  exec_if_verbose_on((Goal1,Goal2,Goal3)).

exec_if_verbose_on(Goal1,Goal2) :-
  exec_if_verbose_on((Goal1,Goal2)).

exec_if_verbose_on(Goal) :-
  verbose(on),
  !,
  call(Goal).
exec_if_verbose_on(_Goal).

% Development display

exec_if_development_on(Goal1,Goal2,Goal3,Goal4) :-
  exec_if_development_on((Goal1,Goal2,Goal3,Goal4)).
  
exec_if_development_on(Goal1,Goal2,Goal3) :-
  exec_if_development_on((Goal1,Goal2,Goal3)).

exec_if_development_on(Goal1,Goal2) :-
  exec_if_development_on((Goal1,Goal2)).

exec_if_development_on(Goal) :-
  development(on),
  !,
  call(Goal).
exec_if_development_on(_Goal).


% Writing a term with its textual variable names

write_with_types_NVs(T,[],NVs) :-
  ((development(off), nulls(on))
   ->
    my_hide_nulls(T,HT)
   ;
    HT=T
  ),
  write_with_NVs(HT,NVs).
write_with_types_NVs(T,Types,NVs) :-
  T=..[F|Args],
  write_log_list([F,'(']),
  write_args_with_types_NVs(Args,Types,NVs),
  write_log(')').

write_with_NVs(T,NVs) :- % No type info
  my_term_to_string(T,S,NVs),
  %my_term_to_string_pl(T,[quoted(true)],S,NVs),
  write_string_log(S).

write_args_with_types_NVs([],[],_NVs).
write_args_with_types_NVs([A1,A2|As],[T1,T2|Types],NVs) :-
  write_arg_with_type_NVs(A1,T1,NVs),
  write_log(','),
  write_args_with_types_NVs([A2|As],[T2|Types],NVs).
write_args_with_types_NVs([A],[T],NVs) :-
  write_arg_with_type_NVs(A,T,NVs).

write_arg_with_type_NVs('$NULL'(Id),_,_NVs) :-
  !,
  ((development(off), nulls(on))
   ->
    write_log(null)
   ;
    write_quoted_log('$NULL'(Id))
  ).
write_arg_with_type_NVs(A,string(_),NVs) :-
  !,
  my_term_to_string_pl(A,[quoted(true),portrayed(true)],S,NVs),
  write_string_log(S).
write_arg_with_type_NVs(A,_Type,NVs) :-
  write_with_NVs(A,NVs).

% Replaces each variable in a term by its name
term_to_term_NVs(T,NVs,CT) :-
  copy_term([T,NVs],[CT,CNVs]),
  term_variables(CT,Vs),
  replace_var_by_name_list(Vs,CNVs).

% write_with_NVs_list([],_NVs).
% write_with_NVs_list([T|Ts],NVs) :-
%   write_with_NVs(T,NVs),
%   write_with_NVs_list(Ts,NVs).

write_cond_unquoted_with_NVs_list([],_NVs).
write_cond_unquoted_with_NVs_list([nl|Ts],NVs) :-
  !,
  nl_log,
  write_cond_unquoted_with_NVs_list(Ts,NVs).
write_cond_unquoted_with_NVs_list(['$cond'(G,T1s)|Ts],NVs) :-
  !,
  (call(G) -> write_cond_unquoted_with_NVs_list(T1s,NVs) ; true),
  write_cond_unquoted_with_NVs_list(Ts,NVs).
write_cond_unquoted_with_NVs_list([T|Ts],NVs) :-
  my_ground(T),
  !,
  write_log(T),
  write_cond_unquoted_with_NVs_list(Ts,NVs).
write_cond_unquoted_with_NVs_list([T|Ts],NVs) :-
  T=..[F|_],
  is_system_identifier(F),
  !,
  write_log(T),
  write_cond_unquoted_with_NVs_list(Ts,NVs).
write_cond_unquoted_with_NVs_list([T|Ts],NVs) :-
  write_with_NVs(T,NVs),
  write_cond_unquoted_with_NVs_list(Ts,NVs).

write_unquoted_with_NVs_list([],_NVs).
write_unquoted_with_NVs_list([T|Ts],NVs) :-
  my_ground(T),
  !,
  write_log(T),
  write_unquoted_with_NVs_list(Ts,NVs).
write_unquoted_with_NVs_list([T|Ts],NVs) :-
  write_with_NVs(T,NVs),
  write_unquoted_with_NVs_list(Ts,NVs).

write_with_NVs_delimited_list([],_NVs) :-
  write_log('[]'),
  !.
write_with_NVs_delimited_list(Ts,NVs) :-
  write_log('['),
  my_list_to_tuple(Ts,TTs),
  write_with_NVs(TTs,NVs),
  write_log(']').


% Comma-separated writing of a a list of terms with their textual variable names

write_csa_with_NVs([],_).
write_csa_with_NVs([T],Vs) :-
  write_with_NVs(T,Vs).
write_csa_with_NVs([T1,T2|RTs],Vs) :-
  write_with_NVs(T1,Vs),
  write_log(','), 
  write_csa_with_NVs([T2|RTs],Vs).

% Write a list of terms only to console

write_list(L) :-
  write_list(L,[]).
  
writeq_list(L) :-
  write_list(L,[quoted(true)]).

write_list([],_).
write_list([T|Ts],Opts) :-
  write_term(T,Opts),
  write_list(Ts,Opts).

% Verbose output of lists

write_verb(L) :-
  (verbose(on), tapi(off)
   -> 
    write_verb_list(L)
   ;
    true).
    
write_verb_list(_L) :-
  (tapi(on)
   ;
   verbose(off)
  ),
  !.
write_verb_list([]).
write_verb_list([T|Ts]) :-
  write_log(T),
  write_verb_list(Ts).

% Log Output: Both current stream and log file, if enabled

% write_plain_log(X) :-
%   (output(on)
%    ->
%    write(X)
%    ;
%    true
%   ),
%   (my_log(_F,S)
% %  , \+ batch(_,_,_)
%    -> 
%     write(S,X) 
%    ; 
%     true
%   ).
write_plain_log(X) :-
  output(O),
  (O==off
   ->
   true
   ;
   (O==on
    ->
     write(X)
    ;
     true
   ),
   write_to_logs(X)
  ).

write_nl_to_logs :-
  my_log(Logs),
  member((_,_,S),Logs),
  nl(S),
  fail.
write_nl_to_logs.
  
write_to_logs(X) :-
  my_log(Logs),
  member((_,_,S),Logs),
%  seek(S,0,eof,_), % The log can be externally reduced from its beginning
  write(S,X),
%  flush_output(S),
  fail.
write_to_logs(_X).
  
write_quoted_to_logs(X) :-
  my_log(Logs),
  member((_,_,S),Logs),
%  seek(S,0,eof,_), % The log can be externally reduced from its beginning
  write_term(S,X,[quoted(true)]),
%  flush_output(S),
  fail.
write_quoted_to_logs(_X).
  
write_log(Var) :-
  var(Var),
  !,
  write_plain_log(Var).
write_log(nl) :-
  nl_log,
  !.
write_log('$void') :-
  !.
write_log('$tab'(N)) :-
  (tapi(off)
   ->
    my_spaces(N,S),
    write_log_list([S])
   ;
    true
  ),
  !.
write_log('$quoted'(CT)) :-
  write_quoted_log(CT),
  !.
write_log('$NVs'(T,NVs)) :-
  write_with_NVs(T,NVs),
  !.
write_log('$exec'(G)) :-
  !,
  call(G),
  !.
write_log('$if'(C,T)) :-
  (call(C) -> write_log_list(T) ; true),
  !.
write_log('$tbc') :-
  !.
write_log(T) :-
  write_plain_log(T).
  
write_quoted_log(nl) :-
  nl_log,
  !.
write_quoted_log('.') :-
  write_plain_log('.'),
  !.
write_quoted_log(X) :-
  output(O),
  (O==off
   ->
    true
   ;
   (O==on
    ->
     write_term(X,[quoted(true)])
    ;
     true
   ),
   write_quoted_to_logs(X)
  ).
%      (lq_log(_LF,LS) ->
%       write_term(LS,X,[quoted(true)])
%      ;
%       true))

write_quoted_log_list([]).
write_quoted_log_list([X|Xs]) :-
  write_quoted_log(X),
  write_quoted_log_list(Xs).

nl_log :-
  output(O),
  (O==off
   ->
    true
   ;
   (O==on
    ->
     nl
    ;
     true
   ),
   nl_only_to_log
  ).

nl_no_log :-
  output(on),
  !,
  nl.
nl_no_log.


write_list_log(Xs) :-
  tapi(off),
  !,
  write_log(Xs),
  nl_log.
write_list_log(Xs) :-
  (
   member(X,Xs),
   write_log_list([X,nl]),
   fail
  ;
   true
  ).
  
  
% Log Output: with new program names for variables

write_log_fresh_NVs(X,Types) :-
  assign_variable_names(X,[],NVs),
  write_with_types_NVs(X,Types,NVs).

% Log Output for lists of terms

% write_log_list('$NVs'(Ts,NVs)) :-
%   findall('$NVs'(T,NVs),member(T,Ts),List),
%   write_log_list(List).
%   
write_log_list([]).
write_log_list([T|Ts]) :-
  write_log(T),
  write_log_list(Ts).

write_log_lines([]).
write_log_lines([T|Ts]) :-
  write_log(T),
  nl_log,
  write_log_lines(Ts).

% Log Output for lists of terms possibly in commands

% write_cmd_log_list([]).
% write_cmd_log_list([T|Ts]) :-
%   write_log(T),
%   write_cmd_log_list(Ts).

% Only-to-Log Output

% Strings
write_only_to_log(S) :-
  (S=[_|_] ; S=[]),
  !,
  (output(O),
   O\==off
   ->
    atom_codes(X,S), 
    write_to_logs(X)
   ;
    true). 
% Others
% WARNING: Add support for commands $quoted, $tab, nl, ...
% write_only_to_log(S) :-
%   (my_log(_F,H) ->
%     write(H,S)
%    ;
%     true).

% nl_only_to_log :-
%   (my_log(_F,H) ->
%     nl(H)
%    ;
%     true).
write_only_to_log(X) :-
  (output(O),
   O\==off
   ->
    write_to_logs(X)
   ;
    true).

nl_only_to_log :-
  (output(O),
   O\==off
   ->
    write_nl_to_logs
   ;
    true).
    
% Close immediate logs before user input to allow logs to be accessed while the turn is on the user
close_ilogs :-
  ilog(off),
  !.
close_ilogs :-
  my_log(Logs),
  close_logs(Logs).
    
% Open immediate logs just after the user input
open_ilogs :-
  ilog(off),
  !.
open_ilogs :-
  restore_logs.
  
restore_logs :-
  my_log(Logs),
  open_logs(Logs,NewLogs),
  set_flag(my_log(NewLogs)).

% flush_logs :-
%   ilog(on),
%   !,
%   my_log(Logs),
%   flush_logs(Logs).
% flush_logs.

% flush_logs([]).
% flush_logs([(_F,_,S)|Logs]) :-
%   flush_output(S),
%   flush_logs(Logs).

open_logs([],[]).
open_logs([(F,File,S)|Logs],[(F,File,NS)|NewLogs]) :-
  try_close(S), % 
  open(F,append,NS),
  open_logs(Logs,NewLogs).
    
close_logs :-
  my_log(Logs),
  close_logs(Logs).
  
close_logs([]).
close_logs([(_F,_,S)|Logs]) :-
  flush_output(S),
  try_close(S), 
  close_logs(Logs).
    
disable_log :-
  my_log(Logs),
  set_flag(my_log([])),
  set_flag(disabled_log(Logs)).

% resume_log :-
%   disabled_log(Logs),
%   !,
%   set_flag(my_log(Logs)),
%   retractall(disabled_log(_)).
% resume_log.
resume_log :-
  disabled_log(Logs),
  Logs\==[],
  !,
  set_flag(my_log(Logs)),
  set_flag(disabled_log([])).
resume_log.

% :-dynamic(my_paused_log/2).
% pause_log :-
%   retract(my_log(F,S)),
%   !,
%   assertz(my_paused_log(F,S)).
% pause_log.
%  
% resume_log :-
%   retract(my_paused_log(F,S)),
%   !,
%   assertz(my_log(F,S)).
% resume_log.


% Compact listings

nl_compact_log :-
  ((compact_listings(on) ; silent(on)),
   !
%   ((compact_listings(on)
%     ;
%     silent(on))
%    -> 
%     true 
   ;
    nl_log).

nl_compact_verb :-
  (compact_listings(on)
   -> 
    true 
   ;
    write_verb([nl])).

% TAPI writing

write_notapi_log_list(_M) :-
  tapi(on),
  !.
write_notapi_log_list(M) :-
  write_log_list(M).

write_tapi_log_list(_M) :-
  tapi(off),
  !.
write_tapi_log_list(M) :-
  write_log_list(M).

nl_tapi_log(Command) :-
  nl_tapi_log(Command,_Args).
  
nl_tapi_log(tapi,_Args) :-
  !.
nl_tapi_log(_Command,_Args) :-
  silent(on),
  !.
nl_tapi_log(silent,[Arg|_]) :- % /silent and a command
  Arg\==on,
  Arg\==off,
  !.
nl_tapi_log(_Command,_Args) :-
  nl_compact_log.

nl_tapi_log :-
  tapi(on),
  !.
nl_tapi_log :-
  nl_compact_log.
   
write_tapi_delimiter :-
  write_tapi_delimiter(delim).
  
write_tapi_delimiter(_D) :-
  tapi(off),
  !.  
write_tapi_delimiter(D) :-
  D\==delim,
  !.  
write_tapi_delimiter(_D) :-
  write_log_list(['$',nl]).
  
write_tapi_success :-
  tapi(off),
  !.
write_tapi_success :-
  write_log_list(['$success',nl]).

% write_tapi_true :-
%   tapi(off),
%   !.
write_tapi_true :-
  write_log_list(['$true',nl]).

% write_tapi_false :-
%   tapi(off),
%   !.
write_tapi_false :-
  write_log_list(['$false',nl]).

write_tapi_eot :-
  tapi(off),
  !.
write_tapi_eot :-
  write_log_list(['$eot',nl]).

%
% Error, warning and info messages
%
% Write error message, formatted as display status

write_error_verb_log(_Message) :-
  (verbose(off)
   ;
   tapi(on)
  ),
  !.
write_error_verb_log(Message) :-
  write_error_log(Message).

write_error_log(['$tbc']) :-
  tapi(off),
  !,
  write_log('Error: ').
write_error_log(Message) :-
  tapi(off),
  !,
  write_log('Error: '),
  write_log_list(Message),
  (append(_,[nl],Message)
   ->
   true
   ;
   nl_log
  ).
write_error_log(['$tbc']) :- % To be continued
  write_log_list(['$error',nl,0,nl]).
write_error_log(Message) :-
  write_log_list(['$error',nl,0,nl]),
  write_log_list(Message),
  (append(_,[nl],Message)
   ->
   true % For continuation error messages
   ;
   nl_log,
   write_tapi_eot % End of error report transmission
  ).
  
% Write warning if tapi is disabled
write_notapi_warning_log(_Message) :-
  tapi(on),
  !.
write_notapi_warning_log(Message) :-
  write_warning_log(Message).
  
% Write warning message, formatted as display status
write_warning_log(_Message) :-
  silent(on),
  !.
write_warning_log(Message) :-
  tapi(off),
  !,
  write_log('Warning: '),
  write_log_list(Message),
  nl_log.
write_warning_log(Message) :-
  write_log_list(['$error',nl,1,nl]),
  write_log_list(Message),
  nl_log,
  append(_,[Last],Message),
  ((Last=nl ; Last='$tbc')
   ->
   true % For continuation error messages
   ;
   write_tapi_eot % End of error report transmission
  ).

% Write info if tapi is disabled
write_notapi_info_log(_Message) :-
  tapi(on),
  !.
write_notapi_info_log(Message) :-
  write_info_log(Message).
  
% Write info if tapi is enabled
% write_tapi_info_log(_Message) :-
%   tapi(off),
%   !.
% write_tapi_info_log(Message) :-
%   write_info_log(Message).
  
% Write info message, formatted as display status
% With variable names:
% write_info_log(Message,NVs) :-
%   term_to_term_NVs(Message,NVs,NamedMessage),
%   write_info_log(NamedMessage).
  
% Plain
write_info_log(_Message) :-
  info(off),
  !.
write_info_log(Message) :-
  tapi(off),
  !,
  write_log('Info: '),
  (append(Head,['$tbc'],Message)  % With no ending nl (to be continued)
   ->
    write_log_list(Head)
   ;
    write_log_list(Message),
    nl_log).
write_info_log(Message) :-
  write_log_list(['$error',nl,2,nl]),
  write_log_list(Message),
  nl_log,
  append(_,[Last],Message),
  ((Last=nl ; Last='$tbc')
   ->
   true % For continuation error messages
   ;
   write_tapi_eot % End of error report transmission
  ).
  
  
write_info_silent_log(Message) :-
  (silent(off)
  ;
   tapi(on)),
  !,
  write_info_log(Message).
write_info_silent_log(_Message).

% write_running_info_verb_log(_Message) :-
%   verbose(off),
%   !.
% write_running_info_verb_log(Message) :-
%   write_running_info_log(Message).

% % write_running_info_log
% write_running_info_log(_Message) :-
%   (tapi(on)
%   ;
%    running_info(off)
%   ;
%    batch(_,_,_)).
% write_running_info_log(Message) :-
%   tapi(off),
%   running_info(on),
%   !,
%   write_log('Info: '),
%   write_log_list(Message).
  
% Write verbose info message, formatted as display status
write_info_verb_log(_Message) :-
  (verbose(off)
   ;
   silent(on)
   ;
   tapi(on)
  ),
  !.
write_info_verb_log(Message) :-
  write_info_log(Message).

% Write development info message, formatted as display status
write_info_dev_log(_Message) :-
  (development(off)
   ;
   tapi(on)
  ),
  !.
write_info_dev_log(Message) :-
  write_info_log(Message).

% Writing a string (Log Output)

% write_string_log([]) :-
%   !.
% write_string_log([C|Cs]) :-
%   atom_codes(A,[C]),
%   write_log(A), 
%   write_string_log(Cs).
write_string_log(Cs) :- % Does it work for individual special characters such as \r ?
  atom_codes(A,Cs),
  write_log(A).

% write_string([C|Cs]) :-
%   output(on),
%   !,
%   atom_codes(A,[C]),
%   write(A),
%   write_string(Cs).
% write_string(_Cs).
write_string(Cs) :-
  output(on),
  !,
  atom_codes(A,Cs),
  write(A).
write_string(_Cs).

% write_string_log_list([]) :-
%   !.
% write_string_log_list([C|Cs]) :-
%   write_string_log(C),
%   nl_log,
%   write_string_log_list(Cs).

write_string_info_log_list([]) :-
  !.
write_string_info_log_list([C|Cs]) :-
  write_log('Info: '),
  write_string_log(C),
  nl_log,
  write_string_info_log_list(Cs).

% Writing a string (Current Stream Output)

% write_string(_S) :-
%   output(off),
%   !.
% write_string([]) :-
%   !.
% write_string([C|Cs]) :-
%   atom_codes(A,[C]),
%   write(A), 
%   write_string(Cs).
  
% Writing a term and spaces to fit a given width (Log Output), quoting if needed

% write_tab_log(T,L) :-
%   my_term_to_string(T,ST,[]),
%   write_string_log(ST),
%   length(ST,STL),
%   SL is L-STL,
%   SL>=0,
%   !,
%   my_spaces(SL,S),
%   write_log(S).
% write_tab_log(_T,L) :-
%   my_spaces(L,S),
%   write_log_list([nl,S]).
  
% Writing a term and spaces to fit a given width (Log Output), avoid quoting 

write_unquoted_tab_log(T,L) :-
  write_log(T),
  atom_length(T,TL),
  SL is L-TL,
  SL>=0,
  !,
  my_spaces(SL,S),
  write_log(S).
write_unquoted_tab_log(_T,L) :-
  my_spaces(L,S),
  write_log_list([nl,S]).

  
my_spaces(SL,T):-
  my_spacesS(SL,S),
  atom_codes(T,S).
  
my_spacesS(0,[]) :-
  !.
my_spacesS(N,[Sp|Ts]) :-
  [Sp] = " ",
  N1 is N-1,
  my_spacesS(N1,Ts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Syntax error reporting

% syntax_error(Where) :- 
%   write_error_log(['Syntax error in ', Where]).
  

% Redefinition error

redefinition_error(F,A) :-
%  nl_compact_log,
  my_raise_exception(generic,syntax(['Syntax error. Trying to redefine the builtin ',F,'/',A]),[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For parsing variables, e.g.:
% a --> {p(X)}, X
% a --> {p(X)}, my_string(X)
  
my_string([]) --> 
  [].
my_string([C|Cs]) -->
  [C],
  {[A] = "'",
   A =\= C},
  my_string(Cs).

% For parsing keywords, irrespective of the case
my_kw([],Cs,Cs).
my_kw([CC|CCs],[C|Cs],Ys) :-
  [C] =\= "'",
  to_uppercase_char(C,CC),
  my_kw(CCs,Cs,Ys).

% Finding a keyword
find_kw([],Cs,Cs).
find_kw([CC|CCs],[C|Cs],Ys) :-
  to_uppercase_char(C,CC),
  find_kw(CCs,Cs,Ys).
find_kw(CCs,[_C|Cs],Ys) :-
  find_kw(CCs,Cs,Ys).
  
my_tail([C|Cs]) -->
  [C],
  my_tail(Cs).
my_tail([]) -->
  [].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test whether a term is ground
my_ground(Term) :-
  ground(Term).
% my_ground(Term) :-
%   copy_term(Term,Copy),
%   (Term == Copy ->
%    true;
%    fail).
   
% my_member_var_term: analogous to my_member_var, but for terms
my_member_var_term(X,Y) :-
  my_equal_up_to_renaming(X,Y), 
  !,
  X==Y.
my_member_var_term(_X,T) :- 
  var(T),
  !,
  fail.
my_member_var_term(X,C) :- 
  C =.. [_F|As],
  !, 
  my_member_var_term_list(X,As).

my_member_vars_term([],_T).
my_member_vars_term([X|Xs],T) :-
  my_member_var_term(X,T),
  my_member_vars_term(Xs,T).
  
  
my_member_var_term_list(_X,[]) :-
  !,
  fail.
my_member_var_term_list(X,[T|_Ts]) :-
  my_member_var_term(X,T).
my_member_var_term_list(X,[_T|Ts]) :-
  my_member_var_term_list(X,Ts).
      
% my_member_term: analogous to my_member, but for terms
my_member_term(X,T) :- 
  var(T),
  var(X),
  !,
  X=T.
my_member_term(_X,T) :- 
  var(T),
  !,
  fail.
my_member_term(X,X).
my_member_term(X,C) :-
  C =.. [_F|As],
  my_member_term_list(X,As).

my_member_term_list(_X,[]) :-
  !,
  fail.
my_member_term_list(X,[T|_Ts]) :-
  my_member_term(X,T).
my_member_term_list(X,[_T|Ts]) :-
  my_member_term_list(X,Ts).
      
% my_all_members_term: simitar to my_member_term but return all members, keeping variables
my_all_members_term(X,T,Xs) :-
  my_all_members_term(X,T,nonvar,Xs,[]).
 
% my_all_members_term: simitar to my_member_var_term but return all members, keeping variables
my_all_members_var_term(X,T,Xs) :-
  my_all_members_term(X,T,var,Xs,[]).
 
my_all_members_term(X,T,_) --> 
  {var(T),
   var(X),
   !},
  [T].
my_all_members_term(_X,T,_) --> 
  {var(T),
   !},
  [].
my_all_members_term(X,T,nonvar) -->
  {nonvar(X),
   my_subsumes(X,T),
   !},
  [T].
my_all_members_term(X,T,var) -->
  {nonvar(X),
   \+ \+ my_subsumes(T,X),
   !},
  [T].
my_all_members_term(X,C,V) -->
  {C =.. [_F|As],
  !},
  my_all_members_term_list(X,As,V).

my_all_members_term_list(_X,[],_) -->
  [].
my_all_members_term_list(X,[T|Ts],V) -->
  my_all_members_term(X,T,V),
  my_all_members_term_list(X,Ts,V).
      
% my_nth_member_term(X,N,T) :-
%   findall(X,my_member_term(X,T),Xs),
%   my_nth_member(X,N,Xs).
% % Nth member of a term (first index is 1)
% my_nth1_member_term(X,N,T) :-
%   findall(X,my_member_term(X,T),Xs),
%   my_nth1_member(X,N,Xs).
  
% singletons
singletons(T,Vs) :-
  my_term_variables_bag(T,DVs),
  singleton_vars(DVs,DVs,[],Vs).
  
singleton_vars([],_,Vi,Vi).
singleton_vars([V|DVs],AVs,Vi,[V|Vo]) :-
  findall(V,my_member_var(V,AVs),[V]),
  !,
  singleton_vars(DVs,AVs,Vi,Vo).
singleton_vars([_V|DVs],AVs,Vi,Vo) :-
  singleton_vars(DVs,AVs,Vi,Vo).

my_term_variables_bag(T,Vs) :-
  my_term_variables_bag(T,[],Vs).

my_term_variables_bag(V,Vi,Vo) :- 
  var(V),
  !,
  append(Vi,[V],Vo).
my_term_variables_bag('$NULL'(_),Vi,Vi) :-
  !. 
my_term_variables_bag(C,Vi,Vo) :- 
  C =.. [_F|As],
  !, 
  my_term_variables_bag_list(As,Vi,Vo).

my_term_variables_bag_list([],Vi,Vi).
my_term_variables_bag_list([X|Xs],Vi,Vo) :-
  my_term_variables_bag(X,Vi,Vo1),
  my_term_variables_bag_list(Xs,Vo1,Vo).
  
  
% Term depth less or equal than a given bound. 
% WARNING: Unused from 2.0
%term_depth_leq(T,D) :- 
%  (number(T) ; atom(T) ; var(T)),
%  !,
%  D>=0.
%term_depth_leq(C,D) :- 
%  C =.. [_F|As],
%  !, 
%  D1 is D-1,
%  term_depth_leq_list(As,D1).
%
%term_depth_leq_list([],_D) :-
%  !.
%term_depth_leq_list([T|Ts],D) :-
%  term_depth_leq(T,D), 
%  term_depth_leq_list(Ts,D).

% Replaces each i-th argument in term T
replace_ith_args_term(T,Idxs,IArgs,RT) :-
  T=..[F|Args],
  replace_idx_list(Args,Idxs,IArgs,RArgs),
  RT=..[F|RArgs].

replace_idx_list(Args,Idxs,IArgs,RArgs) :-
  replace_idx_from_list(Args,1,Idxs,IArgs,RArgs).
  
replace_idx_from_list([],_I,[],[],[]).
replace_idx_from_list([_Arg|Args],I,[I|Idxs],[IArg|IArgs],[IArg|RArgs]) :-
  !,
  I1 is I+1,
  replace_idx_from_list(Args,I1,Idxs,IArgs,RArgs).
replace_idx_from_list([Arg|Args],I,Idxs,IArgs,[Arg|RArgs]) :-
  I1 is I+1,
  replace_idx_from_list(Args,I1,Idxs,IArgs,RArgs).

% Replaces all occurrences of each functor in the list by its corresponding replacement in a term T
replace_functors([],[],T,T).
replace_functors([F|Fs],[RF|RFs],T,RT) :-
  replace_functor(F,RF,T,T1),
  replace_functors(Fs,RFs,T1,RT).

% Replaces all occurrences of functor O by N in a term T
replace_functor(_O,_N,T,T) :- 
  (number(T) ; var(T)),
  !.
replace_functor(O,N,O,N) :- 
  atom(O),
  !.
replace_functor(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  (F == O -> RF = N ; RF = F),
  replace_functor_list(O,N,As,RAs),
  RC =.. [RF|RAs].

replace_functor_list(_O,_N,[],[]) :-
  !.
replace_functor_list(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_functor(O,N,T,RT), 
  replace_functor_list(O,N,Ts,RTs).


% Replaces the functor of the terms in a list  
replace_functor_term_list([],_RF,[]).
replace_functor_term_list([T|Ts],RF,[RT|RTs]) :-
  T=..[_F|Args],
  RT=..[RF|Args],
  replace_functor_term_list(Ts,RF,RTs).


% Replaces all occurrences in a term T of functors starting by O by the same functor but replacing N by O
replace_functor_substring(_O,_N,T,T) :- 
  (number(T) ; var(T)),
  !.
replace_functor_substring(O,N,O,N) :- 
  atom(O),
  !.
replace_functor_substring(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  (atom(F), atom_concat(O,R,F) -> atom_concat(N,R,RF) ; RF = F),
  replace_functor_substring_list(O,N,As,RAs),
  RC =.. [RF|RAs].

replace_functor_substring_list(_O,_N,[],[]) :-
  !.
replace_functor_substring_list(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_functor_substring(O,N,T,RT), 
  replace_functor_substring_list(O,N,Ts,RTs).


% Replace occurences of functor Name by NewName in all the rules
% reachable from the give specification: name, predicate, head
replace_functor_dlrules_from(What,WhatObject,Name,NewName) :-
  (
   get_object_dlrules(What,WhatObject,DLs),
   member(DL,DLs),
   replace_functor(Name,NewName,DL,NewDL),
   retract(DL),
   assertz(NewDL),
   fail
   ;
   true
 ).
  
replace_functor_dlrules_from_list(_What,[],_Name,_NewName).
replace_functor_dlrules_from_list(What,[WhatObject|WhatObjects],Name,NewName) :-
   replace_functor_dlrules_from(What,WhatObject,Name,NewName),
   replace_functor_dlrules_from_list(What,WhatObjects,Name,NewName).
  

% Replace all occurrences of term O by N in a term T
replace_term(O,N,T,N) :- 
  O==T,
  !.
replace_term(_O,_N,T,T) :- 
  (number(T) ; var(T) ; atom(T)),
  !.
replace_term(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  replace_terms_list(O,N,As,RAs),
  RC =.. [F|RAs].

replace_terms_list(_O,_N,[],[]) :-
  !.
replace_terms_list(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_term(O,N,T,RT), 
  replace_terms_list(O,N,Ts,RTs).

replace_term_list([],[],T,T) :-  %% WARNING: This collides with previous definition of replace_term_list/4
  !.
replace_term_list([O|Os],[N|Ns],T,RT) :-
  !, 
  replace_term(O,N,T,RT1), 
  replace_term_list(Os,Ns,RT1,RT).

% Replace each term in Ts by each correspondint term in RTs in a term T resulting in RT
replace_list_term([],[],T,T).
replace_list_term([T|Ts],[RT|RTs],Term,RTerm) :-
  replace_term(T,RT,Term,RTerm1),
  replace_list_term(Ts,RTs,RTerm1,RTerm).
  
% Replace all non-var unifiable occurrences of term O by N in a term T
replace_var_term(O,N,T,N) :- 
  nonvar(T),
%  O=T,
  \+ \+ O=T,
  !.
replace_var_term(_O,_N,T,T) :- 
  (number(T) ; var(T) ; atom(T)),
  !.
replace_var_term(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  replace_var_term_list(O,N,As,RAs),
  RC =.. [F|RAs].

replace_var_term_list(_O,_N,[],[]) :-
  !.
replace_var_term_list(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_var_term(O,N,T,RT), 
  replace_var_term_list(O,N,Ts,RTs).

% Replace all non-var unifiable occurrences of term O by N in a term T, unifying variables of O and N
replace_var_term_unif(O,N,T,N) :- 
  nonvar(T),
  O=T,
  !.
replace_var_term_unif(_O,_N,T,T) :- 
  (number(T) ; var(T) ; atom(T)),
  !.
replace_var_term_unif(O,N,C,RC) :- 
  C =.. [F|As],
  !, 
  replace_var_term_unif_list(O,N,As,RAs),
  RC =.. [F|RAs].

replace_var_term_unif_list(_O,_N,[],[]) :-
  !.
replace_var_term_unif_list(O,N,[T|Ts],[RT|RTs]) :-
  !, 
  replace_var_term_unif(O,N,T,RT), 
  replace_var_term_unif_list(O,N,Ts,RTs).

% Replace all non-var unifiable occurrences of pattern O by pattern N in a term T committing condition C
replace_pattern_term(O,N,C,T,CN) :- 
  nonvar(T),
  copy_term([O,N],[CO,CN]),
  copy_term(T,CT),
  CO=CT,
  call(C),
%  O=T,
  !.
replace_pattern_term(_O,_N,_C,T,T) :- 
  (number(T) ; var(T) ; atom(T)),
  !.
replace_pattern_term(O,N,Cond,C,RC) :- 
  C =.. [F|As],
  !, 
  replace_pattern_term_list(O,N,Cond,As,RAs),
  RC =.. [F|RAs].

replace_pattern_term_list(_O,_N,_C,[],[]) :-
  !.
replace_pattern_term_list(O,N,C,[T|Ts],[RT|RTs]) :-
  !, 
  replace_pattern_term(O,N,C,T,RT), 
  replace_pattern_term_list(O,N,C,Ts,RTs).

replace_pattern_term_list([],[],_C,T,T) :-
  !.
replace_pattern_term_list([O|Os],[N|Ns],C,T,RT) :-
  !, 
  replace_pattern_term(O,N,C,T,RT1), 
  replace_pattern_term_list(Os,Ns,C,RT1,RT).


% Concat a list of atomic terms
atomic_concat_list([A],AtA) :-
  ensure_atom(A,AtA).
atomic_concat_list([A,B|C],D) :-
  atomic_concat(A,B,E),
  atomic_concat_list([E|C],D).

atomic_concat(A,B,C) :-
  ensure_atom(A,AtA),
  ensure_atom(B,AtB),
  atom_concat(AtA,AtB,C).
  
'remove_\r\n'(M,RM) :-
  atom_concat(RM,'\r\n',M),
  !.
'remove_\r\n'(M,M).

% substr(+String,+Offset,+Length,-Result)
substr(_S,_O,L,R) :-
  L<1,
  !,
  R=''.
substr(S,O,L,R) :-
  O<1,
  NL is L+O-1,
  !,
  substr(S,1,NL,R).
substr(S,O,_L,R) :-
  atom_length(S,LS),
  O>LS,
  !,
  R=''.
substr(S,O,L,R) :-
  atom_length(S,LS),
  O+L-1>LS,
  !,
  NL is LS-O+1,
  substr(S,O,NL,R).
substr(S,O,L,R) :-
  O1 is O-1,
  atom_codes(S,Ss),
  length(Ss,SL),
  (O1+L>SL -> L1 is SL-O1 ; L1=L),
  length(Ls,L1),
  length(Hs,O1),
  append(Hs,Ls,HLs),
  append(HLs,_,Ss),
  atom_codes(R,Ls).

% sublist(Member,List): True if the list Member is a sublist of List. [b,c] is a sublist of [a,b,c,d], but [b,d] is not.  
sublist(Xs,Ys) :-
  sublist(Xs,Ys,Xs).
  
sublist([],_Ys,_Zs).
sublist([X|Xs],[X|Ys],Zs) :-
  sublist(Xs,Ys,Zs).
sublist([X|Xs],[Y|Ys],Zs) :-
  X\==Y,
  [X|Xs]\==Zs,
  sublist(Zs,[Y|Ys],Zs).
sublist([X|Xs],[Y|Ys],Zs) :-
  X\==Y,
  [X|Xs]==Zs,
  sublist(Zs,Ys,Zs).

% subatom(Member,Atom): True if Member is part of Atom
subatom(M,A) :-
  atom_codes(M,Ms),
  atom_codes(A,As),
  sublist(Ms,As).
  
% subatom_list([Atoms],Atom): True if any of the Atoms is part of Atom 
subatom_list([M|_Ms],A) :-
  subatom(M,A).
subatom_list([_|Ms],A) :-
  subatom_list(Ms,A).

% Get user predicates in a rule
reachable_user_predicates_rule_list(Rules,Preds) :-
  reachable_predicates_rule_list(Rules,program,AllPreds),
  filter_system_predicates(AllPreds,Preds).

reachable_user_predicates_rule(Rule,Preds) :-
  reachable_user_predicates_rule_list([Rule],Preds).

user_predicates_body(B,Preds) :-
%   reachable_user_predicates_body(B,rule,Preds).
  reachable_predicates_body(B,rule,AllPreds),
  filter_system_predicates(AllPreds,Preds).

filter_system_predicates([],[]).
filter_system_predicates([S/_|APs],Ps) :-
  is_system_identifier(S),
  !,
  filter_system_predicates(APs,Ps).
filter_system_predicates([P|APs],[P|Ps]) :-
  filter_system_predicates(APs,Ps).
  
filter_system_identifiers([],[]).
filter_system_identifiers([S|APs],Ps) :-
  is_system_identifier(S),
  !,
  filter_system_identifiers(APs,Ps).
filter_system_identifiers([P|APs],[P|Ps]) :-
  filter_system_identifiers(APs,Ps).
  
reachable_predicates_rule(Rule,Context,Preds) :-
  reachable_predicates_rule_list([Rule],Context,[],Preds).

reachable_predicates_rule(':-'(B),Context,IPreds,OPreds) :-
  reachable_predicates_body(B,Context,IPreds,OPreds),
  !. % WARNING 12/10/2016
reachable_predicates_rule(':-'(H,B),Context,IPreds,OPreds) :-
  !,
  reachable_predicates_atom(H,Context,IPreds,Preds1),
  reachable_predicates_body(B,Context,Preds1,OPreds).
reachable_predicates_rule(H,Context,IPreds,OPreds) :-
  reachable_predicates_atom(H,Context,IPreds,OPreds).

reachable_predicates_rule_list(Rules,Preds) :-
  reachable_predicates_rule_list(Rules,program,Preds).

reachable_predicates_rule_list(Rules,Context,Preds) :-
  reachable_predicates_rule_list(Rules,Context,[],Preds).

reachable_predicates_rule_list([],_Context,IPreds,OPreds) :-
  my_remove_duplicates_sort(IPreds,OPreds).
reachable_predicates_rule_list([R|Rs],Context,IPreds,OPreds) :-
  reachable_predicates_rule(R,Context,IPreds,TPreds),
  reachable_predicates_rule_list(Rs,Context,TPreds,OPreds).

reachable_predicates_body(B,Preds) :-
  reachable_predicates_body(B,program,Preds).

reachable_predicates_body(B,Context,Preds) :-
  reachable_predicates_body(B,Context,[],Preds).

reachable_predicates_body(V,_Context,Preds,Preds) :-
  var(V),
  !.
reachable_predicates_body((B,Bs),Context,Predsi,Predso) :-
  !,
  reachable_predicates_literal(B,Context,Predsi,Preds1),
  reachable_predicates_body(Bs,Context,Preds1,Predso).
reachable_predicates_body((B;Bs),Context,Predsi,Predso) :-
  !,
  reachable_predicates_body((B,Bs),Context,Predsi,Predso).
reachable_predicates_body(B,Context,Predsi,Predso) :-
  reachable_predicates_literal(B,Context,Predsi,Predso).
  
% reachable_predicates_literal(L,Preds) :-
%   reachable_predicates_literal(L,[],Preds).
  
% Specify those builtins which can have in some of their arguments
reachable_predicates_literal(not(A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_literal(A,Context,Predsi,Predso).  
reachable_predicates_literal(-(A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_atom(A,Context,Predsi,Predso).  
reachable_predicates_literal(st(A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_literal(A,Context,Predsi,Predso).  
reachable_predicates_literal(call(A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_literal(A,Context,Predsi,Predso).  
reachable_predicates_literal(Ag,Context,Predsi,Predso) :-
  Ag=..[count,B|As],
  length(As,L),
  L>=1,
  L=<2,
  !,
  reachable_predicates_body(B,Context,Predsi,Predso).
reachable_predicates_literal(Ag,Context,Predsi,Predso) :-
  Ag=..[FAg,B,_,_],
  my_aggregate_relation(FAg,3),
  !,
  reachable_predicates_atom(B,Context,Predsi,Predso).
reachable_predicates_literal(OJ,Context,Predsi,Predso) :-
  OJ=..[OJF,L,R,C],
  my_outer_join_relation(OJF/3),
  !,
  reachable_predicates_atom(L,Context,Predsi,Preds1),
  reachable_predicates_atom(R,Context,Preds1,Preds2),
  reachable_predicates_body(C,Context,Preds2,Predso).
reachable_predicates_literal(group_by(A,_,_B,C),Context,Predsi,Predso) :-
  !,
  reachable_predicates_atom(A,Context,Predsi,Preds1),
  reachable_predicates_body(C,Context,Preds1,Predso).
reachable_predicates_literal(top(_N,A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_atom(A,Context,Predsi,Predso).
reachable_predicates_literal(distinct(A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_atom(A,Context,Predsi,Predso).
reachable_predicates_literal(distinct(_Vs,A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_atom(A,Context,Predsi,Predso).
reachable_predicates_literal(order_by(A,_Exprs,_Os),Context,Predsi,Predso) :-
  !,
  reachable_predicates_body(A,Context,Predsi,Predso).
reachable_predicates_literal(or(A,B),Context,Predsi,Predso) :-
  !,
  reachable_predicates_literal(A,Context,Predsi,Preds1),
  reachable_predicates_literal(B,Context,Preds1,Predso).
reachable_predicates_literal('=>'(_,A),Context,Predsi,Predso) :-
  !,
  reachable_predicates_body(A,Context,Predsi,Predso).
reachable_predicates_literal(A,_Context,Preds,Preds) :-
  functor(A,N,2),
  my_infix_comparison(N,_),
  !.
reachable_predicates_literal(A,Context,Predsi,Predso) :-
  reachable_predicates_atom(A,Context,Predsi,Predso).
  
% reachable_predicates_atom(A,Context,Predsi,Predso) :-
%   functor(A,N,Ar),
%   my_builtin_pred(N/Ar),
%   !,
%   A=..[N|Bs],
%   reachable_predicates_body_list(Bs,Context,Predsi,Predso).
% reachable_predicates_atom(A,_Context,Preds,Preds) :-
%   functor(A,F,_Ar),
%   is_system_identifier(F),
%   !.
reachable_predicates_atom(A,_Context,Preds,Preds) :-
  functor(A,F,Ar),
  member(F/Ar,Preds),
  !.
reachable_predicates_atom(A,program,Predsi,Predso) :-
  functor(A,F,Ar),
  findall(B,(datalog(':-'(H,B),_,_,_,_,_,_),functor(H,F,Ar)),Bs),
  reachable_predicates_body_list(Bs,program,[F/Ar|Predsi],Predso).
reachable_predicates_atom(A,rule,Predsi,[F/Ar|Predsi]) :-
  functor(A,F,Ar).
  
reachable_predicates_expr(V,_Context,Preds,Preds) :-
%  (var(V) ; atomic(V)),
  (var(V) ; number(V)),
  !.
reachable_predicates_expr(A,_Context,Preds,Preds) :-
  functor(A,F,Ar),
  (function(F,_,_,_,_,Ar) ; my_infix_operator(F,_,_,_,_,_,_)),
  !.

reachable_predicates_body_list([],_Context,Preds,Preds).
reachable_predicates_body_list([B|Bs],Context,Predsi,Predso) :-
  reachable_predicates_body(B,Context,Predsi,Preds1),
  reachable_predicates_body_list(Bs,Context,Preds1,Predso).
    
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% to_uppercase_char_list(+List,-UppercaseList)
% Converts the input list to uppercase

to_uppercase_char_list([],[]).
to_uppercase_char_list([X|Xs],[U|Cs]) :-
  to_uppercase_char(X,U),
  to_uppercase_char_list(Xs,Cs).

to_uppercase_char(X,U) :-
  [X] >= "a",
  [X] =< "z",
  !,
  [UA] = "A",
  [LA] = "a",
  U is X+UA-LA.
to_uppercase_char(X,X).

to_uppercase(LC,UC) :-
  atom_codes(LC,SLC),
  to_uppercase_char_list(SLC,SUC),
  atom_codes(UC,SUC).

to_uppercase_pred_list([],[]).
to_uppercase_pred_list([P|Ps],[UP|UPs]) :-
  to_uppercase_pred(P,UP),
  to_uppercase_pred_list(Ps,UPs).

to_uppercase_pred(N/A,UN/A) :-
  to_uppercase(N,UN).

to_uppercase_arc_list([],[]).
to_uppercase_arc_list([P|Ps],[UP|UPs]) :-
  to_uppercase_arc(P,UP),
  to_uppercase_arc_list(Ps,UPs).
  
to_uppercase_arc(L+R,UL+UR) :-
  to_uppercase_pred(L,UL),
  to_uppercase_pred(R,UR).
to_uppercase_arc(L-R,UL-UR) :-
  to_uppercase_pred(L,UL),
  to_uppercase_pred(R,UR).

% to_lowercase_char_list(+List,-UppercaseList) 
% Converts the input list to lowercase

to_lowercase_char_list([],[]).
to_lowercase_char_list([X|Xs],[U|Cs]) :-
  to_lowercase_char(X,U),
  to_lowercase_char_list(Xs,Cs).

to_lowercase_char(X,L) :-
  var(X),
  !,
  to_uppercase_char(L,X).
to_lowercase_char(X,L) :-
  [X] >= "A",
  [X] =< "Z",
  !,
  [UA] = "A",
  [LA] = "a",
  L is X-UA+LA.
to_lowercase_char(X,X).

to_lowercase(N,N) :-
  number(N),
  !.
to_lowercase(LC,UC) :-
  atom_codes(LC,SLC),
  to_lowercase_char_list(SLC,SUC),
  atom_codes(UC,SUC).

to_lowercase_list([],[]).
to_lowercase_list([T|Ts],[LT|LTs]) :-
  to_lowercase(T,LT),
  to_lowercase_list(Ts,LTs).
  
to_lowercase_term(X,X) :-
  (var(X);number(X);X=[]),
  !.
to_lowercase_term(X,Y) :-
  atom(X),
  !,
  to_lowercase(X,Y).
to_lowercase_term(X,Y) :-
  X=..[F|Args],
  to_lowercase(F,LF),
  to_lowercase_term_list(Args,LArgs),
  Y=..[LF|LArgs].
  
to_lowercase_term_list([],[]).
to_lowercase_term_list([X|Xs],[Y|Ys]) :-
  to_lowercase_term(X,Y),
  to_lowercase_term_list(Xs,Ys).
  
first_char_to_lowercase(Atom,LAtom) :-
  atom_codes(Atom,[C|Cs]),
  to_lowercase_char(C,LC),
  atom_codes(LAtom,[LC|Cs]).
    
replace_all_atom_list(Atom,[],[],Atom).
replace_all_atom_list(AtomIn,[AtomFind|AFs],[AtomReplace|ARs],AtomOut) :-
  replace_all_atom(AtomIn,AtomFind,AtomReplace,AtomOut1),
  replace_all_atom_list(AtomOut1,AFs,ARs,AtomOut).
  
replace_all_atom(AtomIn,AtomFind,AtomReplace,AtomOut) :-
  name(AtomIn,StrIn),
  name(AtomFind,StrFind),
  name(AtomReplace,StrReplace),
  replace_all_string(StrIn,StrFind,StrReplace,StrOut),
  name(AtomOut,StrOut).

replace_all_string(StrIn,StrFind,StrReplace,StrOut) :-
  replace_all_string(StrOut,StrFind,StrReplace,"",StrIn,"").
  
replace_all_string(IStr,StrFind,StrReplace,ROStr) -->
  StrFind,
  !,
  {append(StrReplace,OStr,IStr)},
  replace_all_string(OStr,StrFind,StrReplace,ROStr).
replace_all_string([C|Str],StrFind,StrReplace,OStr) -->
  [C],
  !,
  replace_all_string(Str,StrFind,StrReplace,OStr).
replace_all_string(Str,_StrFind,_StrReplace,Str) -->
  "".

% Display to string
% The output of executing Goal is represented in String
display_to_string(Goal,String) :-
  push_flag(pretty_print,off,PP),
  push_flag(output,on,O),
  disable_log,
%  one_line_display_to_string(Goal,String),
  with_output_to_codes(user:Goal,String),
  resume_log,
  pop_flag(output,O),
  pop_flag(pretty_print,PP).
% , !.
% display_to_string(Goal,_String) :-
%   my_raise_exception(generic,syntax(['Failed goal: ',Goal]),[]).

% one_line_display_to_string(Goal,String) :-
%   pause_log,
%   my_absolute_filename('tmp__.txt',AFN),
%   current_output(S0),
%   open(AFN,write,S1),
%   set_output(S1),
%   call(Goal),
%   set_output(S0),
%   close(S1),
%   !,
%   current_input(S2),
%   open(AFN,read,S3),
%   set_input(S3),
%   readln(String,_),
%   set_input(S2),
%   close(S3),
%   !,
%   rm_file(AFN),
%   !,
%   resume_log.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statistics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_statistics :-
  (display_statistics(off) ; my_statistics(off)),
  !.
display_statistics :-
  my_fz(fp_iterations,NFI),
  write_info_log(['Fixpoint iterations: ',NFI]),
  my_fz(edb_retrievals,NER),
  write_info_log(['EDB retrievals     : ',NER]),
  my_fz(idb_retrievals,NIR),
  write_info_log(['IDB retrievals     : ',NIR]),
  my_fz(et_retrievals,NETR),
  write_info_log(['ET retrievals      : ',NETR]),
  my_fz(et_lookups,NET),
  write_info_log(['ET look-ups        : ',NET]),
  my_fz(ct_lookups,NCT),
  write_info_log(['CT look-ups        : ',NCT]),
  my_fz(cf_lookups,NCF),
  write_info_log(['CF look-ups        : ',NCF]),
  display_et_entries.

display_et_entries :-
  my_fz(et_entries,ETE),
  write_info_log(['ET entries         : ',ETE]),
  my_fz(ct_entries,CTE),
  write_info_log(['CT entries         : ',CTE]).

set_et_statistics :-
  my_statistics(off),
  !.
set_et_statistics :-
  findall(a,et(_,_,_,_,_),ET), 
  length(ET,ETS),
  set_flag(et_entries,ETS),
  findall(a,called(_,_,_),CT), 
  length(CT,CTS),
  set_flag(ct_entries,CTS).
  
reset_statistics :-
  my_statistics(off),
  !.
reset_statistics :-
  set_flag(fp_iterations(0)),
  set_flag(edb_retrievals,0),
  set_flag(idb_retrievals,0),
  set_flag(et_retrievals,0),
  set_flag(et_lookups,0),
  set_flag(ct_lookups,0),
  set_flag(cf_lookups,0).
  % ET and CT number of entries do not need to be reset
  
inc_statistics_flag(_Flag) :-
  my_statistics(off),
  !.
inc_statistics_flag(Flag) :-
  inc_flag(Flag,_Value).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Timing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_query_elapsed_time :-
  store_query_elapsed_time(display),
  get_query_elapsed_time(Parsing,Computation,Display,Total), 
  set_flag(last_query_elapsed_time(Parsing,Computation,Display,Total)), 
  timing(Switch),
  (Switch==detailed
   ->
    format_timing(Parsing,FParsing), 
    format_timing(Computation,FComputation), 
    format_timing(Display,FDisplay), 
    format_timing(Total,FTotal), 
    write_info_log(['Parsing elapsed time    : ',FParsing]),
    write_info_log(['Computation elapsed time: ',FComputation]),
    write_info_log(['Display elapsed time    : ',FDisplay]),
    write_info_log(['Total elapsed time      : ',FTotal])
   ;
    true),
  (Switch==on
   ->
    format_timing(Total,FTotal), 
    write_info_log(['Total elapsed time: ',FTotal])
   ;
    true).

% Format timing only if enabled
format_timing(T,FT) :-
  format_timing(off),
  !,
  atomic_concat_list([T,' ms.'],FT).
% Less than 1 second: ms
format_timing(T,FT) :-
  T<1000,
  !,
  atomic_concat_list([T,' ms.'],FT).
% Less than 60 seconds: s.ms
format_timing(T,FT) :-
  T<60000,
  !,
  S is floor(T/1000),
  MS is T rem 1000,
  pad_zeroes(S,2,PS),
  pad_zeroes(MS,3,PMS),
  atomic_concat_list([PS,'.',PMS,' s.'],FT).
% Less than 60 minutes: m:s.ms
format_timing(T,FT) :-
  T<3600000,
  !,
  M is floor(T/60000),
  S is floor((T-M*60000)/1000),
  MS is T rem 1000,
  pad_zeroes(M,2,PM),
  pad_zeroes(S,2,PS),
  pad_zeroes(MS,3,PMS),
  atomic_concat_list([PM,':',PS,'.',PMS],FT).
% More than 60 minutes: h:m:s.ms
format_timing(T,FT) :-
  H is floor(T/3600000),
  M is floor((T-H*3600000)/60000),
  S is floor((T-H*3600000-M*60000)/1000),
  MS is T rem 1000,
  pad_zeroes(M,2,PM),
  pad_zeroes(S,2,PS),
  pad_zeroes(MS,3,PMS),
  atomic_concat_list([H,':',PM,':',PS,'.',PMS],FT).
  
format_time(H,M,S,FT) :-
  pad_zeroes(H,2,PH),
  pad_zeroes(M,2,PM),
  pad_zeroes(S,2,PS),
  atomic_concat_list([PH,':',PM,':',PS],FT).

format_date(Y,M,D,FD) :-
%   pad_zeroes(Y,4,PY),
%   pad_zeroes(M,2,PM),
%   pad_zeroes(D,2,PD),
%   atomic_concat_list([PY,'-',PM,'-',PD],FD).
  date_to_atom(date(Y,M,D),FD).

% pad_zeroes(+Number,+Width,+PaddedNumber as an atom)
pad_zeroes(N,W,P) :-
  pad_atom_with(N,0,W,P).

pad_atom_with(T,C,W,PT) :- % Pad to the left by default
  pad_atom_with(T,C,W,left,PT).

% pad_atom_with(+Atomic,+PadCharacter,+Width,+Side \in {left,right},-PaddedAtomic as an atom)
% WARNING: Extend this to include not only a padding character, but any string
pad_atom_with(T,C,W,S,PT) :-
  name(T,Cs),
  length(Cs,N),
  N<W,
  !,
  atom_codes(AT,Cs),
  W1 is W-N,
  length(L,W1),
  my_map_1('='(C),L),
  atomic_concat_list(L,Zs),
  (S==right
   ->
    atom_concat(AT,Zs,PT)
   ;
    atom_concat(Zs,AT,PT)).
pad_atom_with(T,_C,_W,_S,T).
  

get_query_elapsed_time(Parsing,Computation,Display,Total) :-
  query_elapsed_time(Parsing,Computation,Display),
  Total is round(Parsing+Computation+Display+0.0).
  
store_query_elapsed_time(parsing) :-
  retract(query_elapsed_time(_,Computation,Display)),
  update_elapsed_time(Parsing),
  assertz(query_elapsed_time(Parsing,Computation,Display)).
store_query_elapsed_time(computation) :-
  retract(query_elapsed_time(Parsing,_,Display)),
  update_elapsed_time(Computation),
  assertz(query_elapsed_time(Parsing,Computation,Display)).
store_query_elapsed_time(display) :-
  retract(query_elapsed_time(Parsing,Computation,_)),
  update_elapsed_time(Display),
  assertz(query_elapsed_time(Parsing,Computation,Display)).
  
:-dynamic(command_instance_id/1).

current_command_instance_id(CId1) :-
  command_instance_id(CId),
  !,
  CId1 is CId+1,
  set_flag(command_instance_id(CId1)).
current_command_instance_id(0) :-
  set_flag(command_instance_id(0)).
  
store_command_elapsed_time(CId) :-
  current_command_instance_id(CId),
  get_command_elapsed_time(Time),
  assertz(command_elapsed_time(CId,Time)).
  
display_command_elapsed_time(_CId) :-
  tapi(on),
  !.
display_command_elapsed_time(CId) :-
%  retract(command_elapsed_time(CId,Time)),
  command_elapsed_time(CId,Time),
  my_get_time(WallTime),
  CTime is WallTime-Time,
  set_flag(last_command_elapsed_time(CTime)), 
  format_timing(CTime,FTime), 
  timing(Switch),
  (Switch==off
   ->
    true
   ;
    write_info_log(['Command elapsed time: ',FTime])
  ).

% Timeout in seconds
my_timeout(Goal,Time,Result) :-
  timeout(off),
  !,
  set_flag(timeout(on)),
  system_timeout(Goal,Time,Result),
  set_flag(timeout(off)).
my_timeout(Goal,_Time,success) :-
  call(Goal).
  
% Call a goal with a timeout
call_with_timeout(Goal,Timeout) :-
  call_with_timeout(Goal,Timeout,_Result).
  
call_with_timeout(Goal,Timeout,Result) :-
  my_timeout(Goal,Timeout,Result),
%  catch(my_timeout(Goal,Timeout,Result),_M,true),
  (Result==time_out
   -> 
    (output(on) -> write('\r') ; true), % If running info is enabled, overwrite the message
    write_info_log(['Timeout exceeded.          ']), 
    nl_compact_log,
    complete_pending_tasks('$aborted'), 
    set_flag(error(2)) 
   ;
    true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Date and time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Normalized date
% Without a base date to determine calendar type
my_normalized_date(date(UY,UM,UD),date(Y,M,D)) :-
  my_normalized_date(date(UY,UM,UD),date(UY,UM,UD),date(Y,M,D)).

% With a base date to determine calendar type
my_normalized_date(date(UY,UM,UD),date(BY,BM,BD),date(Y,M,D)) :-
  my_normalize_datetime(date(UY,UM,UD,0,0,0),date(BY,BM,BD),date(Y,M,D,0,0,0)),
  !.
% my_normalized_date(D,_,_) :-
%   my_raise_exception(generic,syntax(['Invalid date ',D,'. Dates start at date(1970,1,1) (Epoch) and end at date(3001,1,1)']),[]).
my_normalized_date(D,_,_) :-
  my_raise_exception(generic,syntax(['Invalid date ',D]),[]).

% Normalized time
% (No need for a base date)
my_normalized_time(time(UH,UM,US),time(H,M,S)) :-
  my_normalize_time(time(UH,UM,US),time(H,M,S)),
  !.
my_normalized_time(D,_) :-
  my_raise_exception(generic,syntax(['Invalid time ',D]),[]).

% Normalized datetime
% Without a base date to determine calendar type
my_normalized_datetime(datetime(UY,UM,UD,UH,UMi,US),datetime(Y,M,D,H,Mi,S)) :-
  my_normalized_datetime(datetime(UY,UM,UD,UH,UMi,US),date(UY,UM,UD),datetime(Y,M,D,H,Mi,S)).
  
% With a base date to determine calendar type
my_normalized_datetime(datetime(UY,UM,UD,UH,UMi,US),date(BY,BM,BD),datetime(Y,M,D,H,Mi,S)) :-
  my_normalize_datetime(date(UY,UM,UD,UH,UMi,US),date(BY,BM,BD),date(Y,M,D,H,Mi,S)),
  !.
my_normalized_datetime(D,_,_) :-
  my_raise_exception(generic,syntax(['Invalid datetime ',D,'. Dates start at date(1970,1,1) (Epoch) and end at date(3001,1,1)']),[]).

  
% my_normalize_datetime(date(+UY,+UM,+UD,+UH,+UMn,+US),date(+BY,+BM,+BD),date(?Y,?M,?D,?H,?Mn,?S))
% Takes a ground datetime structure date/6 with possibly non-normalized values 
% (i.e., out of bounds, including negative numbers) and returns the 
% normalized datetime structure (i.e., all values in bounds), w.r.t.
% a base date which is used to determine the input calendar type (Gregorian or Julian).
% (If a numerical internal representation for dates was used, this would not
%  be needed. The date structure is used instead for users to easily interpret dates.
%  No one does this way, I guess).
% The input and output datetimes are assumed to represent a calendar (as DB2),
% i.e., a Gregorian calendar since 15 October 1582, and a Julian calendar
% on or before 4 October 1582. Note that 11s day were omitted. 
% Internally, an astronomical Julian Date is used: integers representing
% days after the start of an epoch, and a fractional part for the time.
% Astronomical calendar integers refer to a day at noon (12:00:00)
% The current epoch (7980 years) spans from January 1, 4713 BC (Julian).
% Data structures include a year field including negative numbers and 0.
% So, 0 is year 1 BC, -1 is year 2 BC and so on.
% Notes: 
% | ?- calendar_to_astronomical_julian(1582,10,15,J).
% J = 2299161 ? 
% | ?- calendar_to_astronomical_julian(1582,10,4,J). 
% J = 2299160 ? 
% | ?- calendar_to_astronomical_julian(1,1,1,J).
% J = 1721424 ? 
% | ?- calendar_to_astronomical_julian(0,12,31,J).
% J = 1721423 ? 
my_normalize_datetime(date(UY,UM,UD,UH,UMn,US),date(Y,M,D,H,Mn,S)) :-
  my_normalize_datetime(date(UY,UM,UD,UH,UMn,US),date(UY,UM,UD),date(Y,M,D,H,Mn,S)).
  
my_normalize_datetime(date(UY,UM,UD,UH,UMn,US),date(BY,BM,BD),date(Y,M,D,H,Mn,S)) :-
  my_normalize_time(time(UH,UMn,US),time(H,Mn,S),ExcessDays),
  calendar_to_astronomical_julian(date(UY,UM,UD),date(BY,BM,BD),J),
  EJ is J+ExcessDays,  
  astronomical_julian_to_calendar(EJ,Y,M,D).
  
calendar_type(date(Y,_M,_D),gregorian) :-
  Y>1582,
  !.
calendar_type(date(1582,M,_D),gregorian) :-
  M>10,
  !.
calendar_type(date(1582,10,D),gregorian) :-
  D>=15,
  !.
calendar_type(date(_Y,_M,_D),julian).
  

% Given hours, minutes and seconds, return them with upper limits (23, 59 and 59, resp.) and the excess in days
% Ex: 24:00:00 -> 00:00:00 and 1 day in excess
% my_normalize_time(time(24,0,0),time(H,M,S),D) 
my_normalize_time(time(UH,UMn,US),time(H,Mn,S)) :-
  my_normalize_time(time(UH,UMn,US),time(H,Mn,S),_ExcessDays).
%   (ExcessDays\==0
%    -> 
%     write_warning_log(['Time overflow in: ',time(UH,UMn,US)])
%    ;
%     true).
  
my_normalize_time(time(UH,UMn,US),time(H,Mn,S),ExcessDays) :-
  IS is integer(US),
  DS is US-IS,
  S is DS + IS mod 60,
  Mn is (UMn+(IS div 60)) mod 60,
  H is (UH+((UMn+(IS div 60)) div 60)) mod 24,
  ExcessDays is (UH+((UMn+(IS div 60)) div 60)) div 24.

% calendar_to_astronomical_julian(+Date,+BaseDate,?J)
% Converts a date w.r.t. to a base date (used to determine the type of calendar)
% to an Astronomical Julian Date J.
calendar_to_astronomical_julian(date(Y,M,D),date(BY,BM,BD),J) :-
  valid_base_date(date(BY,BM,BD)),
  calendar_type(date(BY,BM,BD),CalendarType),
  calendar_to_astronomical_julian(date(Y,M,D),CalendarType,J).
% Gregorian:
calendar_to_astronomical_julian(date(Y,M,D),gregorian,J) :-
  J is ((1461 * (Y + 4800 + ((M - 14) // 12))) // 4) +
        ((367 * (M - 2 - 12 * ((M - 14) // 12))) // 12) -
        ((3 * ((Y + 4900 + ((M - 14) // 12)) // 100)) // 4) +
        D - 32075.
% Julian:
calendar_to_astronomical_julian(date(Y,M,D),julian,J) :-
  J is 367 * Y - ((7 * (Y + 5001 + ((M - 9)//7)))//4) + ((275 * M)//9) + D + 1729777.

valid_base_date(date(1582,10,D)) :-
  D>4, 
  D<15,
  !,
  my_raise_exception(generic,syntax(['Invalid base date: ','$exec'((date_to_atom(date(1582,10,D),A),write_log(A))),'. Dates between 1582/10/5 and 1582-10-14 are not valid.']),[]).
valid_base_date(date(Y,M,D)) :-
  Y < -4712,
  !,
  my_raise_exception(generic,syntax(['Invalid base date: ','$exec'((date_to_atom(date(Y,M,D),A),write_log(A))),'. Dates before BC 4713 are not valid.']),[]).
valid_base_date(date(_Y,_M,_D)).  
  
% Astronomical Julian Date to calendar date (depending on the year, either Gregorian or Julian)
astronomical_julian_to_calendar(J,Y,M,D) :-
  (J >= 2299161 % After or on A.D. 1582 Oct 15 (In fact, 5-14 didn't exist in several countries, but we accept them as Julian)
   -> % Gregorian
    F is J+1401+((((4*J+274277)//146097)*3)//4)-38
  ;   % Julian (don't miss target Julian date with Astronomical Julian) before or on A.D. 1582 Oct 4
    F is J+1401
  ),
  E is 4*F+3,
  G is (E mod 1461)//4,
  H is 5*G+2,
  D is ((H mod 153)//5)+1,
  M is (((H//153)+2) mod 12)+1,
  Y is (E//1461)-4716+((14-M)//12).

% Date/Time operations
  
% Adding days to a date
datetime_add(date(Y,M,D),I,date(Y1,M1,D1)) :-
  DI is D+I,
  my_normalized_date(date(Y,M,DI),date(Y,M,D),date(Y1,M1,D1)).
% Adding seconds to a time
datetime_add(time(H,Mi,S),I,time(H1,Mi1,S1)) :-
  SI is S+I,
  my_normalized_time(time(H,Mi,SI),time(H1,Mi1,S1)).
% Adding seconds to a datetime
datetime_add(datetime(Y,M,D,H,Mi,S),I,datetime(Y1,M1,D1,H1,Mi1,S1)) :-
  SI is S+I,
  my_normalized_datetime(datetime(Y,M,D,H,Mi,SI),date(Y,M,D),datetime(Y1,M1,D1,H1,Mi1,S1)).

datetime_sub(date(Y1,M1,D1),date(Y2,M2,D2),Field,Result) :-
  my_date_sub((Y1,M1,D1),(Y2,M2,D2),Field,Result).
datetime_sub(time(H1,Mi1,S1),time(H2,Mi2,S2),Field,Result) :-
  my_time_sub((H1,Mi1,S1),(H2,Mi2,S2),Field,Result).
datetime_sub(datetime(Y1,M1,D1,H1,Mi1,S1),datetime(Y2,M2,D2,H2,Mi2,S2),Field,Result) :-
  my_datetime_sub((Y1,M1,D1,H1,Mi1,S1),(Y2,M2,D2,H2,Mi2,S2),Field,Result).
  
% Number of days between two dates
my_date_sub((Y1,M1,D1),(Y2,M2,D2),day,Days) :-
  my_datetime_sub((Y1,M1,D1,0,0,0),(Y2,M2,D2,0,0,0),day,Days).

% Number of seconds between two times
my_time_sub((H1,Mn1,S1),(H2,Mn2,S2),second,Seconds) :-
  my_datetime_sub((2000,1,1,H1,Mn1,S1),(2000,1,1,H2,Mn2,S2),second,Seconds).
   
% Number of time units (expressed with Field \in \{day,second\}) between two datetimes
my_datetime_sub((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),Field,Result) :-
  (Y1,M1,D1,H1,Mn1,S1)@>=(Y2,M2,D2,H2,Mn2,S2),
  !,
  my_ordered_datetime_sub((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),Field,Result).
my_datetime_sub((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),Field,Result) :-
  !,
  my_ordered_datetime_sub((Y2,M2,D2,H2,Mn2,S2),(Y1,M1,D1,H1,Mn1,S1),Field,PResult),
  Result is -(PResult).
  
% The first datetime is greater than the second one
my_ordered_datetime_sub((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),second,Seconds) :-
  my_ordered_datetime_sub_seconds((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),Seconds).
my_ordered_datetime_sub((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),day,Days) :-
  my_ordered_datetime_sub_seconds((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),Seconds),
  Days is truncate(Seconds/3600/24).

my_ordered_datetime_sub_seconds((Y1,M1,D1,H1,Mn1,S1),(Y2,M2,D2,H2,Mn2,S2),Seconds) :-
  calendar_to_astronomical_julian(date(Y1,M1,D1),date(Y1,M1,D1),J1),
  calendar_to_astronomical_julian(date(Y2,M2,D2),date(Y2,M2,D2),J2),
  DH is H1-H2,
  DMn is Mn1-Mn2,
  DS is S1-S2,
  my_normalize_time(time(DH,DMn,DS),time(H,Mn,S),ExcessDays),
  Days is J1-J2+ExcessDays,
  Seconds is Days*86400+H*3600+Mn*60+S.
  
last_day_of_month(date(Y,M,_D),LD) :-
  MI is M+1,
  my_normalized_date(date(Y,MI,1),date(Y1,M1,D1)),
  datetime_add(date(Y1,M1,D1),-1,date(_,_,LD)).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellanea
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Maximum of two numbers
my_max(A,B,A) :-
  A>=B,
  !.
my_max(_A,B,B).

% Minimum of two numbers
% my_min(A,B,A) :-
%   A=<B,
%   !.
% my_min(_A,B,B).

% Maximum in a list
my_list_max([X|Xs],M) :-
 my_list_max(Xs,X,M).

my_list_max([],M,M).
my_list_max([X|Xs],M,Max) :- 
  (X @> M ->
   my_list_max(Xs,X,Max)
   ;
   my_list_max(Xs,M,Max)
  ).

% from
from(N,M,[]) :-
  N>M,
  !.
from(N,M,[N|Ns]) :-
  N1 is N+1,
  from(N1,M,Ns).
  
% Display a list of datalog rules and its number
display_tuples_and_nbr_info(SDLs,ODLs) :-
  exec_if_verbose_on(
    (development(on) -> 
      length(ODLs,Nbr)
      ; 
      length(SDLs,Nbr)),
    (Nbr==1 -> S =' ' ; S='s '),
    (Nbr==0 -> D = '.' ; D = (':')),
    write_info_log(['',Nbr,' rule',S,'retracted',D]), 
    (development(on) -> 
      display_dlrule_list(ODLs,2)
      ; 
      display_source_dlrule_list(SDLs,2))).


% my_get0_echo(Char) :-
%   my_get0(Char),
%   (batch(_,_,_) -> 
%     atom_codes(A,[Char]),
%     write_log(A)
%    ;
%     true
%   ).

% complete_anonymous_variables(Variables,NAVariableNames,VariableNames)
complete_anonymous_variables([],_,[]).
complete_anonymous_variables([V|Vs],NANVs,[N=V|NVs]) :-
  find_var_name(V,N,NANVs),
  !,
  complete_anonymous_variables(Vs,NANVs,NVs).
complete_anonymous_variables([V|Vs],NANVs,['_'=V|NVs]) :-
  complete_anonymous_variables(Vs,NANVs,NVs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aggregates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
% ord_aggregate([t(a,b,1),t(a,b,2),t(c,d,3)],t(X,Y,Z),[X,Y],max(Z),[t(a,b,2),t(c,d,3)])
% ord_aggregate(Tuples,Schema,GroupBy,Function,AggTuples)
ord_aggregate([],_Schema,_GroupBy,_Function,[]).
ord_aggregate([Tuple|MoreTuples],Schema,GroupBy,Function,AggTuples) :-
  Function=..[FunctionName,Var],
  term_arg_position(Schema,Var,Pos),
  term_args_positions(Schema,GroupBy,GBPos),
  length(GBPos,LGBPos),
  functor(Tuple,_,Arity),
  CGBLength is Arity-LGBPos,
  list_between(1,Arity,Numbers),
  my_set_diff(Numbers,GBPos,CGBPos),
  ord_aggregate_aux([Tuple|MoreTuples],Schema,CGBPos,CGBLength,FunctionName,Pos,AggTuples).

ord_aggregate_aux([],_Schema,_CGBPos,_CGBLength,_FunctionName,_Pos,[]).
ord_aggregate_aux([Tuple|MoreTuples],Schema,CGBPos,CGBLength,FunctionName,Pos,[AggTuple|AggTuples]) :-
  copy_term(Schema,Schema1),
  Schema1=Tuple,
  length(Vs,CGBLength),
  replace_ith_args_term(Schema1,CGBPos,Vs,RSchema1),
  arg(Pos,Tuple,Value),
  solve_ord_aggregate(MoreTuples,RemTuples,RSchema1,FunctionName,Pos,Value,AggTuple),
  ord_aggregate_aux(RemTuples,Schema,CGBPos,CGBLength,FunctionName,Pos,AggTuples).

% solve_ord_aggregate([t(a,b,2),t(c,d,3)],RemTuples,t(a,b,_),max,3,1,AggTuple)
solve_ord_aggregate([Tuple|Tuples],RemTuples,Schema,FunctionName,Pos,Value,AggTuple) :-
  \+ \+ Tuple=Schema,
  !,
  arg(Pos,Tuple,CurrValue),
  solve_aggregate_function(FunctionName,Value,CurrValue,NewValue),
  solve_ord_aggregate(Tuples,RemTuples,Schema,FunctionName,Pos,NewValue,AggTuple).
solve_ord_aggregate(Tuples,Tuples,Schema,_FunctionName,Pos,Value,Schema) :-
  arg(Pos,Schema,Value).
  
solve_aggregate_function(max,V1,V2,V1) :-
  V1@>=V2,
  !.
solve_aggregate_function(max,_V1,V2,V2) :-
  !.
solve_aggregate_function(min,V1,V2,V1) :-
  V1@=<V2,
  !.
solve_aggregate_function(min,_V1,V2,V2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Inserting
insert_into_last_but_one_pos([L],X,[X,L]).
insert_into_last_but_one_pos([L1,L2|Ls],X,[L1|RLs]) :-
  insert_into_last_but_one_pos([L2|Ls],X,RLs).

% Removing one element from a list
remove_one_element_from_list(X,[X|Xs],Xs).
remove_one_element_from_list(X,[Y|Xs],[Y|Ys]) :-
  remove_one_element_from_list(X,Xs,Ys).

remove_one_element_if_exists_from_list(E,L,RL) :-
  (remove_one_element_from_list(E,L,RL)
     ->
      true
     ;
      RL=L
    ).
    
% Remove from list
remove_from_list(_X,[],[]).
remove_from_list(X,[Y|Ys],Zs) :-
  \+ \+ X=Y,
  !,
  remove_from_list(X,Ys,Zs).
remove_from_list(X,[Y|Ys],[Y|Zs]) :-
  remove_from_list(X,Ys,Zs).

% Remove var from list, failing if not found
remove_one_var_f(X,[Y|Ys],Ys) :-
  X==Y,
  !.
remove_one_var_f(X,[Y|Ys],[Y|Zs]) :-
  remove_one_var_f(X,Ys,Zs).
  
% Remove var from list, succeed even if not found
remove_one_var(X,Ys,Zs) :-
  remove_one_var_f(X,Ys,Zs),
  !.
remove_one_var(_X,Ys,Ys).

remove_one_var_list([],Ys,Ys).
remove_one_var_list([X|Xs],Ys,Zs) :-
  remove_one_var(X,Ys,Us),
  remove_one_var_list(Xs,Us,Zs).

remove_vars_not_in([],_Ys,[]).
remove_vars_not_in([X|Xs],Ys,Zs) :-
  var(X),
  \+ my_member_var(X,Ys),
  !,
  remove_vars_not_in(Xs,Ys,Zs).
remove_vars_not_in([X|Xs],Ys,[X|Zs]) :-
  remove_vars_not_in(Xs,Ys,Zs).

% Remove nth element N from a list, where 1 is the first element
% Fail if N is out of bounds
remove_nth(N,Xs,Ys) :-
  remove_nth(N,1,Xs,Ys).
  
remove_nth(N,N,[_X|Xs],Xs).
remove_nth(N,I,[X|Xs],[X|Ys]) :-
  I1 is I+1,
  remove_nth(N,I1,Xs,Ys).
  
  
% Check whether its input argument is a list
% my_is_list(+L)
my_is_list([]).
my_is_list([_X|Xs]) :-
  my_is_list(Xs).

my_list_to_list_of_lists([],[]).
my_list_to_list_of_lists([A|As],[[A]|Bs]) :-
  my_list_to_list_of_lists(As,Bs).

% Append one list
append_one_list(As,[],As).
append_one_list([A|As],[A],As).
  

  
yfx_connect_with([X],_,X).
yfx_connect_with([X1,X2|Xs],C,CX1X2Xs) :-
  CX1X2=..[C,X1,X2],
  yfx_connect_with([CX1X2|Xs],C,CX1X2Xs).

xfy_connect_with([X],_,X).
xfy_connect_with([X1,X2|Xs],C,CX1Xs) :-
  CX1Xs=..[C,X1,CX2Xs],
  xfy_connect_with([X2|Xs],C,CX2Xs).
  
concat_strs_with(Strs,C,Str) :-
  concat_strs_with(Strs,C,Str,"").
  
concat_strs_with([],_C) -->
  [].
concat_strs_with([X],_C) -->
  X.
concat_strs_with([X1,X2|Xs],C) -->
  X1,
  C,
  concat_strs_with([X2|Xs],C).
  
memberchk_list([X|_Xs],Ys,X) :-
  memberchk(X,Ys),
  !.
memberchk_list([_|Xs],Ys,X) :-
  memberchk_list(Xs,Ys,X).
  
remove_delimiting_chars(X,C,Y) :-
  atom_chars(X,[C|Xs]),
  append(Ys,[C],Xs),
  atom_chars(Y,Ys),
  !.
remove_delimiting_chars(X,_C,X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logical
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prolog implementation of negation
% my_not(G) :- 
%   call(G), 
%   !, 
%   fail.
% my_not(_G).

% Logical disjunction
my_or(true,true,true).
my_or(true,false,true).
my_or(false,true,true).
my_or(false,false,false).

% Uncertainty disjunction
my_u_or(L,R,true) :-
  ((var(L),R==true);(L==true,var(R))),
  !.
my_u_or(L,R,_O) :-
  (var(L);var(R)),
  !.
my_u_or(L,R,O) :-
  my_or(L,R,O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Atoms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atom_concat_list([A],A).
atom_concat_list([A,B|As],C) :-
  atom_concat(A,B,D),
  atom_concat_list([D|As],C).

positive_atom(-(A),A) :-
  !.  
positive_atom(A,A).  


is_negative_rule(':-'(-(_H),_)) :-
  !.
is_negative_rule(-(_H)).

is_negative_name(-(_N)).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rule Conversion and Info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% datalog(...) to (R,NVs), where either R=H:-B or R=H
dlrule_to_ruleNVs_list([],[]).
dlrule_to_ruleNVs_list([DR|DRs],[RNVs|RNVss]) :-
  dlrule_to_ruleNVs(DR,RNVs),
  dlrule_to_ruleNVs_list(DRs,RNVss).

dlrule_to_ruleNVs(datalog(R,NVs,_,_,_,_,_),(R,NVs)).

% datalog(...) to R, where either R=H:-B or R=H
dlrule_to_rule_list([],[]).
dlrule_to_rule_list([DR|DRs],[R|Rs]) :-
  dlrule_to_rule(DR,R),
  dlrule_to_rule_list(DRs,Rs).

dlrule_to_rule(datalog(R,_,_,_,_,_,_),R).
  
% datalog(...) to (R,NVs), where either R=H:-B or R=H
source_dlrule_to_ruleNVs_list([],[]).
% Source rules:
source_dlrule_to_ruleNVs_list([datalog(R,NVs,_,_,_,_,source)|DRs],[(R,NVs)|RNVs]) :-
  !,
  source_dlrule_to_ruleNVs_list(DRs,RNVs).
% Simplified or safed rules (no other rules are part of this compilation)
source_dlrule_to_ruleNVs_list([datalog(R,NVs,_,_,_,_,compilation(_SR,_SNVs,[]))|DRs],[(R,NVs)|RNVs]) :-
  system_mode(des),
  !,
  source_dlrule_to_ruleNVs_list(DRs,RNVs).
% Fuzzy rules are always transformed from its source form
source_dlrule_to_ruleNVs_list([datalog(_R,_NVs,_,_,_,_,compilation(SR,SNVs,[]))|DRs],[(SR,SNVs)|RNVs]) :-
  !,
  source_dlrule_to_ruleNVs_list(DRs,RNVs).
% Compilation root with other compiled rules for this root
%source_dlrule_to_ruleNVs_list([datalog(_R,_RNVs,_,_,_,_,compilation(':-'(SH,SB),NVs,_RIds))|DRs],[(':-'(SH,SB),NVs)|RNVs]) :-
source_dlrule_to_ruleNVs_list([datalog(_R,_RNVs,_,_,_,_,compilation(SR,NVs,_RIds))|DRs],[(SR,NVs)|RNVs]) :-
  source_dlrule_to_ruleNVs_list(DRs,RNVs).

% R to (R,NVs), where either R=H:-B or R=H
rule_to_ruleNVs_list(Rs,RNVss) :-
  rule_to_ruleNVs_list(Rs,[],RNVss).

rule_to_ruleNVs_list([],_,[]).
rule_to_ruleNVs_list([R|Rs],NVs,[RNVs|RNVss]) :-
  rule_to_ruleNVs(R,NVs,RNVs),
  rule_to_ruleNVs_list(Rs,NVs,RNVss).

rule_to_ruleNVs(R,NVs,(R,RNVs)) :-
  assign_variable_names(R,NVs,RNVs).
  
% (R,NVs) to R, where either R=H:-B or R=H
ruleNVs_to_rule_list([],[]).
ruleNVs_to_rule_list([RNVs|RNVss],[R|Rs]) :-
  ruleNVs_to_rule(RNVs,R),
  ruleNVs_to_rule_list(RNVss,Rs).
  
ruleNVs_to_rule((R,_NVs),R).
  
ruleNVs_to_rule_NVs_list([],[],[]).
ruleNVs_to_rule_NVs_list([(R,NVs)|RNVss],[R|Rs],[NVs|NVss]) :-
  ruleNVs_to_rule_NVs_list(RNVss,Rs,NVss).
  
rule_NVs_to_ruleNVs_list(SRRs,NVss,RRNVs) :-
  my_zipWith(',',SRRs,NVss,RRNVs).

% ruleNVs_to_rule(RNVs,R) :-
%   ruleNVs_to_rule_list(RNVs,[R]).
  
% Predicate - Rule
pred_rule(Pred,':-'(Head,_Body)) :-
  pred_head(Pred,Head),
  !.
pred_rule(Pred,Head) :-
  pred_head(Pred,Head).

% pred_head(Name/Arity,@(Head,_Degree)) :-
%   !,
%   pred_head(Name/Arity,Head).
pred_head(Name/Arity,-(Head)) :-
  !,
  functor(Head,Name,Arity).
pred_head(Name/Arity,Head) :-
  functor(Head,Name,Arity).
 
univ_head(Name,Args,'-',-(Head)) :-
  Head=..[Name|Args],
  !.
univ_head(Name,Args,'+',Head) :-
  Head=..[Name|Args].
 
pred_rule_list([],[]).
pred_rule_list([P|Ps],[R|Rs]) :-
  pred_rule(P,R),
  pred_rule_list(Ps,Rs).

% Rule - Predicate
  
rule_pred(':-'(H,_),Pred) :-
  !,
  rule_pred(H,Pred).
rule_pred(-(H),Pred) :-
  !,
  rule_pred(H,Pred).
rule_pred('@'(H,_D),N/A) :-
  !,
  functor(H,N,A).
rule_pred(H,N/A) :-
  functor(H,N,A).
  
rule_pred_list([],[]).  
rule_pred_list([R|Rs],[P|Ps]) :-
  rule_pred(R,P),
  rule_pred_list(Rs,Ps).
  
ruleNVs_pred((R,_),Pred) :-
  rule_pred(R,Pred).

% Predicate - DLRule
pred_dlrule(Pred,datalog(Rule,_NVs,_RuleId,_CId,_Lines,_FileId,_Source)) :-
  pred_rule(Pred,Rule).
  
% DL rule is a fact
dlrule_is_fact(datalog(Rule,_NVs,_RuleId,_CId,_Lines,_FileId,_Source)) :-
  \+ rule_is_fact(Rule).

% DL rule is a positive fact
dlrule_is_positive_fact(datalog(Rule,_NVs,_RuleId,_CId,_Lines,_FileId,_Source)) :-
  rule_is_positive_fact(Rule).

% Rule is a fact
rule_is_fact(Rule) :-
  \+ Rule =.. [':-'|_].

% DL rule is a positive fact
rule_is_positive_fact(Rule) :-
  \+ Rule =.. [':-'|_],
  Rule \= -(_).
  
% DL Rule identifier
dlrule_id(datalog(_Rule,_NVs,RuleId,_CId,_Lines,_FileId,_Source),RuleId).

dlrule_id_list([],[]).
dlrule_id_list([DL|DLs],[Id|Ids]) :-
  dlrule_id(DL,Id),
  dlrule_id_list(DLs,Ids).

% DL Rule context identifier
dlrule_cid(datalog(_Rule,_NVs,_RuleId,CId,_Lines,_FileId,_Source),CId) :-
  nonvar(CId),
  !.
dlrule_cid(datalog(_Rule,_NVs,_RuleId,_CId,_Lines,_FileId,_Source),[]).

% DL Rule NVs
dlrule_NVs(datalog(_Rule,NVs,_RuleId,_CId,_Lines,_FileId,_Source),NVs).

% Void DL rule
void_dlrule(datalog(_Rule,_NVs,-1,_CId,_Lines,_FileId,_Source)).

replace_functor_DLs_by_RId_list(_T,_NT,[]).
replace_functor_DLs_by_RId_list(T,NT,[RId|RIds]) :-
  retract(datalog(Rule,NVs,RId,CId,Lines,FileId,Source)),
  replace_functor(T,NT,Rule,NRule),
  assertz(datalog(NRule,NVs,RId,CId,Lines,FileId,Source)),
  replace_functor_DLs_by_RId_list(T,NT,RIds).


get_rule_table_name_arity(':-'(-(H),B),Tablename,Arity) :-
  !,
  get_rule_table_name_arity(':-'(H,B),Tablename,Arity).
get_rule_table_name_arity(':-'(H,_B),Tablename,Arity) :-
  !,
  functor(H,Tablename,Arity).
get_rule_table_name_arity(-(H),Tablename,Arity) :-
  !,
  get_rule_table_name_arity(H,Tablename,Arity).
get_rule_table_name_arity(H,Tablename,Arity) :-
  functor(H,Tablename,Arity).


positive_name(-(N),N) :- 
  !.
positive_name(N,N).


'NVs_relevant_vars'(IVs,NVs,RVs) :-
  my_unzip(NVs,Ns,Vs),
  'NVs_relevant_vars'(IVs,Ns,Vs,RVs).


'NVs_relevant_vars'([],_Ns,_Vs,[]).
'NVs_relevant_vars'([IV|IVs],Ns,Vs,RVs) :-
  my_nth1_member_var(IV,I,Vs),
  my_nth1_member(N,I,Ns),
  atom_concat('_',_,N),
  !,
  'NVs_relevant_vars'(IVs,Ns,Vs,RVs).
'NVs_relevant_vars'([IV|IVs],Ns,Vs,[IV|RVs]) :-
  'NVs_relevant_vars'(IVs,Ns,Vs,RVs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metapredicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Get name and arity of a query
query_predicate(Query,N/A) :-
  (Query=not(Q)
  ;
   Query=(_L=>Q)
  ;
   Query=Q
  ),
  !,
  functor(Q,N,A).

% my_fz: If fails, then 0
my_fz(Pred,Val) :-
  Goal =.. [Pred,Val],
  call(Goal),
  !.
my_fz(_Pred,0).
  
% is_system_identifier
is_system_identifier(T) :-
  atomic(T),
  atomic_concat('$',_,T).
% is_system_identifier(T) :-
%   atomic(T),
%   "$"=[D],
%   atom_codes(T,[D|_]).

% Call list
call_list([]).
call_list([X|Xs]) :-
  call(X),
  call_list(Xs).
  
% Call list. No fail
call_nf_list([]).
call_nf_list([X|Xs]) :-
  call(X),
  !,
  call_nf_list(Xs).
call_nf_list([_X|Xs]) :-
  call_nf_list(Xs).

% Map to a list of argument lists
my_map(_X,[]).
my_map(X,[L|Ls]) :-
  L = [_H|_T],
  !,
  T =.. [X|L],
  call(T),
  my_map(X,Ls).
my_map(X,[Y|Ys]) :-
  !,
  T =.. [X,Y],
  call(T),
  my_map(X,Ys).

% id(X) :-
%   X.
%   
% nf_id(X) :-
%   id(X),
%   !.
% nf_id(_X).
  
% Map to exactly one argument (that can be a list)
my_map_1(_X,[]).
my_map_1(X,[Y|Ys]) :-
  my_apply(X,Y),
  my_map_1(X,Ys).

my_apply(my_apply(X,Y),Z) :-
  !,
  my_add_tup_arg(X,Y,T),
  my_apply(T,Z).
my_apply(X,Y) :-
  my_add_tup_arg(X,Y,T),
  call(T).

my_add_tup_arg(X,Y,T) :-
  X=..LX,
  append(LX,[Y],Ts),
  T=..Ts.
  
my_add_tup_arg_list(_X,[],[]).
my_add_tup_arg_list(X,[Y|Ys],[T|Ts]) :-
  my_add_tup_arg(X,Y,T),
  my_add_tup_arg_list(X,Ys,Ts).

    
% zipWith
% +Operator/Predicate +List(LeftOp) +List(RightOp) +List(Operator(LeftOp,RightOp))
my_zipWith(_Z,[],_Bs,[]).
my_zipWith(_Z,[_A|_As],[],[]).
my_zipWith(Z,[A|As],[B|Bs],[P|Ps]) :-
  P=..[Z,A,B],
  my_zipWith(Z,As,Bs,Ps).
   
% unzip
% +List(Operator(LeftOp,RightOp)) +List(LeftOp) +List(RightOp)
my_unzip([],[],[]).
my_unzip([P|Ps],[A|As],[B|Bs]) :-
  P=..[_Z,A,B],
  my_unzip(Ps,As,Bs).
  
my_unzip_list([],[],[]).
my_unzip_list([A|As],[B|Bs],[C|Cs]) :-
  my_unzip(A,B,C),
  my_unzip_list(As,Bs,Cs).
   
% L1=[1,2,3]
% L2=[a,b,c]
% L=[1,a,2,b,3,c]
merge_zip([],Ys,Ys) :-
  !.
merge_zip(Xs,[],Xs) :-
  !.
merge_zip([X|Xs],[Y|Ys],[X,Y|XYs]) :-
  merge_zip(Xs,Ys,XYs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tuple_append(+Tuple1,+Tuple2,-Tuple) Appends the two input
%   tuples, returning a concatenated tuple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tuple_append(('.'), A, A) :- !.
tuple_append(A, ('.'), A) :- !.
tuple_append((A,B), C, (A,D)) :-
  !, 
  tuple_append(B, C, D).
tuple_append(A, B, (A,B)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% append_goals(+Goals1,+Goals2,-Goals) Appends the two input
%   goals, returning a concatenated goal and excluding
%   true goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append_goals(true, (A,B), C) :-
  !,
  append_goals(A,B,C).
append_goals((A,B), true, C) :-
  !,
  append_goals(A,B,C).
append_goals(true, true, true) :-
  !.
append_goals(true, A, A) :-
  !.
append_goals(A,true, A) :-
  !.
append_goals((A,B), C, E) :-
  !, 
  append_goals(B, C, D),
  append_goals(A, D, E).
append_goals(A, (B,C), (A,D)) :-
  !,
  append_goals(B, C, D).
append_goals(A, B, (A,B)).

append_goals_list([A],A).
append_goals_list([A,B|Gs],G) :-
  append_goals(A,B,C),
  append_goals_list([C|Gs],G).
  
% my_univ_list
% +functor +List -List
my_univ_list(_F,[],[]).
my_univ_list(F,[A|As],[B|Bs]) :-
  B=..[F|A],
  my_univ_list(F,As,Bs).
   
copy_term_list([],[]).
copy_term_list([T|Ts],[CT|CTs]) :-
  copy_term(T,CT),
  copy_term_list(Ts,CTs).

% Testing whether the input list contains variables
%vars([]).
%vars([V|Vs]) :- 
%  var(V), 
%  vars(Vs).

% Return always an atom, just in case its input is a number
ensure_atom(N,A) :-
  (number(N) -> number_codes(N,CL), atom_codes(A,CL); N=A).

% Return a float or variable
ensure_float(N,F) :-
  var(N),
  !,
  F=N.
ensure_float(N,F) :-
  float(N),
  !,
  F=N.
ensure_float(N,F) :-
  F is float(N).

% Findall
my_nf_bagof(X,G,Xs) :-
  (bagof(X,G,Xs) -> true ; Xs=[]).

% Non-failing setof: Return empty list if setof fails
my_nf_setof(X,G,Xs) :-
  (setof(X,G,Xs) -> true ; Xs=[]).

% Unifiable: Test whether two terms are unifiable
my_unifiable(X,Y) :-
  \+ \+ X=Y.

% Copy term for lists
% copy_term_list([],[]).
% copy_term_list([T|Ts],[CT|CTs]) :-
%   copy_term(T,CT),
%   copy_term_list(Ts,CTs).

% Literals
my_literal(L) :-
  nonvar(L),
  L=..[_F|Args],
  my_atom_list(Args).
  
my_atom_list([]).  
my_atom_list([A|As]) :-
  (var(A);atomic(A)),
  !,
  my_atom_list(As).  
  
  
my_atom(A) :-
  atom(A).  
my_atom(T) :-
  T =.. [F|Args],
  length(Args,L),
  not_builtin(F/L),
  atom(F),
  my_noncompound_terms(Args).

not_builtin(F/L) :-
  F/L \== lj/3,
  F/L \== rj/3,
  F/L \== fj/3,
  F/L \== st/1,
  F/L \== call/1,
%   F/L \== lj/1,
%   F/L \== rj/1,
%   F/L \== fj/1,
%  F/L \== '$diff'/1,
  F/L \== (not)/1,
  F/L \== top/2,
  F/L \== distinct/1,
  F/L \== distinct/2,
  F/L \== order_by/3,
  F/L \== group_by/3,
  F/L \== group_by/4,
  F/L \== avg/3,
  F/L \== avg_distinct/3,
  F/L \== count/3,
  F/L \== count/2,
  F/L \== count_distinct/3,
  F/L \== count_distinct/2,
  F/L \== max/3,
  F/L \== min/3,
  F/L \== sum/3,
  F/L \== sum_distinct/3,
  F/L \== times/3,
  F/L \== times_distinct/3,
  F/L \== (',')/2,
  F/L \== (';')/2.
  
my_noncompound_terms([]).
my_noncompound_terms([Term|Terms]) :-
  my_noncompound_term(Term),
  my_noncompound_terms(Terms).

my_noncompound_term(T) :-
  atomic(T),
  !.
my_noncompound_term(T) :-
  var(T),
  !.
my_noncompound_term('$NULL'(_ID)) :-
  !.
my_noncompound_term(date(_,_,_)) :-
  !.
my_noncompound_term(time(_,_,_)) :-
  !.
my_noncompound_term(datetime(_,_,_,_,_,_)) :-
  !.
    
my_var_or_constant(V) :-
  var(V),
  !.
my_var_or_constant(C) :-
  my_constant(C).

my_constant(V) :-
  var(V),
  !,
  fail.
my_constant('$NULL'(_ID)) :-
  !.
my_constant(N) :-
  number(N),
  !.
my_constant(date(_,_,_)) :-
  !.
my_constant(time(_,_,_)) :-
  !.
my_constant(datetime(_,_,_,_,_,_)) :-
  !.
my_constant(C) :-
  atom(C),
  functor(C,F,0),
  \+ my_aggregate_function(F,0).

my_compound_term(T) :-
  \+ my_noncompound_term(T).
  
%call_list([]).
%call_list([H|T]) :-
%  call(H),
%  call_list(T).

my_n_repeat(0,_Goal) :-
  !.
my_n_repeat(N,Goal) :-
  call(Goal),
  N1 is N-1,
  my_n_repeat(N1,Goal).

  
display_string_list_sql_on(_Strings) :-
  show_sql(off),
  !.  
display_string_list_sql_on(Strings) :-
  write_string_info_log_list(Strings).
 
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DWIM for help on identifiers
%
% Show alternatives (dwim) which are spelled similar to the given one, i.e.:
% - Equal up to case
% - Equal up to one char (case insensitive) for at least 4 characters
% - Equal but one char (one of the characters is missing, case insensitive) for at least 4 characters

display_user_predicate_alternatives(_Name) :-
  tapi(on),
  parsing_only(off),
  write_tapi_eot,
  !.
display_user_predicate_alternatives(Name) :-
  display_object_alternatives(user_predicate,Name).
  
display_predicate_alternatives(_Name) :-
  tapi(on),
  parsing_only(off),
  write_tapi_eot,
  !.
display_predicate_alternatives(Name) :-
  display_object_alternatives(predicate,Name).
  
display_relation_alternatives(_Name) :-
  tapi(on),
  parsing_only(off),
  write_tapi_eot,
  !.
display_relation_alternatives(Name/Arity) :-
  !,
  display_object_alternatives(relation,Name/Arity).
display_relation_alternatives(Name) :-
  display_object_alternatives(relation,Name).
  
display_table_alternatives(_Name) :-
  tapi(on),
  parsing_only(off),
  write_tapi_eot,
  !.
display_table_alternatives(Name) :-
  display_object_alternatives(table,Name).
  
display_view_alternatives(_Name) :-
  tapi(on),
  parsing_only(off),
  write_tapi_eot,
  !.
display_view_alternatives(Name) :-
  display_object_alternatives(view,Name).
  
display_column_alternatives(_TableName,_Name) :-
  tapi(on),
  parsing_only(off),
  write_tapi_eot,
  !.
display_column_alternatives(TableName,Name) :-
  display_object_alternatives(column(TableName),Name).
  
display_object_alternatives(_Object,_T) :-
  tapi(on),
  parsing_only(off),
  !.
display_object_alternatives(Object,O) :-
  once((O=T/X ; O=T)),
  atom_length(T,L),
  to_uppercase(T,UT),
  findall([T1,X],
        (possible_name(Object,T1,X),
         to_uppercase(T1,UT1),
         (
          % Equal up to case
          UT1==UT
         ;
          L>3,
          equal_up_to_one_char(UT,UT1)
         ;
          L>3,
          equal_but_one_char(UT,UT1)
         ;
          L>1,
          equal_but_swapped_chars(UT,UT1)
         ;
          L>1,
          sub_atom(UT1,_,_,_,UT)
         )
        ),
        UTXs),
  (Object\==command -> CaseMsg=' (respect case): ' ; CaseMsg=': '),
  format_object_alternatives_display_list(UTXs,Object,O,FTs),
  FTs\==[], % At least, one alternative for displaying
  my_remove_duplicates_sort(FTs,Ts),
  (Ts=[_] -> Pl='' ; Pl='s'),
  format_object(Object,FObject),
  write_info_log(['Possible ',FObject,Pl,CaseMsg,Ts]),
%  nl_compact_log,
  !.
display_object_alternatives(_O,_T) :-
  tapi(on),
  parsing_only(on),
  !,
  write_tapi_eot.
display_object_alternatives(_O,_T).

format_object(column(_Table),column) :-
  !.
format_object(user_predicate,'user predicate') :-
  !.
format_object(Object,Object).

format_object_alternatives_display_list([],_Object,_Name,[]).
format_object_alternatives_display_list([TX|TXs],Object,Name,[FT|FTs]) :-
  format_object_alternatives_display(TX,Object,Name,FT),
  format_object_alternatives_display_list(TXs,Object,Name,FTs).

format_object_alternatives_display([T,X],command,_Name,FT) :-
  !,
  (X==''
   ->
    M=''
   ;
    atom_concat(' ',X,M)
  ),
  atom_concat_list(['/',T,M],FT).
format_object_alternatives_display([V,_A],view,_Name,V) :-
  !.
format_object_alternatives_display([T,_A],table,_Name,T) :-
  !.
format_object_alternatives_display([C,Table],column(_Table),Name,TC) :-
  !,
  Name\==C, % Do not display the same column name (maybe because a table renaming
  atom_concat_list([Table,'.',C],TC).
%   (Name==C 
%    ->
%     TC=C  % This does not work for column renamings
%    ;
%     atom_concat_list([Table,'.',C],TC)).
format_object_alternatives_display([R,_A],relation,_,R) :-
  !.
format_object_alternatives_display([P,A],Object,_,PA) :-
  member(Object,[builtin,predicate,user_predicate]),
  !,
  atomic_concat_list([P,'/',A],PA).
format_object_alternatives_display(L,_Other,_,L).

% +Object (Var,table,view,column(TableName),command), -Name, -OtherInfo. 
% possible_name(+Object,Table,Name) :-
possible_name(Object,T,X) :-
        (
          Object==relation,
          !,
          my_table('$des',T,X)
         ;
          Object==(table), % SWI-Prolog defines table as an operator
          !,
          my_table('$des',T,_),
          \+ my_view('$des',T,X,_,_,_,_,_,_)
         ;
          Object==view, 
          !,
          my_view('$des',T,X,_,_,_,_,_,_)
         ;
          Object=column(X), 
          nonvar(X),
          my_relation(X),
          !,
          my_attribute('$des',_,X,T,_)
         ;
          Object=column(_), % Unknown table, maybe a renaming
          !,
          my_attribute('$des',_,X,T,_),
          \+ is_system_identifier(T)
          % atom_concat_list([TN,'.',C],T)
         ;
          Object==command, 
          !,
          command(_,_,_,T,X,_,_)
         ;
          Object==predicate,
          !,
          (possible_name(builtin,T,X)
           ;
           possible_name(user_predicate,T,X)
          )
         ;
          Object==builtin,
          !,
          my_builtin_preds(Preds),
          member(T/X,Preds)
         ;
%           Object==user_predicate,
%           user_predicates(Preds),
%           member(T/X,Preds)
%          ;
          Object==user_predicate, 
%          pdg_user_predicates(Preds), % WARNING: Do this with flags computed already
          user_predicates(Preds), % User predicates can be known only by its type declaration. 
          member(T/X,Preds),
          length(As,X),
          once((
            PH=..[T|As],
            (H = PH ; H = -(PH)),
            (datalog(H,_,_,_,_,_,_)
             ;
             datalog((H:-_B),_NVs,_RuleId,_CId,_Lines,_FileId,_Source))
          ))
         ).
         
pdg_user_predicates(Preds) :-
  pdg((Preds,_Arcs)),
  !.
pdg_user_predicates(Preds) :-
  user_predicates(Preds).
         
equal_up_to_one_char(T,T1) :-
  atom_codes(T,Ts),
  atom_codes(T1,T1s),
  equal_up_to_one_char_string(Ts,T1s).
  
%equal_up_to_one_char_string([_C],[]).
equal_up_to_one_char_string([],[]).
equal_up_to_one_char_string([X|Xs],[X|Ys]) :-
  !,
  equal_up_to_one_char_string(Xs,Ys).
equal_up_to_one_char_string([_X|Xs],[_Y|Xs]).
  
equal_but_one_char(T,T1) :-
  atom_codes(T,Ts),
  atom_codes(T1,T1s),
  equal_but_one_char_string(Ts,T1s).
  
equal_but_one_char_string([_C],[]).
equal_but_one_char_string([],[_C]).
equal_but_one_char_string([X|Xs],[X|Ys]) :-
  !,
  equal_but_one_char_string(Xs,Ys).
equal_but_one_char_string([_X|Xs],Xs).
equal_but_one_char_string(Xs,[_Y|Xs]).

equal_but_swapped_chars(T,T1) :-
  atom_codes(T,ST),
  atom_codes(T1,ST1),
  my_mergesort(ST,OST),
  my_mergesort(ST1,OST).
   
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debugging during development
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deb.

deb(_).

deb(Goal,Name) :-
  (functor(Goal,Name,_) -> deb ; true).
  
%%%%%%%%%%%%%%%  END des_common.pl  %%%%%%%%%%%%%%%
