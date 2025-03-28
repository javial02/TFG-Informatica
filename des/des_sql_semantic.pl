/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    SQL Semantic Checker                               */
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


% Check for SQL semantic errors in a syntactically-correct SQL statement. 
% Warn users of such possible errors since they are not errors per se 
% (a statement containing such errors might be intended).
% This follows the error descriptions in the paper [BG06]:
% "Semantic Errors in SQL Queries: A Quite Complete List", 
%  S. Brass and C. Goldberg, Elsevier's Journal of Systems and Software, 79(5), 2006


:- use_module(library(clpr),[]).  % Loaded at start-up. 
%:- use_module(library(clpfd)). % Already loaded
:- use_module(library(chr)).

% Bridges for propagating bindings in the abstract execution of built-ins
% Example: year(Date,2021), year(Date,Year) is translated into
% bridge(Date,2021), bridge(Date,Year) where, upon solving,
% Year is bound to 2021.
:- chr_constraint bridge/2.

reflexivity @ bridge(X,X) <=> true.
propagation @ bridge(X,Y) \ bridge(X,Z) <=> Y=Z.

% Solver for the string domain
% Constraints:
%   str_leq(X,Y): X is less than or equal to Y
%   str_lt(X,Y) : X is less than Y
%   str_neq(X,Y): X is not equal to Y
%   str_dom(X,L): X can only take a value in the sorted list L
% The Herbrand domain deals with equality 
% :- chr_option(debug,on).
:- chr_constraint str_leq/2, str_lt/2, str_neq/2, str_dom/2.

reflexivity     @ str_leq(X,X) <=> true.
antisymmetry    @ str_leq(X,Y), str_leq(Y,X) <=> X = Y.
idempotence     @ str_leq(X,Y) \ str_leq(X,Y) <=> true.
transitivity    @ str_leq(X,Y), str_leq(Y,Z) ==> str_leq(X,Z).
simplify_leq    @ str_leq(X,Y) <=> nonvar(X), nonvar(Y) | X @=< Y.

antireflexivity @ str_lt(X,X) <=> false.
asymmetry       @ str_lt(X,Y), str_lt(Y,X) <=> false.
idempotence     @ str_lt(X,Y) \ str_lt(X,Y) <=> true.
transitivity    @ str_lt(X,Y), str_lt(Y,Z) ==> str_lt(X,Z).
simplify_lt     @ str_lt(X,Y) <=> nonvar(X), nonvar(Y) | X @< Y.

distinct        @ str_lt(X,Y), str_leq(Y,X)  <=> false.
simplify_lt_leq @ str_lt(X,Y) \ str_leq(X,Y) <=> true.

simplify_neq_1  @ str_neq(X,Y) <=> nonvar(X), nonvar(Y) | X \== Y.

domain_pruning  @ str_dom(X,L), str_neq(X,Y) <=> nonvar(Y) | ord_subtract(L,[Y],L1), str_dom(X,L1).

empty_domain    @ str_dom(_X,[]) <=> fail.
singleton       @ str_dom(X,[Y]) <=> X = Y.
member_check    @ str_dom(X,L)   <=> nonvar(X) | memberchk(X,L).
intersection    @ str_dom(X,L1), str_dom(X,L2) <=> ord_intersection(L1,L2,L3), str_dom(X,L3).
lower_bound     @ str_dom(X,[Y|_]) ==> str_leq(Y,X).
upper_bound     @ str_dom(X,L) ==> append(_,[M],L) | str_leq(X,M).
domain_gaps     @ str_dom(X,L), str_lt(Y,X), str_lt(X,Z) <=> append(_,[Y,Z|_],L) | false.

% check_sql_semantic_error(+Lang,+SQLst,+Rs,+ARs)
% Checks SQL semantic errors in an SQL statement (represented in the 
% syntactic tree SQLst, and compiled into Datalog rules Rs).
check_sql_semantic_error(Lang,_SQLst,_RNVss,_ARs) :-
  (Lang\==sql ; sql_semantic_check(off)),
  !.
check_sql_semantic_error(_Lang,SQLst,RNVss,ARs) :-
%     write_rulesNVs_list(RNVss,0,x), nl,
%     display_rules_list(ARs,0), nl,
%  push_flag(type_casting,off,TC),
  push_flag(type_inferencing,off,TI),
  copy_term(RNVss,CRNVss),
  check_sql_semantic_error(SQLst,CRNVss,ARs),
  copy_term(CRNVss,RNVss,_), % Discard constraints but keep bindings
  pop_flag(type_inferencing,TI),
%  pop_flag(type_casting,TC).
  true.
  
check_sql_semantic_error(_SQLst,_RNVss,_ARs) :-
  % Retrieve and display already detected semantic errors during parsing
  %trace,
  semantic_error(M),
  sql_semantic_error_warning(M),
  fail.
check_sql_semantic_error((select(AD,T,Of,P,TL,F,W,G,H,O),AS),RNVss,ARs) :-
  check_sql_select_semantic_error((select(AD,T,Of,P,TL,F,W,G,H,O),AS),RNVss,ARs),
  !.
check_sql_semantic_error((SetSt,_AS),RNVss,ARs) :-
  SetSt =.. [SetOp,_AD,_SQLstL,_SQLstR],
  memberchk(SetOp,[union,except,intersect]),
  !,
  % Don't know which rules belong to which branch. Analyze only those errors not depending on the SQL statement
  check_sql_select_semantic_error(no_sql,RNVss,ARs).
check_sql_semantic_error((with(_SQLst,_SQLsts),_AS),_RNVss,_ARs). % Warning: TODO
  

check_sql_select_semantic_error(SQLst,RNVss,ARs) :-
  ruleNVs_to_rule_NVs_list(RNVss,Rs,NVss),
  rule_head_body_goals_list(Rs,Hs,Bss),
  check_sql_inconsistent_condition(Rs,ARs),               % Error  1
  check_sql_distinct(Hs,Bss,(Hs,Bss)),                    % Error  2
  check_sql_constant_column(SQLst,Rs),                    % Error  3
  check_sql_duplicated_column_values(SQLst,Rs),           % Error  4
  check_sql_unused_tuple(Bss,NVss),                       % Error  5
  check_sql_unnecessary_join(Bss,NVss),                   % Error  6
                                                          % Error  7 (processed in sql_to_dl)
  check_sql_tautological_condition(SQLst),                % Error  8 (also processed in post_table_constraint)
  check_sql_null_tautological_inconsistent_condition(Rs), % Error  8 for IS [NOT] NULL
  check_sql_null_comparison(SQLst),                       % Error  9
  check_sql_general_comparison(SQLst),                    % Error 11
  check_sql_like(SQLst),                                  % Error 12
  check_sql_complicated_exists(SQLst),                    % Error 13
  check_sql_distinct_aggregate(Rs,Bss),                   % Error 16 (first stage of this error is checked during parsing)
  check_sql_not_null_count(Rs,Bss),                       % Error 17
  check_sql_missing_join_cond(Bss,NVss),                  % Error 27
  check_sql_having_wo_group_by(SQLst),                    % Error 32
  check_sql_distinct_sum_avg(Bss),                        % Error 33
  check_sql_unnec_distinct(SQLst),                        % Error 2
  check_group_by_in_exists_subqry(SQLst),                 % Error 18
  check_if_distinct_intead_of_groub_by(SQLst).            % Error 22
  
  
%% Error 1: Inconsistent condition.

% check_sql_inconsistent_condition(+Rs,+ARs)
% Receives Datalog rules Rs and type-annotated rules ARs, and translates ARs to a CLP program 
% which is "partially" evaluated with the aim of detecting inconsistent conditions.
% ARs are needed for knowing the types of arguments and thus the target solver for constraints.
% As a side-effect, ground instantiations of variables can be found, which are passed to further 
% checking steps.
% Only the translation of SQL conditions and database constraints are of interest,
% so that the actual database instance is not used. Therefore, a call to a user
% predicate is considered to be simply true (together with possible constraints due to database 
% constraints).
check_sql_inconsistent_condition(Rs,ARs) :-
  semantic_check_translate_rules(Rs,ARs,TRs,Consistent),
%     display_rules_list(TRs,0),
  (Consistent==false
   ->
%    true 
    % Do not apply constraint solving if already determined inconsistent along translation
    sql_semantic_error_warning(['Inconsistent condition.'])
   ;
    (setof(TRs,semantic_check_partial_evaluation(TRs),L)
     ->
      (L=[TRs] -> true ; fail) % This failure means that more than one solution has been found. So, undo bindings
     ;
      sql_semantic_error_warning(['Inconsistent condition.'])
    )
  ),
  !.
check_sql_inconsistent_condition(_Rs,_ARs). % Do not bind constrained variables because of disjunctions

semantic_check_translate_rules(Rs,ARs,TRs,Consistent) :-
  semantic_check_translate_rules(Rs,ARs,TRs,_ExpandCtrs,Consistent).

semantic_check_translate_rules(Rs,ARs,TRs,ExpandCtrs,Consistent) :-
  semantic_check_translate_rules(Rs,ARs,[],TRs,ExpandCtrs,Consistent). % More than one translated rule per rule because of integrity constraints
  
semantic_check_translate_rules([],_ARs,TRs,TRs,_E,_C).
semantic_check_translate_rules([R|Rs],[AR|ARs],TRsi,[TR|TRso],E,C) :-
  semantic_check_translate_rule(R,AR,TR,TRsi,TRsi1,E,C),
  semantic_check_translate_rules(Rs,ARs,TRsi1,TRso,E,C).
  
semantic_check_translate_rule((H:-B),(_AH:-AB),(H:-TB),TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_rule(H,AH,TH,TRsi,TRso,E,C) :-
  semantic_check_translate_goal(H,AH,TH,TRsi,TRso,E,C).
  
semantic_check_translate_body((B1,B2),(AB1,AB2),(TB1,TB2),TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_goal(B1,AB1,TB1,TRsi,TRsi1,E,C),
  semantic_check_translate_body(B2,AB2,TB2,TRsi1,TRso,E,C).
semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C) :-
  semantic_check_translate_goal(B,AB,TB,TRsi,TRso,E,C).
  
% semantic_check_translate_goal(Condition,TypeAnnotatedCondition,TranslatedCondition,ConsistentCondition)
semantic_check_translate_goal('$autocast'(V1,V2,CV1,CV2),'$autocast'(Type1,Type2,Type1,Type2),'$autocast'(V1,V2,CV1,CV2),TRs,TRs,_E,_C) :-
  !.
semantic_check_translate_goal('$ctr'(Ctr,Type),_,'$ctr'(Ctr,Type),TRs,TRs,_E,_C) :-
  !.
semantic_check_translate_goal(G,_,true,TRs,TRs,_E,_C) :-
  semantic_binding_goal(G),
  !.
% ConsistentCondition can be found to be false in this stage due to a former condition simplification
semantic_check_translate_goal(not true,_,false,TRs,TRs,_E,false) :-
  !.
semantic_check_translate_goal(not false,_,true,TRs,TRs,_E,true) :-
  !.
% semantic_check_translate_goal(not DomainGoal,_,'$ctr'(str_dom(X,Domain),string(_)),TRs,TRs,_E,true) :-
%   str_domain_goal(DomainGoal, X, Domain),
%   !.
semantic_check_translate_goal(false,_,false,TRs,TRs,_E,false) :-
  !.
%   sql_semantic_error_warning(['Inconsistent condition. This statement returns no rows.']).
semantic_check_translate_goal(Cond,ACond,TCond,TRs,TRs,_E,_C) :-
  Cond=..[COp,_L,_R],
  map_cond(_,COp),
  !,
%   (ground(Cond)
%    ->
%     TCond=Cond
%    ;
    ACond=..[COp,Type,Type],
    constraint_typed_condition(Cond,Type,TCond).
%    ).
% semantic_check_translate_goal(or(L,R),or(AL,AR),(TL;TR),TRsi,TRso,C) :-
%   !,
%   semantic_check_translate_body(L,AL,TL,TRsi,TRsi1,C),
%   semantic_check_translate_body(R,AR,TR,TRsi1,TRso,C).
semantic_check_translate_goal(or(L,R),or(AL,AR),or(TL,TR),TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(L,AL,TL,TRsi,TRsi1,E,C),
  semantic_check_translate_body(R,AR,TR,TRsi1,TRso,E,C).
semantic_check_translate_goal(top(_,B),top(_,AB),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(offset(B,_),offset(AB,_),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(offset(B,_,_),offset(AB,_,_),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(distinct(_,B),distinct(_,AB),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(distinct(true),true,_TB,TRs,TRs,_E,_C) :-
  !.
semantic_check_translate_goal(distinct(B),distinct(AB),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(group_by(B,_,_),group_by(AB,_,_),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(order_by(B,_),order_by(AB,_),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(order_by(B,_,_),order_by(AB,_,_),TB,TRsi,TRso,E,C) :-
  !,
  semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
% semantic_check_translate_goal(not(B),not(AB),not(TB),TRsi,TRso,E,C) :-
%   !,
%   semantic_check_translate_body(B,AB,TB,TRsi,TRso,E,C).
semantic_check_translate_goal(RelGoal,_ARelGoal,TRelGoal,TRsi,TRso,E,C) :-
  E\==no_expand,
  functor(RelGoal,TableName,Arity),
  my_table('$des',TableName,Arity), 
  !,
  deb,
  findall((PDLs,APDLs),
          (
           my_integrity_constraint('$des',_Preds,Constraint,_NVs,_Head,_Ids,_SQL,IPDLs,IAPDLs,TableName),
           (str_domain_constraint(Constraint,PDLs,APDLs)
            ->
             true
            ;
             PDLs = IPDLs,
             APDLs = IAPDLs
           )
          ),
          PDLsAPDLsList),
  (PDLsAPDLsList==[]
   ->
    TRelGoal=true,
    TRso=TRsi
   ;
    my_unzip(PDLsAPDLsList,PDLsList,APDLsList),
    replace_var_term_unif(RelGoal,true,PDLsList,TPDLsList), % Bind variables with the first occurrence of RelGoal in the translated rules
    replace_var_term(RelGoal,true,APDLsList,TAPDLsList),
    my_zipWith(',',TPDLsList,TAPDLsList,TPDLsAPDLsList),
    semantic_check_translate_rules_arules(TPDLsAPDLsList,RelGoal,TRsi,TRso,TRelGoal,E,C)
  ).
semantic_check_translate_goal(UserGoal,_,UserGoal,TRs,TRs,_E,_C) :-
  functor(UserGoal,TableName,Arity),
  \+ my_table('$des',TableName,Arity), 
  user_predicate_goal(UserGoal),
  non_recursive_predicate(TableName/Arity),
  !.
semantic_check_translate_goal(G,_,bridge(X,Y),TRs,TRs,_E,_C) :-
  semantic_propagation_goal(G,X,Y),
  !.
semantic_check_translate_goal(_G,_,true,TRs,TRs,_E,_C).

% str_domain_goal(+DomainGoal, -X, -Domain)
% str_domain_goal(DomainGoal, X, Domain) :-
%   DomainGoal =.. [ICPredName|Args],
%   atom_concat('$ic',_,ICPredName),
%   setof(ICPredName)
% str_domain_constraint(+Constraint,-PDLs,-APDLs)
str_domain_constraint((RelGoal, not DomainGoal),PDLs,APDLs) :-
  DomainGoal =.. [ICName,Var],
  atom_concat('$ic',_,ICName),
  RelGoal =.. [TableName|Columns],
  my_nth1_member_var(Var,N,Columns),
  my_attribute('$des',N,TableName,_Att,Type),
  (Type=number(integer) ; Type=string(_Str)), % Domain constraint is only available for these domains
  !,
  setof(Var,A^B^C^D^E^F^datalog(DomainGoal,A,B,C,D,E,F),Domain),
  constraint_typed_condition(dom(Var,Domain),Type,Ctr),
  PDLs = [(DomainGoal :- RelGoal, Ctr)],
  copy_term((RelGoal,PDLs),(ARelGoal,APDLs)),
  get_table_types(TableName,Types),
  ARelGoal =.. [TableName|Types].
  
% Functional dependencies of builtins
% semantic_propagation_goal(Function,X,Y): Arguments X determine arguments Y
% This is used to propagate bindings for consequents among calls with the same antecedents
semantic_propagation_goal('select_not_null'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$trunc'(X,Y),X,Y).
semantic_propagation_goal('$_power'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$round'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$concat'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$instr'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$length'(X,Y),X,Y).
semantic_propagation_goal('$left'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$lower'(X,Y),X,Y).
semantic_propagation_goal('$lpad'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$lpad'(X,Y,Z,U),[X,Y,Z],U).
semantic_propagation_goal('$ltrim'(X,Y),X,Y).
semantic_propagation_goal('$repeat'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$replace'(X,Y,Z,U),[X,Y,Z],U).
semantic_propagation_goal('$reverse'(X,Y),X,Y).
semantic_propagation_goal('$right'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$rpad'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$rpad'(X,Y,Z,U),[X,Y,Z],U).
semantic_propagation_goal('$rtrim'(X,Y),X,Y).
semantic_propagation_goal('$space'(X,Y),X,Y).
semantic_propagation_goal('$substr'(X,Y,Z,U),[X,Y,Z],U).
semantic_propagation_goal('$trim'(X,Y),X,Y).
semantic_propagation_goal('$upper'(X,Y),X,Y).
semantic_propagation_goal('$year'(X,Y),X,Y).
semantic_propagation_goal('$month'(X,Y),X,Y).
semantic_propagation_goal('$day'(X,Y),X,Y).
semantic_propagation_goal('$hour'(X,Y),X,Y).
semantic_propagation_goal('$minute'(X,Y),X,Y).
semantic_propagation_goal('$second'(X,Y),X,Y).
semantic_propagation_goal('$last_day'(X,Y),X,Y).
semantic_propagation_goal('$to_char'(X,Y),X,Y).
semantic_propagation_goal('$to_char'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$to_date'(X,Y),X,Y).
semantic_propagation_goal('$to_date'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$datetime_add'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$datetime_sub'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$add_months'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$coalesce'(X,Y),X,Y).
semantic_propagation_goal('$greatest'(X,Y),X,Y).
semantic_propagation_goal('$least'(X,Y),X,Y).
semantic_propagation_goal('$nvl'(X,Y,Z),[X,Y],Z).
semantic_propagation_goal('$nvl2'(X,Y,Z,U),[X,Y,Z],U).
semantic_propagation_goal('$nullif'(X,Y,Z),[X,Y],Z).

semantic_binding_goal('$cast'(Atom,_Type,Atom)) :- !.
semantic_binding_goal('$lower'(Atom,Atom)) :- !.
semantic_binding_goal('$upper'(Atom,Atom)) :- !.
semantic_binding_goal('$like'(Atom,Pattern)) :- 
  \+ subatom_list(['%','_'],Pattern),
  !,
  Atom=Pattern.
semantic_binding_goal('$like'(Atom,Pattern,EscapeChar)) :- 
  my_set_diff(['%','_'],[EscapeChar],DECs),
  \+ subatom_list(DECs,Pattern),
  !,
  Atom=Pattern.
  

semantic_check_translate_rules_arules([(Rules,ARules)],_RelGoal,TRsi,TRso,Goal,E,C) :-
  semantic_check_translate_rules(Rules,ARules,TRsi,TRso,E,C),
  TRso=[Rule|_],
  rule_body(Rule,Goal).  
semantic_check_translate_rules_arules([GR1,GR2|GoalRulesList],RelGoal,TRsi,TRso,(Goal1,Goals),E,C) :- 
  semantic_check_translate_rules_arules([GR1],RelGoal,TRsi,TRsi1,Goal1,E,C),
  semantic_check_translate_rules_arules([GR2|GoalRulesList],RelGoal,TRsi1,TRso,Goals,E,C). 
  
constraint_typed_condition(Cond,number(float),'$ctr'(Cond,number(float))) :-
  !.
constraint_typed_condition(Cond,number(integer),'$ctr'(Cond,number(integer))) :-
  !.
constraint_typed_condition(Cond,string(T),'$ctr'(Cond,string(T))) :-
  !.
% constraint_typed_condition(Cond,datetime(_),'$ctr'(Cond,datetime(_))) :-
%   !.
constraint_typed_condition(Cond,_Type,Cond) :-
  ground(Cond),
  !.
constraint_typed_condition(_Cond,_Type,true).


% Partial evaluation of given rules
% The body of the first rule is partially evaluated w.r.t. the remaining rules
% User predicates are assumed to be true and conditions are posted to their 
% corresponding solvers. 
semantic_check_partial_evaluation([R|Rs]) :-
  rule_body(R,B),
  partial_clp_evaluation(B,Rs,[],_Bridges).
% semantic_check_partial_evaluation(_Rs) :-
%   sql_semantic_error_warning(['Inconsistent condition. This statement returns no rows.']).
  
partial_clp_evaluation((B,Bs),Rs,Bri,Bro) :-
  !,
  partial_clp_evaluation(B,Rs,Bri,Bri1),
  partial_clp_evaluation(Bs,Rs,Bri1,Bro).
partial_clp_evaluation((B1s;B2s),Rs,Bri,Bro) :-
  !,
  (partial_clp_evaluation(B1s,Rs,Bri,Bro)
   ;
   partial_clp_evaluation(B2s,Rs,Bri,Bro)).
partial_clp_evaluation(B,Rs,Bri,Bro) :-
  partial_clp_goal_evaluation(B,Rs,Bri,Bro).

partial_clp_goal_evaluation(true,_Rs,Br,Br) :-
  !.
partial_clp_goal_evaluation('$ctr'(Ctr,Type),_Rs,Bri,Bro) :-
  !,
  post_clp_ctr('$ctr'(Ctr,Type),Bri,Bro).
partial_clp_goal_evaluation(or(CtrL,CtrR),_Rs,Bri,Bro) :-
  !,
  post_clp_disj_ctr(CtrL,CtrR,Bri,Bro).
partial_clp_goal_evaluation(G,Rs,Bri,Bro) :-
  user_predicate_goal(G),
  !,
  (member((G:-B),Rs)  % All rules should have head because heads have been left linearized
   ;
   member(G,Rs),  %... but a translation resulted in an open fact (!): SELECT a FROM t E1 WHERE SELECT E1.a> SELECT AVG(a) FROM t;
   G\=(_:-_),
   B=true
  ),
  partial_clp_evaluation(B,Rs,Bri,Bro).
% partial_clp_goal_evaluation(not(G),Rs,Bri,Bro) :-
%   partial_clp_goal_evaluation(G,Rs,Bri,Bro),
%   !,
%   fail.
% partial_clp_goal_evaluation(not(_G),_Rs,Bri,Bri) :-
%   !.
partial_clp_goal_evaluation(G,_Rs,Bri,Bri) :-
  call(G).

'$autocast'(V1,V2,V1,V2). % Better call compute_conversion_primitive ?

% Posting constraints
% Constraints that are not supported by the underlying solvers
% are simply ignored (with catch).
post_clp_ctr('$ctr'(Ctr,number(float)),Bri,Bri) :-
  !,
  catch(clpr:{Ctr},_,true).
post_clp_ctr('$ctr'(dom(X,D),number(integer)),Bri,Bro) :-
  !,
  copy_term(X,FX),
  add_bridges(fd,[X],[FX],Bri,Bro),
  build_fd_range(D,Range),
  FX in Range.
post_clp_ctr('$ctr'(Cond,number(integer)),Bri,Bro) :-
  !,
  Cond=..[Op,L,R],
  copy_term([L,R],FDArgs),
  copy_term([L,R],QArgs),
  term_variables([L,R],Vs),
  term_variables(FDArgs,FDVs),
  term_variables(QArgs,QVs),
  op_fdop(Op,FDOp),
  FDCtr=..[FDOp|FDArgs],
  QCtr=..[Op|QArgs],
  add_bridges(fd,Vs,FDVs,Bri,Bri1),
  add_bridges(q,Vs,QVs,Bri1,Bro),
  catch(call(FDCtr),_,true),
  catch(clpq:{QCtr},_,true).
post_clp_ctr('$ctr'(Cond,string(_)),Bri,Bri) :-
  !,
  post_clp_str_ctr(Cond).
% post_clp_ctr('$ctr'(Cond,datetime(_))) :-
%   !,
%   post_clp_dt_ctr(Cond).
post_clp_ctr(_Ctr,Bri,Bri).

% post_clp_disj_ctr('$ctr'(LCond,number(integer)),'$ctr'(RCond,number(integer)),Br,Br) :-
%   !,
%   add_bridges(fd,Vs,FDVs,Bri,Bri1),
%   #\/(LCond,RCond).
post_clp_disj_ctr(_LCtr,_RCtr,Br,Br).
  

%%%% Domains:

% Domain of rationals: CLP(Q)

% Finite domain of integers: CLP(FD)
% WARNING: Consider translating those non-linear functions and operators
% that can be handled by the solver (as ** into ^)

op_fdop(=,#=).
op_fdop(<,#<).
op_fdop(>,#>).
op_fdop(>=,#>=).
op_fdop(=<,#=<).
op_fdop(\=,#\=).

add_bridges(_D,[],[],Br,Br).
add_bridges(D,[X|Xs],[Y|Ys],Bri,Bro) :-
  add_bridge(br(D,X,Y),Bri,Bri1),
  add_bridges(D,Xs,Ys,Bri1,Bro). 

add_bridge(br(D,X,Y),Bri,Bri) :-             % Existing bridge, retrieve bindings
  bridge_in(br(D,X,Y),Bri),
  !.
add_bridge(br(D,X,Y),Bri,[br(D,X,Y)|Bri]) :- % New bridge
  var(X),
  var(Y),
  !,
  add_domain_binding_daemon(D,X,Y).
add_bridge(_,Bri,Bri).
  
bridge_in(br(D,X,Y),[br(D,BX,Y)|_Bri]) :-
  var(BX),
  X==BX,
  !.
bridge_in(br(D,X,Y),[_|Bri]) :-
  bridge_in(br(D,X,Y),Bri).
  
add_domain_binding_daemon(fd,X,FD) :-
%  freeze(X,FD=X),
  freeze(X,FD#=X),
  freeze(FD,X=FD).
add_domain_binding_daemon(q,X,Q) :-
  freeze(X,clpq:{Q=X}),
  freeze(Q,q_to_int(Q,X)).
%   freeze(X,number_to_q(X,Q)),
%   freeze(Q,q_to_int(Q,X)).

% String domain: Ad-hoc solver.
post_clp_str_ctr(dom(X,D)) :-
  !,
  str_dom(X,D).
post_clp_str_ctr(L=R) :-
  !,
  L=R.
post_clp_str_ctr(Ctr) :-
  ground(Ctr),
  !,
  compute_comparison_primitive(Ctr,'SQL statement').
post_clp_str_ctr(L\=R) :-
  !,
  % dif(L,R).
  str_neq(L,R).
post_clp_str_ctr(L>R) :-
  !,
  str_lt(R,L).
post_clp_str_ctr(L<R) :-
  !,
  str_lt(L,R).
post_clp_str_ctr(L>=R) :-
  !,
  str_leq(R,L).
post_clp_str_ctr(L=<R) :-
  !,
  str_leq(L,R).
post_clp_str_ctr(_Ctr).
  

%% Error 2: Unnecessary DISTINCT
%%
%%          a) Warn if the query returns no duplicates and includes DISTINCT
%%
%%   Examples:
%%     create table t(a int primary key, b int);
%%     create table s(c int primary key, d int);
%%     select distinct a from t;
%%       Warning: Unnecessary DISTINCT because of primary key in [t].
%%     select distinct * from t,s where t.a=s.c;
%%       Warning: Unnecessary DISTINCT because of primary key in [s,t].
%%     select distinct t.a,s.d from t,s where t.a=s.c;
%%       Warning: Unnecessary DISTINCT because of primary key in [s,t].
%%     select b from t;
%%       No warning
%%
%%          b) Warn if distinct is applied several times
%%
%%   Examples:
%%     create table t(a int);
%%     create table s(a int);
%%     select distinct * from ((select * from t) union (select * from s));
%%

% check_sql_distinct(+Hs,+Bss,+(Hs,Bss))
check_sql_distinct([],[],_Rss).
check_sql_distinct([H|Hs],[Bs|Bss],Rss) :-
  check_sql_distinct_goals(Bs,H,Rss),
  check_sql_distinct(Hs,Bss,Rss).

check_sql_distinct_goals([],_H,_Rss).
check_sql_distinct_goals([distinct(G)|Gs],H,Rss) :-
  !,
%  G=..[_|Vs],
  term_variables(G,Vs),
  check_sql_distinct_goals([distinct(Vs,G)|Gs],H,Rss).
% Case b) Warn if distinct is applied several times
check_sql_distinct_goals([distinct(Vs,G)|_Gs],H,(Hs,Bss)) :-
  \+ \+ check_sql_subsequent_distinct_call(Vs,G,H,Hs,Bss),
  fail. % Explore the next case (a)
% Case a) Warn if the query returns no duplicates and includes DISTINCT
check_sql_distinct_goals([distinct(Vs,G)|Gs],H,Rss) :-
  !,
  \+ \+ check_sql_distinct_call(G,Vs,Rss),  % Discard groundings
  check_sql_distinct_goals(Gs,H,Rss).
check_sql_distinct_goals([_G|Gs],H,Rss) :-
  check_sql_distinct_goals(Gs,H,Rss).

check_sql_subsequent_distinct_call(Vs,G,H,[G|_Gs],[[distinct(G1)]|_Bss]) :-
  H\==G, % Dismiss the same rule
  term_variables(G1,G1Vs),
  length(Vs,L),
  length(G1Vs,L),
  sql_semantic_error_warning(['Unnecessary DISTINCT: Applied twice.']),
  !.
check_sql_subsequent_distinct_call(Vs,G,H,[_G|Gs],[_Bs|Bss]) :-
  check_sql_subsequent_distinct_call(Vs,G,H,Gs,Bss).

check_sql_distinct_call(G,Vs,_Rss) :-
  user_predicate_goal(G),
  G=..[F|_],
  \+ atom_concat(answer,_,F),
  !,
  check_sql_distinct_goal_list([G],Vs).
check_sql_distinct_call(G,Vs,Rss) :-
  check_sql_distinct_predicate_call(G,Vs,Rss).
  
check_sql_distinct_predicate_call(G,Vs,(Hs,Bss)) :-
  my_zipWith(',',Hs,Bss,Rss),
  findall(Bs,member((G,Bs),Rss),[Bs]), % Do not analyze union of rules 
  !,
  member((G,Bs),Rss), % Bind variables (findall does not)
  user_predicate_goals(Bs,Gs),
  check_sql_distinct_goal_list(Gs,Vs).
check_sql_distinct_predicate_call(_G,_Vs,_).
  
check_sql_distinct_goal_list(Gs,Vs) :-
  check_sql_unneeded_distinct_goal_list(Gs,Vs,Rels),
  !,
  sql_semantic_error_warning(['Unnecessary DISTINCT because of primary key in ',Rels,'.']).
check_sql_distinct_goal_list(_Gs,_Vs).

% check_sql_unneeded_distinct_goal_list(+Gs,+Vs,-Rels)
% true if distinct is unneeded, fail otherwise
check_sql_unneeded_distinct_goal_list(Gs,Vs) :-
  check_sql_unneeded_distinct_goal_list(Gs,Vs,_Rels).

check_sql_unneeded_distinct_goal_list(Gs,Vs,Rels) :-
  Gs\==[],
  goal_rels_fds_list(Gs,RFDs), % Keys (primary and unique as Xs->Ys) projected on goal arguments as: [(Rel,[(Xs-Ys)])]
  length(Gs,LGs),
  length(RFDs,LFDs),
  LFDs>=LGs,            % Each relation has one key at least (necessary condition)
  my_unzip(RFDs,Rels,FDss),
  concat_lists(FDss,FDs),
  bind_keys_and_consequents_fp(FDs,Vs),
  ground(Gs).


goal_rels_fds_list(Gs,FDs) :-
  goal_rels_fds_list(Gs,[],FDs).
    
% Fail if no key is found for a given goal 
goal_rels_fds_list([],FDs,FDs).  
goal_rels_fds_list([G|Gs],IFDs,OFDs) :-
  G=..[Rel|Args],
  findall(Atts,(my_primary_key('$des',Rel,Atts);my_candidate_key('$des',Rel,Atts)),Attss), 
  Attss\==[],
  !,
  project_tuple_list(G,Attss,Argss),
  my_set_diff_list(Args,Argss,DArgss),
  my_zipWith('-',Argss,DArgss,FDs),
  append(IFDs,[(Rel,FDs)],TFDs),
  goal_rels_fds_list(Gs,TFDs,OFDs).  
  
bind_keys_and_consequents_fp(FDs,Vs) :-
  term_variables(FDs,V1FDs),
  bind_keys_and_consequents(FDs,Vs),
  term_variables(FDs,V2FDs),
  length(V1FDs,L),
  (length(V2FDs,L)
   ->
    true
   ;
    bind_keys_and_consequents_fp(FDs,Vs)
  ).  
  
bind_keys_and_consequents([],_Vs).
bind_keys_and_consequents([(Ls-Rs)|FDs],Vs) :-
  term_variables(Ls,VLs),
  my_check_subset_var(VLs,Vs), % True if either there are variables in Ls that are in Vs or Ls is ground
  !,
  make_ground(VLs),
  make_ground(Rs),
  bind_keys_and_consequents(FDs,Vs).
bind_keys_and_consequents([_|FDs],Vs) :-
  bind_keys_and_consequents(FDs,Vs).


%% ENTENDIDO!
%% Error 3: Constant output column
%%          Warn if a column becomes trivially constant when it is not projected as such
%%   Examples:
%%     create table t(a int)
%%     select a from t where a=1
%%       Warning: Constant output column 'a' with value: 1.
%%     select 1 from t where a=1
%%       No warning

check_sql_constant_column((select(_AD,_T,_Of,Cs,_TL,_F,_W,_G,_H,_O),[Rel|_]),[R|_Rs]) :-
  check_sql_constant_column(on),
  rule_head(R,H),   %% extrae la cabeza de la regla
  H=..[_|Args],
  check_sql_constant_column_list(Args,Rel,Cs),
  !.
check_sql_constant_column(_SQLst,_Rs).
  
check_sql_constant_column_list([],_Rel,[]).
check_sql_constant_column_list([Arg|Args],Rel,[C|Cs]) :-
%  nonvar(Arg),
  ground(Arg),
  my_member_term(attr(R,_,_),C),
  R\==Rel, % Expressions in the projection list are allowed to be reused as in: select 1 a, a+1
  !,
  sql_semantic_error_warning(['Constant output column "','$exec'(write_proj_list([C],0)),'" with value "',Arg,'".']),
  check_sql_constant_column_list(Args,Rel,Cs).
check_sql_constant_column_list([_Arg|Args],Rel,[_C|Cs]) :-
  check_sql_constant_column_list(Args,Rel,Cs).

%% ENTENDIDO! 
%% Error 4: Duplicated column values
%%          Warn if two columns refer to the same expression
%%   Examples:
%%     create table t(a int)
%%     select a,a from t
%%       Warning: Duplicated column values in projection list.
%%     select 1,1       
%%       Warning: Duplicated column values in projection list.

check_sql_duplicated_column_values((select(_AD,_T,_Of,Cs,_TL,_F,_W,_G,_H,_O),_AS),[R|_Rs]) :-
  Cs\=='*',      % Do not warn if retrieving all columns with '*'
  check_sql_duplicated_column_values(on),
  rule_head(R,H), %%extrae la cabeza de la regla de datalog
  H=..[_|Args],
  extract_duplicates_var(Args,DVars),
  (DVars==[]
   ->
    true
   ;
    sql_semantic_error_warning(['Duplicated output column values in SELECT list.'])
  ),
  !.
check_sql_duplicated_column_values(_SQLst,_Rs).
  

%% ENTENDIDO!
%% Error 5: Unused tuple variable. Top 5
%%          An unaccessed single relation in the FROM list from the root query (Error 27 captures all other cases)
%%   Examples:
%%     create table t(a int)
%%     select 1 from t
%%       Warning: Unused relation 't'.

check_sql_unused_tuple([Bs|_Bss],[NVs|_NVss]) :- % Error 5
  user_predicate_goals(Bs,[G]),
  G=..[Rel|Args],
  term_variables(G,GVs),
  length(Args,L),
  length(GVs,L),
  'NVs_relevant_vars'(GVs,NVs,[]),
  !,
  sql_semantic_error_warning(['Columns of relation "',Rel,'" are not used.']).
check_sql_unused_tuple(_Bss,_NVss).


%% Error 6: Unnecessary join.
%%   Examples:
%%     create table dept(deptno int primary key, deptname string)
%%     create table emp(empno int primary key, empname string, deptno int references dept(deptno))
%%     select emp.* from emp natural inner join dept
%%       Warning: Unnecessary join with 'dept'.
%%     select dept.* from emp natural inner join dept
%%       No warning (maybe for another reason: duplicates)

check_sql_unnecessary_join([],_NVss).
check_sql_unnecessary_join([Bs|Bss],[NVs|NVss]) :-
  all_user_predicate_goals(Bs,Gs),
  check_sql_unnecessary_join_goals(Gs,NVs),
  check_sql_unnecessary_join(Bss,NVss).

check_sql_unnecessary_join_goals([],_NVs).
check_sql_unnecessary_join_goals([XG|Gs],NVs) :-
  (XG=G, % XG is the goal that references another goal via FK
   G=..[FKRel|_Args],
   my_foreign_key(_DBMS,FKRel,FKAtts,RFKRel,RFKAtts,_),
   relation_in_goals(Gs,RFKRel,RG),
   RG=..[_|RArgs]
  ;
   XG=RG, % XG is the referenced goal from another goal via FK
   RG=..[RFKRel|RArgs],
   my_foreign_key(_DBMS,FKRel,FKAtts,RFKRel,RFKAtts,_),
   relation_in_goals(Gs,FKRel,G)
  ),
  project_tuple(G,FKAtts,FK_Vars),
  project_tuple(RG,RFKAtts,RFK_Vars),
  FK_Vars==RFK_Vars,
  my_set_diff(RArgs,RFK_Vars,OArgs),
  term_variables(OArgs,OVars),
  length(OArgs,L),
  length(OVars,L), % Less variables means that some argument position is used
% This is remarked for: SELECT Discurre.Nombre, Discurre.Top�nimo FROM R�os, Discurre WHERE R�os.Nombre=Discurre.Nombre ORDER BY Discurre.Nombre;
  'NVs_relevant_vars'(OVars,NVs,[]),
  !,
  sql_semantic_error_warning(['Unnecessary join with "',RFKRel,'". There is a foreign key relating this with "',FKRel,'", and non-key attributes are not accessed.']),
  check_sql_unnecessary_join_goals(Gs,NVs).
% check_sql_unnecessary_join_goals([RG|Gs],NVs) :-
%   RG=..[RFKRel|RArgs],
%   my_foreign_key(_DBMS,FKRel,FKAtts,RFKRel,RFKAtts,_RIds),
%   relation_in_goals(Gs,FKRel,G),
%   project_tuple(G,FKAtts,FK_Vars),
%   project_tuple(RG,RFKAtts,RFK_Vars),
%   FK_Vars==RFK_Vars,
%   my_set_diff(RArgs,RFK_Vars,OArgs),
%   term_variables(OArgs,OVars),
%   'NVs_relevant_vars'(OVars,NVs,[]),
%   !,
%   sql_semantic_error_warning(['Unnecessary join with "',RFKRel,'". There is a foreign key relating this with "',FKRel,'", and non-key attributes are not accessed.']),
%   check_sql_unnecessary_join_goals(Gs,NVs).
check_sql_unnecessary_join_goals([_G|Gs],NVs) :-
  check_sql_unnecessary_join_goals(Gs,NVs).
  
% Fail if not found. Leave choice points for further warnings upon backtracking:
relation_in_goals([G|_Gs],Rel,G) :-
  G=..[Rel|_].
relation_in_goals([_G|Gs],Rel,RG) :-
  relation_in_goals(Gs,Rel,RG).


%% ENTENDIDO!
%% Error 7: Tuple variables are always identical.
%%          Processed before unfold_rules in cra_to_dl.
%%   Examples:
%%     create table dept(deptno int primary key, deptname string)
%%     create table emp(empno int primary key, empname string, deptno int references dept(deptno))
%%     select * from emp natural inner join emp
%%       Warning: Tuple variables are always identical: 'emp'.

check_sql_identical_tuples(Lang,_Rs) :-
  (Lang\==sql ; sql_semantic_check(off)),
  !.
check_sql_identical_tuples(_Lang,Rs) :-
  check_sql_identical_tuples(Rs).
  
check_sql_identical_tuples([]).
check_sql_identical_tuples([R|Rs]) :-
  rule_body(R,B),   %%guarda en B el cuerpo de la regla de Datalog
  my_list_to_tuple(Bs,B),   %%guarda B en forma de lista
  check_sql_identical_tuples_goal_list(Bs),
  check_sql_identical_tuples(Rs).

check_sql_identical_tuples_goal_list([_B]).
check_sql_identical_tuples_goal_list([B|Bs]) :-
  my_member_var(B,Bs),    %%comprueba si B esta (repetido) en Bs
  B=..[Rel|Args],   
  length(Args,L),
  user_predicate(Rel/L),
  !,
  sql_semantic_error_warning(['Tuple variables are always identical for the different occurrences of "',Rel,'".']),
  check_sql_identical_tuples_goal_list(Bs).
check_sql_identical_tuples_goal_list([_B|Bs]) :-
  check_sql_identical_tuples_goal_list(Bs).
  
 
%% Error 8: Implied or tautological condition.
%% 
%% Part of original error 8 which also included inconsistent condition,
%% but an inconsistent condition is checked in Error 1
%% Checking this is based on testing whether the complement of the condition fails,
%% meaning that the condition is trivially true (tautological)
%%   Examples:
%%     create table t (a int check a>0 or a<1)

check_sql_tautological_condition(PSQLst) :-
  PSQLst=(select(Distinct,Top,Offset,PList,TList,From,where(SQLCondition),GroupBy,Having,OrderBy),Schema), % Check, for now, this kind of select statements
  SQLCondition\==true,
  copy_term(SQLCondition,CSQLCondition),
  normalize_cond(not(CSQLCondition),NOTSQLCondition),
  NSQLst=(select(Distinct,Top,Offset,PList,TList,From,where(NOTSQLCondition),GroupBy,Having,OrderBy),Schema), 
  catch(
    (sql_to_dl(sql,NSQLst,_,_,_,DLs)),
    Message,
    (write_warning_log([Message]),
     fail)),
  rule_to_open_head_rule_list(DLs,ONRs),
  infer_types_rule_list(ONRs,ANRs,_,_,_),
  check_sql_tautological_condition(SQLCondition,ONRs,ANRs,no_expand),
  !.
check_sql_tautological_condition(_PSQLst).

% Rs are the Datalog rules for the complemented SQL condition in SELECT * FROM rel WHERE C;
% check_sql_tautological_condition(true,_Rs,_ARs,_ExpandCtrs) :-
%   !.
check_sql_tautological_condition(Condition,Rs,ARs,ExpandCtrs) :-
  semantic_check_translate_rules(Rs,ARs,TRs,ExpandCtrs,Consistent),
  copy_term(TRs,CTRs),
  (Consistent==false
   ->
    sql_semantic_error_warning(['Tautological condition: ','$exec'(write_sql_cond(Condition,0))])
   ;
    %true % Look for simplifiable conditions here
    (semantic_check_partial_evaluation(CTRs)
     ->
       true
     ;
       sql_semantic_error_warning(['Tautological condition: ','$exec'(write_sql_cond(Condition,0))])
    )
  ),
  !.
  
%% Error 8: Inconsistent / Implied or tautological condition over IS [NOT] NULL comparison.
%% 
% check_sql_null_tautological_inconsistent_condition(+Rs). 
% Check whether is_null / is_not_null is applied to a not-nullable column
check_sql_null_tautological_inconsistent_condition([]).
check_sql_null_tautological_inconsistent_condition([(_H :- B)|Rs]) :-
  !,
  my_list_to_tuple(Bs,B),
  all_user_predicate_goals(Bs,Gs),
  not_null_source_vars_list(Gs,SGs,NNVs),
  check_sql_null_list(Bs,SGs,NNVs),
  check_sql_null_tautological_inconsistent_condition(Rs).
check_sql_null_tautological_inconsistent_condition([_Fact|Rs]) :-
  check_sql_null_tautological_inconsistent_condition(Rs).
  
not_null_source_vars_list(Gs,SGs,NNVs) :-
  not_null_source_vars_list(Gs,[],SGs,[],NNVs).

not_null_source_vars_list([],SGs,SGs,NNVs,NNVs).
not_null_source_vars_list([G|Gs],SGsi,SGso,NNVsi,NNVso) :-
  not_null_source_vars(G,SGs,NNVs),
  append(SGsi,SGs,SGsi1),
  append(NNVsi,NNVs,NNVsi1),
  not_null_source_vars_list(Gs,SGsi1,SGso,NNVsi1,NNVso).

not_null_source_vars(G,SGs,NNVs) :-
  G=..[TableName|Vs],
  current_db(Conn),
  (my_not_nullables(Conn,TableName,NNColumnNames) -> true ; NNColumnNames=[]),
  (my_primary_key(Conn,TableName,PKColumnNames) -> true ; PKColumnNames=[]),
  findall(CKColumnName,(my_candidate_key(Conn,TableName,CKColumnNames),member(CKColumnName,CKColumnNames)),CKsColumnNames),
  my_set_union(NNColumnNames,PKColumnNames,NNPKColumnNames),
  my_set_union(NNPKColumnNames,CKsColumnNames,ColumnNames),
  (ColumnNames==[]
   ->
    SGs=[],
    NNVs=[]
   ;
    SGs=[G],
    get_att_positions(TableName,ColumnNames,Positions),
    my_nth1_member_list(NNVs,Positions,Vs)
  ).
  
check_sql_null_list([],_SGs,_Vs).
check_sql_null_list([is_null(V)|Gs],SGs,Vs) :-
  my_member_var(V,Vs),
  !,
  table_column_var_source_goals(V,SGs,TableColumn),
  sql_semantic_error_warning(['Inconsistent condition: IS NULL is applied to a column with a NOT NULL constraint (',TableColumn,')']),
  check_sql_null_list(Gs,SGs,Vs).
check_sql_null_list([is_not_null(V)|Gs],SGs,Vs) :-
  my_member_var(V,Vs),
  !,
  table_column_var_source_goals(V,SGs,TableColumn),
  sql_semantic_error_warning(['Tautological condition: IS NOT NULL is applied to a column with a NOT NULL constraint (',TableColumn,')']),
  check_sql_null_list(Gs,SGs,Vs).
check_sql_null_list([_G|Gs],SGs,Vs) :-
  check_sql_null_list(Gs,SGs,Vs).

table_column_var_source_goals(_V,[],'?').
table_column_var_source_goals(V,[G|_Gs],TableColumn) :-
  G=..[TableName|Vs],
  my_nth1_member_var(V,N,Vs),
  !,
  my_attribute(N,TableName,ColumnName,_Type),
  atom_concat_list(['"',TableName,'"."',ColumnName,'"'],TableColumn).
table_column_var_source_goals(V,[_G|Gs],TableColumn) :-
  table_column_var_source_goals(V,Gs,TableColumn).


%% ENTENDIDO!
%% Error 9: Comparison with NULL.
%%   Examples:
%%     create or replace table t(a string)
%%     select a from t where a=null
%%     select a from t where a<>null

check_sql_null_comparison(SQLst) :-
  ( (G=(_ =  cte('$NULL'(_),_)), NOT='') ; 
    (G=(cte('$NULL'(_),_) = _), NOT='') ;
    (G='<>'(_ ,  cte('$NULL'(_),_)), NOT='NOT ') ; 
    (G='<>'(cte('$NULL'(_),_), _), NOT='NOT ') ),
  my_member_term(G,SQLst),
  !,
  sql_semantic_error_warning(['Null comparison in: "','$exec'(write_sql_cond(G,0,'$des')),'". Consider using IS ',NOT,'NULL instead.']).
check_sql_null_comparison(_SQLst).
  
%% ENTENDIDO!
%% Error 11: Unnecessary general comparison operator.
%%           Warn if:
%%             - LIKE '%' occurs, which is equivalent to IS NOT NULL
%%             - Other cases pending as future work
%%           Additionally it warns about trivially true (resp. false) conditions as cte LIKE '%' (resp. NOT LIKE)
%%             This would be checked by a string solver in Error 1
%%   Examples:
%%     select 1 from dual where a like '%'
%%       Warning: Condition ''a' $like '%'' can be better rewritten as ''a' IS NOT NULL'.
%%     select 1 from dual where 'a' like '%' escape '%'
%%       No warning (maybe for another reason: no wildcards)

check_sql_general_comparison(SQLst) :-
  (Cond='$like'(L,cte('%',string(_)),EC), T=true ; Cond='$like'(L,cte('%',string(_))), EC=[], T=true ;
   Cond='$not_like'(L,cte('%',string(_)),EC), T=false ; Cond='$not_like'(L,cte('%',string(_))), EC=[], T=false
  ),
  my_member_term(Cond,SQLst),
  escape_characters_from_expr(EC,ECs),
  ECs\==['%'],
  (L\=cte(_,_)
   ->
    sql_semantic_error_warning(['Condition "','$exec'(write_sql_cond(Cond,0,'$des')),'" can be better rewritten as "','$exec'(write_expr(L,0,'$des')),' IS NOT NULL''.'])
   ;
    sql_semantic_error_warning(['Condition "','$exec'(write_sql_cond(Cond,0,'$des')),'" is trivially ',T,'.'])
  ).
check_sql_general_comparison(_SQLst).


%% ENTENDIDO!
%% Error 12: LIKE without wildcards.
%%   Examples:
%%     create or replace table t(a string)
%%     select a from t where a like 'x'
%%       Warning: LIKE pattern 'x' without wildcards.
%%     select a from t where a like '_' escape '_';  Note that "_" is a wildcard
%%       Warning: LIKE pattern '_' without wildcards.

check_sql_like(SQLst) :-
  Es=['%','_'],
  ((T='$like'(_,cte(Str,string(_)),EC) ; T='$not_like'(_,cte(Str,string(_)),EC)),
   my_member_term(T,SQLst), % Expressions other than constant strings are not checked
   escape_characters_from_expr(EC,ECs),
   my_set_diff(Es,ECs,DECs)
  ;
   (T='$like'(_,cte(Str,string(_))) ; T='$not_like'(_,cte(Str,string(_)))),
   my_member_term(T,SQLst),
   DECs=Es
  ),
  (DECs==[]
   ->
    true % Nothing to check
   ;
    (subatom_list(DECs,Str)
     ->
      true % At least one wildcard is in the like pattern
     ;
      sql_semantic_error_warning(['LIKE pattern "',Str,'" without wildcards (either % or _).'])
    )
  ),
  fail. % Check other like's in SQLst
check_sql_like(_SQLst). 

% escape_characters_from_expr([],[]) :-
%   !.
escape_characters_from_expr(cte(C,string(_)),[C]) :-
  !.
escape_characters_from_expr(_,[]) :-
  !.


%% ENTENDIDO!  
%% Error 13: Unnecessarily complicated SELECT in EXISTS-subquery.
%%   Examples:
%%     create or replace table t(a int, b int)
%%     select a from t where exists select a,b from t
%%       Warning: Unnecessarily complicated SELECT in EXISTS-subquery. Consider using * instead of 'a,b'.

check_sql_complicated_exists(SQLst) :-
  my_member_term(exists((select(_AD,_T,_Of,Cs,_TL,_F,_W,_G,_H,_O),_AS)),SQLst), 
  Cs\=[expr(attr(_,_,_),_,_)], % Column
  Cs\=[expr(cte(_,_),_,_)],    % Constant
  Cs\='*',                     % All columns
  sql_semantic_error_warning(['Unnecessarily complicated SELECT in EXISTS-subquery. Consider using "*" instead of "','$exec'(write_proj_list(Cs,0)),'".']),
  fail. % Check further errors
check_sql_complicated_exists(_SQLst). 


%% Error 16: Unnecessary DISTINCT in aggregation function.
%%           1- Warn if either MIN or MAX is used with a DISTINCT argument.
%%           2- Warn if other aggregate is used with a DISTINCT expression involving key columns
%%   Examples:
%%     create or replace table t(a int primary key, b int unique, c int);
%%     select sum(distinct a)-avg(distinct b) from t where b>1 GROUP BY a,b HAVING count(a)>1;
%%       Warning: Unnecesary DISTINCT in aggregate SUM for a key argument.
%%       Warning: Unnecesary DISTINCT in aggregate AVG for a key argument.
%%     select min(distinct c) from t;
%%       Warning: [Sem] DISTINCT should not be applied to the argument of MIN.
%%       This error is identified while parsing in des_sql.pl
%%     select sum(a) from t;
%%       No warning.

% Point 1: Checked during parsing (DISTINCT is omitted in both the SQL syntax tree and its Datalog compilation)
% Point 2: Checked after compilation as follows

check_sql_distinct_aggregate(Rs,Bss) :-
  (Aggr=sum_distinct(V) ; Aggr=avg_distinct(V) ; Aggr=times_distinct(V) ; Aggr=count_distinct(V)),
  Aggr=..[DF,V],
  atom_concat(F,'_distinct',DF),
  to_uppercase(F,UF),
  my_member_term(group_by(Goal,_,_,Cond),Bss),
  my_member_term(_=Aggr,Cond),
  ( ground(V), % Warning: This would be applied if simplification would work on group_by goal argument
    sql_semantic_error_warning(['Constant argument found for aggregate argument ',UF,'(DISTINCT ',V,').'])
   ;
    user_predicate_along_goal_call(Goal,Rs,UPCall),
    UPCall=..[Rel|_Args],
    (my_primary_key('$des',Rel,Atts) ; my_candidate_key('$des',Rel,Atts)),
    project_tuple(UPCall,Atts,PArgs),
    my_set_diff(PArgs,[V],[]), % Warning: Extend this to several variables when expressions are allowed as aggregate arguments
    sql_semantic_error_warning(['Unnecesary DISTINCT in aggregate ',UF,' for a key argument.']),
    fail % Request other possible occurrences
  ).    
check_sql_distinct_aggregate(_Rs,_Bss).

user_predicate_along_goal_call((Goal,Goals),Rs,UPCall) :-
  user_predicate_along_goal_call(Goal,Rs,UPCall)
  ;
  user_predicate_along_goal_call(Goals,Rs,UPCall).
user_predicate_along_goal_call(Goal,_Rs,Goal) :-
  user_predicate_goal(Goal),
  !.
user_predicate_along_goal_call(Goal,Rs,UPCall) :-
  member((Goal:-Body),Rs),
  my_list_to_tuple(Bs,Body),
  member(UPCall,Bs),
  user_predicate_goal(UPCall).


%% Error 17: Unnecessary argument of COUNT.
%%           Warn if COUNT is applied to an argument that cannot be null.
%%   Examples:
%%     create or replace table t(a int, b int, c int, unique (a,b));
%%     select count(a) from t where b>1 GROUP BY a,b HAVING count(a)>1;
%%       Warning: Unnecesary argument in COUNT because it cannot be NULL. Consider using COUNT(*) instead.
%%     select count(c) from t where c=1;
%%       This would be warned if simplification would made to work on group_by goal argument
%%     select count(c) from t;
%%       No warning.

check_sql_not_null_count(Rs,Bss) :-
  my_member_term(group_by(Goal,_,Cond),Bss),
  (my_member_term(_=count(V),Cond), Aggr = count
   ;
   my_member_term(_=count_distinct(V),Cond), Aggr = count_distinct),
  ( ground(V), % Warning: This would be applied if simplification would work on group_by goal argument
    sql_semantic_error_warning(['Constant argument found for COUNT(',V,').'])
   ;
    user_predicate_along_goal_call(Goal,Rs,UPCall),
    UPCall=..[Rel|_Args],
    ( my_primary_key('$des',Rel,Atts), Reason='primary key' ; 
      my_candidate_key('$des',Rel,Atts), Reason='candidate key' ; 
      Aggr\==count_distinct, my_not_nullables('$des',Rel,Atts), Reason='not null constraint' ),
    project_tuple(UPCall,Atts,PArgs),
    check_sql_not_null_count_reason(Aggr, V, PArgs, Reason),
    !
    %fail % Request other possible occurrences, but without further info for the user, better omit them
  ).    
check_sql_not_null_count(_Rs,_Bss).

% The argument of count is part of a key
check_sql_not_null_count_reason(count, V, PArgs, Reason) :-
  my_set_diff([V],PArgs,[]), % Check if the COUNT argument is part of a key
  sql_semantic_error_warning(['Unnecesary argument of COUNT because it cannot be null due to a ',Reason,'.']),
  !.
% The argument of count_distinct coincides with a key
check_sql_not_null_count_reason(count_distinct, V, PArgs, _Reason) :-
  [V]==PArgs,
  sql_semantic_error_warning(['Unnecesary DISTINCT in COUNT because it applies to a key.']).


%% Error 27: Missing join condition
%%           Warn if two relations are not joined by a criterium
%% Includes Error 5 for a single unused relation
%%   Examples:
%%     create table t(a int)
%%     create table s(a int)
%%     select * from t,s
%%       Warning: Missing join condition for [t,s].
%%     select * from t left join s 
%%       Warning: Missing join condition for [t,s].
%%     select * from t natural left join s
%%       No warning
%%     % Also nested queries:
%%     select * from (select * from t),(select * from s)                    
%%       Warning: Missing join condition for [t,s].

check_sql_missing_join_cond(Bss,NVss) :-
  copy_term((Bss,NVss),(CBss,CNVss)),
  bind_body_conditions_list(CBss),
  check_sql_missing_join_cond_list(CBss,CNVss),
  !.
check_sql_missing_join_cond(_Bss,_NVss).
  
bind_body_conditions_list([]).
bind_body_conditions_list([Bs|Bss]) :-
%  my_list_to_tuple(Gs,B),
  bind_conditions(Bs),
  bind_body_conditions_list(Bss).
  
bind_conditions([]).
bind_conditions([G|Gs]) :-
  bind_condition(G),
  bind_conditions(Gs).
  
% bind_condition(G) :-
%   G =.. [Op,A1,A2],
%   my_infix_comparison(Op,_),
% %   var(A1),
% %   var(A2),
%   term_variables(A1,[VA1]),
%   term_variables(A2,[VA2]),
%   !,
% %  A1=A2.
%   VA1=VA2.
bind_condition(G) :-  %2/6/2021
  G =.. [Op,A1,A2],
  my_infix_comparison(Op,_),
  term_variables(A1,VA1s),
  term_variables(A2,VA2s),
  !,
  (append(VA1s,VA2s,[H|Vs])
   ->
    my_map_1('='(H),Vs)
   ;
    true).
bind_condition(G) :-
  G=..[JOp,L,R,C],
  my_outer_join_relation(JOp/3),
  !,
  my_list_to_tuple(Cs,C),
  my_list_to_tuple(Ls,L),
  my_list_to_tuple(Rs,R),
  bind_conditions(Ls),
  bind_conditions(Rs),
  bind_conditions(Cs).
bind_condition(group_by(G,_GB,H)) :-
  !,
  my_list_to_tuple(Gs,G),
  my_list_to_tuple(Hs,H),
  bind_conditions(Gs),
  bind_conditions(Hs),
  append(Gs,Hs,[H|Vs]), %2/6/2021
  my_map_1('='(H),Vs). %2/6/2021
bind_condition(_G).

check_sql_missing_join_cond_list([],[]).
check_sql_missing_join_cond_list([Bs|Bss],[NVs|NVss]) :-
%  my_list_to_tuple(BL,B),
  user_predicate_goals(Bs,all,relation,Gs), % Only tables and views
  check_body_missing_join_cond(Gs,NVs),
  check_sql_missing_join_cond_list(Bss,NVss).
  
check_body_missing_join_cond(Gs,_NVs) :- 
  Gs=[_,_|_],
  !,
  goals_args(Gs,GAss),
  uncorrelated_relations(GAss,Rels),
  (Rels==[]
   ->
    true
   ;
    sql_semantic_error_warning(['Missing join condition for ',Rels,'.'])).
check_body_missing_join_cond(_Gs,_NVs).
 
all_user_predicate_goals(Bs,Gs) :-
  user_predicate_goals(Bs,all,_,Gs).
  
user_predicate_goals(Bs,Gs) :-
  user_predicate_goals(Bs,no_meta,_,Gs).

% user_predicate_goals(+Goals,+Meta,+Rel,+PredicateGoals). 
% Meta \in {all,no_meta}: either all predicates or only not in metapredicates (group_by, distinct)
% Rel \in {all,relation}: either all predicates or only those that the user has defined (my_table)
user_predicate_goals([],_Meta,_Rel,[]). 
user_predicate_goals([B|Bs],Meta,relation,[B|Gs]) :- 
  functor(B,Relation,_Arity),
  my_relation(Relation),
  !,
  user_predicate_goals(Bs,Meta,relation,Gs).
user_predicate_goals([B|Bs],Meta,Rel,[B|Gs]) :- 
  Rel \== relation,
  user_predicate_goal(B),
  !,
  user_predicate_goals(Bs,Meta,Rel,Gs).
user_predicate_goals([Join|Bs],Meta,Rel,Gs) :- 
  Join=..[JOp,L,R,_C],
  my_outer_join_relation(JOp/3),
  !,
  user_predicate_goals([L,R|Bs],Meta,Rel,Gs).
user_predicate_goals([group_by(G,_GB,_H)|Bs],all,Rel,Gs) :- 
  !,
  my_list_to_tuple(GGs,G),
  append(GGs,Bs,GBs),
  user_predicate_goals(GBs,all,Rel,Gs).
user_predicate_goals([top(_N,G)|Bs],all,Rel,Gs) :- 
  !,
  my_list_to_tuple(GGs,G),
  append(GGs,Bs,GBs),
  user_predicate_goals(GBs,all,Rel,Gs).
user_predicate_goals([_B|Bs],Meta,Rel,Gs) :- 
  user_predicate_goals(Bs,Meta,Rel,Gs).
  
user_predicate_goal(G) :- 
  functor(G,F,A),
  (atom_concat('$',_,F)
   ->
    fail
   ;
    user_predicate(F/A)).
  
goals_args([],[]). 
goals_args([G|Gs],[(G,As)|GAss]) :- 
  G=..[_|As],
  goals_args(Gs,GAss).
  
uncorrelated_relations(GAss,UGs) :-
  uncorrelated_relations(GAss,[],UGs).
  
% uncorrelated_relations(+[(Goal,GArgs)],+Args,-Relations)
uncorrelated_relations([],_LAs,[]).
uncorrelated_relations([(G,Vs)|GVss],LVs,RRels) :-
  append(Vs,LVs,LLVs),
  my_unzip(GVss,_,Vss),
  my_set_union_list(Vss,GVs),
  append(GVs,LVs,RVs),
  my_set_diff(Vs,RVs,UVs),
  length(Vs,L),
  (length(UVs,L)
   ->
    G=..[Rel|_],
    RRels=[Rel|Rels],
    uncorrelated_relations(GVss,LLVs,Rels)
   ;
%    RRels=Rels,
    uncorrelated_relations(GVss,LLVs,RRels)).
  

%% ENTENDIDO!
%% Error 32: Strange HAVING.
%%           Warn if a SELECT with HAVING does not include a GROUP BY.
%%           Such a statement returns zero o a single tuple.
%%   Examples:
%%     create table t(a int)
%%     select sum(a) from t having avg(a)>1
%%       Warning: Found a HAVING clause without a GROUP BY clause.

check_sql_having_wo_group_by((SQLst,_AS)) :-
  my_member_term(select(_AD,_T,_Of,_P,_TL,_F,_W,group_by([]),having(H),_O),SQLst),
  H\==true,   %% clausula HAVING no vacía
  !,
  sql_semantic_error_warning(['Found a HAVING clause with condition "','$exec'(write_sql_cond(H,0,'$des')),'" without a GROUP BY clause.']).
check_sql_having_wo_group_by(_SQLst).


%% ENTENDIDO!
%% Error 33: SUM(DISTINCT ...) or AVG(DISTINCT ...) 
%%           Warn if duplicate elimination is included for the argument of either SUM or AVG.
%%           If included, this might not be an error, but it is suspicious because usually duplicates are relevant for these aggregates
%%   Examples:
%%     create table t(a int)
%%     select sum(distinct a) from t
%%       Warning: .
check_sql_distinct_sum_avg(Bss) :-
  (my_member_term(sum_distinct(_),Bss),
   Fn='SUM'
  ;
   my_member_term(avg_distinct(_),Bss),
   Fn='AVG'
  ),
  !,
  sql_semantic_error_warning(['Using ',Fn,' with DISTINCT might not be appropriate.']).
check_sql_distinct_sum_avg(_Bss).
  


%% User warning
sql_semantic_error_warning(Message) :-
  parsing_only(off),
  !,
  write_notapi_warning_log(['[Sem] '|Message]).
sql_semantic_error_warning(Message) :-
  append(Message,['$tbc'],ContMessage),
  write_warning_log(['[Sem] '|ContMessage]).


%% ---------------AQUI EMPIEZO YO---------------------------------

check_sql_unnec_distinct((select(D,_T,_Of,Cs,_TL,from(Rls),where(Cond),group_by(Group),_H,_O),_AS)) :-
  D == 'distinct',
  extract_attributes(Cs, X),
  %%hacer un sort para eliminar duplicados. sort(X, Xnueva)
  sort(X, Xnueva),
  extract_attributes_from_equalities_in_cnf(Cond, Res),
  %hacer merge de Res con Xnueva y comprobar si se ordenan o no paraeliminar duplicados
  merge_lists(Xnueva, Res, X2), 
  %%Paso4: Hacer una cosa parecida a lo de las constantes, pero con attr, y comprobar con member_check, si el attr esta en el conjunto X
  loop_add_check(Cond, Rls, X2, X3),
  %%Paso5: Comprobar si las key de cada tabla estan en K
  (Group == [] 
    -> check_if_key_is_included(X3, Rls)
    ;
    check_if_group_by_in_k(X3, Group)),
  !,
  sql_semantic_error_warning(['Using unnecesary DISTINCT.']).

check_sql_unnec_distinct(_SQLst).

%%----------Paso2. Función para extraer atributos y comprobar que no son constantes----------
extract_attributes([],[]).
extract_attributes([expr(attr(A1, A2, A3), _, _)|Cs], [attr(A1, A2, A3)|As]):-
  extract_attributes(Cs, As).  
  
%%----------Paso3. Extraer los attr que se comparan con cte en where-------------------------
get_attr_from_cte_equality(attr(A1, A2, A3)=cte(_, _), attr(A1, A2, A3)).
get_attr_from_cte_equality(cte(_, _) = attr(A1, A2, A3), attr(A1, A2, A3)).

extract_attributes_from_equalities_in_cnf(Cond, [Attr]):-
  get_attr_from_cte_equality(Cond, Attr),
  !.

extract_attributes_from_equalities_in_cnf(and(C1,C2), [Attr | Resto]):-
  get_attr_from_cte_equality(C1, Attr),
  !,
  extract_attributes_from_equalities_in_cnf(C2, Resto).

extract_attributes_from_equalities_in_cnf(and(_,C2), Resto):-
  !,
  extract_attributes_from_equalities_in_cnf(C2, Resto).

extract_attributes_from_equalities_in_cnf(_Cond, []).

%%----------Unir las listas--------------------------------------
merge_lists(X, Y, Z):-
  append(X,Y, Aux),     %%Juntamos las dos listas
  remove_attr_duplicates(Aux, [], Z).  %%eliminamos duplicados pasandolo a conjunto

remove_attr_duplicates([], Acc, Acc). % Caso base: lista vacía
remove_attr_duplicates([attr(R, N, T)|Rest], Acc, Kout) :-
    (member(attr(R, N, _), Acc) ->  % Si ya hay un attr con el mismo R y N...
        remove_attr_duplicates(Rest, Acc, Kout)  % Lo ignoramos
    ;
        remove_attr_duplicates(Rest, [attr(R, N, T)|Acc], Kout) % Lo agregamos
    ).
 

%%----------Paso4. Bucle del algoritmo----------------------------------------------
%%-----add(Cond, Kin, Kout) que añade los attr de la igualdad A = B a K----------
add(attr(R1,N1,T1) = attr(R2,N2,T2), Kin, Kout):-
  (memberchk(attr(R1,N1,T1), Kin),
  \+ memberchk(attr(R2,N2,T2), Kin) %%\+ es not.
  -> Kin1 = [attr(R2,N2,T2)|Kin]
  ;  Kin1 = Kin ),
  (Kin == Kin1 ,
  memberchk(attr(R2,N2,T2), Kin1),
  \+ memberchk(attr(R1,N1,T1), Kin1) %%\+ es not.
  -> Kout = [attr(R1,N1,T1)|Kin1]
  ;  Kout = Kin1).
         


add(and(C1,C2), Kin, Kout):-
  add(C1, Kin, Kin1),
  add(C2, Kin1, Kout).

add(_, Kin, Kin).


%%------------Meter en K todos los attr de las relaciones que tengan una primary key en K------------


check_if_pk([],_,_).

check_if_pk([(Rname,_)|Rels], Kin, Kout):-
  (my_primary_key('$des',Rname, Atts) %%sacamos la primary key de esa rel
   ->check_attr_in_k(Atts, Kin),   %%guardamos todos los atributos de la tabla Rel en la lista As
   findall(attr(Rname,A,_), my_attribute('$des',_,Rname,A,_), As),
   %%convert_to_attr(As, Attrs),
   %%Unimos la nueva lista de atributos a nuestar K anterior y eliminamos duplicados
   merge_lists(Kin, As, Kmid),
   Kout = Kmid
   ; Kout = Kin), 
   check_if_pk(Rels, Kmid, Kout).




loop_add_check(Cond, Rels,Kin, KoutFinal) :-
  add(Cond, Kin, Kmid),      
  check_if_pk(Rels, Kmid, Kout), 
  msort(Kmid, SortedKmid),
  msort(Kout, SortedKout),
  (SortedKmid == SortedKout 
  -> KoutFinal = Kout
  ;
  loop_add_check(Cond, Rels, Kout, KoutFinal)).

loop_add_check(_, _, K, K).  % Caso base: Si Kin == Kout, detener el bucle.




%%--------------Paso5. Iremos tabla por tabla para comprobar si las key estan en K----------------
check_if_key_is_included(_, []):- !.     %%caso base

check_if_key_is_included(K, [(Rname,_) | Rels]):-
  (my_primary_key('$des',Rname, Atts) %%sacamos la primary key de esa rel
   ->check_attr_in_k(Atts, K)
   ; true),             %%ahora hay que comprobar si esa key esta en nuestro conjunto K
  check_if_key_is_included(K, Rels).


%%!!!!!!!!!!!!!!!!!!!Aqui tengo la duda de si deben estar todas las primary keys de la tabla incluidas o solo alguna!!!!!!!!!!!!!!!!
check_attr_in_k([Att | Atts], K):-
  (memberchk(attr(_,Att,_), K)
  ->true
  ;
  check_attr_in_k(Atts, K)).

check_attr_in_k([],_):- fail.


convert_to_attr([As|Ass],[attr(_,As,_)|Res]):-
  convert_to_attr(Ass, Res).

convert_to_attr([],_):- !.

%% --------------Caso para el Group By-----------------
check_if_group_by_in_k(_, []).

check_if_group_by_in_k(K, [expr(attr(_,Attr,_),_,_) | Gps]):-
  memberchk(attr(_,Attr,_), K),
  check_if_group_by_in_k(K, Gps).



%%ERROR 18
%%Unnecesary Group by in Exists subquery
%%Examples:
%%    create table t(a int) 
%%    create table s(a int, b int)
%%    select a from t where exists (select a from s group by a)

check_group_by_in_exists_subqry((select(_D,_T,_Of,_Cs,_TL,_F,where(exists((Cond))),_G,_H,_O),_AS)):-
  check_group_by_and_having(Cond),
  !,
  sql_semantic_error_warning(['Using unnecesary GROUP BY in EXISTS Subquery.']).

check_group_by_in_exists_subqry(_SQLst).
check_group_by_and_having((select(_D,_T,_Of,_Cs,_TL,_F,_W,group_by(Group),having(H),_O),_AS)):-
  Group \== [],
  H == 'true'.



%%ERROR 22
%%GROUP BY can be replaced by DISTINCT
%%Examples:
%%    create table s(a int, b int)
%%    select a,b from s group by a,b   

check_if_distinct_intead_of_groub_by((select(_D,_T,_Of,Cs,_TL,_F,_W,group_by(Group),_H,_O),_AS)):-
  extract_attributes(Cs, X),                %Extraemos los atributos de las columnas del select
  check_if_group_by_in_k(X, Group),         %Comprobamos que cualquier atributo del group by esta en las columnas del select         
  check_if_k_in_group_by(Group, X),         %Comprobamos que cualquier columna del select esta en los atributos del group by
  !,
  sql_semantic_error_warning(['GROUP BY can be replaced by DISTINCT']).

check_if_distinct_intead_of_groub_by(_SQLst).



%% --------------Caso contrario para ver si K en Group By-----------------
check_if_k_in_group_by(_, []).

check_if_k_in_group_by(Gps, [K | Ks]):-
  memberchk(expr(K,_,_), Gps),
  check_if_k_in_group_by(Gps, Ks).




%%ERROR 19
%%GROUP BY with singleton groups


check_group_by_with_singleton_groups((select(_D,_T,_Of,_Cs,_TL,from(Rls),_W,group_by(Group),_H,_O),_AS)):-



check_group_by_with_singleton_groups(_SQLst).


check_if_key_is_included(K, [(Rname,_) | Rels]):-
  (my_primary_key('$des',Rname, Atts) %%sacamos la primary key de esa rel
   ->check_attr_in_k(Atts, K)
   ; true),             %%ahora hay que comprobar si esa key esta en nuestro conjunto K
  check_if_key_is_included(K, Rels).


%%!!!!!!!!!!!!!!!!!!!Aqui tengo la duda de si deben estar todas las primary keys de la tabla incluidas o solo alguna!!!!!!!!!!!!!!!!
check_attr_in_k([Att | Atts], K):-
  (memberchk(attr(_,Att,_), K)
  ->true
  ;
  check_attr_in_k(Atts, K)).

check_attr_in_k([],_):- fail.



check_if_group_by_in_k(_, []).

check_if_group_by_in_k(K, [expr(attr(_,Attr,_),_,_) | Gps]):-
  memberchk(attr(_,Attr,_), K),
  check_if_group_by_in_k(K, Gps).