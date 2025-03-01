/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    Fuzzy Subsystem                                    */
/*                                                       */
/*                                                       */
/*                                 Pascual Julian-Iranzo */
/*                                  Fernando Saenz-Perez */
/*                                         (c) 2004-2021 */
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

:- encoding(iso_latin_1). % SWI-Prolog

%:- use_module(library(clpq)). % Loaded when switching to FDES with /system_mode fuzzy

/*********************************************************************/
/* Operators                                                         */
/*********************************************************************/

:- op(1010,yfx,[with]).  % Approximation degree in rules
:- op(300,yfx,[~]).      % Similarity Relation
:- op(300,yfx,[~~]).     % Weak Unification


/*********************************************************************/
/* Flags                                                             */
/*********************************************************************/

:- dynamic(lambda_cut/1).        % Lambda cut 
:- dynamic(t_norm/2).            % t-norm(Relation,T-norm): goedel (or min), product, luka, drastic, nilpotent or hamacher
:- dynamic(fuzzy_expansion/1).   % Fuzzy expansion of clauses: bpl (as in Bousi~Prolog) or des
:- dynamic(fuzzy_answer_subsumption/1). % Flag indicating whether fuzzy answer subsumption is enabled
:- dynamic(t_closure_comp/2).    % t-closure computation for a given relation: datalog (with Datalog rules) or prolog (a specific, naive algorithm)
:- dynamic(transitivity/1).      % Transitivity directive: as in Bousi~Prolog
:- dynamic(t_closure_updated/1). % If present for its argument (fuzzy relation name), indicates that its t-closure is updated
                                 % The t-closure of each fuzzy relation name is represented with a dynamic predicate RelName(Node,Node,Degree)
:- dynamic(weak_unification/1).  % Algorithm for weak unification: a1 (Sessa) or a3 (Block-based)
:- dynamic(weak_negation/1).     % Flag indicating if all the meaning is required (for negation) or not: 'on' or 'off'

% Setting fuzzy flags at system start-up:
set_default_fuzzy_flags :-
  set_flag(fuzzy_answer_subsumption,on), % Answer subsumption enabled
  set_flag(lambda_cut,0.0),              % Default lambda cut set to 0.0. Possible values in the interval [0.0..1.0)
  set_flag(fuzzy_expansion,des),         % Default fuzzy expansion. Possible values: bpl, des
  set_flag(weak_unification,a1),         % Default algorithm for weak unification. Possible values: a1 (Sessa), a3 (Block-based)
  set_flag(t_closure_comp,'~',prolog).   % Default t-closure computation for ~ (values: prolog, datalog)

/*********************************************************************/
/* Tables                                                            */
/*********************************************************************/

:- dynamic(fuzzy_relation/1).          % Defining a symbol as a fuzzy relation
:- dynamic(fuzzy_relation_property/2). % Properties: RelationName, Property (reflexive, symmetric, transitive)

/*********************************************************************/
/* Settings                                                          */
/*********************************************************************/

% Set default fuzzy setting after either abolishing (e.g., in /consult or /abolish) or startup
reset_fuzzy_setting :-
  \+ system_mode(fuzzy),
  !,
  unset_fuzzy_default_operators.
reset_fuzzy_setting :-
  !,
  retractall(t_closure_updated(_)),
  abolish_t_closures,
  set_fuzzy_default_operators,
  set_t_norm('~',goedel),
  set_fuzzy_relation_properties('~',[reflexive,symmetric,transitive]), % Begin. This should be done by set_weak_unification
%  set_fuzzy_relation_properties('~',[reflexive,symmetric]), % As BPL  %
  set_fuzzy_relation('~').                                             % End.
%   set_weak_unification(a1).
  
% set_weak_unification(Algorithm) :- 
%   set_flag(weak_unification,Algorithm),
%     (Algorithm==a3
%      ->
%       set_fuzzy_relation_properties('~',[reflexive,symmetric])
%      ;
%       set_fuzzy_relation_properties('~',[reflexive,symmetric,transitive])
%     ),
%     retractall(t_closure_updated('~')),
%     set_fuzzy_relation('~').
  
abolish_t_closures :-
  fuzzy_relation(RelName),
  functor(Rel,RelName,4),
  retractall(Rel),
  retract(fuzzy_relation(RelName)),
  fail.
abolish_t_closures.

set_fuzzy_default_operators :- 
  op(800,yfx,[@]),
  op(1010,yfx,[with]),
  op(300,yfx,[~,~~]).
%  op(800,yfx,[@,with]).
unset_fuzzy_default_operators :- 
  op(1200,xfx,[@]). % As for CHR

% Set fuzzy relation properties
set_fuzzy_relation_properties(_Relation,[]).
set_fuzzy_relation_properties(Relation,[Property|Properties]) :-
  set_tuple(fuzzy_relation_property(Relation,Property)),
  set_fuzzy_relation_properties(Relation,Properties).
  
del_fuzzy_relation_properties(_Relation,[]).
del_fuzzy_relation_properties(Relation,[Property|Properties]) :-
  del_tuple(fuzzy_relation_property(Relation,Property)),
  del_fuzzy_relation_properties(Relation,Properties).
  
% Set a fuzzy relation w.r.t. its properties defined in the table fuzzy_relation_property/2
set_fuzzy_relation(Relation) :-
  push_flag(system_mode,des,fuzzy),
  push_flag(batch,on,Batch),
  atom_concat('$',Relation,DRelation),
  set_tuple(fuzzy_relation(Relation)),
  (fuzzy_relation_property(Relation,reflexive)
   ->
    set_reflexive_relation(DRelation)
   ;
    del_reflexive_relation(DRelation)),
  (fuzzy_relation_property(Relation,symmetric)
   ->
    set_symmetric_relation(DRelation)
   ;
    del_symmetric_relation(DRelation)),
  (fuzzy_relation_property(Relation,transitive)
   ->
    set_transitive_relation(DRelation),
    del_intransitive_relation(DRelation),
    set_t_closure_relation(Relation)
   ;
    set_intransitive_relation(DRelation),
    del_transitive_relation(DRelation),
    del_t_closure_relation(Relation)
  ),
  set_similarity_relation(Relation),
  retractall(listing_hide_identifier(Relation)),
  assertz(listing_hide_identifier(Relation)),
  op(300,yfx,[Relation]),
  pop_flag(batch,Batch),
  compute_stratification,
  pop_flag(system_mode,fuzzy).

% Set a tuple for a table. Do nothing if it exists already
set_tuple(Tuple) :-
  Tuple,
  !.
set_tuple(Tuple) :-
  assertz(Tuple).  

% Delete a tuple from a table. Do nothing if it does not exist
del_tuple(Tuple) :-
  retract(Tuple),
  !.
del_tuple(_Tuple).  

  
/*********************************************************************/
/* Commands                                                          */
/*********************************************************************/

%%%% Parsing commands %%%%
parse_fuzzy_cmd(t_norm,[Relation,Value],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("T_NORM"),
  my_blanks,
  !,
  my_chars_but_blank_symbol(Relation),
  my_blanks,
  my_symbol(Value),
  my_blanks_star,
  !.
parse_fuzzy_cmd(Fuzzy_relation_cmd,[Relation,Properties],_NVs) -->
  {my_fuzzy_relation_cmd(Fuzzy_relation_cmd,Fuzzy_relation_cmdStr)},
  command_begin,
  my_blanks_star,
  my_kw(Fuzzy_relation_cmdStr),
  my_blanks,
  !,
  my_chars_but_blank_symbol(Relation),
  my_blanks,
  my_list_of_terms(Properties),
  my_blanks_star,
  !.
parse_fuzzy_cmd(transitivity,[TNorm],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("TRANSITIVITY"),
  my_blanks,
%  my_term(TNorm,[],_),
  my_symbol(TNorm),
  my_blanks_star,
  !.
parse_fuzzy_cmd(transitivity,[Relation,TNorm],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("TRANSITIVITY"),
  my_blanks,
  my_chars_but_blank_symbol(Relation),
  my_blanks,
%  my_term(TNorm,[],_),
  my_symbol(TNorm),
  my_blanks_star,
  !.
parse_fuzzy_cmd(t_closure_comp,[Relation,Value],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("T_CLOSURE_COMP"),
  my_blanks,
  my_chars_but_blank_symbol(Relation),
  my_blanks,
  my_symbol(Value),
  my_blanks_star,
  !.

my_fuzzy_relation_cmd(fuzzy_relation,"FUZZY_RELATION").
my_fuzzy_relation_cmd(fuzzy_rel,"FUZZY_REL").

%%%% Processing commands %%%%
process_fuzzy_command(fuzzy_answer_subsumption,[],_NVs,yes) :-
  !, 
  fuzzy_answer_subsumption(Switch),
  write_info_log(['Fuzzy answer subsumption is ', Switch, '.']).
process_fuzzy_command(fuzzy_answer_subsumption,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(fuzzy_answer_subsumption,'Fuzzy answer subsumption is',Switch).
process_fuzzy_command(lambda_cut,[],_NVs,yes) :-
  !, 
  lambda_cut(Value),
  write_info_log(['Lambda cut is ', Value, '.']).
process_fuzzy_command(lambda_cut,[Value],_NVs,yes) :-
  !,
  ensure_float(Value,FValue), 
  process_set_command_flag(lambda_cut,'Lambda cut is',FValue,'$interval'(float,0.0,1.0)).
process_fuzzy_command(list_fuzzy_equations,[],[],yes) :-
  process_fuzzy_command(list_fuzzy_equations,['~'],[],yes).
process_fuzzy_command(list_fuzzy_equations,[Relation],[],yes) :-
  !,
  FAtom=..[Relation,X,Y],
  UEq=(FAtom=D),
  atom_concat('$',Relation,DRelation),
  DSimRelation=..[DRelation,X,Y,D],
  (development(off)
   ->
%    findall(X~Y=D,datalog($~(X,Y,D),_,_,_,_,_,_),Eqs)
    findall(UEq,datalog(DSimRelation,_,_,_,_,_,_),Eqs)
   ;
%    findall($~(X,Y,D),datalog($~(X,Y,D),_,_,_,_,_,_),Eqs)),
    findall(DSimRelation,datalog(DSimRelation,_,_,_,_,_,_),Eqs)),
  display_objects(Eqs,equation,equations,listed).
process_fuzzy_command(list_t_closure,[],[],yes) :-
  process_fuzzy_command(list_t_closure,['~'],[],yes).
process_fuzzy_command(list_t_closure,[Relation],[],yes) :-
  !,
  update_fuzzy_relation(Relation,[]),
  Eq=..[Relation,X,Y,[],D],
  FAtom=..[Relation,X,Y],
  UEq=(FAtom=D),
  (development(off)
   ->
    findall(UEq,Eq,Eqs)
   ;
    findall(Eq,Eq,Eqs)
  ),
  display_objects(Eqs,equation,equations,listed).
process_fuzzy_command(t_closure_entries,[],NVs,yes) :-
  !, 
  process_fuzzy_command(t_closure_entries,[~],NVs,yes).
process_fuzzy_command(t_closure_entries,[Relation],_NVs,yes) :-
  !, 
  update_fuzzy_relation(Relation,[]),
  Rel=..[Relation,_I,_J,_CId,_Degree],
  catch(findall(a,Rel,Rs),Nbr=0,true),
  length(Rs,Nbr),
  set_flag(t_closure_entries,Nbr),
  write_info_log(['Number of entries: ', Nbr, '.']).
process_fuzzy_command(t_norm,[],NVs,yes) :-
  !, 
  process_fuzzy_command(t_norm,['~'],NVs,yes).
process_fuzzy_command(t_norm,[TNorm],NVs,yes) :-
  t_norm_values(ValidValues),
  member(TNorm,ValidValues),
  !,
  process_fuzzy_command(t_norm,['~',TNorm],NVs,yes).
process_fuzzy_command(t_norm,[Relation],_NVs,yes) :-
  !, 
  (t_norm(Relation,Value)
   ->
    atom_concat_list(['Current t-norm for ''',Relation,''' is '], Msg),
    write_info_log([Msg, Value, '.'])
   ;
    write_error_log(['No t-norm defined for ''',Relation,''' yet'])).
process_fuzzy_command(t_norm,[Relation,TNorm],_NVs,yes) :-
  !, 
  t_norm_values(ValidValues),
  (member(TNorm,ValidValues)
   ->
    set_t_norm(Relation,TNorm),
    write_info_verb_log(['Current t-norm for ''',Relation,''' is ', TNorm])
   ;
    write_error_log(['Incorrect value ''',TNorm,'''. Possible values are ',ValidValues])).
process_fuzzy_command(fuzzy_expansion,[],_NVs,yes) :-
  !, 
  fuzzy_expansion(Value),
  write_info_log(['Current fuzzy expansion is ', Value, '.']).
process_fuzzy_command(fuzzy_expansion,[Value],_NVs,yes) :-
  !, 
  process_set_command_flag(fuzzy_expansion,'Current fuzzy expansion is',Value,[bpl,des],Result),
  (Result==changed
   ->
    drop_database,
    write_info_verb_log(['Database cleared'])
   ;
    true).
process_fuzzy_command(weak_unification,[],_NVs,yes) :-
  !, 
  weak_unification(Value),
  write_info_log(['Current algorithm for weak unification is ', Value, '.']).
process_fuzzy_command(weak_unification,[Value],_NVs,yes) :-
  !, 
  (check_process_fuzzy_command
   ->
    process_set_command_flag(weak_unification,'Current algorithm for weak unification is',Value,[a1,a3],Result),
    (Result==changed
     ->
      drop_database,
%       (Value==a3
%        ->
%         del_tuple(fuzzy_relation_property('~',transitive))
%        ;
%         set_tuple(fuzzy_relation_property('~',transitive))
%       ),
      retractall(t_closure_updated('~')),
      set_fuzzy_relation('~'),
      write_info_verb_log(['Database cleared'])
     ;
      true)
   ;
    true).
process_fuzzy_command(t_closure_comp,[],_NVs,yes) :-
  !, 
  process_fuzzy_command(t_closure_comp,['~'],[],_).
process_fuzzy_command(t_closure_comp,[Relation],_NVs,yes) :-
  fuzzy_relation(Relation),
  !, 
  t_closure_comp(Relation,Value),
  write_info_log(['Current t-closure computation for ',Relation,' is ''', Value, '''.']).
process_fuzzy_command(t_closure_comp,[Value],NVs,yes) :-
  !,
  (member(Value,[datalog,prolog])
   ->
    process_fuzzy_command(t_closure_comp,['~',Value],NVs,yes)
   ;
    write_error_log(['Incorrect relation name or t-closure computation value: ',Value])
  ).
process_fuzzy_command(t_closure_comp,[Relation,Value],NVs,yes) :-
  !, 
  (fuzzy_relation(Relation), member(Value,[datalog,prolog])
   ->
    set_flag(t_closure_comp,Relation,Value),
    exec_if_verbose_on(processC(t_closure_comp,[Relation],NVs,_))
   ;
    write_error_log(['Incorrect relation name or t-closure computation value: ',[Relation,Value]])
  ).
process_fuzzy_command(fuzzy_relation,[],NVs,yes) :-
  !,
    (my_nf_setof(Relation,Property^fuzzy_relation_property(Relation,Property),Relations),
     member(Relation,Relations),
     process_fuzzy_command(fuzzy_relation,[Relation],NVs,yes),
     fail
    ;
     true).
process_fuzzy_command(fuzzy_relation,[Relation],_NVs,yes) :-
  !, 
  (fuzzy_relation(Relation)
   ->
    findall(Property,fuzzy_relation_property(Relation,Property),Properties),
    (Properties=[_] -> Number='Property', Verb=is ; Number='Properties', Verb=are),
    write_info_log([Number,' of ''',Relation,''' ',Verb,' ',Properties])
   ;
    write_warning_log(['Fuzzy relation ',Relation,' does not exist'])
  ).
process_fuzzy_command(fuzzy_relation,[Relation,Properties],_NVs,yes) :-
  !, 
  (check_process_fuzzy_command
   ->
    t_norm_values(TNormValues),
    my_add_tup_arg_list(transitive,[no|TNormValues],TranTNorm),
    append([reflexive,symmetric,transitive],TranTNorm,ValidProperties),
    my_subtract_var(Properties,ValidProperties,IncorrectValues),
    (IncorrectValues==[]
     ->
      my_remove(transitive(TNorm),Properties,RProperties),
      (Properties==RProperties
       ->
        SProperties=Properties,
        (\+ t_norm(Relation,_) -> set_t_norm(Relation,goedel) ; true) % Set the default t-norm for a new user-defined relation 
       ;
        (TNorm==no
         ->
          SProperties=RProperties
         ;
          set_t_norm(Relation,TNorm),
          SProperties=[transitive|RProperties]
        )
      ), 
      set_fuzzy_relation_properties(Relation,SProperties),
      my_subtract_var(ValidProperties,SProperties,DelProperties),
      del_fuzzy_relation_properties(Relation,DelProperties),
      retractall(t_closure_updated(_)),
      set_fuzzy_relation(Relation),
      update_fuzzy_relation(Relation,[]),
      (Properties=[_] -> Number=property, Verb=is ; Number=properties, Verb=are),
      write_info_verb_log(['Current ',Number,' for ''',Relation,''' ',Verb,' ', Properties])
     ;
      (IncorrectValues=[_] -> Number='' ; Number='s'),
      write_error_log(['Incorrect value',Number,' ''',IncorrectValues,'''. Possible values are ',ValidProperties]))
   ;
    true).
process_fuzzy_command(transitivity,[],_NVs,yes) :-
  !,
  list_transitivity('~').
process_fuzzy_command(transitivity,[Relation],_NVs,yes) :-
  fuzzy_relation(Relation),
  !,
  list_transitivity(Relation).
process_fuzzy_command(transitivity,[Transitivity],NVs,yes) :-
  t_norm_values(ValidValues),
  (memberchk(Transitivity,[no|ValidValues])
   ->
    process_fuzzy_command(transitivity,['~',Transitivity],NVs,yes)
   ;
    write_error_log(['Incorrect relation name or transitivity value: ',Transitivity])
  ),
  !.
process_fuzzy_command(transitivity,[Rel,TNorm],_NVs,yes) :-
  !,
  atom_concat('$',Rel,DRel),
  representative_t_norm(TNorm,RTNorm),
  t_norm(Rel,CTNorm), % Current t-norm
  ((CTNorm==RTNorm, % Same t-norm as before and already transitive
    fuzzy_relation_property(Rel,transitive)
   ;
    RTNorm==no,     % It was not transitive already
    \+ fuzzy_relation_property(Rel,transitive)
    )
   ->
    write_warning_log(['Transitivity for ',Rel,' has not been changed. It was already ''',TNorm,''''])
   ;
    % Update the relation property 'transitive' 
    update_transitivity_change(Rel,RTNorm),
    % Tag the t-closure as non-updated
    retractall(t_closure_updated(Rel)),
    set_transitivity(DRel,transitive(RTNorm)),
    write_info_verb_log(['Transitivity for ',Rel,' set to: ',transitive(RTNorm)])
  ).
  
  
check_process_fuzzy_command :-
  system_mode(fuzzy),
  !.
check_process_fuzzy_command :-
  write_error_log(['This command can only be used in fuzzy mode (change to this mode with /system_mode fuzzy)']),
  fail. 
  
update_transitivity_change(RelName,NewTNorm) :-
  % If the relation was transitive and becomes intransitive,
  % remove the property 'transitive' and 
  fuzzy_relation_property(RelName,transitive),
  NewTNorm==no,
  !,
  del_tuple(fuzzy_relation_property(RelName,transitive)).
update_transitivity_change(RelName,_NewTNorm) :-
  % If the relation was not transitive and becomes transitive,
  % set the property 'transitive' and tag the t-closure as non-updated
  \+ fuzzy_relation_property(RelName,transitive),
  !,
  set_tuple(fuzzy_relation_property(RelName,transitive)).
update_transitivity_change(_RelName,_NewTNorm).
  % Otherwise, the relation was transitive (but changes the t-norm),
  % nothing else to do
  

list_transitivity(Relation) :-
  (fuzzy_relation_property(Relation,transitive)
   ->
    t_norm(Relation,TNorm)
   ;
    TNorm=no),
  write_info_log(['Transitivity for ',Relation,': ',transitive(TNorm)]).
  
  
/*********************************************************************/
/* Synonymous commands                                               */
/*********************************************************************/

synonymous_command_options(t_norm,goedel,min).
synonymous_command_options(t_norm,min,goedel).
  

/*********************************************************************/
/* Setting relation properties                                       */
/*********************************************************************/

set_transitivity(DRel,Transitivity) :-
%  push_flag(batch,on,Batch),  % :::WARNING: From intransitive to transitive may deal to a non-updated stratification (because of group_by)
  push_flag(system_mode,des,Mode),
  set_transitivity_aux(DRel,Transitivity),
  pop_flag(system_mode,Mode).
%  pop_flag(batch,Batch).
  
set_transitivity_aux(DRel,transitive(no)) :-
  !,
  set_intransitive_relation(DRel),
  del_transitive_relation(DRel),
  atom_concat('$',Rel,DRel),
  del_t_closure_relation(Rel).
set_transitivity_aux(DRel,transitive(yes)) :-
  !,
  set_transitivity_aux(DRel,transitive(goedel)).
set_transitivity_aux(DRel,transitive) :-
  !,
  set_transitivity_aux(DRel,transitive(goedel)).
set_transitivity_aux(DRel,transitive(min)) :-
  !,
  set_transitivity_aux(DRel,transitive(goedel)).
set_transitivity_aux(DRel,transitive(luka)) :-
  !,
  set_transitivity_aux(DRel,transitive(lukasiewicz)).
set_transitivity_aux(DRel,transitive(TNorm)) :-
  set_transitivity_t_norm(DRel,TNorm).

set_transitivity_t_norm(DRel,TNorm) :-
  atom_concat('$',Rel,DRel),
  set_transitive_relation(DRel),
  set_t_closure_relation(Rel),
  del_intransitive_relation(DRel),
  set_t_norm(Rel,TNorm).
  
display_objects(List,Object,Objects,Action) :-
  my_mergesort(List,OList),
  display_rules_list(OList,0),
  length(List,Nbr),
  (Nbr==1 -> Txt = Object ; Txt = Objects),
  (compact_listings(on) -> true ; nl_log),
  write_info_log([Nbr,' ',Txt,' ',Action,'.']).

% Changing from des to fuzzy and the other way round
process_system_mode_change(des,fuzzy) :-
%  use_module(library(clpq)),
  enable_fuzzy_operators,
  processC(abolish,[],_,yes).
process_system_mode_change(fuzzy,des) :-
%  use_module(library(clpr)),
  disable_fuzzy_operators,
  processC(abolish,[],_,yes).
  
enable_fuzzy_operators :-
  op(1010,yfx,[with]), % Operator declaration for graded rules and answers
  op(300,yfx,[~]),
  op(300,yfx,[~~]).
  
disable_fuzzy_operators :-
  op(0,yfx,[with]), % Remove operator declaration
  op(0,yfx,[~]),
  op(0,yfx,[~~]).
   
/*********************************************************************/
/* T-Closure operations                                              */
/*********************************************************************/

% Retract closures of fuzzy relations for a given closure
% retract_fuzzy_relations_closure(+CId)
retract_fuzzy_relations(CId) :-
  fuzzy_relation(RelName),
  (weak_unification(a1) -> Arity=4 ; Arity=5),
  functor(Rel,RelName,Arity),
  Rel=..[RelName,_L,_R,CId|_],
  retractall(Rel),
  fail.
retract_fuzzy_relations(_CId).


% Update all fuzzy relations (needed in hypothetical queries due to asserting new equations)
update_fuzzy_relations(CId) :-
  system_mode(fuzzy),
  write_info_verb_log(['Updating fuzzy relations...']),
  fuzzy_relation(RelName),
  update_fuzzy_relation(RelName,CId),
  fail.
update_fuzzy_relations(_CId).

% Update ET contents with the meaning of a fuzzy relation
update_fuzzy_relation(RelName,_CId) :-
  t_closure_updated(RelName), % If already updated, do nothing
  !.
update_fuzzy_relation(RelName,CId) :-
  force_update_fuzzy_relation(RelName,CId),
  !,
  gen_rb(CId).
  
force_update_fuzzy_relation(RelName,CId) :-
  t_closure_comp(RelName,datalog),
  CId=[], % To enable t-closure computation for further contexts, datalog_all_levels in solve_goal should be tweaked, akin to the 'prolog' t-closure computation
  !,
  push_flag(lambda_cut,0.0,OldValue),
  functor(Rel,RelName,3), % Rel is a binary operator with an associated degree
  solve_datalog_query_complete_fill(Rel,[],CId),
  update_t_closure_from_et(Rel,CId),
%  update_e_clauses(RelName,CId),
  pop_flag(lambda_cut,OldValue).
force_update_fuzzy_relation(RelName,CId) :-
  init_sq_matrix(RelName,CId,Matrix,Nodes),
  (fuzzy_relation_property(RelName,transitive)
   ->
    t_closure_matrix(RelName,Matrix)
   ;
    true),
  functor(Rel,RelName,4),
  arg(3,Rel,CId),
  retractall(Rel), % Retract
  update_t_closure_from_matrix(RelName,CId,Matrix,Nodes).
%  update_e_clauses(RelName,CId).
  
update_t_closure_from_et(Rel,CId) :-
  Rel=..[RelName,F,T,Degree],
  CRel=..[RelName,F,T,CId,Degree],
  retractall(CRel),
  et(Rel,_,CId,_),
  Degree>0,
  assertz(CRel),
  fail.
update_t_closure_from_et(Rel,_CId) :-
  functor(Rel,RelName,_),
  retractall(t_closure_updated(RelName)),
  assertz(t_closure_updated(RelName)).

update_t_closure_from_matrix(RelName,CId,Matrix,Nodes) :-
  my_nth1_member(I,NI,Nodes),
  my_nth1_member(J,NJ,Nodes),
  matrix_get_cell(Matrix,NI,NJ,Degree),
  Degree>0,
  Rel=..[RelName,I,J,CId,Degree],
  assertz(Rel),
  fail.
update_t_closure_from_matrix(RelName,_CId,_Matrix,_Nodes) :-
  retractall(t_closure_updated(RelName)),
  assertz(t_closure_updated(RelName)).

% init_sq_matrix(+RelName,-Matrix,-Nodes) is det
init_sq_matrix(RelName,CId,Matrix,Nodes) :-
  atom_concat('$',RelName,DRelName),
  functor(DRel,DRelName,3),
  DRel=..[DRelName,From,To,Degree],
  findall(arc(From,To,Degree,RCId),datalog_all_levels(DRel,_NVs,_RId,CId,RCId,_Ls,_FId,_Rs),Arcs),
  ord_aggregate(Arcs,arc(From,To,Degree,ACId),[From,To],max(ACId),MaxArcs), % For duplicated equations (at differente levels), choose the one in the deepest level
  my_member_list(MaxArcs,Arcs), % Set the degree
  my_nf_setof(Node,Arc^(member(Arc,MaxArcs),(arg(1,Arc,Node);arg(2,Arc,Node))),Nodes),
  index_arcs(MaxArcs,Nodes,IndArcs),
  length(Nodes,Dim),
  init_matrix(Dim,Matrix),
  matrix_reflexive(RelName,Dim,Matrix),
  matrix_symmetric(RelName,IndArcs,Matrix),
  matrix_zeroes(Matrix).
  
index_arcs([],_Nodes,[]).
index_arcs([arc(F,T,D,_)|Arcs],Nodes,[arc(IF,IT,D)|IndArcs]) :-
  my_nth1_member(F,IF,Nodes),
  my_nth1_member(T,IT,Nodes),
  index_arcs(Arcs,Nodes,IndArcs).
 
init_matrix(Dim,Matrix) :-
  functor(Matrix,vector,Dim),
  matrix_column_fill(1,Dim,Matrix).

matrix_column_fill(I,Dim,Matrix) :-
  I=<Dim,
  !,
  arg(I,Matrix,Column),
  functor(Column,vector,Dim),
  I1 is I+1,
  matrix_column_fill(I1,Dim,Matrix).
matrix_column_fill(_I,_Dim,_Matrix).

matrix_reflexive(RelName,Dim,Matrix) :-
  fuzzy_relation_property(RelName,reflexive),
  !,
  matrix_reflexive_fill(1,Dim,Matrix).
matrix_reflexive(_RelName,_Dim,_Matrix).
  
matrix_reflexive_fill(I,Dim,Matrix) :-
  I=<Dim,
  !,
  matrix_set_cell(Matrix,I,I,1.0),
  I1 is I+1,
  matrix_reflexive_fill(I1,Dim,Matrix).
matrix_reflexive_fill(_I,_Dim,_Matrix).

matrix_symmetric(RelName,IndArcs,Matrix) :-
  (fuzzy_relation_property(RelName,symmetric)
   -> 
    Symmetric=true
   ;
    Symmetric=false),
  matrix_symmetric_fill(IndArcs,Symmetric,Matrix).

matrix_symmetric_fill([],_Symmetric,_Matrix).
matrix_symmetric_fill([arc(F,T,D)|IndArcs],Symmetric,Matrix) :-
  matrix_set_cell(Matrix,F,T,D),
  (Symmetric==true
   ->
    matrix_set_cell(Matrix,T,F,D)
	 ;
	  true
	),
  matrix_symmetric_fill(IndArcs,Symmetric,Matrix).

matrix_zeroes(Matrix) :-
  term_variables(Matrix,Vs),
  set_mutable_list(Vs,0.0).
  
% Floyd–Warshall-based algorithm for computing a t-closure

t_closure_matrix(RelName,Matrix) :-
  functor(Matrix,_,Dim),
  t_norm_matrix_k(1,1,1,Dim,RelName,Matrix,Change),
  (Change==true
   ->
    t_closure_matrix(RelName,Matrix)
   ;
    true).
    
t_norm_matrix_k(K,_I,_J,Dim,_RelName,_Matrix,_Change) :-
  K>Dim.
t_norm_matrix_k(K,I,J,Dim,RelName,Matrix,Change) :-
  t_norm_matrix_i(K,I,J,Dim,RelName,Matrix,Change),
  K1 is K+1,
  t_norm_matrix_k(K1,I,J,Dim,RelName,Matrix,Change).

t_norm_matrix_i(_K,I,_J,Dim,_RelName,_Matrix,_Change) :-
  I>Dim.
t_norm_matrix_i(K,I,J,Dim,RelName,Matrix,Change) :-
  t_norm_matrix_j(K,I,J,Dim,RelName,Matrix,Change),
  I1 is I+1,
  t_norm_matrix_i(K,I1,J,Dim,RelName,Matrix,Change).

t_norm_matrix_j(_K,_I,J,Dim,_RelName,_Matrix,_Change) :-
  J>Dim.
t_norm_matrix_j(K,I,J,Dim,RelName,Matrix,Change) :-
  matrix_get_cell(Matrix,I,J,IJ),
  matrix_get_cell(Matrix,I,K,IK),
  matrix_get_cell(Matrix,K,J,KJ),
  t_norm_composition(RelName,[IK,KJ],T),
  (T > IJ
   ->
	  matrix_set_cell(Matrix,I,J,T),
	  Change=true
	 ;
	  true
	),
  J1 is J+1,
  t_norm_matrix_j(K,I,J1,Dim,RelName,Matrix,Change).

  
% BEGIN Matrix operations:

matrix_set_cell(Matrix,I,J,Value) :-
  arg(I,Matrix,Column),
  arg(J,Column,Cell),
  set_mutable(Value,Cell).
 
matrix_get_cell(Matrix,I,J,Value) :-
  arg(I,Matrix,Column),
  arg(J,Column,Cell),
  my_get_mutable(Value,Cell).
 
% END Matrix operations:


% BEGIN Mutable cell operations:

set_mutable(Value,Cell) :-
  (var(Cell)
   ->
  	my_create_mutable(Value,Cell)
   ;
  	my_update_mutable(Value,Cell)
  ). 

set_mutable_list([],_).
set_mutable_list([X|Xs],Value) :-
  set_mutable(Value,X),
  set_mutable_list(Xs,Value).

% END Mutable cell operations

/*********************************************************************/
/* Assertions                                                        */
/*********************************************************************/

% Commands which can be parsed as directives :-Command(Arguments)
fuzzy_assertion(fuzzy_expansion,1).
fuzzy_assertion(fuzzy_relation,2).
fuzzy_assertion(fuzzy_rel,2). % BPL Compatibility
fuzzy_assertion(lambda_cut,1).
fuzzy_assertion(lambdacut,1). % BPL Compatibility
fuzzy_assertion(list_t_closure,0).
fuzzy_assertion(list_t_closure,1).
fuzzy_assertion(t_closure_comp,1).
fuzzy_assertion(t_norm,1).
fuzzy_assertion(t_norm,2).
fuzzy_assertion(transitivity,1).
fuzzy_assertion(transitivity,2).
fuzzy_assertion(weak_unification,1).
  
/*********************************************************************/
/* Functions                                                         */
/*********************************************************************/

fuzzy_function('t_norm','t_norm','Fuzzy t-norm',fuzzy,[number(_),list(_)],1) :- !.
%fuzzy_function('~','sim','Fuzzy similarity',fuzzy,[number(_),string(_),string(_)],2).
fuzzy_function(RelName,'sim','Fuzzy similarity',fuzzy,[number(_),string(_),string(_),string(_)],3) :-
  fuzzy_relation(RelName).

/*********************************************************************/
/* Operators                                                */
/*********************************************************************/

%my_fuzzy_infix_operator('~',"~",'sim',[number(_),string(_),string(_)],'Approximation degree',400,yfx) .
my_fuzzy_infix_operator(RelName,StrRelName,'sim'(RelName),[number(_),string(_),string(_)],'Approximation degree',300,yfx) :-
  fuzzy_relation(RelName),
  atom_codes(RelName,StrRelName).

/*********************************************************************/
/* Parsing                                                           */
/*********************************************************************/

% Fuzzy atoms
%my_fuzzy_atom('$~'(L,R,D),Vi,Vo) -->
my_fuzzy_atom(SimRelation,Vi,Vo) -->
  my_noncompound_term_or_pred_spec(L,Vi,Vo1),
  my_blanks_star,
  {fuzzy_relation(Relation),
   atom_codes(Relation,StrRelation)},
%  "~",
  StrRelation,
  my_blanks_star,
  my_noncompound_term_or_pred_spec(R,Vo1,Vo2),
  {assign_variable_names([D],Vo2,Vo),
   atom_concat('$',Relation,DRelation),
   SimRelation=..[DRelation,L,R,D]}.
% my_fuzzy_atom('$t_norm'(Relation,List,Degree),Vi,Vo) -->
%   "'$t_norm'",
%   my_blanks_star,
%   "(",
%   my_blanks_star,
%   my_user_atom(Relation,Vi,Vo1),
%   my_blanks_star,
%   ",",
%   my_blanks_star,
%   my_noncompound_term_list(List,Vo1,Vo2),
%   my_blanks_star,
%   ",",
%   my_blanks_star,
%   my_noncompound_term(Degree,Vo2,Vo),
%   my_blanks_star,
%   ")".
% my_fuzzy_atom(@(A,D),Vi,Vo) -->
%   my_user_atom(A,Vi,Vo),
%   my_blanks_star,
%   my_fuzzy_degree_operator,
%   my_blanks_star,
%   my_number(D).
  
my_fuzzy_degree_operator -->
  "@".
% my_fuzzy_degree_operator -->
%   "with".

my_noncompound_term_or_pred_spec(C,Vi,Vo) -->
  my_noncompound_term(C,Vi,Vo).
my_noncompound_term_or_pred_spec(P,Vi,Vi) -->
  my_pred_spec(P).


% Relocate rule approximation degrees
% Degrees are relocated from the body to the head
relocate_fuzzy_rule_degrees(T,RT) :-
  system_mode(fuzzy),
  !,
  relocate_fuzzy_rule_degrees_aux(T,RT).
relocate_fuzzy_rule_degrees(T,T).

relocate_fuzzy_rule_degrees_aux(T,T) :-
  (var(T) ; atomic(T)),
  !.
relocate_fuzzy_rule_degrees_aux((H:-(B@D)),((H@D):-RB)) :-
  !,
  relocate_fuzzy_rule_degrees_aux(B,RB).
relocate_fuzzy_rule_degrees_aux((H:-(L=>(B@D))),((H@D):-(RL=>RB))) :-
  !,
  relocate_fuzzy_rule_degrees_aux(L,RL),
  relocate_fuzzy_rule_degrees_aux(B,RB).
relocate_fuzzy_rule_degrees_aux(T,RT) :-
%  T =.. [F|As],
  univ_head(F,As,Restricted,T),
  relocate_fuzzy_rule_degrees_aux_list(As,RAs),
%  RT =.. [F|RAs].
  univ_head(F,RAs,Restricted,RT).
    
relocate_fuzzy_rule_degrees_aux_list([],[]) :-
  !.
relocate_fuzzy_rule_degrees_aux_list([T|Ts],[RT|RTs]) :-
  relocate_fuzzy_rule_degrees_aux(T,RT), 
  relocate_fuzzy_rule_degrees_aux_list(Ts,RTs).


% Fuzzy rules:
% 1. Equations
parse_fuzzy_rule(Rule,Vi,Vo) -->
  my_fuzzy_equation(Rule,Vi,Vo).
% 2. Clauses 
% This case is handled by my_body in des.pl
% parse_fuzzy_rule(((H@D) :- B),Vi,Vo) -->
%   my_head(H,Vi,Vo1),
%   my_blanks_star,
%   ":-",
%   my_blanks_star,
%   my_body(B,Vo1,Vo),
%   my_fuzzy_rule_degree(D).
parse_fuzzy_rule(((H@D) :- B),Vi,Vo) -->
  my_fuzzy_head((H@D),Vi,Vo1),
  my_blanks_star,
  ":-",
  my_blanks_star,
  my_body(B,Vo1,Vo).
% 3. Facts
parse_fuzzy_rule(F,Vi,Vo) -->
  my_fuzzy_fact(F,Vi,Vo).

my_fuzzy_fact((H@D),Vi,Vo) -->
  my_fuzzy_head((H@D),Vi,Vo).
% my_fuzzy_fact((H@D),Vi,Vo) -->
%   my_head(H,Vi,Vo),
%   my_fuzzy_rule_degree(D).
 
my_fuzzy_head((H@D),Vi,Vo) -->
  my_head(H,Vi,Vo),
  my_blanks_star,
  "@",
  my_blanks_star,
  my_number(D).
my_fuzzy_head((H@D),Vi,Vo) -->
  my_head(H,Vi,Vo),
  my_fuzzy_rule_degree(D).
  
my_fuzzy_rule_degree(D) -->
  my_blanks_star,
  "with",
  my_blanks_star,
  my_number(D).
% my_fuzzy_rule_degree(1.0) -->
%   [].

my_fuzzy_equation('$~'(L,R,D),Vi,Vo) -->
  my_fuzzy_atom('$~'(L,R,D),Vi,Vo),
  push_syntax_error(['Invalid fuzzy equation. Expected equality symbol (=)'],Old),
  my_blanks_star,
  "=",
  pop_syntax_error(Old),
  my_blanks_star,
  my_positive_number(N),
  !,
  {check_degree(N),
   ensure_float(N,D)}. % Convert to float
my_fuzzy_equation(Equation,V,V) -->
  {fuzzy_relation(Relation),
   Relation\=='~',
   atom_codes(Relation,StrRelation)},
  my_constant(L),
  my_blanks_star,
  StrRelation,
  my_blanks_star,
  my_constant(R),
  push_syntax_error(['Invalid fuzzy equation. Expected equality symbol (=)'],Old),
  my_blanks_star,
  "=",
  pop_syntax_error(Old),
  my_blanks_star,
  my_positive_number(N),
  !,
  {check_degree(N),
   ensure_float(N,D),
   atom_concat('$',Relation,DRelation),
   Equation=..[DRelation,L,R,D]
  }.
   
   
check_degree(D) :-
  D>=0.0,
  D=<1.0,
  !.
check_degree(D) :-
  my_raise_exception(generic,syntax(['Non valid approximation degree ',D,'. Must be between 0.0 and 1.0.']),[]).

  
my_fuzzy_basic_literal(N,V,V) --> % Unification degree for 'with' modifier
  my_possibly_fractional_positive_number(N).
my_fuzzy_basic_literal(Rule,Vi,Vo) -->
  my_fuzzy_equation(Rule,Vi,Vo).
my_fuzzy_basic_literal('~~'(L,R),Vi,Vo) --> % Weak unification
  my_variable_or_constant(L,Vi,Vo1),
  my_blanks_star,
  "~~",
  my_blanks_star,
  my_variable_or_constant(R,Vo1,Vo).
%my_fuzzy_basic_literal('~'(L,R),Vi,Vo) --> % Similarity operator
my_fuzzy_basic_literal(SimOperator,Vi,Vo) --> % Similarity operator
  my_variable_or_constant(L,Vi,Vo1),
  my_blanks_star,
  {fuzzy_relation(Relation),
   atom_codes(Relation,StrRelation)},
%  "~",
  StrRelation,
  my_blanks_star,
  my_variable_or_constant(R,Vo1,Vo),
  {SimOperator=..[Relation,L,R]}.
my_fuzzy_basic_literal('~'(L,R,D),Vi,Vo) --> % Fuzzy relation
  "~",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_variable_or_constant(L,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_variable_or_constant(R,Vo1,Vo2),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_variable_or_number(D,Vo2,Vo),
  my_blanks_star,
  ")".
my_fuzzy_basic_literal(approx_degree(G,D),Vi,Vo) --> % approx_degree
  "approx_degree",
  my_blanks_star,
  "(",
  my_blanks_star,
  my_basic_literal(G,Vi,Vo1),
  my_blanks_star,
  ",",
  my_blanks_star,
  my_variable_or_number(D,Vo1,Vo),
  my_blanks_star,
  ")".
my_fuzzy_basic_literal('$t_norm'(R,Xs,X),Vi,Vo) --> % t_norm
  "'$t_norm'",
  my_blanks_star,
  "(",
  my_blanks_star,
  push_syntax_error(['Expecting a proximity relation name'],Old0),
  my_user_atom(R),
  pop_syntax_error(Old0),
  my_blanks_star,
  my_comma,
  my_blanks_star,
  push_syntax_error(['The second argument of ''$t_norm''/3 must be a list of variables or numbers between 0 and 1'],Old1),
  my_list_of([my_variable,my_number],Xs,Vi,Vo1),
  my_blanks_star,
  pop_syntax_error(Old1),
  my_blanks_star,
  my_comma,
  my_blanks_star,
  my_term_of([my_variable,my_number],X,Vo1,Vo),
  my_blanks_star,
  my_right_parenthesis,
  !.
  
  

/*********************************************************************/
/* Primitives                                                        */
/*********************************************************************/

% % Debug:
% compute_fuzzy_primitive('$sim'('~',P,Q,D),_R) :-
%   write_log_list([P,'~',Q,'=',D]).

% Fuzzy primitive computation: '~'
%compute_fuzzy_primitive('$~'(P,Q,DN),_R) :-
compute_fuzzy_primitive(FuzzyEquation,CId,_R) :-
  FuzzyEquation=..[DRelation,P,Q,DN],
  atom_concat('$',Relation,DRelation),
  fuzzy_relation(Relation),
  ensure_float(DN,D),
  FFuzzyEquation=..[DRelation,P,Q,_C,D],
  datalog(FFuzzyEquation,_NVs,_RId,CId,_Ls,_FId,_Rs). 
  %!.
compute_fuzzy_primitive('$t_norm'(Relation,List,Degree),_CId,_R) :-
  t_norm_composition(Relation,List,Degree),
  over_lambda_cut(Degree).
compute_fuzzy_primitive('$over_lambda_cut'(Degree),_CId,_R) :-
  over_lambda_cut(Degree).
compute_fuzzy_primitive('$unify_arguments'(Arguments),CId,_R) :-
  unify_arguments(Arguments,CId).
% compute_fuzzy_primitive('~'(X,Y,D),_R) :-
%   unify(X,Y,D).

% Fuzzy built-in relations:
% approx_degree(G,D) :-
%   G=..[_|DArgs],
%   append(_,[D],DArgs).

multivalued_predicate_function('$sim'(_,_,_,_)).

:- dynamic(listing_hide_identifier/1).
%listing_hide_identifier('~').

% For safety checking:
my_fuzzy_builtin_pred('~~'/2).
%my_fuzzy_builtin_pred(approx_degree/2).

% Sorting first descending by degree, and ascending by the other arguments:
sort_fuzzy_entries([],[]) :-
  !.
sort_fuzzy_entries(L,OL) :-
  L=[T|_],
  fuzzy_spec_order(T,Os),
  my_mergesort(L,my_multi_key_compare(Os,fuzzy_tuple_order_sel),OL).
  
fuzzy_spec_order(T,[d|OrdSpecs]) :-
  functor(T,_,A),
  A>0,
  !,
  A1 is A-1, % WARNING. A3
  length(OrdSpecs,A1),
  my_map_1('='(a),OrdSpecs).
fuzzy_spec_order(_T,[d]).
  
fuzzy_tuple_order_sel(T,[D|Hs]) :- % WARNING:   univ_head(F,As,Restricted,T),
  T=..[_|As],
  append(Hs,[D],As).


/*********************************************************************/
/* Fuzzy queries                                                     */
/*********************************************************************/

build_fuzzy_query(Query,FQuery) :-
  system_mode(fuzzy),
  !,
%  Query=..[F|Args],
  univ_head(F,Args,Restricted,Query),
  number_of_fuzzy_extra_args(N),
  length(ExtraArgs,N),
  append(Args,ExtraArgs,FArgs),
%  FQuery=..[F|FArgs].
  univ_head(F,FArgs,Restricted,FQuery).
build_fuzzy_query(Query,Query).

number_of_fuzzy_extra_args(3) :-
  weak_unification(a3),
  !.
number_of_fuzzy_extra_args(1).


/*********************************************************************/
/* Properties of relations                                           */
/*********************************************************************/

%%%%% Rules for properties %%%%%

reflexive_relation_rule(DRel,(Head :- Goal1 ; Goal2),['X'=X,'Y'=Y,'_D1'=D1,'_D2'=D2]) :-
  Head  =.. [DRel,X,X,1.0],
  Goal1 =.. [DRel,X,Y,D1],
  Goal2 =.. [DRel,Y,X,D2].
% reflexive_relation_rule(DRel,(Head :- Goal1 ; Goal2),['_X'=X,'_Y'=Y,'_D1'=D1,'_D2'=D2]) :-
%   atom_concat('$',Rel,DRel),
%   Head =.. [Rel,X,X,1.0],
%   Goal1 =.. [Rel,X,Y,D1],
%   Goal2 =.. [Rel,Y,X,D2].

symmetric_relation_rule(DRel,(Head :- Body),['X'=X,'Y'=Y,'D'=D]) :-
  Head =.. [DRel,X,Y,D],
  Body =.. [DRel,Y,X,D].
% symmetric_relation_rule(DRel,(Head :- Body),['X'=X,'Y'=Y,'D'=D]) :-
%   atom_concat('$',Rel,DRel),
%   Head =.. [Rel,X,Y,D],
%   Body =.. [Rel,Y,X,D].
  
% $call is host-unsafe:
%transitive_relation_rule(DRel,(Head :- Goal1,'$call'(X\==Z),Goal2,'$call'(Z\==Y),'$call'(X\==Y),'$t_norm'(Rel,[D1,D2],D)),['X'=X,'Y'=Y,'Z'=Z,'D'=D,'D1'=D1,'D2'=D2]) :-
% \= does not work for predicate similarities, as p/0~q/0=0.5 
%transitive_relation_rule(DRel,(Head :- Goal1,X\=Z,Goal2,Z\=Y,X\=Y,'$t_norm'(Rel,[D1,D2],D)),['X'=X,'Y'=Y,'Z'=Z,'D'=D,'D1'=D1,'D2'=D2]) :-
transitive_relation_rule(DRel,(Head :- Goal1,X\==Z,Goal2,Z\==Y,X\==Y,'$t_norm'(Rel,[D1,D2],D)),['X'=X,'Y'=Y,'Z'=Z,'D'=D,'D1'=D1,'D2'=D2]) :-
  Head  =.. [DRel,X,Y,D],
  Goal1 =.. [DRel,X,Z,D1],
  Goal2 =.. [DRel,Z,Y,D2],
  atom_concat('$',Rel,DRel).

intransitive_relation_rule(DRel,(Head :- Goal),['X'=X,'Y'=Y,'D'=D]) :-
  atom_concat('$',Rel,DRel),
  Head =.. [Rel,X,Y,D],
  Goal =.. [DRel,X,Y,D].

%%%%% Setting/Unsetting of rules for properties %%%%%

% Assert a rule specifying the reflexive property of a given relation
set_reflexive_relation(DRel) :-
  reflexive_relation_rule(DRel,Rule,NVs),
  set_rule(Rule,NVs).

% Retract the rule specifying the reflexive property of a given relation
del_reflexive_relation(DRel) :-
  reflexive_relation_rule(DRel,Rule,NVs),
  del_rule(Rule,NVs).

% Assert a rule specifying the symmetric property or a given relation
set_symmetric_relation(DRel) :-
  symmetric_relation_rule(DRel,Rule,NVs),
  set_rule(Rule,NVs).

% Retract the rule specifying the symmetric property of a given relation
del_symmetric_relation(DRel) :-
  symmetric_relation_rule(DRel,Rule,NVs),
  del_rule(Rule,NVs).

% Assert a rule specifying the transitive property or a given relation
set_transitive_relation(DRel) :-
  transitive_relation_rule(DRel,Rule,NVs),
  set_rule(Rule,NVs).
  
% Retract the rule specifying the transitive property of a given relation
del_transitive_relation(DRel) :-
  transitive_relation_rule(DRel,Rule,NVs),
  del_rule(Rule,NVs).
  
% Assert a rule specifying the intransitive property or a given relation
set_intransitive_relation(DRel) :-
  intransitive_relation_rule(DRel,Rule,NVs),
  set_rule(Rule,NVs).
  
% Retract the rule specifying the transitive property of a given relation
del_intransitive_relation(DRel) :-
  intransitive_relation_rule(DRel,Rule,NVs),
  del_rule(Rule,NVs).

set_rule(Rule,NVs) :-
  (exists_rule(Rule) -> true ; processC(assert,[Rule],NVs,yes)).
  
del_rule(Rule,NVs) :-
  (exists_rule(Rule) -> processC(retract,[Rule],NVs,yes) ; true).
  
exists_rule(Rule) :-
  get_filtered_source_dlrules(rule,Rule,[],[_|_]).
  
/*********************************************************************/
/* T-Closure                                                       */
/*********************************************************************/

% Assert a rule specifying the t-closure of the relation $Rel as the relation Rel
set_t_closure_relation(Rel) :-
  t_closure_rule(Rel,Rule,NVs),
  set_rule(Rule,NVs).

% Retract the rule specifying the t_closure of the transitive property for a given relation
del_t_closure_relation(Rel) :-
  t_closure_rule(Rel,Rule,NVs),
  del_rule(Rule,NVs).
  
t_closure_rule(Rel,(Head :- Body),['X'=X,'Y'=Y,'D'=D,'D1'=D1]) :-
  Head =.. [Rel,X,Y,D],
  atom_concat('$',Rel,DRel),
  DGoal =.. [DRel,X,Y,D1],
  Body = group_by(DGoal,[X,Y],D=max(D1)).

/*********************************************************************/
/* Similarity relation                                               */
/*********************************************************************/

set_similarity_relation(Rel) :-
  similarity_relation_rule(Rel,Rule,NVs),
  set_rule(Rule,NVs).
  
similarity_relation_rule(Rel,('$sim'(Rel,X,Y,D) :- Body),['X'=X,'Y'=Y,'D'=D]) :-
  Body =.. [Rel,X,Y,D].


/*********************************************************************/
/* t-norm                                                            */
/*********************************************************************/

set_t_norm(Relation,Value) :-
  representative_t_norm(Value,TNorm),
  del_tuple(t_norm(Relation,_)),
  set_tuple(t_norm(Relation,TNorm)),
  set_valid_t_closure_comp(Relation,TNorm).
  
% If the the transitive property is set, only the t-norm goedel is supported by the Datalog t-closure computation (otherwise, the Floyd-Warshall algorithm must be applied)
set_valid_t_closure_comp(Relation,TNorm) :-
  TNorm\==goedel,
  fuzzy_relation_property(Relation,transitive),
  t_closure_comp(Relation,datalog),
  process_fuzzy_command(t_closure_comp,[Relation,prolog],[],_),
  !.
set_valid_t_closure_comp(_Relation,_TNorm).
% set_valid_t_closure_comp(Relation,_TNorm) :-
%   set_flag(t_closure_comp,Relation,TNorm).

t_norm_values(ValidValues) :-
  ValidValues=[yes,goedel,min,product,luka,lukasiewicz,drastic,nilpotent,hamacher].

% t-norm binary operator: Given a t-norm and two degrees, return the computed result degree
t_norm_op(goedel,D1,D2,D) :-
  D is min(D1,D2).  
t_norm_op(min,D1,D2,D) :-    % 'min' and 'goedel' are synonymous
  t_norm_op(goedel,D1,D2,D).
t_norm_op(product,D1,D2,D) :-
%  D is D1*D2.  
  clpq_solve(D1*D2,D).
t_norm_op(luka,D1,D2,D) :-
%  D is max(0,D1+D2-1.0).
  clpq_solve(D1+D2-1.0,DT),
  D is max(0,DT).
t_norm_op(lukasiewicz,D1,D2,D) :-
  t_norm_op(luka,D1,D2,D).
t_norm_op(drastic,D1,D2,D) :-
  (D1=1.0, D=D2, !) ; (D2=1.0, D=D1).
t_norm_op(nilpotent,D1,D2,D) :-
  (D1+D2>1.0, D is min(D1,D2), !) ; (D1+D2=<1.0, D=0.0).
t_norm_op(hamacher,D1,D2,D) :-
%  (D1=0.0,D2=0.0,D=0.0, !) ; (D1+D2>0.0, D is D1*D2/(D1+D2-D1*D2)).
  (D1=0.0,D2=0.0,D=0.0, !) ; (D1+D2>0.0, clpq_solve(D1*D2/(D1+D2-D1*D2),D)).
  
clpq_solve(E,R) :-
  clpq:{Q=E},
  !,
  q_to_number(Q,R).
% For CLPQ solvers that do not admit decimals, such as SWI-Prolog's
clpq_solve(E,R) :-
  R is E.
  
q_to_number(rat(X,Y),N) :-
  !,
  N is X/Y.  
q_to_number(N,N).
 
q_to_int(rat(X,Y),I) :-
  !,
  I is integer(X/Y).  
q_to_int(N,I) :-
  I is integer(N). % WARNING: This discards decimals
 
number_to_q(X,rat(X,1)) :- !.
number_to_q(X,X).
 
% Representative value for a t-norm set of synonyms
representative_t_norm(yes,goedel) :- !.
representative_t_norm(min,goedel) :- !.
representative_t_norm(luka,lukasiewicz) :- !.
representative_t_norm(X,X).
  
/*********************************************************************/
/* Translating fuzzy rules                                           */
/*********************************************************************/
% Add an additional argument for the approximation degree
% Expand rule with lambda cut, weak unifications and
% approximation degree computations
% The translation depends on the context (hypothetical implications)
 
translate_fuzzy_rule_list([],[],_CId,[],[],_Compiled).
translate_fuzzy_rule_list([RNVs|RNVsList],TRNVsList,CId,[IArgs|IArgsListi],IArgsListo,Compiled) :-
  translate_fuzzy_rule(RNVs,TRNVsList1,CId,IArgs,IArgsList1,Compiled),
  translate_fuzzy_rule_list(RNVsList,TRNVsList2,CId,IArgsListi,IArgsList2,Compiled),
  append(TRNVsList1,TRNVsList2,TRNVsList),
  append(IArgsList1,IArgsList2,IArgsListo).

translate_fuzzy_rule(RuleNVs,[RuleNVs],_CId,IArgs,[IArgs],_Compiled) :-
  \+ system_mode(fuzzy),
  !.
translate_fuzzy_rule((Rule,NVs),[(Rule,NVs)],_CId,IArgs,[IArgs],true) :-
  % A fuzzy equation is not translated, and its t-closure becomes non-updated
  functor(Rule,DRelName,3),
  atom_concat('$',RelName,DRelName),
  fuzzy_relation(RelName),
  retractall(t_closure_updated(RelName)),
  !.
translate_fuzzy_rule(RuleNVs,FRuleNVss,CId,IArgs,IArgsList,Compiled) :-
  update_fuzzy_relation('~',CId),
  translate_expand_fuzzy_rule(RuleNVs,FRuleNVss,CId,IArgs,IArgsList,Compiled),
  clear_et. % This should be incrementally cleared
  
% translate_expand_fuzzy_rule((-(_Head),_NVs),_,_,_,_,_Compiled) :-
%   my_raise_exception(generic,syntax(['Unsupported fuzzy negative literals.']),[]).
translate_expand_fuzzy_rule(((Head:-Body),NVs),RuleNVsso,CId,IArgs,IArgsList,true) :-
  !,
  translate_fuzzy_atom(Head,THead,Cin,Cout,RuleDegree,UnifDegree),
  linearize_head(THead,FHead,UnifGoals,NVs,LNVs),
  translate_fuzzy_body(Body,TBody,CBin,Cout,Degrees),
  remove_crisp_degrees([UnifDegree,RuleDegree|Degrees],FuzzyDegrees),
  add_ctrs_and_degree_var_names(NVs,Cin,Cout,UnifDegree,NNVs),
  append(NNVs,LNVs,ANVs),
  append_goals_list([UnifGoals,TBody],FBody),
  expand_rules([[(FHead:-FBody),ANVs,Cin,CBin,FuzzyDegrees]],RuleNVsso,CId,[IArgs],IArgsList).
% translate_expand_fuzzy_rule(('$~'(X,Y,D),NVs),[('$~'(X,Y,D),NVs)],IArgs,[IArgs],true) :-
%   !.
translate_expand_fuzzy_rule((Head,NVs),RuleNVsso,CId,IArgs,IArgsList,Compiled) :-
  linearize_head(Head,FHead,UnifGoals,NVs,LNVs),
  translate_expand_fuzzy_rule(((FHead:-UnifGoals),LNVs),RuleNVsso,CId,IArgs,IArgsList,Compiled).

add_ctrs_and_degree_var_names(NVs,_,_,UnifDegree,['_D'=UnifDegree|NVs]) :-
  weak_unification(a1),
  !.
add_ctrs_and_degree_var_names(NVs,Cin,Cout,UnifDegree,['_Cin'=Cin,'_Cout'=Cout,'_D'=UnifDegree|NVs]).
%  weak_unification(a3).

translate_fuzzy_atom((X~~Y),'$unify_arguments'([[X,Y,UnifDegree]]),Cin,Cin,1.0,UnifDegree) :- % WARNING: Link CId with the corresponding context in this and next rules
  weak_unification(a1),
  !.
translate_fuzzy_atom((X~~Y),'$unify_arguments'([[X,Y,Cin,Cout,UnifDegree]]),Cin,Cout,1.0,UnifDegree) :-
%  weak_unification(a3),
  !.
% translate_fuzzy_atom(approx_degree(G,D),approx_degree(TG,D),AtomDegree,D) :-
%   !,
%   translate_fuzzy_atom(G,TG,AtomDegree,D).
translate_fuzzy_atom(FAtom,FRelation,Cin,Cin,1.0,Degree) :-
  FAtom=..[Relation,X,Y],
  fuzzy_relation(Relation),
  FRelation=..[Relation,X,Y,Degree],
  !.
translate_fuzzy_atom(Atom,Atom,Cin,Cin,1.0,1.0) :-
  functor(Atom,F,A),
  [F,A] \= ['not',1], % Negation is translated
  [F,A] \= ['-',1],   % Restricted predicates are translated
%  F/A \= '='/2,   % Equality is translated
  (my_builtin_pred(F/A) ; F/A='~'/3 ; F/A='$~'/3 ), % These are not translated
  !.
translate_fuzzy_atom(Atom,FAtom,Cin,Cout,AtomDegree,UnifDegree) :-
  (Atom=(HAtom@AtomDegree),
   !
  ; 
   Atom=HAtom,
   AtomDegree=1.0),
  translate_fuzzy_atom(HAtom,FAtom,Cin,Cout,UnifDegree).
  
% translate_fuzzy_atom('-'(Atom),'-'(FAtom),Cin,Cout,UnifDegree) :-
%   translate_fuzzy_atom(Atom,FAtom,Cin,Cout,UnifDegree).
translate_fuzzy_atom(not Atom,not(FAtom,UnifDegree),Cin,Cout,UnifDegree) :-
  translate_fuzzy_atom(Atom,FAtom,Cin,Cout,_AtomUnifDegree).
translate_fuzzy_atom(Atom,FAtom,Cin,Cout,UnifDegree) :-
%  Atom=..[F|Args],
  univ_head(F,Args,Restricted,Atom),
  (weak_unification(a1)
   ->
    append(Args,[UnifDegree],FArgs)
   ;
    append(Args,[Cin,Cout,UnifDegree],FArgs)
  ),
%  FAtom=..[F|FArgs].
  univ_head(F,FArgs,Restricted,FAtom).

translate_fuzzy_body((G,Gs),(TG,TGs),Cin,Cout,[Degree|Degrees]) :-
  !,
  translate_fuzzy_goal(G,TG,Cin,Cin1,Degree),
  translate_fuzzy_body(Gs,TGs,Cin1,Cout,Degrees).
translate_fuzzy_body(G,TG,Cin,Cout,[Degree]) :-
  translate_fuzzy_goal(G,TG,Cin,Cout,Degree).

translate_fuzzy_goal(approx_degree(G,Degree),approx_degree(TG,Degree),Cin,Cout,1.0) :-
  !,
  translate_fuzzy_atom(G,TG,Cin,Cout,_GDegree,Degree). 
translate_fuzzy_goal('=>'(F,G),'=>'(F,TG),Cin,Cout,Degree) :- % Intuitionistic assumption
  translate_fuzzy_atom(G,TG,Cin,Cout,_GDegree,Degree). 
translate_fuzzy_goal(G,TG,Cin,Cout,Degree) :-
  translate_fuzzy_atom(G,TG,Cin,Cout,_GDegree,Degree). % Goals could be annotated with degrees too, if considering an approach somewhat related to GALP

% linearize_head(+Head,-LHead,-UnifGoals (conjunction),+NVs,-LNVs (new variable names due to linearization))
linearize_head(Head,Head,true,NVs,NVs) :- % For testing with no linearization
 !.
linearize_head(THead,FHead,UnifGoals,NVs,LNVs) :-
%  THead=..[F|Args],
  univ_head(F,Args,Restricted,THead),
  linearize_head_arguments(Args,LArgs,[],Vs,true,UnifGoals),
  assign_variable_names(Vs,NVs,LNVs),
%  FHead=..[F|LArgs].
  univ_head(F,LArgs,Restricted,FHead).

linearize_head_arguments([],[],Vs,Vs,Eqs,Eqs).
linearize_head_arguments([V|Args],[V1|LArgs],Vsi,Vso,Eqsi,Eqso) :-
  my_member_var(V,Vsi),
  !,
  append_goals(Eqsi,(V~V1),Eqsi1),
  linearize_head_arguments(Args,LArgs,[V|Vsi],Vso,Eqsi1,Eqso).
linearize_head_arguments([V|Args],[V|LArgs],Vsi,Vso,Eqsi,Eqso) :-
  linearize_head_arguments(Args,LArgs,[V|Vsi],Vso,Eqsi,Eqso).

remove_crisp_degrees([],[]).
remove_crisp_degrees([One|Degrees],FDegrees) :-
  (One==1.0 ; One==1),
  !,
  remove_crisp_degrees(Degrees,FDegrees).
remove_crisp_degrees([Degree|Degrees],[Degree|FDegrees]) :-
  remove_crisp_degrees(Degrees,FDegrees).

  
/*********************************************************************/
/* Solving                                                           */
/*********************************************************************/

/* 
   Adjusting the approximation degree when computing restricted
   predicates.
*/
  
adjust_fuzzy_approx_degree(Query,CId) :-
  Query=..[F|Args],
  append(FArgs,[Degree],Args),
  setof(Degree,Ids^It^et(-(Query),Ids,CId,It),ResDegrees),
  setof(Degree,Ids^It^et(Query,Ids,CId,It),PosDegrees),
  append(_,[MaxResDegree],ResDegrees),
  append(_,[MaxPosDegree],PosDegrees),
  et_remove_less_than_degree(-(Query),CId,MaxResDegree),
  et(Query,Ids,CId,It),
  my_idx_retract(et(Query,Ids,CId,It)),
  (Degree>=MaxPosDegree,
   % NewDegree is Degree-MaxDegree,
   clpq_solve(Degree-MaxResDegree,NewDegree), % Do not lose precision due to floating point calculations
   NewDegree>0
    -> 
     append(FArgs,[NewDegree],NewArgs),
     NewQuery=..[F|NewArgs],
     my_idx_assertz(et(NewQuery,Ids,CId,It))
    ;
     true
  ),
  fail.
adjust_fuzzy_approx_degree(_Query,_CId).

% Remove entries in et with degree less than the maximum
et_remove_less_than_degree(-(Query),CId,MaxResDegree) :-
  Query=..[_F|FArgs],
  append(_,[Degree],FArgs),
  et(-(Query),Ids,CId,It),
  (Degree<MaxResDegree
   ->
    my_idx_retract(et(-(Query),Ids,CId,It))
   ;
    true),
  fail.
et_remove_less_than_degree(_Query,_CId,_MaxResDegree).



/*********************************************************************/
/* Writing and Formatting                                            */
/*********************************************************************/

/* Solutions as displayed after a query */

format_fuzzy_solutions(Ss,FSs) :-
  remove_subsumed_fuzzy_entries(Ss,RSs),
  sort_fuzzy_entries(RSs,OL),
  (development(off)
   ->
    format_fuzzy_solution_list(OL,FSs)
   ;
    FSs=OL).
  
format_fuzzy_solution_list([],[]).
format_fuzzy_solution_list([S|Ss],[FS|FSs]) :-
  format_fuzzy_solution(S,FS),
  format_fuzzy_solution_list(Ss,FSs).
  
format_fuzzy_solution(S,FS) :-
%  S=..[F|Args],
  univ_head(F,Args,Restricted,S),
  Args\==[],
  append(FArgs,[Degree],Args),
  float(Degree),
  FDegree=Degree, 
  !,
%  T=..[F|FArgs],
  univ_head(F,FArgs,Restricted,T),
  (weak_unification(a3)
   ->
    append(FTArgs,[_Cin,_Cout],FArgs),
%    FT=..[F|FTArgs]
    univ_head(F,FTArgs,Restricted,FT)
   ;
    FT=T
  ),
  (FDegree=:=1.0
   ->
    FS=FT
   ; 
%     (development(off)
%      ->
    FS=with(FT,FDegree)
%      ;
%       FS='@'(T,FDegree) 
%     )
  ).
format_fuzzy_solution(S,S).


remove_subsumed_fuzzy_entries(L,L) :-
  fuzzy_answer_subsumption(off),
  !.
remove_subsumed_fuzzy_entries(L,RL) :-
  my_sort(L,SL),
  remove_subsumed_fuzzy_entry_list(SL,RL).

remove_subsumed_fuzzy_entry_list([],[]).
remove_subsumed_fuzzy_entry_list([E],[E]).
remove_subsumed_fuzzy_entry_list([E1,E2|Es],REs) :-
%  E1=..[F|AD1s],
  univ_head(F,AD1s,Restricted,E1),
%  E2=..[F|AD2s],
  univ_head(F,AD2s,Restricted,E2),
  append(As,[D1],AD1s),
  append(As,[D2],AD2s),
  (D1>=D2 -> E=E1 ; E=E2),
  !,
  remove_subsumed_fuzzy_entry_list([E|Es],REs).
remove_subsumed_fuzzy_entry_list([E|Es],[E|REs]) :-
  remove_subsumed_fuzzy_entry_list(Es,REs).


/* Rules as displayed in listings */

format_fuzzy_rules(Ss,Ss) :-
   development(on),
  !.
format_fuzzy_rules(Ss,FSs) :-
  format_fuzzy_rules_aux(Ss,FSs).
  
format_fuzzy_rules_aux([],[]).
format_fuzzy_rules_aux([(R,NVs)|Rs],[(FR,NVs)|FRs]) :-
  format_fuzzy_rule(R,FR),
  format_fuzzy_rules_aux(Rs,FRs).
  
format_fuzzy_rule(T,T) :-
  (var(T) ; atomic(T)),
  !.
format_fuzzy_rule(((H@D):-B),(H:-FB with D)) :-
  !,
  format_fuzzy_rule(B,FB).
format_fuzzy_rule((H@D),(H with D)) :-
  !.
format_fuzzy_rule(T,RT) :-
%  T =.. [F|As],
  univ_head(F,As,Restricted,T),
  format_fuzzy_rule_list(As,RAs),
%  RT =.. [F|RAs].
  univ_head(F,RAs,Restricted,RT).
    
format_fuzzy_rule_list([],[]) :-
  !.
format_fuzzy_rule_list([T|Ts],[RT|RTs]) :-
  format_fuzzy_rule(T,RT), 
  format_fuzzy_rule_list(Ts,RTs).


% Write Fuzzy equations and weighted rules

% Fuzzy equations
%write_fuzzy_equation(('$~'(P,Q,D),_NVs),I) :-
write_fuzzy_datalog_rule((with(':-'(H,B),Degree),NVs),Pr,M,Pa,I) :-
  !,
  add_displayed_parenthesis(':-',Pr,M,Pa,Pa1),
  write_datalog_rule_no_dot((':-'(H,B),NVs),1300,M,Pa1,I),
  write_log_list([' with ',Degree]),
  write_closing_parenthesis(Pa,Pa1).
write_fuzzy_datalog_rule((with(Fact,Degree),NVs),_Pr,M,Pa,I) :-
  !,
  write_datalog_rule_no_dot((Fact,NVs),1300,M,Pa,I),
  write_log_list([' with ',Degree]).
write_fuzzy_datalog_rule((DFuzzyEquation,_NVs),_Pr,_M,Pa,I) :-
  development(off),
  DFuzzyEquation=..[DRelation,P,Q,D],
  atom_concat('$',Relation,DRelation),
  fuzzy_relation(Relation),
  !,
  FuzzyEquation=..[Relation,P,Q],
  write_indent(I),
  write_opening_parentheses(Pa),
  write_log_list([FuzzyEquation=D]).

  
/*********************************************************************/
/* Updating Fuzzy expansions                                         */
/*********************************************************************/

update_fuzzy_expansion(AssertRetract,Equation,CId) :-
  Equation=..[DRel,N_A,_,_D],
  dollar_fuzzy_relation(DRel,Rel),
  !,
  (fuzzy_expansion(des)
   ->
    Eq=..[Rel,L,R,CId,D],
    asserta(Eq), % Rel might not been defined before
    retract(Eq),
    findall((Rel,L,R,D),(Eq,L\==R),ISimList), % t-closure before removing the equation
    force_update_fuzzy_relation(Rel,CId), 
    findall((Rel,L,R,D),(Eq,L\==R),RSimList), % t-closure after removing the equation
    (AssertRetract==retract
     ->
      my_set_diff(ISimList,RSimList,SimList),   % Entries in fuzzy relation which are no longer valid 
      retract_simlist_fuzzy_rules(SimList)      % WARNING: When retracting equations along hypotheses, should PDG be rebuilt?
     ;             % assert
      my_set_diff(RSimList,ISimList,SimList),   % New entries in fuzzy relation 
      add_e_clause_list(SimList,Rules),         % Add expanded clauses
      update_stratification_add_rules(Rules)    % Update stratification
    )
  ;
    N_A=Name/Arity,
    get_filtered_source_dlrules(namearity,Name/Arity,[],DLs), 
    source_dlrule_to_ruleNVs_list(DLs,RNVs),
    ruleNVs_to_rule_list(RNVs,Rules), 
    retract_source_rule_list(Rules,Error), % Retract all the rules for Name/Arity
    force_update_fuzzy_relation(Rel,CId),  % t-closure after removing the equation
    assert_rules(RNVs,CId,datalog,[no_safety],CRNVs,_ODLIds,_Unsafe,Error), % Reassert the source rules for Name/Arity
    ruleNVs_to_rule_list(CRNVs,CRs),
    update_stratification_add_rules(CRs)
  ).
update_fuzzy_expansion(_,_,_).

% void(CId,Error):-
% get_filtered_source_dlrules(namearity,p/1,[],DLs), source_dlrule_to_ruleNVs_list(DLs,RNVs),ruleNVs_to_rule_list(RNVs,Rules), retract_source_rule_list(Rules,Error), 
%   assert_rules(RNVs,CId,datalog,[no_safety],_CRNVs,_ODLIds,_Unsafe,Error).


retract_simlist_fuzzy_rules([]).
retract_simlist_fuzzy_rules([Sim|SimList]) :-
  build_e_clause(Sim,ExpClause), % This fails if Sim does not relate predicates (identified with arities)
  !,
  retract(datalog(ExpClause,_NVs,_RId,_CId,_Ls,_FId,_C)),
  retract_simlist_fuzzy_rules(SimList).
% Sim does not match a valid proximity equation between predicates (identified with arities)
retract_simlist_fuzzy_rules([_Sim|SimList]) :-
  !,
  retract_simlist_fuzzy_rules(SimList).
  
/*********************************************************************/
/* Updating e-clauses                                                */
/*********************************************************************/

% update_e_clauses(_Rel,_CId) :-
%   fuzzy_expansion(bpl),
%   !.
% update_e_clauses(Rel,CId) :-
%   Eq=..[Rel,L/A,R/A,CId,D],
%   findall((Rel,L/A,R/A,D),(Eq,L\==R),SimList),
%   !,
%   add_e_clause_list(SimList,Rules),
%   update_stratification_add_rules(Rules).
  
add_e_clause_list([],[]).
add_e_clause_list([Sim|SimList],[Rule|Rules]) :-
  add_e_clause(Sim,Rule),
  !,
  add_e_clause_list(SimList,Rules).
add_e_clause_list([_Sim|SimList],Rules) :-
  add_e_clause_list(SimList,Rules).
  
add_e_clause((Rel,L/A,R/A,Degree),ExpClause) :-
  build_e_clause((Rel,L/A,R/A,Degree),ExpClause),
  % Do not add the rule if already added
  (datalog(ExpClause,_NVs,_RId,_CId,_Ls,_FId,_C)
   ->
    !,
    fail
   ;
%    term_variables([RDegree,LDegree|HeadVars],Vs),
    term_variables(ExpClause,Vs),
    my_var_name_list(Vs,[],NVs),
    my_current_datetime(DT),
    DL=datalog(ExpClause,NVs,_,[],[],asserted(DT),compiled),
    assertz_dlrule(DL)
  ).

% Build the DES expansion of a clause with respect to a given fuzzy equation
build_e_clause((Rel,L/A,R/A,Degree),ExpClause) :-
  ExpClause = (Head :- Body),
  arity_fuzzy_arity(A,FA),
  FA1 is FA-1,
  length(HeadVars,FA1), % This contains Cin and Cout variables, if applicable (only for a2 -not yet implemented- and a3)
  append(HeadVars,[LDegree],LArgs),
  append(HeadVars,[RDegree],RArgs),
  Head    =.. [L|LArgs],
%  univ_head(L,LArgs,Restricted,Head), % Not needed: only for positive equations. 
  SimHead =.. [R|RArgs],
%  univ_head(R,RArgs,Restricted,SimHead), % Not needed: only for positive equations. 
  build_t_norm_goal(Rel,[Degree,RDegree,LDegree], LDegree, TNormGoal),
  append_goals_list(['$over_lambda_cut'(Degree),
                     SimHead,
                     TNormGoal],Body).
                     
           
/*********************************************************************/
/* Asserting fuzzy equations                                         */
/*********************************************************************/

% Assert a fuzzy equation, replacing existing equation (if any)
% If DL is not a fuzzy equation, fail

assert_dl_fuzzy_equation(DL) :-
  system_mode(fuzzy),
  DL=datalog(Eq,_NVs,_RId,CId,_Ls,_Fid,_C),
  Eq=..[DRel,F,T,_D],
  dollar_fuzzy_relation(DRel,Rel),
  !,
  update_fuzzy_relation(Rel,CId), % update the t-closure before adding the equation
  EEq=..[DRel,F,T,_],
  retractall(datalog(EEq,_,_,CId,_,_,_)),
  assertz_dlrule_aux(DL),
  update_fuzzy_expansion(assert,Eq,CId). % add expanded rules corresponding to the increased t-closure
  

/*********************************************************************/
/* Retracting fuzzy equations                                        */
/*********************************************************************/

retract_dl_fuzzy_equation(datalog(Eq,NVs,RId,CId,Ls,Fid,C)) :-
  Eq=..[DRelName,_,_,_],
  dollar_fuzzy_relation(DRelName),
  retract(datalog(Eq,NVs,RId,CId,Ls,Fid,C)),
  update_fuzzy_expansion(retract,Eq,CId).
  
          
/*********************************************************************/
/* Miscellanea                                                       */
/*********************************************************************/

pred_fuzzy_pred_list(Ps,Ps) :-
  \+ system_mode(fuzzy),
  !.
pred_fuzzy_pred_list(Ps,FPs) :-
  pred_fuzzy_pred_aux_list(Ps,FPs).

pred_fuzzy_pred_aux_list([],[]).
pred_fuzzy_pred_aux_list([P|Ps],[FP|FPs]) :-
  pred_fuzzy_pred(P,FP),
  pred_fuzzy_pred_aux_list(Ps,FPs).
  
pred_fuzzy_pred(N/A,N/FA) :-
  arity_fuzzy_arity(A,FA).
  

arity_fuzzy_arity(A,FA) :-
  weak_unification(a1),
  !,
  A #= FA-1.
arity_fuzzy_arity(A,FA) :-
  % For A2 (not yet implemented, and most likely, never) and A3
  A #= FA-3.

  
arc_fuzzy_arc_list([],[]).
arc_fuzzy_arc_list([Arc|Arcs],[FArc|FArcs]) :-
  arc_fuzzy_arc(Arc,FArc),
  arc_fuzzy_arc_list(Arcs,FArcs).
  
arc_fuzzy_arc(F+T,FF+FT) :-
  pred_fuzzy_pred(F,FF),
  pred_fuzzy_pred(T,FT).
arc_fuzzy_arc(F-T,FF-FT) :-
  pred_fuzzy_pred(F,FF),
  pred_fuzzy_pred(T,FT).


display_pdg_fuzzy((Ns,As)) :-
  system_mode(fuzzy),
  development(off), % Hide extra arguments in predicate arities
  pred_fuzzy_pred_list(FNs,Ns),
  arc_fuzzy_arc_list(FAs,As),
  display_pdg_aux((FNs,FAs)).


dollar_fuzzy_relation(DRelName) :-
  dollar_fuzzy_relation(DRelName,_RelName).
  
dollar_fuzzy_relation(DRelName,RelName) :-
  atom_concat('$',RelName,DRelName),
  fuzzy_relation(RelName).
  

/*********************************************************************/
/* BEGIN Code borrowed and adapted from Bousi~Prolog with permission */
/*       from the authors J. Gallardo and P. Julian                  */
/*********************************************************************/

/*********************************************************************/
/* Rules                                                             */
/*********************************************************************/

%% expand_rules(+Rules, -ExpandedRules, +CId, +InputArguments, -NewInputArguments)
%
%     Scans a list of Rules, each one of the form [Rule, NVs, DegreeVars],
%     where Rule is the bare rule, NVs are pairs (VariableName,Variable),
%     and DegreeVars are the variables for the approximation degree in 
%     the rule. 
%     Builds a translated rule for each rule and outputs it in the
%     ExpandedRules list. Rules that aren't clauses are copied as is
%     in the output list.
%     CId is the hypothetical context for which rules need to be expanded.
%     InputArguments is a list of position arguments which are known 
%     to be input. NewInputArguments is InputArguments extended with the 
%     possible new input arguments.

expand_rules([], [], _CId, [], []).
expand_rules([[Rule, NVs, Cin, CBin, DegreeVars]|MoreRules], ExpandedRules, CId, [IArgsi|IArgsListi], IArgsListo) :-
  rule_head(Rule,Head),
  rule_body(Rule,Body),
%  functor(Head, Functor, Arity), 
  pred_head(Functor/Arity, Head),
%  LArity is either Arity-1 or Arity-3 (algorithms A1 and A3, resp.), % For the symbol in the fuzzy equation, the degree (and block constraints for A3) does not count for the arity 
  arity_fuzzy_arity(LArity,Arity),
  (Functor == answer
   ->
    SimList=[[answer,1.0]] % No need for an ET look-up
   ;
    (fuzzy_expansion(bpl)
     ->
      % Extracts all the symbols that are similar to this rule's head
      findall([Sim,Degree],'~'(Functor/LArity,Sim/LArity,CId,Degree),RSimList),
      my_set_union([[Functor,1.0]],RSimList,SimList) % Just because there is no similarity equation for Functor
     ;
      % Only one rule in the expansion (rules for similarities are dealt when asserting/retracting equations)
      SimList=[[Functor,1.0]]
    )
  ),
  % Expands this rule using the resulting list
  my_set_union([Arity],IArgsi,IArgs), % The aproximation degree (last argument of the head) is always an output argument
  expand_rule(SimList, (Head :- Body), NVs, Cin, CBin, DegreeVars, ExpandedRules1, IArgs, IArgsListo1),
  % Expands the remaining rules
  expand_rules(MoreRules, ExpandedRules2, CId, IArgsListi, IArgsListo2),
  append(ExpandedRules1, ExpandedRules2, ExpandedRules),
  append(IArgsListo1, IArgsListo2, IArgsListo).


%% expand_rule(+SimDegrees, +Clause, +NVs, +RuleDegreeVars, -ExpandedClauses, +InputArguments, -NewInputArguments)
%
%     Internal predicate used to expand a single Clause. SimDegrees
%     must be a list consisting of lists with two items: a symbol and
%     an approximation degree.
%
%     @see expand_rules/2
%

expand_rule([], _Clause, _NVs, _Cin, _CBin, _RuleDegreeVars, [], _, []).
%expand_rule([[Symbol, Degree]|MoreSimDegrees], Clause, NVs, RuleDegreeVars, [(SimExpClause,SNVs)|MoreExpClauses], IArgs, [IArgs|IArgsList]) :-
expand_rule([[Symbol, Degree]|MoreSimDegrees], Clause, NVs, Cin, CBin, RuleDegreeVars, ExpClauses, IArgs, IArgsListo) :-
  Clause = (Head :- Body),
  ExpClause = (NewHead :- NewBody),
  % Creates the lists of variables and approximation degrees that
  % will be used in the weak unifications of each rule's argument
%  Head =.. [HeadFunctor|HeadArgsWithCtrsAndDegree],
  univ_head(HeadFunctor,HeadArgsWithCtrsAndDegree,Restricted,Head),
  length(HeadArgsWithCtrsAndDegree, ExtendedRuleArity),
  expanded_rule_arity(ExtendedRuleArity,RuleArity),
  length(HeadArgs, RuleArity),
  length(HeadVars, RuleArity),
  length(HeadDegrees, RuleArity),
  append(HeadArgs, CtrsAndDegreeVar, HeadArgsWithCtrsAndDegree),
  append(Ctrs,[HeadDegreeVar],CtrsAndDegreeVar),
  init_ctr_store(HeadFunctor,Cin),
  create_weak_unification_goals(HeadVars, HeadArgs, Ctrs, Cin, CBin, HeadDegrees, UnificationGoals),
  % Builds the new rule's head with the symbol that is similar to the
  % original functor and the variables of the HeadVars list
  append(HeadVars, CtrsAndDegreeVar, NewHeadArgsWithCtrsAndDegree),
%  NewHead =.. [Symbol|NewHeadArgsWithCtrsAndDegree],
  univ_head(Symbol,NewHeadArgsWithCtrsAndDegree,Restricted,NewHead),
  build_unify_arguments_goal(UnificationGoals,UnifyArgumentsGoal),
  (Degree < 1.0
   ->
    (fuzzy_expansion(bpl)
     ->
      % Fuzzy expansion as BPL
      % Builds the new rule's body with a check of the lambda-cut value,
      % the weak unification of the arguments, the original body and the
      % computation of the approximation degree
      concat_lists([RuleDegreeVars, HeadDegrees, [Degree]], DegreeVars),
      build_t_norm_goal('~',DegreeVars, HeadDegreeVar, TNormGoal),
      append_goals_list(['$over_lambda_cut'(Degree),
                         UnifyArgumentsGoal,
                         Body,
                         TNormGoal],NewBody),
      ExpClauses=[(SimExpClause,SNVs)|MoreExpClauses],
      IArgsListo=[IArgs|IArgsList]
     ;
      % Fuzzy expansion as DES
      % Builds the new rule's body with a check of the lambda-cut value,
      % a direct call to the predicate it is similar to, and the
      % computation of the approximation degree
      append(HeadVars,[DDegree],DArgs),
%      DNewHead =.. [HeadFunctor|DArgs],
      univ_head(HeadFunctor,DArgs,Restricted,DNewHead),
      build_t_norm_goal('~',[Degree,DDegree,HeadDegreeVar], HeadDegreeVar, TNormGoal),
      append_goals_list(['$over_lambda_cut'(Degree),
                         DNewHead,
                         TNormGoal],NewBody),
      % Do not add the rule if already added
      (datalog(ExpClause,_NVs,_RId,_CId,_Ls,_FId,_C)
       ->
        ExpClauses=MoreExpClauses,
        IArgsListo=IArgsList
       ;
        ExpClauses=[(SimExpClause,SNVs)|MoreExpClauses],
        IArgsListo=[IArgs|IArgsList]
      )
    )
  ;
    % Builds the new rule's body with the weak unification of the
    % arguments, the original body and the computation of the
    % approximation degree
    concat_lists([RuleDegreeVars, HeadDegrees], DegreeVars),
    build_t_norm_goal('~',DegreeVars, HeadDegreeVar, TNormGoal),
    append_goals_list([UnifyArgumentsGoal,
                       Body,
                       TNormGoal],NewBody),
    ExpClauses=[(SimExpClause,SNVs)|MoreExpClauses],
    IArgsListo=[IArgs|IArgsList]
  ),
   force_simplify_ruleNVs((ExpClause,NVs),(SimExpClause,SNVs)),
  % Scans the remaining symbols
  expand_rule(MoreSimDegrees, Clause, NVs, Cin, CBin, RuleDegreeVars, MoreExpClauses, IArgs, IArgsList).

expanded_rule_arity(ExtendedRuleArity,RuleArity) :-
  weak_unification(a1),
  !,
  RuleArity is ExtendedRuleArity - 1. % The last argument is the approximation degree
expanded_rule_arity(ExtendedRuleArity,RuleArity) :-
%   weak_unification(a3),
%   !,
  RuleArity is ExtendedRuleArity - 3. % The last three arguments iare: the input and output block constraint stores, and the approximation degree

build_unify_arguments_goal([],true) :-
  !.
build_unify_arguments_goal(UnificationGoals,'$unify_arguments'(UnificationGoals)).

build_t_norm_goal(_Relation,[D], D, D=1.0) :-
  !. 
build_t_norm_goal(Relation,DegreeVars, HeadDegreeVar, '$t_norm'(Relation, DegreeVars, HeadDegreeVar)).

init_ctr_store(answer,Cin) :-
  weak_unification(a3),
  !,
  new_ctr_store(Cin).
init_ctr_store(_HeadFunctor,_Cin).
  

%% create_weak_unification_goals(+Vars, +Args, +Degrees, -Problems)
%
%     Returns a list of unification Problems suitable for the
%     unify_arguments/1 predicate. Each of the returned problems will be
%     a list with an item of each of the three input lists: Vars, Args
%     and Degrees.
%
%     For example, given Vars = [X, Y], Args = [a, b] and
%     Degrees = [D1, D2], this predicate will return
%     Problems = [[X, a, D1], [Y, b, D2]].
%

create_weak_unification_goals(Vars, Args, _Ctrs, _Cin, _Cout, Degrees, Goals) :-
  weak_unification(a1),
  !,
  create_weak_unification_a1_goals(Vars, Args, Degrees, Goals).
create_weak_unification_goals(Vars, Args, [Cin,_], Cin, Cout, Degrees, Goals) :-
%   weak_unification(a3),
%   !,
  create_weak_unification_a3_goals(Vars, Args, Cin, Cout, Degrees, Goals).

create_weak_unification_a1_goals([], [], [], []).
create_weak_unification_a1_goals([Var|MoreVars], [Arg|MoreArgs], [Degree|MoreDegrees],
                                [[Var, Arg, Degree]|MoreGoals]) :-
  create_weak_unification_a1_goals(MoreVars, MoreArgs, MoreDegrees, MoreGoals).

create_weak_unification_a3_goals([], [], Cin, Cin, [], []).
create_weak_unification_a3_goals([Var|MoreVars], [Arg|MoreArgs], Cin, Cout, [Degree|MoreDegrees],
                                [[Var, Arg, Cin, Cin1, Degree]|MoreGoals]) :-
  create_weak_unification_a3_goals(MoreVars, MoreArgs, Cin1, Cout, MoreDegrees, MoreGoals).

  
/*********************************************************************/
/* Primitives                                                        */
/*********************************************************************/

%% t_norm_composition(+RelationName, +List, -Degree)             is det
%
%     Returns the approximation degree for the given relation as the 
%     composition of approximation degrees in List.
%

%% t_norm_aux(+RelationName, +List, -Degree)
%
%     Internal predicate used to avoid backtracking.
%
%     @see t_norm/2
%

t_norm_composition(Relation, List, Degree) :-
  t_norm(Relation, TNorm),  % Current t-norm
  t_norm_aux(List, TNorm, Degree).

t_norm_aux([], _TNorm, 1.0).
t_norm_aux([Number|List], TNorm, Degree) :-
  number(Number), 
  !,
  t_norm_aux(List, TNorm, CurrentDegree),
  t_norm_op(TNorm, Number, CurrentDegree, Degree).
t_norm_aux([_NotANumber|List], TNorm, Degree) :-
  t_norm_aux(List, TNorm, Degree).


%% over_lambda_cut(+Degree)                                   is semidet
%
%     Suceeds if the specified approximation Degree is greater than the
%     current lambda-cut value.
%

over_lambda_cut(Degree) :-
  lambda_cut(Lambda),
  Degree >= Lambda.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Negation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_fuzzy_not(G,D,St,CId,GId,X,R,N,NR,IdG) :-
  solve(G,St,CId,GId,X,R,N,NR,IdG),
  fuzzy_negated_goal_degree(G,D),
  !.
solve_fuzzy_not(_G,1.0,_St,_CId,_GId,_X,_R,_N,_NR,_IdG).

fuzzy_negated_goal_degree(G,D) :-
  G=..[_F|Args],
  append(_,[GD],Args),
  t_norm_composition('~',[GD],GD),
%  D is 1.0-GD,
  clpq_solve(1.0-GD,D),
  D>0.0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Weak unification algorithm A1. Used for /weak_unification a1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% weak_unify(?Term1, ?Term2, +CId, +Lambda, ?Degree)
%
%     Unifies Term1 and Term2 using unification by proximity/similarity
%     and returns the approximation Degree in which both terms unify.
%     Lambda is the lower bound allowed for the approximation degree
%     of the weak unifications.
%     CId is the context identifier in which weak unification is applied
%

% Weak Unify for Weak Negation
weak_unify(Atomic1, Atomic2, CId, Lambda, Degree) :-
  weak_negation(on),
  atomic(Atomic1), 
  atomic(Atomic2), 
  !,
  % Retrieve all possible entries in the fuzzy relation ~
  ~(Atomic1, Atomic2, CId, Degree),
  Degree >= Lambda.
weak_unify(Atomic1, Atomic2, CId, Lambda, Degree) :-
  \+ weak_negation(on),
  % Atom (constant) unification
  atomic(Atomic1), 
  atomic(Atomic2), 
  !,
  unification_degree(Atomic1, Atomic2, CId, Degree),
  Degree >= Lambda.
% weak_unify(Term1, Term2, Lambda, Degree) :-
%   % Term decomposition
%   compound(Term1), compound(Term2), !,
%   Term1 =.. [Functor1|Args1],
%   Term2 =.. [Functor2|Args2],
%   length(Args1, Arity),
%   length(Args2, Arity),
%   sim('~',Functor1, Functor2, DegreeFunctor), 
%   DegreeFunctor >= Lambda,
%   weak_unify_args(Args1, Args2, Lambda, DegreeArgs),
%   Degree is min(DegreeFunctor, DegreeArgs). % WARNING: Compute with t-norm
% weak_unify(LVariable, RVariable, _Lambda, 1.0) :-
%   % Term/variable swap + Variable removal
%   var(LVariable), 
%   var(RVariable), 
%   !,
%   (fuzzy_relation_property(~, reflexive)
%    ->
%     LVariable=RVariable
%    ;
%     !,
%     fail).
weak_unify(Term, Variable, _CId, _Lambda, 1.0) :-
  % Term/variable swap + Variable removal
  nonvar(Term), 
  var(Variable), 
  !,
  Variable = Term.
weak_unify(Variable, Term, _CId, _Lambda, 1.0) :-
  % Variable removal / Trivial equation removal
  var(Variable),
  Variable = Term.


%% weak_unify_args(?Args1, ?Args2, +CId, +Lambda, ?Degree)
%
%     Checks if the terms in the Args1 and Args2 lists can unify one
%     with each other and returns the minimum approximation Degree of
%     the unifications.
%

weak_unify_args([], [], _CId, _Lambda, 1.0).
weak_unify_args([Arg1|MoreArgs1], [Arg2|MoreArgs2], CId, Lambda, Degree) :-
  weak_unify(Arg1, Arg2, CId, Lambda, DegreeArg),
  weak_unify_args(MoreArgs1, MoreArgs2, CId, Lambda, DegreeMoreArgs),
  t_norm_composition('~',[DegreeArg, DegreeMoreArgs],Degree),
  over_lambda_cut(Degree).


unification_degree(Atomic, Atomic, _CId, 1.0) :- % Don't lookup and assume true for all the data universe for a reflexive relation
  (fuzzy_relation_property(~, reflexive)
   ->
    !
   ;
    fail).
unification_degree(Atomic1, Atomic2, CId, Degree) :-
  ~(Atomic1, Atomic2, CId, Degree),
  !.

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unification by proximity/similarity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% unify(?Term1, ?Term2, +CId, ?Degree)
%
%     Unifies Term1 and Term2 in the context CId using unification by 
%     proximity/similarity, and returns the approximation Degree in 
%     which both terms unify.
%

% A1:
unify(Term1, Term2, CId, Degree) :-
  lambda_cut(Lambda),
  weak_unify(Term1, Term2, CId, Lambda, Degree).

% A3:
unify(Term1, Term2, CId, Cin, Cout, Degree) :-
  lambda_cut(Lambda),
  weak_unify(Term1, Term2, CId, Lambda, Cin, Cout, Degree).


%% unify(?Term1, ?Term2, +Comparer, ?Value)                  is semidet
%
%     Unifies Term1 and Term2 using unification by proximity/similarity
%     and succeeds only if the resulting approximation degree satisfies
%     the expression "degree Comparer Value" (for example, "degree >
%     0.5"). Comparer can be any Prolog arithmetic comparison operator
%     (=:=, =\=, >=, =<, >, <) or the unification operator (in the
%     latter case, Value will be unified with the approximation
%     degree).
%

% % A1:
% unify(Term1, Term2, Comparer, Value) :-
%   unify(Term1, Term2, Degree),
%   apply(Comparer, [Degree, Value]),
%   !.

% % A3:
% unify(Term1, Term2, Comparer, Value) :-
%   unify(Term1, Term2, Degree),
%   apply(Comparer, [Degree, Value]),
%   !.


%% unify_arguments(?Problems,+CId)
%
%     Solves several unification Problems using the weak unification
%     algorithm. Problems must be a list containing sublists with three
%     items: two terms and a variable where the approximation degree
%     of the terms will be stored. If any of the weak unifications
%     fail, the whole predicate will fail.
%

unify_arguments([], _CId).
% A1:
unify_arguments([[Term1, Term2, Degree]|MoreProblems], CId) :-
  unify(Term1, Term2, CId, Degree),
  unify_arguments(MoreProblems, CId).
% A3:
unify_arguments([[Term1, Term2, Cin, Cout, Degree]|MoreProblems], CId) :-
  unify(Term1, Term2, CId, Cin, Cout, Degree),
  unify_arguments(MoreProblems, CId).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Weak unification algorithm A3. Used for /weak_unification a3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(assoc)).

%% weak_unify(?Term1, ?Term2, +CId, +Lambda, +Cin, -Cout, ?Degree)
%
%     Unifies Term1 and Term2 using unification by proximity/similarity
%     and returns the approximation Degree in which both terms unify.
%     Lambda is the lower bound allowed for the approximation degree
%     of the weak unifications. Cin is the input constraint store of 
%     block constraints, and Cout the output constraint store.

weak_unify(Atomic1, Atomic2, CId, Lambda, Cin, Cout, Degree) :-
  % Atom (constant) unification
  atomic(Atomic1), 
  atomic(Atomic2), 
  !,
  (Atomic1==Atomic2
   ->
    Degree=1,
    Cout=Cin
   ;
    unification_degree(Atomic1, Atomic2, CId, Block, Degree),
    Degree >= Lambda,
    sat([Atomic1:Block, Atomic2:Block], Cin, Cout)
  ).
% weak_unify(Term1, Term2, Lambda, Cin, Cout, Degree) :-
%   % Term decomposition
%   compound(Term1), 
%   compound(Term2), 
%   !,
%   Term1 =.. [Functor1|Args1],
%   Term2 =.. [Functor2|Args2],
%   length(Args1, Arity),
%   length(Args2, Arity),
%   (Functor1==Functor2
%    ->
%     Cin1=Cin,
%     DegreeFunctor=1
%    ;
%     unification_degree(Functor1, Functor2, Block, DegreeFunctor),
%     DegreeFunctor >= Lambda,
%     sat([Functor1:Block, Functor2:Block], Cin, Cin1)
%   ),
%   weak_unify_args(Args1, Args2, Lambda, Cin1, Cout, DegreeArgs),
%   Degree is min(DegreeFunctor, DegreeArgs). % WARNING: Compute with t-norm
weak_unify(Term, Variable, _CId, _Lambda, Cin, Cin, 1.0) :-
  % Term/variable swap + Variable removal
  nonvar(Term), 
  var(Variable), 
  !,
%  occur_check(Variable, Term),
  Variable = Term.
weak_unify(Variable, Term, _CId, _Lambda, Cin, Cin, 1.0) :-
  % Variable removal / Trivial equation removal
  var(Variable),
%  occur_check(Variable, Term),
  Variable = Term.

%% weak_unify_args(?Args1, ?Args2, +CId, +Lambda, +Cin, -Cout, ?Degree)
%
%     Checks if the terms in the lists Args1 and Args2 can unify one
%     with each other and returns the minimum approximation Degree of
%     the unifications.
%

weak_unify_args([], [], _CId, _Lambda, Cin, Cin, 1.0).
weak_unify_args([Arg1|MoreArgs1], [Arg2|MoreArgs2], CId, Lambda, Cin, Cout, Degree) :-
  weak_unify(Arg1, Arg2, CId, Lambda, Cin, Cin1, DegreeArg),
  weak_unify_args(MoreArgs1, MoreArgs2, CId, Lambda, Cin1, Cout, DegreeMoreArgs),
  t_norm_composition('~',[DegreeArg, DegreeMoreArgs],Degree),
  over_lambda_cut(Degree).

% occur_check(Variable, Term) :-
%   term_variables(Term, Variables),
%   \+ memberchk_eq(Variable,Variables). % Needs library hprolog.pl

unification_degree(Atomic, Atomic, _CId, _Block, 1.0) :- % Don't lookup and assume true for all the data universe for a reflexive relation
  (fuzzy_relation_property(~, reflexive)
   ->
    ! 
   ;
    fail).
unification_degree(Atomic1, Atomic2, CId, Block, Degree) :-
  '~'(Atomic1, Atomic2, CId, Block, Degree),
  !.

%% sat([Constraint|MoreConstraints], InConstraints, OutConstraints)
%
sat([], Cin, Cin).
sat([Ctr|Ctrs], Cin, Cout) :-
  sat_ctr(Ctr, Cin, Cin1),
  sat(Ctrs, Cin1, Cout).

sat_ctr(Symbol:Block, Cin, Cin) :-
  get_assoc(Symbol, Cin, Block1),
  !,
  Block=Block1.
sat_ctr(Symbol:Block, Cin, Cout) :-
  put_assoc(Symbol, Cin, Block, Cout).

new_ctr_store(C) :-
  empty_assoc(C). % WARNING: Test for SICStus
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generating the Extended Proximity Relation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(ordsets)).

/***********************************************************************
gen_rb/1 generates the extended proximity relation RB from the proximity
relation R (~). Notice that the proximity relation R is implemented 
by the predicate sim/3 and the extended proximity relation RB is implemented 
by the predicate sim/4.
Its single argument is the context identifier.
***********************************************************************/

gen_rb(CId) :- 
  weak_unification(a3),
  retractall('~'(_F,_T,CId,_B,_D)),
%  update_fuzzy_relation('~'), % gen_rb is called from this updating
  setof(F,T^D^'~'(F,T,CId,D),G),  % Nodes
  !,
  allMaxCliques(G,LC),
  build_ctrs(LC,Ctrs),        % Build annotations Symbol:Block as an ordered list
  gen_rb_entries(Ctrs, CId).
gen_rb(_CId). % No fuzzy equations (setof would fail) or weak unification algorithm different from A3
                
build_ctrs(LC,Cout) :-
  build_ctrs(LC,1,[],Cout).    
   
build_ctrs([],_B,Cin,Cin).
build_ctrs([Cs|LC],B,Cin,Cout) :-
  ctrs_from_block(Cs,B,Cin,Cin1),
  B1 is B+1,
  build_ctrs(LC,B1,Cin1,Cout).
  
ctrs_from_block([],_B,Cin,Cin).
ctrs_from_block([C|Cs],B,Cin,Cout) :-
  ord_union([C:B],Cin,Cin1), 
  ctrs_from_block(Cs,B,Cin1,Cout).

ord_ctr_member(X,[X|_Xs]). 
ord_ctr_member(X:B,[Y:_|Xs]) :-
  X@>=Y,
  ord_ctr_member(X:B,Xs). 
  
gen_rb_entries(Ctrs, CId) :-
  '~'(F,T,_CId,D),
  F\==T, % Do not include reflexive entries
  ord_ctr_member(F:B,Ctrs),
  ord_ctr_member(T:B,Ctrs),
  assertz('~'(F,T,CId,B,D)),
  fail.
gen_rb_entries(_Ctrs, _CId).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maximal cliques from the graph defined by the 'sim' relation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/****************************************************************************
BronKerbosch2(R,P,X):
    if P and X are both empty:
         report R as a maximal clique 
    choose a pivot vertex u in P ? X 
    for each vertex v in P \ N(u):
         BronKerbosch2(R ? {v}, P ? N(v), X ? N(v)) 
         P := P \ {v}
         X:=X ? {v}
*****************************************************************************/

allMaxCliques(LC) :-
  setof(F,T^D^C^'~'(F,T,D,C),Fs),
  allMaxCliques(Fs,LC).

%% allMaxCliques(G,LC): LC is a list of all maximal cliques of a graph G 
%% (passed as a set of nodes). G must be ordered.
allMaxCliques(G,LC) :-
%  list_to_ord_set(G,OG),         % Use this alternative when
%  setof(C, maxClique(OG,C), LC). % G is not expected to be ordered
  setof(C, maxClique(G,C), LC).

% %% GOAL EXAMPLE
% g(LC) :- allMaxCliques([1,2,3,4,5,6],LC).

%% maxClique(G,C): C is a maximal clique of a graph G passed as a set of nodes.
%% G must be ordered
maxClique(G, C) :-
  maxClique(G,[],[], C).

%% maxClique(P,X,A,R): 
/*
Finds the maximal cliques that include all of the vertices in R, some of the 
vertices in P, and none of the vertices in X. In each call to maxClique/4, 
P and X are disjoint sets whose union consists of those vertices that form 
cliques when added to R. 
When P and X are both empty there are no further elements that can be added to R, 
so R is a maximal clique and maxClique/4 outputs R.

A is an accumulator parameter where R is built step by step.

Notice: maxClique/4 implements BronKerbosch2 but with variations; for instance, the 
instructions P := P \ {v} and X:=X ? {v} are not considered; perhaps, by this reason 
the implementation is more inefficient, obtaining many redundant answers. 
*/

%% R is a maximal clique for a graph G
%% maxClique/4 obtains a clique for each V in DP=(P \ N(u))
%% (Many redundancies; then, low efficiency).
maxClique([],[],R,OR) :-
  !, % FSP
  list_to_ord_set(R,OR).
maxClique(P,X,A,R) :-
  ord_union(P,X,PX), 
  pivot(PX,U),  
  neighborSet(U,NU), 
  ord_subtract(P,NU,DP), 
  member(V,DP), 
  neighborSet(V,NV), 
  ord_intersection(P,NV,PNV), 
  ord_intersection(X,NV,XNV), 
  maxClique(PNV,XNV,[V|A],R). 


% Neighbor set of vertex V in G
neighborSet(V,NV) :-
  setof(N, D^C^('~'(V,N,D,C),V\==N), NV). % Explicit Symmetry, excluding reflexivity
%  setof(N, D^(sim(V,N,D) ; sim(N,V,D)), NV). % Implicit Symmetry


% Neighbor set of vertex V in the set PX
% NOT USED.
% neighborSet(V,PX,NVPX) :-
%   setof(N, D^sim(V,N,D), NV), 
%   intersect(PX,NV, NVPX).


%%pivot(PX,U)
%% IMPORTANT: We choose a pivot U that maximizes N(U), i.e., the neighbor 
%% set of U.
%% 
pivot(PX,U) :-
  pivot(PX,0,[],U).
  
pivot([],_,[A],A).
pivot([V|Vs],AN,_,U) :-
  neighborSet(V,NV), 
  length(NV,L), 
  L>AN, 
  !, 
  pivot(Vs,L,[V],U).
pivot([_|Vs],AN,[A],U) :-
  pivot(Vs,AN,[A],U).

/*****************************************
%% IMPORTANT: When we follow Tomita's option, consisting in
%% maximizing P ? N(U), this program does not work. 
%%
%%pivot(PX,P,U)
%% 
pivot(PX,P,U) :- pivot(PX,P,0,[],U).
pivot([],_,_,[A],A).
pivot([V|Vs],P,AN,_,U) :- neighborSet(V,NVP), %neighborSet(V,P,NVP),
                    length(NVP,L), L>AN, !, pivot(Vs,P,L,[V],U).
pivot([_|Vs],P,AN,[A],U) :- pivot(Vs,P,AN,[A],U).
*******************************************/