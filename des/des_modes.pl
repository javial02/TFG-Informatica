/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    Basic modes for assertions                         */
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODE ASSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% A mode assertion is automatically set when an
% unsafe rule is asserted
%
% :-mode(ModeSchema)
% 
% ModeSchema ::= PredName(Mode,...,Mode)
%
% Mode ::= i  % The argument must be ground at call time
% Mode ::= o  % The argument can be a free variable at call time
%
% In Datalog, all modes should be 'o', but because of infinite 
% builtins (as comparison operators <, ...), it is interesting
% to allow 'i' modes as well, as for the next example:
%
% :-mode(p(i,o)).
% p(T,1).
% p(T,X) :- p(T,Y),X=Y+1,X<T.
%
% Expected goals must have a ground first argument, as:
%
% p(100,X)
%
% which returns the first 100 naturals

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCESSING ASSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_modes_datalog_assertion(modes(N/A,GivenModes),_Ls,_FId,_Action,_Error) :-
  (my_modes(N/A,OldModes)
   ->
    retract(my_modes(N/A,OldModes)),
    join_modes(GivenModes,OldModes,NewModes)
   ;
    NewModes=GivenModes  
  ),
  assert_modes(my_modes(N/A,NewModes)).
  
assert_modes(my_modes(N/A,Modes)) :-
  assertz(my_modes(N/A,Modes)),
  PredModes=..[N|Modes],
  write_info_verb_log(['Assertion :-modes(',PredModes,') set.']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UPDATING MODES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_pred_modes(Head) :-
  functor(Head,N,A),
  head_modes(Head,HeadModes),
  pred_modes(N/A,CurrentModes),
  join_modes(HeadModes,CurrentModes,Modes),
  process_modes_datalog_assertion(modes(N/A,Modes),_Ls,_FId,_Action,_Error).
  
head_modes(Head,Modes) :-
  functor(Head,N,A),
  Head=..[N|Args],
  length(Args,A),
  args_modes(Args,Modes).
  
% head_input_var_modes(Head,IVs,Modes) :-
%   term_variables(Head,HVs),
%   my_set_diff(HVs,IVs,GVs),
%   copy_term([Head,GVs],[CH,CGVs]),
%   make_ground(CGVs),
%   head_modes(CH,Modes).
  
input_positions_modes(Ps,Modes) :-
  input_positions_modes(1,Ps,Modes).

input_positions_modes(_P,[],[]).
input_positions_modes(P,[P|Ps],[i|Modes]) :-
  !,
  P1 is P+1,
  input_positions_modes(P1,Ps,Modes).
input_positions_modes(P,Ps,[o|Modes]) :-
  P1 is P+1,
  input_positions_modes(P1,Ps,Modes).
  
args_modes([],[]).
args_modes([Var|Args],[i|Modes]) :-
  var(Var),
  !,
  args_modes(Args,Modes).
args_modes([_|Args],[o|Modes]) :-
  args_modes(Args,Modes).

pred_modes(N/A,CurrentModes) :-
  my_modes(N/A,CurrentModes),
  !.
pred_modes(_N/A,CurrentModes) :-
  length(CurrentModes,A).
  
join_pred_modes_list(_N/_A,[],[]).
join_pred_modes_list(N/A,ModesList,Modes) :-
  findall(Modes,member((N/A,Modes),ModesList),PredModesList),
  join_modes_list(PredModesList,Modes). 
  
join_modes_list([Ms],Ms).
join_modes_list([M1s,M2s|Ms],JMs) :-
  join_modes(M1s,M2s,TMs),
  join_modes_list([TMs|Ms],JMs).

join_modes(LVar,RVar,_) :-
  var(LVar),
  var(RVar),
  !.
join_modes([],[],[]).
join_modes([LMode|LModes],[RMode|RModes],[Mode|Modes]) :-
  top_pred_mode(LMode,RMode,Mode),
  join_modes(LModes,RModes,Modes).
  
top_pred_mode(Var1,Var2,Mode) :-
  var(Var1),
  var(Var2),
  !,
  Mode=o.
top_pred_mode(Var,Mode,Mode) :-
  var(Var),
  !.
top_pred_mode(Mode,Var,Mode) :-
  var(Var),
  !.
top_pred_mode(i,o,i) :-
  !.
top_pred_mode(o,i,i) :-
  !.  
top_pred_mode(Mode,Mode,Mode).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DROP MODES ASSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

drop_modes_assertion(modes(Pred,Modes)) :-
  % Silently do nothing, if there is no assertion
  \+ my_modes(Pred,Modes),
  !.
drop_modes_assertion(modes(N/A,Modes)) :-
  my_modes(N/A,Modes),
  PredModes=..[N|Modes],
  drop_flag(my_modes(N/A,Modes)),
  retractall(my_rule_modes(_RId,N/A,_RuleModes)),
  write_info_verb_log(['Assertion :-modes(',PredModes,') dropped.']).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET RULE MODES ASSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Given a new rule, assert its modes and update predicate modes

set_rule_modes_assertion(_RId,answer/_A,_RuleModes) :-
  !.
set_rule_modes_assertion(_,_,RuleModes) :-
  \+ member(i,RuleModes),
  !.
set_rule_modes_assertion(_RId,_N/0,_RuleModes) :- % Fuzzy facts
  !.
set_rule_modes_assertion(RId,N/A,CRuleModes) :-
  source_rule_modes(A,CRuleModes,RuleModes),
  assertz(my_rule_modes(RId,N/A,RuleModes)),
  (my_modes(N/A,CurrentModes) ; length(CurrentModes,A)),
  !,
  join_modes(CurrentModes,RuleModes,Modes),
  process_modes_datalog_assertion(modes(N/A,Modes),_Ls,_FId,_Action,_Error).
  
source_rule_modes(Arity,RuleModes,RuleModes) :-
  length(RuleModes,Arity),
  !.
source_rule_modes(Arity,CompiledRuleModes,SourceRuleModes) :-
  length(SourceRuleModes,Arity),
  append(SourceRuleModes,_,CompiledRuleModes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RETRACT RULE MODES UPDATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Given a new rule, assert its modes and update predicate modes

retract_rule_modes_update(RId) :-
  (retract(my_rule_modes(RId,N/A,_)),
   retract(my_modes(N/A,Modes))
   ->
    findall(RuleModes,my_rule_modes(_,N/A,RuleModes),RuleModesList),
    (RuleModesList==[]
     ->
%      true
      PredModes=..[N|Modes],
      write_info_verb_log(['Assertion :-modes(',PredModes,') dropped.'])
     ;
      join_modes_list(RuleModesList,JModes),
      assert_modes(my_modes(N/A,JModes))
    )
  ;
   true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHECK MODES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
check_modes(G,R) :-
  functor(G,N,A),
  my_modes(N/A,Modes),
  !,
  G=..[N|Args],
  (check_arg_mode_list(Args,Modes)
   ->
    true
   ;
    my_raise_exception(G,instantiation,R)).
check_modes(_G,_R).
  
check_arg_mode_list([],[]).
check_arg_mode_list([Arg|Args],[Mode|Modes]) :-
  check_arg_mode(Arg,Mode),
  check_arg_mode_list(Args,Modes).

check_arg_mode(_Arg,o) :-
  !.
check_arg_mode(Arg,i) :-
  my_ground(Arg),
  !.
check_arg_mode(Arg,i) :-
  contain_null(Arg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BUILT-IN MODES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

built_in_modes_list(L) :-
  L=[
   (is_null/1,[i]),
   (is_not_null/1,[i]),
   (select_not_null/3,[i,i,o]),
   ('$concat'/3,[i,i,o]),
   ('$greatest'/2,[i,o]),
   ('$instr'/3,[i,i,o]),
   ('$least'/2,[i,o]),
   ('$length'/2,[i,o]),
   ('$left'/3,[i,i,o]),
   ('$lower'/2,[i,o]),
   ('$lpad'/3,[i,i,o]),
   ('$lpad'/4,[i,i,i,o]),
   ('$rpad'/3,[i,i,o]),
   ('$rpad'/4,[i,i,i,o]),
   ('$ltrim'/2,[i,o]),
   ('$repeat'/3,[i,i,o]),
   ('$replace'/4,[i,i,i,o]),
   ('$reverse'/2,[i,o]),
   ('$right'/3,[i,i,o]),
   ('$rtrim'/2,[i,o]),
   ('$trim'/2,[i,o]),
   ('$space'/2,[i,o]),
   ('$substr'/4,[i,i,i,o]),
   ('$upper'/2,[i,o]),
   ('$like'/2,[i,o]),
   ('$like'/3,[i,i,o]),
   ('$not_like'/2,[i,o]),
   ('$not_like'/3,[i,i,o]),
   ('$year'/2,[i,o]),
   ('$month'/2,[i,o]),
   ('$day'/2,[i,o]),
   ('$hour'/2,[i,o]),
   ('$minute'/2,[i,o]),
   ('$second'/2,[i,o]),
   ('$last_day'/2,[i,o]),
   ('$datetime_add'/3,[i,i,o]),
   ('$datetime_sub'/3,[i,i,o]),
   ('$to_char'/2,[i,o]),
   ('$to_char'/3,[i,i,o]),
   ('$to_date'/2,[i,o]),
   ('$to_date'/3,[i,i,o]),
   ('$cast'/3,[i,i,o]),
   ('$coalesce'/2,[i,o]),
   ('$greatest'/2,[i,o]),
   ('$least'/2,[i,o]),
   ('$nvl'/3,[i,i,o]),
   ('$nvl2'/4,[i,i,i,o]),
   ('$nullif'/3,[i,i,o]),
   ('$iif'/4,[i,o,o,o]),
   ('$_power'/3,[i,i,o]),
   ('$trunc'/3,[i,i,o]),
   ('$trunc'/2,[i,o])
    ].
   
set_built_in_modes :-
  built_in_modes_list(Ms),
  findall(_,(member((P,M),Ms),assert_modes(my_modes(P,M))),_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GROUND ARGUMENTS w.r.t. MODES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ground_list_modes(+Args,+Modes)
ground_list_modes([],[]).
ground_list_modes([Arg|Args],[i|Modes]) :-
  my_ground(Arg),
  ground_list_modes(Args,Modes).
ground_list_modes([_Arg|Args],[o|Modes]) :-
  ground_list_modes(Args,Modes).
