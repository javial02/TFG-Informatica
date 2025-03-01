/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    Commands                                           */
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
% PARSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

is_command(CInput) :-
  command_begin(CInput,_).
  
% is_tapi_command(Cs) :-
%   is_tapi_command(Cs,_).

is_tapi_command -->
  command_begin,
  my_blanks_star, 
  my_kw("TAPI"),
  my_blanks.

command_begin -->
  my_blanks_star,
  "/*",
  {!,
   fail}.
command_begin -->
  my_blanks_star,
  "/".
  
my_command_input(Input,Command) :-
  my_command_input(Command,Input,_),
  !.
  
my_command_input(reconsult) -->
  command_begin,
  my_blanks_star, 
  "[+",
  !.
my_command_input(consult) -->
  command_begin,
  my_blanks_star, 
  "[",
  !.
my_command_input(repeat(_N)) -->
  command_begin,
  my_blanks_star, 
  my_kw("REPEAT"),
  !.
my_command_input(Command) -->
  command_begin,
  my_blanks_star, 
  my_command(Command),
  my_blanks_star.

    
parse_command(Cmd,Args,NVs) -->
  parse_cmd(Cmd,Args,NVs),
  {reset_syntax_error}.

parse_cmd(assert,[Rule],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("ASSERT"),
  " ",
  !,
  parse_rule(Rule,[],NVs),
  my_blanks_star,
  {rule_head(Rule,Head),
   check_redef(Head)}.
parse_cmd(retract,[Rule],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("RETRACT"),
  " ",
  !,
%  my_blanks_star,
%  my_rule(Rule,[],NVs),
  parse_rule(Rule,[],NVs),
  my_blanks_star.
parse_cmd(des,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("DES"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(tapi,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("TAPI"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(time,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("TIME"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(silent,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SILENT"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(if,[Condition,Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("IF"),
  my_blanks,
  !,
  my_command_condition(Condition,[],_Vo),
  my_blanks,
  my_tail(Cs),
  {name(Input,Cs)}.
parse_cmd(set_flag,[Flag,Expression],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET_FLAG"),
  my_blanks,
  my_chars_but_blank_symbol(Flag),
  my_blanks,
  my_expression(Expression,_,[],_Vo),
  my_blanks_star,
  !.
parse_cmd(set_flag,[Flag,Value],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET_FLAG"),
  my_blanks,
  !,
  my_chars_but_blank_symbol(Flag),
  my_blanks,
  my_chars(Cs),
  {name(Value,Cs)},
  my_blanks_star.
parse_cmd(set,[Variable],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET"),
  my_blanks,
  my_chars_but_blank_symbol(Variable),
  my_blanks_star,
  !.
parse_cmd(set,[Variable,Expression],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET"),
  my_blanks,
  my_chars_but_blank_symbol(Variable),
  my_blanks,
  my_expression(Expression,_,[],_Vo),
  my_blanks_star,
  !.
parse_cmd(set,[Variable,Value],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET"),
  my_blanks,
  !,
  my_chars_but_blank_symbol(Variable),
  my_blanks,
  my_chars(Cs),
  {name(Value,Cs)},
  my_blanks_star.
parse_cmd(Process,[File|Params],[]) -->
  command_begin,
  my_blanks_star, 
  my_process_command(Process),
  my_blanks,
  !,
  my_file(File),
  my_params(Params).
parse_cmd(run,[File|Params],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("RUN"),
  my_blanks,
  !,
  my_file(File),
  my_params(Params).
parse_cmd(load_db,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("LOAD_DB"),
  my_blanks,
  !,
  my_file(File).
% parse_cmd(load_hq,[File],[]) -->
%   command_begin,
%   my_blanks_star, 
%   my_kw("LOAD_HQ"),
%   my_blanks,
%   !,
%   my_file(File).
parse_cmd(repeat(N),[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("REPEAT"),
  my_blanks,
  !,
  my_positive_integer(N),
  my_blanks,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(goto,[Label],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("GOTO"),
  my_blanks,
  !,
  my_symbol(Label),
  my_blanks_star.
parse_cmd(timeout,[T,Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("TIMEOUT"),
  my_blanks,
  !,
  my_positive_integer(T),
  my_blanks,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(set_default_parameter,[I,Value],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET_DEFAULT_PARAMETER"),
  my_blanks,
  !,
  my_positive_integer(I),
  my_blanks,
  my_chars(Value).
% parse_cmd(set_var,[Var,Value],[]) -->
%   command_begin,
%   my_blanks_star, 
%   my_kw("SET_VAR"),
%   my_blanks,
%   !,
%   my_constant(Var),
%   my_blanks,
%   my_chars(Cs),
%   {name(Value,Cs)},
%   my_blanks_star.
parse_cmd(shell,[Command],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SHELL"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Command,Cs)},
  my_blanks_star.
parse_cmd(ashell,[Command],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("ASHELL"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Command,Cs)},
  my_blanks_star.
parse_cmd(Log,[append,normal,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
  my_blanks,
  my_kw("APPEND"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(Log,[write,normal,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
%  my_kw("LOG"),
  my_blanks,
  my_kw("WRITE"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(Log,[write,normal,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
%  my_kw("LOG"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(Log,[],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
%  my_kw("LOG"),
  my_blanks_star,
  !.
parse_cmd(write_to_file,[File,Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("WRITE_TO_FILE"),
  !,
  my_blanks,
  my_file(File),
  parse_write_output(Input).
parse_cmd(writeln,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("WRITELN"),
  !,
  parse_write_output(Input).
% parse_cmd(writeqln,[Input],[]) -->
%   command_begin,
%   my_blanks_star, 
%   my_kw("WRITEQLN"),
%   !,
%   parse_write_output(Input).
parse_cmd(write,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("WRITE"),
  !,
  parse_write_output(Input).
parse_cmd(csv,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("CSV"),
  my_blanks,
  my_file(File),
  !.
parse_cmd(csv,[],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("CSV"),
  !.
parse_cmd(cd,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("CD"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(Ls,[Path],[]) -->
  {LsStr = "LS",
   Ls = ls
  ;
   LsStr = "DIR",
   Ls = dir},
  command_begin,
  my_blanks_star, 
  my_kw(LsStr),
  my_blanks,
  my_file(Path),
  my_blanks_star,
  !.
parse_cmd(Copy,[FromFile,ToFile],[]) -->
  {CopyStr = "CP",
   Copy = cp
  ;
   CopyStr = "COPY",
   Copy = copy},
  command_begin,
  my_blanks_star, 
  my_kw(CopyStr),
  my_blanks,
  my_file(FromFile),
  my_blanks,
  my_file(ToFile),
  my_blanks_star,
  !.
parse_cmd(save_ddb,[force,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_DDB"),
  my_blanks,
  my_kw("FORCE"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(save_ddb,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_DDB"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(save_ddb,[],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_DDB"),
  my_blanks_star,
  !.
parse_cmd(restore_ddb,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("RESTORE_DDB"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(restore_ddb,[],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("RESTORE_DDB"),
  my_blanks_star,
  !.
parse_cmd(save_state,[force,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_STATE"),
  my_blanks,
  my_kw("FORCE"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(save_state,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_STATE"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(restore_state,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("RESTORE_STATE"),
  my_blanks,
  my_file(File),
  my_blanks_star,
  !.
parse_cmd(reconsult,Files,[]) -->
  command_begin,
  my_blanks_star, 
  "[+",
  my_blanks_star,
  my_files(Files),
  my_blanks_star,
  "]",
  my_blanks_star,
  !.
parse_cmd(reconsult,Files,[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("RECONSULT"),
  my_blanks_star,
  my_files(Files),
  my_blanks_star,
  !.
parse_cmd(reconsult,Files,[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("R"),
  my_blanks,
  my_files(Files),
  my_blanks_star,
  !.
parse_cmd(consult,Files,[]) -->
  command_begin,
  my_blanks_star,
  "[",
  my_blanks_star,
  my_files(Files),
  my_blanks_star,
  "]",
  my_blanks_star,
  !.
parse_cmd(consult,Files,[]) -->
  command_begin,
  my_blanks_star,
  my_kw("CONSULT"),
  my_blanks_star,
  my_files(Files),
  my_blanks_star,
  !.
parse_cmd(consult,Files,[]) -->
  command_begin,
  my_blanks_star,
  my_kw("C"),
  my_blanks,
  my_files(Files),
  my_blanks_star,
  !.
parse_cmd(drop_ic,[Constraint],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DROP_IC"),
  parse_datalog_constraint(Constraint,NVs),
  !.
parse_cmd(drop_assertion,[Assertion],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DROP_ASSERTION"),
  my_blanks_star,
  parse_datalog_assertion(Assertion,NVs),
  !.
% parse_cmd(datalog,[Goal],NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_command(datalog),
%   " ",
%   !,
%   my_blanks_star,
%   my_body(Goal,[],NVs),
%   my_blanks_star. 
% parse_cmd(prolog,[Goal],NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_command(prolog),
%   " ",
%   !,
%   my_blanks_star,
%   my_body(Goal,[],NVs),
%   my_blanks_star. 
parse_cmd(dependent_relations,[P|Ms],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEPENDENT_RELATIONS"),
  my_blanks,
  my_optional_command_exclusive_modifier(["DIRECT","ALL"],M1s),
  my_optional_command_modifier("DECLARED",M2s),
  my_pattern(P),
  my_blanks_star,
  {append(M1s,M2s,Ms)},
  !.
parse_cmd(dependent_relations,[N|Ms],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEPENDENT_RELATIONS"),
  my_blanks,
  my_optional_command_exclusive_modifier(["DIRECT","ALL"],M1s),
  my_optional_command_modifier("DECLARED",M2s),
  my_relation_identifier(N),
  my_blanks_star,
  {append(M1s,M2s,Ms)},
  !.
parse_cmd(referenced_relations,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("REFERENCED_RELATIONS"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(referenced_relations,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("REFERENCED_RELATIONS"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(spy,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("SPY"),
  my_blanks,
  my_pred_spec(N/A),
  my_blanks_star,
  !.
parse_cmd(nospy,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("NOSPY"),
  my_blanks,
  my_pred_spec(N/A),
  my_blanks_star,
  !.
parse_cmd(system,[Goal],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("SYSTEM"),
  my_blanks,
%  my_body(Goal,[],_NVs),
  my_prolog_term(Goal),
  my_blanks_star,
  !.
parse_cmd(abolish,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("ABOLISH"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(list_schema,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_SCHEMA"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(DBSchema,[R],[]) -->
  {DBSchemaStr = "DBSCHEMA",
   DBSchema = dbschema
  ;
   DBSchemaStr = "DB_SCHEMA",
   DBSchema = db_schema},
  command_begin,
  my_blanks_star,
  my_kw(DBSchemaStr),
  my_blanks,
  my_possibly_qualified_relation(R),
%   my_sql_user_identifier(C),
%   my_blanks_star,
%   ":",
%   my_blanks_star,
%   my_sql_user_identifier(N),
  my_blanks_star,
  !.
% parse_cmd(DBSchema,[N],[]) -->
%   {DBSchemaStr = "DBSCHEMA",
%    DBSchema = dbschema
%   ;
%    DBSchemaStr = "DB_SCHEMA",
%    DBSchema = db_schema},
%   command_begin,
%   my_blanks_star,
%   my_kw(DBSchemaStr),
%   my_blanks,
%   my_sql_user_identifier(N),
%   my_blanks_star,
%   !.
parse_cmd(DBSchema,[],[]) -->
  {DBSchemaStr = "DBSCHEMA",
   DBSchema = dbschema
  ;
   DBSchemaStr = "DB_SCHEMA",
   DBSchema = db_schema},
  command_begin,
  my_blanks_star,
  my_kw(DBSchemaStr),
  my_blanks_star,
  !.
parse_cmd(describe,[R],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DESCRIBE"),
  my_blanks,
  my_possibly_qualified_relation(R),
  my_blanks_star,
  !.
parse_cmd(relation_schema,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_SCHEMA"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(get_relation,[C,N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("GET_RELATION"),
  my_blanks,
  my_sql_user_identifier(C),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(relation_modified,[],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_MODIFIED"),
  my_blanks_star,
  !.
parse_cmd(relation_exists,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_EXISTS"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(is_empty,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_EXISTS"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(list_table_constraints,[TableName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_TABLE_CONSTRAINTS"),
  my_blanks,
  my_sql_user_identifier(TableName),
  my_blanks_star.
parse_cmd(list_et,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_ET"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(list_modes,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_MODES"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(list_modes,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_MODES"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(listing,[R],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
%  my_rule(R,[],_NVs),
  parse_rule(R,[],_NVs),
  my_blanks_star,
  !.
parse_cmd(listing,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(listing,[H],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_head(H,[],_NVs),
  my_blanks_star,
  !.
parse_cmd(listing,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(listing_asserted,[R],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_rule(R,[],_NVs),
  my_blanks_star.
parse_cmd(listing_asserted,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(listing_asserted,[H],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_head(H,[],_NVs),
  my_blanks_star,
  !.
parse_cmd(listing_asserted,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(list_sources,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_SOURCES"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(pdg,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("PDG"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(pdg,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("PDG"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(rdg,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RDG"),
  my_blanks,
  my_pred_spec(N/A),
  my_blanks_star,
  !.
parse_cmd(rdg,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RDG"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(strata,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("STRATA"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_cmd(strata,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("STRATA"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_cmd(retractall,[Head],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RETRACTALL"),
  " ",
  !,
  my_blanks_star,
  my_head(Head,[],_NVs),
  my_blanks_star.
parse_cmd(open_db,[Connection|Options],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("OPEN_DB"),
  my_blanks,
  !,
  my_symbol(Connection),
  my_blanks_star,
  my_atoms_star(Options,[],_),
  my_blanks_star.
parse_cmd(close_db,[Connection],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("CLOSE_DB"),
  my_blanks,
  !,
  my_symbol(Connection),
  my_blanks_star.
% parse_cmd(hrsql,[Connection],_NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_kw("HRSQL"),
%   my_blanks,
%   !,
%   my_symbol(Connection),
%   my_blanks_star.
parse_cmd(parse,[Input],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("PARSE"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_cmd(mparse,[],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("MPARSE"),
  !,
  my_blanks_star.
parse_cmd(solve,[Body],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("SOLVE"),
  my_blanks,
  !,
  my_body(Body,[],NVs),
  my_blanks_star,
  !.
% parse_cmd(host_safe_goal,[Goal],[]) -->
%   command_begin,
%   my_blanks_star,
%   my_kw("HOST_SAFE_GOAL"),
%   my_blanks,
%   !,
%   my_chars(String),
%   my_blanks_star,
%   {append(String,".",StrGoal),
%    my_string_to_term(StrGoal,Goal)}.
parse_cmd(debug_datalog,[Goal,Level],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DATALOG"),
  my_blanks,
  my_literal(Goal,[],NVs),
  my_blanks,
  my_symbol_option(Level),
  my_blanks_star,
  !.
parse_cmd(debug_datalog,[Goal],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DATALOG"),
  my_blanks,
  !,
  my_literal(Goal,[],NVs),
  my_blanks_star.
parse_cmd(debug_dl,[N/A|AnswerOptions],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DL"),
  my_blanks,
  my_pred_spec(N/A),
  my_optional_dl_debug_answer(Answer),
  {Answer\==answer(abort)},
%  my_optional_dl_debug_file(File),
  my_optional_debug_options(Options),
  my_blanks_star,
  !,
  {append(Answer,Options,AnswerOptions)}.
parse_cmd(debug_dl_answer,[Question,Answer],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DL_ANSWER"),
  !,
  my_blanks,
  my_debug_dl_question(Question),
  my_blanks,
  my_debug_dl_answer(Answer),
  my_blanks_star,
  !,
  {debug_dl_valid_question(Question),
   debug_dl_valid_answer(Question,Answer)}.
parse_cmd(debug_dl_set_node,[N/A,State],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DL_SET_NODE"),
  my_blanks,
  my_pred_spec(N/A),
  my_blanks,
  my_symbol(State),
  my_blanks_star,
  !.
parse_cmd(debug_sql,[ViewName|AnswerOptions],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL"),
  my_blanks,
  !,
  my_sql_user_identifier(ViewName),
  my_optional_sql_debug_answer(Answer),
  {Answer\==answer(abort)},
  my_optional_debug_options(Options),
  my_blanks_star,
  !,
  {append(Answer,Options,AnswerOptions)}.
parse_cmd(debug_sql_answer,[Question,Answer],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL_ANSWER"),
  !,
  my_blanks,
  my_debug_sql_question(Question),
  my_blanks,
  my_debug_sql_answer(Answer),
  my_blanks_star,
  !,
  {debug_sql_valid_answer(Question,Answer)}.
parse_cmd(debug_sql_set_node,[Node,State],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL_SET_NODE"),
  my_blanks,
  my_sql_user_identifier(Node),
  my_blanks,
  my_symbol(State),
  my_blanks_star,
  !.
parse_cmd(trace_sql,[ViewName,Ordering],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_symbol_option(Ordering),
  my_blanks_star,
  !.
parse_cmd(trace_sql,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks_star,
  !.
parse_cmd(trace_datalog,[Query,Ordering],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_DATALOG"),
  my_blanks,
  my_literal(Query,[],NVs),
  my_blanks,
  my_symbol_option(Ordering),
  my_blanks_star,
  !.
parse_cmd(trace_datalog,[Query],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_DATALOG"),
  my_blanks,
  my_literal(Query,[],NVs),
  my_blanks_star,
  !.
parse_cmd(test_case,[ViewName,Opt1,Opt2],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TEST_CASE"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_symbol_option(Opt1),
  my_blanks,
  my_symbol_option(Opt2),
  my_blanks_star,
  !.
parse_cmd(test_case,[ViewName,Opt],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TEST_CASE"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_symbol_option(Opt),
  my_blanks_star,
  !.
parse_cmd(test_case,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TEST_CASE"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks_star,
  !.
parse_cmd(tc_size,[Min,Max],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TC_SIZE"),
  my_blanks,
  !,
  my_integer(Min),
  my_blanks_star,
  my_integer(Max),
  my_blanks_star.
parse_cmd(tc_domain,[Min,Max],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TC_DOMAIN"),
  my_blanks,
  !,
  my_integer(Min),
  my_blanks_star,
  my_integer(Max),
  my_blanks_star.
parse_cmd(system_mode,[Mode],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("SYSTEM_MODE"),
  my_blanks,
  my_symbol_option(Mode),
  my_blanks_star,
  !.
parse_cmd(generate_db,[NbrTables,TableSize,NbrViews,MaxDepth,MaxWidth,FileName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("GENERATE_DB"),
  my_blanks,
  !,
  my_integer(NbrTables),
  my_blanks,
  my_integer(TableSize),
  my_blanks,
  my_integer(NbrViews),
  my_blanks,
  my_integer(MaxDepth),
  my_blanks,
  my_integer(MaxWidth),
  my_blanks,
  my_file(FileName),
  my_blanks_star,
  !.
parse_cmd(debug_sql_bench,[NbrTables,TableSize,NbrViews,MaxDepth,MaxWidth,FileName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL_BENCH"),
  my_blanks,
  !,
  my_integer(NbrTables),
  my_blanks,
  my_integer(TableSize),
  my_blanks,
  my_integer(NbrViews),
  my_blanks,
  my_integer(MaxDepth),
  my_blanks,
  my_integer(MaxWidth),
  my_blanks,
  my_file(FileName),
  my_blanks_star,
  !.
parse_cmd(Help,[Keyword],[]) -->
  command_begin,
  my_blanks_star,
  my_help_command(Help),
  my_blanks,
  my_optional_char("/"),
  my_blanks_star,
  my_chars_but_blank_symbol(Keyword),
  my_blanks_star,
  !.
parse_cmd(Command,Arguments,NVs) -->
  parse_fuzzy_cmd(Command,Arguments,NVs).
parse_cmd(Command,[],[]) -->
  command_begin,
  my_blanks_star,
  my_command(Command),
  my_blanks_star
  %,!
  .
parse_cmd(Command,Arguments,[]) -->
  %WARNING: This is needed to avoid the cut in the clause above. 
  {Arguments\==[]},
  command_begin,
  my_blanks_star,
  my_command(Command),
  my_blanks,
  my_arguments(Arguments),
  my_blanks_star.
  
my_help_command(help) -->
  my_kw("HELP").
my_help_command(h) -->
  my_kw("H").

my_optional_char(C) -->
  C.
my_optional_char(_C) -->
  [].
  
my_optional_command_exclusive_modifier([ModifierStr|_ModifiersStr],Modifier) -->
  my_optional_command_modifier(ModifierStr,Modifier).
my_optional_command_exclusive_modifier([_ModifierStr|ModifiersStr],Modifier) -->
  my_optional_command_exclusive_modifier(ModifiersStr,Modifier).
my_optional_command_exclusive_modifier(_,[]) -->
  [].

my_optional_command_modifier(_,[]) -->
  [].
my_optional_command_modifier(ModifierString,[Modifier]) -->
  my_kw(ModifierString),
  my_blanks,
  {to_lowercase_char_list(ModifierString,DCModifierString),
   atom_codes(Modifier,DCModifierString)}.

my_debug_dl_question(all(N/A)) -->
  "all(",
  my_pred_spec(N/A),
  ")".
my_debug_dl_question(subset(N1/A,N2/A)) -->
  "subset(",
  my_pred_spec(N1/A),
  ",",
  my_pred_spec(N2/A),
  ")".
my_debug_dl_question(nonsubset(N1/A,N2/A)) -->
  "nonsubset(",
  my_pred_spec(N1/A),
  ",",
  my_pred_spec(N2/A),
  ")".
my_debug_dl_question(empty(N/A)) -->
  "empty(",
  my_pred_spec(N/A),
  ")".
my_debug_dl_question(nonempty(N/A)) -->
  "nonempty(",
  my_pred_spec(N/A),
  ")".

% % eq(c(q/1),i(q))
% my_debug_dl_question(eq(c(R/N),i(R))) -->
%   "eq(c(",
%   my_pred_spec(R/N),
%   "),i(",
%   my_symbol(R),
%   "))".
% % subset(Sign,pi(N,M,c(R1,C)),i(R2))
% my_debug_dl_question(subset(Sign,pi(N,M,c(R1,C)),i(R2))) -->
%   "subset(",
%   my_symbol(Sign),
%   ",pi(",
%   my_positive_integer(N),
%   ",",
%   my_positive_integer(M),
%   ",c(",
%   my_symbol(R1),
%   ",",
%   my_symbol(C),
%   ")),i(",
%   my_symbol(R2),
%   "))".
  
  
my_debug_sql_question(all(RelName)) -->
  my_kw("ALL"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_sql_user_identifier(RelName),
  my_sql_blanks_star,
  ")".
my_debug_sql_question(in(Tuple,RelName)) -->
  my_kw("IN"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_sql_tuple(Tuple),
  my_sql_blanks_star,
  my_comma,
  my_sql_blanks_star,
  my_sql_user_identifier(RelName),
  my_sql_blanks_star,
  ")".
my_debug_sql_question(subset(RelName1,RelName2)) -->
  my_kw("SUBSET"),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_sql_user_identifier(RelName1),
  my_sql_blanks_star,
  my_comma,
  my_sql_blanks_star,
  my_sql_user_identifier(RelName2),
  my_sql_blanks_star,
  ")".
  
my_sql_tuple(Tuple) -->
  my_sql_user_identifier(RelName),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_sql_sequence_of_ctes(Ctes),
  my_sql_blanks_star,
  ")",
  {Tuple=..[RelName|Ctes]}.
  
my_sql_partial_tuple(Tuple) -->
  my_sql_user_identifier(RelName),
  my_sql_blanks_star,
  "(",
  my_sql_blanks_star,
  my_sql_sequence_of_ctes_undef(CtesVars),
  my_sql_blanks_star,
  ")",
  {Tuple=..[RelName|CtesVars]}.
  
my_debug_sql_answer(Answer) -->
  my_atom(Answer,[],[]),
  {memberchk(Answer,[valid,nonvalid,abort])}.
my_debug_sql_answer(missing(Tuple)) -->
  "missing(",
  my_sql_partial_tuple(Tuple),
  ")".
my_debug_sql_answer(wrong(Tuple)) -->
  "wrong(",
  my_sql_tuple(Tuple),
  ")".
  
debug_dl_valid_question(_Question).
debug_dl_valid_answer(_Question,_Answer).
  
% debug_sql_valid_answer(+Question,+Answer).
% Answer is already known to be valid. This checks if the answer is valid w.r.t. Question
debug_sql_valid_answer(all(_),_).
debug_sql_valid_answer(subset(_,_),Answer) :-
  PAnswers=[abort,valid,nonvalid,wrong(_)],
  debug_sql_valid_answer_check(subset,Answer,PAnswers).
debug_sql_valid_answer(in(_,_),Answer) :-
  PAnswers=[abort,valid,nonvalid],
  debug_sql_valid_answer_check(in,Answer,PAnswers).
  
debug_sql_valid_answer_check(QuestionType,Answer,PAnswers) :-
  (memberchk(Answer,PAnswers)
   ->
    true
   ;
    my_raise_exception(generic,syntax(['Incorrect answer option for ''',QuestionType,''' question: ''',Answer,'''. Possible values are: ',PAnswers]),[])
  ).

my_sql_sequence_of_ctes(Ctes) -->
  my_sql_sequence_of([my_sql_constant],ICtes),
  {findall(Cte,member(cte(Cte,_),ICtes),Ctes)}.

my_sql_sequence_of_ctes_undef(Ctes) -->
  my_sql_sequence_of([my_sql_constant,my_sql_undef_value],ICtes),
  {findall(Cte,(member(E,ICtes),nonvar(E),E=cte(Cte,_)),Ctes)}.
  
my_sql_undef_value(_FreshVar) -->
  "_".
  
% my_optional_dl_debug_file(file(File)) -->
%   my_blanks,
%   my_charsbutcomma(Fs),
%   {name(File,Fs)}.
% my_optional_dl_debug_file(nofile) -->
%   [].

my_optional_dl_debug_answer([answer(Answer)]) -->
  my_blanks,
  my_debug_dl_answer(Answer).
my_optional_dl_debug_answer([answer(no)]) -->
  {tapi(on),
   !},
  [].
my_optional_dl_debug_answer([]) -->
  [].
    
my_debug_dl_answer(Answer) -->
  my_symbol(CAnswer),
  {memberchk(CAnswer,[valid,nonvalid,abort]),
   debug_dl_command_answer_internal_answer(CAnswer,Answer)}.
my_debug_dl_answer(u_missing(Tuple,R)) -->
  "missing(",
  my_blanks_star,
  my_atom(Fact,[],_),
  my_blanks_star,
  ")",
  {Fact=..[R|As],
   my_list_to_tuple(As,Tuple)}.
my_debug_dl_answer(u_wrong(Tuple,R)) -->
  "wrong(",
  my_blanks_star,
  my_atom(Fact,[],_),
  my_blanks_star,
  ")",
  {Fact=..[R|As],
   my_list_to_tuple(As,Tuple)}.
  
debug_dl_command_answer_internal_answer(valid,yes) :- !.
debug_dl_command_answer_internal_answer(nonvalid,no) :- !.
debug_dl_command_answer_internal_answer(Answer,Answer).
  
my_optional_sql_debug_answer([answer(Answer)]) -->
  my_blanks,
  my_debug_sql_answer(Answer).
my_optional_sql_debug_answer([answer(nonvalid)]) -->
  {tapi(on),
   !},
  [].
my_optional_sql_debug_answer([]) -->
  [].
    
my_optional_debug_options([Option|Options]) -->
  my_blanks,
  my_debug_option(Option),
  my_optional_debug_options(Options).
my_optional_debug_options([]) -->
  [].
  
my_debug_option(file(FileName)) -->
  "file(",
  my_file(FileName),
  ")",
  !.
my_debug_option(Option) -->
  my_atom(Option,[],[]).

my_sql_sequence_of(Ps,[X|Xs]) -->
  my_sql_term_of(Ps,X),
  my_sql_blanks_star,
  my_comma,
  my_sql_blanks_star,
  my_sql_sequence_of(Ps,Xs).
my_sql_sequence_of(Ps,[X]) -->
  my_sql_term_of(Ps,X).

my_sql_term_of(Ps,X) -->
  {member(P,Ps),
   CP=..[P,X]},
  push_pattern_syntax_error(Ps,Old),
  CP,
  pop_syntax_error(Old).

parse_write_output(Input) -->
  my_blank,
  my_chars(StrInput),
  {!,
   name(Input,StrInput)}.
parse_write_output('') -->
  my_blank.
parse_write_output('') -->
  [].

my_symbol_option(Opt) -->
  my_downcased_token(Opt).
    
my_command(Command) -->
  my_downcased_token(Command).
  
my_prolog_term(Term,Cs,[]) :-
  "."=[D],
  append(Cs,[D],DCs),
  read_from_codes(DCs,Term).
  
% Parse a token as a downcased atom (for commands and their arguments)
my_downcased_token(Token) -->
  my_chars_but_blank([C|Cs]), % At least a char
  {to_lowercase_char_list([C|Cs],LCs),
   atom_codes(Token,LCs)}.
   
my_log_command(log) -->
  my_kw("LOG").
% my_log_command(logiql_log) -->
%   my_kw("LOGIQL_LOG").

my_process_command(Process) -->
  my_kw("PROCESS"),
  !,
  {Process=p ; Process=process}.
my_process_command(Process) -->
  my_kw("P"),
  {Process=p ; Process=process}.

my_file(File) -->
  my_str_argument(StrFile),
  {name(File,StrFile)}.

my_files([]) -->
  [].
my_files([A|As]) -->
  my_blanks_star,
  my_charsbutcomma(Cs),
  {my_file(A,Cs,"")},
  my_blanks_star,
  ",",
  my_files(As).
my_files([A]) -->
  my_blanks_star,
  my_file(A),
  my_blanks_star.
  
my_str_argument(File) -->
  """",
  my_chars_but_double_quotes(File),
  """",
  !.
my_str_argument(File) -->
  my_chars_but(" ;",File),
  my_blanks_star.
  
my_chars_but(Bs,[C|Cs]) -->
  [C],
  {\+member(C,Bs)},
  my_chars_but(Bs,Cs).
my_chars_but(_Bs,[]) -->
  [].
  
my_chars_but_double_quotes([C|Cs]) -->
  [C],
  {""""\==[C]},
  my_chars_but_double_quotes(Cs).
my_chars_but_double_quotes([]) -->
  [].
  
my_params([P|Ps]) -->
  my_blanks,
  my_str_argument(P),
  {P\==""},
  my_params(Ps).
my_params([]) -->
  my_blanks_star,
  [].

% my_command_condition(A,Vi,Vo) -->
%   my_opening_parentheses_star(N),
%   my_blanks_star,
%   my_expression(L,_,Vi,Vo1),
%   my_blanks_star,
%   my_infix_comparison(Op),
%   my_blanks_star,
%   my_expression(R,_,Vo1,Vo),
%   my_blanks_star,
%   my_closing_parentheses_star(N),
%   my_blanks,
%   {A =.. [Op,L,R]}.
my_command_condition(Condition,Vi,Vo) -->
  my_opening_parentheses_star(N),
  my_body(Condition,Vi,Vo),
  my_closing_parentheses_star(N).


my_possibly_qualified_relation(C:N) -->
  my_sql_user_identifier(C),
  my_blanks_star,
  ":",
  my_blanks_star,
  my_sql_user_identifier(N).
my_possibly_qualified_relation(N) -->
  my_sql_user_identifier(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

process_command(SCommand,CId,yes) :- 
  reset_syntax_error, 
  lang_interpreter_cmd(Language,SCommand,CInput),
%  \+ (my_blanks_star(CInput,[])),
  CInput\==[],
  !,
%  save_state_flags, % WARNING 20-05-2020
  state_flags_number(StateNbr),
  save_state_flags(StateNbr),
  language(CurrentLanguage),
  processC(Language,[],_,_),
  nl_compact_log,
  atom_concat('process_',Language,F),
  P=..[F,CInput],
  catch(
    (call(P)
     ->
      processC(CurrentLanguage,[],_,_),
      display_command_elapsed_time(CId),
      nl_compact_log
     ;
      processC(CurrentLanguage,[],_,_),
      process_command_error(SCommand)
    ),
    M, (my_exception_handling(M), complete_pending_tasks(M))),
  restore_state_flags(StateNbr).
process_command(SCommand,CId,Continue) :- 
  my_command_input(SCommand,compact_listings),
  !,
  (parse_command(Command,Arguments,NVs,SCommand,[]),
   !,
   store_query_elapsed_time(parsing),
   compact_listings(CL),
   silent(SIL),
   (((CL==off, Arguments==[off]);(CL==off, Arguments==[]);(CL==on, Arguments==[off])),SIL==off -> nl_log ; true),
   processC(Command,Arguments,NVs,Continue),
   display_command_elapsed_time(CId),
   (((CL==off, Arguments==[off]);(CL==off, Arguments==[]);(CL==on, Arguments==[off])),SIL==off -> nl_log ; true)
  ; 
   process_command_error(SCommand)
  ).
process_command(SCommand,CId,Continue) :- 
  my_command_input(SCommand,_),
%  (command(_,_,s(SCommandName,_),Command,_,_,_) -> true ; SCommandName=Command),
%  SCommandName=Command,
  parse_command(Command,Arguments,NVs,SCommand,[]),
  !,
  Command=..[CommandName|Params], % /repeat N is represented as repeat(N) to keep track of iterations
  (command(_,_,y(MCommandName,_),CommandName,_,_,_),
   MCommand=..[MCommandName|Params]
   ->
    true
   ;
    MCommandName=CommandName,
    MCommand=Command
  ),
  nl_tapi_log(MCommandName,Arguments),
  store_query_elapsed_time(parsing),
  (host_safe(on),
   command(Category,_,_,MCommandName,_,_,_),
   ((\+ host_safe_category(Category), 
     \+ safe_command(MCommandName,Arguments))
    ;
     unsafe_command(MCommandName))
   ->
    write_info_log(['This command cannot be executed in host safe mode.'])
   ;
    (silent_command(MCommand,Arguments) -> push_flag(silent,on,OldFlag) ; true),
    processCE(MCommand,Arguments,NVs,Continue),
    (silent_command(MCommand,Arguments) -> pop_flag(silent,OldFlag) ; true)
  ),
  !,
  display_command_elapsed_time(CId),
  nl_tapi_log(MCommand,Arguments).
% process_command(SCommand,CId,Continue) :- 
%   my_command_input(SCommand,Command), 
%   parse_command(Command,Arguments,NVs,SCommand,[]),
%   Command=..[CommandName|Params], % /repeat N is represented as repeat(N) to keep track of iterations
%   (command(_,_,y(MCommandName,_),CommandName,_,_,_), % Synonyms
%    MCommand=..[MCommandName|Params],
%    !
%   ;
%    MCommandName=CommandName,
%    MCommand=Command
%   ),
%   nl_tapi_log(MCommandName,Arguments),
%   store_query_elapsed_time(parsing),
%   (host_safe(on),
%    command(Category,_,_,MCommandName,_,_,_),
%    ((\+ host_safe_category(Category), 
%      \+ safe_command(MCommandName))
%     ;
%      unsafe_command(MCommandName))
%    ->
%     write_info_log(['This command cannot be executed in host safe mode.'])
%    ;
%     (silent_command(MCommand,Arguments) -> push_flag(silent,on,OldFlag) ; true),
%     processC(MCommand,Arguments,NVs,Continue),
%     (silent_command(MCommand,Arguments) -> pop_flag(silent,OldFlag) ; true)
%   ),
%   !,
%   display_command_elapsed_time(CId),
%   nl_tapi_log(MCommand,Arguments).
process_command(SCommand,_CId,yes) :- 
  process_command_error(SCommand).
  
silent_command(silent,[Arg|_Arguments]) :-
  Arg\==on,
  Arg\==off.
  
% Processing syntax errors from queries sent with commands:
process_command_error(SCommand) :-
  process_error(SCommand).
process_command_error(_SCommand) :-
  write_error_log(['Syntax error in command and/or its argument(s)']),
  nl_compact_log.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCESSING individual commands

processCE(Command,Arguments,NVs,Continue) :-
  processC(Command,Arguments,NVs,Continue),
  !.
processCE(Command,Arguments,_NVs,_Continue) :-
  my_raise_exception(generic,syntax(['Internal error processing: ',[Command|Arguments]]),[]).


processC(assert,[T],NVs,yes) :-
  !, 
  assert_rule((T,NVs),[],datalog,[],CRNVs,_ODLIds,_Unsafe,Error),
  (Error==true -> 
    true
    ;
    clear_et, 
    update_stratification_add_ruleNVs(CRNVs),
    rule_pred(T,TableName/_),
    set_m_flag(table_modified(connection_table('$des',TableName))),
    set_flag(db_modified(true)), % The database has changed since the last commit
    write_info_verb_log(['Rule asserted.'])
  ).
processC(retract,[R],_NVs,yes) :-
  !,
  rule_pred(R,TableName/_),
  set_m_flag(table_modified(connection_table('$des',TableName))), 
  set_flag(db_modified(true)), % The database has changed since the last commit
  retract_source_rule(R,_Error).
%  (var(Error), system_mode(fuzzy) -> update_fuzzy_expansion(retract,R) ; true). % Now, done in my_retract
processC(retractall,[H],_NVs,yes) :-
  !, 
  get_filtered_source_dlrules(head,H,[],SDLs),
  (SDLs==[] -> 
    write_warning_log(['Nothing retracted.'])
   ;
    retract_source_dlrule_list(SDLs,RSDLs,RODLs,_Error),
    (RSDLs\==[] ->
      clear_et,
      compute_stratification
     ;
      true
    ),
    rule_pred(H,TableName/_),
    set_m_flag(table_modified(connection_table('$des',TableName))),
    set_flag(db_modified(true)), % The database has changed since the last commit
    display_tuples_and_nbr_info(RSDLs,RODLs)
  ).
processC(fail,[],_NVs,yes) :- % Just to test a system failure
  !, 
  fail.
processC(des_sql_solving,[],_NVs,yes) :-
  !, 
  des_sql_solving(Switch),
  write_info_log(['Forcing DES solving for external DBMS is ', Switch, '.']).
processC(des_sql_solving,[Switch],_NVs,yes) :-
  process_set_binary_flag(des_sql_solving,'Forcing DES solving for external DBMS is',Switch).
processC(sql_semantic_check,[],_NVs,yes) :-
  !, 
  sql_semantic_check(Switch),
  write_info_log(['Checking possible SQL semantic errors is ', Switch, '.']).
processC(sql_semantic_check,[Switch],_NVs,yes) :-
  process_set_binary_flag(sql_semantic_check,'Checking possible SQL semantic errors is',Switch).
processC(des,[],NVs,yes) :-
  processC(datalog,[],NVs,yes).
processC(des,[Input],_NVs,yes) :-
  !,
%  save_state_flags,
  state_flags_number(StateNbr),
  save_state_flags(StateNbr),
  set_flag(des_sql_solving,on),
  %set_flag(compact_listings,on),
  atom_codes(Input,InputStr),
%  process_input(InputStr,_Continue,no_nl,_),
  catch(process_input_no_timeout(InputStr,_Continue,no_nl,_), M, (my_exception_handling(M), complete_pending_tasks(M))),
%  restore_state_flags.
  restore_state_flags(StateNbr).
processC(breakpoint,[],_NVs,yes) :-
  debug,
  spy(deb),
  deb.
%% BEGIN TAPI commands and statements
processC(tapi,[Input],_NVs,yes) :-
  !,
  (tapi_log(off) -> disable_log ; true),
%  save_state_flags,
  state_flags_number(StateNbr),
  save_state_flags(StateNbr),
  set_flag(tapi,on),
  set_flag(compact_listings,on),
  set_flag(language,datalog),
  set_flag(verbose,off),
  atom_codes(Input,InputStr),
%  process_input(InputStr,_Continue,no_nl,_), 
  catch(process_input_no_timeout(InputStr,_Continue,no_nl,_), M, (my_exception_handling(M), complete_pending_tasks(M))),
%  restore_state_flags.
  restore_state_flags(StateNbr),
  (tapi_log(off) -> resume_log ; true).
processC(mtapi,[],_NVs,yes) :-
  !,
  read_lines_up_to_eot(InputStr,Lines,EOF,EOT),
  (current_batch_ID(ID) % Update batch file current line if in batch processing
   ->
    batch(ID,CurrentLine,File,Stream,Mode),
    NewLine is CurrentLine+Lines,
    set_flag(batch(ID,NewLine,File,Stream,Mode)),
    write_string_log(InputStr),
    (EOT=end_of_transmission -> write_string_log("$eot\n") ; true),
    (EOF=end_of_file -> nl_log ; true)
   ;
    true),
   (\+ current_batch_ID(_), 
    my_log([_|_])
    ->
     write_only_to_log(InputStr), 
     nl_only_to_log
    ;
     true),
  state_flags_number(StateNbr),
  save_state_flags(StateNbr),
  set_flag(tapi,on),
  set_flag(compact_listings,on),
  set_flag(verbose,off),
  set_flag(multiline,on),
  catch(process_input_no_timeout(InputStr,_Continue,_NL,_), M, (my_exception_handling(M), complete_pending_tasks(M))),
  restore_state_flags(StateNbr).
processC(tapi_log,[],_NVs,yes) :-
  !, 
  tapi_log(Switch),
  write_info_log(['TAPI logging is ', Switch, '.']).
processC(tapi_log,[Switch],_NVs,yes) :-
  process_set_binary_flag(tapi_log,'TAPI logging is',Switch).
processC(test_tapi,[],_NVs,yes) :-
  !,
  push_flag(tapi,on,CurrentValue),
  write_tapi_success,
  pop_flag(tapi,CurrentValue).
%% END TAPI commands and statements
processC(timeout,[T,Input],_NVs,yes) :-
  !,
  atom_codes(Input,InputStr),
  call_with_timeout(process_input_no_timeout(InputStr,_Continue,no_nl,_),T).
%   my_timeout(process_input(InputStr,_Continue,no_nl),T,S),
%   (S==time_out
%    -> 
%     (output(on) -> write('\r') ; true), % If running info is enabled, overwrite the message
%     write_info_log(['Timeout exceeded.          ']), 
%     complete_pending_tasks('$aborted'), 
%     set_flag(error(2)) 
%    ;
%     true).
processC(set_timeout,[],_NVs,yes) :-
  !,
  global_timeout(T),
  (T==off
   ->
    write_info_log(['Default timeout is disabled.'])
   ;
    write_info_log(['Default timeout is ', T, ' s.'])
  ).
processC(set_timeout,[T],_NVs,yes) :-
  !,
  global_timeout(OT),
  ((number(T), T>0 ; T==off)
   ->
    set_flag(global_timeout,T),
    (T==off
     ->
      (OT==off
       ->
        write_info_verb_log(['Default timeout already disabled.'])
       ;
        write_info_verb_log(['Default timeout disabled.'])
      )
     ;
      write_info_verb_log(['Default timeout set to ',T,' s.'])
    )
   ;
    write_error_log(['Incorrect argument. Should be either a positive non-zero integer or ''off''.'])
  ).
processC(input,[Variable],_NVs,yes) :-
  !,
  restore_batch_user_input(Switch),
  readln(String,_E),
  restore_batch_file_input(Switch),
  name(Value,String),
  set_variable(Variable,Value).
processC(write,[Input],_NVs,yes) :-
  !,
%  atom_codes(Input,StrInput),
%  instance_system_vars(TInput,StrInput,[]),
%  write_string_log(TInput).
  write_log(Input).
processC(writeln,[Input],NVs,yes) :-
  !,
  processC(write,[Input],NVs,yes),
  nl_log.
% processC(writeqln,[Input],NVs,yes) :-
%   !,
%   processC(write,['$quoted'(Input)],NVs,yes),
%   nl_log.
processC(write_to_file,[File,Input],_NVs,yes) :-
  !,
  my_absolute_filename(File,AFN),
  open(AFN,append,S),
%  atom_codes(Input,StrInput),
%  instance_system_vars(TInput,StrInput,[]),
%  atom_codes(T,Input),
  write(S,Input),
  flush_output(S),
  close(S).
processC(writeln_to_file,[File],_NVs,yes) :-
  !,
  my_absolute_filename(File,AFN),
  open(AFN,append,S),
  nl(S),
  flush_output(S),
  close(S).
processC(csv,[],_NVs,yes) :-
  !,
  (csv(CSV),
   CSV\==off
   ->
    write_info_log(['CSV dump to file ',CSV])
   ;
    write_info_log(['CSV dump is disabled'])
  ).
processC(csv,[File],_NVs,yes) :-
  !,
  (File==off
   -> 
    set_flag(csv,off),
    write_info_verb_log(['CSV dump disabled'])
   ;
    my_absolute_filename(File,AFN),
    set_flag(csv,AFN),
    write_info_verb_log(['CSV dump to file ',AFN])
  ).
processC(sql_left_delimiter,[],_NVs,yes) :-
  !,
  current_db(_,DBMS),
  my_sql_left_quotation_mark(LQstr,DBMS),
  write_string_log(LQstr),
  nl_log.
processC(sql_right_delimiter,[],_NVs,yes) :-
  !,
  current_db(_,DBMS),
  my_sql_right_quotation_mark(RQstr,DBMS),
  write_string_log(RQstr),
  nl_log.
processC(Quit,[],_NVs,no) :- 
  (Quit=q; Quit=quit; Quit=e; Quit=exit; Quit=halt), 
  !, 
  (my_log([_|_]) -> processC(nolog,[],_,_) ; true),
  close_dbs,
  halt.
processC(Terminate,[],_NVs,no) :- 
  (Terminate=terminate ; Terminate=t), 
  !.
processC(debug,[],_NVs,yes) :- 
  !,
  debug.
processC(Spy,[Predicate],_NVs,yes) :- 
  Spy=(spy), 
  !,
  my_spy(Predicate).
processC(NoSpyAll,[],_NVs,yes) :- 
  NoSpyAll=nospyall, 
  !,
  my_nospyall.
processC(NoSpy,[Predicate],_NVs,yes) :- 
  NoSpy=nospy, 
  !,
  my_nospy(Predicate).
processC(system,[Goal],_NVs,yes) :- 
  !, 
  (call(Goal) ->
    true
   ;
    write_log_list(['no',nl])
  ).
processC(Shell,[C],_NVs,yes) :- 
  (Shell=shell; Shell=s), 
  !, 
  my_shell(C,sync,S),
  set_flag(shell_exit_code,S),
  (S=0 -> 
    write_info_verb_log(['Operating system command executed.'])
   ;
    true % Error message handled by my_shell
  ).
processC(ashell,[C],_NVs,yes) :- 
  !, 
  my_shell(C,async,_S).
processC(Help,[],_NVs,yes) :- 
  (Help=h; Help=help), 
  !,
  display_help,
  write_tapi_success.
processC(Help,[KW],_NVs,yes) :- 
  (Help=h ; Help=help ; Help=apropos), 
  !,
  (atom_concat('/',CKW,KW), \+ atom_concat('/',_,KW), \+ KW=='', ! % Ensure we are not looking for the operators / and //
   ; CKW=KW),
  atom_codes(CKW,StrCKW),
  remove_initial_blanks(StrCKW,StrBKW),
  to_lowercase_char_list(StrBKW,StrLKW),
  atom_codes(BKW,StrLKW),
  !,
  display_help(BKW),
  write_tapi_success.
processC(save_ddb,[],NVs,yes) :-
  !, 
  processC(save_ddb,[force,'des.ddb'],NVs,yes).
processC(save_ddb,[File],NVs,yes) :-
  !, 
  (my_file_exists(File) ->
    write_log('Info: File exists already. Overwrite? (y/n) [n]: '),
    user_input_string(Str),
    ((Str=="y" ; Str=="Y") ->
      Continue=yes
      ;
     Continue=no)
   ;
    Continue=yes
  ),
  (Continue==yes ->
    processC(save_ddb,[force,File],NVs,yes)
   ;
    true
  ).
processC(save_ddb,[force,File],_NVs,yes) :-
  push_flag(output,only_to_log,OutputFlag),
  push_flag(development,off,DevelopmentFlag),
  push_flag(pretty_print,off,PrettyPrintFlag),
  disable_log,
  processC(log,[write,silent,File],[],yes),
%  list_rules_wo_number(0,_DLs),
  catch(
    (findall(RId,(my_foreign_key('$des',_F,_FK_AttNames,_ForeignTablename,_PK_AttNames,RIds), member(RId,RIds)),RIds),
     list_filtered_rules_wo_number(0,_D,[filter_rids(RIds)],_ODLs),
     list_constraint_rules),
    Message,
    true
    ),
  processC(nolog,[File],[],yes),
  resume_log,
  pop_flag(pretty_print,PrettyPrintFlag),
  pop_flag(development,DevelopmentFlag),
  pop_flag(output,OutputFlag),
  (nonvar(Message) -> write_exception_message(save_ddb,Message,[]) ; true).
processC(save_state,[],NVs,yes) :-
  processC(save_state,[force,'./des.sds'],NVs,yes).
processC(save_state,[File],NVs,yes) :-
  !, 
  (tapi(off),
   my_file_exists(File)
   ->
    write_log('Info: File exists already. Overwrite? (y/n) [n]: '),
    user_input_string(Str),
    ((Str=="y" ; Str=="Y") ->
      Continue=yes
      ;
     Continue=no)
   ;
    Continue=yes
  ),
  (Continue==yes
   ->
    processC(save_state,[force,File],NVs,yes)
   ;
    true
  ).
processC(save_state,[force,File],_NVs,yes) :-
%  push_flag(host_safe,off,OldFlag),
  save_state(File),
  write_tapi_success.
%  pop_flag(host_safe,OldFlag).
processC(restore_state,[],NVs,yes) :-
  DefaultFile='./des.sds',
  (my_file_exists(DefaultFile)
   ->
    processC(restore_state,[DefaultFile],NVs,yes)
   ;
    write_error_log(['Default file for saved state does not exist.'])
  ).
processC(restore_state,[File],_NVs,yes) :-
  restore_state(File).
processC(autosave,[],_NVs,yes) :-
  !, 
  autosave(Switch),
  write_info_log(['Database autosave is ', Switch, '.']).
processC(autosave,[Switch],_NVs,yes) :-
  process_set_toggle_binary_flag(autosave,'Database autosave is',Switch),
  autosave(NSwitch),
  process_autosave(NSwitch).
processC(Cat,[File],_NVs,yes) :-
  (Cat=cat ; Cat=type), 
  !, 
  (my_file_exists(File)
   ->
    cat_file(File),
    nl_log
   ;
    write_warning_log(['File not found ''',File,'''.'])
   ).
processC(Edit,[File],_NVs,yes) :-
  (Edit=edit ; Edit=e), 
  !, 
%  (my_file_exists(File) ->
    (editor(Editor) ->
      atom_concat_list([Editor,' ',File],C),
      my_shell(C,async,_S)
     ;
      write_error_log(['External editor has not been set. Please set it with the command /set_editor <your_editor>.'])
    )
%   ;
%    write_warning_log(['File not found ''',File,'''.'])
%   ).
  .
processC(set_editor,[],_NVs,yes) :-
  !, 
  (editor(Editor)
   ->
    write_info_log(['Current external editor is ',Editor,'.'])
   ;
    write_error_log(['External editor has not been set. Please set it with the command /set_editor <your_editor>.'])
  ).
processC(set_editor,[Editor],_NVs,yes) :-
  !, 
%  (my_file_exists(Editor) ->
    set_flag(editor,Editor),
    write_info_verb_log(['Current external editor set to ',Editor,'.'])
%   ;
%    write_error_log(['External editor has not been found.'])
%   ).
  .
processC(restore_ddb,[],NVs,yes) :-
  !,
  processC(restore_ddb,['des.ddb'],NVs,yes).
processC(Consult,Files,_NVs,yes) :-
  (Consult=consult ; Consult=c ; Consult=restore_ddb), 
  !, 
  (Files==[]
   ->
    write_warning_log(['Nothing consulted.'])
   ;
    reset_database,
    remove_duplicates_var(Files,UFiles),
    consult_DL_list(UFiles,Success),
    (Success -> compute_stratification ; true)
  ),
  write_tapi_eot.
processC(Reconsult,Files,_NVs,yes) :-
  (Reconsult=reconsult; Reconsult=r), 
  !, 
  (Files==[]
   ->
    write_warning_log(['Nothing reconsulted.'])
   ;
    clear_et, 
    remove_duplicates_var(Files,UFiles),
    consult_DL_list(UFiles,Success),
    (Success -> compute_stratification ; true)
  ),
  write_tapi_eot.
processC(abolish,[],_NVs,yes) :-
  !, 
  drop_database.
processC(abolish,[N/A],_NVs,yes) :-
  !,
  abolish_relation(N,A).
processC(abolish,[N],_NVs,yes) :-
  !,
  abolish_relation(N,_A).
processC(drop_all_tables,[],_NVs,yes) :-
  !, 
  drop_all_tables(Ts),
  (Ts==[]
   ->
    true
   ;
    clear_et,
    compute_stratification,
    write_tapi_success).
processC(drop_all_views,[],_NVs,yes) :-
  !, 
  drop_all_views(Vs),
  (Vs==[]
   ->
    true
   ;
    clear_et,
    compute_stratification,
    write_tapi_success).
processC(drop_all_relations,[],_NVs,yes) :-
  !, 
  drop_all_relations(Rs),
  (Rs==[]
   ->
    true
   ;
    clear_et,
    compute_stratification).
processC(close_persistent,[],_NVs,yes) :-
  !,
  close_single_persistent.
processC(close_persistent,[N],_NVs,yes) :-
  !,
  close_persistent(N).
processC(drop_ic,[Constraint],NVs,yes) :-
  !,
  drop_ic(Constraint,NVs,_Error).
processC(drop_assertion,[Assertion],_NVs,yes) :-
  !,
  drop_assertion(Assertion).
processC(listing,[],_NVs,yes) :-
  !,
  list_rules(0,delim).
processC(listing,[N/A],_NVs,yes) :-
  !,
  list_rules(N,A,0,delim).
processC(listing,[PN],_NVs,yes) :-
  (PN = -(N); PN=N),
  atom(N),
  !,
  list_rules(PN,0,delim).
processC(listing,[H],_NVs,yes) :-
  H\=':-'(_H,_T),
  !,
  list_rules_from_head(H,0,delim).
processC(listing,[R],_NVs,yes) :-
  !,
  list_rules_from_rule(R,0,delim).
processC(listing_asserted,[],_NVs,yes) :-
  !,
  list_filtered_rules(0,delim,[asserted]).
processC(listing_asserted,[N/A],_NVs,yes) :-
  !,
  list_filtered_rules(N,A,0,delim,[asserted]).
processC(listing_asserted,[PN],_NVs,yes) :-
  (PN = -(N); PN=N),
  atom(N),
  !,
  list_filtered_rules(PN,0,delim,[asserted]).
processC(listing_asserted,[H],_NVs,yes) :-
  H\=':-'(_H,_T),
  !,
  list_filtered_rules_from_head(H,0,delim,[asserted]).
processC(listing_asserted,[R],_NVs,yes) :-
  !,
  list_filtered_rules_from_rule(R,0,delim,[asserted]).
processC(list_sources,[N/A],_NVs,yes) :-
  !,
  list_sources(N,A).
processC(list_predicates,[],_NVs,yes) :-
  !,
  display_pdg(nodes).
processC(list_relations,[],_NVs,yes) :-
  !,
  list_relations.
processC(list_tables,[],_NVs,yes) :-
  !,
  list_tables.
processC(list_table_schemas,[],_NVs,yes) :-
  !,
  list_table_schemas.
processC(list_views,[],_NVs,yes) :-
  !,
  list_views.
processC(list_view_schemas,[],_NVs,yes) :-
  !,
  list_view_schemas.
processC(list_table_constraints,[Tablename],_NVs,yes) :-
  !,
  exist_table(Tablename),
  list_table_constraints(Tablename),
  write_tapi_eot.
processC(relation_schema,[Relation],_NVs,yes) :-
  !,
  exist_relation(Relation),
  list_relation_schema(Relation),
  write_tapi_eot.
processC(check_db,[],_NVs,yes) :-
  !,
  check_db.
processC(DBSchema,[],_NVs,yes) :-
  (DBSchema = dbschema
  ;
   DBSchema = db_schema),
  !,
  list_schema.
processC(DBSchema,[Connection:Relation],_NVs,yes) :-
  (DBSchema = dbschema
  ;
   DBSchema = db_schema),
  !,
  my_odbc_identifier_name(Connection,Relation,ODBCRelation),
  list_schema(Connection,ODBCRelation).
processC(DBSchema,[Relation],_NVs,yes) :-
  (DBSchema = dbschema
  ;
   DBSchema = db_schema),
  current_db(Connection),
  my_odbc_identifier_name(Connection,Relation,ODBCRelation),
  relation_exists(ODBCRelation),
  !,
  list_schema(Connection,ODBCRelation).
processC(DBSchema,[Connection],_NVs,yes) :-
  (DBSchema = dbschema
  ;
   DBSchema = db_schema),
  !,
  list_schema(Connection,_Relation).
processC(dbs_schemas,[],_NVs,yes) :-
  (tapi(off)
   ->
    findall(_,(opened_db(Connection), list_schema(Connection,_Relation), nl_compact_log),_)
  ;
    % Write a term for all database schemas
    dbs_schemas(DBsSchemas),
    write_log_list(['$quoted'(DBsSchemas),nl])
  ).
% processC(db_schema_modified,[],_NVs,yes) :-
%   db_schema_modified(Boolean),
%   write_log_list([Boolean,nl]).
processC(describe,[Relation],NVs,C) :-
  (relation_exists(Relation)
   ->
    processC(db_schema,[Relation],NVs,C)
   ;
    write_error_log(['Table or view ''', Relation,''' does not exist'])
  ).
processC(list_et,[],_NVs,yes) :-
  !,
  list_et.
processC(list_et,[N/A],_NVs,yes) :-
  !,
  list_et(N/A).
processC(list_et,[N],_NVs,yes) :-
  !,
  list_et(N).
processC(clear_et,[],_NVs,yes) :- 
  !,
  verb_clear_et.
processC(builtins,[],_NVs,yes) :-
  !,
  list_builtins,
  write_tapi_success.
processC(command_assertions,[],_NVs,yes) :-
  !,
  list_command_assertions,
  write_tapi_success.
processC(datalog,[],_NVs,yes) :-
  !,
  set_flag(language(datalog)),
  write_info_verb_log(['Switched to Datalog interpreter.']).
processC(prolog,[],_NVs,yes) :-
  !,
  set_flag(language(prolog)),
  write_info_verb_log(['Switched to Prolog interpreter.']).
processC(sql,[],_NVs,yes) :-
  !,
  set_flag(language(sql)),
  write_info_verb_log(['Switched to SQL interpreter.']).
processC(ra,[],_NVs,yes) :-
  !,
  set_flag(language(ra)),
  write_info_verb_log(['Switched to RA interpreter.']).
processC(drc,[],_NVs,yes) :-
  !,
  set_flag(language(drc)),
  write_info_verb_log(['Switched to DRC interpreter.']).
processC(trc,[],_NVs,yes) :-
  !,
  set_flag(language(trc)),
  write_info_verb_log(['Switched to TRC interpreter.']).
processC(system_mode,[],_NVs,yes) :-
  !, 
  system_mode(Switch),
  write_info_log(['System mode is ', Switch, '.']).
processC(system_mode,[Mode],_NVs,yes) :-
  !, 
  system_mode(CMode),
  process_set_command_flag(system_mode,'System mode is',Mode,[des,fuzzy],Result),
  ((CMode==Mode ; Result==error) -> true ; process_system_mode_change(CMode,Mode)).
% processC(system_mode,[],_NVs,yes) :-
%   !,
%   system_mode(Mode),
%   write_info_log(['System mode is ''', Mode, '''.']).
% processC(system_mode,[Mode],_NVs,yes) :-
%   !,
%   system_mode(OldMode),
%   (OldMode==Mode
%    ->
%     write_warning_log(['The system is already in mode ''',Mode,'''.'])
%    ;
%     ((Mode==des ; Mode==hrsql)
%      ->
%       set_flag(system_mode(Mode)),
%       (Mode==des
%        ->
%         processC(optimize_st,[off],[],_),
%         processC(silent,[off],[],_),
%         compute_stratification
%        ;
%         processC(silent,[on],[],_),
%         processC(optimize_st,[max],[],_),
%         processC(compact_listings,[on],[],_),
%         processC(multiline,[on],[],_),
%         processC(type_casting,[on],[],_),
%         compute_stratification
%       ),
%       write_info_verb_log(['Current mode is ''',Mode,'''.'])
%      ;
%       write_error_log(['Incorrect mode. Possible values are ''des'' and ''hrsql''.'])
%     )
%   ).
% processC(hrsql,[Connection],_NVs,yes) :-
%   !, 
%   process_hrsql_command(hrsql,[Connection]).
% processC(load_db,[File],_NVs,yes) :-
%   !, 
%   process_load_db_command(load_db,[File]).
% processC(load_hq,[File],_NVs,yes) :-
%   !, 
%   process_load_hq_command(load_db,[File]).
% processC(process_db,[],_NVs,yes) :-
%   !, 
%   (system_mode(hrsql)
%    ->
%     (hrsql_file(_File)
%      ->
%       process_process_db_command(process_db,[])
%      ;
%       write_error_log(['HR-SQL database not loaded yet. Use: /load_db File'])
%     )
%    ;
%     write_error_log(['Cannot process HR-SQL database in this mode. To switch mode, type: /system_mode hrsql'])
%   ).
% processC(transform_db,[],_NVs,yes) :-
%   !, 
%   (system_mode(hrsql)
%    ->
%     (hrsql_file(_File)
%      ->
%       process_transform_db_command(transform_db,[])
%      ;
%       write_error_log(['HR-SQL database not loaded yet. Use: /load_db File'])
%     )
%    ;
%     write_error_log(['Cannot process HR-SQL database in this mode. To switch mode, type: /system_mode hrsql'])
%   ).
% processC(process_db,[File],_NVs,yes) :-
%   !, 
%   (system_mode(hrsql)
%    ->
%     process_process_db_command(process_db,[File])
%    ;
%     write_error_log(['Cannot process HR-SQL database in this mode. To switch mode, type: /system_mode hrsql'])
%   ).
% processC(sql,[Query],_NVs,yes) :-
%   !,
%   retract(simplification(S)),
%   assertz(simplification(on)),
%   reset_pred_id,
%   solve_sql_query(Query),
%   retract(simplification(on)), % WARNING: If Query fails, simplification mode is lost
%   assertz(simplification(S)).
processC(cd,[],_NVs,yes) :-
  !,
  start_path(Path),
  cd_path(Path).
processC(cd,[Path],_NVs,yes) :-
  !,
  cd_path(Path).
processC(pwd,[],_NVs,yes) :-
  !,
  pwd_path.
processC(LS,[],_NVs,yes) :-
  (LS=ls
  ;
   LS=dir),
  !,
  ls.
processC(LS,[P],_NVs,yes) :-
  (LS=ls
  ;
   LS=dir),
  !,
  ls(P).
processC(RM,[FileName],_NVs,yes) :-
  (RM=rm
  ;
   RM=del),
  !,
  rm_file(FileName).
processC(CP,[FromFile,ToFile],_NVs,yes) :-
  (CP=cp
  ;
   CP=copy),
  !,
  copy_file(FromFile,ToFile).
processC(safety_warnings,[],_NVs,yes) :-
  !, 
  safety_warnings(Switch),
  write_info_log(['Safety warnings are ', Switch, '.']).
processC(safety_warnings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(safety_warnings,'Safety warnings are',Switch).
processC(singleton_warnings,[],_NVs,yes) :-
  !, 
  singleton_warnings(Switch),
  write_info_log(['Singleton warnings are ', Switch, '.']).
processC(singleton_warnings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(singleton_warnings,'Singleton warnings are',Switch).
processC(type_casting,[],_NVs,yes) :-
  !, 
  type_casting(Switch),
  write_info_log(['Automatic type casting is ', Switch, '.']).
processC(type_casting,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(type_casting,'Automatic type casting is',Switch,Result),
  (Result == changed -> recompile_des_views ; true).
processC(undef_pred_warnings,[],_NVs,yes) :-
  !, 
  undef_pred_warnings(Switch),
  write_info_log(['Undefined predicate warnings are ', Switch, '.']).
processC(undef_pred_warnings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(undef_pred_warnings,'Undefined predicate warnings are',Switch).
processC(show_compilations,[],_NVs,yes) :-
  !, 
  show_compilations(Switch),
  write_info_log(['Display of compilations is ', Switch, '.']).
processC(show_compilations,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(show_compilations,'Display of compilations is',Switch).
processC(show_sql,[],_NVs,yes) :-
  !, 
  show_sql(Switch),
  write_info_log(['Display of compiled SQL statements is ', Switch, '.']).
processC(show_sql,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(show_sql,'Display of compiled SQL statements is',Switch).
processC(fp_info,[],_NVs,yes) :-
  !, 
  fp_info(Switch),
  write_info_log(['Display of fixpoint info is ', Switch, '.']).
processC(fp_info,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(fp_info,'Display of fixpoint info is',Switch).
processC(running_info,[],_NVs,yes) :-
  !, 
  running_info(Switch),
  write_info_log(['Display of running info is ', Switch, '.']).
% processC(running_info,[Switch],_NVs,yes) :-
%   !, 
%   process_set_binary_flag(running_info,'Display of running info is',Switch).
processC(running_info,[Mode],_NVs,yes) :-
  running_info(CMode),
  process_set_command_flag(running_info,'Display of running info is',Mode,[on,off,batch],Result),
  ((CMode==Mode ; Result==error) -> true ; set_flag(running_info,Mode)).
processC(verbose,[],_NVs,yes) :-
  !, 
  verbose(Switch),
  write_info_log(['Verbose output is ', Switch, '.']).
processC(verbose,[Switch],_NVs,yes) :-
  !, 
  process_set_toggle_binary_flag(verbose,'Verbose output is',Switch).
processC(silent,[],_NVs,yes) :-
  !, 
  silent(Switch),
  write_info_silent_log(['Silent batch output is ', Switch, '.']).
processC(silent,[Switch],_NVs,yes) :-
  (Switch==on ; Switch==off),
  !, 
  process_set_binary_flag(silent,'Silent batch output is',Switch).
processC(silent,[Input],_NVs,Continue) :-
  !,
  atom_codes(Input,InputStr),
  process_input_no_timeout(InputStr,Continue,_NL,_EchoLog).
processC(if,[Condition,Input],_NVs,Continue) :-
  !,
  my_term_to_string_pl(Condition,[quoted(true),portrayed(true)],ConditionStr,[]),
  push_flag(info,off,Old0),
  push_flag(display_answer,off,OldDA),
  push_flag(language,datalog,OldL),
  push_flag(keep_answer_table,on,OldKA),
  process_input_no_timeout(ConditionStr,Continue,_,_),
  pop_flag(keep_answer_table,OldKA),
  pop_flag(language,OldL),
  pop_flag(display_answer,OldDA),
  pop_flag(info,Old0),
  (answer_table(schema_data(_,[]))
   ->
    true
   ;
    atom_codes(Input,InputStr),
    push_flag(compact_listings,on,OldCL),
    process_input_no_timeout(InputStr,Continue,_,_),
    pop_flag(compact_listings,OldCL)
    ).
processC(set_flag,[Flag,Expression],_NVs,yes) :-
  !,
  eval_expr(Expression,Value,_R),
  set_flag(Flag,Value).
processC(set,[],_NVs,yes) :-
  !,
  list_user_variables.
processC(set,[Variable],_NVs,yes) :-
  !,
  list_user_variable(Variable).
processC(set,[Variable,Expression],_NVs,yes) :-
  !,
  eval_expr(Expression,Value,_R),
  set_variable(Variable,Value).
processC(current_flag,[Flag],_NVs,yes) :-
  !,
  F1=..[Flag,_],
  (current_predicate(_,F1) -> call(F1), write_info_log([F1]) ; true),
  F2=..[Flag,_,_],
  (current_predicate(_,F2) -> call(F2), write_info_log([F2]) ; true),
  F3=..[Flag,_,_,_],
  (current_predicate(_,F3) -> call(F3), write_info_log([F3]) ; true).
processC(duplicates,[],_NVs,yes) :-
  !, 
  duplicates(Switch),
  write_info_log(['Duplicates are ', Switch, '.']).
processC(duplicates,[Switch],_NVs,yes) :-
  !,
  (duplicates(off), Switch==on
   ->
%    my_idx_retractall(complete_flag(_P,_G,_CF,_CId)) 
    clear_et
   ;
    true),
  process_set_binary_flag(duplicates,'Duplicates are',Switch).
processC(compact_listings,[],_NVs,yes) :-
  !, 
  compact_listings(Switch),
  write_info_log(['Compact listings are ', Switch, '.']).
processC(compact_listings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(compact_listings,'Compact listings are',Switch).
processC(prompt,[],_NVs,yes) :-
  !, 
  prompt(Switch),
  write_info_log(['Current prompt is set to ''', Switch, '''.']).
processC(prompt,[Switch],_NVs,yes) :-
  !, 
  ((Switch == des ; Switch == des_db ; Switch == plain ; Switch == prolog ; Switch == no) ->
    set_flag(prompt,Switch),
    (Switch==no -> prompt(_,'') ; prompt(_,'|: ')),
    exec_if_verbose_on(processC(prompt,[],_,_))
    ;
    write_error_log(['Incorrect switch. Use ''des'', ''des_db'', ''plain'' or ''no'''])
    ).
processC(timing,[],_NVs,yes) :-
  !, 
  timing(Switch),
  write_info_log(['Elapsed time display is ', Switch, '.']).
processC(timing,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off ; LSwitch == detailed ) ->
    retractall(timing(_)), 
    assertz(timing(LSwitch)),
    exec_if_verbose_on(processC(timing,[],_,_))
    ;
    write_error_log(['Incorrect switch. Use ''on'', ''off'' or ''detailed'''])
    ).
processC(format_datetime,[],_NVs,yes) :-
  !, 
  format_datetime(Switch),
  write_info_log(['Formatting of date and time is ', Switch, '.']).
processC(format_datetime,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(format_datetime,'Formatting of date and time is',Switch).
processC(format_timing,[],_NVs,yes) :-
  !, 
  format_timing(Switch),
  write_info_log(['Formatting of timing is ', Switch, '.']).
processC(format_timing,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(format_timing,'Formatting of timing is',Switch).
processC(time,[Input],_NVs,yes) :-
  !, 
  push_flag(timing,detailed,CurrentValue),
  atom_codes(Input,InputStr),
  process_input_no_timeout(InputStr,_Continue,no_nl,_),
  pop_flag(timing,CurrentValue).
processC(time,[],_NVs,yes) :-
  !, 
  my_current_datetime((_Y,_M,_D,H,Mi,S)),
  format_time(H,Mi,S,FT),
  write_info_log(['Time: ', FT]).
processC(date,[],_NVs,yes) :-
  !, 
  my_current_datetime((Y,M,D,_H,_Mi,_S)),
  format_date(Y,M,D,FD),
  write_info_log(['Date: ', FD]).
processC(datetime,[],_NVs,yes) :-
  !, 
  my_current_datetime((Y,M,D,H,Mi,S)),
  format_date(Y,M,D,FD),
  format_time(H,Mi,S,FT),
  write_info_log(['Datetime: ', FD, ' ', FT]).
processC(date_format,[],_NVs,yes) :-
  !, 
  date_format_str(Format),
  write_info_log(['Date format is ', Format]).
processC(date_format,[NewFormat],_NVs,yes) :-
  !,
  remove_delimiting_chars(NewFormat,'''',UNewFormat),
  process_set_command_flag(date_format_str,'Date format is',UNewFormat,'$check'(check_date_format(UNewFormat,Format)),Result),
  (Result==changed->set_flag(date_format(Format));true).
processC(time_format,[],_NVs,yes) :-
  !, 
  time_format_str(Format),
  write_info_log(['Time format is ', Format]).
processC(time_format,[NewFormat],_NVs,yes) :-
  !,
  remove_delimiting_chars(NewFormat,'''',UNewFormat),
  process_set_command_flag(time_format_str,'Time format is',UNewFormat,'$check'(check_time_format(UNewFormat,Format)),Result),
  (Result==changed->set_flag(time_format(Format));true).
processC(pretty_print,[],_NVs,yes) :-
  !, 
  pretty_print(Switch),
  write_info_log(['Pretty print is ', Switch, '.']).
processC(pretty_print,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(pretty_print,'Pretty print is',Switch).
processC(safe,[],_NVs,yes) :-
  !, 
  safe(Switch),
  write_info_log(['Program transformation for safety is ', Switch, '.']).
processC(safe,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(safe,'Program transformation for safety is',Switch).
processC(simplification,[],_NVs,yes) :-
  !, 
  simplification(Switch),
  write_info_log(['Program simplification is ', Switch, '.']).
processC(simplification,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(simplification,'Program simplification is',Switch).
processC(reorder_goals,[],_NVs,yes) :-
  !, 
  reorder_goals(Switch),
  write_info_log(['Goal reordering is ', Switch, '.']).
processC(reorder_goals,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(reorder_goals,'Goal reordering is',Switch).
processC(unfold,[],_NVs,yes) :-
  !, 
  unfold(Switch),
  write_info_log(['Program unfolding is ', Switch, '.']).
processC(unfold,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(unfold,'Program unfolding is',Switch).
processC(db_rules,[],_NVs,yes) :-
  !, 
  findall(a,datalog(_,_,_,_,_,_,_),Rs),
  length(Rs,Nbr),
  set_flag(db_rules,Nbr),
  write_info_log(['Number of loaded rules in database: ', Nbr, '.']).
processC(display_statistics,[],_NVs,yes) :-
  !, 
  display_statistics(Switch),
  write_info_log(['Display Statistics is ', Switch, '.']).
processC(display_statistics,[Switch],_,yes) :-
  !, 
  process_set_binary_flag(display_statistics,'Display Statistics is',Switch,Result),
  (Result==changed, Switch==on
   ->
    processC(statistics,[Switch],_,_)
   ;
    true).
processC(statistics,[],_NVs,yes) :-
  !, 
  my_statistics(Switch),
  write_info_log(['Statistics collection is ', Switch, '.']),
  (Switch == on
   -> 
    display_statistics
   ;
    true).
processC(statistics,[Switch],_NVs,yes) :-
  !, 
  my_statistics(OldSwitch),
  process_set_binary_flag(my_statistics,'Statistics collection is',Switch),
  (Switch == on
   -> 
    (OldSwitch == off
     ->
      reset_statistics
     ;
      true
    ),
    display_statistics
   ;
    true).
processC(host_statistics,[Kw],_NVs,yes) :-
  !, 
  display_host_statistics(Kw).
processC(start_stopwatch,[],_NVs,yes) :-
  !, 
  start_stopwatch,
  verb_display_stopwatch.
processC(stop_stopwatch,[],_NVs,yes) :-
  !, 
  stop_stopwatch,
  verb_display_stopwatch.
processC(reset_stopwatch,[],_NVs,yes) :-
  !, 
  reset_stopwatch,
  verb_display_stopwatch.
processC(display_stopwatch,[],_NVs,yes) :-
  !, 
  display_stopwatch.
processC(license,[],_NVs,yes) :-
  !,
  display_license.
processC(output,[],_NVs,yes) :-
  !, 
  output(Switch),
  write_info_log(['Output is ', Switch, '.']).
processC(output,[Mode],_NVs,yes) :-
  !, 
  to_lowercase(Mode,LMode),
  ((LMode == on ; LMode == off; LMode == only_to_log) ->
    retractall(output(_)), 
    assertz(output(LMode)),
    exec_if_verbose_on(processC(output,[],_,_))
    ;
   write_error_log(['Incorrect mode. Use ''on'', ''off'' or ''only_to_log'''])
  ).
processC(display_banner,[],_NVs,yes) :-
  !, 
  display_banner(Switch),
  write_info_log(['Display banner is ', Switch, '.']).
processC(display_banner,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(display_banner,'Display banner is',Switch).
processC(display_answer,[],_NVs,yes) :-
  !, 
  display_answer(Switch),
  write_info_log(['Display of answers is ', Switch, '.']).
processC(display_answer,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(display_answer(_)), 
    assertz(display_answer(LSwitch)),
    exec_if_verbose_on(processC(display_answer,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(display_nbr_of_tuples,[],_NVs,yes) :-
  !, 
  display_nbr_of_tuples(Switch),
  write_info_log(['Display of the number of computed tuples is ', Switch, '.']).
processC(display_nbr_of_tuples,[Switch],_NVs,yes) :-
  process_set_binary_flag(display_nbr_of_tuples,'Display of the number of computed tuples is',Switch).
processC(order_answer,[],_NVs,yes) :-
  !, 
  order_answer(Switch),
  (Switch == on -> Message = ordered ; Message = 'not ordered by default'),
  write_info_log(['Display of answers is ', Message, '.']).
processC(order_answer,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(order_answer(_)), 
    assertz(order_answer(LSwitch)),
    exec_if_verbose_on(processC(order_answer,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(multiline,[],_NVs,yes) :-
  !, 
  multiline(Switch),
  write_info_log(['Multiline input is ', Switch, '.']).
processC(multiline,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(multiline(_)), 
    assertz(multiline(LSwitch)),
    exec_if_verbose_on(processC(multiline,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(indexing,[],_NVs,yes) :-
  !, 
  indexing(Switch),
  write_info_log(['Hash indexing on memo tables is ', Switch, '.']).
processC(indexing,[Switch],NVs,yes) :-
  !, 
  indexing(OldSwitch),
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(indexing(_)), 
    assertz(indexing(LSwitch)),
    exec_if_verbose_on(processC(indexing,[],_,_)),
    (OldSwitch == LSwitch -> true ; processC(clear_et,[],NVs,yes))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(batch,[],_NVs,yes) :-
  !, 
  batch(Switch),
  write_info_log(['Batch mode is ', Switch, '.']).
processC(batch,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(batch,'Batch mode is',Switch).
processC(check,[],_NVs,yes) :-
  !, 
  check_ic(Switch),
  write_info_log(['Integrity constraint checking is ', Switch, '.']).
processC(check,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(check_ic,'Integrity constraint checking is',Switch).
processC(keep_answer_table,[],_NVs,yes) :-
  !, 
  keep_answer_table(Switch),
  write_info_log(['Keeping answer table is ', Switch, '.']).
processC(keep_answer_table,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(keep_answer_table,'Keeping answer table is',Switch,Result),
  (Result == changed, Switch == on -> reset_answer_table ; true).
processC(nulls,[],_NVs,yes) :-
  !, 
  nulls(Switch),
  write_info_log(['Null values are ', Switch, '.']).
processC(nulls,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(nulls,'Nulls are',Switch).
processC(Host_safe,[],_NVs,yes) :-
  (Host_safe=host_safe ; Host_safe=sandboxed),
  !, 
  host_safe(Switch),
  write_info_log(['Host safe mode is ', Switch, '.']).
processC(Host_safe,[Switch],_NVs,yes) :-
  (Host_safe=host_safe ; Host_safe=sandboxed),
  !, 
  host_safe(CSwitch),
  (CSwitch==on,
   Switch==off
  ->
    write_info_log(['Once enabled, host safe mode cannot be disabled.'])
  ;
    process_set_binary_flag(host_safe,'Host safe mode is',Switch)
  ).
processC(host_safe_goal,[Goal],_NVs,yes) :-
  !, 
  (host_safe_goal(Goal)
  ->
    call(Goal)
  ;
    write_error_log(['Unsafe Prolog goal'])
  ).
processC(hypothetical,[],_NVs,yes) :-
  !, 
  hypothetical(Switch),
  write_info_log(['Hypothetical queries are ', Switch, '.']).
processC(hypothetical,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(hypothetical,'Hypothetical queries are',Switch).
processC(optimize,[],_NVs,yes) :-
  !, 
  processC(optimize_cc,[],_,_),
  processC(optimize_ep,[],_,_),
%  processC(optimize_edb,[],_,_),
  processC(optimize_nrp,[],_,_),
  processC(optimize_st,[],_,_),
  processC(optimize_sn,[],_,_),
  processC(indexing,[],_,_).
processC(optimize_cc,[],_NVs,yes) :-
  !, 
  optimize_cc(Switch),
  write_info_log(['Complete computations optimization is ', Switch, '.']).
processC(optimize_cc,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(optimize_cc,'Complete computations optimization is',Switch).
% processC(optimize_edb,[],_NVs,yes) :-
%   !, 
%   optimize_edb(Switch),
%   write_info_log(['Extensional database optimization is ', Switch, '.']).
% processC(optimize_edb,[Switch],_NVs,yes) :-
%   !, 
%   process_set_binary_flag(optimize_edb,'Extensional database optimization is',Switch).
processC(optimize_ep,[],_NVs,yes) :-
  !, 
  optimize_ep(Switch),
  write_info_log(['Extensional predicate optimization is ', Switch, '.']).
processC(optimize_ep,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(optimize_ep,'Extensional predicate optimization is',Switch).
processC(optimize_nrp,[],_NVs,yes) :-
  !, 
  optimize_nrp(Switch),
  write_info_log(['Non-recursive predicates optimization is ', Switch, '.']).
processC(optimize_nrp,[Switch],_NVs,yes) :-
  !, 
  optimize_nrp(OldSwitch),
  process_set_binary_flag(optimize_nrp,'Non-recursive predicates optimization is',Switch),
  (OldSwitch==on, Switch==off -> retractall(complete_flag(_,_,_,_,_)) ; true).
processC(optimize_st,[],_NVs,yes) :-
  !, 
  optimize_st(Switch),
  write_info_log(['Stratum optimization is ', Switch, '.']).
processC(optimize_st,[Switch],_NVs,yes) :-
  !, 
  optimize_st(OldSwitch),
  process_set_command_flag(optimize_st,'Stratum optimization is',Switch,[on,off,max],Result),
  ((OldSwitch == Switch ; Result==error) -> true ; compute_stratification).
processC(optimize_sn,[],_NVs,yes) :-
  !, 
  optimize_sn(Switch),
  write_info_log(['Differential semi-naive optimization is ', Switch, '.']).
processC(optimize_sn,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(optimize_sn,'Differential semi-naive optimization is',Switch).
processC(strata,[],_NVs,yes) :-
  !, 
  display_strata.
processC(strata,[N/A],_NVs,yes) :-
  !, 
  display_strata_for(N/A).
processC(strata,[N],_NVs,yes) :-
  !, 
  display_strata_for(N/_A).
processC(dependent_relations,[Relation/Arity|Ms],_NVs,yes) :-
%  exist_relation(RelationName),
%  current_db(Connection),
%  my_odbc_identifier_name(Connection,RelationName,ODBCRelationName),
  dependent_relations(Relation/Arity,Ms,Preds),
  write_list_log(Preds),
  write_tapi_eot.
processC(dependent_relations,[Relation|Ms],_NVs,yes) :-
%  exist_relation(RelationName),
%  current_db(Connection),
%  my_odbc_identifier_name(Connection,RelationName,ODBCRelationName),
  dependent_relations(Relation,Ms,RelationNames),
  write_list_log(RelationNames),
  write_tapi_eot.
processC(referenced_relations,[RelationName/Arity],_NVs,yes) :-
%  exist_relation(RelationName),
  current_db(Connection),
  my_odbc_identifier_name(Connection,RelationName,ODBCRelationName),
  referenced_relations(ODBCRelationName/Arity,Preds),
  write_list_log(Preds),
  write_tapi_eot.
processC(referenced_relations,[RelationName],_NVs,yes) :-
%  exist_relation(RelationName),
  current_db(Connection),
  my_odbc_identifier_name(Connection,RelationName,ODBCRelationName),
  referenced_relations(ODBCRelationName,RelationNames),
  write_list_log(RelationNames),
  write_tapi_eot.
processC(dangling_relations,[],_NVs,yes) :-
  pdg((_,Arcs)),
  (setof([RelationName,DependsOn],
         A1^A2^
         ((member((RelationName/A1+DependsOn/A2),Arcs) ; 
           member((RelationName/A1-DependsOn/A2),Arcs)), 
          \+ relation_exists(DependsOn)),
         RelationNameDependOnList)
   ->
    write_dangling_relations(RelationNameDependOnList)
   ;
    write_info_log(['There are no dangling relations.',nl])).
processC(pdg,[],_NVs,yes) :-
  !,
  display_pdg.
processC(pdg,[N/A],_NVs,yes) :-
  !, 
  display_sub_pdg_for(N/A).
processC(pdg,[N],_NVs,yes) :-
  !, 
  display_sub_pdg_for(N/_A).
processC(rdg,[],_NVs,yes) :-
  !,
  display_rdg.
processC(rdg,[N/A],_NVs,yes) :-
  !, 
  display_sub_rdg_for(N/A).
processC(rdg,[N],_NVs,yes) :-
  !, 
  display_sub_rdg_for(N/_A).
processC(external_pdg,[],_NVs,yes) :-
  !, 
  external_pdg(Switch),
  write_info_log(['External pdg construction is ', Switch, '.']).
processC(external_pdg,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(external_pdg,'External pdg construction is',Switch).
processC(log,[],_NVs,yes) :-
%  (Log==log -> (Flag=my_log(F,_), M='') ; (Log==logiql_log -> Flag=logiql_log(F,_), M='LogiQL ')), 
  !,
  (my_log([Log|Logs])
   -> 
    findall(F,member((F,_,_),[Log|Logs]),Fs),
    write_info_log(['Currently logging to ',Fs,'.'])
   ;
    write_info_log(['Logging disabled.'])
  ).
processC(log,[Mode,Output,File],_NVs,yes) :-
%  (Log==log -> (Flag=my_log(F,S), M='', N='/nolog') ; (Log==logiql_log -> Flag=logiql_log(F,S), M='LogiQL ', N='/logiql_nolog')), 
  !,
  my_log(Logs), 
  my_absolute_filename(File,F),
  (member((F,_,_),Logs)
   ->
    write_warning_log(['Logging to ''',File,''' already.'])
   ;
   (my_dir_file(F,AP,_FN),
    (my_directory_exists(AP)
     ->
      open(F,Mode,S),
      set_flag(my_log([(F,File,S)|Logs])), 
      (Output==silent
       ->
        true
       ;
        write_info_verb_log(['Logging enabled to ',F,'.'])
%       set_flag(logiql,on)
      )
     ;
      write_warning_log(['Directory ',AP,' does not exist.'])
    )
   )
  ). 
processC(nolog,[File],_NVs,yes) :-
  !, 
  my_log(Logs),
  ((Log=(File,_,_) % Select either the file as submitted by the user or the complete filename (including the path)
    ;
    Log=(_,File,_Stream)),
   member(Log,Logs)
   ->
    close_logs([Log]),
    remove_from_list(Log,Logs,NLogs),
    set_flag(my_log(NLogs)), 
    write_info_verb_log(['Logging to ', File,' disabled.'])
   ;
    write_warning_log(['No logging to ',File,' is currently enabled.'])
  ).
processC(nolog,[],_NVs,yes) :-
%  (Nolog==nolog -> (Flag=my_log(F,S), M='') ; (Nolog==logiql_nolog -> Flag=logiql_log(F,S), M='LogiQL ')), 
  !, 
  (Logs=[_|_], % At least one opened log
   my_log(Logs)
   ->
    findall(F,member((F,_,_),Logs),Fs),
    close_logs(Logs),
%    retractall(disabled_log(_)),
    set_flag(my_log([])), 
    write_info_verb_log(['Logging to ', Fs, ' disabled.'])
   ;
    write_warning_log(['Logging already disabled.'])
  ).
processC(ilog,[],_NVs,yes) :-
  !, 
  ilog(Switch),
  write_info_log(['Immediate log is ', Switch, '.']).
processC(ilog,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(ilog,'Immediate log is',Switch).
processC(Process,[F|Params],_NVs,Continue) :-
  (Process=process; Process=p), 
  !,
  processC_process([F|Params],Continue,full). % Full batch mode: no interactive user input
processC(Run,[F|Params],_NVs,Continue) :-
  (Run=run; Run=r), 
  !,
  processC_process([F|Params],Continue,semi). % Semi batch mode: interactive user input for selected commands such as /input
processC(repeat(N),[Input],_NVs,yes) :-
  !,
  atom_codes(Input,InputStr),
  length(L,N),
  (member(_,L),
   replace_system_flags(RInputStr,InputStr,[]),
   process_input_no_timeout(RInputStr,_Continue,no_nl,_),
   fail
  ;
   true).
processC(goto,[Label],_NVs,yes) :-
  !,
  (current_batch_ID(ID)
   ->
    batch(ID,CurrentLine,File,Stream,Mode),
    (batch_label(ID,Label,StreamPosition,Line)
     ->
      set_stream_position(Stream,StreamPosition)
     ;
      stream_find_label(Stream,Label,SkippedLines),
      Line is CurrentLine+SkippedLines
    ),
    set_flag(batch(ID,Line,File,Stream,Mode))
   ;
    write_error_log(['This command can only be used in a script'])
  ).
processC(set_default_parameter,[ParamVectorIndex,Value],_NVs,yes) :-
  !,
  (param_vector_i(ParamVectorIndex,_Value)
   ->
    true
   ;
    set_param_vector_i(ParamVectorIndex,Value)).
% processC(set_var,[Var,Value],_NVs,yes) :-
%   !,
%   set_flag(Var,Value).
% processC(version,[],_NVs,yes) :-
%   language(hrsql),
%   !, 
%   hrsql_version(V), 
%   write_info_log(['HR-SQL version ',V,'.']).
processC(version,[],_NVs,yes) :-
  !, 
  des_version(V), 
  des_date(D),
  des_time(T),
  write_info_log(['DES version ',V,' ',D,' ',T,'.']).
processC(prolog_system,[],_NVs,yes) :-
  !, 
  prolog_system(_P,V), 
  write_info_log(['Prolog engine: ',V,'.']).
processC(status,[],_NVs,yes) :-
  !, 
  display_status.
processC(restore_default_status,[],NVs,C) :-
  !, 
  processC(reset,[],NVs,C).
processC(reset,[],_NVs,yes) :-
  !, 
  set_initial_status.
processC(open_db,[Connection|Opts],_NVs,yes) :-
  !, 
  open_db(Connection,Opts,_Error).
processC(close_db,[],NVs,yes) :-
  !, 
  current_db(Connection),
  processC(close_db,[Connection],NVs,yes).
processC(close_db,[Connection],_NVs,yes) :-
  !, 
  (Connection=='$des'
   ->
    write_warning_log(['Default database ''$des'' cannot be closed.'])
   ;
    (my_persistent(Connection,_PredSchema)
     ->
      write_error_log(['Cannot close connection. There are persistent predicates on it.'])
     ;
      current_db(CurrConnection),
      my_close_odbc(Connection,CurrConnection),
      (CurrConnection==Connection
       ->
        processC(clear_et,[],[],yes), % Clear ET and compute stratification if the current DB was the one to close
        compute_stratification,
        set_flag(db_schema_modified(true))
       ;
        true
      ),
      write_tapi_success
    )
  ).
processC(close_dbs,[],_NVs,yes) :-
  !, 
  opened_dbs(Connections,_Options),
  (Connections==[]
   ->
    write_warning_log(['No opened ODBC connections.'])
   ;
    set_initial_db
   ).
processC(current_db,[],_NVs,yes) :-
  !, 
  current_db(Connection,DBMS),
  write_notapi_info_log(['Current database is ''',Connection,'''. DBMS: ',DBMS]),
  write_tapi_log_list([Connection,nl,DBMS,nl]).
processC(get_relation,['$des',answer],_NVs,yes) :-
  !,
  answer_table(T),
  write_log_list(['$quoted'(T),nl]).
processC(get_relation,[Connection,RelName],_NVs,yes) :-
  !, 
  (get_table_typed_schema(Connection,RelName,Schema)
   ->
    schema_to_user_schema(Schema,UserSchema),
    UserSchema=..[_|Cols],
    length(Cols,Arity),
    get_tuples_in_relation(Connection,RelName,Arity,DevTuples),
    hide_nulls(DevTuples,NTuples),
    format_solutions(NTuples,Tuples),
    clear_et,
    write_log_list(['$quoted'(schema_data(UserSchema,Tuples)),nl])
   ;
    write_error_log(['Table/View not declared'])
  ).
processC(relation_modified,[],_NVs,yes) :-
  !, 
  (setof(['$quoted'(Connection_Relation),nl],table_modified(Connection_Relation),TMs)
   ->
    concat_lists(TMs,Message),
    write_log_list(Message)
   ;
    true),
  write_tapi_eot.
processC(refresh_db,[],_NVs,yes) :-
  !, 
  current_db(Connection),
  refresh_db_metadata,
  (Connection=='$des'
   ->
    write_notapi_info_log(['Default database ''$des'' refreshed.'])
   ;
    write_notapi_info_log(['Local metadata from external database refreshed.'])),
  write_tapi_success.
processC(list_dbs,[],NVs,yes) :-
  processC(show_dbs,[],NVs,yes).
processC(show_dbs,[],_NVs,yes) :-
  !, 
  setof([Connection,nl],opened_db(Connection),LConnectionLines),
  concat_lists(LConnectionLines,ConnectionLines),
  write_log_list(ConnectionLines),
  write_tapi_eot.
processC(list_persistent,[],_NVs,yes) :-
  !, 
  my_nf_setof([Connection:PredSchema,nl],my_persistent(Connection,PredSchema),LConnectionLines),
  concat_lists(LConnectionLines,ConnectionLines),
  write_log_list(ConnectionLines),
  write_tapi_eot.
processC(list_undefined,[],_NVs,yes) :-
  !, 
  pdg((Nodes,_Arcs)),
  my_nf_setof([Predicate,nl],
    (member(Predicate,Nodes),
     \+ declared_predicate(Predicate)),
    LPredicateLines),
  concat_lists(LPredicateLines,PredicateLines),
  write_log_list(PredicateLines),
  write_tapi_eot.
processC(list_modes,[],NVs,yes) :-
  !, 
  processC(list_modes,[_],NVs,yes).
processC(list_modes,[N/A],_NVs,yes) :-
  !, 
  findall([PredModes,nl],(my_modes(N/A,Modes), PredModes=..[N|Modes]),LLines),
  my_mergesort(LLines,OLLines),
  concat_lists(OLLines,Lines),
  write_log_list(Lines),
  write_tapi_eot.
processC(list_modes,[N],NVs,yes) :-
  !, 
  processC(list_modes,[N/_A],NVs,yes).
processC(relation_exists,[Relation],_NVs,yes) :-
  !, 
  (relation_exists(Relation)
   ->
   write_tapi_true
   ;
   write_tapi_false
  ).
processC(is_empty,[Relation],_NVs,yes) :-
  !, 
  exist_relation(Relation),
  (is_empty_relation(Relation)
   ->
   write_tapi_true
   ;
   write_tapi_false
  ).
processC(use_ddb,[],NVs,Cont) :-
  !, 
  processC(use_db,['$des'],NVs,Cont).
processC(use_db,[Connection],NVs,yes) :-
  !, 
  (current_db(Connection) 
   ->
    write_warning_log(['Database already in use.'])
   ;
    (opened_db(Connection,_Handle,DBMS)
     ->
%       (Connection=='$des',
%        language(hrsql) 
%        ->
%         set_flag(language,datalog),
%         processC(use_db,[Connection],NVs,yes)
%         % write_error_log(['Cannot change to default deductive database for the HR-SQL system.'])
%        ;
      set_flag(current_db(Connection,DBMS)),
      % When switching between databases - either ODBC connection or default $des -
      % ET must be cleared since it may hold results from the origin database
      processC(clear_et,[],NVs,yes),
      push_flag(verbose,on,CurrentValue),
      compute_stratification,
      pop_flag(verbose,CurrentValue),
      write_info_verb_log(['Current database changed to ''',Connection,'''.'])
%       )
     ;
      write_warning_log(['Database is not opened yet. Opening it...']),
      processC(open_db,[Connection],NVs,yes)
      %write_error_log(['Database is not opened yet. Use /open_db'])
    )
  ).
processC(parse,[Input],_NVs,yes) :-
  !,
  atom_codes(Input,InputStr),
  (state_flag(0,language,Language) % Use the language as is (changed by TAPI to Datalog)
   ;
    language(Language)
  ),
  state_flags_number(StateNbr),
  save_state_flags(StateNbr),
  set_flag(parsing_only,on),
  set_flag(language,Language),
  catch(process_input_no_timeout(InputStr,_Continue,no_nl,_), M, (my_exception_handling(M), complete_pending_tasks(M))),
  restore_state_flags(StateNbr).
processC(mparse,[],_NVs,yes) :-
  !,
  read_lines_up_to_eot(InputStr,Lines,EOF,EOT),
  (current_batch_ID(ID) % Update batch file current line if in batch processing
   ->
    batch(ID,CurrentLine,File,Stream,Mode),
    NewLine is CurrentLine+Lines,
    set_flag(batch(ID,NewLine,File,Stream,Mode)),
    write_string_log(InputStr),
    (EOT=end_of_transmission -> write_string_log("$eot\n") ; true),
    (EOF=end_of_file -> nl_log ; true)
   ;
    true),
   (\+ current_batch_ID(_), 
    my_log([_|_])
    ->
     write_only_to_log(InputStr), 
     nl_only_to_log
    ;
     true),
  (state_flag(0,language,Language) % Use the language as is (changed by TAPI to Datalog)
   ;
    language(Language)
  ),
  state_flags_number(StateNbr),
  save_state_flags(StateNbr),
  set_flag(multiline,on),
  set_flag(parsing_only,on),
  set_flag(language,Language),
  catch(process_input_no_timeout(InputStr,_Continue,_NL,_), M, (my_exception_handling(M), complete_pending_tasks(M))),
  restore_state_flags(StateNbr).
processC(solve,[Body],NVs,Continue) :-
  !, 
  my_term_to_string(Body,BodyStr,NVs),
  (consult(_,_) -> compute_stratification ; true), % Solving during consulting requires pdg and strata computation
  process_input_no_timeout(BodyStr,Continue,_NL,_EchoLog).
processC(stop_batch,[],_NVs,stop_batch) :- % Stop all nested batch processing
  !.
processC(return,[],_NVs,return) :-      % Stop current batch processing and return a 0 code (parent batches may continue)
  !,
  set_flag(return_code,0).
processC(return,[Code],_NVs,return) :- % Stop current batch processing and returns Code (parent batches may continue)
  !,
  set_flag(return_code,Code).
processC(trace_sql,[ViewName],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_identifier_name(Connection,ViewName,ODBCViewName),
  exist_view(ODBCViewName),
  trace_sql(ODBCViewName,preorder).
processC(trace_sql,[ViewName,Ordering],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_identifier_name(Connection,ViewName,ODBCViewName),
  exist_view(ODBCViewName),
  trace_sql(ODBCViewName,Ordering).
processC(trace_datalog,[Query],NVs,yes) :-
  !, 
  functor(Query,F,A), 
  exist_user_predicate(F/A),
  trace_datalog(Query,NVs,preorder).
processC(trace_datalog,[Query,Ordering],NVs,yes) :-
  !, 
  trace_datalog(Query,NVs,Ordering).
processC(debug_datalog,[Goal],NVs,yes) :-
  !, 
  Level='p',
  processC(debug_datalog,[Goal,Level],NVs,yes).
processC(debug_datalog,[Goal,Level],NVs,yes) :-
  !, 
  debug_dl_plain(Goal,Level,NVs).
processC(debug_dl,[Name/Arity|Options],_NVs,yes) :-
  !, 
  (member(file(FileName),Options)
   ->
    (my_file_exists(FileName)
     ->
      debug_dl_full(Name,Arity,Options)
     ;
      write_warning_log(['File not found ''',FileName,'''.'])
     )
   ;
    debug_dl_full(Name,Arity,Options)
  ).
processC(debug_dl_answer,[CommandQuestion,Answer],_NVs,yes) :-
  !,
  (debug_dl_current_question(CurrentQuestion)
   ->
    debug_dl_question_command_question(Question,CommandQuestion),
    ((Question=CurrentQuestion ; CurrentQuestion==[])
     ->
      % relname_in_sql_question(Question,RelName),
      % complete_sql_process_answer(RelName,Answer,CAnswer),
      debug_dl_process_answer(Question,Answer)
     ;
      write_error_log(['Current question mismatch: ',CurrentQuestion,' vs. ',Question,'.'])
    )
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_dl_current_question,[],_NVs,yes) :-
  !,
  (debug_dl_current_question(Question)
   ->
    debug_dl_question_command_question(Question,CommandQuestion),
    write_log_list(['$quoted'(CommandQuestion),nl])
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_dl_explain,[],_NVs,yes) :-
  (debug_dl_buggy(BuggyNodes)
   ->
    display_dl_buggy_nodes(BuggyNodes)
   ;
    write_error_log(['Debugging session not finished yet.'])
  ).
processC(debug_dl_node_state,[],_NVs,yes) :-
  (debug_dl_current_question(_CurrentQuestion)
   ->
    display_dl_node_states
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_dl_set_node,[N/A,State],_NVs,yes) :-
  !,
  (debug_dl_check_valid_node(N/A)
   ->
    (debug_dl_check_valid_state_change(State)
     ->
      debug_dl_set_node(N/A,State)
     ;
      write_error_log(['Invalid state change.'])
    )
   ;
    write_error_log(['Invalid node.'])
  ).
processC(debug_dl_statistics,[],_NVs,yes) :-
  !,
  (debug_dl_current_question(_Question)
   ->
    display_debug_statistics(dl)
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_sql,[ViewName|Options],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_identifier_name(Connection,ViewName,ODBCViewName),
  exist_view(ODBCViewName),
  % complete_debug_sql_answer_from_options(ViewName,Options,ROptions),
  push_flag(sql_semantic_check,off,OldValue), % Disable SQL semantic checking
  debug_sql(ODBCViewName,Options),
  pop_flag(sql_semantic_check,OldValue).
processC(debug_sql_current_question,[],_NVs,yes) :-
  !,
  (debug_sql_current_question(Question)
   ->
    write_sql_question(Question),
    nl_log
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_sql_answer,[Question,Answer],_NVs,yes) :-
  !,
  (debug_sql_current_question(CurrentQuestion)
   ->
    (Question==CurrentQuestion
     ->
      % relname_in_sql_question(Question,RelName),
      % complete_sql_process_answer(RelName,Answer,CAnswer),
      debug_sql_process_answer(Question,Answer)
     ;
      write_error_log(['Current question mismatch: ',CurrentQuestion,' vs. ',Question,'.'])
    )
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_sql_node_state,[],_NVs,yes) :-
  (debug_sql_current_question(_CurrentQuestion)
   ->
    display_sql_node_states
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(debug_sql_set_node,[Node,State],_NVs,yes) :-
  !,
  (debug_sql_check_valid_node(Node)
   ->
    (debug_sql_check_valid_state_change(State)
     ->
      debug_sql_set_node(Node,State)
     ;
      write_error_log(['Invalid state change.'])
    )
   ;
    write_error_log(['Invalid node name.'])
  ).
processC(debug_sql_statistics,[],_NVs,yes) :-
  !,
  (debug_max_questions(_) % Set when a session is started
   ->
    display_debug_statistics(sql)
   ;
    write_error_log(['Debugging session not started yet.'])
  ).
processC(test_case,[ViewName|Options],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_identifier_name(Connection,ViewName,ODBCViewName),
  process_test_case(ODBCViewName,Options).
processC(tc_size,[],_NVs,yes) :-
  !, 
  tc_size(Min,Max),
  write_info_log(['Test case size is set between ', Min, ' and ', Max, '.']).
processC(tc_size,[Min,Max],NVs,yes) :-
  !, 
  (number(Min), number(Max), Min>0, Min=<Max ->
    set_flag(tc_size,Min,Max),
    exec_if_verbose_on(processC(tc_size,[],NVs,_))
   ;
    write_error_log(['Incorrect parameter(s).'])).
processC(tc_domain,[],_NVs,yes) :-
  !, 
  tc_domain(Min,Max),
  write_info_log(['Test case domain is set between ', Min, ' and ', Max, '.']).
processC(tc_domain,[Min,Max],NVs,yes) :-
  !, 
  (number(Min), number(Max), Min=<Max ->
    set_flag(tc_domain,Min,Max),
    exec_if_verbose_on(processC(tc_domain,[],NVs,_))
   ;
    write_error_log(['Incorrect parameter(s).'])).
processC(development,[],_NVs,yes) :-
  !, 
  development(Switch),
  write_info_log(['Development listings are ', Switch, '.']).
processC(development,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(development,'Development listings are',Switch).
processC(list_lex_datalog,[],_NVs,yes) :-
  !, 
  setof(KW,datalog_keyword(KW),Cs),
  (member(C,Cs), write_log_list([C,nl]), fail ;  true).
% processC(list_lex_sql,[],_NVs,yes) :-
%   !, 
%   setof(KW,sql_identifier(KW),Cs),
%   findall(_,(member(C,Cs),write_log_list([C,nl])),_).
processC(list_lex_sql_kws,[],_NVs,yes) :-
  !, 
  setof(KW,sql_keyword(KW),Cs),
  (member(C,Cs), write_log_list([C,nl]), fail ; true).
processC(list_lex_sql_kws,[acide],_NVs,yes) :-
  !, 
  setof(KW,sql_keyword(KW),Cs),
  (member(C,Cs), write_log_list(['              <string>',C,'</string>',nl]), fail ; true).
processC(list_lex_fnops,[],_NVs,yes) :-
  !, 
  setof(KW,sql_function_operator(KW),Cs),
  (member(C,Cs), write_log_list([C,nl]), fail ; true).
processC(list_lex_fnops,[acide],_NVs,yes) :-
  !, 
  setof(KW,sql_function_operator(KW),Cs),
  (member(C,Cs), write_log_list(['              <string>',C,'</string>',nl]), fail ; true).
processC(list_lex_ra,[],_NVs,yes) :-
  !, 
  setof(KW,ra_keyword(KW),Cs),
  (member(C,Cs), write_log_list([C,nl]), fail ; true).
processC(list_lex_cmds,[],_NVs,yes) :-
  !, 
  setof(Com,A^B^C^D^E^F^command(A,B,C,Com,D,E,F),Cs),
  (member(C,Cs),write_log_list(['/',C,nl]), fail ; true).
processC(list_lex_cmd_opts,[],_NVs,yes) :-
  !, 
  command_options(Cs),
  (member(C,Cs), write_log_list([C,nl]), fail ; true).
processC(list_lex_cmd_opts,[acide],_NVs,yes) :-
  !, 
  command_options(Cs),
  (member(C,Cs), write_log_list(['              <string>',C,'</string>',nl]), fail ; true).
processC(list_lex_cmds,[acide],_NVs,yes) :-
  !, 
  setof(Com,A^B^C^D^E^F^command(A,B,C,Com,D,E,F),Cs),
  (member(C,Cs), write_log_list(['              <string>/',C,'</string>',nl]), fail ; true).
processC(generate_db,[NbrTables,TableSize,NbrViews,MaxDepth,MaxWidth,FileName],_NVs,yes) :-
  !, 
  push_flag(sql_semantic_check,off,OldFlag),
  generate_db_instance(NbrTables,TableSize,NbrViews,MaxDepth,MaxWidth,FileName),
  pop_flag(sql_semantic_check,OldFlag).
processC(debug_sql_bench,[NbrTables,TableSize,NbrViews,MaxDepth,MaxWidth,FileName],_NVs,yes) :-
  !, 
  debug_sql_bench(NbrTables,TableSize,NbrViews,MaxDepth,MaxWidth,FileName).
% processC(Fuzzy,[],_NVs,yes) :-
%   !, 
%   fuzzy(Switch),
%   write_info_log(['Fuzzy Datalog is ', Switch, '.']).
% processC(Fuzzy,[Switch],_NVs,yes) :-
%   !, 
%   fuzzy(CSwitch),
%   (CSwitch==Switch -> true ; processC(abolish,[],_,yes)),
%   process_set_binary_flag(fuzzy,'Fuzzy Datalog is',Switch).
% processC(Logiql,[],_NVs,yes) :-
%   (Logiql=logiql ; Logiql=lq),
%   !, 
%   logiql(Switch),
%   write_info_log(['LogiQL output is ', Switch, '.']).
% processC(Logiql,[Switch],_NVs,yes) :-
%   (Logiql=logiql ; Logiql=lq),
%   !, 
%   process_set_binary_flag(logiql,'LogiQL output is',Switch).
% processC(logiql_cmd,[],_NVs,yes) :-
%   !, 
%   logiql_cmd(Switch),
%   write_info_log(['LogiQL commands are ', Switch, '.']).
% processC(logiql_cmd,[Switch],_NVs,yes) :-
%   !, 
%   process_set_binary_flag(logiql_cmd,'LogiQL commands are',Switch),
%   (logiql_cmd(on) -> set_flag(logiql,on) ; true).
processC(Fuzzy,Args,NVs,yes) :-
  %system_mode(fuzzy), 
  process_fuzzy_command(Fuzzy,Args,NVs,yes).
processC(Command,_L,_NVs,yes) :-
  !, 
  write_error_log(['Unknown command or incorrect number of arguments. Use ''/help'' or ''/help keyword'' for help.']),
  display_object_alternatives(command,Command).

  
processC_process([F|Params],Continue,Mode) :-
  (host_safe(on),
   batch(_,_,_,_,_)
   ->
    write_info_log(['This command cannot be nested in host safe mode.'])
   ;
    push_param_vector,
    set_param_vector(Params),
    process_batch(F,Continue,Mode,_Error),
    pop_param_vector).


% Switch to user input during processing /run. Switch indicates whether this command has been processed in the current batch
% restore_batch_user_input(-Switch)
restore_batch_user_input(on) :-
  current_batch_ID(ID),
  batch(ID,_L,_F,_S,semi),
  !,
  set_input(user_input).
restore_batch_user_input(off).


% Switch to file input during processing /run if needed.
% restore_batch_user_input(+Switch)
restore_batch_file_input(off) :-
  !.
restore_batch_file_input(_Switch) :-
  current_batch_ID(ID),
  batch(ID,_L,_F,S,semi),
  !,
  set_input(S).

% Look the license in the start path
display_license :-
  (start_path(StartPath)
   ->
    BasePath=StartPath
   ;
    BasePath='./'),
  display_license(BasePath),
  !.
% Look the license two upper levels in the working directory (as in DESweb)
display_license :-
  my_working_directory(WorkingDirectory),
  atom_concat(WorkingDirectory,'../../',RelBaseDir),
  my_absolute_filename(RelBaseDir,UBasePath),
  atom_concat(UBasePath,'/',BasePath),
  display_license(BasePath),
  !.
% Look in the working directory
display_license :-
  my_working_directory(WorkingDirectory),
  atom_concat(WorkingDirectory,'/',BasePath),
  display_license(BasePath),
  !.
% No luck
display_license :-
  write_error_log(['License not found locally. Please consult it at http://www.gnu.org/licenses/']).

display_license(BasePath) :-
  FileCopy1 = 'license/COPYING',
  FileCopy2 = 'license/COPYING.LESSER',
  atom_concat_list([BasePath,FileCopy1],PathFileCopy1),
  atom_concat_list([BasePath,FileCopy2],PathFileCopy2),
  my_file_exists(PathFileCopy1),
  my_file_exists(PathFileCopy2),
  my_working_directory(WorkingDirectory),
  my_relative_file_path(PathFileCopy1,WorkingDirectory,RelPathFileCopy1),
  my_relative_file_path(PathFileCopy2,WorkingDirectory,RelPathFileCopy2),
  processC(cat,[RelPathFileCopy1],_,yes),
  processC(cat,[RelPathFileCopy2],_,yes).

write_dangling_relations([]).
write_dangling_relations([[RelName,DepOnRelName]|Rest]) :-
  write_log_list(['"',RelName,'" depends on "',DepOnRelName,'", which does not exist.',nl]),
  write_dangling_relations(Rest).

refresh_db_metadata :-
  processC(clear_et,[],[],yes),
  compute_stratification_verbose.

open_db(Connection,Opts,Error) :-
  (opened_db(Connection)
   ->
    current_db(CurrentConnection),
    (CurrentConnection\==Connection
     ->
      write_warning_log(['The database ''',Connection,''' is opened already.']),
      processC(use_db,[Connection],[],yes)
     ;
      write_warning_log(['The database ''',Connection,''' is opened already and is the current one.'])
    )
   ;
    my_open_odbc(Connection,Opts),
    (opened_db(Connection,_Handle,DBMS)
     ->
      set_flag(current_db(Connection,DBMS)),
      enable_rdb_datasource(Connection),
      refresh_db_metadata,
      write_tapi_success,
      set_flag(db_schema_modified(true))
     ;
      Error=true % Error message displayed already 
    )
  ).
  
command_options(Os) :-
  Os =  [a1,a3,abort,add,append,acide,batch,bpl,datalog,declared,des,des_db,detailed,direct,display,file,fuzzy,goedel,hamacher,luka,lukasiewicz,min,missing,nilpotent,no,nonvalid,on,off,plain,postorder,preorder,product,prolog,reflexive,replace,runtime,symmetric,total_runtime,transitive,trust_extension,trust_file,trust_tables,valid,write,wrong,yes].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMAND FLAGS
  
% Sets a binary flag (on, off values). Called from processC
process_set_binary_flag(FlagFunctor,Message,Switch) :-
  process_set_binary_flag(FlagFunctor,Message,Switch,_Result).
  
process_set_binary_flag(FlagFunctor,Message,Switch,Result) :-
  to_lowercase(Switch,LSwitch),
  Flag=..[FlagFunctor,VarSwitch],
  ((LSwitch == on; LSwitch == off) ->
    (LSwitch == on, call(Flag), VarSwitch==on ->
      write_info_silent_log(['',Message,' already enabled.']),
      Result=nop
     ;
      (LSwitch == off, call(Flag), VarSwitch==off ->
        write_info_silent_log(['',Message,' already disabled.']),
        Result=nop
       ;
        set_flag(FlagFunctor,LSwitch),
        write_info_verb_log(['',Message,' ', Switch, '.']),
        Result=changed
      )
    )
   ;
    write_error_log(['Incorrect switch. Use ''on'' or ''off''']),
    Result=error
  ).
  
process_set_toggle_binary_flag(FlagFunctor,Message,Switch) :-
  to_lowercase(Switch,LSwitch),
  Flag=..[FlagFunctor,VarSwitch],
  call(Flag),
  (LSwitch==toggle
   ->
    (VarSwitch==on
     ->
      TSwitch=off
     ;
      (VarSwitch==off
       ->
        TSwitch=on
       ;
        TSwitch=LSwitch)
    )
   ;
    TSwitch=LSwitch
  ),
  process_set_binary_flag(FlagFunctor,Message,TSwitch).

process_set_command_flag(FlagFunctor,Message,Value,PossibleValues) :-
  process_set_command_flag(FlagFunctor,Message,Value,PossibleValues,_Result).

% Result in {nop (no operation - nothing changed), changed, error (some value error) }
% Switch/Value: Is the requested new Switch/Value
process_set_command_flag(FlagFunctor,Message,Value,'$interval'(Type,Lower,Upper),Result) :-
  ValidType=..[Type,Value],
  (\+ ValidType
   ->
    write_error_log(['Invalid value ',Value,' of type ', Type]),
    Result=error
   ;
    ((Value<Lower ; Value>Upper)
     ->
      write_error_log(['Value ',Value,' out of interval [', Lower,', ',Upper,']']),
      Result=error
     ;
      set_flag(FlagFunctor,Value),
      write_info_verb_log(['',Message,' ', Value, '.']),
      Result=changed
    )
  ).
process_set_command_flag(FlagFunctor,Message,NewValue,'$check'(CheckGoal),Result) :-
  (CheckGoal % This may bind variables
   ->
    Flag=..[FlagFunctor,OldValue],
    call(Flag), 
    (OldValue==NewValue
     ->
      write_info_silent_log(['',Message,' already set to ',NewValue,'.']),
      Result=nop
     ;
      set_flag(FlagFunctor,NewValue),
      Result=changed
    )
   ;
    Result=error
  ).
process_set_command_flag(FlagFunctor,Message,Switch,PossibleValues,Result) :-
  to_lowercase(Switch,LSwitch),
  Flag=..[FlagFunctor,VarSwitch],
  call(Flag), 
  (member(LSwitch,PossibleValues)
   ->
    (LSwitch == VarSwitch
     ->
      (LSwitch == on
       ->
        write_info_silent_log(['',Message,' already enabled.']),
        Result=nop
       ;
       (LSwitch == off
        ->
         write_info_silent_log(['',Message,' already disabled.']),
         Result=nop
        ;  
         write_info_silent_log(['',Message,' already set to ',Switch,'.']),
         Result=nop
       )
      )
     ;
      set_flag(FlagFunctor,LSwitch),
      write_info_verb_log(['',Message,' ', Switch, '.']),
      Result=changed
     )
   ;
    write_error_log(['Incorrect switch. Possible values: ',PossibleValues]),
    Result=error
  ).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command assertions

process_command_assertion(Command) :-
  Command=..[Name|Args],
  length(Args,Arity),
  command_assertion(Name,Arity),
  (
   command(_,_,y(MName,_),Name,_,_,_)
  ;
   MName=Name
  ),
  processC(MName,Args,[],_).

command_assertion(Name,Arity) :-
  fuzzy_assertion(Name,Arity).
command_assertion(clear_et,0).
command_assertion(solve,1).
command_assertion(system_mode,1).

list_command_assertions :-
  command_assertion(Name,Arity),
  write_log_list([Name,'/',Arity,nl]),
  fail.
list_command_assertions.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% Help system
% Commands
% CategoryOrder allows ordered listings. It should not be viewed as a category identifier
% command_category(CategoryOrder,CategoryName,SafeCategory).
command_category( 10,'DDB Database',yes).
command_category( 20,'ODBC/DDB Database',yes).
command_category( 30,'Dependency Graph and Stratification',yes).
command_category( 40,'Debugging and Test Case Generation',yes).
command_category( 50,'Tabling',yes).
command_category( 60,'Operating System',no).
command_category( 70,'Logging',no).
command_category( 80,'Informative',yes).
command_category( 90,'Query Languages',yes).
command_category(100,'TAPI',yes).
command_category(110,'Settings',yes).
command_category(120,'Timing',yes).
command_category(130,'Statistics',yes).
command_category(140,'Scripting',yes).
command_category(150,'Miscellanea',yes).
command_category(160,'Implementor',no).
command_category(170,'Fuzzy',yes).
% command_category(180,'LogiQL Front-end',no).
% command_category(190,'HR-SQL',yes).

% command(CategoryOrder,CommandOrder,CommandType:(c)ommand|(s(Cmd))horthand|(s(Cmd))ynonim),Command,Arguments,ShortDescription,ExtendedDescription)
% CommandOrder allows ordered listings. It should not be viewed as a command identifier

% DDB Database:
command(10, 10,c,'[','Filenames]','Consult Datalog files, abolishing previous rules','Load the Datalog programs found in the comma-separated list [Filenames], discarding the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed. Arguments in the list are comma-separated').
command(10, 20,c,'[+','Filenames]','Consult Datalog files, keeping previous rules','Load the Datalog programs found in the comma-separated list Filenames, keeping rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed.').
command(10, 30,c,abolish,'','Abolish the Datalog database','Delete the Datalog database. This includes all the local rules (including those which are the result of SQL compilations) and external rules (persistent predicates). Integrity constraints and SQL table and view definitions are removed. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10, 40,c,abolish,'Name','Abolish the predicates matching Name','Delete the predicates matching Name. This includes all their local rules (including those which are the result of SQL compilations) and external rules (persistent predicates). Their Integrity constraints and SQL table and view definitions are removed. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10, 50,c,abolish,'Name/Arity','Abolish the predicate matching the pattern','Delete the predicates matching the pattern Name/Arity. This includes all their local rules (including those which are the result of SQL compilations) and external rules (persistent predicates). Their Integrity constraints and SQL table and view definitions are removed. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10, 60,c,assert,'Head:-Body','Assert a rule. :-Body is optional (for facts)','Add a Datalog rule. If Body is not specified, it is simply a fact. Rule order is irrelevant for Datalog computation. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10, 70,c,close_persistent,'','Close the single connection to a persistent predicate','If there is only one connection to a persistent predicate, it is close. Otherwise, the user is warned with the different predicate alternatives. After closing the connection, the predicate is no longer visible except its metadata. The external DBMS keeps its definition. For restoring its visibility again, simply submit an assertion as :-persistent(PredSpec,DBMS)').
command(10, 70,c,close_persistent,'Name','Close the connection to the persistent predicate Name','Close the connection to the persistent predicate Name. The predicate is no longer visible except its metadata. The external DBMS keeps its definition. For restoring its visibility again, simply submit an assertion as :-persistent(PredSpec,DBMS)').
command(10, 80,c,consult,'Filename','Consult a Datalog file, abolishing previous rules','Load the Datalog program found in the file Filename, discarding the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed. The default extension .dl for Datalog programs can be omitted').
command(10, 90,s(consult,'Filename'),'c','Filename','Shorthand for /consult Filename','Load the Datalog program found in the file Filename, discarding the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed. The default extension .dl for Datalog programs can be omitted').
command(10,100,c,check_db,'','Check database consistency w.r.t. declared integrity constraints','Check database consistency w.r.t. declared integrity constraints (types, existency, primary key, candidate key, foreign key, functional dependency, and user-defined). Displays a report with the outcome').
command(10,110,c,des,'Input','Force DES to solve Input','Force DES to solve Input. If Input is an SQL query, DES solves it instead of relying on external DBMS solving. This allows to try the more expressive queries which are available in DES (as, e.g., hypothetical and non-linear recursive queries)').
command(10,120,c,drop_ic,'Constraint','Drop an integrity constraint','Drop the specified integrity constraint, which starts with \':-\' (either one of :-nn(Table,Columns), :-pk(Table,Columns), :-ck(Table,Columns), :-fk(Table,Columns,RTable,RColumns), :-fd(Table,Columns,DColumns), :-Goal, where Goal specifies a user-defined integrity constraint). Only one constraint can be dropped at a time. TAPI enabled').
command(10,130,c,drop_assertion,'Assertion','Drop an assertion','Drop the specified assertion, which starts with \':-\'. So far, there is only support for :-persistent(Schema[,Connection]). Where Schema is the ground atom describing the predicate (predicate and argument names, as: pred_name(arg_name1,...,arg_nameN)) that has been made persistent on an external DBMS via ODBC, and Connection is an optional connection name for the external RDB. Only one assertion can be dropped at a time').
command(10,135,c,list_predicates,'','List predicates','List predicates (name and arity). Include intermediate predicates which are a result of compilations if development mode is enabled (cf. the command /development). TAPI enabled').
command(10,140,c,listing,'','List the loaded Datalog rules','List the loaded Datalog rules. Neither integrity constraints nor SQL views and metadata are displayed').
command(10,150,c,listing,'Name','List the loaded Datalog rules matching Name','List the loaded Datalog rules matching Name, including restricting rules. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,160,c,listing,'Name/Arity','List Datalog rules matching the pattern','List the loaded Datalog rules matching the pattern Name/Arity, including restricting rules. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,170,c,listing,'Head','List Datalog rules whose head is subsumed by Head','List the Datalog loaded rules whose heads are subsumed by the head Head. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,180,c,listing,'Head:-Body','List Datalog rules that are subsumed by Head:-Body','List the Datalog loaded rules that are subsumed by Head:-Body. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,190,c,listing_asserted,'','List the asserted Datalog rules','List the Datalog rules that have been asserted with command. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,200,c,listing_asserted,'Name/Arity','List the asserted Datalog rules matching the pattern','List the Datalog rules matching the given pattern Name/Arity that have been asserted with command. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,210,c,listing_asserted,'Head','List the asserted Datalog rules whose head is subsumed by Head','List the the Datalog rules that have been asserted with command whose heads are subsumed by the head Head. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,220,c,listing_asserted,'Head:-Body','List the asserted Datalog rules that are subsumed by Head:-Body','List the the Datalog rules that have been asserted with command that are subsumed by Head:-Body. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,230,c,list_modes,'','List the expected modes for unsafe predicates in order to be correctly computed','List the expected modes for unsafe predicates in order to be correctly computed. Modes can be \'i\' (for an input argument) and \'o\' (for an output argument)').
command(10,240,c,list_modes,'Name','List expected modes, if any, for predicates with name Name in order to be correctly computed','List expected modes, if any, for predicates with name Name in order to be correctly computed. Modes can be \'i\' (for an input argument) and \'o\' (for an output argument)').
command(10,250,c,list_modes,'Name/Arity','List expected modes, if any, for the given predicate Name/Arity in order to be correctly computed','List expected modes, if any, for the given predicate Name/Arity in order to be correctly computed. Modes can be \'i\' (for an input argument) and \'o\' (for an output argument)').
command(10,260,c,list_persistent,'','List persistent predicates','List persistent predicates along with their ODBC connection names. TAPI enabled').
command(10,265,c,list_undefined,'','List undefined predicates','List undefined predicates, i.e., those which are not built-in, not external (ODBC table/view), and not defined with a Datalog rule. TAPI enabled').
command(10,270,c,list_sources,'Name/Arity','List the sources of the Datalog rules matching the pattern','List the sources of the Datalog rules matching the pattern Name/Arity').
command(10,280,c,reconsult,'Filename','Consult a Datalog file, keeping previous rules','Load a Datalog program found in the file Filename, keeping the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,290,s(reconsult,'Filename'),'r','Filename','Shorthand for /reconsult Filename','Load a Datalog program found in the file Filename, keeping the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,300,c,restore_ddb,'','Restore the database from Filename','Restore the database from the default file des.ddb . Constraints (type, existence, primary key, candidate key, functional dependency, foreign key, and user-defined) are also restored, if present, from des.ddb').
command(10,305,c,restore_ddb,'Filename','Restore the database from Filename','Restore the Datalog database from the given Filename (same as consult). Constraints (type, not nullables, primary key, candidate key, functional dependency, foreign key, and user-defined) are also restored, if present, from Filename').
command(10,310,c,restore_state,'','Restore the database state from des.sds','Restore the database state from the file des.sts. Equivalent to /restore_state des.sds, where the current path is the start path').
command(10,320,c,restore_state,'Filename','Restore the database state from Filename','Restore the database state from Filename').
command(10,330,c,retract,'Head:-Body','Retract a rule. :-Body is optional (for facts)','Delete the first Datalog rule that unifies with Head:-Body (or simply with Head, if Body is not specified. In this case, only facts are deleted). The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,340,c,retractall,'Head','Retract all rules matching the given head','Delete all the Datalog rules whose heads unify with Head. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,350,c,save_ddb,'','Save the current database to the file des.ddb','Save the current database to the file des.ddb, rewritting this file if already present. Constraints (type, not nullables, primary key, candidate key, functional dependency, foreign key, and user-defined) are also saved').
command(10,360,c,save_ddb,'[force] Filename','Save the current database to a file','Save the current database to the file Filename. If option \'force\' is included, no question is asked to the user should the file exists already. Constraints (type, not nullables, primary key, candidate key, functional dependency, foreign key, and user-defined) are also saved').
command(10,370,c,save_state,'','Save the current database state to the file des.sds','Save the current database state to the file des.sts. Equivalent to /save_state force des.sds, where the current path is the start path').
command(10,380,c,save_state,'[force] Filename','Save the current database state to a file','Save the current database state to the file Filename. If option \'force\' is included, no question is asked to the user should the file exists already. The whole database (including its current state) can be saved to a file, and restored in a subsequent session. An automatic saving and restoring can be stated respectively by adding the commands /save_state and /restore_state in the files des.ini and des.out. This way, the user can restart its session in the same state point it was left, including the deductive database, metadata information (types, constraints, SQL text, ...), system settings, all opened external databases and persistent predicates').

% ODBC/DDB Database:
command(20,  5,c,dangling_relations,'','Display dangling relations','Display the relations that depend on others which do not exist').
command(20, 10,c,db_schema,'','Display the database schema','Display the database schema: Tables, views and constraints. TAPI enabled').
command(20, 20,c,db_schema,'Name','Display the database schema for the given connection, view or table','Display the database schema for the given connection, view or table name. TAPI enabled').
command(20, 30,c,db_schema,'Connection:Relation','Display the database schema for the given view or table','Display the database schema for the given view or table name. TAPI enabled').
command(20, 40,y(db_schema,''),dbschema,'','Synonym for /db_schema','Synonym for /db_schema').
command(20, 50,y(db_schema,'Name'),'dbschema','Name','Synonym for /db_schema Name','Synonym for /db_schema Name').
command(20, 60,y(db_schema,'Connection:Relation'),'dbschema','Connection:Relation','Synonym for /db_schema Connection:Relation','Synonym for /db_schema Connection:Relation').
command(20, 65,c,dbs_schemas,'','Display the schema of each open database','Display the schema of each open database: Tables, views and constraints. TAPI enabled').
command(20, 70,c,db_schema_modified,'','Display whether the database schema has been modified','Display whether the database schema has been modified by a previous input. Reset by DESweb when refreshing the database panel. TAPI enabled').
command(20, 80,c,dependent_relations,'[direct] [declared] Relation','Display relations that depend on Relation','Display a list of relations that depend on Relation. Relation can be either a pattern R/A or a relation R. A relation R can be either a relation name N or C:N, where C refers to a specific connection and N is a relation name. If ''direct'' is included, the dependency is only direct; otherwise, the dependency is both direct and indirect. If ''declared'' is included, only declared (typed) relations are included in the outcome. In development mode, system-generated predicates are also considered. TAPI enabled').
% command(20, 80,c,dependent_relations,'[all] [declared] Name/Arity','Display relations that depend on relation Name/Arity','Display a list of relations that depend on Name/Arity. If ''all'' is included, the dependency is both direct and indirect; otherwise, the dependency is direct. If ''declared'' is included, only declared (typed) relations are included in the outcome. TAPI enabled').
command(20, 85,y(describe,'Relation'),'describe','Relation','Synonym for /db_schema Relation','Synonym for /db_schema Relation, where Relation can only be either a table or a view, possibly qualified with the database name (as connection:relation)').
command(20, 90,c,drop_all_tables,'','Drop all tables from the current database','Drop all tables from the current database but ''dual'' if it exists. If the current connection is an external database, tables in ''$des'' are not dropped. TAPI enabled').
command(20,100,c,drop_all_relations,'','Drop all relations from the current database','Drop all relations from the current database but ''dual'' if it exists. If the current connection is an external database, relations in ''$des'' are not dropped').
command(20,110,c,drop_all_views,'','Drop all views from the current database','Drop all views from the current database. If the current connection is an external database, views in ''$des'' are not dropped. TAPI enabled').
command(20,120,c,open_db,'Conn [Opts]','Open and set the current ODBC connection','Open and set the current ODBC connection to Name, where Opts=[user(''Username'')] [password(''Password'')]. Username and Password must be delimited by single quotes (''). This connection must be already defined at the OS layer. TAPI enabled').
command(20,130,c,close_db,'','Close the current ODBC connection','Close the current ODBC connection. TAPI enabled').
command(20,140,c,close_db,'Name','Close the ODBC connection Name','Close the ODBC connection denoted as Name. TAPI enabled').
command(20,145,c,close_dbs,'','Close all the opened ODBC connections','Close all the opened ODBC connections. Make $des the current database').
command(20,150,c,current_db,'','Display the current ODBC connection info','Display the current ODBC connection name and DSN provider. TAPI enabled').
command(20,155,c,get_relation,'Connection Relation','Display the relation schema and data for the given connection as a Prolog term','Display the relation schema and data for the given connection as the Prolog term schema_data(Schema,Data), where Schema is of the form relname(col_1:type_1,...,col_n:type_n), Data is a list of tuples relname(val_1,...,val_n), col_i are column names, type_i are type names, and val_i are values of the corresponding type type_i. If Connection=$des and Relation=answer, the outcome corresponds to the answer to the last submitted query').
command(20,160,c,is_empty,'Name','Display whether the given relation is empty','Display $true if the given relation is empty, and $false otherwise. TAPI enabled').
command(20,170,c,list_dbs,'','Display the current opened databases','Display the current opened databases, handled by DES and external DBMS\'s').
command(20,180,c,list_relations,'','List relation names','List relation (both tables and views) names. TAPI enabled').
command(20,190,c,list_tables,'','List table names','List table names. TAPI enabled').
command(20,200,c,list_table_schemas,'','List table schemas','List table schemas. TAPI enabled').
command(20,210,c,list_table_constraints,'Name','List table constraints for the given table name','List table constraints for the given table name. TAPI enabled').
command(20,220,c,list_views,'','List views','List view schemas. TAPI enabled').
command(20,230,c,list_view_schemas,'','List view schemas','List view schemas. TAPI enabled').
command(20,240,c,referenced_relations,'Name','Display relations directly referenced by a foreign key in Name','Display the name of relations that are directly referenced by a foreign key in relation Name. TAPI enabled').
command(20,250,c,referenced_relations,'Name/Arity','Display relations directly referenced by a foreign key in Name/Arity','Display the name of relations that are directly referenced by a foreign key in relation Name/Arity. TAPI enabled').
command(20,260,c,refresh_db,'','Refresh local database metadata','Refresh local metadata from either the deductive or the current external database, clear the cache, and recompute the PDG and strata. TAPI enabled').
command(20,270,c,relation_exists,'Name','Display whether relation Name exists','Display $true if the given relation exists, and $false otherwise. TAPI enabled').
command(20,280,c,relation_schema,'Name','Display relation schema of relation Name','Display relation schema of relation Name. TAPI enabled').
command(20,290,y(list_dbs,''),'show_dbs','','Synonym for /list_dbs','Synonym for /list_dbs').
command(20,300,c,sql_left_delimiter,'','Display the SQL left delimiter of current DBMS','Display the SQL left delimiter as defined by the current database manager (either DES or the external DBMS via ODBC). TAPI enabled').
command(20,310,c,sql_right_delimiter,'','Display the SQL right delimiter of current DBMS','Display the SQL right delimiter as defined by the current database manager (either DES or the external DBMS via ODBC). TAPI enabled').
command(20,315,c,relation_modified,'','Display the relations modified so far','Display the relations modified so far (Datalog relation, SQL table or view), whether it is typed or not, as Prolog terms connection_table(Connection, Relation), where Connection is the connection for which the relation with name Relation has been modified. TAPI enabled').
command(20,320,c,use_db,'Name','Make Name the current database','Make Name the current database, handled by DES or by an external DBMS. If it is not open already, it is automatically opened').
command(20,330,c,use_ddb,'','Make $des the current database','Make $des (the default deductive database) the current database. Shorthand for /use_db $des').

% Dependency Graph and Stratification
command(30, 10,c,external_pdg,'','Display whether external PDG construction is enabled','Display whether external PDG construction is enabled').
command(30, 20,c,external_pdg,'Switch','Enable or disable external PDG construction','Enable or disable external PDG construction is enabled (on or off, resp.) Some ODBC drivers are so slow that makes external PDG construction impractical. If disabled, tracing and debugging external databases are not possible').
command(30, 30,c,pdg,'','Display the current predicate dependency graph','Display the current predicate dependency graph').
command(30, 40,c,pdg,'Name','Display the predicate dependency graph restricted to predicate with name Name','Display the current predicate dependency graph restricted to predicate with name Name').
command(30, 50,c,pdg,'Name/Arity','Display the predicate dependency graph restricted to the predicate Name/Arity','Display the current predicate dependency graph restricted to the predicate Name/Arity').
command(30, 60,c,rdg,'','Display the current relation dependency graph','Display the current relation dependency graph').
command(30, 70,c,rdg,'Name','Display the relation dependency graph restricted to relation Name','Display the current relation dependency graph restricted to relation with name Name').
command(30, 80,c,rdg,'Name/Arity','Display the relation dependency graph restricted to the relation Name/Arity','Display the current relation dependency graph restricted to the relation Name/Arity').
command(30, 90,c,strata,'','Display the current stratification','Display the current stratification as a list of pairs (PredName/Arity, Stratum)').
command(30,100,c,strata,'Name','Display the current stratification restricted to predicate with name Name','Display the current stratification restricted to predicate with name Name').
command(30,110,c,strata,'Name/Arity','Display the current stratification restricted to the predicate Name/Arity','Display the current stratification restricted to the predicate Name/Arity').

% Debugging and Test Case Generation:
command(40, 10,c,debug_datalog,'Goal [Level]','Debug a Datalog basic goal','Start the debugger for the basic goal Goal at predicate or clause levels, which is indicated with the options p and c for Level, respectively. Default is p').
command(40, 20,c,debug_dl,'Name/Arity File','Debug a Datalog relation included in a file','Start the debugger for the relation Name/Arity which is defined in file File. It is assumed that a predicate name only occurs in the program with the same arity. This debugger implements the framework described in PPDP 2015 paper. TAPI enabled').
command(40, 22,c,debug_dl_answer,'Question Answer','Answer a question when debuging a Datalog relation','Answer a question when debuging a Datalog relation. Possible answers are abort, valid, nonvalid, missing(Tuple), and wrong(Tuple), where Tuple is of the form rel(cte1, ..., cten), where rel is the relation name and each argument ctei is a Datalog constant. Placeholders (_) are allowed for missing tuples instead of constants. TAPI enabled').
command(40, 24,c,debug_dl_current_question,'','Display the current question when debuging a Datalog relation','Display the current question when debuging a Datalog relation. TAPI enabled').
command(40, 25,c,debug_dl_explain,'','Explain the outcome of the last Datalog debugging session','Explain the outcome of the last Datalog debugging session. TAPI enabled').
command(40, 26,c,debug_dl_node_state,'','Display Datalog debugging node states','Display Datalog debugging node states. TAPI enabled').
command(40, 28,c,debug_dl_statistics,'','Display Datalog debugging session statistics','Display Datalog debugging session statistics. TAPI enabled').
command(40, 30,c,debug_sql,'View [Opts]','Debug an SQL view','Debug an SQL view where:\n           Opts=[trust_tables([yes|no])] [trust_file(FileName)]\n           [oracle_file(FileName)] [debug([full|plain])]\n           [order([cardinality|topdown])]\n           Defaults are trust tables, no trust file,\n           no oracle file, full debugging, and\n           navigation order based on relation cardinality. TAPI enabled').
command(40, 32,c,debug_sql_answer,'Question Answer','Answer a question when debuging an SQL relation','Answer a question when debuging an SQL relation. Possible answers are abort, valid, nonvalid, missing(Tuple), and wrong(Tuple), where Tuple is of the form rel(cte1, ..., cten), where rel is the relation name and each argument ctei is an SQL constant. Placeholders (_) are allowed for missing tuples instead of constants. TAPI enabled').
command(40, 34,c,debug_sql_current_question,'','Display the current question when debuging an SQL view','Display the current question when debuging an SQL view. TAPI enabled').
command(40, 36,c,debug_sql_set_node,'Node State','Set the state for a node with an unknown state','Set the state for a node with an unknown state. State can be either ''valid'' or ''nonvalid''. TAPI enabled').
command(40, 38,c,debug_sql_statistics,'','Display SQL debugging session statistics','Display SQL debugging session statistics. TAPI enabled').
command(40, 40,c,trace_datalog,'Goal [Order]','Trace a Datalog basic goal','Trace a Datalog goal in the given order (postorder or the default preorder)').
command(40, 50,c,trace_sql,'View [Order]','Trace an SQL view in preorder (default) or postorder','Trace an SQL view in the given order (postorder or the default preorder)').
command(40, 60,c,test_case,'View [Opts]','Generate test case classes for the given view','Generate test case classes for the view View. Options may include a class and/or an action parameters. The test case class is indicated by the values all (positive-negative, the default), positive, or negative in the class parameter. The action is indicated by the values display (only display tuples, the default), replace (replace contents of the involved tables by the computed test case), or add (add the computed test case to the contents of the involved tables) in the action parameter').
command(40, 70,c,tc_size,'','Display the test case size bounds','Display the minimum and maximum number of tuples generated for a test case').
command(40, 80,c,tc_size,'Min Max','Bound the test case size','Set the minimum and maximum number of tuples generated for a test case').
command(40, 90,c,tc_domain,'','Display the domain of values for test cases','Display the domain of values for test cases').
command(40,100,c,tc_domain,'Min Max','Set the domain of values for test cases','Set the domain of values for test cases between Min and Max').

% Tabling:
command(50, 10,c,clear_et,'','Clear the extension table','Delete the contents of the extension table. Can be used as a directive').
command(50, 20,c,list_et,'','List extension table contents','List the contents of the extension table in lexicographical order. First, answers are displayed, then calls. TAPI enabled').
command(50, 30,c,list_et,'Name','List extension table contents matching a name','List the contents of the extension table matching Name. First, answers are displayed, then calls. TAPI enabled').
command(50, 40,c,list_et,'Name/Arity','List extension table contents matching the pattern','List the contents of the extension table matching the pattern Name/Arity. First, answers are displayed, then calls. TAPI enabled').

% Operating System:
command(60, 10,c,ashell,'Command','Asynchronous submit of Command to the OS shell','Asynchronous submit of Command to the operating system shell. As  /shell Command but without waiting for the process to finish and also eliding output. See also /shell.').
command(60, 20,c,cat,'Filename','Type the contents of Filename','Type the contents of Filename enclosed between the following lines: %% BEGIN AbsoluteFilename %% %% END   AbsoluteFilename %%').
command(60, 30,c,cd,'','Set current directory to the one DES was started from','Set the current directory to the directory where DES was started from').
command(60, 40,c,cd,'Path','Set the current directory to Path','Set the current directory to Path').
command(60, 50,c,cp,'FromFile ToFile','Copy the file FromFile to ToFile','Copy the file FromFile to ToFile').
command(60, 60,y(cp,'FromFile ToFile'),copy,'FromFile ToFile','Synonym for /cp FromFile ToFile','Synonym for /cp FromFile ToFile').
command(60, 70,y(rm,'Filename'),'del','Filename','Synonym for /rm Filename','Synonym for /rm Filename').
command(60, 80,y(edit,'Filename'),'e','Filename','Synonym for /edit Filename','Synonym for /edit Filename').
command(60, 90,c,edit,'Filename','Edit Filename','Edit Filename by calling the predefined external text editor. This editor is set with the command /set_editor <your_editor>').
command(60,100,y(ls,''),'dir','','Synonym for /ls','Synonym for /ls').
command(60,110,y(ls,'Path'),'dir','Path','Synonym for /ls Path','Synonym for /ls Path').
command(60,120,c,ls,'','Display the contents of the current directory','Display the contents of the current directory in alphabetical order. First, files are displayed, then directories').
command(60,130,c,ls,'Path','Display the contents of the given directory','Display the contents of the given directory in alphabetical order. It behaves as /ls').
command(60,140,c,pwd,'','Display the current directory','Display the absolute filename for the current directory').
command(60,150,c,rm,'Filename','Delete Filename','Delete (remove) Filename').
command(60,160,c,set_editor,'','Display the current external text editor','Display the current external text editor').
command(60,170,c,set_editor,'Editor','Set the current external text editor to Editor','Set the current external text editor to Editor. This editor is called from the command /edit Filename').
command(60,180,c,shell,'Command','Submit Command to the OS shell','Submit Command to the operating system shell\nNotes for platform specific issues:\n* Windows users:\n  command.exe is the shell for Windows 98, whereas cmd.exe is the one for Windows NT/2000/2003/XP/Vista/7.\n* Ciao users:\n  The environment variable SHELL must be set to the required shell.\n* SICStus users:\n  Under Windows, if the environment variable SHELL is defined, it is expected to name a Unix like shell, which will be invoked with the option -c Command. If SHELL is not defined, the shell named by COMSPEC will be invoked with the option /C Command.\n* Windows and Linux/Unix executable users:\n  The same note for SICStus is applied.').
command(60,190,s(shell,'Command'),'s','Command','Shorthand for /shell','Shorthand for /shell').
command(60,200,y(cat,'Filename'),'type','Filename','Synonym for /cat Filename','Synonym for /cat Filename').

% Logging:
command(70, 10,c,ilog,'','Display whether immediate logging is enabled','Display whether immediate logging is enabled. If enabled, each log is closed before user input and opened again afterwards').
command(70, 20,c,ilog,'Switch','Enable or disable immediate logging','Enable or disable immediate logging (on or off, resp.). If enabled, each log is closed before user input and opened again afterwards').
command(70, 30,c,log,'','Display the current log files, if any','Display the current log files, if any').
command(70, 40,c,log,'Filename','Set logging to the given filename','Set logging to the given filename overwriting the file, if exists, or creating a new one. Simultaneous logging to different logs is supported. Simply issue as many /log Filename commands as needed').
command(70, 50,c,log,'Mode Filename','Set logging to the given filename and mode','Set logging to the given filename and mode: write (overwriting the file, if exists, or creating a new one) or append (appending to the contents of the existing file, if exists, or creating a new one)').
command(70, 60,c,nolog,'Filename','Disable logging for the given filename','Disable logging for the given filename').
command(70, 70,c,nolog,'','Disable logging','Disable logging for all enabled logs').

% Informative:
command(80, 10,y(help,'Keyword'),'apropos','Keyword','Synonym for /help Keyword','Synonym for /help Keyword').
command(80, 20,c,builtins,'','List predefined operators, functions, and predicates','List predefined operators, functions, and predicates').
command(80, 25,c,command_assertions,'','List commands that can be used as assertions','List commands that can be used as assertions. A Datalog program can contain assertions :- command(arg1,...,argn), where command is the command name, and argi are its arguments').
command(80, 30,c,development,'','Display whether development listings are enabled','Display whether development listings are enabled').
command(80, 40,c,development,'Switch','Enable or disable development listings','Enable or disable development listings (on or off, resp.) These listings show the source-to-source translations needed to handle null values, Datalog outer join built-ins, and disjunctive literals').
command(80, 50,c,display_answer,'','Display whether display of computed tuples is enabled','Display whether display of computed tuples is enabled').
command(80, 60,c,display_answer,'Switch','Enable or disable display of computed tuples','Enable or disable display of computed tuples (on or off, resp.) The number of tuples is still displayed if enabled (see the command display_nbr_of_tuples)').
command(80, 70,c,display_nbr_of_tuples,'','Display whether display of the number of computed tuples is enabled','Display whether display of the number of computed tuples is enabled').
command(80, 80,c,display_nbr_of_tuples,'Switch','Enable or disable display of the number of computed tuples','Enable or disable display of the number of computed tuples (on or off, resp.)').
command(80, 90,c,help,'','Display this help','Display detailed help').
command(80,100,s(help,''),'h','','Shorthand for /help','Shorthand for /help').
command(80,110,c,help,'Keyword','Detailed help on Keyword','Detailed help on Keyword, which can be a command or built-in').
command(80,120,s(help,'Keyword'),'h','Keyword','Shorthand for /help Keyword','Shorthand for /help Keyword').
command(80,130,c,license,'','Display GPL and LGPL licenses','Display GPL and LGPL licenses. If not found, please visit http://www.gnu.org/licenses').
command(80,140,c,prolog_system,'','Display the underlying Prolog engine version','Display the underlying Prolog engine version').
command(80,150,c,silent,'','Display whether silent batch output is enabled','Display whether silent batch output is either enabled or disabled (on or off, resp.)').
command(80,160,c,silent,'Option','Enable or disable silent batch output messages','Enable or disable silent batch output messages (on or off, resp.) If this command precedes any other input, it is processed in silent mode (the command is not displayed and some displays are elided, as in particular verbose outputs)').
command(80,170,c,status,'','Display the current status of the system','Display the current system status, i.e., verbose mode, logging, elapsed time display, program transformation, current directory, current database and other settings').
command(80,180,c,verbose,'','Display whether verbose output is enabled','Display whether verbose output is either enabled or disabled (on or off, resp.)').
command(80,190,c,verbose,'Switch','Enable or disable verbose output messages','Enable or disable verbose output messages (on or off, resp.) The option toggle toggles its state (from on to off and vice versa').
command(80,200,c,version,'','Display the current DES system version','Display the current DES system version').

% Query Languages:
command(90, 10,c,datalog,'','Switch to Datalog interpreter','Switch to Datalog interpreter. All subsequent queries are parsed and executed first by the Datalog engine. If it is not a Datalog query, then it is tried in order as an SQL, RA, TRC, and DRC query').
command(90, 20,c,datalog,'Query','Trigger Datalog evaluation for Query','Trigger Datalog resolution for the query Query. The query is parsed and executed in Datalog, but if a parsing error is found, it is tried in order as an SQL, RA, TRC, and DRC query').
command(90, 25,y(datalog,''),des,'','Synonym for /datalog','Synonym for /datalog').
command(90, 30,c,drc,'','Switch to DRC interpreter','Switch to DRC interpreter (all queries are parsed and executed by DRC processor)').
command(90, 40,c,drc,'Query','Trigger DRC evaluation for Query','Trigger DRC evaluation for Query').
command(90, 50,c,prolog,'','Switch to Prolog interpreter','Switch to Prolog interpreter (all queries are parsed and executed in Prolog)').
command(90, 60,c,prolog,'Goal','Trigger Prolog evaluation for Goal','Trigger Prolog\'s SLD resolution for the goal Goal').
command(90, 70,c,ra,'','Switch to RA interpreter','Switch to RA interpreter (all queries are parsed and executed by RA processor)').
command(90, 80,c,ra,'RA_expression','Trigger RA evaluation for RA_expression','Trigger RA evaluation for RA_expression').
command(90, 90,c,sql,'','Switch to SQL interpreter','Switch to SQL interpreter (all queries are parsed and executed by SQL processor)').
command(90,100,c,sql,'SQL_statement','Trigger SQL evaluation for SQL_statement','Trigger SQL resolution for SQL_statement').
command(90,110,c,trc,'','Switch to TRC interpreter','Switch to TRC interpreter (all queries are parsed and executed by TRC processor)').
command(90,120,c,trc,'Query','Trigger TRC evaluation for Query','Trigger TRC evaluation for Query').

% TAPI:
command(100, 10,c,tapi,'Input','Process Input and format its output for TAPI communication','Process Input (either a command or query) and format its output for TAPI communication').
command(100, 20,c,mtapi,'','Process by TAPI the next input lines, terminated by $eot','Process the next input lines by TAPI. It behaves as /tapi Input, where Input are the lines following /mtapi and terminated by a single line containing $eot').
command(100, 30,c,tapi_log,'','Display whether TAPI logging is enabled','Display whether TAPI logging is enabled. If enabled, both TAPI commands and their results are not logged').
command(100, 40,c,tapi_log,'Switch','Enable or disable TAPI logging','Enable or disable TAPI logging (on or off, resp.) If enabled, both TAPI commands and their results are not logged').
command(100, 50,c,test_tapi,'','Test the current TAPI connection','Test the current TAPI connection. Return $success upon a successful communication. TAPI enabled').

% Settings:
command(110, 10,c,autosave,'','Display whether the database is automatically saved and restored','Display whether the database is automatically saved upon exiting and restored upon starting in the file des.sds (on) or not (off)').
command(110, 20,c,autosave,'Switch','Enable or disable automatic saving and restoring of the database','Enable or disable automatic saving and restoring of the database (on or off, resp.) . The option toggle toggles its state (from on to off and vice versa. If enabled, the complete database is automatically saved upon exiting and restored upon starting in the file des.sds').
command(110, 30,c,batch,'','Display whether batch mode is enabled','Display whether batch mode is enabled. If enabled, batch mode avoids PDG construction').
command(110, 40,c,batch,'Switch','Enable or disable batch mode','Enable or disable batch mode (on or off, resp.) If enabled, batch mode avoids PDG construction').
command(110, 50,c,check,'','Display whether integrity constraint checking is enabled','Display whether integrity constraint checking is enabled').
command(110, 60,c,check,'Switch','Enable or disable integrity constraint checking','Enable or disable integrity constraint checking (on or off, resp.)').
command(110, 70,c,compact_listings,'','Display whether compact listings are enabled','Display whether compact listings are enabled (on) or not (off)').
command(110, 80,c,compact_listings,'Switch','Enable or disable compact listings','Enable or disable compact listings (on or off, resp.)').
command(110, 90,c,current_flag,'Flag','Display the current value of flag Flag','Display the current value of flag Flag, if it exists').
command(110,100,c,des_sql_solving,'','Display whether DES is forced to solve SQL queries for external DBs','Display whether DES is forced to solve SQL queries for external DBs. If enabled, this allows to experiment with more expressive queries as, e.g., hypothetical and non-linear recursive queries targeted at an external DBMS').
command(110,110,c,des_sql_solving,'Switch','Enable or disable DES SQL solving for external DBs','Enable or disable DES solving for SQL queries when the current database is an open ODBC connection (on or off, resp.) This allows to experiment with more expressive queries as, e.g., hypothetical and non-linear recursive queries targeted at an external DBMS').
command(110,120,c,display_banner,'','Display whether the banner is displayed at startup','Display whether the system banner is displayed at startup').
command(110,130,c,display_banner,'Switch','Enable or disable the display of the banner at startup','Enable or disable the display of the system banner at startup (on or off, resp.) Only useful in a batch file des.ini or des.cnf').
command(110,140,c,duplicates,'','Display whether duplicates are enabled','Display whether duplicates are enabled').
command(110,150,c,duplicates,'Switch','Enable or disable duplicates','Enable or disable duplicates (on or off, resp.)').
command(110,160,c,fp_info,'','Display whether fixpoint information is to be displayed','Display whether fixpoint information, as the ET entries deduced for the current iteration, is to be displayed').
command(110,170,c,fp_info,'Switch','Enable display of fixpoint information','Enable or disable display of fixpoint information, as the ET entries deduced for the current iteration (on or off, resp.)').
command(110,180,c,host_safe,'','Display whether host safe mode is enabled','Display whether host safe mode is enabled (on) or not (off). Enabling host safe mode prevents users and applications using DES from accessing the host (typically used to shield the host from outer attacks, hide host information, protect the file system, and so on)').
command(110,190,c,host_safe,'on','Enable host safe mode','Enable host safe mode. Once enabled, this mode cannot be disabled').
% command(110,195,c,host_safe_goal,'Goal','Submit the Prolog goal Goal','Submit the Prolog goal Goal. Goal can only be a host safe Prolog goal').
command(110,200,y(host_safe,''),'sandboxed','','Synonym for /host_safe','Synonym for /host_safe').
command(110,210,y(host_safe,'Switch'),'sandboxed','','Synonym for /host_safe Switch','Synonym for /host_safe Switch').
command(110,220,c,hypothetical,'','Display whether hypothetical SQL queries are enabled','Display whether hypothetical SQL queries are enabled (on) or not (off)').
command(110,230,c,hypothetical,'Switch','Enable or disable SQL hypothetical queries','Enable or disable hypothetical SQL queries (on or off, resp.)').
command(110,240,c,keep_answer_table,'','Display whether keeping the answer table is enabled','Display whether keeping the answer table is enabled').
command(110,250,c,keep_answer_table,'Switch','Enable or disable  keeping the answer table','Enable or disable keeping the answer table (on or off, resp.)').
command(110,260,c,multiline,'','Display whether multi-line input is enabled','Display whether multi-line input is enabled').
command(110,270,c,multiline,'Switch','Enable or disable multi-line input','Enable or disable  multi-line input (on or off resp.) When enabled, Datalog inputs must end with a dot (.) and SQL inputs with a semicolon (;). When disabled, each line is considered as a single (Datalog or SQL) input and ending characters are optional').
command(110,280,c,nulls,'','Display whether nulls are enabled','Display whether nulls are enabled (on or off, resp.)').
command(110,290,c,nulls,'Switch','Enable or disable nulls','Enable or disable nulls (on or off, resp.)').
command(110,300,c,order_answer,'','Display whether displayed answers are ordered by default','Display whether displayed answers are ordered by default').
command(110,310,c,order_answer,'Switch','Enable or disable a default ordering of displayed computed tuples','Enable or disable a default (ascending) ordering of displayed computed tuples (on or off, resp.) This order is overriden if the user query contains either a group by specification or a call to a view with such a specification').
command(110,320,c,output,'','Display the display output mode','Display the display output mode (on, off or only_to_log). In mode ''on'', both console and log outputs are enabled. In mode ''off'', no output is enabled. In mode ''only_to_log'', only log output is enabled').
command(110,330,c,output,'Mode','Set the display output mode','Set the display output mode (on, off or only_to_log)').
command(110,340,c,pretty_print,'','Display whether pretty print listings is enabled','Display whether pretty print listings is enabled').
command(110,350,c,pretty_print,'Switch','Enable or disable pretty print','Enable or disable pretty print for listings (on or off, resp.)').
command(110,360,c,prompt,'','Display the current value for prompt format','Display the current value for prompt format (des, des_db or plain)').
% command(110,330,c,prompt,'Option','Set the format of the prompt (des, des_db, plain, prolog or no)','Set the format of the prompt. The value \'des\' sets the prompt to \'DES>\'. The value \'des_db\' adds the current database name DB as \'DES:DB>\' unless the current system is HR-SQL. In this case, \'HR-SQL(Connection)>\' is displayed. The value \'plain\' sets the prompt to \'>\'. The value \'prolog\' sets the prompt to \'?-\'. Finally, \'no\' display nothing for the prompt. Note that, for the values \'des\' and \'des_db\', if a language other than Datalog is selected, the language name preceded by a dash is also displayed before \'>\', as \'DES-SQL>\'). % but for HR-SQL, which is simply \'HR-SQL>\'').
command(110,370,c,prompt,'Option','Set the format of the prompt (des, des_db, plain, prolog or no)','Set the format of the prompt. The value \'des\' sets the prompt to \'DES>\'. The value \'des_db\' adds the current database name DB as \'DES:DB>\'. The value \'plain\' sets the prompt to \'>\'. The value \'prolog\' sets the prompt to \'?-\'. Finally, \'no\' display nothing for the prompt. Note that, for the values \'des\' and \'des_db\', if a language other than Datalog is selected, the language name preceded by a dash is also displayed before \'>\', as \'DES-SQL>\'). % but for HR-SQL, which is simply \'HR-SQL>\'').
command(110,380,c,reorder_goals,'','Display whether pushing equalities to the left is enabled','Display whether pushing equalities to the left is enabled').
command(110,390,c,reorder_goals,'Switch','Enable or disable pushing equalities to the left ','Enable or disable pushing equalities to the left (on or off, resp.) Equalities in bodies are moved to the left, which in general allows more efficient computations').
command(110,400,y(restore_default_status,''),'reset','','Synonym for /restore_default_status','Synonym for /restore_default_status').
command(110,410,c,restore_default_status,'','Restore the status of the system to the initial status','Restore the status of the system to the initial status, i.e., set all user-configurable flags to their initial values, including the default database and the start-up directory. Neither the database nor the extension table are cleared').
command(110,420,c,running_info,'','Display whether running information is to be displayed','Display whether running information, such as the incremental number of consulted rules as they are read and the current batch line, is to be displayed').
command(110,430,c,running_info,'Value','Enable display of running information','Enable or disable display of running information, such as the number of consulted rules as they are read (value ''on'') and the current batch line (value ''batch'', which applies only when /output is set to ''only_to_log''). The default switch value ''off'' disables this display').
command(110,440,c,safe,'','Display whether program transformation for unsafe rules is enabled','Display whether program transformation for unsafe rules is enabled').
command(110,450,c,safe,'Switch','Enable or disable safety transformation','Enable or disable program transformation (on or off, resp.)').
command(110,460,c,safety_warnings,'','Display whether singleton warnings are enabled','Display whether singleton warnings are enabled').
command(110,470,c,safety_warnings,'Switch','Enable or disable singleton warnings','Enable or disable singleton warnings (on or off, resp.)').
command(110,480,c,set_flag,'Flag Expr','Set the system flag Flag to Expr','Set the system flag Flag to the value corresponding to evaluating the expression Expr. An expression can be simply a constant value. Use quotes to delimit a string value (otherwise, it can be interpreted as a variable if it starts with either a capital letter or an underscore). Any system flag can be changed but unexpected behaviour can occur if thoughtlessly setting a flag').
command(110,490,c,show_compilations,'','Display whether compilations are to be displayed','Display whether compilations from SQL DQL statements to Datalog rules are to be displayed').
command(110,500,c,show_compilations,'Switch','Enable display of compilations','Enable or disable display of extended information about compilation of SQL DQL statements to Datalog clauses (on or off, resp.) The final executable Datalog form is only shown by enabling development listings with the command /development on, and it can contain further simplifications that are not shown when this flag is disabled').
command(110,510,c,show_sql,'','Display whether SQL compilations is to be displayed','Display whether SQL compilations are to be displayed').
command(110,520,c,show_sql,'Switch','Enable or disable display of SQL compilations','Enable or disable display of SQL compilations (on or off, resp.) SQL statements can come from either RA, or DRC, or TRC, or Datalog compilations. In this last case, they are intented to be externally processed').
command(110,530,c,simplification,'','Display whether program simplification is enabled','Display whether program simplification is enabled').
command(110,540,c,simplification,'Switch','Enable or disable program simplification','Enable or disable program simplification (on or off, resp.) Rules with equalities, true, and not(BooleanValue) are simplified. Simplification is always forced for SQL, RA, TRC and DRC compilations, irrespective of this setting').
command(110,550,c,singleton_warnings,'','Display whether singleton warnings are enabled','Display whether singleton warnings are enabled').
command(110,560,c,singleton_warnings,'Switch','Enable or disable singleton warnings','Enable or disable singleton warnings (on or off, resp.)').
command(110,570,c,sql_semantic_check,'','Display whether SQL semantic check is enabled','Display whether SQL semantic check is enabled').
command(110,580,c,sql_semantic_check,'Switch','Enable or disable SQL semantic check','Enable or disable SQL semantic check (on or off, resp.) When enabled, possible semantic errors are warned').
command(110,590,c,system_mode,'','Display the current system mode','Display the current system mode, which can be either ''des'' or ''fuzzy''').
command(110,600,c,system_mode,'Mode','Set the system mode to Mode (''des'' or ''fuzzy'')','Set the system mode to Mode (''des'' or ''fuzzy''). Switching between modes abolishes the current database. Can be used as a directive').
command(110,610,c,type_casting,'','Display whether automatic type casting is enabled','Display whether automatic type casting is enabled').
command(110,620,c,type_casting,'Switch','Enable or disable automatic type casting','Enable or disable automatic type casting (on or off, resp.) This applies to Datalog fact assertions and SQL insertions and selections. Enabling this provides a closer behaviour of SQL statement solving. Changing the status of this mode implies the recompilation of views in the local database').
command(110,630,c,undef_pred_warnings,'','Display whether undefined predicate warnings are enabled','Display whether undefined predicate warnings are enabled').
command(110,640,c,undef_pred_warnings,'Switch','Enable or disable undefined predicate warnings','Enable or disable undefined predicate warnings (on or off, resp.)').
command(110,650,c,unfold,'','Display whether program unfolding is enabled','Display whether program unfolding is enabled').
command(110,660,c,unfold,'Switch','Enable or disable program unfolding','Enable or disable program unfolding (on or off, resp.) Unfolding affects to the set of rules which result from the compilation of a single source rule. Unfolding is always forced for SQL and RA compilations, irrespective of this setting').

% Timing:
command(120, 10,c,date,'','Display the current host date','Display the current host date as specified by the command /date_format which, by default, is ISO 8601: YYYY-MM-DD for the year (YYYY), month (MM), and day (DD)').
command(120, 15,c,date_format,'','Display the current date format','Display the current date format').
command(120, 20,c,date_format,'Format','Set the date format for display and insert','Set the date format for display and insert, specifying dates as strings. Format is an unquoted string including single numeric occurrences of YYYY (year), MM (month), DD (day of the month), and a (single-char) separator between them. Default is ISO 8601: YYYY-MM-DD').
command(120, 25,c,datetime,'','Display the current host date and time','Display the current host date as specified by the command /date_format which, by default, is: YYYY-MM-DD for the year (YYYY), month (MM), and day (DD), and the host time as specified by the command /date_format which, by default, is: HH:Mi:SS for hours (HH), minutes (Mi), and seconds (SS) in 24-hour format according to extended ISO 8601').
command(120, 30,c,display_stopwatch,'','Display stopwatch','Display stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(120, 50,c,format_datetime,'','Display whether formatted date and time is enabled','Display whether formatted date and time is enabled. It is disabled by default').
command(120, 55,c,format_datetime,'Switch','Enable or disable formatted date and time for datetime values','Enable or disable formatted date and time for datetime values (on or off, resp.) If disabled (as default), dates are displayed as date(year, month, day), and time is displayed as time(hour, minute, second), both with positive numbers for each term argument. If enabled, dates are displayed in the date format as specified by the command /date_format, and time as specified by /time_format').
command(120, 50,c,format_timing,'','Display whether formatted timing for measured times is enabled, such as those displayed by statistics','Display whether formatted timing of measured times is enabled, such as those displayed by statistics').
command(120, 55,c,format_timing,'Switch','Enable or disable formatted timing for measured times, such as those displayed by statistics','Enable or disable formatted timing of measured times (on or off, resp.), such as those displayed by statistics. Given that ms, s, m, h represent milliseconds, seconds, minutes, and hours, respectively, times less than 1 second are displayed as ms; times between 1 second and less than 60 are displayed as s.ms; times between 60 seconds and less than 60 minutes are displayed as m:s.ms; and times from 60 minutes on are displayed as h:m:s.ms').
command(120, 60,c,reset_stopwatch,'','Reset stopwatch','Reset stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(120, 70,c,set_timeout,'','Display whether a default timeout is set','Display whether a default timeout is set. If set, any input is restricted to be processed for a time period of up to the specified number of seconds. If the timeout is exceeded, then the execution is stopped as if an exception was raised').
command(120, 80,c,set_timeout,'Value','Set the default timeout to Value (either in seconds as an integer or ''off'')','Set the default timeout to Value (either in seconds as an integer or ''off''). If an integer is provided, any input is restricted to be processed for a time period of up to this number of seconds. If the timeout is exceeded, then the execution is stopped as if an exception was raised. If Value is ''off'', the timeout is disabled').
command(120, 90,c,start_stopwatch,'','Start stopwatch','Start stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(120,100,c,stop_stopwatch,'','Stop stopwatch','Stop stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(120,110,c,time,'','Display the current host time','Display the current host time as HH:Mi:SS for hours (HH), minutes (Mi), and seconds (SS) in 24-hour format according to ISO 8601').
command(120,120,c,time,'Input','Process Input and display detailed elapsed time','Process Input and display detailed elapsed time. Its output is the same as processing Input with /timing detailed').
command(120, 15,c,time_format,'','Display the current time format','Display the current time format').
command(120, 20,c,time_format,'Format','Set the time format for display and insert','Set the time format for display and insert, specifying times as strings. Format is an unquoted string including single numeric occurrences of HH (hour), Mi (minute), SS (second), and a (single-char) separator between them. Default is the extended format ISO 8601: HH:Mi:SS').
command(120,130,c,timing,'','Display whether elapsed time display is enabled','Display whether elapsed time display is enabled').
command(120,140,c,timing,'Option','Sets the required level of elapsed time display','Sets the required level of elapsed time display as disabled, enabled or detailed (off, on or detailed, resp.)').
command(120,150,c,timeout,'Seconds Input','Process Input for up to Seconds','Process Input for a time period of up to the number of seconds specified in Seconds. If the timeout is exceeded, then the execution is stopped as if an exception was raised. Timeout commands can not be nested. In this case, the outermost command is the prevailing one').

% Statistics:
command(130, 10,c,db_rules,'','Display the number of rules in the database','Display the number of rules in the database. It includes all the rules that can be listed in development mode (including compilations to core Datalog). Therefore, a number greater than the user rules can be displayed. The system flag $db_rules$ is updated each time this command is executed').
command(130, 20,c,display_statistics,'','Display whether statistics display is enabled','Display whether statistics display is enabled or not (on or off, resp., and disabled by default)').
command(130, 30,c,display_statistics,'Switch','Enable or disable statistics display','Enable or disable statistics display (on or off, resp., and disabled by default). Enabling statistics display also enables statistics collection, but disabling statistics display does not disable statistics collection. Statistics include numbers for: Fixpoint iterations, EDB (Extensional Database - Facts) retrievals, IDB (Intensional Database - Rules) retrievals, ET (Extension Table) retrievals, ET lookups, CT (Call Table) lookups, CF (Complete Computations) lookups, ET entries and CT entries. Individual statistics can be displayed in any mode with write commands and system flags (e.g., /writeln $et_entries$). Enabling statistics incurs in a run-time overhead').
command(130, 40,c,host_statistics,'Keyword','Display host Prolog statistics for Keyword (\'runtime\' or \'total_runtime\')','Display host Prolog statistics for Keyword (\'runtime\' or \'total_runtime\'). For \'runtime\', this command displays the CPU time used while executing, excluding time spent in memory management tasks or in system calls since the last call to this command. For \'total_runtime\', this command displays the total CPU time used while executing, including memory management tasks such as garbage collection but excluding system calls since the last call to this command').
command(130, 50,c,statistics,'','Display whether statistics collection is enabled','Display whether statistics collection is enabled or not (on or off, resp.) It also displays last statistics, if enabled').
command(130, 60,c,statistics,'Switch','Enable or disable statistics collection','Enable or disable statistics collection (on or off, resp., and disabled by default). Statistics include numbers for: Fixpoint iterations, EDB (Extensional Database - Facts) retrievals, IDB (Intensional Database - Rules) retrievals, ET (Extension Table) retrievals, ET lookups, CT (Call Table) lookups, CF (Complete Computations) lookups, ET entries and CT entries. Individual statistics are displayed only in verbose mode, but they can be displayed in any mode with write commands and system flags (e.g., /writeln $et_entries$). Enabling statistics incurs in a run-time overhead').

% Scripting:
command(140, 10,c,if,'Condition Input','Process Input if Condition holds','Process Input if Condition holds. A condition is written as a Datalog goal, including all the primitive predicates, operators and functions. Condition may have to be enclosed between parentheses to avoid ambiguity').
command(140, 20,c,goto,'Label','Set the current script position to the next line where Label is located','Set the current script position to the next line where the label Label is located. A label is defined as a single line starting with a colon (:) and followed by its name. If the label is not found, an error is displayed and processing continue with the next script line. This command can not be the last one in a script and does not apply to interactive mode').
command(140, 30,c,process,'Filename [Parameters]','Process the contents of Filename','Process the contents of Filename as if they were typed at the system prompt. A parameter is a string delimited by either blanks or double quotes (") if the parameter contains a blank. The same is applied to Filename. The value for each parameter is retrieved by the tokens $parv1$, $parv2$, ... for the first, second, ... parameter, respectively').
command(140, 40,s(process,'Filename [Parameters]'),'p','Filename','Shorthand for /process Filename [Parameters]','Shorthand for /process Filename [Parameters]').
command(140, 45,c,run,'Filename [Parameters]','Process the contents of Filename stopping at user inputs for selected commands','Reminiscent of old 8 bit computers, this command allows for processing a file but retaining user input for selected commands such as /input. Process the contents of Filename as if they were typed at the system prompt. A parameter is a string delimited by either blanks or double quotes (") if the parameter contains a blank. The same is applied to Filename. The value for each parameter is retrieved by the tokens $parv1$, $parv2$, ... for the first, second, ... parameter, respectively').
command(140, 50,c,repeat,'Number Input','Repeat Input as many times as Number','Repeat Input as many times as Number, where Input can be any legal input at the command prompt').
command(140, 60,c,return,'','Stop processing of current script, returning a 0 code','Stop processing of current script, returning a 0 code. This code is stored in the system variable $return_code$. Parent scripts continue processing').
command(140, 70,c,return,'Code','Stop processing of current script, returning Code','Stop processing of current script, returning Code. This code is stored in the system variable $return_code$. Parent scripts continue processing').
command(140, 75,c,set,'','Display each user variable and its corresponding value','Display each user variable and its corresponding value').
command(140, 78,c,set,'Var','Display the value for the user variable Var','Display the value for the user variable Var').
command(140, 80,c,set,'Var Expr','Set the user variable Var to Expr','Set the user variable Var to the value corresponding to evaluating Expr. An expression can be simply a constant value. Use quotes to delimit a string value (otherwise, it can be interpreted as a variable if it starts with either a capital letter or an underscore). Refer to a user variable by delimiting it with dollars. If a user variable name coincides with the name of a system flag, the system flag overrides the user variable').
command(140, 90,c,set_default_parameter,'Index Value','Set the default value for the i-th parameter (denoted by the number Index) to Value','Set the default value for the i-th parameter (denoted by the number Index) to Value').
command(140,100,c,stop_batch,'','Stop batch processing','Stop batch processing. The last return code is kept. All parent scripts are stopped').
command(140,110,c,input,'Var','Wait for a user input (terminated by Intro) to be set on Var','Wait for a user input (terminated by Intro) to be set on the user variable Var').
command(140,180,c,write,'String','Write String to console','Write String to console. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after the command is considered as the string').
command(140,190,c,writeln,'String','Write String to console appending a new line','Write String to console appending a new line. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after the command is considered as the string').
% command(160,195,c,writeqln,'String','Quoted write of String to console appending a new line','Quoted write of String to console appending a new line. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after the command is considered as the string').
command(140,200,c,write_to_file,'File String','Write String to File','Write String to File. If File does not exist, it is created; otherwise, previous contents are not deleted and String is simply appended to File. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after File is considered as the string').
command(140,210,c,writeln_to_file,'File','Write a new line to File','Appends a new line to File. If File does not exist, it is created; otherwise, previous contents are not deleted and the new line is simply appended to File. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after the command is considered as the string').

% Miscellanea:
command(150, 10,c,csv,'','Display whether csv dump is enabled','Display whether csv dump is enabled. If so, the output csv file name is displayed').
command(150, 20,c,csv,'FileName','Enables semicolon-separated csv output of answer tuples','Enables semicolon-separated csv output of answer tuples. If FileName is ''off'', output is disabled. If the file already exists, tuples are appended to the existing file').
command(150, 30,c,debug_sql_bench,'NbrTables TableSize NbrViews MaxDepth MaxChildren FileName','Randomly generate a database instance and a mutated one','Randomly generate a database instance by specifying the number of tables (NbrTables) and its rows (TableSize), the maximum number of views (NbrViews), the height of the computation tree (i.e., the maximum number of view descendants in a genealogic line) (MaxDepth), the maximum number of children for views (MaxChildren), and the output filename (FileName) for the mutated SQL database. The name of the original instance is appended with ''_trust''').
command(150, 40,y(halt,''),'exit','','Synonym for /halt','Synonym for /halt').
command(150, 50,s(exit,''),'e','','Shorthand for /exit','Shorthand for /exit').
command(150, 60,c,generate_db,'NbrTables TableSize NbrViews MaxDepth MaxChildren FileName','Randomly generate a database instance in FileName','Randomly generate a database instance by specifying the number of tables (NbrTables) and its rows (TableSize), the maximum number of views (NbrViews), the height of the computation tree (i.e., the maximum number of view descendants in a genealogic line) (MaxDepth), the maximum number of children for views (MaxChildren), and the output filename (FileName)').
command(150, 70,c,halt,'','Quit DES','Quit DES. The Prolog host is also exited').
command(150, 80,c,mparse,'','Parse the next input lines, terminated by a single line containing $eot','Parse the next input lines as they were directly submitted from the prompt, terminated by a single line containing $eot. Return syntax errors and semantic warnings, if present in Input (only SQL DQL queries supported up to now). TAPI enabled').
command(150, 90,c,parse,'Input','Parse the input','Parse the input as it was directly submitted from the prompt, avoiding its execution. Return syntax errors and semantic warnings, if present in Input (only SQL DQL queries supported up to now). TAPI enabled').
command(150,100,y(halt,''),'quit','','Synonym for /halt','Synonym for /halt').
command(150,110,s(quit,''),'q','','Shorthand for /quit','Shorthand for /quit').
command(150,120,c,solve,'Input','Solve the input','Solve the input as it was directly submitted from the prompt. The command, used as a directive, can submit goals during consulting a Datalog program. Can be used as a directive').

% Implementor:
command(160, 10,c,breakpoint,'','Set a breakpoint','Set a breakpoint: start host Prolog debugging').
command(160, 15,c,debug,'','Enable debugging in the host Prolog interpreter','Enable debugging in the host Prolog interpreter. Only working for source distributions').
command(160, 20,c,indexing,'','Display whether hash indexing is enabled','Display whether hash indexing on memo tables is enabled').
command(160, 30,c,indexing,'Switch','Enable or disable hash indexing','Enable or disable hash indexing on extension table (on or off, resp.) Default is enabled, which shows a noticeable speed-up gain in some cases').
command(160, 40,c,nospyall,'','Remove all Prolog spy points','Remove all Prolog spy points in the host Prolog interpreter. Disable debugging. Only working for source distributions').
command(160, 50,c,nospy,'Pred[/Arity]','Remove the spy point on the given predicate','Remove the spy point on the given predicate in the host Prolog interpreter. Only working for source distributions').
command(160, 60,c,optimize_cc,'','Display whether complete computations optimization is enabled','Display whether complete computations optimization is enabled or not (on or off, resp.)').
command(160, 70,c,optimize_cc,'Switch','Enable or disable complete computations optimization','Enable or disable complete computations optimization (on or off, resp. and enabled by default). Fixpoint iterations and/or extensional database retrievals might been saved').
command(160, 80,c,optimize_ep,'','Display whether extensional predicates optimization is enabled','Display whether extensional predicates optimization is enabled or not (on or off, resp. and enabled by default)').
command(160, 90,c,optimize_ep,'Switch','Enable or disable extensional predicates optimization','Enable or disable extensional predicates optimization (on or off, resp. and enabled by default). Fixpoint iterations and extensional database retrievals are saved for extensional predicates as a single linear fetching is performed for computing them').
command(160,100,c,optimize_nrp,'','Display whether non-recursive predicates optimization is enabled','Display whether non-recursive predicates optimization is enabled or not (on or off, resp.)').
command(160,110,c,optimize_nrp,'Switch','Enable or disable non-recursive predicates optimization','Enable or disable non-recursive predicates optimization (on or off, resp. and enabled by default). Memoing is only performed for top-level goals').
command(160,120,c,optimize_st,'','Display whether stratum optimization is enabled','Display whether stratum optimization is enabled or not (on or off, resp. and disabled by default)').
command(160,130,c,optimize_st,'Switch','Enable or disable stratum optimization','Enable or disable stratum optimization (on or off, resp. and enabled by default). Extensional table lookups are saved for non-recursive predicates calling to recursive ones, but more tuples might be computed if the non-recursive call is filtered, as in this case an open call is submitted instead (i.e., not filtered)').
command(160,135,c,'optimize_sn','','Display whether differential semi-naive optimization is enabled','Display whether differential semi-naive optimization is enabled or not (on or off, resp. and disabled by default)').
command(160,140,c,'optimize_sn','Switch','Enable or disable differential semi-naive optimization','Enable or disable differential semi-naive optimization (on or off, resp. and enabled by default). Computing linear recursive predicates saves reusing useless tuples in older fixpoint iterations.').
command(160,145,c,spy,'Pred[/Arity]','Set a spy point on the given predicate','Set a spy point on the given predicate in the host Prolog interpreter. Binary distributions do not support spy points. Use source distributions instead').
command(160,150,c,system,'Goal','Submit a goal to the host system','Submit a goal to the host Prolog system').
command(160,160,c,terminate,'','Terminate the current DES session','Terminate the current DES session without halting the host Prolog system').
command(160,170,s(terminate,''),'t','','Shorthand for /terminate','Shorthand for /terminate').


% Fuzzy:
command(170, 10,c,fuzzy_answer_subsumption,'','Display whether fuzzy answer subsumption is enabled','Display whether fuzzy answer subsumption is enabled (on or off, resp. and enabled by default)').
command(170, 20,c,fuzzy_answer_subsumption,'Switch','Enable or disable fuzzy answer subsumption','Enable or disable fuzzy answer subsumption (on or off, resp. and enabled by default). Enabling fuzzy answer subsumption prunes answers for the same tuple with less approximation degrees, in general saving computations').
command(170, 30,c,fuzzy_expansion,'','Display current fuzzy expansion','Display current fuzzy expansion: bpl (Bousi~Prolog) or des (DES). For each fuzzy equation P~Q=D, the first one generates as many rules for Q as rules for P, whereas for the second one, generates only one rule for Q').
command(170, 40,c,fuzzy_expansion,'Value','Set the fuzzy expansion as of the given system','Set the fuzzy expansion as of the given system: bpl (Bousi~Prolog) or des (DES). If changed, the database is cleared. The value bpl is for experimental purposes and may develop unexpected behaviour when retracting either clauses or equations. Can be used as a directive').
command(170, 50,c,fuzzy_relation,'','Display each fuzzy relation and its properties','Display each fuzzy relation and its properties').
command(170, 60,c,fuzzy_relation,'Relation ListOfProperties','Set the relation with its properties','Set the relation name with its properties given as a list of: reflexive, symmetric and transitive. If a property is not given, its counter-property is assumed (irreflexive for reflexive, asymmetric for symmetric, and intransitive for transitive). Can be used as a directive').
command(170, 70,y(fuzzy_relation,'Relation ListOfProperties'),fuzzy_rel,'Relation ListOfProperties','Synonym for /fuzzy_relation Relation ListOfProperties','Synonym for /fuzzy_relation Relation ListOfProperties').
command(170, 80,c,lambda_cut,'','Display current lambda cut value','Display current lambda cut value, a float between 0.0 and 1.0. It defines a threshold for approximation degrees of answers').
command(170, 90,y(lambda_cut,''),lambdacut,'','Synonym for /lambda_cut','Synonym for /lambda_cut').
command(170,100,c,lambda_cut,'Value','Set the lambda cut value','Set the lambda cut value, a float between 0.0 and 1.0. It defines a threshold for approximation degrees of answers. Can be used as a directive').
command(170,110,y(lambda_cut,'Value'),lambda_cut,'Value','Synonym for /lambda_cut Value','Synonym for /lambda_cut Value. Can be used as a directive').
command(170,120,c,list_fuzzy_equations,'','List fuzzy proximity equations for ~','List fuzzy proximity equations of the form X~Y=D, meaning that the symbol X is similar to the symbol Y with approximation degree D. Equivalent to /list_fuzzy_equations ~').
command(170,130,c,list_fuzzy_equations,'Relation','List fuzzy equations for Relation','List fuzzy equations of the form X Relation Y = D, meaning that the symbol (either a predicate or constant) X is related under Relation to the symbol Y with approximation degree D').
command(170,140,c,list_t_closure,'','List the t-closure of ~','List the t-closure of the similarity relation ~ as fuzzy proximity equations of the form X~Y=D, meaning that the symbol X is similar to the symbol Y with approximation degree D. Equivalent to /list_t_closure ~. Can be used as a directive').
command(170,150,c,list_t_closure,'Relation','List the t-closure of Relation','List the t-closure of the relation Relation as fuzzy equations of the form X Relation Y=D, meaning that the symbol X is similar to the symbol Y with approximation degree D.  Can be used as a directive').
command(170,160,c,t_closure_comp,'','Display the way for computing the t-closure','Display the way for computing the t-closure of fuzzy relations, which can be either datalog or prolog. While the former uses the deductive engine for computing this t-closure, the latter uses a more-efficient, specific-purpose Floyd-Warshall algorithm').
command(170,170,c,t_closure_comp,'Value','Set the way for computing the t-closure','Set the way for computing the t-closure of fuzzy relations, which can be either datalog or prolog. While the former uses the deductive engine for computing this t-closure, the latter uses a more-efficient, specific-purpose Floyd-Warshall algorithm. Can be used as a directive').
command(170,180,y(t_closure_entries,'Relation'),t_closure_entries,'','Synonym for /t_closure_entries ~','Synonym for /t_closure_entries ~').
command(170,190,c,t_closure_entries,'Relation','Display the number of entries in the t-closure of Relation','Display the number of entries in the t-closure of Relation. The system flag $t_closure_entries$ is updated each time this command is executed').
command(170,200,y(t_norm,'Relation'),t_norm,'','Synonym for /t_norm ~','Synonym for /t_norm ~').
command(170,210,y(t_norm,'Relation Value'),t_norm,'Value','Synonym for /t_norm ~ Value','Synonym for /t_norm ~ Value. Can be used as a directive').
command(170,220,c,t_norm,'Relation','Display the current t-norm for Relation','Display the t-norm for the given relation, a value which can be: goedel, lukasiewicz, product, hamacher, nilpotent, where ''min'' is synonymous for ''goedel'', and ''luka'' for ''lukasiewicz''. Can be used as a directive').
command(170,230,c,t_norm,'Relation Value','Set the t-norm for the given relation','Set the t-norm for the given relation, a value which can be: goedel, lukasiewicz, product, hamacher, nilpotent, where ''min'' is synonymous for ''goedel'', and ''luka'' for ''lukasiewicz''. Can be used as a directive').
command(170,240,y(transitivity,'Relation'),transitivity,'','Synonym for /transitivity ~','Synonym for /transitivity ~').
command(170,250,y(transitivity,'Relation Value'),transitivity,'Value','Synonym for /transitivity ~ Value','Synonym for /transitivity ~ Value. Can be used as a directive').
command(170,260,c,transitivity,'Relation','Display the current transitivity setting for Relation','Display the current transitivity setting for Relation, which can be: transitive(no), transitive(goedel), transitive(lukasiewicz), transitive(product), transitive(hamacher), transitive(nilpotent), where ''min'' is synonymous for ''goedel'' and ''yes'', and ''luka'' for ''lukasiewicz''').
command(170,270,c,transitivity,'Relation Value','Set the transitivity setting for the given relation','Set the transitivity setting for the given relation, which can be: transitive(no), transitive(goedel), transitive(lukasiewicz), transitive(product), transitive(hamacher), transitive(nilpotent), where ''min'' is synonymous for ''goedel'' and ''yes'', and ''luka'' for ''lukasiewicz''').
command(170,280,c,weak_unification,'','Display current weak unification algorithm','Display current weak unification algorithm: a1 (Sessa) or a3 (Block-based). The algorithm a3, though a bit slower at run-time, is complete for proximity relations. However, it shows exponential time complexity for compilation').
command(170,290,c,weak_unification,'Value','Set the weak unification algorithm','Set the weak unification algorithm: a1 (Sessa) or a3 (Block-based). If changed, the database is cleared. The algorithm a3, though a bit slower at run-time, is complete for proximity relations. However, it shows exponential time complexity for compilation. Can be used as a directive').

% % LogiQL:
% command(180,10,c,logiql,'','Display whether LoqiQL output is enabled','Display whether LoqiQL output is enabled').
% command(180,20,c,logiql,'Switch','Enable or disable LoqiQL output','Enable or disable LoqiQL output (on or off, resp.)').
% command(180,30,s(logiql,''),'lq','','Shorthand for /logiql','Shorthand for /logiql').
% command(180,40,s(logiql,'Switch'),'lq','','Shorthand for /logiql Switch','Shorthand for /logiql Switch').
% command(180,50,c,logiql_log,'','Display the current LogiQL log file, if any','Display the current LogiQL log file, if any').
% command(180,60,c,logiql_log,'Filename','Set the current LogiQL log to the given filename','Set the current LogiQL log to the given filename overwriting the file, if exists, or creating a new one').
% command(180,70,c,logiql_log,'Mode Filename','Set the current LogiQL log to the given filename and mode','Set the current LogiQL log to the given filename and mode: write (overwriting the file, if exists, or creating a new one) or append (appending to the contents of the existing file, if exists, or creating a new one)').
% command(180,80,c,logiql_nolog,'','Disable LogiQL logging','Disable LogiQL logging').

% % HR-SQL:
% command(190,10,c,hrsql,'Connection','Switch to the HR-SQL system for an ODBC connection','Switch to the HR-SQL system for an ODBC connection. In this mode, first you can process (with the command /load_db) a file containing an HR-SQL database definition, second, an HR-SQL query (/load_hq), and third, a regular SQL query to an external relational database').
% command(190,20,c,load_db,'File','Load an HR-SQL database defined in File','Load the HR-SQL database defined in File. Only preprocessing is applied to the consulted relation definitions').
% command(190,30,c,process_db,'','Process the HR-SQL database already loaded','Process the (preprocessed) HR-SQL database already loaded in the local database, and generate and execute a Python script for the current relational database (as specified with the command /hrsql) in order to materialize all HR-SQL relation definitions').
% command(190,40,c,process_db,'File','Load and process the HR-SQL database in File','Load and process the HR-SQL database defined in File. Its result is the same as submitting first /load_db File and then /process_db').
% command(190,50,c,transform_db,'','Transform the loaded HR-SQL database into an R-SQL database','Transform the loaded (already preprocessed) HR-SQL database into an R-SQL database. This command is not needed to process an HR-SQL database, but a means to inspect the result of applying the transform algorithm. Anyway, after submitting this command, it is possible to process the current database with the command /process_db').

% ASSIGN NUMBERS TO COMMANDS:
% Run in SWI-Prolog
% Uncomment the following code, 
% run write_commands_new_ids, which generates cmds.pl, 
% and paste its contents above, replacing old command predicate
% write_commands_new_ids :-
%   setof(CategoryId-Category-Commands,
%         (command_category(CategoryId,Category),
%          setof(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc), 
%                command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc),
%                Commands)),
%         IdsCategoriesCommands),
%   tell('cmds.pl'),
%   write_commands(IdsCategoriesCommands),
%   told.

% write_commands([]).
% write_commands([_CategoryId-Category-Commands|Tail]) :-
%   write_log_list([nl, '% ',Category,':',nl]),
%   write_commands_list(Commands,10),
%   write_commands(Tail).

% write_commands_list([],_).
% write_commands_list([command(C,_N,A3,A4,A5,A6,A7)|Cmds],ID) :-
%   write_term(command(C,ID,A3,A4,A5,A6,A7),[character_escapes(true),quoted(true)]),
%   write('.'),
%   nl,
%   ID1 is ID+10,
%   write_commands_list(Cmds,ID1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Host-safe command safety
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

host_safe_category(Category) :-
  command_category(Category,_,yes).
  
unsafe_command(autosave).
% unsafe_command(open_db).
% unsafe_command(restore_ddb).
% unsafe_command(restore_state).
% unsafe_command(save_ddb).
% unsafe_command(save_state).
unsafe_command(set_flag).
% unsafe_command(set_var).
% unsafe_command(use_db).
unsafe_command(csv).
unsafe_command(debug_sql_bench).
unsafe_command(halt).
unsafe_command(exit).
unsafe_command(quit).
unsafe_command(generate_db).
unsafe_command(repeat).
unsafe_command(set_timeout).

safe_command(write,_).
safe_command(writeln,_).
safe_command(system,[Argument]) :-
  % Only safe for the goals that are listed in predicate host_safe_goal
  host_safe_goal(Argument).

host_safe_goal(retractall(table_modified(_))).