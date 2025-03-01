/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    Help on commands and built-ins                     */
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
% DISPLAY help on commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interactive help by command categories
display_help :-
  write_log_list(['Help on commands, classified in categories.', nl,
                  'Use /help keyword: For detailed help on a single <keyword>', nl,
                  'More info at des.sourceforge.net/html/manual/manualDES.html', nl, nl]),
  setof(CategoryId-Category,Safe^command_category(CategoryId,Category,Safe),IdCategoryList),
  my_unzip(IdCategoryList,_Ids,Categories),
  length(Categories,L),
  from(1,L,Ns),
  my_zipWith(':',Ns,Categories,NsCategories),
  forall(member(N:Category,NsCategories), (pad_atom_with(N,' ',2,PN), write_log_list([PN,': ',Category,nl]))),
  write_log_list([' A: All commands',nl]),
  write_log_list([' B: Help on built-ins',nl]),
  write_log_list([nl,'Type a category number or letter + Intro.',nl,'(Intro exits this help.)',nl]),
  repeat,
    user_input_string(Str),
    (is_tapi_command(Str,_)
     ->
      process_input_no_timeout(Str,_,_,_),
      set_flag(tapi,off),
      fail
     ;
      valid_help_input_str(Str,L,CN),
      (Str==""
       -> 
        true
       ;
        (CN == -2 -> display_help ;
         CN == -1 -> list_builtins ;
         CN == 0
         ->
          display_help_menu_delimiter,
          display_full_help,
          display_help_menu_delimiter,
          display_help
         ;
          my_nth1_member(SelectedCategory,CN,Categories),
          display_help_menu_delimiter,
          display_full_help(SelectedCategory),
          display_help_menu_delimiter,
          display_help
        )
      )
    ).
    
display_help_menu_delimiter :-
  write_log_list(['|=========================================================================|',nl]).


valid_help_input_str(Str,_T,0) :-
  my_kw("A",Str,""),
  !.
valid_help_input_str(Str,_T,-1) :-
  my_kw("B",Str,""),
  !.
valid_help_input_str(Str,_T,-2) :-
  my_kw("C",Str,""),
  !.
valid_help_input_str("",_T,_N) :-
  !.
valid_help_input_str(Str,T,N) :-
  my_positive_integer(N,Str,""),
  !,
  (between(1,T,N)
   -> 
    true
   ;
    write_log_list(['Please input a number between 1 and ',T,'.',nl]),
    fail
  ).
valid_help_input_str(_Str,_T,_N) :-
  write_log_list(['Please input a valid number or simply Intro to exit.',nl]),
  fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List help on commands

% display_full_help :-
%   system_mode(hrsql),
%   !,
%   display_hrsql_help.
display_full_help :-
  display_full_help(_Category).
  
display_full_help(Category) :-
  write_log_list(['|--COMMAND-------------|--SHORT HELP--------------------------------------|',nl]),
  setof(CategoryId-Category-Commands,
        Safe^(command_category(CategoryId,Category,Safe),
         setof(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc), 
               C^A^(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc),
                (Type=c ; Type=y(C,A))),
               Commands)),
        IdsCategoriesCommands),
  display_categories_commands(IdsCategoriesCommands).
  
display_categories_commands([]).
display_categories_commands([_CategoryId-Category-Commands|Tail]) :-
  write_log_list(['* ',Category,':',nl]),
  my_map(display_command_short,Commands),
  display_categories_commands(Tail).

display_command_short(command(_CategoryId,_CommandId,_CommandType,Command,Arguments,ShortDesc,_ExtDesc)) :-
  Width=75,
  Tab=24,
  atomic_concat_list(['   /',Command,' ',Arguments],ACommand),
  atom_length(ACommand,Length),
  (Length >= Tab -> write_log_list([ACommand,nl]), my_spaces(24,S), write_log_list([S]) ; write_unquoted_tab_log(ACommand,Tab)),
  display_width_restricted(Width,Tab,ShortDesc),
  display_command_shorthands(24,Command,Arguments),
  display_command_synonyms(24,Command,Arguments,Command).

display_command_shorthands(Tab,Cmd,CmdArgs) :-
  setof(BCommand,
        CategoryId^CommandId^Command^Arguments^ShortDesc^ExtDesc^(command(CategoryId,CommandId,s(Cmd,CmdArgs),Command,Arguments,ShortDesc,ExtDesc),atom_concat('/',Command,BCommand)),
        LShorthands),
  !,
  my_list_to_tuple(LShorthands,TShorthands),
  (LShorthands=[_] -> P='' ; P='s'),
  my_spaces(Tab,S),
  write_log_list([S,'Shorthand',P,': ',TShorthands,nl]).
display_command_shorthands(_Tab,_Cmd,_CmdArgs).
  
display_command_synonyms(Tab,Cmd,CmdArgs,OriCmd) :-
  setof(BCommand,
    CategoryId^CommandId^Command^Arguments^ShortDesc^ExtDesc^
    (command(CategoryId,CommandId,y(Cmd,CmdArgs),Command,Arguments,ShortDesc,ExtDesc),
     atom_concat('/',Command,BCommand),
     OriCmd\==Command),
    LSynonyms),
  !,
  my_list_to_tuple(LSynonyms,TSynonyms),
  (LSynonyms=[_] -> P='' ; P='s'),
  my_spaces(Tab,S),
  write_log_list([S,'Synonym',P,': ',TSynonyms,nl]).
display_command_synonyms(_Tab,_Cmd,_CmdArgs,_OriCmd).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% List help on keyword. It can be either a command or built-in
  
display_help(KW) :-
  findall(KW,display_help_command(KW,_Args),CKWs),
  findall(KW,display_help_builtins(KW),BKWs),
  append(CKWs,BKWs,[_|_]).
display_help(KW) :-
  write_warning_log(['Unknown keyword ''',KW,'''']),
  display_object_alternatives(command,KW),
  display_object_alternatives(builtin,KW).
  
display_help_command(Command,Arguments) :-
  display_help_command(Command,Arguments,Command).
  
display_help_command(Command,Arguments,OriginCommand) :-
  setof(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc), 
        command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc),
        Commands),
  !,
  display_command_extended_list(Commands,OriginCommand).
  
display_help_builtins(KW) :-
  list_builtins(KW),
  !.
  
  
display_command_extended(command(_CategoryId,_CommandId,CommandType,Command,Arguments,_ShortDesc,ExtDesc),OriginCommand) :-
  write_log_list(['/',Command,' ',Arguments,' : ',ExtDesc,nl]),
  display_command_shorthands(0,Command,Arguments),
  display_command_synonyms(0,Command,Arguments,OriginCommand),
  (CommandType=y(Cmd,CmdArgs) -> display_help_command(Cmd,CmdArgs,OriginCommand) ; true),
  (CommandType=s(Cmd,CmdArgs) -> display_help_command(Cmd,CmdArgs,OriginCommand) ; true).
  
display_command_extended_list([],_OriginCommand).
display_command_extended_list([Command|Commands],OriginCommand) :-
  display_command_extended(Command,OriginCommand),
  display_command_extended_list(Commands,OriginCommand).
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISPLAY help on builtins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interactive help by built-in categories
list_builtins :-
  write_log_list(['Help on builtins, classified in categories.', nl,
                  'Use /help keyword: For detailed help on a single <keyword>', nl,
                  'More info at des.sourceforge.net/html/manual/manualDES.html', nl, nl]),
  setof(CategoryId-Category,B^M^G^A^builtin_category(CategoryId,Category,B,M,G,A),IdCategoryList),
  my_unzip(IdCategoryList,_Ids,Categories),
  length(Categories,L),
  from(1,L,Ns),
  my_zipWith(':',Ns,Categories,NsCategories),
  forall(member(N:Category,NsCategories), (pad_atom_with(N,' ',2,PN), write_log_list([PN,': ',Category,nl]))),
  write_log_list([' A: All builtins',nl]),
  write_log_list([' C: Help on commands',nl]),
  write_log_list([nl,'Type a category number or letter + Intro.',nl,'(Intro exits this help.)',nl]),
  repeat,
    user_input_string(Str),
    (is_tapi_command(Str,_)
     ->
      process_input_no_timeout(Str,_,_,_),
      set_flag(tapi,off),
      fail
     ;
      valid_help_input_str(Str,L,CN),
      (Str==""
       -> 
        true
       ;
        (CN == -2 -> display_help ;
         CN == -1 -> list_builtins ;
         CN == 0
         ->
          display_help_menu_delimiter,
          list_builtins(_,_),
          display_help_menu_delimiter,
          list_builtins
         ;
          my_nth1_member(SelectedCategory,CN,Categories),
          display_help_menu_delimiter,
          list_builtins(_,SelectedCategory),
          display_help_menu_delimiter,
          list_builtins
        )
      )
    ).

builtin_category(1, 'Comparison Operators',B,M,my_infix_comparison(B,M),[]).
builtin_category(2, 'Prefix Operators',    B,M,unary_operator(B,_,M),[]).
builtin_category(3, 'Infix Operators',     B,M,my_infix_operator(B,_,_,_,M,_,_),[]).
builtin_category(4, 'Arithmetic Functions',B,M,function(B,_,M,arithmetic,_,_),[]).
builtin_category(5, 'String Functions',    B,M,function(B,_,M,string,_,_),[]).
builtin_category(6, 'Date/Time Functions', B,M,function(B,_,M,datetime,_,_),[]).
builtin_category(7, 'Selection Functions', B,M,function(B,_,M,selection,_,_),[]).
builtin_category(8, 'Aggregate Functions', B,M,function(B,_,M,aggregate,_,Arity),['/',Arity]).
builtin_category(9, 'Arithmetic Constants',B,M,function(B,_,M,arithmetic_cte,_,_),[]).
builtin_category(10,'Predicates',          _,_,predicates,_).

list_builtins(KW) :-
  list_builtins(KW,_Category).
  
% list_builtins(?Builtin,?Category)
list_builtins(B,C) :-
  Width=75,
  Tab=9,
  list_builtins(Width,Tab,B,M,my_infix_comparison(B,M),[],C,'Comparison Operators',Found),
  list_builtins(Width,Tab,B,M,unary_operator(B,_,M),[],C,'Prefix Operators',Found),
  list_builtins(Width,Tab,B,M,my_infix_operator(B,_,_,_,M,_,_),[],C,'Infix Operators',Found),
  list_builtins(Width,Tab,B,M,function(B,_,M,arithmetic,_,_),[],C,'Arithmetic Functions',Found),
  list_builtins(Width,Tab,B,M,function(B,_,M,string,_,_),[],C,'String Functions',Found),
  list_builtins(Width,Tab,B,M,function(B,_,M,datetime,_,_),[],C,'Date/Time Functions',Found),
  list_builtins(Width,Tab,B,M,function(B,_,M,selection,_,_),[],C,'Selection Functions',Found),
  list_builtins(Width,Tab,B,M,function(B,_,M,aggregate,_,Arity),['/',Arity],C,'Aggregate Functions',Found),
  list_builtins(Width,Tab,B,M,function(B,_,M,arithmetic_cte,_,_),[],C,'Arithmetic Constants',Found),
  %list_builtins(Width,Tab,_B,_M,predicates,[],C,'Predicates',Found),
  list_builtins(Width,Tab,B,_M,predicates,[],C,'Predicates',Found),
  !,
  nonvar(Found).

list_builtins(Width,Tab,Builtin,Message,Goal,Arity,InCategory,Category,Found) :-
  InCategory\=='Predicates',
  Goal\==predicates,
  (nonvar(InCategory) -> Category==InCategory, Category=InCategory ; true),
  !,
  (\+ \+ call(Goal) -> 
    write_log_list(['* ',Category,':',nl]),
    Found=true
   ;
    true),
  (call(Goal),
   atomic_concat_list(['   ',Builtin|Arity],T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,Message),
   fail
   ; 
   true).
%list_builtins(Width,Tab,_B,_M,Goal,_A,InCategory,Category,Found) :-
list_builtins(Width,Tab,B,_M,Goal,_A,InCategory,Category,Found) :-
  Goal==predicates,
  (nonvar(InCategory) -> Category==InCategory, Category=InCategory ; true),
  (\+ \+ (my_infix_relation(B,_) ; my_builtin_relation(B,_,_,_)) ->
    write_log_list(['* ',Category,':',nl]),
    Found=true
   ;
    true),
  (my_infix_relation(B,M),
   atomic_concat_list(['   ',B,'/','2'],T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (my_builtin_relation(B,A,M,_),
   number_codes(A,As),
   atom_codes(Ar,As),
   atomic_concat_list(['   ',B,'/',Ar],T),
   write_unquoted_tab_log(T,8), 
   write_log(' '), 
   display_width_restricted(Width,Tab,M),
   fail
   ; 
   true),
  (\+ \+ B=not ->
    write_log_list(['   not/1 Stratified negation',nl]),
    Found=true
   ;
    true),
  (\+ \+ B=answer ->
    write_log_list(['   answer/N',nl,'$tab'(Tab)]),
    display_width_restricted(Width,Tab,'Reserved word for the outcome relation of an automatic temporary view'),
    Found=true
   ;
    true).
list_builtins(_Width,_Tab,_B,_M,_G,_A,_C,_Category,_Found).
   
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DISPLAY. Restricted to a page width in columns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
display_width_restricted(Width,Tab,Atom) :-
  atom_length(Atom,AtomLength), 
  CWidth is AtomLength+Tab,
  (CWidth > Width
   ->
    LineWidth is Width-Tab,
    split_line(Atom,LineWidth,A1,A2),
%    write_log_list([A1,nl,'$tab'(Tab),A2,nl])
    write_log_list([A1,nl,'$tab'(Tab)]),
    display_width_restricted(Width,Tab,A2)
   ; 
    write_log_list([Atom,nl])).
    
split_line(L,Width,L1,L2) :-
  atom_codes(L,Ls),
  setof((M,L1,L2),
    LL1^L1s^L2s^(
      concat_lists([L1s," ",L2s],Ls),
      length(L1s,LL1),
      M is Width-LL1, 
      M>=0, 
      atom_codes(L1,L1s), 
      atom_codes(L2,L2s)),
        [(M,L1,L2)|_]).
  
