/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.6.7                 */
/*                                                       */
/*    INITIALIZATION FILES                               */
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


:- initialization(ensure_loaded('des_atts.pl')).        % Attributed variables for type tracing
:- initialization(ensure_loaded('des_glue.pl')).        % Prolog system-dependent predicates
:- initialization(ensure_loaded('des_common.pl')).      % Common predicates
:- initialization(ensure_loaded('des_commands.pl')).    % Commands
:- initialization(ensure_loaded('des_help.pl')).        % Help system
:- initialization(ensure_loaded('des_sql_debug.pl')).   % SQL declarative debugger
:- initialization(ensure_loaded('des_sql_semantic.pl')). % SQL semantic checker
:- initialization(ensure_loaded('des_dl_debug.pl')).    % Datalog declarative debugger
%:- initialization(ensure_loaded('des_logiql.pl')).      % LogiQL subsystem
:- initialization(ensure_loaded('des_dbigen.pl')).      % DB instance generation
:- initialization(ensure_loaded('des_modes.pl')).       % Mode assertions
:- initialization(ensure_loaded('des_persistence.pl')). % Persistence support for predicates
:- initialization(ensure_loaded('des_sql.pl')).         % SQL query processor
:- initialization(ensure_loaded('des_ra.pl')).          % AR query processor
:- initialization(ensure_loaded('des_drc.pl')).         % DRC query processor
:- initialization(ensure_loaded('des_trc.pl')).         % TRC query processor
:- initialization(ensure_loaded('des_trace.pl')).       % Datalog and SQL tracers
:- initialization(ensure_loaded('des_tc.pl')).          % Test case generator
:- initialization(ensure_loaded('des_types.pl')).       % Type system
:- initialization(ensure_loaded('des_fuzzy.pl')).       % Fuzzy Datalog subsystem
