   --------------------------------
  | DES INSTALLATION (quick guide) |
   --------------------------------

================================================
Windows Binary Distribution with ACIDE GUI
================================================
- Double-click on des_acide.jar for starting the 
  ACIDE GUI. Requires Java JDK 1.8

================================================
Windows Binary Distribution
================================================
- Double-click on deswin.exe for starting the Windows
  application
- Execute des.exe for starting the console application

================================================
Linux/MacOSX Binary Distribution with ACIDE GUI
================================================
- Add execute permission to files des and des_start,
  typically with:
    chmod +x ./bin/des
    chmod +x ./bin/des_start
  or from the file explorer.
- Check unixodbc is installed
- Start ACIDE from a terminal with:
  java -jar des_acide.jar  
  Requires Java JDK 1.8

================================================
Linux/MacOSX Binary Distribution
================================================
- Add execute permission to files des and des_start,
  typically with:
    chmod +x ./bin/des
    chmod +x ./bin/des_start
  or from the file explorer.
- Check unixodbc is installed
- Start ./des in a terminal from its installation path

================================================
Windows Source Distributions
================================================
1. Create a shortcut in the desktop for running the Prolog 
   interpreter of your choice. 
2. Modify the start directory in the "Properties" dialog box 
   of the shortcut to the installation directory for DES. 
   This allows the system to consult the needed files at startup.
3. Append the following options to the Prolog executable complete 
   filename, depending on the Prolog interpreter you use:
   (a) SICStus Prolog: -l des.pl
   (b) SWI Prolog: -g "ensure_loaded(des)" (remove --win_app if present)
Another alternative is to write a batch file similar to the 
script file described just in the above section.
    
================================================
Linux/MacOSX Source Distributions
================================================
You can write a script for starting DES according to the 
selected Prolog interpreter, as follows:
(a) SICStus Prolog: 
    $SICSTUS -l des.pl 
    Provided that $SICSTUS is the variable which holds 
    the absolute filename of the SICStus Prolog executable.
(b) SWI Prolog: 
    $SWI -g "ensure_loaded(des)"
    Provided that $SWI is the variable which holds the 
    absolute filename of the SWI Prolog executable.

================================================
More Information: 
================================================

- See User Manual
  'Documentation' entry in 
  http://des.sourceforge.net/html/download.html
- http://des.sourceforge.net


Version 6.7 of DES (released on September, 4th, 2021)
 
* Enhancements:
  o	Added string solver for semantic analysis with disequality, inequality and domain constraints
  o	Improved precision for tautological and inconsistent semantic warnings involving autocast
  o	Added domain constraint to integer solver for semantic analysis
  o	Missing join condition, inconsistent condition, constant argument warnings for aggregates
  o	Added SQL semantic warning for grouped columns in a HAVING clause without an aggregate (Error 18 in [BG06])
  o	Added SQL semantic warning for repeated DISTINCT
  o	Added SQL semantic warning for COUNT on a not-null constrained argument
  o	Added semantic warning for OFFSET 0
  o	Dropped semantic false positive for DISTINCT in IN subqueries, which leads to better compilations
  o	Improved SQL semantic analysis precision for automatic type casting
  o	An SQL constraint may receive a void name
  o	Inform about possible column names when the requested column does not exist
  o	An SQL constraint name can be optionally given when altering tables
  o	Improved and more precise error messages for SQL grouping statements
  o	Interval check in SQL BETWEEN
  o	Reduced arguments in ORDER BY compilations
  o	SQL TOP, OFFSET and LIMIT clauses and RA top operator can include expressions instead of just integers
  o	SQL constants can be specified as 0-arity functions (with parentheses)
  o	Simplified compilations for SQL MINUS DISTINCT and INTERSECT DISTINCT
  o	Removed unneeded DISTINCT's in SQL DIVISION translation
  o	Simplification of true goals in top
  o	Reworked help on built-ins, which becomes interactive
  o	Added help on selection functions
  o	Added exception for compilation failures
  o	Added context information in stratum solving for verbose mode
  o	Added informative message for trying to removing an unexisting constraint
  o	Formatted exceptions on evaluation error 
  o	More flexible date formats
  o	Reused temporary rule identifiers
  o	Simplification of true goals in top
  o	The /if command accepts a generic Datalog goal, instead of just a comparison condition. Thus, the condition (goal) may have to be enclosed between parentheses
  o	The commands /save_state and /restore_state become TAPI-enabled

  o	New functions and predicates. For each function below, there is a counterpart Datalog predicate with the same name prepended with $, and with an extra final argument as output:
    - add_months(X,Y) Add to the datetime X the number of months Y
    - datetime_add(X,Y) Return the datetime X increased by the number Y. If X is a date, Y represents days, and seconds otherwise. This function is equivalent to the overloaded X + Y 
    - datetime_sub(X,Y) If Y is a number, return the datetime X decreased by the days Y. If X and Y are dates, return the number of days between them. If X and Y are either times or timestamps, return the number of seconds between them. This function is equivalent to the overloaded X - Y
    - greatest(X1,...,Xn) Return the greatest element Xi in the lexicographic order
    - instr(X,Y) Return the first numeric position in the string X of the searched substring Y
    - last_day(X) Return the last day of the month for the given datetime X
    - least(X1,...,Xn) Return the least element Xi in the lexicographic order
    - mod(X,Y) X modulo Y. Apply to two integers and return an integer
    - nvl2(X,Y,Z) Return either Y if X is a not null value, or Z otherwise
    - replace(X,Y,Z) Replace the string Y by Z in the given string X
    - reverse(X) Reverse the string X
    - rpad(X,Y) Return the given string X padded to the right with spaces, with the given total length Y
    - rpad(X,Y,Z) Return the given string X padded to the right with Z, with the given total length Y
    - to_char(X) Convert a datetime X to a string
    - to_char(X,Y) Convert a datetime to a string for a given format
    - to_date(X) Convert the string X to a date
    - to_date(X,Y) Convert the string X to a date for the given format Y
    - trim(X) Remove both leading and trailing spaces from the string X

  o New commands:
    - /command_assertions List commands that can be used as assertions. A Datalog program can contain assertions :- command(arg1,...,argn), where command is the command name, and argi are its arguments
    - /input Variable Wait for a user input (terminated by Intro) to be set on the user variable Variable
    - /list_predicates List predicates (name and arity). Include intermediate predicates which are a result of compilations if development mode is enabled (cf. the command /development). TAPI enabled
    - /mparse Input Parse the next input lines as they were directly submitted from the prompt, terminated by a single line containing $eot. Return syntax errors and semantic warnings, if present in Input (only SQL DQL queries supported up to now). TAPI enabled
    - /mtapi Process the next input lines by TAPI. It behaves as /tapi Input, where Input are the lines following /mtapi and terminated by a single line containing $eot
    - /parse Input Parse the input as it was directly submitted from the prompt, avoiding its execution. Return syntax errors and semantic warnings, if present in Input (only SQL DQL queries supported up to now).  TAPI enabled
    - /run Filename [Parameters] Reminiscent of old 8 bit computers, this command allows for processing a file but retaining user input for selected commands such as /input. Process the contents of Filename as if they were typed at the system prompt. Extensions by default are: .sql and .ini. When looking for a file f, the following filenames are checked in this order: f, f.sql, and f.ini. A parameter is a string delimited by either blanks or double quotes (") if the parameter contains a blank. The same is applied to Filename. The value for each parameter is retrieved by the tokens $parv1$, $parv2$, ... for the first, second, ... parameter, respectively
    - /sandboxed Synonym for /host_safe. Display whether host safe mode is enabled (on) or not (off). Enabling host safe mode prevents users and applications using DES from accessing the host (typically used to shield the host from outer attacks, hide host information, protect the file system, and so on)
    - /sandboxed Switch Synonym for /host_safe Switch. Enable host safe mode. Once enabled, this mode cannot be disabled 
    
* Changes:
  o	The command /builtins displays an interactive help based on categories. This can navigate to the interactive command help and vice versa
  o	Added separators in interactive help
  o	TAPI commands does not output elapsed time
  o	String values in SQL results are delimited between single quotes
  o	Null values are placed after any other symbol in the lexicographic and numeric orders. This affects the previous ordering of answer displays
  o	Write commands changed from category Implementor to Scripting
  o	When silent mode is enabled (with the command /silent on), display prompt inputs on batch processing are not displayed, thus behaving more similar to applying /silent Input to each line in the batch file
  o	Each call to rand returns the same value along fixpoint computation to avoid floundering (though different calls return in general different values)

* Fixed bugs:
  o	Exception catching in /save_ddb
  o	Nulls were not ignored when imposing a foreign key constraint
  o	Repeated column names in INSERT INTO did not raise an error
  o	Exception in some cases of RA natural full join
  o	Dropping a table with a foreign key to itselft raised unlimited warnings
  o	Some expressions involving SQL statements produced unordered compiled goals
  o	Incompatible schemas in SQL/RA division involving subqueries raised an exception instead of informing about the actual error
  o	SELECT ... INTO modified system variables instead of user variables
  o	Missing relation renamings in the right operand of SQL MINUS and INTERSECT
  o	Tuples causing constraint violations were not always quoted
  o	The constant pi was not detected with its type in SQL expressions
  o	Translation of nested full outer joins were incorrect
  o	Incorrect scoping in alias passing for SQL AND conditions
  o	False positive in missing join condition for a conjunctive condition
  o	Exception in corner cases of SQL autocasting, including arithmetic constants
  o	Expressions in IN statements were not handled properly
  o	Functions iif and case were not correctly solved in assumed contexts
  o	The system flag command_elapsed_time/2 was not deleted for TAPI commands
  o	Tuples with expressions in the left side of IN / NOT IN made parsing to fail
  o	Strings with the name of arithmetic constants were not correctly handled
  o	Unhandled exception in group_by out of bounds for propositional goals
  o	SQL debugger statistics were not shown with the command /debug_sql_statistics after a debugging session
  o	Exception in run-time autocasting for unquoted atoms
  o	Some SQL remarks in multiline mode were not correctly parsed
  o	Some nested top calls missed variables and solutions
  o	Applying top on comparison calls dealt no answer
  o	Arguments of /debug_dl were not read properly
  o	A bug in determining the stratum of a metapredicate goal implied missing answers in some queries
