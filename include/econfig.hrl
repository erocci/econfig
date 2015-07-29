-ifndef(econfig_hrl).
-define(econfig_hrl, true).

-define(econfig_file, ".econfig").

-type econfig_entry() :: {Key     :: atom(),
			  Desc    :: string(), 
			  Type    :: econfig_entry_type(),
			  Default :: term(),
			  Options :: [econfig_entry_opt()]}.

-type econfig_entry_type() :: boolean
			    | string
			    | {enum, [atom()]}
			    | integer 
			    | {range, Low :: integer(), High :: integer()}.

-type econfig_entry_opt() :: {depends, [econfig_entry_dep()]}
			   | {help, string()}
			   | {priority, integer()}.

-type econfig_entry_dep() :: {Key :: econfig_entry_key(), Value :: econfig_entry_def()}.

-type econfig_entry_key() :: Name :: atom() | {App :: atom(), Name :: atom()}.

-type econfig_entry_def() :: boolean() | integer() | string() | atom() | {choice, tuple()} | '_'.

-type econfig_config() :: [econfig_value_entry()].

-type econfig_value_entry() :: {App :: atom(), Name :: atom(), Value :: econfig_value()}.

-type econfig_value() :: boolean() | integer() | string() | atom().

-type econfig_err()   :: {invalid_filename, string()}
		       | {cycle, term()}
		       | {badentry, econfig_entry_key()}
		       | {invalid_command, string()}
		       | {invalid_input, term()}
		       | {invalid_type, term()}
		       | eacces.

-define(frontends, [defconfig,tty]).

-endif.
