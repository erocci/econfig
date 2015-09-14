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
			   | {priority, integer()}
			   | {call, mfa()}.

-type econfig_entry_dep() :: {Key :: econfig_entry_depkey(), Value :: econfig_entry_def()}
			   | {Key :: econfig_entry_depkey()}.

-type econfig_entry_key() :: Name :: atom() | {App :: atom(), Name :: atom()}.

-type econfig_entry_depkey() :: {Name :: atom()} | {App :: atom(), Name :: atom()}.

-type econfig_entry_def() :: boolean()
			   | integer()
			   | string() 
			   | atom()
			   | {choice, list()}
			   | {lt, integer()}
			   | {gt, integer()}
			   | '_'.

-type econfig_config() :: [econfig_value_entry()].

-type econfig_value_entry() :: {App :: atom(), Name :: atom(), Value :: econfig_value()}.

-type econfig_value() :: boolean() | integer() | string() | atom().

-type econfig_err()   :: {invalid_filename, string()}
		       | {cycle, term()}
		       | {badentry, term()}
		       | {invalid_command, string()}
		       | {invalid_input, term()}
		       | {invalid_type, term()}
		       | {invalid_config, term()}
		       | {missing_source, term()}
		       | eacces.

-define(frontends, [defconfig,tty]).

-endif.
