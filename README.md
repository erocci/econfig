# econfig

econfig is a configuration manager for component based applications.

It takes as input a list of files describing each component
variability model and build a graph of dependancies from it for
getting configuration values.

[![Build Status](https://travis-ci.org/erocci/econfig.svg)](https://travis-ci.org/erocci/econfig)

# Prerequisites

Get erlang/OTP platform from
https://www.erlang-solutions.com/downloads/download-erlang-otp

# Build

```
$ make
```

Default Makefile target build the `econfig` script.

# Usage

```
$ econfig help ...
```

# Options

## Frontend

By default, user is asked values from a tty based frontend. Frontend
can be selected with `-f` option.

Available frontends:
* tty (default): console based, interactive;
* defconfig: non interactive, set configuration with default values.

# Configuration format

`.econfig` files must be named after the component name. The file
contains a list of dot-terminated ('.') erlang terms. Each term is of
type `econfig_entry()`, as described in the `include/econfig.hrl` file:

```erlang
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

-type econfig_value_entry() :: {App :: atom(), Name :: atom(), Value :: econfig_value()}.

-type econfig_value() :: boolean() | integer() | string() | atom().

-type econfig_err()   :: {invalid_filename, string()}
		       | {cycle, term()}
		       | {badentry, econfig_entry_key()}.

```
