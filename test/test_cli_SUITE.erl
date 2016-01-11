%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Jan 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(test_cli_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

groups() ->
    [
     {all, [], [
		test_base
	       ,test_ns
	       ]},
     {all_default, [], [{group, all}]}
    ].


all() ->
    [{group, all_default}].


init_per_suite(Config) ->
    DftArgs = ["-C", ?config(data_dir, Config)],
    [{args, DftArgs} | Config].

end_per_suite(_Config) ->
    ok.


init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->    
    ok.

test_base(Config) ->
    Args = ?config(args, Config) ++ ["configure", "-f", "defconfig"],
    run(Args, Config),
    CfgFile = filename:join([?config(data_dir, Config), ".econfig"]),
    ?assertMatch({ok, [{"test_cli_SUITE_data.config1", true}]}, file:consult(CfgFile)),
    ok = file:delete(CfgFile).

test_ns(Config) ->
    Args = ?config(args, Config) ++ ["configure", "-f", "defconfig", "app:."],
    run(Args, Config),
    CfgFile = filename:join([?config(data_dir, Config), ".econfig"]),
    ?assertMatch({ok, [{"app.config1", true}]}, file:consult(CfgFile)),
    ok = file:delete(CfgFile).

%%%
%%% Priv
%%%
run(Args, Config) ->
    Cmd = filename:join([?config(data_dir, Config), "econfig"]) ++ " " ++ string:join(Args, " "),
    ct:log(default, ?STD_IMPORTANCE, "RUN: ~s", [Cmd]),
    os:cmd(Cmd).
    
