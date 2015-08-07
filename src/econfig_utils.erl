%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  7 Aug 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(econfig_utils).

-export([mktemp/1,
	 system_tmpdir/0]).

mktemp(Template) ->
    TmpDir = system_tmpdir(),
    mktemp_(TmpDir, Template).

system_tmpdir() ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    "./tmp";
	_SysArch ->
	    "/tmp"
    end.

%%
%% Priv
%%
mktemp_(TmpDir, Template) ->
    TmpFile = filename:join([TmpDir, Template ++ io_lib:format(".~p", [erlang:phash2(make_ref())])]),
    case filelib:is_regular(TmpFile) of
	false -> 
	    ok = file:write_file(TmpFile, []),
	    TmpFile;
	true ->
	    mktemp_(TmpDir, Template)
    end.
