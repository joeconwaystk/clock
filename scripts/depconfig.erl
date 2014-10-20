#!/usr/bin/env escript

main([AppName, GithubUser, RepoName]) ->
	{ok, Data} = file:consult("rebar.config"),

    Deps = proplists:get_value(deps, Data),

    Path = "git://github.com/" ++ GithubUser ++ "/" ++ RepoName ++ ".git",

    NewEntry = {list_to_atom(AppName), ".*", {git, Path, "master"}},
    NewEntries = case Deps of
        undefined -> [NewEntry];
        _ -> [NewEntry | Deps]
    end,

	{ok, File} = file:open("rebar.config", [write]),
	
	lists:foreach(fun(E) -> 
		case E of
			{deps, _} ->
				file:write(File, io_lib:fwrite("~p.~n", [{deps, NewEntries}]));
			{Any, D} ->
				file:write(File, io_lib:fwrite("~p.~n", [{Any, D}]))
			end
		end, Data),

	file:close(File).
