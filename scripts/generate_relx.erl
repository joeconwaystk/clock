#!/usr/bin/env escript

% Kind is local for local builds
main([Kind, SysConfigName]) ->
    {ok, Data} = file:consult("rebar.config"),

    {ok, SourcePaths} = file:list_dir("src"),

    [AppSourcePath] = lists:filter(fun (P) ->
        case string:str(P, "app.src") of
            0 -> false;
            _ -> true
        end
    end, SourcePaths),

    {ok, [{application, AppName, _}]} = file:consult("src/" ++ AppSourcePath),

    Release = {release, {list_to_atom(Kind), "1"}, [AppName]},

    RelxFileName = Kind ++ "_relx.config",
    RelxFilePath = string:join(["project", RelxFileName], "/"),
    {ok, File} = file:open(RelxFilePath, [write]),

    file:write(File, io_lib:fwrite("~p.~n", [Release])),
    file:write(File, io_lib:fwrite("{extended_start_script, true}.~n", [])),

    %SysConfigName = "release_" ++ Kind ++ ".config",

    file:write(File, io_lib:fwrite("{sys_config, ~p}.~n", [SysConfigName])),

    file:close(File).
