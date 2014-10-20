#!/usr/bin/env escript

main([]) ->

    % Find all app dependencies
    {ok, RebarConfig} = file:consult("rebar.config"),

    Deps = proplists:get_value(deps, RebarConfig),

    DepApps = lists:map(fun ({DepAppName, _, _}) ->
        DepAppName
    end, Deps),

    {ok, SourcePaths} = file:list_dir("src"),

    Modules = lists:filtermap(fun (File) ->
        Components = string:tokens(File, "."),
        case lists:last(Components) of
            "erl" -> {true, list_to_atom(hd(Components))};
            _ -> false
        end
    end, SourcePaths),


    [AppSourcePath] = lists:filter(fun (P) ->
        case string:str(P, "app.src") of
            0 -> false;
            _ -> true
        end
    end, SourcePaths),

    {ok, [{application, AppName, Values}]} = file:consult("src/" ++ AppSourcePath),

    InitialApps = proplists:get_value(applications, Values),

    ModulesRemoved = proplists:delete(modules, Values),
    AppsRemoved = proplists:delete(applications, ModulesRemoved),
    ModulesAdded = lists:append(AppsRemoved, [{modules, Modules}]),

    % Remove any initial apps from dependency apps
    DepAppsFiltered = lists:filter(fun (App) ->
        case lists:member(App, InitialApps) of
            true -> false;
            false -> true
        end
    end, DepApps),

    AppsAdded = lists:append(ModulesAdded, [{applications, lists:merge(InitialApps, DepAppsFiltered)}]),

    {ok, File} = file:open("src/" ++ AppSourcePath, [write]),

    ok = file:write(File, io_lib:fwrite("~p.~n", [{application, AppName, AppsAdded}])),

    file:close(File).

