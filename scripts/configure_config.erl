#!/usr/bin/env escript

main([InName, OutName]) ->
    {ok, [Base]} = file:consult("project/base.config"),
    {ok, [Top]} = file:consult(string:join(["project", InName ++ ".config"], "/")),

    FilteredBase = lists:filter(fun ({App, _Config}) ->
        case proplists:is_defined(App, Top) of
            true -> false;
            false -> true
        end
    end, Base),

    Result = lists:append(FilteredBase, Top),

    {ok, OutFile} = file:open("project/" ++ OutName ++ "." ++ "config", [write]),

    Terms = io_lib:fwrite("~p.~n", [Result]),

    ok = file:write(OutFile, [Terms]),

    file:close(OutFile);
main([InName, OutName, User]) ->

    {ok, [Base]} = file:consult("project/base.config"),
    {ok, [Top]} = file:consult(string:join(["project", InName ++ ".config"], "/")),

    FilteredBase = lists:filter(fun ({App, _Config}) ->
        case proplists:is_defined(App, Top) of
            true -> false;
            false -> true
        end
    end, Base),

    Result = lists:append(FilteredBase, Top),

    {ok, OutFile} = file:open("project/" ++ OutName ++ "." ++ "config", [write]),

    Terms = io_lib:fwrite("~p.~n", [Result]),

    TermsAdjusted = re:replace(Terms, "TEMPLATE_USER", User),

    ok = file:write(OutFile, [TermsAdjusted]),

    file:close(OutFile).