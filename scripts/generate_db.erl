#!/usr/bin/env escript
%%! -pa ebin deps/sch/ebin

% Kind is local for local builds
main([ModelDirectoryPath, DBName]) ->
    {ok, Filenames} = file:list_dir(ModelDirectoryPath),
    Modules = lists:filtermap(fun (File) ->
        case string:str(File, "model_") of
            1 ->
                Tokens = string:tokens(File, "."),
                [ModuleName | [Extension]] = Tokens,
                case Extension of
                    "erl" -> {true, list_to_atom(ModuleName)};
                    _ -> false
                end;
            _ ->
                false
        end
    end, Filenames),


    case length(Modules) of
        0 -> ok;
        _ -> generate_from_modules(Modules, DBName)
    end.

generate_from_modules(Modules, DBName) ->
    Schemas = lists:map(fun (ModelModule) ->
        Name = ModelModule:name(),
        Schema = ModelModule:schema(),
        Map = maps:new(),
        maps:put(Name, Schema, Map)
    end, Modules),

    Tables = lists:map(fun (SchemaMap) ->
        [ModelName] = maps:keys(SchemaMap),
        Schema = maps:get(ModelName, SchemaMap),
        table(ModelName, Schema)
    end, Schemas),

    IndicesAndConstraints = lists:flatten(lists:map(fun (SchemaMap) ->
        [ModelName] = maps:keys(SchemaMap),
        Schema = maps:get(ModelName, SchemaMap),
        extras(ModelName, Schema)
    end, Schemas)),

    {ok, OutFile} = file:open(DBName ++ ".sql", [write]),

    ok = file:write(OutFile, io_lib:fwrite("\\c postgres~n", [])),
    ok = file:write(OutFile, io_lib:fwrite("drop database if exists ~s;~n", [DBName])),
    ok = file:write(OutFile, io_lib:fwrite("create database ~s;~n", [DBName])),
    ok = file:write(OutFile, io_lib:fwrite("\\c ~s~n", [DBName])),

    ok = file:write(OutFile, io_lib:fwrite("CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;", [])),
    ok = file:write(OutFile, io_lib:fwrite("CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\" WITH SCHEMA public;", [])),

    lists:foreach(fun (Term) ->
        ok = file:write(OutFile, io_lib:fwrite("~s~n", [binary_to_list(Term)]))
    end, Tables),
    lists:foreach(fun (Term) ->
        ok = file:write(OutFile, io_lib:fwrite("~s~n", [binary_to_list(Term)]))
    end, IndicesAndConstraints),

    file:close(OutFile).


extras(ModelName, Schema) ->
    Extras = lists:map(fun (ColumnName) ->
        ColumnInfo = maps:get(ColumnName, Schema),
        {ColumnName, ColumnInfo}
    end, maps:keys(Schema)),

    lists:flatten(lists:map(fun ({Name, Info}) ->
        extra(ModelName, Name, Info)
    end, Extras)).


extra(ModelName, ColumnName, {_, Options}) ->
    lists:filtermap(fun (Opt) ->
        case Opt of
            index ->
                {true, <<"create index on ", ModelName/binary, "(", ColumnName/binary, ");">>};
            {relationship, ForeignAtom, DeleteRule} ->
                ForeignName = ForeignAtom:name(),
                DeleteRuleString = delete_rule_string(DeleteRule),
                {true, [
                        <<
                            "alter table only ",
                            ModelName/binary,
                            " add foreign key (",
                            ColumnName/binary,
                            ") references ",
                            ForeignName/binary,
                            "(_id) on delete ",
                            DeleteRuleString/binary,
                            ";"
                        >>,
                        <<"create index on ", ModelName/binary, "(", ColumnName/binary, ");">>
                    ]
                };
            _ -> false
        end
    end, Options);
extra(_, _, _) ->
    <<>>.

delete_rule_string(restrict) ->
    <<"restrict">>;
delete_rule_string(cascade) ->
    <<"cascade">>;
delete_rule_string(nullify) ->
    <<"set null">>;
delete_rule_string(default) ->
    <<"set default">>.


table(ModelName, Schema) ->
    Columns = columns(Schema),
    <<"create table ", ModelName/binary, " (", Columns/binary, ");">>.


columns(Schema) ->
    Columns = lists:filtermap(fun (ColumnName) ->
        ColumnInfo = maps:get(ColumnName, Schema),
        case ColumnInfo of
            {inverse, _} -> false;
            ColumnInfo ->
                {true, column(ColumnName, ColumnInfo)}
        end
    end, maps:keys(Schema)),

    sch_utils:binary_join(Columns, <<",">>).

column(<<"_id">>, _) ->
    <<"_id uuid primary key">>;
column(ColumnName, {Type, Options}) ->
    ColumnType = column_type(Type),
    Opts = column_options(Options),
    <<ColumnName/binary, " ", ColumnType/binary, " ", Opts/binary>>;
column(ColumnName, Type) ->
    ColumnType = column_type(Type),
    <<ColumnName/binary, " ", ColumnType/binary>>.

column_type(Type) ->
    case Type of
        id -> <<"uuid">>;
        string -> <<"text">>;
        integer -> <<"integer">>;
        double -> <<"double">>;
        float -> <<"real">>;
        timestamp -> <<"timestamp without time zone">>;
        boolean -> <<"boolean">>;
        binary -> <<"bytea">>
    end.

% Must handle later: index, foreign key constraint, delete rule?
column_options(Opts) ->
    List = lists:map(fun (Option) ->
        case Option of
            required -> <<"not null">>;
            unique -> <<"unique">>;
            {default, DefaultVal} ->
                default_value(DefaultVal);
            _ -> <<>>
        end
    end, Opts),
    sch_utils:binary_join(List, <<" ">>).

default_value(DefaultVal) when is_binary(DefaultVal) ->
    <<"default ", DefaultVal/binary>>;
default_value(DefaultVal) when is_integer(DefaultVal) ->
    default_value(integer_to_binary(DefaultVal));
default_value(DefaultVal) when is_float(DefaultVal) ->
    default_value(float_to_binary(DefaultVal));
default_value(true) ->
    default_value(<<"true">>);
default_value(false) ->
    default_value(<<"false">>).