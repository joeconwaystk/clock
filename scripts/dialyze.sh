#!/bin/bash

rebar compile

# If cleaning, kill off old versions first
if [ "$1" == "reload" ]
    then
        rm ~/.dialyzer_plt
        rm .deps_plt
fi

# Build plt for Erlang/OTP code first if need be
if [ ! -e "$HOME/.dialyzer_plt" ]
    then
        dialyzer --build_plt --apps erts kernel stdlib crypto public_key ssl inets --output_plt ~/.dialyzer_plt
fi

# Built plt for all dependencies
if [ ! -e ".deps_plt" ]
    then
        dialyzer deps/*/ebin --build_plt --output_plt .deps_plt
fi

dialyzer --plts ~/.dialyzer_plt .deps_plt --src src