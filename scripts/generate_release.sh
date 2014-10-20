#!/bin/bash

NAME=$1

rm -rf "_rel/$NAME"

rebar compile

echo 'Configuring Application File...'
escript scripts/configure_app_source.erl

echo 'Configuring config file...'
escript scripts/configure_config.erl "$NAME" "${NAME}_release"

echo 'Configuring release...'
escript scripts/generate_relx.erl "$NAME" "${NAME}_release.config"

relx -V 0 --config "project/${NAME}_relx.config"

rm "project/${NAME}_relx.config"
rm "project/${NAME}_release.config"

if [ "$2" = "console" ]; then
    "./_rel/${NAME}/bin/${NAME}" console
fi