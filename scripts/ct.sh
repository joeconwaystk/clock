#!/bin/sh
rebar compile
mkdir -p testlogs

escript scripts/configure_config.erl test release_test $USER

sh scripts/generate_db src clock

if [ -f clock.sql ]; then
    '/Applications/Postgres.app/Contents/Versions/9.3/bin'/psql -f clock.sql
fi

# Note that -erl_args splits up stuff that ct needs and stuff that the emulator needs at runtime
ct_run -suite test/clock_SUITE -pa deps/*/ebin/ ebin -logdir testlogs -erl_args -config project/release_test.config

# Opens up the results automatically
open testlogs/index.html

rm project/release_test.config

if [ -f clock.sql ]; then
    sh scripts/destruct_db.sh clock
fi
