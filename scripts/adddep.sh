#!/bin/bash

escript scripts/depconfig.erl $1 $2 $3
rebar get-deps