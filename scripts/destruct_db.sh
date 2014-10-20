#!/bin/sh

echo "drop database if exists $1;" > dropper.txt
'/Applications/Postgres.app/Contents/Versions/9.3/bin'/psql -f dropper.txt
rm dropper.txt