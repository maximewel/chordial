#!/bin/bash

cd ..

rebar3 clean
rebar3 compile
rebar3 shell --sname papanoel --setcookie chordial