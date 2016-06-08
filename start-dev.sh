#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname erl_om_dev \
    -s erl_om \
    -s reloader
