#!/bin/sh
ERL=erl
COOKIE=cookie_{{appid}}
NODE_NAME={{appid}}@127.0.0.1
CONFIG=priv/{{appid}}.config
LIBS_DIR="./deps"

exec $ERL \
    -pa deps/*/ebin apps/*/ebin ebin \
    -boot start_sasl \
    -setcookie $COOKIE \
    -config $CONFIG \
    -env ERL_LIBS $LIBS_DIR \
    -s lager \
    -name $NODE_NAME \
    -s {{appid}}


