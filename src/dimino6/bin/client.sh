#!/bin/bash

ERL=/opt/local/bin/erl

ROOT=/opt/src/mark6/src/dimino6
CLIENT_DIR=${ROOT}/lib/client/ebin
CLIENT_MODULE=client
CLIENT_FUNC=start_server


${ERL} -pa ${CLIENT_DIR} \
	-s ${CLIENT_MODULE} ${CLIENT_FUNC} false \
	-run init stop -noshell
