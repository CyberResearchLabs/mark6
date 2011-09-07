#!/bin/bash

ERL=/opt/local/bin/erl

ROOT=/opt/src/mark6/src/dimino6
CLIENT_DIR=${ROOT}/lib/client/ebin
CLIENT_MODULE=client
CLIENT_FUNC=start


${ERL} -pa ${CLIENT_DIR} \
	-run ${CLIENT_MODULE} ${CLIENT_FUNC} \
	-run init stop -noshell
