#!/bin/bash

ERL=/opt/local/bin/erl
ROOT=/home/dlapsley/mark6/src/tstmark6/

CLIENT_DIR=${ROOT}/ebin
CLIENT_MODULE=tstmark6
CLIENT_FUNC=start_server


${ERL} -pa ${CLIENT_DIR} \
	-s ${CLIENT_MODULE} ${CLIENT_FUNC} false \
	-noshell
