#!/bin/bash

# ERL=/opt/local/bin/erl
ERL=/usr/bin/erl
ROOT=/opt/mit/mark6
VERSION=0.1

CLIENT_DIR=${ROOT}/lib/vdiff-${VERSION}/ebin
CLIENT_MODULE=vdiff
CLIENT_FUNC=start_server

${ERL} -pa ${CLIENT_DIR} \
	-s ${CLIENT_MODULE} ${CLIENT_FUNC} false \
	-noshell
