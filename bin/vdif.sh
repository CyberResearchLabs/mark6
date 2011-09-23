#!/bin/bash

# ERL=/opt/local/bin/erl
ERL=/usr/bin/erl
ROOT=/opt/mit/mark6
VERSION=0.1

CLIENT_DIR=${ROOT}/lib/vdif-${VERSION}/ebin
CLIENT_MODULE=vdif
CLIENT_FUNC=start_server

${ERL} -pa ${CLIENT_DIR} \
	-s ${CLIENT_MODULE} ${CLIENT_FUNC} /mnt/disk0/cap.m6 \
	-noshell
