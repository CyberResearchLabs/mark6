#!/bin/bash

ERL=/opt/local/bin/erl
<<<<<<< HEAD
ROOT=/opt/src/mark6/src/tstmark6
=======
ROOT=/home/dlapsley/mark6/src/tstmark6/
>>>>>>> e1ec64936f9fc146992fe6fa18c35126406e0a72

CLIENT_DIR=${ROOT}/ebin
CLIENT_MODULE=tstmark6
CLIENT_FUNC=start_server


${ERL} -pa ${CLIENT_DIR} \
	-s ${CLIENT_MODULE} ${CLIENT_FUNC} false \
	-noshell
