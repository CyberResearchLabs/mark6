#!/bin/bash

rm bin/*
rm lib/*
rm include/*

cd src/dutils
make clean
cd ../vsocket
make clean
cd ../app
make clean
