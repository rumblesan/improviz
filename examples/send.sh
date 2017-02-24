#! /bin/bash

FILENAME=$1

curl --data "@${FILENAME}" http://localhost:3000/read

