#! /bin/bash

FILENAME=$1

curl --data-binary "@${FILENAME}" http://localhost:3000/read

