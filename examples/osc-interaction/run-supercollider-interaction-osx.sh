#!/bin/bash

SUPERCOLLIDER=${SCEXE:-"/Applications/SuperCollider.app/Contents/MacOS/sclang"}

STARTUP_FILE="./supercollider-osc.scd"

eval "${SUPERCOLLIDER} ${STARTUP_FILE}"
