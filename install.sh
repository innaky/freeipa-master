#!/bin/bash

sbcl --eval '(ql:quickload "freeipa-master")' \
     --eval '(freeipa-master:main)' \
     --eval '(quit)'
