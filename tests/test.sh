#!/bin/sh
echo "Make sure all build directories in this project are deleted"
emacs -batch -l cl-lib -l ert -l ../cpputils-cmake.el  -l main.el -f ert-run-tests-batch-and-exit
