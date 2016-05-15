#!/bin/sh
emacs -batch -l cl-lib -l ert -l ../cpputils-cmake.el  -l main.el -f ert-run-tests-batch-and-exit
