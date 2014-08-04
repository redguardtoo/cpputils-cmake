#!/bin/bash
#pkg=cpputils-cmake-`date +%Y%m%d.%H%M`
pkg=cpputils-cmake-0.4.13
mkdir $pkg
cp README.org $pkg
cp *.el $pkg
if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/

