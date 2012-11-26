#!/bin/bash
#pkg=cpputils-cmake-`date +%Y%m%d.%H%M`
pkg=cpputils-cmake-0.0.1
mkdir $pkg
cp *.el $pkg
if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/

