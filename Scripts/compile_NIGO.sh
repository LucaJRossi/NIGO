#!/bin/bash
# 
rm makefile
# 
# Create the makefile
#
mkmf -a ./ -m makefile -p nigo -t /path/to/directory/template.txt
#
# Compile the source files
#
make
