#!/bin/bash
# 
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORIES HERE %%%%%%%%
# Make sure that the mkmf file is in the same directory of the source codes
cd \path\to\source\codes
rm makefile
# 
# Create the makefile
#
mkmf -a ./ -m makefile -p nigo -t /path/to/directory/template.txt
#
# Compile the source files
#
make
