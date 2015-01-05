#!/bin/bash
#
# Install SLATEC library
#
cd Slatec_library
gfortran -o f90split f90split.f90
mkdir temp
cd temp
rm *
#
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORY HERE %%%%%%%%
#
/path/to/directory/NIGO/Slatec_library/f90split ../slatec.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f90
#
ar qc libslatec.a *.o
rm *.o
#
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORY HERE %%%%%%%%
#
mv libslatec.a /path/to/directory/NIGO/Slatec_library
cd ..
rmdir temp
#
echo "Library installed as libslatec.a"
#
# Install ODE library
#
cd ODE_library
gfortran -o f90split f90split.f90
#
mkdir temp
cd temp
rm *
#
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORY HERE %%%%%%%%
#
/path/to/directory/NIGO/ODE_library/f90split ../ode.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f90
#
ar qc libode.a *.o
rm *.o
#
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORY HERE %%%%%%%%
#
mv libode.a /path/to/directory/NIGO/ODE_library
cd ..
rmdir temp
#
echo "Library installed as libode.a"
#
# Install ASA147 library
#
cd ASA147_library
gfortran -o f90split f90split.f90
#
mkdir temp
cd temp
rm *
#
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORY HERE %%%%%%%%
#
/path/to/directory/NIGO/ASA147_library/f90split ../ode.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -g $FILE >& compiler.txt
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
  rm compiler.txt
done
rm *.f90
#
ar qc libasa147.a *.o
rm *.o
#
#
#%%%%%%%%%% USER: EDIT THE PATH TO YOUR WORKING DIRECTORY HERE %%%%%%%%
#
mv libasa147.a /path/to/directory/NIGO/ASA147_library
cd ..
rmdir temp
#
echo "Library installed as libasa147.a"
#