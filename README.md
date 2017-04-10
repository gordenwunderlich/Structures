# Structures
Fortran 2008 Structures Library
This is a library for different structures containing arbitrary data types using fortran 2003 class(*) type and fortran 2008 submodule features.
Currently this Project only contains a simple linked list with basic functionality and a test program which more or less explains the use of this lib.

This project is designed to be easily extendable with custom types for the list. Just add a use statement in Structures.f90 and add a type select similar to that of the list itself to Type_selects.fi .
You have to compile with /fpp for the #include statement to work

tested compilers:
    gfortran 5.4 on windows using cygwin does not support submodules.
    ifort 17.0 on windows works
