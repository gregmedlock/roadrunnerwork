%module RoadRunner
%{
#define SWIG_FILE_WITH_INIT
    #include "rr_c_api.h"
%}

char* getCopyright();
