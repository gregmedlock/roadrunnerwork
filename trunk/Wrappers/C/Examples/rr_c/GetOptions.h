#ifndef rrGetOptionsH
#define rrGetOptionsH                                                                                  \
#include "rrExporter.h"
//---------------------------------------------------------------------------

namespace rr
{
extern int                         optind;
extern  int                         opterr;
extern  char                       *optarg;
        int                         GetOptions(int argc, char *argv[], char *optstring);
}

#endif
