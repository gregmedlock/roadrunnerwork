#ifndef rrGetOptionsH
#define rrGetOptionsH
//---------------------------------------------------------------------------
namespace rr
{
	extern int 			optind, opterr;
	extern char 	   *optarg;
	int 			   	GetOptions(int argc, char *argv[], char *optstring);
}

#endif
