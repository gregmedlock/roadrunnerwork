#include <stdio.h>
#pragma hdrstop
#include "rrC_API.h"

#pragma argsused
int main()
{
    RRHandle handle;
    char* text;
	printf("This is c \n");


    handle = getRRInstance();
    text = getCopyright();
    printf("In C: %s", text);

    deleteRRInstance(handle);
	return 0;
}

