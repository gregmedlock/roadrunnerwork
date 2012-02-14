#ifdef MTK_PCH
#include "mtk_pch.h"
#endif
#pragma hdrstop
#include <algorithm>
#include <windows.h>
#include "mtkLoggerUtils.h"
#ifdef __CODEGEARC__
#pragma package(smart_init)
#endif

//---------------------------------------------------------------------------
string GetLogTime(bool show_milli_sec)
{
    const int MAX_LEN = 200;
    char buffer[MAX_LEN];
    if (GetTimeFormatA(LOCALE_USER_DEFAULT, 0, 0, "HH':'mm':'ss", buffer, MAX_LEN) == 0)
	{
        return "Error in mtk::GetTime()";
    }

    char result[100] = {0};
    if(show_milli_sec)
    {
	    static DWORD first = GetTickCount();
    	std::sprintf(result, "%s.%03ld", buffer, (long)(GetTickCount() - first) % 1000);
	    return string(result);
    	}
    else
    {
    	return string(buffer);
    }
}
