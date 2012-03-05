#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include "rrToken.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------


namespace rr
{

Token::Token(const CodeTypes& code)
:
tokenCode(code),
tokenDouble(0),
tokenInteger(0),
tokenString(""),
tokenValue(0) // Used to retrieve int or double
{}

}
