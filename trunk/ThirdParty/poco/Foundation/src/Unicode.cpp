//
// Unicode.cpp
//
// $Id: //poco/1.4/Foundation/src/Unicode.cpp#1 $
//
// Library: Foundation
// Package: Text
// Module:  Unicode
//
// Copyright (c) 2007, Applied Informatics Software Engineering GmbH.
// and Contributors.
//
// Permission is hereby granted, free of charge, to any person or organization
// obtaining a copy of the software and accompanying documentation covered by
// this license (the "Software") to use, reproduce, display, distribute,
// execute, and transmit the Software, and to prepare derivative works of the
// Software, and to permit third-parties to whom the Software is furnished to
// do so, all subject to the following:
// 
// The copyright notices in the Software and this entire statement, including
// the above license grant, this restriction and the following disclaimer,
// must be included in all copies of the Software, in whole or in part, and
// all derivative works of the Software, unless such copies or derivative
// works are solely in the form of machine-executable object code generated by
// a source language processor.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT
// SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE
// FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//


#include "Poco/Unicode.h"


extern "C"
{
#include "pcre_config.h"
#include "pcre_internal.h"
}


namespace Poco {


void Unicode::properties(int ch, CharacterProperties& props)
{
	const ucd_record* ucd = GET_UCD(ch);
	props.category = static_cast<CharacterCategory>(_pcre_ucp_gentype[ucd->chartype]);
	props.type     = static_cast<CharacterType>(ucd->chartype);
	props.script   = static_cast<Script>(ucd->script);
}


int Unicode::toLower(int ch)
{
	if (isUpper(ch))
		return static_cast<int>(UCD_OTHERCASE(static_cast<unsigned>(ch)));
	else
		return ch;
}


int Unicode::toUpper(int ch)
{
	if (isLower(ch))
		return static_cast<int>(UCD_OTHERCASE(static_cast<unsigned>(ch)));
	else
		return ch;
}


} // namespace Poco
