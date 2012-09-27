//
// TestApp.cpp
//
// $Id: //poco/1.4/Foundation/testsuite/src/TestApp.cpp#3 $
//
// Copyright (c) 2005-2006, Applied Informatics Software Engineering GmbH.
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


#if defined(_WIN32)
#define _CRT_SECURE_NO_WARNINGS
#endif


#include <string>
#include <iostream>
#include <cstdlib>


int main(int argc, char** argv)
{
	if (argc > 1)
	{
		std::string arg(argv[1]);
		if (arg == "-hello")
		{
			std::cout << "Hello, world!";
		}
		else if (arg == "-count")
		{
			int n = 0;
			int c = std::cin.get();
			while (c != -1) { ++n; c = std::cin.get(); }
			return n;
		}
		else if (arg == "-env")
		{
			const char* s = std::getenv("TESTENV");
			if (s)
			{
				std::cout << s;
				return 0;
			}
			else return 1;
		}
	}
	return argc - 1;
}