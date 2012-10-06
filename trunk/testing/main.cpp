#include "UnitTest++.h"
#include "XmlTestReporter.h"
#include "TestReporterStdOut.h"
#include <fstream>
using namespace std;
main()
{
//	fstream aFile;
//    aFile.open("tests.xml", ios::out);
//    if(!aFile)
//    {
//    	return -1;
//    }

//	UnitTest::XmlTestReporter reporter(aFile);

	UnitTest::TestReporterStdout reporter;//(aFile);
	UnitTest::TestRunner runner(reporter);

	return runner.RunTestsIf(UnitTest::Test::GetTestList(), NULL, UnitTest::True(),0);
}

