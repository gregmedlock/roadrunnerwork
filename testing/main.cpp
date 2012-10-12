#include <fstream>
#include "rrLogger.h"
#include "rrUtils.h"
#include "UnitTest++.h"
#include "XmlTestReporter.h"
#include "TestReporterStdOut.h"

using namespace std;
using namespace rr;
main()
{
//	fstream aFile;
//    aFile.open("tests.xml", ios::out);
//    if(!aFile)
//    {
//    	return -1;
//    }

//	UnitTest::XmlTestReporter reporter(aFile);

	LogOutput::mLogToConsole = true;
    gLog.SetCutOffLogLevel(lDebug);
	UnitTest::TestReporterStdout reporter;//(aFile);
	UnitTest::TestRunner runner(reporter);

    runner.RunTestsIf(UnitTest::Test::GetTestList(), NULL, UnitTest::True(),0);
    Pause();
	return 0;
}

