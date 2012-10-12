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
	fstream aFile;
    aFile.open("tests.xml", ios::out);
    if(!aFile)
    {
    	return -1;
    }

	UnitTest::XmlTestReporter reporter(aFile);

	LogOutput::mLogToConsole = false;
    gLog.SetCutOffLogLevel(lDebug);
//	UnitTest::TestReporterStdout reporter;//(aFile);
	UnitTest::TestRunner runner(reporter);

    runner.RunTestsIf(UnitTest::Test::GetTestList(), NULL, UnitTest::True(),0);
//    Pause();
	return 0;
}

#if defined(CG_IDE)
#pragma comment(lib, "rr_c_api.lib")
#pragma comment(lib, "roadrunner.lib")
#pragma comment(lib, "sundials_cvode.lib")
#pragma comment(lib, "sundials_nvecserial.lib")
#pragma comment(lib, "nleq-static.lib")
#pragma comment(lib, "rr-libstruct-static.lib")
#pragma comment(lib, "libsbml-static.lib")
#pragma comment(lib, "libxml2_xe.lib")
#pragma comment(lib, "blas.lib")
#pragma comment(lib, "lapack.lib")
#pragma comment(lib, "libf2c.lib")
#pragma comment(lib, "unit_test.lib")
#endif

