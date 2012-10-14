#include <fstream>
#include "rrLogger.h"
#include "rrUtils.h"
#include "UnitTest++.h"
#include "XmlTestReporter.h"
#include "TestReporterStdOut.h"

using namespace std;
using namespace rr;
int main(int argc, char* argv[])
{
	string resultOutputPath;
    string reportFile("c_api_tests.xml");

	if(argc > 1)
    {
		char* path = argv[1];
        resultOutputPath = argv[1];
        reportFile = JoinPath(resultOutputPath, reportFile);
    }

	fstream aFile;
    aFile.open(reportFile.c_str(), ios::out);
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
#pragma comment(lib, "unit_test-static.lib")
#endif

