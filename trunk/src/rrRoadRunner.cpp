#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include <complex>
#include "rrRoadRunner.h"
#include "rrException.h"
#include "rrModelGenerator.h"
#include "rrCompiler.h"
#include "rrStreamWriter.h"
#include "rrLogger.h"
#include "rrCSharpGenerator.h"
#include "rrCGenerator.h"
#include "rrStringUtils.h"
#include "rrModelFromC.h"
//---------------------------------------------------------------------------
#if defined(__CODEGEARC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------

using namespace std;
namespace rr
{

//Initialize statics..
bool RoadRunner::mComputeAndAssignConservationLaws = true;
bool RoadRunner::mConservedTotalChanged 			= false;
bool RoadRunner::mReMultiplyCompartments 			= false;

RoadRunner::RoadRunner(bool generateCSharp)
:
emptyModelStr("A model needs to be loaded before one can use this method"),
STEADYSTATE_THRESHOLD(1.E-2),
mCVode(NULL),
mCompiler(NULL),
mL(NULL),
mL0(NULL),
mN(NULL),
mNr(NULL),
DiffStepSize(0.05),
timeStart(0),
timeEnd(10),
numPoints(21),
sbmlStr(""),
mModel(NULL),
mModelDllHandle(NULL)
{
	Log(lDebug4)<<"In RoadRunner CTOR";
	mCSharpGenerator = new CSharpGenerator();
	mCGenerator = new CGenerator();
    mModelGenerator = generateCSharp == true ? mCSharpGenerator : mCGenerator;
}

RoadRunner::~RoadRunner()
{
	Log(lDebug4)<<"In RoadRunner DTOR";
 	delete mCSharpGenerator;
	delete mCGenerator;
    delete mModel;
    delete mCVode;
    delete mCompiler;
}

ModelGenerator*	RoadRunner::GetCodeGenerator()
{
	return mModelGenerator;
}

void RoadRunner::Reset()
{
	if(mModelGenerator)
    {
		mModelGenerator->Reset();
	}
}

string RoadRunner::GetModelSourceCode()
{
	return mModelCode;
}

bool RoadRunner::InitializeModel()
{
    if(!mModel)
    {
    	Log(lError)<<"Trying to initialize a NULL model";
    	return false ;
    }

    //model.Warnings.AddRange(ModelGenerator.Instance.Warnings);

    modelLoaded = true;
    mConservedTotalChanged = false;

    mModel->setCompartmentVolumes();
    mModel->initializeInitialConditions();
    mModel->setParameterValues();
    mModel->setCompartmentVolumes();
    mModel->setBoundaryConditions();
    mModel->setInitialConditions();
    mModel->convertToAmounts();
    mModel->evalInitialAssignments();
    mModel->computeRules(mModel->y);
    mModel->convertToAmounts();

    if (mComputeAndAssignConservationLaws)
    {
    	mModel->computeConservedTotals();
    }

    mCVode = new CvodeInterface(mModel);

    reset();

    // Construct default selection list
    selectionList.resize(mModel->getNumTotalVariables() + 1); // + 1 to include time
    selectionList[0].selectionType = clTime;
    for (int i = 0; i < mModel->getNumTotalVariables(); i++)
    {
        selectionList[i + 1].index = i;
        selectionList[i + 1].selectionType = clFloatingSpecies;
    }
    return true;
}


//bool RoadRunner::IsNleqAvailable()
//{
//    return NLEQInterface.IsAvailable;
//}

//        void RoadRunner::Test()
//        {
//
//            double[,] results;
//            RoadRunner sim;
//            sim = new RoadRunner();
//            //RoadRunner.ReMultiplyCompartments(false);
//            //RoadRunner.ComputeAndAssignConservationLaws(false);
//            sim.setTolerances(1E-4, 1E-4, 100);
//            //sim.loadSBMLFromFile(@"C:\Development\sbwBuild\source\Translators\TestModels\MathMLTests.xml");
//            //sim.loadSBMLFromFile(@"C:\Development\trunk-sbml\trunk\test-suite\cases\semantic\00938\00938-sbml-l3v1.xml");
//            //sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00978\00978-sbml-l3v1.xml");
//            string test = "01104";
//            sim.loadSBMLFromFile(string.Format(@"C:\Development\test-suite\cases\semantic\{0}\{0}-sbml-l3v1.xml", test));
//
//            //sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00951\00951-sbml-l3v1.xml");
//            //sim.setSelectionList(new ArrayList(new string[] {
//            //    "time", "x", "y", "p", "q"
//            //    }));
//            //results = sim.simulateEx(0, 2, 11);
//            ////var writer = new StringWriter();
//
//            //sim.loadSBMLFromFile(@"C:\Users\fbergmann\Desktop\max.xml");
//            sim.setSelectionList(new ArrayList(new string[] {
//                "time", "Xref"
//            }));
//            results = sim.simulateEx(0, 10, 11);
//
//            DumpResults(Console.Out, results, sim.getSelectionList());
//
//
//
//            //double[,] results;
//            //RoadRunner sim;
//            //sim = new RoadRunner();
//            ////sim.loadSBMLFromFile(@"C:\Development\sbwBuild\source\Translators\TestModels\MathMLTests.xml");
//            ////sim.loadSBMLFromFile(@"C:\Development\trunk-sbml\trunk\test-suite\cases\semantic\00938\00938-sbml-l3v1.xml");
//            //sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00952\00952-sbml-l3v1.xml");
//            ////sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00951\00951-sbml-l3v1.xml");
//            //sim.setSelectionList(new ArrayList(new string[] {
//            //    "time", "S", "Q", "R", "reset"
//            //    }));
//            //results = sim.simulateEx(0, 1, 11);
//            ////var writer = new StringWriter();
//            //DumpResults(Console.Out, results, sim.getSelectionList());
//
//            //sim = new RoadRunner();
//            //sim.setTolerances(1e-10, 1e-9);
//            //sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00374\00374-sbml-l2v4.xml");
//            //sim.setSelectionList(new ArrayList(new string[] {
//            //    "time", "S1", "S2", "S3", "S4"
//            //    }));
//            //results = sim.simulateEx(0, 2, 51);
//            //DumpResults(Console.Out, results,sim.getSelectionList());
//
//            //sim = new RoadRunner();
//            //mComputeAndAssignConservationLaws = false;
//            //sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00424\00424-sbml-l3v1.xml");
//            //sim.setSelectionList(new ArrayList(new string[] {
//            //    "time", "S1", "S2", "S3"
//            //    }));
//
//            ////sim.CorrectMaxStep();
//            //CvodeInterface.MaxStep = 0.0001;
//            //sim.mCVode.reStart(0.0, sim.model);
//
//            ////sim.mCVode.
//            //results = sim.simulateEx(0, 5, 51);
//            //DumpResults(Console.Out, results, sim.getSelectionList());
//
//            //Debug.WriteLine(writer.GetStringBuilder().ToString());
//            //Debug.WriteLine(sim.getWarnings());
//            //Debug.WriteLine(sim.getCapabilities());
//        }
//#endif
//
//         void RoadRunner::emptyModel()
//        {
//            throw new SBWApplicationException(emptyModelStr);
//        }
//
//
double RoadRunner::GetValueForRecord(const TSelectionRecord& record)
{
    double dResult;
    switch (record.selectionType)
    {
        case TSelectionType::clFloatingSpecies:
            dResult = mModel->getConcentration(record.index);
            break;
        case TSelectionType::clBoundarySpecies:
            dResult = mModel->bc[record.index];
            break;
        case TSelectionType::clFlux:
            dResult = mModel->rates[record.index];
            break;
        case TSelectionType::clRateOfChange:
            dResult = mModel->dydt[record.index];
            break;
        case TSelectionType::clVolume:
            dResult = mModel->c[record.index];
            break;
        case TSelectionType::clParameter:
            {
                if (record.index > mModel->gp.size() - 1)
                    dResult = mModel->ct[record.index - mModel->gp.size()];
                else
                    dResult = mModel->gp[record.index];
            }
            break;
        case TSelectionType::clFloatingAmount:
            dResult = mModel->amounts[record.index];
            break;
        case TSelectionType::clBoundaryAmount:
            int nIndex;
//            if (
//                mModelGenerator->compartmentList.find(
//                    mModelGenerator->boundarySpeciesList[record.index].compartmentName,
//                    out nIndex)) //Todo: enable this
                dResult = mModel->bc[record.index] * mModel->c[nIndex];
//            else
                dResult = 0.0;
            break;
        case TSelectionType::clElasticity:
            dResult = getEE(record.p1, record.p2, false);
            break;
        case TSelectionType::clUnscaledElasticity:
            dResult = getuEE(record.p1, record.p2, false);
            break;
        case TSelectionType::clEigenValue:	//Todo: Enable this..
//            vector< complex<double> >oComplex = LA.GetEigenValues(getReducedJacobian());
//            if (oComplex.Length > record.index)
//            {
//                dResult = oComplex[record.index].Real;
//            }
//            else
//                dResult = Double.NaN;
            break;
        case TSelectionType::clStoichiometry:
            dResult = mModel->sr[record.index];
            break;
        default:
            dResult = 0.0;
            break;
    }
    return dResult;
}

double RoadRunner::GetNthSelectedOutput(const int& index, const double& dCurrentTime)
{
    TSelectionRecord record = selectionList[index];
    if (record.selectionType == TSelectionType::clTime)
    {
        return dCurrentTime;
    }

    return GetValueForRecord(record);
}

void RoadRunner::AddNthOutputToResult(vector< vector<double> >& results, int nRow, double dCurrentTime)
{
    for (int j = 0; j < selectionList.size(); j++)
    {
        results[nRow][j] = GetNthSelectedOutput(j, dCurrentTime);
    }
}

vector<double> RoadRunner::BuildModelEvalArgument()
{
    vector<double> dResult;// = new double[model.amounts.Length + model.rateRules.Length];
    vector<double> dCurrentRuleValues = mModel->GetCurrentValues();
    dResult = dCurrentRuleValues;//.CopyTo(dResult, 0);
    mModel->amounts = dResult;//.CopyTo(dResult, model.rateRules.Length);
    return dResult;
}

vector< vector<double> > RoadRunner::runSimulation()
{
    double hstep = (timeEnd - timeStart) / (numPoints - 1);
    vector< vector<double> >  results;// = new double[numPoints, selectionList.Length];
    results.resize(selectionList.size());

    if(mModel)
    {
    	return results;
    }

    vector<double> y;
    y=BuildModelEvalArgument();
    mModel->evalModel(timeStart, y);

    AddNthOutputToResult(results, 0, timeStart);

    if (mCVode->HaveVariables())
    {
        int restartResult = mCVode->reStart(timeStart, mModel);
        if (restartResult != 0)
            throw new SBWApplicationException("Error in reStart call to CVODE");
    }
    double tout = timeStart;
    for (int i = 1; i < numPoints; i++)
    {
        mCVode->OneStep(tout, hstep);
        tout = timeStart + i * hstep;
        AddNthOutputToResult(results, i, tout);
    }
    return results;
}

void RoadRunner::DumpResults(TextWriter& writer, vector< vector<double> >& data, const StringList& colLabels)
{
    for (int i = 0; i < colLabels.Count(); i++)
    {
        writer.Write(colLabels[i] + "\t");
        Log(lDebug)<<colLabels[i]<<"\t";
    }
    writer.WriteLine();
    Log(lDebug)<<endl;

    int size =  data.size();
    for (int i = 0; i < data.size(); i++)
    {
        for (int j = 0; j < data[0].size(); j++)
        {
        	string val = ToString(data[i][j]);
            writer.Write(val + "\t");
            Log(lDebug)<<val << "\t";
        }
        writer.WriteLine();
	    Log(lDebug)<<endl;
    }
}

// -------------------------------------------------------------------------------
//
//        void RoadRunner::PrintTout(double start, double end, int numPoints)
//        {
//            double hstep = (end - start) / (numPoints - 1);
//            Debug.WriteLine("Using step " + hstep);
//            double tout = start;
//            for (int i = 1; i < numPoints; i++)
//            {
//                tout = start + i*hstep;
//                Debug.WriteLine(tout.ToString("G17"));
//            }
//        }

//        //void RoadRunner::TestDirectory(string directory, bool testSubDirs)
//        //{
//        //    //TestDirectory(directory, testSubDirs, "*sbml-l3v1.xml");
//        //    TestDirectory(directory, testSubDirs, "*.xml");
//        //}
//
//    //    void RoadRunner::TestDirectory(string directory, bool testSubDirs, string pattern)
//    //{
//    //        var files = Directory.GetFiles(directory, pattern,
//    //            (testSubDirs ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly));
//    //        foreach (var item in files)
//    //        {
//    //            try
//    //            {
//    //                var rr = new RoadRunner();
//    //                rr.setTolerances(1e-6, 1e-3);
//    //                rr.loadSBMLFromFile(item);
//    //                rr.simulateEx(0, 10, 1000);
//    //                Debug.WriteLine(string.Format("File: {0} passed", Path.GetFileName(item)));
//    //            }
//    //            catch (Exception ex)
//    //            {
//    //                Debug.WriteLine(string.Format("File: {0} failed: ", Path.GetFileName(item), ex.Message));
//    //            }
//    //        }
//    //    }


void RoadRunner::SimulateSBMLFile(const string& fileName, const bool& useConservationLaws)
{
    //var sim = new RoadRunner();
    ComputeAndAssignConservationLaws(useConservationLaws);

    string fullFilePath(fileName);
    ifstream inFileStream(fullFilePath.c_str());
    if(!inFileStream)
    {
        throw(Exception("Failed to open the model file:" + fullFilePath));
    }
    Log(lInfo)<<"\n\n ===== Reading model file:"<<fullFilePath<<" ==============";
    std::string sbml((std::istreambuf_iterator<char>(inFileStream)), std::istreambuf_iterator<char>());
    inFileStream.close();
    Log(lDebug5)<<"Before loading SBML. SBML model code size: "<<sbml.size();
    mModelXMLFileName = fullFilePath;

    loadSBML(sbml);

    vector< vector<double> > data;
    data = simulate();
    StringList list = getSelectionList();
    TextWriter writer(cout);

    DumpResults(writer, data, list);
    return;
}

void RoadRunner::SimulateSBMLFile(const string& fileName, const bool& useConservationLaws, const double& startTime, const double& endTime, const int& numPoints)
{
//    var sim = new RoadRunner();
//    ComputeAndAssignConservationLaws(useConservationLaws);
//    sim.loadSBML(File.ReadAllText(fileName));
//
//    try
//    {
//        double[,] data = sim.simulateEx(startTime, endTime, numPoints);
//        ArrayList list = sim.getSelectionList();
//        TextWriter writer = Console.Error;
//
//        DumpResults(writer, data, list);
//    }
//    catch (Exception ex)
//    {
//        Debug.WriteLine(ex);
//    }
//
//    //Debug.WriteLine(sim.getCapabilities());
//
//    return;
}

//        Help("Load SBML into simulator")
void RoadRunner::loadSBMLFromFile(const string& fileName)
{
    ifstream ifs(fileName.c_str());
    if(!ifs)
    {
		stringstream msg;
        msg<<"Failed opening file: "<<fileName;
		throw SBWApplicationException(msg.str());
    }

    std::string sbml((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
    cout<<sbml<<endl;
    mModelXMLFileName = fileName;
    loadSBML(sbml);
}

//        Help("Load SBML into simulator")
bool RoadRunner::loadSBML(const string& sbml)
{
	Log(lDebug4)<<"Loading SBML into simulator";
    if (!sbml.size())
    {
        throw RRException("No SBML  content..!");
    }

    // If the user loads the same model again, don't both loading into NOM,
    // just reset the initial conditions
    if (modelLoaded && mModel != NULL && (sbml == sbmlStr) && (sbml != ""))
    {
        InitializeModel();
        //reset();
    }
    else
    {
        if(mModel != NULL)
        {
            delete mCVode;
            mCVode = NULL;
            delete mModel;
            mModel = NULL;
            modelLoaded = false;
        }

        sbmlStr = sbml;

		mModelCode = mModelGenerator->generateModelCode(sbmlStr);

        if(!mModelCode.size())
        {
            throw RRException("Failed to generate Model Code");
        }
        Log(lDebug3)<<" ------ Model Code --------\n"
        			<<mModelCode
                    <<" ----- End of Model Code -----\n";

 		if(!mCompiler)
        {
        	mCompiler  = new Compiler;
        }

        //The get instance function actually compiles the supplied code..
//        rrObject* o = mCompiler->getInstance(_sModelCode, "TModel", sLocation);
		CGenerator *codeGen = dynamic_cast<CGenerator*>(mModelGenerator);
        codeGen->SetXMLModelFileName(mModelXMLFileName);
        string srcCodeFolder("C:\\rrw\\Testing\\rr_code_output\\c_from_rr++");
		codeGen->SaveSourceCodeToFolder(srcCodeFolder);

        mModelDllHandle = mCompiler->CompileC_DLL(codeGen->GetSourceCodeFileName());

        if (mModelDllHandle != NULL)
        {
        	//Create a model
            ModelFromC *rrCModel = new ModelFromC(codeGen, mModelDllHandle);
            mModel = rrCModel;			//Should use an auto pointer?
            if(!mModel)
            {
            	Log(lError)<<"Failed to create ModelFromC";
              	return false;
            }


            if(!InitializeModel())
            {
            	Log(lError)<<"Failed Initializing C Model";
                return false;
            }


        }
        else
        {
            mModel 		= NULL;
            modelLoaded = false;
          	string filePath = "SBW_ErrorLog.txt";
            try
            {

                StreamWriter sw(filePath);// Environment.GetEnvironmentVariable("TEMP") + "/SBW_ErrorLog.txt");
                try
                {
                    sw.WriteLine("ErrorMessage: ");
                    //sw.WriteLine(mCompiler->getLastErrors());
                    sw.WriteLine("C# Model Code: ");
                    sw.Write(mModelCode);
                    sw.Close();
                }
                catch(...)
                {
					throw SBWApplicationException("Failed to write to file");
                }
            }
            catch (RRException)
            {
            }
           	throw SBWApplicationException("Internal Error: The model has failed to compile." + NL
                                          + "The model file has been deposited at " +
                                          filePath);
        }

//        _L = mStructAnalysis.GetLinkMatrix();
//        _L0 = StructAnalysis.GetL0Matrix();
//        _N = StructAnalysis.GetReorderedStoichiometryMatrix();
//        _Nr = StructAnalysis.GetNrMatrix();
    }
    return true;
}

//        Help("Returns the initially loaded model as SBML")
//        string RoadRunner::getSBML()
//        {
//            return sbmlStr;
//        }
//
//        Help("get the currently set time start")
//        double RoadRunner::getTimeStart()
//        {
//            return timeStart;
//        }
//
//        Help("get the currently set time end")
//        double RoadRunner::getTimeEnd()
//        {
//            return timeEnd;
//        }
//
//        Help("get the currently set number of points")
//        int RoadRunner::getNumPoints()
//        {
//            return numPoints;
//        }
//
//        Help("Set the time start for the simulation")
//        void RoadRunner::setTimeStart(double startTime)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (startTime < 0)
//                throw new SBWApplicationException("Time Start most be greater than zero");
//            this.timeStart = startTime;
//        }
//
//        Help("Set the time end for the simulation")
//        void RoadRunner::setTimeEnd(double endTime)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (endTime <= 0)
//                throw new SBWApplicationException("Time End most be greater than zero");
//            this.timeEnd = endTime;
//        }
//
//        Help("Set the number of points to generate during the simulation")
//        void RoadRunner::setNumPoints(int nummberOfPoints)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (nummberOfPoints <= 0)
//                nummberOfPoints = 1;
//            this.numPoints = nummberOfPoints;
//        }
//
//        Help("reset the simulator back to the initial conditions specified in the SBML model")
void RoadRunner::reset()
{
//            if (!modelLoaded)
//            {
//                // rather make sure that the simulator is!!!! in a stable state
//                model = null;
//                sbmlStr = "";
//            }
//            else
//            {
//                model.time = 0.0;
//
//                // Reset the event flags
//                model.resetEvents();
//
//                model.setCompartmentVolumes();
//
//                model.setInitialConditions();
//
//                model.convertToAmounts();
//
//                // in case we have ODE rules we should assign those as initial values
//                model.InitializeRateRuleSymbols();
//                model.InitializeRates();
//                // and of course initial assignments should override anything
//                model.evalInitialAssignments();
//                model.convertToAmounts();
//                // also we might need to set some initial assignment rules.
//                model.convertToConcentrations();
//                model.computeRules(model.y);
//                model.InitializeRates();
//                model.InitializeRateRuleSymbols();
//                model.evalInitialAssignments();
//                model.computeRules(model.y);
//
//                model.convertToAmounts();
//
//                if (mComputeAndAssignConservationLaws && !_bConservedTotalChanged) model.computeConservedTotals();
//
//                mCVode.AssignNewVector(model, true);
//                mCVode.TestRootsAtInitialTime();
//
//                //double hstep = (timeEnd - timeStart) / (numPoints - 1);
//                //CvodeInterface.MaxStep = Math.Min(CvodeInterface.MaxStep, hstep);
//                //if (CvodeInterface.MaxStep == 0)
//                //    CvodeInterface.MaxStep = hstep;
//
//
//                model.time = 0.0;
//                mCVode.reStart(0.0, model);
//
//                mCVode.assignments.Clear();
//
//                try
//                {
//                    model.testConstraints();
//                }
//                catch (Exception e)
//                {
//                    model.Warnings.Add("Constraint Violated at time = 0\n" + e.Message);
//                }
//            }
}
//
//        Help(
//            "Change the initial conditions to another concentration vector (changes only initial conditions for floating Species)"
//            )
//        void RoadRunner::changeInitialConditions(double[] ic)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            for (int i = 0; i < ic.Length; i++)
//            {
//                model.setConcentration(i, ic[i]);
//                if (model.init_y.Length > i)
//                model.init_y[i] = ic[i];
//            }
//            model.convertToAmounts();
//            model.computeConservedTotals();
//        }
//
//
//        Help("Carry out a time course simulation")
vector< vector<double> > RoadRunner::simulate()
{
    try
    {
        if (!modelLoaded)
        {
            throw new SBWApplicationException(emptyModelStr);
        }
        if (timeEnd <= timeStart)
        {
            throw new SBWApplicationException("Error: time end must be greater than time start");
        }
        return runSimulation();
    }
    catch (Exception e)
    {
        throw new SBWApplicationException("Unexpected error from simulate(): " + e.Message);
    }
}

//        Help(
//            "Extension method to simulate (time start, time end, number of points). This routine resets the model to its initial condition before running the simulation (unlike simulate())"
//            )
//        double[,] RoadRunner::simulateEx(double startTime, double endTime, int numberOfPoints)
//        {
//            try
//            {
//                if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//                reset(); // reset back to initial conditions
//
//                if (endTime < 0 || startTime < 0 || numberOfPoints <= 0 || endTime <= startTime)
//                    throw new SBWApplicationException("Illegal input to simulateEx");
//
//                this.timeEnd = endTime;
//                this.timeStart = startTime;
//                numPoints = numberOfPoints;
//                return runSimulation();
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from simulateEx()", e.Message);
//            }
//        }
//
//        Help("Returns the current vector of reactions rates")
//        double[] RoadRunner::getReactionRates()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            model.convertToConcentrations();
//            model.computeReactionRates(0.0, model.y);
//            return model.rates;
//        }
//
//        Help("Returns the current vector of rates of change")
//        double[] RoadRunner::getRatesOfChange()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            model.computeAllRatesOfChange();
//            return model.dydt;
//        }
//
//        Help(
//            "Returns a list of floating species names: This method is deprecated, please use getFloatingSpeciesNames()")
//        
//        ArrayList RoadRunner::getSpeciesNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            return mModelGenerator->getFloatingSpeciesConcentrationList(); // Reordered list
//        }
//
//        Help("Returns a list of reaction names")
//        ArrayList RoadRunner::getReactionNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            return mModelGenerator->getReactionNames();
//        }
//
//
//        // ---------------------------------------------------------------------
//        // Start of Level 2 API Methods
//        // ---------------------------------------------------------------------
//
//        int RoadRunner::UseKinsol { get; set; }
//
//        Help("Get Simulator Capabilities")
//        string RoadRunner::getCapabilities()
//        {
//            CapsSupport current = CapsSupport.CurrentSettings;
//            current["integration"].Capabilities.Add(new CapsSupport.Capability
//            {
//                Name = "usekinsol", IntValue = UseKinsol, Hint = "Is KinSol used as steady state integrator", Type = "int"
//            }
//                );
//
//            return current.ToXml();
//        }
//
//        void RoadRunner::setTolerances(double aTol, double rTol)
//        {
//            CvodeInterface.relTol = rTol;
//            CvodeInterface.absTol = aTol;
//        }
//
//        void RoadRunner::setTolerances(double aTol, double rTol, int maxSteps)
//        {
//            setTolerances(aTol, rTol);
//            CvodeInterface.MaxNumSteps = maxSteps;
//        }
//
//        void RoadRunner::CorrectMaxStep()
//        {
//            double maxStep = (timeEnd - timeStart) / (numPoints);
//            maxStep = Math.Min(CvodeInterface.MaxStep, maxStep);
//            CvodeInterface.MaxStep = maxStep;
//        }
//
//        Help("Set Simulator Capabilites")
//        void RoadRunner::setCapabilities(string capsStr)
//        {
//            var cs = new CapsSupport(capsStr);
//            cs.Apply();
//
//            //CorrectMaxStep();
//
//            if (modelLoaded)
//            {
//                mCVode = new CvodeInterface(model);
//                for (int i = 0; i < model.getNumIndependentVariables; i++)
//                {
//                    mCVode.setAbsTolerance(i, CvodeInterface.absTol);
//                }
//                mCVode.reStart(0.0, model);
//            }
//
//            if (cs.HasSection("integration") && cs["integration"].HasCapability("usekinsol"))
//            {
//
//                CapsSupport.Capability cap = cs["integration", "usekinsol"];
//                UseKinsol = cap.IntValue;
//            }
//
//        }
//
//        Help("Sets the value of the given species or global parameter to the given value (not of local parameters)")
//        void RoadRunner::setValue(string sId, double dValue)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//
//            int nIndex = -1;
//            if (mModelGenerator->globalParameterList.find(sId, out nIndex))
//            {
//                model.gp[nIndex] = dValue;
//                return;
//            }
//            if (mModelGenerator->boundarySpeciesList.find(sId, out nIndex))
//            {
//                model.bc[nIndex] = dValue;
//                return;
//            }
//            if (mModelGenerator->compartmentList.find(sId, out nIndex))
//            {
//                model.c[nIndex] = dValue;
//                return;
//            }
//            if (mModelGenerator->floatingSpeciesConcentrationList.find(sId, out nIndex))
//            {
//                model.setConcentration(nIndex, dValue);
//                model.convertToAmounts();
//                if (!_bConservedTotalChanged) model.computeConservedTotals();
//                return;
//            }
//            if (mModelGenerator->conservationList.find(sId, out nIndex))
//            {
//                model.ct[nIndex] = dValue;
//                model.updateDependentSpeciesValues(model.y);
//                _bConservedTotalChanged = true;
//                return;
//            }
//
//            var initialConditions =
//                new List<string>((string[])getFloatingSpeciesInitialConditionNames().ToArray(typeof(string)));
//            if (initialConditions.Contains(sId))
//            {
//                int index = initialConditions.IndexOf(sId);
//                model.init_y[index] = dValue;
//                reset();
//                return;
//            }
//
//
//            throw new SBWApplicationException(String.Format("Given Id: '{0}' not found.", sId),
//                                              "Only species and global parameter values can be set");
//        }
//
//
//        Help("Gets the Value of the given species or global parameter (not of local parameters)")
//        double RoadRunner::getValue(string sId)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            int nIndex = 0;
//            if (mModelGenerator->globalParameterList.find(sId, out nIndex))
//            {
//                return model.gp[nIndex];
//            }
//            if (mModelGenerator->boundarySpeciesList.find(sId, out nIndex))
//            {
//                return model.bc[nIndex];
//            }
//            if (mModelGenerator->floatingSpeciesConcentrationList.find(sId, out nIndex))
//            {
//                return model.y[nIndex];
//            }
//
//            if (mModelGenerator->floatingSpeciesConcentrationList.find(sId.Substring(0, sId.Length - 1),
//                                                                              out nIndex))
//            {
//                //fs[j] + "'" will be interpreted as rate of change
//                return model.dydt[nIndex];
//            }
//
//            if (mModelGenerator->compartmentList.find(sId, out nIndex))
//            {
//                return model.c[nIndex];
//            }
//            if (mModelGenerator->reactionList.find(sId, out nIndex))
//            {
//                return model.rates[nIndex];
//            }
//
//            if (mModelGenerator->conservationList.find(sId, out nIndex))
//            {
//                return model.ct[nIndex];
//            }
//
//            var initialConditions =
//                new List<string>((string[])getFloatingSpeciesInitialConditionNames().ToArray(typeof(string)));
//            if (initialConditions.Contains(sId))
//            {
//                int index = initialConditions.IndexOf(sId);
//                return model.init_y[index];
//            }
//
//            if (sId.StartsWith("EE:"))
//            {
//                string parameters = sId.Substring(3);
//                var p1 = parameters.Substring(0, parameters.IndexOf(","));
//                var p2 = parameters.Substring(parameters.IndexOf(",") + 1);
//                return getEE(p1, p2, false);
//            }
//
//
//            if (sId.StartsWith("uEE:"))
//            {
//                string parameters = sId.Substring(4);
//                var p1 = parameters.Substring(0, parameters.IndexOf(","));
//                var p2 = parameters.Substring(parameters.IndexOf(",") + 1);
//                return getuEE(p1, p2, false);
//            }
//
//            if (sId.StartsWith("eigen_"))
//            {
//                var species = (sId).Substring("eigen_".Length);
//                int index;
//                mModelGenerator->floatingSpeciesConcentrationList.find(species, out index);
//                Complex[] oComplex = LA.GetEigenValues(getReducedJacobian());
//                if (oComplex.Length > selectionList[index].index)
//                {
//                    return oComplex[selectionList[index].index].Real;
//                }
//                return Double.NaN;
//            }
//
//            throw new SBWApplicationException("Given Id: '" + sId + "' not found.",
//                                              "Only species, global parameter values and fluxes can be returned");
//        }
//
//        Help(
//            "Returns symbols of the currently loaded model, that can be used for the selectionlist format array of arrays  { { \"groupname\", { \"item1\", \"item2\" ... } } }."
//            )
//        ArrayList RoadRunner::getAvailableSymbols()
//        {
//            var oResult = new ArrayList {new ArrayList(new object[] {"Time", new ArrayList(new object[] {"time"})})};
//
//            if (!modelLoaded) return oResult;
//
//            oResult.Add(new ArrayList(new object[] { "Floating Species", getFloatingSpeciesNames() }));
//            oResult.Add(new ArrayList(new object[] { "Boundary Species", getBoundarySpeciesNames() }));
//            oResult.Add(new ArrayList(new object[] { "Floating Species (amount)", getFloatingSpeciesAmountNames() }));
//            oResult.Add(new ArrayList(new object[] { "Boundary Species (amount)", getBoundarySpeciesAmountNames() }));
//            oResult.Add(new ArrayList(new object[] { "Global Parameters", getParameterNames() }));
//            oResult.Add(new ArrayList(new object[] { "Fluxes", getReactionNames() }));
//            oResult.Add(new ArrayList(new object[] { "Rates of Change", getRateOfChangeNames() }));
//            oResult.Add(new ArrayList(new object[] { "Volumes", mModelGenerator->getCompartmentList() }));
//            oResult.Add(new ArrayList(new object[] { "Elasticity Coefficients", getElasticityCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[] { "Unscaled Elasticity Coefficients", getUnscaledElasticityCoefficientNames() }));
//            oResult.Add(new ArrayList(new object[] { "Eigenvalues", getEigenValueNames() }));
//
//            return oResult;
//        }
//
//
//        Help("Returns the currently selected columns that will be returned by calls to simulate() or simulateEx(,,).")
StringList RoadRunner::getSelectionList()
{
    //var oResult = new ArrayList();
    StringList oResult;// = new ArrayList();

    if (!modelLoaded)
    {
        oResult.Add("time");
        return oResult;
    }

    StringList oFloating = mModelGenerator->getFloatingSpeciesConcentrationList();
    StringList oBoundary = mModelGenerator->getBoundarySpeciesList();
    StringList oFluxes = mModelGenerator->getReactionNames();
    StringList oVolumes = mModelGenerator->getCompartmentList();
    StringList oRates = getRateOfChangeNames();
    StringList oParameters = getParameterNames();

	vector<TSelectionRecord>::iterator iter;
    for(iter = selectionList.begin(); iter != selectionList.end(); iter++)
    {
    	TSelectionRecord record = (*iter);
        switch (record.selectionType)
        {
            case TSelectionType::clTime:
                oResult.Add("time");
                break;
            case TSelectionType::clBoundaryAmount:
                oResult.Add(Format("[{0}]", oBoundary[record.index]));
                break;
            case TSelectionType::clBoundarySpecies:
                oResult.Add(oBoundary[record.index]);
                break;
            case TSelectionType::clFloatingAmount:
                oResult.Add(Format("[{0}]", oFloating[record.index]));
                break;
            case TSelectionType::clFloatingSpecies:
                oResult.Add(oFloating[record.index]);
                break;
            case TSelectionType::clVolume:
                oResult.Add(oVolumes[record.index]);
                break;
            case TSelectionType::clFlux:
                oResult.Add(oFluxes[record.index]);
                break;
            case TSelectionType::clRateOfChange:
                oResult.Add(oRates[record.index]);
                break;
            case TSelectionType::clParameter:
                oResult.Add(oParameters[record.index]);
                break;
            case TSelectionType::clEigenValue:
                oResult.Add("eigen_" + record.p1);
                break;
            case TSelectionType::clElasticity:
                oResult.Add(Format("EE:{0},{1}", record.p1, record.p2));
                break;
            case TSelectionType::clUnscaledElasticity:
                oResult.Add(Format("uEE:{0},{1}", record.p1, record.p2));
                break;
            case TSelectionType::clStoichiometry:
                oResult.Add(record.p1);
                break;
        }
    }

    return oResult;
}


//        Help("Set the columns to be returned by simulate() or simulateEx(), valid symbol names include" +
//              " time, species names, , volume, reaction rates and rates of change (speciesName')")
//        void RoadRunner::setSelectionList(ArrayList newSelectionList)
//        {
//            selectionList = new TSelectionRecord[newSelectionList.Count];
//            ArrayList fs = mModelGenerator->getFloatingSpeciesConcentrationList();
//            ArrayList bs = mModelGenerator->getBoundarySpeciesList();
//            ArrayList rs = mModelGenerator->getReactionNames();
//            ArrayList vol = mModelGenerator->getCompartmentList();
//            ArrayList gp = mModelGenerator->getGlobalParameterList();
//            var sr = mModelGenerator->ModifiableSpeciesReferenceList;
//
//            for (int i = 0; i < newSelectionList.Count; i++)
//            {
//                // Check for species
//                for (int j = 0; j < fs.Count; j++)
//                {
//                    if ((string)newSelectionList[i] == (string)fs[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clFloatingSpecies;
//                        break;
//                    }
//
//                    if ((string)newSelectionList[i] == "[" + (string)fs[j] + "]")
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clFloatingAmount;
//                        break;
//                    }
//
//                    // Check for species rate of change
//                    if ((string)newSelectionList[i] == (string)fs[j] + "'")
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clRateOfChange;
//                        break;
//                    }
//                }
//
//                // Check fgr boundary species
//                for (int j = 0; j < bs.Count; j++)
//                {
//                    if ((string)newSelectionList[i] == (string)bs[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clBoundarySpecies;
//                        break;
//                    }
//                    if ((string)newSelectionList[i] == "[" + (string)bs[j] + "]")
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clBoundaryAmount;
//                        break;
//                    }
//                }
//
//
//                if ((string)newSelectionList[i] == "time")
//                {
//                    selectionList[i].selectionType = TSelectionType::clTime;
//                }
//
//                for (int j = 0; j < rs.Count; j++)
//                {
//                    // Check for reaction rate
//                    if ((string)newSelectionList[i] == (string)rs[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clFlux;
//                        break;
//                    }
//                }
//
//                for (int j = 0; j < vol.Count; j++)
//                {
//                    // Check for volume
//                    if ((string)newSelectionList[i] == (string)vol[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clVolume;
//                        break;
//                    }
//                }
//
//                for (int j = 0; j < gp.Count; j++)
//                {
//                    // Check for volume
//                    if ((string)newSelectionList[i] == (string)gp[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType::clParameter;
//                        break;
//                    }
//                }
//
//                if (((string)newSelectionList[i]).StartsWith("EE:"))
//                {
//                    string parameters = ((string)newSelectionList[i]).Substring(3);
//                    var p1 = parameters.Substring(0, parameters.IndexOf(","));
//                    var p2 = parameters.Substring(parameters.IndexOf(",") + 1);
//                    selectionList[i].selectionType = TSelectionType::clElasticity;
//                    selectionList[i].p1 = p1;
//                    selectionList[i].p2 = p2;
//                }
//
//                if (((string)newSelectionList[i]).StartsWith("uEE:"))
//                {
//                    string parameters = ((string)newSelectionList[i]).Substring(4);
//                    var p1 = parameters.Substring(0, parameters.IndexOf(","));
//                    var p2 = parameters.Substring(parameters.IndexOf(",") + 1);
//                    selectionList[i].selectionType = TSelectionType::clUnscaledElasticity;
//                    selectionList[i].p1 = p1;
//                    selectionList[i].p2 = p2;
//                }
//                if (((string)newSelectionList[i]).StartsWith("eigen_"))
//                {
//                    var species = ((string)newSelectionList[i]).Substring("eigen_".Length);
//                    selectionList[i].selectionType = TSelectionType::clEigenValue;
//                    selectionList[i].p1 = species;
//                    mModelGenerator->floatingSpeciesConcentrationList.find(species, out selectionList[i].index);
//                }
//
//                int index;
//                if (sr.find((string)newSelectionList[i], out index))
//                {
//                    selectionList[i].selectionType = TSelectionType::clStoichiometry;
//                    selectionList[i].index = index;
//                    selectionList[i].p1 = (string) newSelectionList[i];
//                }
//
//            }
//        }
//
//
//        Help(
//            "Carry out a single integration step using a stepsize as indicated in the method call (the intergrator is reset to take into account all variable changes). Arguments: double CurrentTime, double StepSize, Return Value: new CurrentTime."
//            )
//        double RoadRunner::oneStep(double currentTime, double stepSize)
//        {
//            return oneStep(currentTime, stepSize, true);
//        }
//
//        Help(
//           "Carry out a single integration step using a stepsize as indicated in the method call. Arguments: double CurrentTime, double StepSize, bool: reset integrator if true, Return Value: new CurrentTime."
//           )
//        double RoadRunner::oneStep(double currentTime, double stepSize, bool reset)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if (reset)
//                mCVode.reStart(currentTime, model);
//            return mCVode.OneStep(currentTime, stepSize);
//        }
//
//
//        // ---------------------------------------------------------------------
//        // Start of Level 3 API Methods
//        // ---------------------------------------------------------------------
//
//        /*Help("Compute the steady state of the model, returns the sum of squares of the solution")
//        double RoadRunner::steadyState () {
//            try {
//                if (modelLoaded) {
//                    kinSolver = new kinSolverInterface(model);
//                    return kinSolver.solve(model.y);
//                } else throw new SBWApplicationException (emptyModelStr);
//            } catch (SBWApplicationException) {
//                throw;
//            } catch (Exception e) {
//                throw new SBWApplicationException ("Unexpected error from steadyState()", e.Message);
//            }
//        }*/
//
//
//        //void TestSettings()
//        //{
//        //    var rr = new RoadRunner();
//
//        //    Debug.WriteLine(rr.getCapabilities());
//
//        //    rr.UseKinsol = 1;
//        //    Debug.WriteLine(rr.getCapabilities());
//
//        //    rr.setCapabilities(rr.getCapabilities());
//
//        //    rr.UseKinsol = 0;
//        //    Debug.WriteLine(rr.getCapabilities());
//
//        //}
//
//        Help("Compute the steady state of the model, returns the sum of squares of the solution")
double RoadRunner::steadyState()
{
    if (!modelLoaded)
    {
        throw SBWApplicationException(emptyModelStr);
    }

    try
    {
        if (UseKinsol == 0)
        {
            steadyStateSolver = new NLEQInterface(mModel);
        }
        else
        {
//            steadyStateSolver = new KinSolveInterface(mModel);
        }
        //oneStep(0.0,0.05);
        double ss = steadyStateSolver->solve(mModel->amounts);
        mModel->convertToConcentrations();
        return ss;
    }
    catch (SBWException)
    {
        throw;
    }
    catch (Exception e)
    {
        throw new SBWApplicationException("Unexpected error from steadyState solver:" +  e.Message);
    }
}
//
//
//        // ******************************************************************** }
//        // Multiply matrix 'm1' by 'm2' to give result in Self                  }
//        //                                                                      }
//        // Usage:  A.mult (A1, A2); multiply A1 by A2 giving A                  }
//        //                                                                      }
//        // ******************************************************************** }
//        double[][] mult(double[][] m1, double[][] m2)
//        {
//            int m1_nRows = m1.GetLength(0);
//            int m2_nRows = m2.GetLength(0);
//
//            int m1_nColumns = 0;
//            int m2_nColumns = 0;
//
//            if (m1_nRows > 0)
//                m1_nColumns = m1[0].GetLength(0);
//
//            if (m2_nRows > 0)
//                m2_nColumns = m2[0].GetLength(0);
//
//            if (m1.Length == 0)
//                return m1;
//            if (m2.Length == 0)
//                return m2;
//
//            if (m1_nColumns == m2_nRows)
//            {
//                var result = new double[m1_nRows][];
//                for (int i = 0; i < m1_nRows; i++) result[i] = new double[m2_nColumns];
//
//                for (int i = 0; i < result.GetLength(0); i++)
//                    for (int j = 0; j < m2_nColumns; j++)
//                    {
//                        double sum = 0.0;
//                        for (int k = 0; k < m1_nColumns; k++)
//                            sum = sum + (m1[i][k] * m2[k][j]);
//                        result[i][j] = sum;
//                    }
//                return result;
//            }
//
//            if (m1_nRows == m2_nColumns)
//            {
//                return mult(m2, m1);
//            }
//
//            throw new SBWApplicationException("Incompatible matrix operands to multiply");
//        }
//
//
//        Help("Compute the reduced Jacobian at the current operating point")
//        double[][] RoadRunner::getReducedJacobian()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    double[][] uelast = getUnscaledElasticityMatrix();
//
//                    //                    double[][] Nr = StructAnalysis.getNrMatrix();
//                    //                    double[][] L = StructAnalysis.getLinkMatrix();
//
//                    double[][] I1 = mult(_Nr, uelast);
//                    return mult(I1, _L);
//                }
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from fullJacobian()", e.Message);
//            }
//        }
//
//
//        Help("Compute the full Jacobian at the current operating point")
//        double[][] RoadRunner::getFullJacobian()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    double[][] uelast = getUnscaledElasticityMatrix();
//                    //					double[][] N = StructAnalysis.getReorderedStoichiometryMatrix();
//
//                    return mult(_N, uelast);
//                }
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from fullJacobian()", e.Message);
//            }
//        }
//
//
//        // ---------------------------------------------------------------------
//        // Start of Level 4 API Methods
//        // ---------------------------------------------------------------------
//
//        Help("Returns the Link Matrix for the currently loaded model")
//        double[][] RoadRunner::getLinkMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//
//                    return _L; //StructAnalysis.getLinkMatrix();
//
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getLMatrix()", e.Message);
//            }
//        }
//
//        Help("Returns the reduced stoichiometry matrix (Nr) for the currently loaded model")
//        double[][] RoadRunner::getNrMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//
//                    return _Nr; //StructAnalysis.getNrMatrix();
//
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getNrMatrix()", e.Message);
//            }
//        }
//
//        Help("Returns the L0 matrix for the currently loaded model")
//        double[][] RoadRunner::getL0Matrix()
//        {
//            try
//            {
//                if (modelLoaded)
//
//                    return _L0; // StructAnalysis.getL0Matrix();
//
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getL0Matrix()", e.Message);
//            }
//        }
//
//        Help("Returns the stoichiometry matrix for the currently loaded model")
//        double[][] RoadRunner::getStoichiometryMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//
//                    return _N; //StructAnalysis.getReorderedStoichiometryMatrix();
//
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getReorderedStoichiometryMatrix()", e.Message);
//            }
//        }
//
//        Help("Returns the conservation matrix (gamma) for the currently loaded model")
//        double[][] RoadRunner::getConservationMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//                    return StructAnalysis.GetGammaMatrix();
//                //return StructAnalysis.getConservationLawArray();
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getConservationLawArray()", e.Message);
//            }
//        }
//
//        Help("Returns the number of dependent species in the model")
//        int RoadRunner::getNumberOfDependentSpecies()
//        {
//            try
//            {
//                if (modelLoaded)
//                    return StructAnalysis.GetNumDependentSpecies();
//                //return StructAnalysis.getNumDepSpecies();
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getNumberOfDependentSpecies()", e.Message);
//            }
//        }
//
//        Help("Returns the number of independent species in the model")
//        int RoadRunner::getNumberOfIndependentSpecies()
//        {
//            try
//            {
//                if (modelLoaded)
//                    return StructAnalysis.GetNumIndependentSpecies();
//                //return StructAnalysis.getNumIndSpecies();
//                throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getNumberOfIndependentSpecies()", e.Message);
//            }
//        }
//
//        double RoadRunner::getVariableValue(TVariableType variableType, int variableIndex)
//        {
//            switch (variableType)
//            {
//                case TVariableType.vtFlux:
//                    return model.rates[variableIndex];
//
//                case TVariableType.vtSpecies:
//                    return model.y[variableIndex];
//
//                default:
//                    throw new SBWException("Unrecognised variable in getVariableValue");
//            }
//        }
//

void RoadRunner::setParameterValue(const TParameterType& parameterType, const int& parameterIndex, const double& value)
{
    switch (parameterType)
    {
        case TParameterType::ptBoundaryParameter:
            mModel->bc[parameterIndex] = value;
            break;

        case TParameterType::ptGlobalParameter:
            mModel->gp[parameterIndex] = value;
            break;

        case TParameterType::ptFloatingSpecies:
            mModel->y[parameterIndex] = value;
            break;

        case TParameterType::ptConservationParameter:
            mModel->ct[parameterIndex] = value;
            break;

        case TParameterType::ptLocalParameter:
            throw Exception("Local parameters not permitted in setParameterValue (getCC, getEE)");
    }
}

double RoadRunner::getParameterValue(const TParameterType& parameterType, const int& parameterIndex)
{
    switch (parameterType)
    {
        case TParameterType::ptBoundaryParameter:
            return mModel->bc[parameterIndex];

        case TParameterType::ptGlobalParameter:
            return mModel->gp[parameterIndex];

        // Used when calculating elasticities
        case TParameterType::ptFloatingSpecies:
            return mModel->y[parameterIndex];
        case TParameterType::ptConservationParameter:
            return mModel->ct[parameterIndex];
        case TParameterType::ptLocalParameter:
            throw Exception("Local parameters not permitted in getParameterValue (getCC?)");

        default:
            return 0.0;
    }
}
//
//        /// <summary>
//        /// Fills the second argument with the Inverse of the first argument
//        /// </summary>
//        /// <param name="T2">The Matrix to calculate the Inverse for</param>
//        /// <param name="Inv">will be overriden wiht the inverse of T2 (must already be allocated)</param>
//        void RoadRunner::GetInverse(Matrix T2, Matrix Inv)
//        {
//            try
//            {
//                Complex[][] T8 = LA.GetInverse(ConvertComplex(T2.data));
//                for (int i1 = 0; i1 < Inv.nRows; i1++)
//                {
//                    for (int j1 = 0; j1 < Inv.nCols; j1++)
//                    {
//                        Inv[i1, j1].Real = T8[i1][j1].Real;
//                        Inv[i1, j1].Imag = T8[i1][j1].Imag;
//                    }
//                }
//            }
//            catch (Exception)
//            {
//                throw new SBWApplicationException("Could not calculate the Inverse");
//            }
//        }
//
//        Help(
//            "Derpar Continuation, stepSize = stepsize; independentVariable = index to parameter; parameterType = {'globalParameter', 'boundarySpecies'"
//            )
//        void RoadRunner::computeContinuation(double stepSize, int independentVariable, string parameterTypeStr)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            var derpar = new TDerpar(this, mModel->getNumTotalVariables, mModel->getNumIndependentVariables);
//            derpar.setup(mModel->amounts);
//            switch (parameterTypeStr)
//            {
//                case "globalParameter":
//                    mModel->amounts =
//                        (double[])
//                        derpar.evalOneStep(mModel->amounts, stepSize, independentVariable, TDerpar.GLOBAL_PARAMETER_TYPE).
//                            Clone();
//                    break;
//                case "boundarySpecies":
//                    mModel->amounts =
//                        (double[])
//                        derpar.evalOneStep(mModel->amounts, stepSize, independentVariable, TDerpar.BOUNDARY_SPECIES_TYPE).
//                            Clone();
//                    break;
//            }
//        }
//
//        Help("Returns the Symbols of all Flux Control Coefficients.")
//        ArrayList RoadRunner::getFluxControlCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oReactions = getReactionNames();
//            ArrayList oParameters = mModelGenerator->getGlobalParameterList();
//            ArrayList oBoundary = mModelGenerator->getBoundarySpeciesList();
//            ArrayList oConservation = mModelGenerator->getConservationList();
//
//            foreach (string s in oReactions)
//            {
//                var oCCReaction = new ArrayList();
//                var oInner = new ArrayList();
//                oCCReaction.Add(s);
//
//                foreach (string sParameter in oParameters)
//                {
//                    oInner.Add("CC:" + s + "," + sParameter);
//                }
//
//                foreach (string sBoundary in oBoundary)
//                {
//                    oInner.Add("CC:" + s + "," + sBoundary);
//                }
//
//                foreach (string sConservation in oConservation)
//                {
//                    oInner.Add("CC:" + s + "," + sConservation);
//                }
//
//                oCCReaction.Add(oInner);
//                oResult.Add(oCCReaction);
//            }
//
//            return oResult;
//        }
//
//        Help("Returns the Symbols of all Concentration Control Coefficients.")
//        ArrayList RoadRunner::getConcentrationControlCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oFloating = getFloatingSpeciesNames();
//            ArrayList oParameters = mModelGenerator->getGlobalParameterList();
//            ArrayList oBoundary = mModelGenerator->getBoundarySpeciesList();
//            ArrayList oConservation = mModelGenerator->getConservationList();
//
//            foreach (string s in oFloating)
//            {
//                var oCCFloating = new ArrayList();
//                var oInner = new ArrayList();
//                oCCFloating.Add(s);
//
//                foreach (string sParameter in oParameters)
//                {
//                    oInner.Add("CC:" + s + "," + sParameter);
//                }
//
//                foreach (string sBoundary in oBoundary)
//                {
//                    oInner.Add("CC:" + s + "," + sBoundary);
//                }
//
//                foreach (string sConservation in oConservation)
//                {
//                    oInner.Add("CC:" + s + "," + sConservation);
//                }
//
//                oCCFloating.Add(oInner);
//                oResult.Add(oCCFloating);
//            }
//
//            return oResult;
//        }
//
//        Help("Returns the Symbols of all Unscaled Concentration Control Coefficients.")
//        ArrayList RoadRunner::getUnscaledConcentrationControlCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oFloating = getFloatingSpeciesNames();
//            ArrayList oParameters = mModelGenerator->getGlobalParameterList();
//            ArrayList oBoundary = mModelGenerator->getBoundarySpeciesList();
//            ArrayList oConservation = mModelGenerator->getConservationList();
//
//            foreach (string s in oFloating)
//            {
//                var oCCFloating = new ArrayList();
//                var oInner = new ArrayList();
//                oCCFloating.Add(s);
//
//                foreach (string sParameter in oParameters)
//                {
//                    oInner.Add("uCC:" + s + "," + sParameter);
//                }
//
//                foreach (string sBoundary in oBoundary)
//                {
//                    oInner.Add("uCC:" + s + "," + sBoundary);
//                }
//
//                foreach (string sConservation in oConservation)
//                {
//                    oInner.Add("uCC:" + s + "," + sConservation);
//                }
//
//                oCCFloating.Add(oInner);
//                oResult.Add(oCCFloating);
//            }
//
//            return oResult;
//        }
//
//        Help("Returns the Symbols of all Elasticity Coefficients.")
//        ArrayList RoadRunner::getElasticityCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList reactionNames = getReactionNames();
//            ArrayList floatingSpeciesNames = mModelGenerator->getFloatingSpeciesConcentrationList();
//            ArrayList boundarySpeciesNames = mModelGenerator->getBoundarySpeciesList();
//            ArrayList conservationNames = mModelGenerator->getConservationList();
//            ArrayList globalParameterNames = mModelGenerator->getGlobalParameterList();
//
//            foreach (string s in reactionNames)
//            {
//                var oCCReaction = new ArrayList();
//                var oInner = new ArrayList();
//                oCCReaction.Add(s);
//
//                foreach (string variable in floatingSpeciesNames)
//                {
//                    oInner.Add(String.Format("EE:{0},{1}", s, variable));
//                }
//
//                foreach (string variable in boundarySpeciesNames)
//                {
//                    oInner.Add(String.Format("EE:{0},{1}", s, variable));
//                }
//
//                foreach (string variable in globalParameterNames)
//                {
//                    oInner.Add(String.Format("EE:{0},{1}", s, variable));
//                }
//
//                foreach (string variable in conservationNames)
//                {
//                    oInner.Add(String.Format("EE:{0},{1}", s, variable));
//                }
//
//                oCCReaction.Add(oInner);
//                oResult.Add(oCCReaction);
//            }
//
//            return oResult;
//        }
//
//        Help("Returns the Symbols of all Unscaled Elasticity Coefficients.")
//        ArrayList RoadRunner::getUnscaledElasticityCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oReactions = getReactionNames();
//            ArrayList oFloating = mModelGenerator->getFloatingSpeciesConcentrationList();
//            ArrayList oBoundary = mModelGenerator->getBoundarySpeciesList();
//            ArrayList oGlobalParameters = mModelGenerator->getGlobalParameterList();
//            ArrayList oConservation = mModelGenerator->getConservationList();
//
//            foreach (string s in oReactions)
//            {
//                var oCCReaction = new ArrayList();
//                var oInner = new ArrayList();
//                oCCReaction.Add(s);
//
//                foreach (string variable in oFloating)
//                {
//                    oInner.Add(String.Format("uEE:{0},{1}", s, variable));
//                }
//
//                foreach (string variable in oBoundary)
//                {
//                    oInner.Add(String.Format("uEE:{0},{1}", s, variable));
//                }
//
//                foreach (string variable in oGlobalParameters)
//                {
//                    oInner.Add(String.Format("uEE:{0},{1}", s, variable));
//                }
//
//                foreach (string variable in oConservation)
//                {
//                    oInner.Add(String.Format("uEE:{0},{1}", s, variable));
//                }
//
//
//                oCCReaction.Add(oInner);
//                oResult.Add(oCCReaction);
//            }
//
//            return oResult;
//        }
//
//        Help("Returns the Symbols of all Floating Species Eigenvalues.")
//        ArrayList RoadRunner::getEigenValueNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oFloating = mModelGenerator->getFloatingSpeciesConcentrationList();
//
//            foreach (string s in oFloating)
//            {
//                oResult.Add("eigen_" + s);
//            }
//
//            return oResult;
//        }
//
//        Help(
//            "Returns symbols of the currently loaded model, that can be used for steady state analysis. Format: array of arrays  { { \"groupname\", { \"item1\", \"item2\" ... } } }  or { { \"groupname\", { \"subgroup\", { \"item1\" ... } } } }."
//            )
//        ArrayList RoadRunner::getAvailableSteadyStateSymbols()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            oResult.Add(new ArrayList(new object[] { "Floating Species", getFloatingSpeciesNames() }));
//            oResult.Add(new ArrayList(new object[] { "Boundary Species", getBoundarySpeciesNames() }));
//            oResult.Add(new ArrayList(new object[] { "Floating Species (amount)", getFloatingSpeciesAmountNames() }));
//            oResult.Add(new ArrayList(new object[] { "Boundary Species (amount)", getBoundarySpeciesAmountNames() }));
//            oResult.Add(new ArrayList(new object[] { "Global Parameters", getParameterNames() }));
//            oResult.Add(new ArrayList(new object[] { "Volumes", mModelGenerator->getCompartmentList() }));
//            oResult.Add(new ArrayList(new object[] { "Fluxes", getReactionNames() }));
//            oResult.Add(new ArrayList(new object[] { "Flux Control Coefficients", getFluxControlCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[] { "Concentration Control Coefficients", getConcentrationControlCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[
//                                  {
//                                      "Unscaled Concentration Control Coefficients",
//                                      getUnscaledConcentrationControlCoefficientNames()
//                                  }));
//            oResult.Add(new ArrayList(new object[] { "Elasticity Coefficients", getElasticityCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[] { "Unscaled Elasticity Coefficients", getUnscaledElasticityCoefficientNames() }));
//            oResult.Add(new ArrayList(new object[] { "Eigenvalues", getEigenValueNames() }));
//
//            return oResult;
//        }
//
//        Help("Returns the selection list as returned by computeSteadyStateValues().")
//        ArrayList RoadRunner::getSteadyStateSelectionList()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (_oSteadyStateSelection == null)
//            {
//                // default should be species only ...
//                ArrayList floatingSpecies = getFloatingSpeciesNames();
//                _oSteadyStateSelection = new TSelectionRecord[floatingSpecies.Count];
//                for (int i = 0; i < floatingSpecies.Count; i++)
//                {
//                    _oSteadyStateSelection[i] = new TSelectionRecord
//                                                    {
//                                                        selectionType = TSelectionType::clFloatingSpecies,
//                                                        p1 = (string) floatingSpecies[i],
//                                                        index = i
//                                                    };
//                }
//            }
//
//            ArrayList oFloating = mModelGenerator->getFloatingSpeciesConcentrationList();
//            ArrayList oBoundary = mModelGenerator->getBoundarySpeciesList();
//            ArrayList oFluxes = mModelGenerator->getReactionNames();
//            ArrayList oVolumes = mModelGenerator->getCompartmentList();
//            ArrayList oRates = getRateOfChangeNames();
//            ArrayList oParameters = getParameterNames();
//
//            var result = new ArrayList();
//            foreach (var record in _oSteadyStateSelection)
//            {
//                switch (record.selectionType)
//                {
//                    case TSelectionType::clTime:
//                        result.Add("time");
//                        break;
//                    case TSelectionType::clBoundaryAmount:
//                        result.Add(string.Format("[{0}]", oBoundary[record.index]));
//                        break;
//                    case TSelectionType::clBoundarySpecies:
//                        result.Add(oBoundary[record.index]);
//                        break;
//                    case TSelectionType::clFloatingAmount:
//                        result.Add("[" + (string)oFloating[record.index] + "]");
//                        break;
//                    case TSelectionType::clFloatingSpecies:
//                        result.Add(oFloating[record.index]);
//                        break;
//                    case TSelectionType::clVolume:
//                        result.Add(oVolumes[record.index]);
//                        break;
//                    case TSelectionType::clFlux:
//                        result.Add(oFluxes[record.index]);
//                        break;
//                    case TSelectionType::clRateOfChange:
//                        result.Add(oRates[record.index]);
//                        break;
//                    case TSelectionType::clParameter:
//                        result.Add(oParameters[record.index]);
//                        break;
//                    case TSelectionType::clEigenValue:
//                        result.Add("eigen_" + record.p1);
//                        break;
//                    case TSelectionType::clElasticity:
//                        result.Add("EE:" + record.p1 + "," + record.p2);
//                        break;
//                    case TSelectionType::clUnscaledElasticity:
//                        result.Add("uEE:" + record.p1 + "," + record.p2);
//                        break;
//                    case TSelectionType::clUnknown:
//                        result.Add(record.p1);
//                        break;
//                }
//
//            }
//
//            return result ;
//        }
//
//        TSelectionRecord[] RoadRunner::GetSteadyStateSelection(ArrayList newSelectionList)
//        {
//            var steadyStateSelection = new TSelectionRecord[newSelectionList.Count];
//            ArrayList fs = mModelGenerator->getFloatingSpeciesConcentrationList();
//            ArrayList bs = mModelGenerator->getBoundarySpeciesList();
//            ArrayList rs = mModelGenerator->getReactionNames();
//            ArrayList vol = mModelGenerator->getCompartmentList();
//            ArrayList gp = mModelGenerator->getGlobalParameterList();
//
//            for (int i = 0; i < newSelectionList.Count; i++)
//            {
//                bool set = false;
//                // Check for species
//                for (int j = 0; j < fs.Count; j++)
//                {
//                    if ((string)newSelectionList[i] == (string)fs[j])
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clFloatingSpecies;
//                        set = true;
//                        break;
//                    }
//
//                    if ((string)newSelectionList[i] == "[" + (string)fs[j] + "]")
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clFloatingAmount;
//                        set = true;
//                        break;
//                    }
//
//                    // Check for species rate of change
//                    if ((string)newSelectionList[i] == (string)fs[j] + "'")
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clRateOfChange;
//                        set = true;
//                        break;
//                    }
//                }
//
//                if (set) continue;
//
//                // Check fgr boundary species
//                for (int j = 0; j < bs.Count; j++)
//                {
//                    if ((string)newSelectionList[i] == (string)bs[j])
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clBoundarySpecies;
//                        set = true;
//                        break;
//                    }
//                    if ((string)newSelectionList[i] == "[" + (string)bs[j] + "]")
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clBoundaryAmount;
//                        set = true;
//                        break;
//                    }
//                }
//
//                if (set) continue;
//
//                if ((string)newSelectionList[i] == "time")
//                {
//                    steadyStateSelection[i].selectionType = TSelectionType::clTime;
//                    set = true;
//                }
//
//                for (int j = 0; j < rs.Count; j++)
//                {
//                    // Check for reaction rate
//                    if ((string)newSelectionList[i] == (string)rs[j])
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clFlux;
//                        set = true;
//                        break;
//                    }
//                }
//
//                for (int j = 0; j < vol.Count; j++)
//                {
//                    // Check for volume
//                    if ((string)newSelectionList[i] == (string)vol[j])
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clVolume;
//                        set = true;
//                        break;
//                    }
//                }
//
//                for (int j = 0; j < gp.Count; j++)
//                {
//                    // Check for volume
//                    if ((string)newSelectionList[i] == (string)gp[j])
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType::clParameter;
//                        set = true;
//                        break;
//                    }
//                }
//
//                if (set) continue;
//
//                // it is another symbol
//                steadyStateSelection[i].selectionType = TSelectionType::clUnknown;
//                steadyStateSelection[i].p1 = (string)newSelectionList[i];
//            }
//            return steadyStateSelection;
//        }
//
//        Help("sets the selection list as returned by computeSteadyStateValues().")
//        void RoadRunner::setSteadyStateSelectionList(ArrayList newSelectionList)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            TSelectionRecord[] steadyStateSelection = GetSteadyStateSelection(newSelectionList);
//
//            _oSteadyStateSelection = steadyStateSelection;
//
//        }
//
//        Help("performs steady state analysis, returning values as given by setSteadyStateSelectionList().")
//        double[] RoadRunner::computeSteadyStateValues()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return computeSteadyStateValues(_oSteadyStateSelection, true);
//        }
//
//        double[] RoadRunner::computeSteadyStateValues(TSelectionRecord[] oSelection, bool computeSteadyState)
//        {
//            if (computeSteadyState) steadyState();
//
//            var oResult = new double[oSelection.Length];
//            for (int i = 0; i < oResult.Length; i++)
//            {
//                oResult[i] = computeSteadyStateValue(oSelection[i]);
//            }
//            return oResult;
//
//        }
//
//        Help("performs steady state analysis, returning values as specified by the given selection list.")
//        double[] RoadRunner::computeSteadyStateValues(ArrayList oSelection)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            var selection = GetSteadyStateSelection(oSelection);
//            return computeSteadyStateValues(selection, true);
//        }
//
//        double RoadRunner::computeSteadyStateValue(TSelectionRecord record)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if (record.selectionType == TSelectionType::clUnknown)
//                return computeSteadyStateValue(record.p1);
//            return GetValueForRecord(record);
//        }
//
//        Help("Returns the value of the given steady state identifier.")
//        double RoadRunner::computeSteadyStateValue(string sId)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (sId.StartsWith("CC:"))
//            {
//                string sList = sId.Substring("CC:".Length);
//                string sVariable = sList.Substring(0, sList.IndexOf(","));
//                string sParameter = sList.Substring(sVariable.Length + 1);
//                return getCC(sVariable, sParameter);
//            }
//            if (sId.StartsWith("uCC:"))
//            {
//                string sList = sId.Substring("uCC:".Length);
//                string sVariable = sList.Substring(0, sList.IndexOf(","));
//                string sParameter = sList.Substring(sVariable.Length + 1);
//                return getuCC(sVariable, sParameter);
//            }
//            if (sId.StartsWith("EE:"))
//            {
//                string sList = sId.Substring("EE:".Length);
//                string sReaction = sList.Substring(0, sList.IndexOf(","));
//                string sVariable = sList.Substring(sReaction.Length + 1);
//                return getEE(sReaction, sVariable);
//            }
//            else if (sId.StartsWith("uEE:"))
//            {
//                string sList = sId.Substring("uEE:".Length);
//                string sReaction = sList.Substring(0, sList.IndexOf(","));
//                string sVariable = sList.Substring(sReaction.Length + 1);
//                return getuEE(sReaction, sVariable);
//            }
//            else
//            {
//                if (sId.StartsWith("eigen_"))
//                {
//                    string sSpecies = sId.Substring("eigen_".Length);
//                    int nIndex;
//                    if (mModelGenerator->floatingSpeciesConcentrationList.find(sSpecies, out nIndex))
//                    {
//                        //SBWComplex[] oComplex = SBW_CLAPACK.getEigenValues(getReducedJacobian());
//                        Complex[] oComplex = LA.GetEigenValues(getReducedJacobian());
//                        if (oComplex.Length > nIndex)
//                        {
//                            return oComplex[nIndex].Real;
//                        }
//                        return Double.NaN;
//                    }
//                    throw new SBWApplicationException(String.Format("Found unknown floating species '{0}' in computeSteadyStateValue()", sSpecies));
//                }
//                try
//                {
//                    return getValue(sId);
//                }
//                catch (Exception )
//                {
//                    throw new SBWApplicationException(String.Format("Found unknown symbol '{0}' in computeSteadyStateValue()", sId));
//                }
//
//            }
//        }
//
//        Help("Returns the values selected with setSelectionList() for the current model time / timestep")
//        double[] RoadRunner::getSelectedValues()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            var result = new double[selectionList.Length];
//
//            for (int j = 0; j < selectionList.Length; j++)
//            {
//                result[j] = GetNthSelectedOutput(j, mModel->time);
//            }
//            return result;
//        }
//
//        Help("Returns any warnings that occured during the loading of the SBML")
//        string[] RoadRunner::getWarnings()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return mModel->Warnings.ToArray();
//        }
//
//        Help("When turned on, this method will cause rates, event assignments, rules and such to be multiplied " +
//              "with the compartment volume, if species are defined as initialAmounts. By default this behavior is off.")
//        
//        void RoadRunner::ReMultiplyCompartments(bool bValue)
//        {
//            _ReMultiplyCompartments = bValue;
//        }
//
//        Help("This method turns on / off the computation and adherence to conservation laws."
//              + "By default roadRunner will discover conservation cycles and reduce the model accordingly.")
void RoadRunner::ComputeAndAssignConservationLaws(const bool& bValue)
{
    mComputeAndAssignConservationLaws = bValue;
}
//
//        Help("Returns the current generated source code")
//        string RoadRunner::getCSharpCode()
//        {
//            if (modelLoaded)
//            {
//                return _sModelCode;
//            }
//
//            throw new SBWApplicationException("Model has to be loaded first");
//        }
//
//        Help(
//            "Performs a steady state parameter scan with the given parameters returning all elments from the selectionList: (Format: symnbol, startValue, endValue, stepSize)"
//            )
//        double[][] RoadRunner::steadyStateParameterScan(string symbol, double startValue, double endValue, double stepSize)
//        {
//            var results = new List<double[]>();
//
//            double initialValue = getValue(symbol);
//            double current = startValue;
//
//            while (current < endValue)
//            {
//                setValue(symbol, current);
//                try
//                {
//                    steadyState();
//                }
//                catch (Exception)
//                {
//                    //
//                }
//
//                var currentRow = new List<double> {current};
//                currentRow.AddRange(getSelectedValues());
//
//                results.Add(currentRow.ToArray());
//                current += stepSize;
//            }
//            setValue(symbol, initialValue);
//
//            return results.ToArray();
//        }
//
//
//        Help("Returns the SBML with the current parameterset")
//        string RoadRunner::writeSBML()
//        {
//            NOM.loadSBML(NOM.getParamPromotedSBML(sbmlStr));
//            var state = new ModelState(model);
//
//            ArrayList array = getFloatingSpeciesNames();
//            for (int i = 0; i < array.Count; i++)
//                NOM.setValue((string)array[i], state.FloatingSpeciesConcentrations[i]);
//
//            array = getBoundarySpeciesNames();
//            for (int i = 0; i < array.Count; i++)
//                NOM.setValue((string)array[i], state.BoundarySpeciesConcentrations[i]);
//
//            array = getCompartmentNames();
//            for (int i = 0; i < array.Count; i++)
//                NOM.setValue((string)array[i], state.CompartmentVolumes[i]);
//
//            array = getGlobalParameterNames();
//            for (int i = 0; i < Math.Min(array.Count, state.GlobalParameters.Length); i++)
//                NOM.setValue((string)array[i], state.GlobalParameters[i]);
//
//            return NOM.getSBML();
//        }
//
//        Help("Get the number of local parameters for a given reaction")
//        int RoadRunner::getNumberOfLocalParameters(int reactionId)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return getNumberOfLocalParameters(reactionId);
//        }
//
//        Help("Sets the value of a global parameter by its index")
//        void RoadRunner::setLocalParameterByIndex(int reactionId, int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < mModel->getNumReactions) &&
//                (index >= 0) && (index < mModel->getNumLocalParameters(reactionId)))
//                mModel->lp[reactionId][index] = value;
//            else
//                throw new SBWApplicationException(string.Format("Index in setLocalParameterByIndex out of range: [{0}]", index));
//        }
//
//        Help("Returns the value of a global parameter by its index")
//        double RoadRunner::getLocalParameterByIndex(int reactionId, int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < mModel->getNumReactions) &&
//                (index >= 0) && (index < mModel->getNumLocalParameters(reactionId)))
//                return mModel->lp[reactionId][index];
//
//            throw new SBWApplicationException(String.Format("Index in getLocalParameterByIndex out of range: [{0}]", index));
//        }
//
//        Help("Set the values for all global parameters in the model")
//        void RoadRunner::setLocalParameterValues(int reactionId, double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//
//            if ((reactionId >= 0) && (reactionId < mModel->getNumReactions))
//                mModel->lp[reactionId] = values;
//            else
//                throw new SBWApplicationException(String.Format("Index in setLocalParameterValues out of range: [{0}]", reactionId));
//        }
//
//        Help("Get the values for all global parameters in the model")
//        double[] RoadRunner::getLocalParameterValues(int reactionId)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < mModel->getNumReactions))
//                return mModel->lp[reactionId];
//            throw new SBWApplicationException(String.Format("Index in getLocalParameterValues out of range: [{0}]", reactionId));
//        }
//
//        Help("Gets the list of parameter names")
//        ArrayList RoadRunner::getLocalParameterNames(int reactionId)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < mModel->getNumReactions))
//                return mModelGenerator->getLocalParameterList(reactionId);
//            throw (new SBWApplicationException("reaction Id out of range in call to getLocalParameterNames"));
//        }
//
//        Help("Returns a list of global parameter tuples: { {parameter Name, value},...")
//        ArrayList RoadRunner::getAllLocalParameterTupleList()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            var tupleList = new ArrayList();
//            for (int i = 0; i < mModelGenerator->getNumberOfReactions(); i++)
//            {
//                var tuple = new ArrayList();
//                ArrayList lpList = mModelGenerator->getLocalParameterList(i);
//                tuple.Add(i);
//                for (int j = 0; j < lpList.Count; j++)
//                {
//                    tuple.Add(lpList[j]);
//                    tuple.Add(mModel->lp[i][j]);
//                }
//                tupleList.Add(tuple);
//            }
//            return tupleList;
//        }
//
//        Help("Get the number of reactions")
//        int RoadRunner::getNumberOfReactions()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return mModel->getNumReactions;
//        }
//
//        Help("Returns the rate of a reaction by its index")
//        double RoadRunner::getReactionRate(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < mModel->getNumReactions))
//            {
//                mModel->convertToConcentrations();
//                mModel->computeReactionRates(0.0, mModel->y);
//                return mModel->rates[index];
//            }
//            throw new SBWApplicationException(String.Format("Index in getReactionRate out of range: [{0}]", index));
//        }
//
//        Help("Returns the rate of changes of a species by its index")
//double RoadRunner::getRateOfChange(const int& index)
//{
//    if (!modelLoaded)
//    {
//        throw SBWApplicationException(emptyModelStr);
//    }
//
//    if ((index >= 0) && (index < mModel->getNumTotalVariables()))
//    {
//        mModel->computeAllRatesOfChange();
//        return mModel->dydt[index];
//    }
//    throw SBWApplicationException(Format("Index in getRateOfChange out of range: [{0}]", index));
//}
//
//        Help("Returns the names given to the rate of change of the floating species")
StringList RoadRunner::getRateOfChangeNames()
{
    if (!modelLoaded)
    {
        throw SBWApplicationException(emptyModelStr);
    }

    StringList sp = mModelGenerator->getFloatingSpeciesConcentrationList(); // Reordered list
    for (int i = 0; i < sp.size(); i++)
    {
        sp[i] = sp[i] + "'";
    }
    return sp;
}
//
//        Help("Returns the rates of changes given an array of new floating species concentrations")
//        double[] RoadRunner::getRatesOfChangeEx(double[] values)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            mModel->y = values;
//            mModel->evalModel(0.0, BuildModelEvalArgument());
//            return mModel->dydt;
//        }
//
//        Help("Returns the rates of changes given an array of new floating species concentrations")
//        double[] RoadRunner::getReactionRatesEx(double[] values)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            mModel->computeReactionRates(0.0, values);
//            return mModel->rates;
//        }
//
//
//        string[] RoadRunner::GetFloatingSpeciesNamesArray()
//        {
//            return (string[])getFloatingSpeciesNames().ToArray(typeof(string));
//        }
//
//        string[] RoadRunner::GetGlobalParameterNamesArray()
//        {
//            return (string[])getGlobalParameterNames().ToArray(typeof(string));
//        }
//
//        Help("Get the number of compartments")
//        int RoadRunner::getNumberOfCompartments()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return mModel->getNumCompartments;
//        }
//
//        Help("Sets the value of a compartment by its index")
//        void RoadRunner::setCompartmentByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < mModel->getNumCompartments))
//                mModel->c[index] = value;
//            else
//                throw new SBWApplicationException(String.Format("Index in getCompartmentByIndex out of range: [{0}]", index));
//        }
//
//        Help("Returns the value of a compartment by its index")
//        double RoadRunner::getCompartmentByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < mModel->getNumCompartments))
//                return mModel->c[index];
//            throw (new SBWApplicationException(String.Format("Index in getCompartmentByIndex out of range: [{0}]", index)));
//        }
//
//        Help("Returns the value of a compartment by its index")
//        void RoadRunner::setCompartmentVolumes(double[] values)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if (values.Length < mModel->getNumCompartments)
//                mModel->c = values;
//            else
//                throw (new SBWApplicationException(String.Format("Size of vector out not in range in setCompartmentValues: [{0}]", values.Length)));
//        }
//
//        Help("Gets the list of compartment names")
StringList RoadRunner::getCompartmentNames()
{
    if (!modelLoaded)
    {
        throw SBWApplicationException(emptyModelStr);
    }
    return mModelGenerator->getCompartmentList();
}

//        Help("Get the number of boundary species")
//        int RoadRunner::getNumberOfBoundarySpecies()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return mModel->getNumBoundarySpecies;
//        }
//
//        Help("Sets the value of a boundary species by its index")
//        void RoadRunner::setBoundarySpeciesByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < mModel->getNumBoundarySpecies))
//                mModel->bc[index] = value;
//            else
//                throw new SBWApplicationException(String.Format("Index in getBoundarySpeciesByIndex out of range: [{0}]", index));
//        }
//
//        Help("Returns the value of a boundary species by its index")
//        double RoadRunner::getBoundarySpeciesByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < mModel->getNumBoundarySpecies))
//                return mModel->bc[index];
//            throw new SBWApplicationException(String.Format("Index in getBoundarySpeciesByIndex out of range: [{0}]", index));
//        }
//
//        Help("Returns an array of boundary species concentrations")
//        double[] RoadRunner::getBoundarySpeciesConcentrations()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return mModel->bc;
//        }
//
//        Help("Set the concentrations for all boundary species in the model")
//        void RoadRunner::setBoundarySpeciesConcentrations(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            mModel->bc = values;
//        }
//
//        Help("Gets the list of boundary species names")
//        ArrayList RoadRunner::getBoundarySpeciesNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return mModelGenerator->getBoundarySpeciesList();
//        }
//
//        Help("Gets the list of boundary species amount names")
//        ArrayList RoadRunner::getBoundarySpeciesAmountNames()
//        {
//            var oResult = new ArrayList();
//            foreach (string s in getBoundarySpeciesNames()) oResult.Add("[" + s + "]");
//            return oResult;
//        }
//
//        Help("Get the number of floating species")
//        int RoadRunner::getNumberOfFloatingSpecies()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return mModel->getNumTotalVariables;
//        }
//
//        Help("Sets the value of a floating species by its index")
//        void RoadRunner::setFloatingSpeciesByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < mModel->getNumTotalVariables))
//            {
//                mModel->setConcentration(index, value); // This updates the amount vector aswell
//                if (!_bConservedTotalChanged) mModel->computeConservedTotals();
//            }
//            else
//                throw new SBWApplicationException(String.Format("Index in setFloatingSpeciesByIndex out of range: [{0}]", index));
//        }
//
//        Help("Returns the value of a floating species by its index")
//        double RoadRunner::getFloatingSpeciesByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < mModel->getNumTotalVariables))
//                return mModel->getConcentration(index);
//            throw new SBWApplicationException(String.Format("Index in getFloatingSpeciesByIndex out of range: [{0}]", index));
//        }
//
//        Help("Returns an array of floating species concentrations")
//        double[] RoadRunner::getFloatingSpeciesConcentrations()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            mModel->convertToConcentrations();
//            return mModel->y;
//        }
//
//        Help("returns an array of floating species initial conditions")
//        double[] RoadRunner::getFloatingSpeciesInitialConcentrations()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return mModel->init_y;
//        }
//
//
//        // This is a level 1 Method 1
//        Help("Set the concentrations for all floating species in the model")
//        void RoadRunner::setFloatingSpeciesConcentrations(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            mModel->y = values;
//            // Update the amounts vector at the same time
//            mModel->convertToAmounts();
//            if (!_bConservedTotalChanged) mModel->computeConservedTotals();
//        }
//
//        Help("Sets the value of a floating species by its index")
//        void RoadRunner::setFloatingSpeciesInitialConcentrationByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < mModel->init_y.Length))
//            {
//                mModel->init_y[index] = value;
//                reset();
//            }
//            else
//                throw new SBWApplicationException(String.Format("Index in setFloatingSpeciesInitialConcentrationByIndex out of range: [{0}]", index));
//        }
//
//        Help("Sets the initial conditions for all floating species in the model")
//        void RoadRunner::setFloatingSpeciesInitialConcentrations(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            mModel->init_y = values;
//            reset();
//        }
//
//
//        // This is a Level 1 method !
//        Help("Returns a list of floating species names")
//        ArrayList RoadRunner::getFloatingSpeciesNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            return mModelGenerator->getFloatingSpeciesConcentrationList(); // Reordered list
//        }
//
//        Help("Returns a list of floating species initial condition names")
//        ArrayList RoadRunner::getFloatingSpeciesInitialConditionNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            ArrayList floatingSpeciesNames = mModelGenerator->getFloatingSpeciesConcentrationList();
//            var result = new ArrayList();
//            foreach (object item in floatingSpeciesNames)
//            {
//                result.Add(String.Format("init({0})", item));
//            }
//            return result;
//        }
//
//
//        Help("Returns the list of floating species amount names")
//        ArrayList RoadRunner::getFloatingSpeciesAmountNames()
//        {
//            var oResult = new ArrayList();
//            foreach (string s in getFloatingSpeciesNames()) oResult.Add(String.Format("[{0}]", s));
//            return oResult;
//        }
//
//        Help("Get the number of global parameters")
//        int RoadRunner::getNumberOfGlobalParameters()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return mModelGenerator->getGlobalParameterList().Count;
//        }
//
//        Help("Sets the value of a global parameter by its index")
//        void RoadRunner::setGlobalParameterByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < mModel->getNumGlobalParameters + mModel->ct.Length))
//            {
//                if (index >= mModel->getNumGlobalParameters)
//                {
//                    mModel->ct[index - mModel->getNumGlobalParameters] = value;
//                    mModel->updateDependentSpeciesValues(mModel->y);
//                    _bConservedTotalChanged = true;
//                }
//                else
//                    mModel->gp[index] = value;
//            }
//            else
//                throw new SBWApplicationException(String.Format("Index in getNumGlobalParameters out of range: [{0}]", index));
//        }
//
//        Help("Returns the value of a global parameter by its index")
//        double RoadRunner::getGlobalParameterByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < (mModel->getNumGlobalParameters + mModel->ct.Length)))
//            {
//                var result = new double[mModel->gp.Length + mModel->ct.Length];
//                mModel->gp.CopyTo(result, 0);
//                mModel->ct.CopyTo(result, mModel->gp.Length);
//                return result[index];
//                //return mModel->gp[index];
//            }
//            throw new SBWApplicationException(String.Format("Index in getNumGlobalParameters out of range: [{0}]", index));
//        }
//
//        Help("Set the values for all global parameters in the model")
//        void RoadRunner::setGlobalParameterValues(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if (values.Length == mModel->gp.Length)
//                mModel->gp = values;
//            else
//            {
//                for (int i = 0; i < mModel->gp.Length; i++)
//                {
//                    mModel->gp[i] = values[i];
//                }
//                for (int i = 0; i < mModel->ct.Length; i++)
//                {
//                    mModel->gp[i] = values[i + mModel->gp.Length];
//                    _bConservedTotalChanged = true;
//                }
//                mModel->updateDependentSpeciesValues(mModel->y);
//            }
//        }
//
//        Help("Get the values for all global parameters in the model")
//        double[] RoadRunner::getGlobalParameterValues()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if (mModel->ct.Length > 0)
//            {
//                var result = new double[mModel->gp.Length + mModel->ct.Length];
//                mModel->gp.CopyTo(result, 0);
//                mModel->ct.CopyTo(result, mModel->gp.Length);
//                return result;
//            }
//            return mModel->gp;
//        }
//
//        Help("Gets the list of parameter names")
//        ArrayList RoadRunner::getGlobalParameterNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return mModelGenerator->getGlobalParameterList();
//        }
//
//        Help("Returns a list of global parameter tuples: { {parameter Name, value},...")
//        ArrayList RoadRunner::getAllGlobalParameterTupleList()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            var tupleList = new ArrayList();
//            ArrayList gp = mModelGenerator->getGlobalParameterList();
//            for (int i = 0; i < gp.Count; i++)
//            {
//                var tuple = new ArrayList {gp[i], mModel->gp[i]};
//                tupleList.Add(tuple);
//            }
//            return tupleList;
//        }
//
StringList RoadRunner::getParameterNames()
{
    if (!modelLoaded)
    {
        throw SBWApplicationException(emptyModelStr);
    }
    StringList sp = mModelGenerator->getGlobalParameterList(); // Reordered list
    return sp;
}

//        Help("Updates the model based on all recent changes")
void RoadRunner::EvalModel()
{
    if (!modelLoaded)
    {
        throw new SBWApplicationException(emptyModelStr);
    }
    mModel->convertToAmounts();
    vector<double> args = mCVode->BuildEvalArgument();
    mModel->evalModel(mModel->time, args);
}
//
//        Help("Returns the name of module")
//        string RoadRunner::getName()
//        {
//            return "roadRunner";
//        }
//
//        Help("Returns the version number of the module")
//        string RoadRunner::getVersion()
//        {
//            return "2.0.1";
//        }
//
//        Help("Returns the name of the module author")
//        string RoadRunner::getAuthor()
//        {
//            return "H. M. Sauro and F. T. Bergmann";
//        }
//
//        Help("Returns a description of the module")
//        string RoadRunner::getDescription()
//        {
//            return "Simulator API based on CVODE/NLEQ/CSharp implementation";
//        }
//
//        Help("Returns the display name of the module")
//        string RoadRunner::getDisplayName()
//        {
//            return "RoadRunner";
//        }
//
//        Help("Returns the copyright string for the module")
string RoadRunner::getCopyright()
{
    return "(c) 2009 H. M. Sauro and F. T. Bergmann, BSD Licence";
}

//        Help("Returns the URL string associated with the module (if any)")
string RoadRunner::getURL()
{
    return "http://sys-bio.org";
}

//       void RoadRunner::TestChange()
//        {
//            var sbml = File.ReadAllText(@"C:\Users\fbergmann\Desktop\testModel.xml");
//            var sim = new RoadRunner();
//            sim.loadSBML(sbml);
//            sim.setTimeStart(0);
//            sim.setTimeEnd(10);
//            sim.setNumPoints(10);
//            var data = sim.simulate();
//            var writer = new StringWriter();
//            DumpResults(writer, data, sim.getSelectionList());
//            sim.changeInitialConditions(new double[] { 20, 0 });
//            sim.reset();
//            data = sim.simulate();
//            writer = new StringWriter();
//            DumpResults(writer, data, sim.getSelectionList());
//        }
//#endif
//    }


//---------------- MCA functions......


//    partial class RoadRunner
//    {
//        [Help("Get unscaled control coefficient with respect to a global parameter")]
//        double getuCC(string variableName, string parameterName)
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    TParameterType parameterType;
//                    TVariableType variableType;
//                    double originalParameterValue;
//                    int variableIndex;
//                    int parameterIndex;
//                    double f1;
//                    double f2;
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    // Check the variable name
//                    if (mModelGenerator->reactionList.find(variableName, out variableIndex))
//                    {
//                        variableType = TVariableType.vtFlux;
//                    }
//                    else if (mModelGenerator->floatingSpeciesConcentrationList.find(variableName,
//                                                                                           out variableIndex))
//                    {
//                        variableType = TVariableType.vtSpecies;
//                    }
//                    else throw new SBWApplicationException("Unable to locate variable: [" + variableName + "]");
//
//                    // Check for the parameter name
//                    if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptGlobalParameter;
//                        originalParameterValue = mModel->gp[parameterIndex];
//                    }
//                    else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptBoundaryParameter;
//                        originalParameterValue = mModel->bc[parameterIndex];
//                    }
//                    else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptConservationParameter;
//                        originalParameterValue = mModel->ct[parameterIndex];
//                    }
//                    else throw new SBWApplicationException("Unable to locate parameter: [" + parameterName + "]");
//
//                    // Get the original parameter value
//                    originalParameterValue = getParameterValue(parameterType, parameterIndex);
//
//                    double hstep = DiffStepSize*originalParameterValue;
//                    if (Math.Abs(hstep) < 1E-12)
//                        hstep = DiffStepSize;
//
//                    try
//                    {
//                        mModel->convertToConcentrations();
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue + hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fi = getVariableValue(variableType, variableIndex);
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue + 2*hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fi2 = getVariableValue(variableType, variableIndex);
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue - hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fd = getVariableValue(variableType, variableIndex);
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue - 2*hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fd2 = getVariableValue(variableType, variableIndex);
//
//                        // Use instead the 5th order approximation double unscaledValue = (0.5/hstep)*(fi-fd);
//                        // The following separated lines avoid small amounts of roundoff error
//                        f1 = fd2 + 8*fi;
//                        f2 = -(8*fd + fi2);
//                    }
//                    finally
//                    {
//                        // What ever happens, make sure we restore the parameter level
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue);
//                        steadyState();
//                    }
//                    return 1/(12*hstep)*(f1 + f2);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getuCC ()", e.Message);
//            }
//        }
//
//
//        [Help("Get scaled control coefficient with respect to a global parameter")]
//        double getCC(string variableName, string parameterName)
//        {
//            TVariableType variableType;
//            TParameterType parameterType;
//            int variableIndex;
//            int parameterIndex;
//            //double originalParameterValue;
//
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            // Check the variable name
//            if (mModelGenerator->reactionList.find(variableName, out variableIndex))
//            {
//                variableType = TVariableType.vtFlux;
//            }
//            else if (mModelGenerator->floatingSpeciesConcentrationList.find(variableName, out variableIndex))
//            {
//                variableType = TVariableType.vtSpecies;
//            }
//            else throw new SBWApplicationException("Unable to locate variable: [" + variableName + "]");
//
//            // Check for the parameter name
//            if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//            {
//                parameterType = TParameterType::ptGlobalParameter;
//                //originalParameterValue = mModel->gp[parameterIndex];
//            }
//            else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//            {
//                parameterType = TParameterType::ptBoundaryParameter;
//                //originalParameterValue = mModel->bc[parameterIndex];
//            }
//            else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//            {
//                parameterType = TParameterType::ptConservationParameter;
//                //originalParameterValue = mModel->ct[parameterIndex];
//            }
//
//
//            else throw new SBWApplicationException("Unable to locate parameter: [" + parameterName + "]");
//
//            steadyState();
//            double variableValue = getVariableValue(variableType, variableIndex);
//            double parameterValue = getParameterValue(parameterType, parameterIndex);
//
//            return getuCC(variableName, parameterName)*parameterValue/variableValue;
//        }
//
//
//        [Help("Get unscaled elasticity coefficient with respect to a global parameter or species")]
double RoadRunner::getuEE(string reactionName, string parameterName)
{
    return getuEE(reactionName, parameterName, true);
}

//[Help("Get unscaled elasticity coefficient with respect to a global parameter or species. Optionally the model is brought to steady state after the computation.")]
double RoadRunner::getuEE(string reactionName, string parameterName, bool computeSteadystate)
{
    try
    {
        if (modelLoaded)
        {
            TParameterType parameterType;
            double originalParameterValue;
            int reactionIndex;
            int parameterIndex;
            double f1;
            double f2;

            mModel->convertToConcentrations();
            mModel->computeReactionRates(mModel->time, mModel->y);

            // Check the reaction name
            if (!mModelGenerator->GetReactionList().find(reactionName, reactionIndex))
            {
                throw new SBWApplicationException("Unable to locate reaction name: [" + reactionName + "]");
            }

            // Find out what kind of parameter we are dealing with
            if (mModelGenerator->GetFloatingSpeciesConcentrationList().find(parameterName, parameterIndex))
            {
                parameterType = TParameterType::ptFloatingSpecies;
                originalParameterValue = mModel->y[parameterIndex];
            }
            else if (mModelGenerator->GetBoundarySpeciesList().find(parameterName, parameterIndex))
            {
                parameterType = TParameterType::ptBoundaryParameter;
                originalParameterValue = mModel->bc[parameterIndex];
            }
            else if (mModelGenerator->GetGlobalParameterList().find(parameterName, parameterIndex))
            {
                parameterType = TParameterType::ptGlobalParameter;
                originalParameterValue = mModel->gp[parameterIndex];
            }
            else if (mModelGenerator->GetConservationList().find(parameterName, parameterIndex))
            {
                parameterType = TParameterType::ptConservationParameter;
                originalParameterValue = mModel->ct[parameterIndex];
            }
            else throw new SBWApplicationException("Unable to locate variable: [" + parameterName + "]");

            double hstep = DiffStepSize*originalParameterValue;
            if (fabs(hstep) < 1E-12)
                hstep = DiffStepSize;

            try
            {
                mModel->convertToConcentrations();

                setParameterValue(parameterType, parameterIndex, originalParameterValue + hstep);
                mModel->computeReactionRates(mModel->time, mModel->y);
                double fi = mModel->rates[reactionIndex];

                setParameterValue(parameterType, parameterIndex, originalParameterValue + 2*hstep);
                mModel->computeReactionRates(mModel->time, mModel->y);
                double fi2 = mModel->rates[reactionIndex];

                setParameterValue(parameterType, parameterIndex, originalParameterValue - hstep);
                mModel->computeReactionRates(mModel->time, mModel->y);
                double fd = mModel->rates[reactionIndex];

                setParameterValue(parameterType, parameterIndex, originalParameterValue - 2*hstep);
                mModel->computeReactionRates(mModel->time, mModel->y);
                double fd2 = mModel->rates[reactionIndex];

                // Use instead the 5th order approximation double unscaledValue = (0.5/hstep)*(fi-fd);
                // The following separated lines avoid small amounts of roundoff error
                f1 = fd2 + 8*fi;
                f2 = -(8*fd + fi2);
            }
            catch(...)
            {}
            //finally
            {
                // What ever happens, make sure we restore the parameter level
                setParameterValue(parameterType, parameterIndex, originalParameterValue);
                mModel->computeReactionRates(mModel->time, mModel->y);
                if (computeSteadystate) steadyState();
            }
            return 1/(12*hstep)*(f1 + f2);
        }
        else throw SBWApplicationException(emptyModelStr);
    }
    catch (Exception e)
    {
        throw SBWApplicationException("Unexpected error from getuEE ()" +  e.Message);
    }
}


//        [Help("Get scaled elasticity coefficient with respect to a global parameter or species")]
double RoadRunner::getEE(string reactionName, string parameterName)
{
    return getEE(reactionName, parameterName, true);
}

//        [Help("Get scaled elasticity coefficient with respect to a global parameter or species. Optionally the model is brought to steady state after the computation.")]
double RoadRunner::getEE(string reactionName, string parameterName, bool computeSteadyState)
{
    TParameterType parameterType;
    int reactionIndex;
    int parameterIndex;

    if (!modelLoaded)
    {
    	throw new SBWApplicationException(emptyModelStr);
    }

    // Check the reaction name
    if (!mModelGenerator->GetReactionList().find(reactionName, reactionIndex))
    {
        throw SBWApplicationException(Format("Unable to locate reaction name: [{0}]", reactionName));
    }

    // Find out what kind of parameter we are dealing with
    if (mModelGenerator->GetFloatingSpeciesConcentrationList().find(parameterName, parameterIndex))
    {
        parameterType = TParameterType::ptFloatingSpecies;
    }
    else if (mModelGenerator->GetBoundarySpeciesList().find(parameterName, parameterIndex))
    {
        parameterType = TParameterType::ptBoundaryParameter;
    }
    else if (mModelGenerator->GetGlobalParameterList().find(parameterName, parameterIndex))
    {
        parameterType = TParameterType::ptGlobalParameter;
    }
    else if (mModelGenerator->GetConservationList().find(parameterName, parameterIndex))
    {
        parameterType = TParameterType::ptConservationParameter;
    }
    else
    {
    	throw SBWApplicationException(Format("Unable to locate variable: [{0}]", parameterName));
    }

    mModel->computeReactionRates(mModel->time, mModel->y);
    double variableValue = mModel->rates[reactionIndex];
    double parameterValue = getParameterValue(parameterType, parameterIndex);
    if (variableValue == 0)
    {
    	variableValue = 1e-12;
    }
    return getuEE(reactionName, parameterName, computeSteadyState) * parameterValue / variableValue;
}

//        [Ignore]
//        // Get a single species elasticity value
//        // IMPORTANT:
//        // Assumes that the reaction rates have been precomputed at the operating point !!
//        double getUnscaledSpeciesElasticity(int reactionId, int speciesIndex)
//        {
//            double f1, f2, fi, fi2, fd, fd2;
//            double originalParameterValue = mModel->getConcentration(speciesIndex);
//
//            double hstep = DiffStepSize*originalParameterValue;
//            if (Math.Abs(hstep) < 1E-12)
//                hstep = DiffStepSize;
//
//            mModel->convertToConcentrations();
//            mModel->setConcentration(speciesIndex, originalParameterValue + hstep);
//            try
//            {
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fi = mModel->rates[reactionId];
//
//                mModel->setConcentration(speciesIndex, originalParameterValue + 2*hstep);
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fi2 = mModel->rates[reactionId];
//
//                mModel->setConcentration(speciesIndex, originalParameterValue - hstep);
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fd = mModel->rates[reactionId];
//
//                mModel->setConcentration(speciesIndex, originalParameterValue - 2*hstep);
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fd2 = mModel->rates[reactionId];
//
//                // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                // The following separated lines avoid small amounts of roundoff error
//                f1 = fd2 + 8*fi;
//                f2 = -(8*fd + fi2);
//            }
//            finally
//            {
//                // What ever happens, make sure we restore the species level
//                mModel->setConcentration(speciesIndex, originalParameterValue);
//            }
//            return 1/(12*hstep)*(f1 + f2);
//        }
//
//
//        [Help(
//            "Returns the elasticity of a given reaction to a given parameter. Parameters can be boundary species or global parameters"
//            )]
//        double getUnScaledElasticity(string reactionName, string parameterName)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            double f1, f2, fi, fi2, fd, fd2;
//            double hstep;
//
//            int reactionId = -1;
//            if (!(mModelGenerator->reactionList.find(reactionName, out reactionId)))
//                throw new SBWApplicationException("Unrecognized reaction name in call to getUnScaledElasticity [" +
//                                                  reactionName + "]");
//
//            int index = -1;
//            // Find out what kind of parameter it is, species or global parmaeter
//            if (mModelGenerator->boundarySpeciesList.find(parameterName, out index))
//            {
//                double originalParameterValue = mModel->bc[index];
//                hstep = DiffStepSize*originalParameterValue;
//                if (Math.Abs(hstep) < 1E-12)
//                    hstep = DiffStepSize;
//
//                try
//                {
//                    mModel->convertToConcentrations();
//                    mModel->bc[index] = originalParameterValue + hstep;
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//                    fi = mModel->rates[reactionId];
//
//                    mModel->bc[index] = originalParameterValue + 2*hstep;
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//                    fi2 = mModel->rates[reactionId];
//
//                    mModel->bc[index] = originalParameterValue - hstep;
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//                    fd = mModel->rates[reactionId];
//
//                    mModel->bc[index] = originalParameterValue - 2*hstep;
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//                    fd2 = mModel->rates[reactionId];
//
//                    // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                    // The following separated lines avoid small amounts of roundoff error
//                    f1 = fd2 + 8*fi;
//                    f2 = -(8*fd + fi2);
//                }
//                finally
//                {
//                    mModel->bc[index] = originalParameterValue;
//                }
//            }
//            else
//            {
//                if (mModelGenerator->globalParameterList.find(parameterName, out index))
//                {
//                    double originalParameterValue = mModel->gp[index];
//                    hstep = DiffStepSize*originalParameterValue;
//                    if (Math.Abs(hstep) < 1E-12)
//                        hstep = DiffStepSize;
//
//                    try
//                    {
//                        mModel->convertToConcentrations();
//
//                        mModel->gp[index] = originalParameterValue + hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fi = mModel->rates[reactionId];
//
//                        mModel->gp[index] = originalParameterValue + 2*hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fi2 = mModel->rates[reactionId];
//
//                        mModel->gp[index] = originalParameterValue - hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fd = mModel->rates[reactionId];
//
//                        mModel->gp[index] = originalParameterValue - 2*hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fd2 = mModel->rates[reactionId];
//
//                        // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                        // The following separated lines avoid small amounts of roundoff error
//                        f1 = fd2 + 8*fi;
//                        f2 = -(8*fd + fi2);
//                    }
//                    finally
//                    {
//                        mModel->gp[index] = originalParameterValue;
//                    }
//                }
//                else if (mModelGenerator->conservationList.find(parameterName, out index))
//                {
//                    double originalParameterValue = mModel->gp[index];
//                    hstep = DiffStepSize*originalParameterValue;
//                    if (Math.Abs(hstep) < 1E-12)
//                        hstep = DiffStepSize;
//
//                    try
//                    {
//                        mModel->convertToConcentrations();
//
//                        mModel->ct[index] = originalParameterValue + hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fi = mModel->rates[reactionId];
//
//                        mModel->ct[index] = originalParameterValue + 2*hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fi2 = mModel->rates[reactionId];
//
//                        mModel->ct[index] = originalParameterValue - hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fd = mModel->rates[reactionId];
//
//                        mModel->ct[index] = originalParameterValue - 2*hstep;
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        fd2 = mModel->rates[reactionId];
//
//                        // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                        // The following separated lines avoid small amounts of roundoff error
//                        f1 = fd2 + 8*fi;
//                        f2 = -(8*fd + fi2);
//                    }
//                    finally
//                    {
//                        mModel->ct[index] = originalParameterValue;
//                    }
//                }
//                else
//                    throw new SBWApplicationException("Unrecognized parameter name in call to getUnScaledElasticity [" +
//                                                      parameterName + "]");
//            }
//            return 1/(12*hstep)*(f1 + f2);
//        }
//
//
//        [Help("Compute the unscaled species elasticity matrix at the current operating point")]
//        double[][] getUnscaledElasticityMatrix()
//        {
//            var uElastMatrix = new double[mModel->getNumReactions][];
//            for (int i = 0; i < mModel->getNumReactions; i++) uElastMatrix[i] = new double[mModel->getNumTotalVariables];
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    mModel->convertToConcentrations();
//                    // Compute reaction velocities at the current operating point
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    for (int i = 0; i < mModel->getNumReactions; i++)
//                        for (int j = 0; j < mModel->getNumTotalVariables; j++)
//                            uElastMatrix[i][j] = getUnscaledSpeciesElasticity(i, j);
//
//                    return uElastMatrix;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from unscaledElasticityMatrix()", e.Message);
//            }
//        }
//
//        [Help("Compute the unscaled elasticity matrix at the current operating point")]
//        double[][] getScaledElasticityMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    double[][] uelast = getUnscaledElasticityMatrix();
//
//                    var result = new double[uelast.Length][];
//                    for (int i = 0; i < uelast.Length; i++)
//                        result[i] = new double[uelast[0].Length];
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//                    double[] rates = mModel->rates;
//                    for (int i = 0; i < uelast.Length; i++)
//                    {
//                        // Rows are rates
//                        if (rates[i] == 0)
//                            throw new SBWApplicationException("Unable to compute elasticity, reaction rate [" +
//                                                              mModelGenerator->reactionList[i].name +
//                                                              "] set to zero");
//
//                        for (int j = 0; j < uelast[0].Length; j++) // Columns are species
//                            result[i][j] = uelast[i][j]*mModel->getConcentration(j)/rates[i];
//                    }
//                    return result;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from scaledElasticityMatrix()", e.Message);
//            }
//        }
//
//
//        [Help("Compute the unscaled elasticity for a given reaction and given species")]
//        double getUnscaledFloatingSpeciesElasticity(string reactionName, string speciesName)
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    int speciesIndex = 0;
//                    int reactionIndex = 0;
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    if (!mModelGenerator->floatingSpeciesConcentrationList.find(speciesName, out speciesIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate species name while computing unscaled elasticity");
//                    if (!mModelGenerator->reactionList.find(reactionName, out reactionIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate reaction name while computing unscaled elasticity");
//
//                    return getUnscaledSpeciesElasticity(reactionIndex, speciesIndex);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from scaledElasticityMatrix()", e.Message);
//            }
//        }
//
//        [Help("Compute the scaled elasticity for a given reaction and given species")]
//        double getScaledFloatingSpeciesElasticity(string reactionName, string speciesName)
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    int speciesIndex = 0;
//                    int reactionIndex = 0;
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    if (!mModelGenerator->floatingSpeciesConcentrationList.find(speciesName, out speciesIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate species name while computing unscaled elasticity");
//                    if (!mModelGenerator->reactionList.find(reactionName, out reactionIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate reaction name while computing unscaled elasticity");
//
//                    return getUnscaledSpeciesElasticity(reactionIndex, speciesIndex)*
//                           mModel->getConcentration(speciesIndex)/mModel->rates[reactionIndex];
//                    ;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from scaledElasticityMatrix()", e.Message);
//            }
//        }
//
//
//        [Ignore]
//        // Changes a given parameter type by the given increment
//        void changeParameter(TParameterType parameterType, int reactionIndex, int parameterIndex,
//                                     double originalValue, double increment)
//        {
//            switch (parameterType)
//            {
//                case TParameterType::ptLocalParameter:
//                    mModel->lp[reactionIndex][parameterIndex] = originalValue + increment;
//                    break;
//                case TParameterType::ptGlobalParameter:
//                    mModel->gp[parameterIndex] = originalValue + increment;
//                    break;
//                case TParameterType::ptBoundaryParameter:
//                    mModel->bc[parameterIndex] = originalValue + increment;
//                    break;
//                case TParameterType::ptConservationParameter:
//                    mModel->ct[parameterIndex] = originalValue + increment;
//                    break;
//            }
//        }
//
//
//        [Help("Returns the unscaled elasticity for a named reaction with respect to a named parameter (local or global)"
//            )]
//        double getUnscaledParameterElasticity(string reactionName, string parameterName)
//        {
//            int reactionIndex;
//            int parameterIndex;
//            double originalParameterValue;
//            TParameterType parameterType;
//
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            mModel->convertToConcentrations();
//            mModel->computeReactionRates(mModel->time, mModel->y);
//
//            if (!mModelGenerator->reactionList.find(reactionName, out reactionIndex))
//                throw new SBWApplicationException(
//                    "Internal Error: unable to locate reaction name while computing unscaled elasticity");
//
//            // Look for the parameter name, check local parameters first, then global
//            if (mModelGenerator->localParameterList[reactionIndex].find(reactionName, parameterName,
//                                                                               out parameterIndex))
//                parameterType = TParameterType::ptLocalParameter;
//            else if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                parameterType = TParameterType::ptGlobalParameter;
//            else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                parameterType = TParameterType::ptBoundaryParameter;
//            else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                parameterType = TParameterType::ptConservationParameter;
//            else
//                return 0.0;
//
//            double f1, f2, fi, fi2, fd, fd2;
//            originalParameterValue = 0.0;
//            switch (parameterType)
//            {
//                case TParameterType::ptLocalParameter:
//                    originalParameterValue = mModel->lp[reactionIndex][parameterIndex];
//                    break;
//                case TParameterType::ptGlobalParameter:
//                    originalParameterValue = mModel->gp[parameterIndex];
//                    break;
//                case TParameterType::ptBoundaryParameter:
//                    originalParameterValue = mModel->bc[parameterIndex];
//                    break;
//                case TParameterType::ptConservationParameter:
//                    originalParameterValue = mModel->ct[parameterIndex];
//                    break;
//            }
//
//            double hstep = DiffStepSize*originalParameterValue;
//            if (Math.Abs(hstep) < 1E-12)
//                hstep = DiffStepSize;
//
//            try
//            {
//                changeParameter(parameterType, reactionIndex, parameterIndex, originalParameterValue, hstep);
//                mModel->convertToConcentrations();
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fi = mModel->rates[reactionIndex];
//
//                changeParameter(parameterType, reactionIndex, parameterIndex, originalParameterValue, 2*hstep);
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fi2 = mModel->rates[reactionIndex];
//
//                changeParameter(parameterType, reactionIndex, parameterIndex, originalParameterValue, -hstep);
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fd = mModel->rates[reactionIndex];
//
//                changeParameter(parameterType, reactionIndex, parameterIndex, originalParameterValue, -2*hstep);
//                mModel->computeReactionRates(mModel->time, mModel->y);
//                fd2 = mModel->rates[reactionIndex];
//
//                // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                // The following separated lines avoid small amounts of roundoff error
//                f1 = fd2 + 8*fi;
//                f2 = -(8*fd + fi2);
//            }
//            finally
//            {
//                // What ever happens, make sure we restore the species level
//                switch (parameterType)
//                {
//                    case TParameterType::ptLocalParameter:
//                        mModel->lp[reactionIndex][parameterIndex] = originalParameterValue;
//                        break;
//                    case TParameterType::ptGlobalParameter:
//                        mModel->gp[parameterIndex] = originalParameterValue;
//                        break;
//                    case TParameterType::ptBoundaryParameter:
//                        mModel->bc[parameterIndex] = originalParameterValue;
//                        break;
//                    case TParameterType::ptConservationParameter:
//                        mModel->ct[parameterIndex] = originalParameterValue;
//                        break;
//                }
//            }
//            return 1/(12*hstep)*(f1 + f2);
//        }
//
//
//        // Use the formula: ucc = -L Jac^-1 Nr
//        [Help("Compute the matrix of unscaled concentration control coefficients")]
//        double[][] getUnscaledConcentrationControlCoefficientMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    Matrix uelast;
//                    Matrix Nr;
//                    Matrix LinkMatrix;
//
//                    setTimeStart(0.0);
//                    setTimeEnd(50.0);
//                    setNumPoints(1);
//                    simulate();
//                    if (steadyState() > STEADYSTATE_THRESHOLD)
//                    {
//                        if (steadyState() > 1E-2)
//                            throw new SBWApplicationException(
//                                "Unable to locate steady state during frequency response computation");
//                    }
//
//                    uelast = new Matrix(getUnscaledElasticityMatrix());
//                    Nr = new Matrix(getNrMatrix());
//                    LinkMatrix = new Matrix(getLinkMatrix());
//
//                    var Inv = new Matrix(Nr.nRows, LinkMatrix.nCols);
//                    var T2 = new Matrix(Nr.nRows, LinkMatrix.nCols); // Stores -Jac  and (-Jac)^-1
//                    var T3 = new Matrix(LinkMatrix.nRows, 1); // Stores (-Jac)^-1 . Nr
//                    var T4 = new Matrix(Nr.nRows, Nr.nCols);
//
//                    // Compute the Jacobian first
//                    var T1 = new Matrix(Nr.nRows, uelast.nCols);
//                    T1.mult(Nr, uelast);
//                    var Jac = new Matrix(Nr.nRows, LinkMatrix.nCols);
//                    Jac.mult(T1, LinkMatrix);
//                    T2.mult(Jac, -1.0); // Compute -Jac
//
//                    //ArrayList reactionNames = getReactionNames();
//                    //ArrayList speciesNames = getSpeciesNames();
//
//                    //SBWComplex[][] T8 = SBW_CLAPACK.Zinverse(T2.data);  // Compute ( - Jac)^-1
//                    //for (int i1 = 0; i1 < Inv.nRows; i1++)
//                    //    for (int j1 = 0; j1 < Inv.nCols; j1++)
//                    //    {
//                    //        Inv[i1, j1].Real = T8[i1][j1].Real;
//                    //        Inv[i1, j1].Imag = T8[i1][j1].Imag;
//                    //    }
//
//
//                    Complex[][] T8 = LA.GetInverse(ConvertComplex(T2.data));
//                    for (int i1 = 0; i1 < Inv.nRows; i1++)
//                        for (int j1 = 0; j1 < Inv.nCols; j1++)
//                        {
//                            Inv[i1, j1].Real = T8[i1][j1].Real;
//                            Inv[i1, j1].Imag = T8[i1][j1].Imag;
//                        }
//
//                    T3.mult(Inv, Nr); // Compute ( - Jac)^-1 . Nr
//
//                    // Finally include the dependent set as well.
//                    T4.mult(LinkMatrix, T3); // Compute L (iwI - Jac)^-1 . Nr
//                    return Matrix.convertToDouble(T4);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException(
//                    "Unexpected error from getUnscaledConcentrationControlCoefficientMatrix()", e.Message);
//            }
//        }
//
//        internal static Complex[][] ConvertComplex(SimpleComplex[][] oMatrix)
//        {
//            var oResult = new Complex[oMatrix.Length][];
//            for (int i = 0; i < oMatrix.Length; i++)
//            {
//                oResult[i] = new Complex[oMatrix[i].Length];
//                for (int j = 0; j < oMatrix[i].Length; j++)
//                {
//                    oResult[i][j] = new Complex();
//                    oResult[i][j].Real = oMatrix[i][j].Real;
//                    oResult[i][j].Imag = oMatrix[i][j].Imag;
//                }
//            }
//            return oResult;
//        }
//
//        [Help("Compute the matrix of scaled concentration control coefficients")]
//        double[][] getScaledConcentrationControlCoefficientMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    double[][] ucc = getUnscaledConcentrationControlCoefficientMatrix();
//
//                    if (ucc.Length > 0)
//                    {
//                        mModel->convertToConcentrations();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        for (int i = 0; i < ucc.Length; i++)
//                            for (int j = 0; j < ucc[0].Length; j++)
//                            {
//                                ucc[i][j] = ucc[i][j]*mModel->rates[j]/mModel->getConcentration(i);
//                            }
//                    }
//                    return ucc;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException(
//                    "Unexpected error from getScaledConcentrationControlCoefficientMatrix()", e.Message);
//            }
//        }
//
//
//        // Use the formula: ucc = elast CS + I
//        [Help("Compute the matrix of unscaled flux control coefficients")]
//        double[][] getUnscaledFluxControlCoefficientMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    double[][] ucc = getUnscaledConcentrationControlCoefficientMatrix();
//                    double[][] uee = getUnscaledElasticityMatrix();
//                    var ucc_m = new Matrix(ucc);
//                    var uee_m = new Matrix(uee);
//
//                    var T1 = new Matrix(uee_m.nRows, ucc_m.nCols);
//                    T1.mult(uee_m, ucc_m);
//                    Matrix T2 = Matrix.Identity(uee.Length);
//                    T1.add(T2);
//                    return Matrix.convertToDouble(T1);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getUnscaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help("Compute the matrix of scaled flux control coefficients")]
//        double[][] getScaledFluxControlCoefficientMatrix()
//        {
//            try
//            {
//                if (modelLoaded)
//                {
//                    double[][] ufcc = getUnscaledFluxControlCoefficientMatrix();
//
//                    if (ufcc.Length > 0)
//                    {
//                        mModel->convertToConcentrations();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        for (int i = 0; i < ufcc.Length; i++)
//                            for (int j = 0; j < ufcc[0].Length; j++)
//                            {
//                                ufcc[i][j] = ufcc[i][j]*mModel->rates[i]/mModel->rates[j];
//                            }
//                    }
//                    return ufcc;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        // ----------------------------------------------------------------------------------------------
//
//
//        [Help(
//            "Compute the value for a particular unscaled concentration control coefficients with respect to a local parameter"
//            )]
//        double getUnscaledConcentrationControlCoefficient(string speciesName, string localReactionName,
//                                                                 string parameterName)
//        {
//            int parameterIndex;
//            int reactionIndex;
//            int speciesIndex;
//            double f1;
//            double f2;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    if (!mModelGenerator->reactionList.find(localReactionName, out reactionIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate reaction name while computing unscaled control coefficient");
//
//                    if (!mModelGenerator->floatingSpeciesConcentrationList.find(speciesName, out speciesIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate species name while computing unscaled control coefficient");
//
//                    // Look for the parameter name
//                    if (mModelGenerator->localParameterList[reactionIndex].find(parameterName,
//                                                                                       out parameterIndex))
//                    {
//                        double originalParameterValue = mModel->lp[reactionIndex][parameterIndex];
//                        double hstep = DiffStepSize*originalParameterValue;
//                        if (Math.Abs(hstep) < 1E-12)
//                            hstep = DiffStepSize;
//
//                        try
//                        {
//                            mModel->convertToConcentrations();
//                            mModel->lp[reactionIndex][parameterIndex] = originalParameterValue + hstep;
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fi = mModel->getConcentration(speciesIndex);
//
//                            mModel->lp[reactionIndex][parameterIndex] = originalParameterValue + 2*hstep;
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fi2 = mModel->getConcentration(speciesIndex);
//
//                            mModel->lp[reactionIndex][parameterIndex] = originalParameterValue - hstep;
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fd = mModel->getConcentration(speciesIndex);
//
//                            mModel->lp[reactionIndex][parameterIndex] = originalParameterValue - 2*hstep;
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fd2 = mModel->getConcentration(speciesIndex);
//
//                            // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                            // The following separated lines avoid small amounts of roundoff error
//                            f1 = fd2 + 8*fi;
//                            f2 = -(8*fd + fi2);
//                        }
//                        finally
//                        {
//                            // What ever happens, make sure we restore the species level
//                            mModel->lp[reactionIndex][parameterIndex] = originalParameterValue;
//                        }
//                        return 1/(12*hstep)*(f1 + f2);
//                    }
//                    else
//                        throw new SBWApplicationException("Unable to locate local parameter [" + parameterName +
//                                                          "] in reaction [" + localReactionName + "]");
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help(
//            "Compute the value for a particular scaled concentration control coefficients with respect to a local parameter"
//            )]
//        double getScaledConcentrationControlCoefficient(string speciesName, string localReactionName,
//                                                               string parameterName)
//        {
//            int localReactionIndex;
//            int parameterIndex;
//            int speciesIndex;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    double ucc = getUnscaledConcentrationControlCoefficient(speciesName, localReactionName,
//                                                                            parameterName);
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    mModelGenerator->reactionList.find(localReactionName, out localReactionIndex);
//                    mModelGenerator->floatingSpeciesConcentrationList.find(localReactionName, out speciesIndex);
//                    mModelGenerator->localParameterList[localReactionIndex].find(parameterName,
//                                                                                        out parameterIndex);
//
//                    return ucc*mModel->lp[localReactionIndex][parameterIndex]/mModel->getConcentration(speciesIndex);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help(
//            "Compute the value for a particular concentration control coefficient, permitted parameters include global parameters, boundary conditions and conservation totals"
//            )]
//        double getUnscaledConcentrationControlCoefficient(string speciesName, string parameterName)
//        {
//            int speciesIndex;
//            int parameterIndex;
//            TParameterType parameterType;
//            double originalParameterValue;
//            double f1;
//            double f2;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    if (!mModelGenerator->floatingSpeciesConcentrationList.find(speciesName, out speciesIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate species name while computing unscaled control coefficient");
//
//                    if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptGlobalParameter;
//                        originalParameterValue = mModel->gp[parameterIndex];
//                    }
//                    else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptBoundaryParameter;
//                        originalParameterValue = mModel->bc[parameterIndex];
//                    }
//                    else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptConservationParameter;
//                        originalParameterValue = mModel->ct[parameterIndex];
//                    }
//                    else throw new SBWApplicationException("Unable to locate parameter: [" + parameterName + "]");
//
//                    double hstep = DiffStepSize*originalParameterValue;
//                    if (Math.Abs(hstep) < 1E-12)
//                        hstep = DiffStepSize;
//
//                    try
//                    {
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue + hstep);
//                        steadyState();
//                        mModel->convertToConcentrations();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fi = mModel->getConcentration(speciesIndex);
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue + 2*hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fi2 = mModel->getConcentration(speciesIndex);
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue - hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fd = mModel->getConcentration(speciesIndex);
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue - 2*hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fd2 = mModel->getConcentration(speciesIndex);
//
//                        // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                        // The following separated lines avoid small amounts of roundoff error
//                        f1 = fd2 + 8*fi;
//                        f2 = -(8*fd + fi2);
//                    }
//                    finally
//                    {
//                        // What ever happens, make sure we restore the species level
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue);
//                        steadyState();
//                    }
//                    return 1/(12*hstep)*(f1 + f2);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help(
//            "Compute the value for a particular scaled concentration control coefficients with respect to a global or boundary species parameter"
//            )]
//        double getScaledConcentrationControlCoefficient(string speciesName, string parameterName)
//        {
//            int parameterIndex;
//            int speciesIndex;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    double ucc = getUnscaledConcentrationControlCoefficient(speciesName, parameterName);
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    mModelGenerator->floatingSpeciesConcentrationList.find(speciesName, out speciesIndex);
//                    if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                        return ucc*mModel->gp[parameterIndex]/mModel->getConcentration(speciesIndex);
//                    else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                        return ucc*mModel->bc[parameterIndex]/mModel->getConcentration(speciesIndex);
//                    else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                        return ucc*mModel->ct[parameterIndex]/mModel->getConcentration(speciesIndex);
//                    return 0.0;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        // ----------------------------------------------------------------------------------------------
//
//
//        [Help("Compute the value for a particular unscaled flux control coefficients with respect to a local parameter")
//        ]
//        double getUnscaledFluxControlCoefficient(string fluxName, string localReactionName, string parameterName)
//        {
//            int parameterIndex;
//            int localReactionIndex;
//            int fluxIndex;
//            double f1;
//            double f2;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    if (!mModelGenerator->reactionList.find(localReactionName, out localReactionIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate reaction name while computing unscaled control coefficient");
//
//                    if (!mModelGenerator->reactionList.find(fluxName, out fluxIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate reaction name while computing unscaled control coefficient");
//
//                    // Look for the parameter name
//                    if (mModelGenerator->localParameterList[localReactionIndex].find(parameterName,
//                                                                                            out parameterIndex))
//                    {
//                        double originalParameterValue = mModel->lp[localReactionIndex][parameterIndex];
//                        double hstep = DiffStepSize*originalParameterValue;
//                        if (Math.Abs(hstep) < 1E-12)
//                            hstep = DiffStepSize;
//
//                        try
//                        {
//                            mModel->convertToConcentrations();
//                            mModel->lp[localReactionIndex][parameterIndex] = originalParameterValue + hstep;
//                            steadyState();
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fi = mModel->rates[fluxIndex];
//
//                            mModel->lp[localReactionIndex][parameterIndex] = originalParameterValue + 2*hstep;
//                            steadyState();
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fi2 = mModel->rates[fluxIndex];
//
//                            mModel->lp[localReactionIndex][parameterIndex] = originalParameterValue - hstep;
//                            steadyState();
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fd = mModel->rates[fluxIndex];
//
//                            mModel->lp[localReactionIndex][parameterIndex] = originalParameterValue - 2*hstep;
//                            steadyState();
//                            mModel->computeReactionRates(mModel->time, mModel->y);
//                            double fd2 = mModel->rates[fluxIndex];
//
//                            // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                            // The following separated lines avoid small amounts of roundoff error
//                            f1 = fd2 + 8*fi;
//                            f2 = -(8*fd + fi2);
//                        }
//                        finally
//                        {
//                            // What ever happens, make sure we restore the species level
//                            mModel->lp[localReactionIndex][parameterIndex] = originalParameterValue;
//                            steadyState();
//                        }
//                        return 1/(12*hstep)*(f1 + f2);
//                    }
//                    else
//                        throw new SBWApplicationException("Unable to locate local parameter [" + parameterName +
//                                                          "] in reaction [" + localReactionName + "]");
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help(
//            "Compute the value for a particular flux control coefficient, permitted parameters include global parameters, boundary conditions and conservation totals"
//            )]
//        double getUnscaledFluxControlCoefficient(string reactionName, string parameterName)
//        {
//            int fluxIndex;
//            int parameterIndex;
//            TParameterType parameterType;
//            double originalParameterValue;
//            double f1;
//            double f2;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    if (!mModelGenerator->reactionList.find(reactionName, out fluxIndex))
//                        throw new SBWApplicationException(
//                            "Internal Error: unable to locate species name while computing unscaled control coefficient");
//
//                    if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptGlobalParameter;
//                        originalParameterValue = mModel->gp[parameterIndex];
//                    }
//                    else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptBoundaryParameter;
//                        originalParameterValue = mModel->bc[parameterIndex];
//                    }
//                    else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                    {
//                        parameterType = TParameterType::ptConservationParameter;
//                        originalParameterValue = mModel->ct[parameterIndex];
//                    }
//                    else throw new SBWApplicationException("Unable to locate parameter: [" + parameterName + "]");
//
//                    double hstep = DiffStepSize*originalParameterValue;
//                    if (Math.Abs(hstep) < 1E-12)
//                        hstep = DiffStepSize;
//
//                    try
//                    {
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue + hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fi = mModel->rates[fluxIndex];
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue + 2*hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fi2 = mModel->rates[fluxIndex];
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue - hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fd = mModel->rates[fluxIndex];
//
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue - 2*hstep);
//                        steadyState();
//                        mModel->computeReactionRates(mModel->time, mModel->y);
//                        double fd2 = mModel->rates[fluxIndex];
//
//                        // Use instead the 5th order approximation double unscaledElasticity = (0.5/hstep)*(fi-fd);
//                        // The following separated lines avoid small amounts of roundoff error
//                        f1 = fd2 + 8*fi;
//                        f2 = -(8*fd + fi2);
//                    }
//                    finally
//                    {
//                        // What ever happens, make sure we restore the species level
//                        setParameterValue(parameterType, parameterIndex, originalParameterValue);
//                        steadyState();
//                    }
//                    return 1/(12*hstep)*(f1 + f2);
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help("Compute the value for a particular scaled flux control coefficients with respect to a local parameter")]
//        double getScaledFluxControlCoefficient(string reactionName, string localReactionName,
//                                                      string parameterName)
//        {
//            int parameterIndex;
//            int reactionIndex;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    double ufcc = getUnscaledFluxControlCoefficient(reactionName, localReactionName, parameterName);
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    mModelGenerator->reactionList.find(reactionName, out reactionIndex);
//                    if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                        return ufcc*mModel->gp[parameterIndex]/mModel->rates[reactionIndex];
//                    else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                        return ufcc*mModel->bc[parameterIndex]/mModel->rates[reactionIndex];
//                    else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                        return ufcc*mModel->ct[parameterIndex]/mModel->rates[reactionIndex];
//                    return 0.0;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//
//
//        [Help(
//            "Compute the value for a particular scaled flux control coefficients with respect to a global or boundary species parameter"
//            )]
//        double getScaledFluxControlCoefficient(string reactionName, string parameterName)
//        {
//            int parameterIndex;
//            int reactionIndex;
//
//            try
//            {
//                if (modelLoaded)
//                {
//                    double ufcc = getUnscaledFluxControlCoefficient(reactionName, parameterName);
//
//                    mModel->convertToConcentrations();
//                    mModel->computeReactionRates(mModel->time, mModel->y);
//
//                    mModelGenerator->reactionList.find(reactionName, out reactionIndex);
//                    if (mModelGenerator->globalParameterList.find(parameterName, out parameterIndex))
//                        return ufcc*mModel->gp[parameterIndex]/mModel->rates[reactionIndex];
//                    else if (mModelGenerator->boundarySpeciesList.find(parameterName, out parameterIndex))
//                        return ufcc*mModel->bc[parameterIndex]/mModel->rates[reactionIndex];
//                    else if (mModelGenerator->conservationList.find(parameterName, out parameterIndex))
//                        return ufcc*mModel->ct[parameterIndex]/mModel->rates[reactionIndex];
//                    return 0.0;
//                }
//                else throw new SBWApplicationException(emptyModelStr);
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from getScaledFluxControlCoefficientMatrix()",
//                                                  e.Message);
//            }
//        }
//    }
//}


}//namespace rr
