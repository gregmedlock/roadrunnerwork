#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <iostream>
#include "rrRoadRunner.h"
#include "rrException.h"
#include "rrModelGenerator.h"
#include "rrCompiler.h"
#include "rrStreamWriter.h"
#include "rrLogger.h"
//---------------------------------------------------------------------------
#if defined(__BORLANDC__)
#pragma package(smart_init)
#endif
//---------------------------------------------------------------------------

using namespace std;
namespace rr
{

//Initialize statics..
bool RoadRunner::_bComputeAndAssignConservationLaws = true;
bool RoadRunner::_bConservedTotalChanged 			= false;
bool RoadRunner::_ReMultiplyCompartments 			= false;

RoadRunner::RoadRunner()
:
DiffStepSize(0.05),
emptyModelStr("A model needs to be loaded before one can use this method"),
STEADYSTATE_THRESHOLD(1.E-2),
cvode(NULL),
mModelGenerator(NULL),
mCompiler(NULL),
_L(NULL),
_L0(NULL),
_N(NULL),
_Nr(NULL),
mModel(NULL)
{
	Log(lDebug4)<<"In RoadRunner CTOR";
	mModelGenerator = new ModelGenerator();
}

RoadRunner::~RoadRunner()
{
	Log(lDebug4)<<"In RoadRunner DTOR";
	delete mModelGenerator;
    delete mModel;
    delete cvode;
    delete mCompiler;
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

	return _sModelCode;

}

void RoadRunner::InitializeModel(IModel* aModel)
{
    mModel = aModel;

    if(!mModel)
    {
    	return;
    }
    IModel& model = *mModel;

    //model.Warnings.AddRange(ModelGenerator.Instance.Warnings);

    modelLoaded = true;
    _bConservedTotalChanged = false;

    model.setCompartmentVolumes();
    model.initializeInitialConditions();
    model.setParameterValues();
    model.setCompartmentVolumes();
    model.setBoundaryConditions();
    model.setInitialConditions();
    model.convertToAmounts();
    model.evalInitialAssignments();
    model.computeRules(model.y);
    model.convertToAmounts();

    if (_bComputeAndAssignConservationLaws) model.computeConservedTotals();

    cvode = new CvodeInterface(mModel);

    reset();

    // Construct default selection list
    selectionList.resize(model.getNumTotalVariables + 1); // + 1 to include time
    selectionList[0].selectionType = clTime;
    for (int i = 0; i < model.getNumTotalVariables; i++)
    {
        selectionList[i + 1].index = i;
        selectionList[i + 1].selectionType = clFloatingSpecies;
    }

//    _oSteadyStateSelection = NULL;
}


//        //private ArrayList _oSteadyStateSelection;
//        private TSelectionRecord[] _oSteadyStateSelection;
//        private string _sModelCode;
//
//        private CvodeInterface cvode;
//        //kinSolverInterface kinSolver;  // Use NLEQ1 instead
//
//        public IModel model = null;
//        public bool modelLoaded = false;
//        private ISteadyStateSolver steadyStateSolver;
//        public int numPoints;
//        public string sbmlStr;
//        private TSelectionRecord[] selectionList;
//        public double timeEnd;
//        public double timeStart;
//
//        public RoadRunner()
//        {
//
//            System.Globalization.CultureInfo culture = System.Globalization.CultureInfo.CreateSpecificCulture("en");
//            culture.NumberFormat.NumberDecimalSeparator = ".";
//            Thread.CurrentThread.CurrentCulture = culture;
//            // Set up some defaults
//            timeStart = 0;
//            timeEnd = 10;
//            numPoints = 21;
//            sbmlStr = "";
//
//            UseKinsol = IsNleqAvailable() ? 0 : 1;
//        }
//
//        private bool IsNleqAvailable()
//        {
//            return NLEQInterface.IsAvailable;
//        }
//#if DEBUG
//        public static void Test()
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
//            //_bComputeAndAssignConservationLaws = false;
//            //sim.loadSBMLFromFile(@"C:\Development\test-suite\cases\semantic\00424\00424-sbml-l3v1.xml");
//            //sim.setSelectionList(new ArrayList(new string[] {
//            //    "time", "S1", "S2", "S3"
//            //    }));
//
//            ////sim.CorrectMaxStep();
//            //CvodeInterface.MaxStep = 0.0001;
//            //sim.cvode.reStart(0.0, sim.model);
//
//            ////sim.cvode.
//            //results = sim.simulateEx(0, 5, 51);
//            //DumpResults(Console.Out, results, sim.getSelectionList());
//
//            //Debug.WriteLine(writer.GetStringBuilder().ToString());
//            //Debug.WriteLine(sim.getWarnings());
//            //Debug.WriteLine(sim.getCapabilities());
//        }
//#endif
//
//        [Ignore]
//        public string NL
//        {
//            [Ignore]
//            get { return _NL(); }
//        }
//
//
//        [Ignore]
//        private string _NL()
//        {
//            return Environment.NewLine;
//        }
//
//
//        [Ignore]
//        private void emptyModel()
//        {
//            throw new SBWApplicationException(emptyModelStr);
//        }
//
//
//        private double GetValueForRecord(TSelectionRecord record)
//        {
//            double dResult;
//            switch (record.selectionType)
//            {
//                case TSelectionType.clFloatingSpecies:
//                    dResult = model.getConcentration(record.index);
//                    break;
//                case TSelectionType.clBoundarySpecies:
//                    dResult = model.bc[record.index];
//                    break;
//                case TSelectionType.clFlux:
//                    dResult = model.rates[record.index];
//                    break;
//                case TSelectionType.clRateOfChange:
//                    dResult = model.dydt[record.index];
//                    break;
//                case TSelectionType.clVolume:
//                    dResult = model.c[record.index];
//                    break;
//                case TSelectionType.clParameter:
//                    {
//                        if (record.index > model.gp.Length - 1)
//                            dResult = model.ct[record.index - model.gp.Length];
//                        else
//                            dResult = model.gp[record.index];
//                    }
//                    break;
//                case TSelectionType.clFloatingAmount:
//                    dResult = model.amounts[record.index];
//                    break;
//                case TSelectionType.clBoundaryAmount:
//                    int nIndex;
//                    if (
//                        ModelGenerator.Instance.compartmentList.find(
//                            ModelGenerator.Instance.boundarySpeciesList[record.index].compartmentName,
//                            out nIndex))
//                        dResult = model.bc[record.index] * model.c[nIndex];
//                    else
//                        dResult = 0.0;
//                    break;
//                case TSelectionType.clElasticity:
//                    dResult = getEE(record.p1, record.p2, false);
//                    break;
//                case TSelectionType.clUnscaledElasticity:
//                    dResult = getuEE(record.p1, record.p2, false);
//                    break;
//                case TSelectionType.clEigenValue:
//                    Complex[] oComplex = LA.GetEigenValues(getReducedJacobian());
//                    if (oComplex.Length > record.index)
//                    {
//                        dResult = oComplex[record.index].Real;
//                    }
//                    else
//                        dResult = Double.NaN;
//                    break;
//                case TSelectionType.clStoichiometry:
//                    dResult = model.sr[record.index];
//                    break;
//                default:
//                    dResult = 0.0;
//                    break;
//            }
//            return dResult;
//        }
//
//        private double GetNthSelectedOutput(int index, double dCurrentTime)
//        {
//            TSelectionRecord record = selectionList[index];
//            if (record.selectionType == TSelectionType.clTime)
//                return dCurrentTime;
//
//            return GetValueForRecord(record);
//        }
//
//        private void AddNthOutputToResult(double[,] results, int nRow, double dCurrentTime)
//        {
//            for (int j = 0; j < selectionList.Length; j++)
//            {
//                results[nRow, j] = GetNthSelectedOutput(j, dCurrentTime);
//            }
//        }
//
//        private double[] BuildModelEvalArgument()
//        {
//            var dResult = new double[model.amounts.Length + model.rateRules.Length];
//            double[] dCurrentRuleValues = model.GetCurrentValues();
//            dCurrentRuleValues.CopyTo(dResult, 0);
//            model.amounts.CopyTo(dResult, model.rateRules.Length);
//            return dResult;
//        }
//
//        [Ignore]
//        public double[,] runSimulation()
//        {
//            double hstep = (timeEnd - timeStart) / (numPoints - 1);
//            var results = new double[numPoints, selectionList.Length];
//
//            model.evalModel(timeStart, BuildModelEvalArgument());
//
//            AddNthOutputToResult(results, 0, timeStart);
//
//            if (cvode.HaveVariables)
//            {
//
//                int restartResult = cvode.reStart(timeStart, model);
//                if (restartResult != 0)
//                    throw new SBWApplicationException("Error in reStart call to CVODE");
//            }
//            double tout = timeStart;
//            for (int i = 1; i < numPoints; i++)
//            {
//                cvode.OneStep(tout, hstep);
//                tout = timeStart + i * hstep;
//                AddNthOutputToResult(results, i, tout);
//            }
//            return results;
//        }
//
//        // -------------------------------------------------------------------------------
//
//#if DEBUG
//        public static void PrintTout(double start, double end, int numPoints)
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
//#endif
//

//        private static void DumpResults(TextWriter writer, double[,] data, ArrayList colLabels)
//        {
//            for (int i = 0; i < colLabels.Count; i++)
//            {
//                writer.Write(colLabels[i] + "\t");
//                Debug.Write(colLabels[i] + "\t");
//            }
//            writer.WriteLine();
//            Debug.WriteLine("");
//
//            for (int i = 0; i < data.GetLength(0); i++)
//            {
//                for (int j = 0; j < data.GetLength(1); j++)
//                {
//                    writer.Write(data[i, j] + "\t");
//                    Debug.Write(data[i, j] + "\t");
//                }
//                writer.WriteLine();
//                Debug.WriteLine("");
//            }
//        }
//
//
//        //public static void TestDirectory(string directory, bool testSubDirs)
//        //{
//        //    //TestDirectory(directory, testSubDirs, "*sbml-l3v1.xml");
//        //    TestDirectory(directory, testSubDirs, "*.xml");
//        //}
//
//    //    public static void TestDirectory(string directory, bool testSubDirs, string pattern)
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
//
//
//        [Ignore]
//        public static void SimulateSBMLFile(string fileName, bool useConservationLaws)
//        {
//            var sim = new RoadRunner();
//            ComputeAndAssignConservationLaws(useConservationLaws);
//            sim.loadSBML(File.ReadAllText(fileName));
//
//            double[,] data = sim.simulate();
//            ArrayList list = sim.getSelectionList();
//            TextWriter writer = Console.Out;
//
//            DumpResults(writer, data, list);
//            return;
//        }
//
//        [Ignore]
//        public static void SimulateSBMLFile(string fileName, bool useConservationLaws, double startTime, double endTime,
//                                            int numPoints)
//        {
//            var sim = new RoadRunner();
//            ComputeAndAssignConservationLaws(useConservationLaws);
//            sim.loadSBML(File.ReadAllText(fileName));
//
//            try
//            {
//                double[,] data = sim.simulateEx(startTime, endTime, numPoints);
//                ArrayList list = sim.getSelectionList();
//                TextWriter writer = Console.Error;
//
//                DumpResults(writer, data, list);
//            }
//            catch (Exception ex)
//            {
//                Debug.WriteLine(ex);
//            }
//
//            //Debug.WriteLine(sim.getCapabilities());
//
//            return;
//        }
//
//        [Help("Load SBML into simulator")]
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
    loadSBML(sbml);
}

//        [Help("Load SBML into simulator")]
void RoadRunner::loadSBML(const string& sbml)
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
        InitializeModel(mModel);
        //reset();
    }
    else
    {
        if(mModel != NULL)
        {
            delete cvode;
            cvode = NULL;
            delete mModel;
            mModel = NULL;
            modelLoaded = false;
        }

        sbmlStr = sbml;

		_sModelCode = mModelGenerator->generateModelCode(sbmlStr);

        if(!_sModelCode.size())
        {
            throw RRException("Failed to generate Model Code");
        }
        Log(lDebug)<<" ------ Model Code --------\n"
        			<<_sModelCode
                    <<" ----- End of Model Code -----\n";

 		if(!mCompiler)
        {
        	mCompiler  = new Compiler;
        }

        //The get instance function actually compiles the supplied code..
        rrObject* o = mCompiler;//.getInstance(_sModelCode, "TModel", sLocation);

        if (o != NULL)
        {
            InitializeModel(dynamic_cast<IModel*>(o));
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
                    sw.Write(_sModelCode);
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
}

//        [Help("Returns the initially loaded model as SBML")]
//        public string getSBML()
//        {
//            return sbmlStr;
//        }
//
//        [Help("get the currently set time start")]
//        public double getTimeStart()
//        {
//            return timeStart;
//        }
//
//        [Help("get the currently set time end")]
//        public double getTimeEnd()
//        {
//            return timeEnd;
//        }
//
//        [Help("get the currently set number of points")]
//        public int getNumPoints()
//        {
//            return numPoints;
//        }
//
//        [Help("Set the time start for the simulation")]
//        public void setTimeStart(double startTime)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (startTime < 0)
//                throw new SBWApplicationException("Time Start most be greater than zero");
//            this.timeStart = startTime;
//        }
//
//        [Help("Set the time end for the simulation")]
//        public void setTimeEnd(double endTime)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (endTime <= 0)
//                throw new SBWApplicationException("Time End most be greater than zero");
//            this.timeEnd = endTime;
//        }
//
//        [Help("Set the number of points to generate during the simulation")]
//        public void setNumPoints(int nummberOfPoints)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if (nummberOfPoints <= 0)
//                nummberOfPoints = 1;
//            this.numPoints = nummberOfPoints;
//        }
//
//        [Help("reset the simulator back to the initial conditions specified in the SBML model")]
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
//                if (_bComputeAndAssignConservationLaws && !_bConservedTotalChanged) model.computeConservedTotals();
//
//                cvode.AssignNewVector(model, true);
//                cvode.TestRootsAtInitialTime();
//
//                //double hstep = (timeEnd - timeStart) / (numPoints - 1);
//                //CvodeInterface.MaxStep = Math.Min(CvodeInterface.MaxStep, hstep);
//                //if (CvodeInterface.MaxStep == 0)
//                //    CvodeInterface.MaxStep = hstep;
//
//
//                model.time = 0.0;
//                cvode.reStart(0.0, model);
//
//                cvode.assignments.Clear();
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
//        [Help(
//            "Change the initial conditions to another concentration vector (changes only initial conditions for floating Species)"
//            )]
//        public void changeInitialConditions(double[] ic)
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
//        [Help("Carry out a time course simulation")]
//        public double[,] simulate()
//        {
//            try
//            {
//                if (!modelLoaded)
//                    throw new SBWApplicationException(emptyModelStr);
//                if (timeEnd <= timeStart)
//                    throw new SBWApplicationException("Error: time end must be greater than time start");
//                return runSimulation();
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from simulate(): " + e.Message);
//            }
//        }
//
//        [Help(
//            "Extension method to simulate (time start, time end, number of points). This routine resets the model to its initial condition before running the simulation (unlike simulate())"
//            )]
//        public double[,] simulateEx(double startTime, double endTime, int numberOfPoints)
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
//        [Help("Returns the current vector of reactions rates")]
//        public double[] getReactionRates()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            model.convertToConcentrations();
//            model.computeReactionRates(0.0, model.y);
//            return model.rates;
//        }
//
//        [Help("Returns the current vector of rates of change")]
//        public double[] getRatesOfChange()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            model.computeAllRatesOfChange();
//            return model.dydt;
//        }
//
//        [Help(
//            "Returns a list of floating species names: This method is deprecated, please use getFloatingSpeciesNames()")
//        ]
//        public ArrayList getSpeciesNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            return ModelGenerator.Instance.getFloatingSpeciesConcentrationList(); // Reordered list
//        }
//
//        [Help("Returns a list of reaction names")]
//        public ArrayList getReactionNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            return ModelGenerator.Instance.getReactionNames();
//        }
//
//
//        // ---------------------------------------------------------------------
//        // Start of Level 2 API Methods
//        // ---------------------------------------------------------------------
//
//        public int UseKinsol { get; set; }
//
//        [Help("Get Simulator Capabilities")]
//        public string getCapabilities()
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
//        [Ignore]
//        public void setTolerances(double aTol, double rTol)
//        {
//            CvodeInterface.relTol = rTol;
//            CvodeInterface.absTol = aTol;
//        }
//
//        [Ignore]
//        public void setTolerances(double aTol, double rTol, int maxSteps)
//        {
//            setTolerances(aTol, rTol);
//            CvodeInterface.MaxNumSteps = maxSteps;
//        }
//
//        public void CorrectMaxStep()
//        {
//            double maxStep = (timeEnd - timeStart) / (numPoints);
//            maxStep = Math.Min(CvodeInterface.MaxStep, maxStep);
//            CvodeInterface.MaxStep = maxStep;
//        }
//
//        [Help("Set Simulator Capabilites")]
//        public void setCapabilities(string capsStr)
//        {
//            var cs = new CapsSupport(capsStr);
//            cs.Apply();
//
//            //CorrectMaxStep();
//
//            if (modelLoaded)
//            {
//                cvode = new CvodeInterface(model);
//                for (int i = 0; i < model.getNumIndependentVariables; i++)
//                {
//                    cvode.setAbsTolerance(i, CvodeInterface.absTol);
//                }
//                cvode.reStart(0.0, model);
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
//        [Help("Sets the value of the given species or global parameter to the given value (not of local parameters)")]
//        public void setValue(string sId, double dValue)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//
//            int nIndex = -1;
//            if (ModelGenerator.Instance.globalParameterList.find(sId, out nIndex))
//            {
//                model.gp[nIndex] = dValue;
//                return;
//            }
//            if (ModelGenerator.Instance.boundarySpeciesList.find(sId, out nIndex))
//            {
//                model.bc[nIndex] = dValue;
//                return;
//            }
//            if (ModelGenerator.Instance.compartmentList.find(sId, out nIndex))
//            {
//                model.c[nIndex] = dValue;
//                return;
//            }
//            if (ModelGenerator.Instance.floatingSpeciesConcentrationList.find(sId, out nIndex))
//            {
//                model.setConcentration(nIndex, dValue);
//                model.convertToAmounts();
//                if (!_bConservedTotalChanged) model.computeConservedTotals();
//                return;
//            }
//            if (ModelGenerator.Instance.conservationList.find(sId, out nIndex))
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
//        [Help("Gets the Value of the given species or global parameter (not of local parameters)")]
//        public double getValue(string sId)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            int nIndex = 0;
//            if (ModelGenerator.Instance.globalParameterList.find(sId, out nIndex))
//            {
//                return model.gp[nIndex];
//            }
//            if (ModelGenerator.Instance.boundarySpeciesList.find(sId, out nIndex))
//            {
//                return model.bc[nIndex];
//            }
//            if (ModelGenerator.Instance.floatingSpeciesConcentrationList.find(sId, out nIndex))
//            {
//                return model.y[nIndex];
//            }
//
//            if (ModelGenerator.Instance.floatingSpeciesConcentrationList.find(sId.Substring(0, sId.Length - 1),
//                                                                              out nIndex))
//            {
//                //fs[j] + "'" will be interpreted as rate of change
//                return model.dydt[nIndex];
//            }
//
//            if (ModelGenerator.Instance.compartmentList.find(sId, out nIndex))
//            {
//                return model.c[nIndex];
//            }
//            if (ModelGenerator.Instance.reactionList.find(sId, out nIndex))
//            {
//                return model.rates[nIndex];
//            }
//
//            if (ModelGenerator.Instance.conservationList.find(sId, out nIndex))
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
//                ModelGenerator.Instance.floatingSpeciesConcentrationList.find(species, out index);
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
//        [Help(
//            "Returns symbols of the currently loaded model, that can be used for the selectionlist format array of arrays  { { \"groupname\", { \"item1\", \"item2\" ... } } }."
//            )]
//        public ArrayList getAvailableSymbols()
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
//            oResult.Add(new ArrayList(new object[] { "Volumes", ModelGenerator.Instance.getCompartmentList() }));
//            oResult.Add(new ArrayList(new object[] { "Elasticity Coefficients", getElasticityCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[] { "Unscaled Elasticity Coefficients", getUnscaledElasticityCoefficientNames() }));
//            oResult.Add(new ArrayList(new object[] { "Eigenvalues", getEigenValueNames() }));
//
//            return oResult;
//        }
//
//
//        [Help("Returns the currently selected columns that will be returned by calls to simulate() or simulateEx(,,).")]
//        public ArrayList getSelectionList()
//        {
//            var oResult = new ArrayList();
//
//            if (!modelLoaded)
//            {
//                oResult.Add("time");
//                return oResult;
//            }
//
//            ArrayList oFloating = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            ArrayList oBoundary = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList oFluxes = ModelGenerator.Instance.getReactionNames();
//            ArrayList oVolumes = ModelGenerator.Instance.getCompartmentList();
//            ArrayList oRates = getRateOfChangeNames();
//            ArrayList oParameters = getParameterNames();
//
//
//            foreach (TSelectionRecord record in selectionList)
//            {
//                switch (record.selectionType)
//                {
//                    case TSelectionType.clTime:
//                        oResult.Add("time");
//                        break;
//                    case TSelectionType.clBoundaryAmount:
//                        oResult.Add(string.Format("[{0}]", oBoundary[record.index]));
//                        break;
//                    case TSelectionType.clBoundarySpecies:
//                        oResult.Add(oBoundary[record.index]);
//                        break;
//                    case TSelectionType.clFloatingAmount:
//                        oResult.Add(string.Format("[{0}]", oFloating[record.index]));
//                        break;
//                    case TSelectionType.clFloatingSpecies:
//                        oResult.Add(oFloating[record.index]);
//                        break;
//                    case TSelectionType.clVolume:
//                        oResult.Add(oVolumes[record.index]);
//                        break;
//                    case TSelectionType.clFlux:
//                        oResult.Add(oFluxes[record.index]);
//                        break;
//                    case TSelectionType.clRateOfChange:
//                        oResult.Add(oRates[record.index]);
//                        break;
//                    case TSelectionType.clParameter:
//                        oResult.Add(oParameters[record.index]);
//                        break;
//                    case TSelectionType.clEigenValue:
//                        oResult.Add("eigen_" + record.p1);
//                        break;
//                    case TSelectionType.clElasticity:
//                        oResult.Add(String.Format("EE:{0},{1}", record.p1, record.p2));
//                        break;
//                    case TSelectionType.clUnscaledElasticity:
//                        oResult.Add(String.Format("uEE:{0},{1}", record.p1, record.p2));
//                        break;
//                    case TSelectionType.clStoichiometry:
//                        oResult.Add(record.p1);
//                        break;
//                }
//            }
//
//            return oResult;
//        }
//
//
//        [Help("Set the columns to be returned by simulate() or simulateEx(), valid symbol names include" +
//              " time, species names, , volume, reaction rates and rates of change (speciesName')")]
//        public void setSelectionList(ArrayList newSelectionList)
//        {
//            selectionList = new TSelectionRecord[newSelectionList.Count];
//            ArrayList fs = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            ArrayList bs = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList rs = ModelGenerator.Instance.getReactionNames();
//            ArrayList vol = ModelGenerator.Instance.getCompartmentList();
//            ArrayList gp = ModelGenerator.Instance.getGlobalParameterList();
//            var sr = ModelGenerator.Instance.ModifiableSpeciesReferenceList;
//
//            for (int i = 0; i < newSelectionList.Count; i++)
//            {
//                // Check for species
//                for (int j = 0; j < fs.Count; j++)
//                {
//                    if ((string)newSelectionList[i] == (string)fs[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType.clFloatingSpecies;
//                        break;
//                    }
//
//                    if ((string)newSelectionList[i] == "[" + (string)fs[j] + "]")
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType.clFloatingAmount;
//                        break;
//                    }
//
//                    // Check for species rate of change
//                    if ((string)newSelectionList[i] == (string)fs[j] + "'")
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType.clRateOfChange;
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
//                        selectionList[i].selectionType = TSelectionType.clBoundarySpecies;
//                        break;
//                    }
//                    if ((string)newSelectionList[i] == "[" + (string)bs[j] + "]")
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType.clBoundaryAmount;
//                        break;
//                    }
//                }
//
//
//                if ((string)newSelectionList[i] == "time")
//                {
//                    selectionList[i].selectionType = TSelectionType.clTime;
//                }
//
//                for (int j = 0; j < rs.Count; j++)
//                {
//                    // Check for reaction rate
//                    if ((string)newSelectionList[i] == (string)rs[j])
//                    {
//                        selectionList[i].index = j;
//                        selectionList[i].selectionType = TSelectionType.clFlux;
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
//                        selectionList[i].selectionType = TSelectionType.clVolume;
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
//                        selectionList[i].selectionType = TSelectionType.clParameter;
//                        break;
//                    }
//                }
//
//                if (((string)newSelectionList[i]).StartsWith("EE:"))
//                {
//                    string parameters = ((string)newSelectionList[i]).Substring(3);
//                    var p1 = parameters.Substring(0, parameters.IndexOf(","));
//                    var p2 = parameters.Substring(parameters.IndexOf(",") + 1);
//                    selectionList[i].selectionType = TSelectionType.clElasticity;
//                    selectionList[i].p1 = p1;
//                    selectionList[i].p2 = p2;
//                }
//
//                if (((string)newSelectionList[i]).StartsWith("uEE:"))
//                {
//                    string parameters = ((string)newSelectionList[i]).Substring(4);
//                    var p1 = parameters.Substring(0, parameters.IndexOf(","));
//                    var p2 = parameters.Substring(parameters.IndexOf(",") + 1);
//                    selectionList[i].selectionType = TSelectionType.clUnscaledElasticity;
//                    selectionList[i].p1 = p1;
//                    selectionList[i].p2 = p2;
//                }
//                if (((string)newSelectionList[i]).StartsWith("eigen_"))
//                {
//                    var species = ((string)newSelectionList[i]).Substring("eigen_".Length);
//                    selectionList[i].selectionType = TSelectionType.clEigenValue;
//                    selectionList[i].p1 = species;
//                    ModelGenerator.Instance.floatingSpeciesConcentrationList.find(species, out selectionList[i].index);
//                }
//
//                int index;
//                if (sr.find((string)newSelectionList[i], out index))
//                {
//                    selectionList[i].selectionType = TSelectionType.clStoichiometry;
//                    selectionList[i].index = index;
//                    selectionList[i].p1 = (string) newSelectionList[i];
//                }
//
//            }
//        }
//
//
//        [Help(
//            "Carry out a single integration step using a stepsize as indicated in the method call (the intergrator is reset to take into account all variable changes). Arguments: double CurrentTime, double StepSize, Return Value: new CurrentTime."
//            )]
//        public double oneStep(double currentTime, double stepSize)
//        {
//            return oneStep(currentTime, stepSize, true);
//        }
//
//        [Help(
//           "Carry out a single integration step using a stepsize as indicated in the method call. Arguments: double CurrentTime, double StepSize, bool: reset integrator if true, Return Value: new CurrentTime."
//           )]
//        public double oneStep(double currentTime, double stepSize, bool reset)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if (reset)
//                cvode.reStart(currentTime, model);
//            return cvode.OneStep(currentTime, stepSize);
//        }
//
//
//        // ---------------------------------------------------------------------
//        // Start of Level 3 API Methods
//        // ---------------------------------------------------------------------
//
//        /*[Help("Compute the steady state of the model, returns the sum of squares of the solution")]
//        public double steadyState () {
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
//        //public static void TestSettings()
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
//        [Help("Compute the steady state of the model, returns the sum of squares of the solution")]
//        public double steadyState()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            try
//            {
//                if (UseKinsol == 0)
//                    steadyStateSolver = new NLEQInterface(model);
//                else
//                    steadyStateSolver = new KinSolveInterface(model);
//                //oneStep(0.0,0.05);
//                double ss = steadyStateSolver.solve(model.amounts);
//                model.convertToConcentrations();
//                return ss;
//            }
//            catch (SBWException)
//            {
//                throw;
//            }
//            catch (Exception e)
//            {
//                throw new SBWApplicationException("Unexpected error from steadyState solver:", e.Message);
//            }
//        }
//
//
//        // ******************************************************************** }
//        // Multiply matrix 'm1' by 'm2' to give result in Self                  }
//        //                                                                      }
//        // Usage:  A.mult (A1, A2); multiply A1 by A2 giving A                  }
//        //                                                                      }
//        // ******************************************************************** }
//        [Ignore()]
//        public double[][] mult(double[][] m1, double[][] m2)
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
//        [Help("Compute the reduced Jacobian at the current operating point")]
//        public double[][] getReducedJacobian()
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
//        [Help("Compute the full Jacobian at the current operating point")]
//        public double[][] getFullJacobian()
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
//        [Help("Returns the Link Matrix for the currently loaded model")]
//        public double[][] getLinkMatrix()
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
//        [Help("Returns the reduced stoichiometry matrix (Nr) for the currently loaded model")]
//        public double[][] getNrMatrix()
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
//        [Help("Returns the L0 matrix for the currently loaded model")]
//        public double[][] getL0Matrix()
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
//        [Help("Returns the stoichiometry matrix for the currently loaded model")]
//        public double[][] getStoichiometryMatrix()
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
//        [Help("Returns the conservation matrix (gamma) for the currently loaded model")]
//        public double[][] getConservationMatrix()
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
//        [Help("Returns the number of dependent species in the model")]
//        public int getNumberOfDependentSpecies()
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
//        [Help("Returns the number of independent species in the model")]
//        public int getNumberOfIndependentSpecies()
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
//        [Ignore]
//        private double getVariableValue(TVariableType variableType, int variableIndex)
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
//        [Ignore]
//        private void setParameterValue(TParameterType parameterType, int parameterIndex, double value)
//        {
//            switch (parameterType)
//            {
//                case TParameterType.ptBoundaryParameter:
//                    model.bc[parameterIndex] = value;
//                    break;
//
//                case TParameterType.ptGlobalParameter:
//                    model.gp[parameterIndex] = value;
//                    break;
//
//                case TParameterType.ptFloatingSpecies:
//                    model.y[parameterIndex] = value;
//                    break;
//
//                case TParameterType.ptConservationParameter:
//                    model.ct[parameterIndex] = value;
//                    break;
//
//                case TParameterType.ptLocalParameter:
//                    throw new SBWException("Local parameters not permitted in setParameterValue (getCC, getEE)");
//            }
//        }
//
//        [Ignore]
//        private double getParameterValue(TParameterType parameterType, int parameterIndex)
//        {
//            switch (parameterType)
//            {
//                case TParameterType.ptBoundaryParameter:
//                    return model.bc[parameterIndex];
//
//                case TParameterType.ptGlobalParameter:
//                    return model.gp[parameterIndex];
//
//                // Used when calculating elasticities
//                case TParameterType.ptFloatingSpecies:
//                    return model.y[parameterIndex];
//                case TParameterType.ptConservationParameter:
//                    return model.ct[parameterIndex];
//                case TParameterType.ptLocalParameter:
//                    throw new SBWException("Local parameters not permitted in getParameterValue (getCC?)");
//
//                default:
//                    return 0.0;
//            }
//        }
//
//        /// <summary>
//        /// Fills the second argument with the Inverse of the first argument
//        /// </summary>
//        /// <param name="T2">The Matrix to calculate the Inverse for</param>
//        /// <param name="Inv">will be overriden wiht the inverse of T2 (must already be allocated)</param>
//        private static void GetInverse(Matrix T2, Matrix Inv)
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
//        [Help(
//            "Derpar Continuation, stepSize = stepsize; independentVariable = index to parameter; parameterType = {'globalParameter', 'boundarySpecies'"
//            )]
//        public void computeContinuation(double stepSize, int independentVariable, string parameterTypeStr)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            var derpar = new TDerpar(this, model.getNumTotalVariables, model.getNumIndependentVariables);
//            derpar.setup(model.amounts);
//            switch (parameterTypeStr)
//            {
//                case "globalParameter":
//                    model.amounts =
//                        (double[])
//                        derpar.evalOneStep(model.amounts, stepSize, independentVariable, TDerpar.GLOBAL_PARAMETER_TYPE).
//                            Clone();
//                    break;
//                case "boundarySpecies":
//                    model.amounts =
//                        (double[])
//                        derpar.evalOneStep(model.amounts, stepSize, independentVariable, TDerpar.BOUNDARY_SPECIES_TYPE).
//                            Clone();
//                    break;
//            }
//        }
//
//        [Help("Returns the Symbols of all Flux Control Coefficients.")]
//        public ArrayList getFluxControlCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oReactions = getReactionNames();
//            ArrayList oParameters = ModelGenerator.Instance.getGlobalParameterList();
//            ArrayList oBoundary = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList oConservation = ModelGenerator.Instance.getConservationList();
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
//        [Help("Returns the Symbols of all Concentration Control Coefficients.")]
//        public ArrayList getConcentrationControlCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oFloating = getFloatingSpeciesNames();
//            ArrayList oParameters = ModelGenerator.Instance.getGlobalParameterList();
//            ArrayList oBoundary = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList oConservation = ModelGenerator.Instance.getConservationList();
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
//        [Help("Returns the Symbols of all Unscaled Concentration Control Coefficients.")]
//        public ArrayList getUnscaledConcentrationControlCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oFloating = getFloatingSpeciesNames();
//            ArrayList oParameters = ModelGenerator.Instance.getGlobalParameterList();
//            ArrayList oBoundary = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList oConservation = ModelGenerator.Instance.getConservationList();
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
//        [Help("Returns the Symbols of all Elasticity Coefficients.")]
//        public ArrayList getElasticityCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList reactionNames = getReactionNames();
//            ArrayList floatingSpeciesNames = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            ArrayList boundarySpeciesNames = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList conservationNames = ModelGenerator.Instance.getConservationList();
//            ArrayList globalParameterNames = ModelGenerator.Instance.getGlobalParameterList();
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
//        [Help("Returns the Symbols of all Unscaled Elasticity Coefficients.")]
//        public ArrayList getUnscaledElasticityCoefficientNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oReactions = getReactionNames();
//            ArrayList oFloating = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            ArrayList oBoundary = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList oGlobalParameters = ModelGenerator.Instance.getGlobalParameterList();
//            ArrayList oConservation = ModelGenerator.Instance.getConservationList();
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
//        [Help("Returns the Symbols of all Floating Species Eigenvalues.")]
//        public ArrayList getEigenValueNames()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            ArrayList oFloating = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//
//            foreach (string s in oFloating)
//            {
//                oResult.Add("eigen_" + s);
//            }
//
//            return oResult;
//        }
//
//        [Help(
//            "Returns symbols of the currently loaded model, that can be used for steady state analysis. Format: array of arrays  { { \"groupname\", { \"item1\", \"item2\" ... } } }  or { { \"groupname\", { \"subgroup\", { \"item1\" ... } } } }."
//            )]
//        public ArrayList getAvailableSteadyStateSymbols()
//        {
//            var oResult = new ArrayList();
//            if (!modelLoaded) return oResult;
//
//            oResult.Add(new ArrayList(new object[] { "Floating Species", getFloatingSpeciesNames() }));
//            oResult.Add(new ArrayList(new object[] { "Boundary Species", getBoundarySpeciesNames() }));
//            oResult.Add(new ArrayList(new object[] { "Floating Species (amount)", getFloatingSpeciesAmountNames() }));
//            oResult.Add(new ArrayList(new object[] { "Boundary Species (amount)", getBoundarySpeciesAmountNames() }));
//            oResult.Add(new ArrayList(new object[] { "Global Parameters", getParameterNames() }));
//            oResult.Add(new ArrayList(new object[] { "Volumes", ModelGenerator.Instance.getCompartmentList() }));
//            oResult.Add(new ArrayList(new object[] { "Fluxes", getReactionNames() }));
//            oResult.Add(new ArrayList(new object[] { "Flux Control Coefficients", getFluxControlCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[] { "Concentration Control Coefficients", getConcentrationControlCoefficientNames() }));
//            oResult.Add(
//                new ArrayList(new object[]
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
//        [Help("Returns the selection list as returned by computeSteadyStateValues().")]
//        public ArrayList getSteadyStateSelectionList()
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
//                                                        selectionType = TSelectionType.clFloatingSpecies,
//                                                        p1 = (string) floatingSpecies[i],
//                                                        index = i
//                                                    };
//                }
//            }
//
//            ArrayList oFloating = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            ArrayList oBoundary = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList oFluxes = ModelGenerator.Instance.getReactionNames();
//            ArrayList oVolumes = ModelGenerator.Instance.getCompartmentList();
//            ArrayList oRates = getRateOfChangeNames();
//            ArrayList oParameters = getParameterNames();
//
//            var result = new ArrayList();
//            foreach (var record in _oSteadyStateSelection)
//            {
//                switch (record.selectionType)
//                {
//                    case TSelectionType.clTime:
//                        result.Add("time");
//                        break;
//                    case TSelectionType.clBoundaryAmount:
//                        result.Add(string.Format("[{0}]", oBoundary[record.index]));
//                        break;
//                    case TSelectionType.clBoundarySpecies:
//                        result.Add(oBoundary[record.index]);
//                        break;
//                    case TSelectionType.clFloatingAmount:
//                        result.Add("[" + (string)oFloating[record.index] + "]");
//                        break;
//                    case TSelectionType.clFloatingSpecies:
//                        result.Add(oFloating[record.index]);
//                        break;
//                    case TSelectionType.clVolume:
//                        result.Add(oVolumes[record.index]);
//                        break;
//                    case TSelectionType.clFlux:
//                        result.Add(oFluxes[record.index]);
//                        break;
//                    case TSelectionType.clRateOfChange:
//                        result.Add(oRates[record.index]);
//                        break;
//                    case TSelectionType.clParameter:
//                        result.Add(oParameters[record.index]);
//                        break;
//                    case TSelectionType.clEigenValue:
//                        result.Add("eigen_" + record.p1);
//                        break;
//                    case TSelectionType.clElasticity:
//                        result.Add("EE:" + record.p1 + "," + record.p2);
//                        break;
//                    case TSelectionType.clUnscaledElasticity:
//                        result.Add("uEE:" + record.p1 + "," + record.p2);
//                        break;
//                    case TSelectionType.clUnknown:
//                        result.Add(record.p1);
//                        break;
//                }
//
//            }
//
//            return result ;
//        }
//
//        private TSelectionRecord[] GetSteadyStateSelection(ArrayList newSelectionList)
//        {
//            var steadyStateSelection = new TSelectionRecord[newSelectionList.Count];
//            ArrayList fs = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            ArrayList bs = ModelGenerator.Instance.getBoundarySpeciesList();
//            ArrayList rs = ModelGenerator.Instance.getReactionNames();
//            ArrayList vol = ModelGenerator.Instance.getCompartmentList();
//            ArrayList gp = ModelGenerator.Instance.getGlobalParameterList();
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
//                        steadyStateSelection[i].selectionType = TSelectionType.clFloatingSpecies;
//                        set = true;
//                        break;
//                    }
//
//                    if ((string)newSelectionList[i] == "[" + (string)fs[j] + "]")
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType.clFloatingAmount;
//                        set = true;
//                        break;
//                    }
//
//                    // Check for species rate of change
//                    if ((string)newSelectionList[i] == (string)fs[j] + "'")
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType.clRateOfChange;
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
//                        steadyStateSelection[i].selectionType = TSelectionType.clBoundarySpecies;
//                        set = true;
//                        break;
//                    }
//                    if ((string)newSelectionList[i] == "[" + (string)bs[j] + "]")
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType.clBoundaryAmount;
//                        set = true;
//                        break;
//                    }
//                }
//
//                if (set) continue;
//
//                if ((string)newSelectionList[i] == "time")
//                {
//                    steadyStateSelection[i].selectionType = TSelectionType.clTime;
//                    set = true;
//                }
//
//                for (int j = 0; j < rs.Count; j++)
//                {
//                    // Check for reaction rate
//                    if ((string)newSelectionList[i] == (string)rs[j])
//                    {
//                        steadyStateSelection[i].index = j;
//                        steadyStateSelection[i].selectionType = TSelectionType.clFlux;
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
//                        steadyStateSelection[i].selectionType = TSelectionType.clVolume;
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
//                        steadyStateSelection[i].selectionType = TSelectionType.clParameter;
//                        set = true;
//                        break;
//                    }
//                }
//
//                if (set) continue;
//
//                // it is another symbol
//                steadyStateSelection[i].selectionType = TSelectionType.clUnknown;
//                steadyStateSelection[i].p1 = (string)newSelectionList[i];
//            }
//            return steadyStateSelection;
//        }
//
//        [Help("sets the selection list as returned by computeSteadyStateValues().")]
//        public void setSteadyStateSelectionList(ArrayList newSelectionList)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            TSelectionRecord[] steadyStateSelection = GetSteadyStateSelection(newSelectionList);
//
//            _oSteadyStateSelection = steadyStateSelection;
//
//        }
//
//        [Help("performs steady state analysis, returning values as given by setSteadyStateSelectionList().")]
//        public double[] computeSteadyStateValues()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return computeSteadyStateValues(_oSteadyStateSelection, true);
//        }
//
//        private double[] computeSteadyStateValues(TSelectionRecord[] oSelection, bool computeSteadyState)
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
//        [Help("performs steady state analysis, returning values as specified by the given selection list.")]
//        public double[] computeSteadyStateValues(ArrayList oSelection)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            var selection = GetSteadyStateSelection(oSelection);
//            return computeSteadyStateValues(selection, true);
//        }
//
//        private double computeSteadyStateValue(TSelectionRecord record)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if (record.selectionType == TSelectionType.clUnknown)
//                return computeSteadyStateValue(record.p1);
//            return GetValueForRecord(record);
//        }
//
//        [Help("Returns the value of the given steady state identifier.")]
//        public double computeSteadyStateValue(string sId)
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
//                    if (ModelGenerator.Instance.floatingSpeciesConcentrationList.find(sSpecies, out nIndex))
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
//        [Help("Returns the values selected with setSelectionList() for the current model time / timestep")]
//        public double[] getSelectedValues()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            var result = new double[selectionList.Length];
//
//            for (int j = 0; j < selectionList.Length; j++)
//            {
//                result[j] = GetNthSelectedOutput(j, model.time);
//            }
//            return result;
//        }
//
//        [Help("Returns any warnings that occured during the loading of the SBML")]
//        public string[] getWarnings()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return model.Warnings.ToArray();
//        }
//
//        [Help("When turned on, this method will cause rates, event assignments, rules and such to be multiplied " +
//              "with the compartment volume, if species are defined as initialAmounts. By default this behavior is off.")
//        ]
//        public static void ReMultiplyCompartments(bool bValue)
//        {
//            _ReMultiplyCompartments = bValue;
//        }
//
//        [Help("This method turns on / off the computation and adherence to conservation laws."
//              + "By default roadRunner will discover conservation cycles and reduce the model accordingly.")]
//        public static void ComputeAndAssignConservationLaws(bool bValue)
//        {
//            _bComputeAndAssignConservationLaws = bValue;
//        }
//
//        [Help("Returns the current generated source code")]
//        public string getCSharpCode()
//        {
//            if (modelLoaded)
//            {
//                return _sModelCode;
//            }
//
//            throw new SBWApplicationException("Model has to be loaded first");
//        }
//
//        [Help(
//            "Performs a steady state parameter scan with the given parameters returning all elments from the selectionList: (Format: symnbol, startValue, endValue, stepSize)"
//            )]
//        public double[][] steadyStateParameterScan(string symbol, double startValue, double endValue, double stepSize)
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
//        [Help("Returns the SBML with the current parameterset")]
//        public string writeSBML()
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
//        #region Get Local Parameter Names / Values
//
//        // -----------------------------------------------------------------
//
//        [Help("Get the number of local parameters for a given reaction")]
//        public int getNumberOfLocalParameters(int reactionId)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return getNumberOfLocalParameters(reactionId);
//        }
//
//        [Help("Sets the value of a global parameter by its index")]
//        public void setLocalParameterByIndex(int reactionId, int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < model.getNumReactions) &&
//                (index >= 0) && (index < model.getNumLocalParameters(reactionId)))
//                model.lp[reactionId][index] = value;
//            else
//                throw new SBWApplicationException(string.Format("Index in setLocalParameterByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Returns the value of a global parameter by its index")]
//        public double getLocalParameterByIndex(int reactionId, int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < model.getNumReactions) &&
//                (index >= 0) && (index < model.getNumLocalParameters(reactionId)))
//                return model.lp[reactionId][index];
//
//            throw new SBWApplicationException(String.Format("Index in getLocalParameterByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Set the values for all global parameters in the model")]
//        public void setLocalParameterValues(int reactionId, double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//
//            if ((reactionId >= 0) && (reactionId < model.getNumReactions))
//                model.lp[reactionId] = values;
//            else
//                throw new SBWApplicationException(String.Format("Index in setLocalParameterValues out of range: [{0}]", reactionId));
//        }
//
//        [Help("Get the values for all global parameters in the model")]
//        public double[] getLocalParameterValues(int reactionId)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < model.getNumReactions))
//                return model.lp[reactionId];
//            throw new SBWApplicationException(String.Format("Index in getLocalParameterValues out of range: [{0}]", reactionId));
//        }
//
//        [Help("Gets the list of parameter names")]
//        public ArrayList getLocalParameterNames(int reactionId)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((reactionId >= 0) && (reactionId < model.getNumReactions))
//                return ModelGenerator.Instance.getLocalParameterList(reactionId);
//            throw (new SBWApplicationException("reaction Id out of range in call to getLocalParameterNames"));
//        }
//
//        [Help("Returns a list of global parameter tuples: { {parameter Name, value},...")]
//        public ArrayList getAllLocalParameterTupleList()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            var tupleList = new ArrayList();
//            for (int i = 0; i < ModelGenerator.Instance.getNumberOfReactions(); i++)
//            {
//                var tuple = new ArrayList();
//                ArrayList lpList = ModelGenerator.Instance.getLocalParameterList(i);
//                tuple.Add(i);
//                for (int j = 0; j < lpList.Count; j++)
//                {
//                    tuple.Add(lpList[j]);
//                    tuple.Add(model.lp[i][j]);
//                }
//                tupleList.Add(tuple);
//            }
//            return tupleList;
//        }
//
//        #endregion
//
//        #region Get Reaction Rate / Names ...
//
//        // -----------------------------------------------------------------
//
//        [Help("Get the number of reactions")]
//        public int getNumberOfReactions()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return model.getNumReactions;
//        }
//
//        [Help("Returns the rate of a reaction by its index")]
//        public double getReactionRate(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < model.getNumReactions))
//            {
//                model.convertToConcentrations();
//                model.computeReactionRates(0.0, model.y);
//                return model.rates[index];
//            }
//            throw new SBWApplicationException(String.Format("Index in getReactionRate out of range: [{0}]", index));
//        }
//
//        [Help("Returns the rate of changes of a species by its index")]
//        public double getRateOfChange(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < model.getNumTotalVariables))
//            {
//                model.computeAllRatesOfChange();
//                return model.dydt[index];
//            }
//            throw new SBWApplicationException(String.Format("Index in getRateOfChange out of range: [{0}]", index));
//        }
//
//        [Help("Returns the names given to the rate of change of the floating species")]
//        public ArrayList getRateOfChangeNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            ArrayList sp = ModelGenerator.Instance.getFloatingSpeciesConcentrationList(); // Reordered list
//            for (int i = 0; i < sp.Count; i++)
//                sp[i] = sp[i] + "'";
//            return sp;
//        }
//
//        [Help("Returns the rates of changes given an array of new floating species concentrations")]
//        public double[] getRatesOfChangeEx(double[] values)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            model.y = values;
//            model.evalModel(0.0, BuildModelEvalArgument());
//            return model.dydt;
//        }
//
//        [Help("Returns the rates of changes given an array of new floating species concentrations")]
//        public double[] getReactionRatesEx(double[] values)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            model.computeReactionRates(0.0, values);
//            return model.rates;
//        }
//
//
//        public string[] GetFloatingSpeciesNamesArray()
//        {
//            return (string[])getFloatingSpeciesNames().ToArray(typeof(string));
//        }
//
//        public string[] GetGlobalParameterNamesArray()
//        {
//            return (string[])getGlobalParameterNames().ToArray(typeof(string));
//        }
//
//        #endregion
//
//        #region Get Compartment Names / Values
//
//        [Help("Get the number of compartments")]
//        public int getNumberOfCompartments()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return model.getNumCompartments;
//        }
//
//        [Help("Sets the value of a compartment by its index")]
//        public void setCompartmentByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < model.getNumCompartments))
//                model.c[index] = value;
//            else
//                throw new SBWApplicationException(String.Format("Index in getCompartmentByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Returns the value of a compartment by its index")]
//        public double getCompartmentByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < model.getNumCompartments))
//                return model.c[index];
//            throw (new SBWApplicationException(String.Format("Index in getCompartmentByIndex out of range: [{0}]", index)));
//        }
//
//        [Help("Returns the value of a compartment by its index")]
//        public void setCompartmentVolumes(double[] values)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if (values.Length < model.getNumCompartments)
//                model.c = values;
//            else
//                throw (new SBWApplicationException(String.Format("Size of vector out not in range in setCompartmentValues: [{0}]", values.Length)));
//        }
//
//        [Help("Gets the list of compartment names")]
StringList RoadRunner::getCompartmentNames()
{
    if (!modelLoaded)
    {
        throw SBWApplicationException(emptyModelStr);
    }
    return mModelGenerator->getCompartmentList();
}

//        #endregion
//
//        #region Get Boundary Species Names / Values
//
//        // -----------------------------------------------------------------
//
//        [Help("Get the number of boundary species")]
//        public int getNumberOfBoundarySpecies()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return model.getNumBoundarySpecies;
//        }
//
//        [Help("Sets the value of a boundary species by its index")]
//        public void setBoundarySpeciesByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < model.getNumBoundarySpecies))
//                model.bc[index] = value;
//            else
//                throw new SBWApplicationException(String.Format("Index in getBoundarySpeciesByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Returns the value of a boundary species by its index")]
//        public double getBoundarySpeciesByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < model.getNumBoundarySpecies))
//                return model.bc[index];
//            throw new SBWApplicationException(String.Format("Index in getBoundarySpeciesByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Returns an array of boundary species concentrations")]
//        public double[] getBoundarySpeciesConcentrations()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return model.bc;
//        }
//
//        [Help("Set the concentrations for all boundary species in the model")]
//        public void setBoundarySpeciesConcentrations(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            model.bc = values;
//        }
//
//        [Help("Gets the list of boundary species names")]
//        public ArrayList getBoundarySpeciesNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return ModelGenerator.Instance.getBoundarySpeciesList();
//        }
//
//        [Help("Gets the list of boundary species amount names")]
//        public ArrayList getBoundarySpeciesAmountNames()
//        {
//            var oResult = new ArrayList();
//            foreach (string s in getBoundarySpeciesNames()) oResult.Add("[" + s + "]");
//            return oResult;
//        }
//
//        #endregion
//
//        #region Get Floating Species Names / Values
//
//        // -----------------------------------------------------------------
//
//        [Help("Get the number of floating species")]
//        public int getNumberOfFloatingSpecies()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return model.getNumTotalVariables;
//        }
//
//        [Help("Sets the value of a floating species by its index")]
//        public void setFloatingSpeciesByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < model.getNumTotalVariables))
//            {
//                model.setConcentration(index, value); // This updates the amount vector aswell
//                if (!_bConservedTotalChanged) model.computeConservedTotals();
//            }
//            else
//                throw new SBWApplicationException(String.Format("Index in setFloatingSpeciesByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Returns the value of a floating species by its index")]
//        public double getFloatingSpeciesByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < model.getNumTotalVariables))
//                return model.getConcentration(index);
//            throw new SBWApplicationException(String.Format("Index in getFloatingSpeciesByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Returns an array of floating species concentrations")]
//        public double[] getFloatingSpeciesConcentrations()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            model.convertToConcentrations();
//            return model.y;
//        }
//
//        [Help("returns an array of floating species initial conditions")]
//        public double[] getFloatingSpeciesInitialConcentrations()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return model.init_y;
//        }
//
//
//        // This is a level 1 Method 1
//        [Help("Set the concentrations for all floating species in the model")]
//        public void setFloatingSpeciesConcentrations(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            model.y = values;
//            // Update the amounts vector at the same time
//            model.convertToAmounts();
//            if (!_bConservedTotalChanged) model.computeConservedTotals();
//        }
//
//        [Help("Sets the value of a floating species by its index")]
//        public void setFloatingSpeciesInitialConcentrationByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < model.init_y.Length))
//            {
//                model.init_y[index] = value;
//                reset();
//            }
//            else
//                throw new SBWApplicationException(String.Format("Index in setFloatingSpeciesInitialConcentrationByIndex out of range: [{0}]", index));
//        }
//
//        [Help("Sets the initial conditions for all floating species in the model")]
//        public void setFloatingSpeciesInitialConcentrations(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//
//            model.init_y = values;
//            reset();
//        }
//
//
//        // This is a Level 1 method !
//        [Help("Returns a list of floating species names")]
//        public ArrayList getFloatingSpeciesNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            return ModelGenerator.Instance.getFloatingSpeciesConcentrationList(); // Reordered list
//        }
//
//        [Help("Returns a list of floating species initial condition names")]
//        public ArrayList getFloatingSpeciesInitialConditionNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            ArrayList floatingSpeciesNames = ModelGenerator.Instance.getFloatingSpeciesConcentrationList();
//            var result = new ArrayList();
//            foreach (object item in floatingSpeciesNames)
//            {
//                result.Add(String.Format("init({0})", item));
//            }
//            return result;
//        }
//
//
//        [Help("Returns the list of floating species amount names")]
//        public ArrayList getFloatingSpeciesAmountNames()
//        {
//            var oResult = new ArrayList();
//            foreach (string s in getFloatingSpeciesNames()) oResult.Add(String.Format("[{0}]", s));
//            return oResult;
//        }
//
//        #endregion
//
//        #region Get Global Parameter  Names / Values
//
//        // -----------------------------------------------------------------
//
//        [Help("Get the number of global parameters")]
//        public int getNumberOfGlobalParameters()
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            return ModelGenerator.Instance.getGlobalParameterList().Count;
//        }
//
//        [Help("Sets the value of a global parameter by its index")]
//        public void setGlobalParameterByIndex(int index, double value)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if ((index >= 0) && (index < model.getNumGlobalParameters + model.ct.Length))
//            {
//                if (index >= model.getNumGlobalParameters)
//                {
//                    model.ct[index - model.getNumGlobalParameters] = value;
//                    model.updateDependentSpeciesValues(model.y);
//                    _bConservedTotalChanged = true;
//                }
//                else
//                    model.gp[index] = value;
//            }
//            else
//                throw new SBWApplicationException(String.Format("Index in getNumGlobalParameters out of range: [{0}]", index));
//        }
//
//        [Help("Returns the value of a global parameter by its index")]
//        public double getGlobalParameterByIndex(int index)
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            if ((index >= 0) && (index < (model.getNumGlobalParameters + model.ct.Length)))
//            {
//                var result = new double[model.gp.Length + model.ct.Length];
//                model.gp.CopyTo(result, 0);
//                model.ct.CopyTo(result, model.gp.Length);
//                return result[index];
//                //return model.gp[index];
//            }
//            throw new SBWApplicationException(String.Format("Index in getNumGlobalParameters out of range: [{0}]", index));
//        }
//
//        [Help("Set the values for all global parameters in the model")]
//        public void setGlobalParameterValues(double[] values)
//        {
//            if (!modelLoaded) throw new SBWApplicationException(emptyModelStr);
//            if (values.Length == model.gp.Length)
//                model.gp = values;
//            else
//            {
//                for (int i = 0; i < model.gp.Length; i++)
//                {
//                    model.gp[i] = values[i];
//                }
//                for (int i = 0; i < model.ct.Length; i++)
//                {
//                    model.gp[i] = values[i + model.gp.Length];
//                    _bConservedTotalChanged = true;
//                }
//                model.updateDependentSpeciesValues(model.y);
//            }
//        }
//
//        [Help("Get the values for all global parameters in the model")]
//        public double[] getGlobalParameterValues()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            if (model.ct.Length > 0)
//            {
//                var result = new double[model.gp.Length + model.ct.Length];
//                model.gp.CopyTo(result, 0);
//                model.ct.CopyTo(result, model.gp.Length);
//                return result;
//            }
//            return model.gp;
//        }
//
//        [Help("Gets the list of parameter names")]
//        public ArrayList getGlobalParameterNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            return ModelGenerator.Instance.getGlobalParameterList();
//        }
//
//        [Help("Returns a list of global parameter tuples: { {parameter Name, value},...")]
//        public ArrayList getAllGlobalParameterTupleList()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//
//            var tupleList = new ArrayList();
//            ArrayList gp = ModelGenerator.Instance.getGlobalParameterList();
//            for (int i = 0; i < gp.Count; i++)
//            {
//                var tuple = new ArrayList {gp[i], model.gp[i]};
//                tupleList.Add(tuple);
//            }
//            return tupleList;
//        }
//
//        private ArrayList getParameterNames()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            ArrayList sp = ModelGenerator.Instance.getGlobalParameterList(); // Reordered list
//            return sp;
//        }
//
//        [Help("Updates the model based on all recent changes")]
//        public void EvalModel()
//        {
//            if (!modelLoaded)
//                throw new SBWApplicationException(emptyModelStr);
//            model.convertToAmounts();
//            model.evalModel(model.time, cvode.BuildEvalArgument());
//        }
//
//        #endregion
//
//        #region Information about Roadrunner: getName
//
//        [Help("Returns the name of module")]
//        public string getName()
//        {
//            return "roadRunner";
//        }
//
//        [Help("Returns the version number of the module")]
//        public static string getVersion()
//        {
//            return "2.0.1";
//        }
//
//        [Help("Returns the name of the module author")]
//        public static string getAuthor()
//        {
//            return "H. M. Sauro and F. T. Bergmann";
//        }
//
//        [Help("Returns a description of the module")]
//        public static string getDescription()
//        {
//            return "Simulator API based on CVODE/NLEQ/CSharp implementation";
//        }
//
//        [Help("Returns the display name of the module")]
//        public static string getDisplayName()
//        {
//            return "RoadRunner";
//        }
//
//        [Help("Returns the copyright string for the module")]
        string RoadRunner::getCopyright()
        {
            return "(c) 2009 H. M. Sauro and F. T. Bergmann, BSD Licence";
        }
//
//        [Help("Returns the URL string associated with the module (if any)")]
//        public static string getURL()
//        {
//            return "http://sys-bio.org";
//        }
//
//        #endregion
//
//        #region Nested type: TSelectionRecord
//
//        public struct TSelectionRecord
//        {
//            public int index;
//            public string p1;
//            public string p2;
//
//            public TSelectionType selectionType;
//        }
//
//        #endregion
//
//
// #if DEBUG
//       public static void TestChange()
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

}//namespace rr