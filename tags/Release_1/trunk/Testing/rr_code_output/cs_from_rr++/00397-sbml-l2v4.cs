//************************************************************************** 
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using LibRoadRunner;
 


class TModel : IModel
{
	// Symbol Mappings


	private List<string> _Warnings = new List<string>();
	private double[] _gp = new double[5];           // Vector containing all the global parameters in the System  
	private double[] _sr = new double[0];           // Vector containing all the modifiable species references  
	private double[][] _lp = new double[0][];       // Vector containing all the local parameters in the System  
	private double[] _y = new double[0];            // Vector containing the concentrations of all floating species 
	private double[] _init_y = new double[0];            // Vector containing the initial concentrations of all floating species 
	private double[] _amounts = new double[0];      // Vector containing the amounts of all floating species 
	private double[] _bc = new double[0];           // Vector containing all the boundary species concentration values   
	private double[] _c = new double[0];            // Vector containing all the compartment values   
	private double[] _dydt = new double[0];         // Vector containing rates of changes of all species   
	private double[] _rates = new double[0];        // Vector containing the rate laws of all reactions    
	private double[] _ct = new double[0];           // Vector containing values of all conserved sums      
	private double[] _eventTests = new double[2];   // Vector containing results of any event tests        
	private TEventDelayDelegate[] _eventDelay = new TEventDelayDelegate[2]; // array of trigger function pointers
	private bool[] _eventType = new bool[2]; // array holding the status whether events are useValuesFromTriggerTime or not
	private bool[] _eventPersistentType = new bool[2]; // array holding the status whether events are persitstent or not
	private double _time;
	private int numIndependentVariables;
	private int numDependentVariables;
	private int numTotalVariables;
	private int numBoundaryVariables;
	private int numGlobalParameters;
	private int numCompartments;
	private int numReactions;
	private int numRules;
	private int numEvents;
	string[] variableTable = new string[0];
	string[] boundaryTable = new string[0];
	string[] globalParameterTable = new string[5];
	int[] localParameterDimensions = new int[0];
	private TEventAssignmentDelegate[] _eventAssignments;
	private double[] _eventPriorities;
	private TComputeEventAssignmentDelegate[] _computeEventAssignments;
	private TPerformEventAssignmentDelegate[] _performEventAssignments;
	private bool[] _eventStatusArray = new bool[2];
	private bool[] _previousEventStatusArray = new bool[2];

	public TModel ()  
	{
		numIndependentVariables = 0;
		numDependentVariables = 0;
		numTotalVariables = 0;
		numBoundaryVariables = 0;
		numGlobalParameters = 5;
		numCompartments = 0;
		numReactions = 0;
		numEvents = 2;
		InitializeDelays();
		_eventAssignments = new TEventAssignmentDelegate[numEvents];
		_eventPriorities = new double[numEvents];
		_computeEventAssignments= new TComputeEventAssignmentDelegate[numEvents];
		_performEventAssignments= new TPerformEventAssignmentDelegate[numEvents];
		_eventAssignments[0] = new TEventAssignmentDelegate (eventAssignment_0);
		_computeEventAssignments[0] = new TComputeEventAssignmentDelegate (computeEventAssignment_0);
		_performEventAssignments[0] = new TPerformEventAssignmentDelegate (performEventAssignment_0);
		_eventAssignments[1] = new TEventAssignmentDelegate (eventAssignment_1);
		_computeEventAssignments[1] = new TComputeEventAssignmentDelegate (computeEventAssignment_1);
		_performEventAssignments[1] = new TPerformEventAssignmentDelegate (performEventAssignment_1);
		resetEvents();

	}

	void loadSymbolTables() {
		globalParameterTable[0] = "S1";
		globalParameterTable[1] = "S2";
		globalParameterTable[2] = "S3";
		globalParameterTable[3] = "k1";
		globalParameterTable[4] = "k2";
	}


	public void resetEvents() {
		_eventStatusArray[0] = false;
		_previousEventStatusArray[0] = false;
		_eventStatusArray[1] = false;
		_previousEventStatusArray[1] = false;
	}

	public void setConcentration(int index, double value) {
		double volume = 0.0;
		_y[index] = value;
		switch (index) {
		}
		_amounts[index] = _y[index]*volume;
	}

	public double getConcentration(int index) {
		return _y[index];
	}

	public void convertToAmounts() {
	}

	public void convertToConcentrations() {
	}

	public double[] y {
		get { return _y; } 
		set { _y = value; } 
	}

	public double[] init_y {
		get { return _init_y; } 
		set { _init_y = value; } 
	}

	public double[] amounts {
		get { return _amounts; } 
		set { _amounts = value; } 
	}

	public double[] bc {
		get { return _bc; } 
		set { _bc = value; } 
	}

	public double[] gp {
		get { return _gp; } 
		set { _gp = value; } 
	}

	public double[] sr {
		get { return _sr; } 
		set { _sr = value; } 
	}

	public double[][] lp {
		get { return _lp; } 
		set { _lp = value; } 
	}

	public double[] c {
		get { return _c; } 
		set { _c = value; } 
	}

	public double[] dydt {
		get { return _dydt; }
		set { _dydt = value; }
	}

	public double[] rateRules {
		get { return _rateRules; }
		set { _rateRules = value; }
	}

	public double[] rates {
		get { return _rates; }
		set { _rates = value; }
	}

	public double[] ct {
		get { return _ct; }
		set { _ct = value; }
	}

	public double[] eventTests {
		get { return _eventTests; }
		set { _eventTests = value; }
	}

	public TEventDelayDelegate[] eventDelay {
		get { return _eventDelay; }
		set { _eventDelay = value; }
	}

	public bool[] eventType {
		get { return _eventType; }
		set { _eventType = value; }
	}

	public bool[] eventPersistentType {
		get { return _eventPersistentType; }
		set { _eventPersistentType = value; }
	}

	public bool[] eventStatusArray {
		get { return _eventStatusArray; }
		set { _eventStatusArray = value; }
	}

	public bool[] previousEventStatusArray {
		get { return _previousEventStatusArray; }
		set { _previousEventStatusArray = value; }
	}

	public double[] eventPriorities {
		get { return _eventPriorities; }
		set { _eventPriorities = value; }
	}

	public TEventAssignmentDelegate[] eventAssignments {
		get { return _eventAssignments; }
		set { _eventAssignments = value; }
	}

	public TComputeEventAssignmentDelegate[] computeEventAssignments {
		get { return _computeEventAssignments; }
		set { _computeEventAssignments = value; }
	}

	public TPerformEventAssignmentDelegate[] performEventAssignments {
		get { return _performEventAssignments; }
		set { _performEventAssignments = value; }
	}

	public double time {
		get { return _time; }
		set { _time = value; }
	}

	public int getNumIndependentVariables {
		get { return numIndependentVariables; }
	}

	public int getNumDependentVariables {
		get { return numDependentVariables; }
	}

	public int getNumTotalVariables {
		get { return numTotalVariables; }
	}

	public int getNumBoundarySpecies {
		get { return numBoundaryVariables; }
	}

	public int getNumGlobalParameters {
		get { return numGlobalParameters; }
	}

	public int getNumLocalParameters(int reactionId)
	{
		return localParameterDimensions[reactionId];
	}

	public int getNumCompartments {
		get { return numCompartments; }
	}

	public int getNumReactions {
		get { return numReactions; }
	}

	public int getNumEvents {
		get { return numEvents; }
	}

	public int getNumRules {
		get { return numRules; }
	}

	public List<string> Warnings {
		get { return _Warnings; }
		set { _Warnings = value; }
	}

	public void initializeInitialConditions ()
	{

	}

	public void setInitialConditions ()
	{

	}

	public void setBoundaryConditions ()
	{
	}

	public void setCompartmentVolumes ()
	{
	}

	public void setParameterValues ()
	{
		_gp[0] = (double)1;
		_gp[1] = (double)2;
		_gp[2] = (double)1;
		_gp[3] = (double)0.75;
		_gp[4] = (double)0.25;
	}

	// Uses the equation: C = Sd - L0*Si
	public void computeConservedTotals ()
	{
	}

	// Compute values of dependent species 
	// Uses the equation: Sd = C + L0*Si
	public void updateDependentSpeciesValues (double[] y)
	{
	}

	public void computeRules(double[] y) {
		_rateRules[0] = _gp[4]*
	_gp[2]+
	-
	(double)1*
	_gp[3]*
	_gp[0]*
	_gp[1];
		_rateRules[1] = _gp[4]*
	_gp[2]+
	-
	(double)1*
	_gp[3]*
	_gp[0]*
	_gp[1];
		_rateRules[2] = _gp[3]*
	_gp[0]*
	_gp[1]+
	-
	(double)1*
	_gp[4]*
	_gp[2];
	}

	private double[] _rateRules = new double[3];           // Vector containing values of additional rate rules      
	public void InitializeRates()
	{
		_rateRules[0] = 		_gp[0];
		_rateRules[1] = 		_gp[1];
		_rateRules[2] = 		_gp[2];
	}

	public void AssignRates()
	{
		_gp[0] = _rateRules[0];
		_gp[1] = _rateRules[1];
		_gp[2] = _rateRules[2];
	}

	public void InitializeRateRuleSymbols()
	{
		_gp[0] = 1;
		_gp[1] = 2;
		_gp[2] = 1;
	}

	public void AssignRates(double[] oRates)
	{
		_gp[0] = oRates[0];
		_gp[1] = oRates[1];
		_gp[2] = oRates[2];
	}

	public double[] GetCurrentValues()
	{
		double[] dResult = new double[3];
		dResult[0] = 		_gp[0];
		dResult[1] = 		_gp[1];
		dResult[2] = 		_gp[2];
		return dResult;
	}

	// Uses the equation: dSd/dt = L0 dSi/dt
	public void computeAllRatesOfChange ()
	{
		double[] dTemp = new double[amounts.Length + rateRules.Length];
		dTemp[0] = 		_gp[0];
		dTemp[1] = 		_gp[1];
		dTemp[2] = 		_gp[2];
		amounts.CopyTo(dTemp, rateRules.Length);
		evalModel (time, dTemp);
	}

	// Compute the reaction rates
	public void computeReactionRates (double time, double[] y)
	{
	}

	// Model Function
	public void evalModel (double timein, double[] oAmounts)
	{
		_gp[0] = oAmounts[0];
		_gp[1] = oAmounts[1];
		_gp[2] = oAmounts[2];

		convertToAmounts();
		_time = timein;  // Don't remove
		updateDependentSpeciesValues (_y);
		computeRules (_y);
		computeReactionRates (time, _y);
		convertToAmounts ();
	}

	// Event handling function
	public void evalEvents (double timeIn, double[] oAmounts)
	{
		_gp[0] = oAmounts[0];
		_gp[1] = oAmounts[1];
		_gp[2] = oAmounts[2];
		_time = timeIn;  // Don't remove
		updateDependentSpeciesValues(_y);
		computeRules (_y);
		previousEventStatusArray[0] = eventStatusArray[0];
		if (supportFunctions._lt(_gp[0],(double)0.75)
	 == 1.0) {
		     eventStatusArray[0] = true;
		     eventTests[0] = 1;
		} else {
		     eventStatusArray[0] = false;
		     eventTests[0] = -1;
		}
		previousEventStatusArray[1] = eventStatusArray[1];
		if (supportFunctions._gt(_gp[2],(double)1.4)
	 == 1.0) {
		     eventStatusArray[1] = true;
		     eventTests[1] = 1;
		} else {
		     eventStatusArray[1] = false;
		     eventTests[1] = -1;
		}
	}

	// Event assignments
	public void eventAssignment_0 () {
		performEventAssignment_0( computeEventAssignment_0() );
	}
	public double[] computeEventAssignment_0 () {
		double[] values = new double[ 1];
		values[0] = (double)1;
		return values;
	}
	public void performEventAssignment_0 (double[] values) {
				_gp[1] = values[0];
	}
	public void eventAssignment_1 () {
		performEventAssignment_1( computeEventAssignment_1() );
	}
	public double[] computeEventAssignment_1 () {
		double[] values = new double[ 1];
		values[0] = (double)1;
		return values;
	}
	public void performEventAssignment_1 (double[] values) {
				_gp[0] = values[0];
	}
	


	private void InitializeDelays() { 
		_eventDelay[0] = new TEventDelayDelegate(delegate { return (double)0; } );
		_eventType[0] = true;
		_eventPersistentType[0] = true;
		_eventDelay[1] = new TEventDelayDelegate(delegate { return (double)0; } );
		_eventType[1] = true;
		_eventPersistentType[1] = true;
	}



	public void computeEventPriorites() { 
		_eventPriorities[0] = 0f;
		_eventPriorities[1] = 0f;
	}

	public void evalInitialAssignments()
	{
		_eventStatusArray[0] = true;
		_previousEventStatusArray[0] = true;
		_eventStatusArray[1] = true;
		_previousEventStatusArray[1] = true;
	}

	public void testConstraints()
	{
	}

}

