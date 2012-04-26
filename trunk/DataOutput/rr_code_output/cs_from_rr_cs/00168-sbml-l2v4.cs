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
	private double[] _gp = new double[7];           // Vector containing all the global parameters in the System  
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
	private double[] _eventTests = new double[0];   // Vector containing results of any event tests        
	private TEventDelayDelegate[] _eventDelay = new TEventDelayDelegate[0]; // array of trigger function pointers
	private bool[] _eventType = new bool[0]; // array holding the status whether events are useValuesFromTriggerTime or not
	private bool[] _eventPersistentType = new bool[0]; // array holding the status whether events are persitstent or not
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
	string[] globalParameterTable = new string[7];
	int[] localParameterDimensions = new int[0];
	private TEventAssignmentDelegate[] _eventAssignments;
	private double[] _eventPriorities;
	private TComputeEventAssignmentDelegate[] _computeEventAssignments;
	private TPerformEventAssignmentDelegate[] _performEventAssignments;
	private bool[] _eventStatusArray = new bool[0];
	private bool[] _previousEventStatusArray = new bool[0];

	public TModel ()  
	{
		numIndependentVariables = 0;
		numDependentVariables = 0;
		numTotalVariables = 0;
		numBoundaryVariables = 0;
		numGlobalParameters = 7;
		numCompartments = 0;
		numReactions = 0;
		numEvents = 0;
		InitializeDelays();
	}

	void loadSymbolTables() {
		globalParameterTable[0] = "S1";
		globalParameterTable[1] = "S2";
		globalParameterTable[2] = "S3";
		globalParameterTable[3] = "S4";
		globalParameterTable[4] = "k1";
		globalParameterTable[5] = "k2";
		globalParameterTable[6] = "k3";
	}


	public void resetEvents() {
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
		_gp[0] = (double)0.02;
		_gp[1] = (double)0.02;
		_gp[2] = (double)0;
		_gp[3] = (double)0;
		_gp[4] = (double)100;
		_gp[5] = (double)0.9;
		_gp[6] = (double)0.7;
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
		_rateRules[0] = -
	(double)1*
	_gp[4]*
	_gp[0]*
	_gp[1]+
	_gp[5]*
	_gp[2]+
	_gp[6]*
	_gp[2];
		_rateRules[1] = _gp[5]*
	_gp[2]+
	-
	(double)1*
	_gp[4]*
	_gp[0]*
	_gp[1];
		_rateRules[2] = _gp[4]*
	_gp[0]*
	_gp[1]+
	-
	(double)1*
	_gp[5]*
	_gp[2]+
	-
	(double)1*
	_gp[6]*
	_gp[2];
		_rateRules[3] = _gp[6]*
	_gp[2];
	}

	private double[] _rateRules = new double[4];           // Vector containing values of additional rate rules      
	public void InitializeRates()
	{
		_rateRules[0] = 		_gp[0];
		_rateRules[1] = 		_gp[1];
		_rateRules[2] = 		_gp[2];
		_rateRules[3] = 		_gp[3];
	}

	public void AssignRates()
	{
		_gp[0] = _rateRules[0];
		_gp[1] = _rateRules[1];
		_gp[2] = _rateRules[2];
		_gp[3] = _rateRules[3];
	}

	public void InitializeRateRuleSymbols()
	{
		_gp[0] = 0.02;
		_gp[1] = 0.02;
		_gp[2] = 0;
		_gp[3] = 0;
	}

	public void AssignRates(double[] oRates)
	{
		_gp[0] = oRates[0];
		_gp[1] = oRates[1];
		_gp[2] = oRates[2];
		_gp[3] = oRates[3];
	}

	public double[] GetCurrentValues()
	{
		double[] dResult = new double[4];
		dResult[0] = 		_gp[0];
		dResult[1] = 		_gp[1];
		dResult[2] = 		_gp[2];
		dResult[3] = 		_gp[3];
		return dResult;
	}

	// Uses the equation: dSd/dt = L0 dSi/dt
	public void computeAllRatesOfChange ()
	{
		double[] dTemp = new double[amounts.Length + rateRules.Length];
		dTemp[0] = 		_gp[0];
		dTemp[1] = 		_gp[1];
		dTemp[2] = 		_gp[2];
		dTemp[3] = 		_gp[3];
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
		_gp[3] = oAmounts[3];

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
		_time = timeIn;  // Don't remove
		updateDependentSpeciesValues(_y);
		computeRules (_y);
	}



	private void InitializeDelays() { 
	}



	public void computeEventPriorites() { 
	}

	public void evalInitialAssignments()
	{
	}

	public void testConstraints()
	{
	}

}
