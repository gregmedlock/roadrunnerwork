//************************************************************************** 
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using LibRoadRunner;
 


class TModel : IModel
{
	// Symbol Mappings

	// y[0] = S3
	// y[1] = S2
	// y[2] = S1
	// y[3] = S4

	private List<string> _Warnings = new List<string>();
	private double[] _gp = new double[3];           // Vector containing all the global parameters in the System  
	private double[] _sr = new double[0];           // Vector containing all the modifiable species references  
	private double[][] _lp = new double[2][];       // Vector containing all the local parameters in the System  
	private double[] _y = new double[4];            // Vector containing the concentrations of all floating species 
	private double[] _init_y = new double[4];            // Vector containing the initial concentrations of all floating species 
	private double[] _amounts = new double[4];      // Vector containing the amounts of all floating species 
	private double[] _bc = new double[0];           // Vector containing all the boundary species concentration values   
	private double[] _c = new double[1];            // Vector containing all the compartment values   
	private double[] _dydt = new double[4];         // Vector containing rates of changes of all species   
	private double[] _rates = new double[2];        // Vector containing the rate laws of all reactions    
	private double[] _ct = new double[2];           // Vector containing values of all conserved sums      
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
	string[] variableTable = new string[4];
	string[] boundaryTable = new string[0];
	string[] globalParameterTable = new string[3];
	int[] localParameterDimensions = new int[2];
	private TEventAssignmentDelegate[] _eventAssignments;
	private double[] _eventPriorities;
	private TComputeEventAssignmentDelegate[] _computeEventAssignments;
	private TPerformEventAssignmentDelegate[] _performEventAssignments;
	private bool[] _eventStatusArray = new bool[2];
	private bool[] _previousEventStatusArray = new bool[2];

	public TModel ()  
	{
		numIndependentVariables = 2;
		numDependentVariables = 2;
		numTotalVariables = 4;
		numBoundaryVariables = 0;
		numGlobalParameters = 3;
		numCompartments = 1;
		numReactions = 2;
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

		localParameterDimensions[0] = 0;
		_lp[0] = new double[0];
		localParameterDimensions[1] = 0;
		_lp[1] = new double[0];
	}

	void loadSymbolTables() {
		variableTable[0] = "S3";
		variableTable[1] = "S2";
		variableTable[2] = "S1";
		variableTable[3] = "S4";
		globalParameterTable[0] = "k1";
		globalParameterTable[1] = "k2";
		globalParameterTable[2] = "p1";
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
			case 0: volume = _c[0];
				break;
			case 1: volume = _c[0];
				break;
			case 2: volume = _c[0];
				break;
			case 3: volume = _c[0];
				break;
		}
		_amounts[index] = _y[index]*volume;
	}

	public double getConcentration(int index) {
		return _y[index];
	}

	public void convertToAmounts() {
		_amounts[0] = _y[0]*_c[0];
		_amounts[1] = _y[1]*_c[0];
		_amounts[2] = _y[2]*_c[0];
		_amounts[3] = _y[3]*_c[0];
	}

	public void convertToConcentrations() {
		_y[0] = _amounts[0]/_c[0];
		_y[1] = _amounts[1]/_c[0];
		_y[2] = _amounts[2]/_c[0];
		_y[3] = _amounts[3]/_c[0];
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
		_init_y[0] = (double)0.0002/ _c[0];
		_init_y[1] = (double)0.00012/ _c[0];
		_init_y[2] = (double)0.0001/ _c[0];
		_init_y[3] = (double)0.0001/ _c[0];

	}

	public void setInitialConditions ()
	{
		_y[0] =  _init_y[0];
		_amounts[0] = _y[0]*_c[0];
		_y[1] =  _init_y[1];
		_amounts[1] = _y[1]*_c[0];
		_y[2] =  _init_y[2];
		_amounts[2] = _y[2]*_c[0];
		_y[3] =  _init_y[3];
		_amounts[3] = _y[3]*_c[0];

	}

	public void setBoundaryConditions ()
	{
	}

	public void setCompartmentVolumes ()
	{
		_c[0] = (double)1;
		_c[0] = 
	(double)1;
	}

	public void setParameterValues ()
	{
		_gp[0] = (double)7.5e+03;
		_gp[1] = (double)2.5e+03;
		_gp[2] = (double)0.5;
	}

	// Uses the equation: C = Sd - L0*Si
	public void computeConservedTotals ()
	{
		_ct[0] =  - _y[1]*_c[0]
 + _y[2]*_c[0]
;
		_ct[1] =  + _y[1]*_c[0]
 + _y[3]*_c[0]
;
	}

	// Compute values of dependent species 
	// Uses the equation: Sd = C + L0*Si
	public void updateDependentSpeciesValues (double[] y)
	{
		_y[2] = 
	(_ct[0] + 
	y[1]*_c[0]
	)/_c[0];
		_y[3] = 
	(_ct[1] + 
	y[1]*_c[0]
	)/_c[0];
	}

	public void computeRules(double[] y) {
	}

	private double[] _rateRules = new double[0];           // Vector containing values of additional rate rules      
	public void InitializeRates()
	{
	}

	public void AssignRates()
	{
	}

	public void InitializeRateRuleSymbols()
	{
	}

	public void AssignRates(double[] oRates)
	{
	}

	public double[] GetCurrentValues()
	{
		double[] dResult = new double[0];
		return dResult;
	}

	// Uses the equation: dSd/dt = L0 dSi/dt
	public void computeAllRatesOfChange ()
	{
		double[] dTemp = new double[amounts.Length + rateRules.Length];
		amounts.CopyTo(dTemp, rateRules.Length);
		evalModel (time, dTemp);
		_dydt[2] =  + _dydt[1]
;
		_dydt[3] =  + _dydt[1]
;
	}

	// Compute the reaction rates
	public void computeReactionRates (double time, double[] y)
	{
		_rates[0] = _c[0]*
	_gp[0]*
	y[2]*
	y[1];
		_rates[1] = _c[0]*
	_gp[1]*
	y[0]*
	y[3];
	}

	// Model Function
	public void evalModel (double timein, double[] oAmounts)
	{
		_y[0] = oAmounts[0]/_c[0];
		_y[1] = oAmounts[1]/_c[0];
		_y[2] = oAmounts[2]/_c[0];
		_y[3] = oAmounts[3]/_c[0];

		convertToAmounts();
		_time = timein;  // Don't remove
		updateDependentSpeciesValues (_y);
		computeReactionRates (time, _y);
		_dydt[0] = + ((double)4*
	_gp[2]) * _rates[0] - 2*_rates[1];
		_dydt[1] = - _rates[0] + _rates[1];
		convertToAmounts ();
	}

	// Event handling function
	public void evalEvents (double timeIn, double[] oAmounts)
	{
		_y[0] = oAmounts[0]/_c[0];
		_y[1] = oAmounts[1]/_c[0];
		_y[2] = oAmounts[2]/_c[0];
		_y[3] = oAmounts[3]/_c[0];
		_time = timeIn;  // Don't remove
		updateDependentSpeciesValues(_y);
		computeRules (_y);
		previousEventStatusArray[0] = eventStatusArray[0];
		if ((_y[3],_y[1])
	 == 1.0) {
		     eventStatusArray[0] = true;
		     eventTests[0] = 1;
		} else {
		     eventStatusArray[0] = false;
		     eventTests[0] = -1;
		}
		previousEventStatusArray[1] = eventStatusArray[1];
		if ((_y[0],(double)1/
	(double)4e+03)
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
		values[0];
		return values;
	}
	public void performEventAssignment_0 (double[] values) {
				_y[2] = values[0];
		convertToConcentrations();
	}
	public void eventAssignment_1 () {
		performEventAssignment_1( computeEventAssignment_1() );
	}
	public double[] computeEventAssignment_1 () {
		double[] values = new double[ 1];
		values[0];
		return values;
	}
	public void performEventAssignment_1 (double[] values) {
				_y[3] = values[0];
		convertToConcentrations();
	}
	


	private void InitializeDelays() { 
		_eventDelay[0] = new TEventDelayDelegate(delegate {{ return (double)0; }} );
		_eventType[0] = true;
		_eventPersistentType[0] = true;
		_eventDelay[1] = new TEventDelayDelegate(delegate {{ return (double)0; }} );
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

