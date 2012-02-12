#ifdef USE_PCH
#include "rrPCH.h"
#endif
#pragma hdrstop
#include <math.h>
#include "rrNOMSupport.h"
#include "rrStringUtils.h"
#include "rrException.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

namespace rr
{

NOMSupport::NOMSupport()
:
mModel(NULL),
mSBMLDoc(NULL)
{}

NOMSupport::~NOMSupport()
{
}

int	NOMSupport::LoadSBML(const string& sbml)
{

	int res = loadSBML(sbml.c_str());
	mModel = GetSBMLModel();
    return res;
}

string NOMSupport::getNthCompartmentId(const int& i)
{
    //NOM
	//DLL_EXPORT int getNthCompartmentId (int nIndex, char **Id)


	char *ID[1];
    if( ::getNthCompartmentId(i, ID) )
    {
		return "";
    }

	return string(ID[0]);
}

double NOMSupport::getValue(const string& id)
{
	double val;
	if(::getValue(id.c_str(), &val))
    {
    	//How to signal error..?
    	return -1;
    }
	return val;
}

//StringListContainer NOMSupport::GetFloatingSpecies()
//{
//	StringContainer floatingSpeciesList;
//	int nrOfSpecies = ::getNumFloatingSpecies();
//
//    for (int i = 0; i < nrOfSpecies; i++)
//    {
//        Species* aSpecies = mModel->getSpecies(i);
//        if( aSpecies != NULL && !aSpecies->getBoundaryCondition())
//        {
//        	StringCollection oSpeciesValues;// = new ArrayList();
//            //oSpeciesValues.Add(GetId(aSpecies));
//            oSpeciesValues.Add(aSpecies->getId());
//            double concentration = aSpecies->isSetInitialConcentration() ? aSpecies->getInitialConcentration() : aSpecies->getInitialAmount();
//
//            oSpeciesValues.Add(ToString(concentration));
//            oSpeciesValues.Add(ToString(aSpecies->isSetInitialConcentration()));
//            floatingSpeciesList.Add(oSpeciesValues);
//        }
//    }
//
//    return floatingSpeciesList;
//}

StringListContainer NOMSupport::getListOfBoundarySpecies()
{
    StringListContainer boundarySpeciesList;// = new StringListContainer();

    if (mModel == NULL)
    {
        throw RRException("You need to load the model first");
    }

    for (int i = 0; i < mModel->getNumSpecies(); i++)
    {
        Species *oSpecies = mModel->getSpecies(i);
        if (oSpecies->getBoundaryCondition())
        {
            StringList oSpeciesValues;// = new ArrayList();
//            oSpeciesValues.Add(GetId(oSpecies));
            oSpeciesValues.Add(oSpecies->getId());
            double concentration = oSpecies->isSetInitialConcentration() ? oSpecies->getInitialConcentration() : oSpecies->getInitialAmount();
            oSpeciesValues.Add(ToString(concentration));
            oSpeciesValues.Add(ToString(oSpecies->isSetInitialConcentration())
            );

            boundarySpeciesList.Add(oSpeciesValues);
        }
    }

    return boundarySpeciesList;
}


// ============ From NOM.cs in SBMLSupport
///// <summary>
//    /// Summary description for NOM.
//    /// </summary>
//    public class NOM
//    {
//
//        static SBMLDocument _oDoc;
//
//        public static SBMLDocument Document
//        {
//            get
//            {
//                return _oDoc;
//            }
//        }
//
//        static Model mModel;
//
//        public static Model Model
//        {
//            get
//            {
//                return mModel;
//            }
//        }
//
//        public static string GetAnnotatedModel(string targetSBML, string sourceSBML, bool checkModelId)
//        {
//            return AnnotationUtil.GetAnnotatedModel(targetSBML, sourceSBML, checkModelId);
//        }
//
//        public static string GetId(SBase element)
//        {
//            if (element.isSetId())
//                return element.getId();
//            return element.getName();
//        }
//
//        public static string getMetaId(string sId)
//        {
//            if (mModel == NULL)
//            {
//                return "";
//                //throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                return oSpecies.getMetaId();
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return oParameter.getMetaId();
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return oCompartment.getMetaId();
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                return oReaction.getMetaId();
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                return oRule.getMetaId();
//            }
//
//            if (mModel.getId() == sId)
//                return mModel.getMetaId();
//
//            return "";
//        }
//
//        private static string GetName(SBase element)
//        {
//            if (element.isSetName())
//                return element.getName();
//            return element.getId();
//        }
//
//        private static bool addMissingModifiers(Model oModel)
//        {
//            StringCollection _species = new StringCollection();
//            for (int i = 0; i < oModel.getNumSpecies(); i++)
//            {
//                _species.Add(GetId(oModel.getSpecies(i)));
//            }
//            int nReactions = (int)oModel.getNumReactions();
//            bool bReplaced = false;
//            for (int i = 0; i < nReactions; i++)
//            {
//                libsbmlcs.Reaction oReaction = oModel.getReaction(i);
//                KineticLaw oLaw = oReaction.getKineticLaw();
//                if (oLaw == NULL) continue;
//                StringCollection symbols = new StringCollection();
//
//                for (int j = 0; j < oReaction.getNumModifiers(); j++)
//                {
//                    symbols.Add(oReaction.getModifier(j).getSpecies());
//                }
//                for (int j = 0; j < oModel.getNumParameters(); j++)
//                {
//                    symbols.Add(GetId(oModel.getParameter(j)));
//                }
//                for (int j = 0; j < oModel.getNumCompartments(); j++)
//                {
//                    symbols.Add(GetId(oModel.getCompartment(j)));
//                }
//                for (int j = 0; j < oModel.getNumFunctionDefinitions(); j++)
//                {
//                    symbols.Add(GetId(oModel.getFunctionDefinition(j)));
//                }
//
//                if (oLaw != NULL)
//                {
//                    for (int j = 0; j < oLaw.getNumParameters(); j++)
//                    {
//                        symbols.Add(GetId(oLaw.getParameter(j)));
//                    }
//                }
//
//                for (int j = 0; j < oReaction.getNumReactants(); j++)
//                {
//                    symbols.Add(oReaction.getReactant(j).getSpecies());
//                }
//                for (int j = 0; j < oReaction.getNumProducts(); j++)
//                {
//                    symbols.Add(oReaction.getProduct(j).getSpecies());
//                }
//                ASTNode oRoot = oLaw.getMath();
//                StringCollection oMissingNames = new StringCollection();
//
//                // here the fancy function that discoveres themissing names and solves all problems
//                // magically ...
//                checkForMissingNames(oRoot, oMissingNames, symbols);
//                string sMissingName;
//                if (oMissingNames.Count > 0)
//                {
//                    bReplaced = true;
//                    for (int j = 0; j < oMissingNames.Count; j++)
//                    {
//                        sMissingName = oMissingNames[j];
//                        if (_species.Contains(sMissingName))
//                        {
//                            ModifierSpeciesReference reference = oReaction.createModifier();
//                            reference.setSpecies(sMissingName);
//                            oReaction.addModifier(reference);
//                        }
//                    }
//                }
//            }
//            return bReplaced;
//        }
//
//        public static string addMissingModifiers(string sModel)
//        {
//
//            SBMLDocument d = libsbml.readSBMLFromString(sModel);
//            string sResult = sModel;
//            try
//            {
//                Model oModel = d.getModel();
//                if (oModel != NULL)
//                {
//                    bool bReplaced = addMissingModifiers(oModel);
//                    if (!bReplaced)
//                    {
//                        return sModel;
//                    }
//                }
//
//                sResult = libsbml.writeSBMLToString(d);
//            }
//            catch
//            {
//                throw new Exception("Exception occured while trying to modify the SBML file");
//            }
//
//            finally
//            {
//                if (d != NULL)
//                    d.Dispose();
//            }
//
//
//            return sResult;
//        }
//
//        private static void checkForMissingNames(ASTNode node, StringCollection results, StringCollection symbols)
//        {
//            for (int i = 0; i < node.getNumChildren(); i++)
//            {
//                checkForMissingNames(node.getChild(i), results, symbols);
//            }
//            if (node.isName())
//            {
//                string sName = node.getName();
//                if (!symbols.Contains(sName) && !results.Contains(sName))
//                    results.Add(sName);
//            }
//        }
//
//        public static int checkConsistency()
//        {
//            if (_oDoc == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)_oDoc.checkConsistency();
//
//        }
//
//        public static string convertLevel1ToLevel2Impl(string sSBML)
//        {
//            SBMLReader oReader = new SBMLReader();
//            SBMLDocument oDoc = oReader.readSBMLFromString(sSBML);
//            string sResult = sSBML;
//
//            try
//            {
//                oDoc.setLevelAndVersion(2, 1);
//                SBMLWriter oWriter = new SBMLWriter();
//                sResult = oWriter.writeToString(oDoc);
//            }
//            finally
//            {
//                if (oDoc != NULL)
//                    oDoc.Dispose();
//            }
//            return sResult;
//        }
//
//        public static string convertLevel2ToLevel1Impl(string sSBML)
//        {
//            SBMLReader oReader = new SBMLReader();
//            SBMLDocument oDoc = oReader.readSBMLFromString(sSBML);
//            string sResult = sSBML;
//
//            try
//            {
//                oDoc.setLevelAndVersion(1, 2);
//                SBMLWriter oWriter = new SBMLWriter();
//                sResult = oWriter.writeToString(oDoc);
//            }
//            finally
//            {
//                if (oDoc != NULL)
//                    oDoc.Dispose();
//            }
//            return sResult;
//        }
//
//        public static string convertMathMLToString(string sMathML)
//        {
//            ASTNode node = libsbml.readMathMLFromString(sMathML);
//            string sResult = libsbml.formulaToString(node);
//            return sResult;
//        }
//
//        public static string convertPowImpl(string sSBML)
//        {
//
//            SBMLDocument doc = libsbml.readSBMLFromString(sSBML);
//            try
//            {
//                Model model = doc.getModel();
//                if (model == NULL)
//                {
//                    throw new Exception("Error in sbml input. ");
//                }
//                string strKineticFormula;
//                for (int i = 0; i < model.getNumReactions(); i++)
//                {
//                    libsbmlcs.Reaction r = model.getReaction(i);
//                    KineticLaw kl = r.getKineticLaw();
//
//                    if (kl == NULL)
//                    {
//                        strKineticFormula = "";
//                    }
//                    else
//                    {
//                        strKineticFormula = kl.getFormula();
//                        if (strKineticFormula == NULL)
//                        {
//                            throw new Exception("The kinetic law has errors");
//                        }
//                    }
//                    ASTNode ast_Node = libsbml.parseFormula(strKineticFormula);
//                    changePow(ast_Node);
//                    kl.setMath(ast_Node);
//
//                }
//
//                doc.setLevelAndVersion(1, 2);
//                return libsbml.writeSBMLToString(doc);
//            }
//            finally
//            {
//                if (doc != NULL)
//                    doc.Dispose();
//            }
//        }
//
//        private static void changePow(ASTNode node)
//        {
//            int c;
//
//            if (node.getType() == libsbml.AST_FUNCTION_POWER)
//            {
//                node.setType(libsbml.AST_POWER);
//            }
//
//            for (c = 0; c < node.getNumChildren(); c++)
//            {
//                changePow(node.getChild(c));
//            }
//        }
//
//        public static string convertSBML(string sModel, int nLevel, int nVersion)
//        {
//            return convertSBML(sModel, nLevel, nVersion, true);
//        }
//
//        private static void RemoveSpatialSizeUnitsFromSpecies(SBMLDocument doc)
//        {
//            if (doc == NULL) return;
//            if (doc.getModel() == NULL) return;
//            var model = doc.getModel();
//            for (int i = 0; i < model.getNumSpecies(); i++)
//            {
//                var species = model.getSpecies(i);
//                if (species.isSetSpatialSizeUnits())
//                    species.unsetSpatialSizeUnits();
//            }
//        }
//
//        private static void RemoveTimeUnitsFromKineticLaws(SBMLDocument doc)
//        {
//            if (doc == NULL) return;
//            if (doc.getModel() == NULL) return;
//            var model = doc.getModel();
//            for (int i = 0; i < model.getNumReactions(); i++)
//            {
//                var reaction = model.getReaction(i);
//                if (reaction.isSetKineticLaw())
//                {
//                    var law = reaction.getKineticLaw();
//                    if (law.isSetTimeUnits())
//                        law.unsetTimeUnits();
//                }
//            }
//        }
//
//        private static void RemoveSubstanceUnitsFromKineticLaws(SBMLDocument doc)
//        {
//            if (doc == NULL) return;
//            if (doc.getModel() == NULL) return;
//            var model = doc.getModel();
//            for (int i = 0; i < model.getNumReactions(); i++)
//            {
//                var reaction = model.getReaction(i);
//                if (reaction.isSetKineticLaw())
//                {
//                    var law = reaction.getKineticLaw();
//                    if (law.isSetSubstanceUnits())
//                        law.unsetSubstanceUnits();
//                }
//            }
//        }
//
//        private static void AddMissingParameter(string parameterId, SBMLDocument doc)
//        {
//            if (doc == NULL) return;
//            var model = doc.getModel();
//            if (model == NULL) return;
//            var parameter = model.createParameter();
//            parameter.setId(parameterId);
//            parameter.setValue(0.1);
//            return;
//        }
//
//        private static void UpgradeToL2V4IfNecessary(SBMLDocument doc)
//        {
//            if (doc.getLevel() == 1 || (doc.getLevel() == 2 && doc.getVersion() < 2))
//            {
//                Model oModel = doc.getModel();
//                addMissingModifiers(oModel);
//
//                if (oModel == NULL)
//                {
//                    SBMLErrorLog oLog = doc.getErrorLog();
//                    StringBuilder oBuilder = new StringBuilder();
//                    for (int i = 0; i < oLog.getNumErrors(); i++)
//                    {
//                        SBMLError error = oLog.getError(i);
//                        oBuilder.Append(String.Format("{0}: {1}{2}", error.getErrorId(), error.getMessage(), Environment.NewLine));
//                    }
//
//                    throw new Exception(oBuilder.ToString());
//                }
//
//                if (!doc.setLevelAndVersion(2, 4))
//                {
//
//                    SBMLErrorLog oLog = doc.getErrorLog();
//                    StringBuilder oBuilder = new StringBuilder();
//                    for (int i = 0; i < oLog.getNumErrors(); i++)
//                    {
//                        SBMLError error = oLog.getError(i);
//
//                        switch (error.getErrorId())
//                        {
//                            case 95004:
//                                {
//                                    RemoveSpatialSizeUnitsFromSpecies(doc);
//                                    break;
//                                }
//                            case 95002:
//                                {
//                                    RemoveTimeUnitsFromKineticLaws(doc);
//                                    break;
//                                }
//                            case 95003:
//                                {
//                                    RemoveSubstanceUnitsFromKineticLaws(doc);
//                                    break;
//                                }
//                            case 10215:
//                                {
//                                    var matches = new System.Text.RegularExpressions.Regex("'(?<id>.*?)'").Matches((error.getMessage()));
//                                    var thisMatch = matches[matches.Count - 1];
//                                    var parameterId = thisMatch.Groups[thisMatch.Groups.Count - 1].Value;
//                                    AddMissingParameter(parameterId, doc);
//                                    break;
//                                }
//                            default:
//                                break;
//                        }
//
//                    }
//                    oLog.clearLog();
//
//                    if (!doc.setLevelAndVersion(2, 4))
//                    {
//                        oLog = doc.getErrorLog();
//                        oBuilder = new StringBuilder();
//                        int numErrors = 0;
//                        for (int i = 0; i < oLog.getNumErrors(); i++)
//                        {
//                            SBMLError error = oLog.getError(i);
//                            if (//error.isError() ||
//                                error.isFatal())
//                                numErrors++;
//
//                            oBuilder.Append(error.getErrorId() + ": " + error.getMessage() + Environment.NewLine);
//                        }
//                        if (numErrors > 0)
//                            throw new Exception(oBuilder.ToString());
//                    }
//
//                }
//            }
//        }
//
//        public static string getSBOCapableSBML(string sModel)
//        {
//            if (sModel == "")
//            {
//                throw new ArgumentException("The model cannot be empty");
//            }
//
//            SBMLDocument oSBMLDoc = libsbml.readSBMLFromString(sModel);
//            try
//            {
//
//                if (oSBMLDoc.getLevel() >= 2 && oSBMLDoc.getVersion() >= 2) return sModel;
//
//                UpgradeToL2V4IfNecessary(oSBMLDoc);
//
//
//                return libsbml.writeSBMLToString(oSBMLDoc);
//            }
//            finally
//            {
//                if (oSBMLDoc != NULL)
//                    oSBMLDoc.Dispose();
//            }
//        }
//
//        public static string convertSBML(string sModel, int nLevel, int nVersion, bool throwError)
//        {
//            if (sModel == "")
//            {
//                throw new ArgumentException("The model cannot be empty");
//            }
//
//            SBMLDocument oSBMLDoc = libsbml.readSBMLFromString(sModel);
//            try
//            {
//                Model oModel = oSBMLDoc.getModel();
//
//                if (oModel == NULL)
//                {
//                    SBMLErrorLog oLog = oSBMLDoc.getErrorLog();
//                    StringBuilder oBuilder = new StringBuilder();
//                    for (int i = 0; i < oLog.getNumErrors(); i++)
//                    {
//                        SBMLError error = oLog.getError(i);
//                        oBuilder.Append(error.getErrorId() + ": " + error.getMessage() + Environment.NewLine);
//                    }
//
//                    throw new Exception(oBuilder.ToString());
//                }
//
//                oSBMLDoc.setLevelAndVersion((int)nLevel, (int)nVersion, false);
//
//                if (throwError && oSBMLDoc.getNumErrors() > 0)
//                {
//                    SBMLErrorLog oLog = oSBMLDoc.getErrorLog();
//                    bool fatal = false;
//                    StringBuilder oBuilder = new StringBuilder();
//                    for (int i = 0; i < oLog.getNumErrors(); i++)
//                    {
//                        SBMLError error = oLog.getError(i);
//                        if (error.getSeverity() == libsbml.LIBSBML_SEV_ERROR ||
//                         error.getSeverity() == libsbml.LIBSBML_SEV_FATAL)
//                        {
//                            fatal = true;
//                            oBuilder.Append(error.getErrorId() + ": " + error.getMessage() + Environment.NewLine);
//                        }
//                    }
//                    if (fatal)
//                    throw new Exception(oBuilder.ToString());
//                }
//
//                return libsbml.writeSBMLToString(oSBMLDoc);
//            }
//            finally
//            {
//                if (oSBMLDoc != NULL)
//                    oSBMLDoc.Dispose();
//            }
//        }
//
//        public static string convertStringToMathML(string var0)
//        {
//            ASTNode node = libsbml.parseFormula(var0);
//            try
//            {
//
//                string sResult = libsbml.writeMathMLToString(node);
//                return sResult;
//            }
//            finally
//            {
//                if (node != NULL)
//                    node.Dispose();
//            }
//        }
//
//        public static string convertTime(string sArg, string sTimeSymbol)
//        {
//            SBMLDocument oSBMLDoc = NULL;
//            Model oModel = NULL;
//
//            try
//            {
//                oSBMLDoc = libsbml.readSBMLFromString(sArg);
//                oModel = oSBMLDoc.getModel();
//
//                if (oModel == NULL)
//                {
//                    throw new Exception("SBML Validation failed");
//                }
//                else
//                {
//                    changeTimeSymbol(oModel, sTimeSymbol);
//                    return libsbml.writeSBMLToString(oSBMLDoc);
//                }
//            }
//            finally
//            {
//                if (oSBMLDoc != NULL)
//                    oSBMLDoc.Dispose();
//            }
//        }
//
//        public static void ChangeConstantForRules(Model model)
//        {
//            var ruleTargets = new List<string>();
//            for (int i = 0; i < model.getNumRules(); i++)
//            {
//                var rule = model.getRule(i);
//                ruleTargets.Add(rule.getVariable());
//            }
//            for (int i = 0; i < model.getNumParameters(); i++)
//            {
//                var parameter = model.getParameter(i);
//                if (ruleTargets.Contains(parameter.getId()))
//                    parameter.setConstant(false);
//            }
//
//            for (int i = 0; i < model.getNumCompartments(); i++)
//            {
//                var compartment = model.getCompartment(i);
//                if (ruleTargets.Contains(compartment.getId()))
//                    compartment.setConstant(false);
//            }
//
//        }
//        /// <summary>
//        /// This function alters the given SBML model by fixing common errors:
//        ///
//        /// parameter "time", "avogadro" will be replaced with their respective CSymbol
//        /// missing modifiers will be added,
//        ///
//        /// also parameters with rules will be set to constant
//        /// </summary>
//        /// <param name="sbml">the sbml string to fix</param>
//        /// <returns></returns>
//        public static string FixCommonIssues(string sbml)
//        {
//            return FixCommonIssues(sbml, NULL, NULL);
//        }
//
//        /// <summary>
//        /// This function alters the given SBML model by fixing common errors:
//        ///
//        /// parameter "time", "avogadro" will be replaced with their respective CSymbol
//        /// missing modifiers will be added,
//        ///
//        /// also parameters with rules will be set to constant
//        /// </summary>
//        /// <param name="sbml">the sbml string to fix</param>
//        /// <param name="programName">program name (or NULL in case of none)</param>
//        /// <param name="programVersion">program version</param>
//        /// <returns></returns>
//        public static string FixCommonIssues(string sbml, string programName, string programVersion)
//        {
//            var doc = libsbml.readSBMLFromString(sbml);
//            var model = doc.getModel();
//            if (model == NULL)
//            {
//                throw new Exception("SBML Validation failed");
//            }
//            ChangeNameToCSymbol(model, "time", libsbml.AST_NAME_TIME);
//            ChangeNameToCSymbol(model, "avogadro", libsbml.AST_NAME_AVOGADRO);
//
//            addMissingModifiers(model);
//
//            ChangeConstantForRules(model);
//
//            using (var writer = new SBMLWriter())
//            {
//                if (!string.IsNullOrEmpty(programName))
//                {
//
//                    writer.setProgramName(programName);
//                    if (!string.IsNullOrEmpty(programVersion))
//                        writer.setProgramVersion(programVersion);
//                }
//
//                return writer.writeSBMLToString(doc);
//            }
//        }
//
//        public static string convertTimeToCSymbol(string sArg, string sTimeSymbol)
//        {
//            SBMLDocument oSBMLDoc = NULL;
//            Model oModel = NULL;
//
//            try
//            {
//                oSBMLDoc = libsbml.readSBMLFromString(sArg);
//                oModel = oSBMLDoc.getModel();
//
//                if (oModel == NULL)
//                {
//                    throw new Exception("SBML Validation failed");
//                }
//                else
//                {
//                    ChangeNameToCSymbol(oModel, sTimeSymbol, libsbml.AST_NAME_TIME);
//                    return libsbml.writeSBMLToString(oSBMLDoc);
//                }
//            }
//            finally
//            {
//                if (oSBMLDoc != NULL)
//                    oSBMLDoc.Dispose();
//            }
//        }
//
//        private static void changeSymbol(Model oModel, string sTimeSymbol, int targetType)
//        {
//            for (int i = 0; i < oModel.getNumReactions(); i++)
//            {
//                libsbmlcs.Reaction r = oModel.getReaction(i);
//                if (r.getKineticLaw() != NULL && r.getKineticLaw().isSetMath())
//                    r.getKineticLaw().setMath(changeSymbol(r.getKineticLaw().getMath(), sTimeSymbol, targetType));
//            }
//            for (int i = 0; i < oModel.getNumRules(); i++)
//            {
//                Rule r = oModel.getRule(i);
//                if (r.isSetMath())
//                    r.setMath(changeSymbol(r.getMath(), sTimeSymbol, targetType));
//            }
//            for (int i = 0; i < oModel.getNumInitialAssignments(); i++)
//            {
//                InitialAssignment initialAssignment = oModel.getInitialAssignment(i);
//                if (initialAssignment.isSetMath())
//                    initialAssignment.setMath(changeSymbol(initialAssignment.getMath(), sTimeSymbol, targetType));
//            }
//            for (int i = 0; i < oModel.getNumEvents(); i++)
//            {
//                Event oEvent = oModel.getEvent(i);
//                if (oEvent.getTrigger().isSetMath())
//                    oEvent.getTrigger().setMath(changeSymbol(oEvent.getTrigger().getMath(), sTimeSymbol, targetType));
//                if (oEvent.isSetDelay() && oEvent.getDelay().isSetMath())
//                    oEvent.getDelay().setMath(changeSymbol(oEvent.getDelay().getMath(), sTimeSymbol, targetType));
//                for (int j = 0; j < oEvent.getNumEventAssignments(); j++)
//                {
//                    EventAssignment assignment = oEvent.getEventAssignment(j);
//                    if (assignment.isSetMath())
//                        assignment.setMath(changeSymbol(assignment.getMath(), sTimeSymbol, targetType));
//                }
//            }
//        }
//
//        private static void ChangeNameToCSymbol(Model model, string name, int type)
//        {
//            for (int i = 0; i < model.getNumReactions(); i++)
//            {
//                libsbmlcs.Reaction r = model.getReaction(i);
//                if (r.getKineticLaw() != NULL && r.getKineticLaw().isSetMath())
//                    r.getKineticLaw().setMath(changeTimeToCSymbol(r.getKineticLaw().getMath(), name, type));
//            }
//            for (int i = 0; i < model.getNumRules(); i++)
//            {
//                Rule r = model.getRule(i);
//                if (r.isSetMath())
//                    r.setMath(changeTimeToCSymbol(r.getMath(), name, type));
//            }
//            for (int i = 0; i < model.getNumEvents(); i++)
//            {
//                Event oEvent = model.getEvent(i);
//                if (oEvent.getTrigger().isSetMath())
//                    oEvent.getTrigger().setMath(changeTimeToCSymbol(oEvent.getTrigger().getMath(), name, type));
//            }
//        }
//
//        private static ASTNode changeSymbol(ASTNode node, string time, int targetType)
//        {
//            int c;
//            if (node.getType() == targetType)
//                node.setName(time);
//
//            for (c = 0; c < node.getNumChildren(); c++)
//                changeSymbol(node.getChild(c), time, targetType);
//            return node;
//        }
//
//        public static ASTNode ReplaceSymbol(ASTNode node, string oldId, string newId)
//        {
//            int c;
//            if (node.getType() == libsbml.AST_NAME && node.getName() == oldId)
//                node.setName(newId);
//
//            for (c = 0; c < node.getNumChildren(); c++)
//                ReplaceSymbol(node.getChild(c), oldId, newId);
//            return node;
//        }
//
//
//        private static ASTNode changeTimeToCSymbol(ASTNode node, string name, int type)
//        {
//            int c;
//            if (node.getName() == name && node.getType() != type)
//                node.setType(type);
//            for (c = 0; c < node.getNumChildren(); c++)
//                changeTimeToCSymbol(node.getChild(c), name, type);
//            return node;
//        }
//
//        public static bool exists(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                return true;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return true;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return true;
//            }
//            return false;
//        }
//
//        public static string getAnnotation(string sId)
//        {
//            if (mModel == NULL)
//            {
//                return "";
//                //throw new Exception("You need to load the model first");
//            }
//
//            string sResult = "";
//
//            if (mModel.getId() == sId || mModel.getName() == sId)
//            {
//                if (mModel.isSetAnnotation())
//                {
//                    sResult = mModel.getAnnotationString();
//                }
//                return sResult;
//
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                if (oSpecies.isSetAnnotation())
//                {
//                    sResult = oSpecies.getAnnotationString();
//                }
//                return sResult;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                if (oParameter.isSetAnnotation())
//                {
//                    sResult = oParameter.getAnnotationString();
//                }
//                return sResult;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                if (oCompartment.isSetAnnotation())
//                {
//                    sResult = oCompartment.getAnnotationString();
//                }
//                return sResult;
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                if (oReaction.isSetAnnotation())
//                {
//                    sResult = oReaction.getAnnotationString();
//                }
//                return sResult;
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                if (oRule.isSetAnnotation())
//                {
//                    sResult = oRule.getAnnotationString();
//                }
//                return sResult;
//            }
//
//            return "";
//            //throw new Exception("Invalid id. No element with the given id exists in the model.");
//        }
//
//        public static string[] getBuiltinFunctionInfo(string var0)
//        {
//            for (int i = 0; i < _oPredefinedFunctions.Length; i++)
//            {
//                if (_oPredefinedFunctions[i][0] == var0)
//                    return _oPredefinedFunctions[i];
//            }
//
//            throw new Exception("Invalid string name. There is no inbuilt function with that name: " + var0);
//        }
//
//        public static string[] getBuiltinFunctions()
//        {
//            string[] sResult = new string[_oPredefinedFunctions.Length];
//
//            int i;
//
//            for (i = 0; i < _oPredefinedFunctions.Length; i++)
//            {
//                sResult[i] = _oPredefinedFunctions[i][0];
//            }
//
//            return sResult;
//
//        }
//
//        public static string getCompartmentIdBySpeciesId(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies == NULL)
//            {
//                throw new Exception("The model does not have a species corresponding to the Id provided");
//            }
//            return oSpecies.getCompartment();
//        }
//
//        public static ArrayList getDerivedUnitDefinition(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                UnitDefinition oUnitDef = oSpecies.getDerivedUnitDefinition();
//                return returnUnitDefinition(oUnitDef);
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                UnitDefinition oUnitDef = oCompartment.getDerivedUnitDefinition();
//                return returnUnitDefinition(oUnitDef);
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                UnitDefinition oUnitDef = oParameter.getDerivedUnitDefinition();
//                return returnUnitDefinition(oUnitDef);
//            }
//            return new ArrayList();
//        }
//
//        private static ArrayList returnUnitDefinition(UnitDefinition oDefinition)
//        {
//            ArrayList oResultDef = new ArrayList();
//            for (int i = 0; i < oDefinition.getNumUnits(); i++)
//            {
//                Unit oUnit = oDefinition.getUnit(i);
//                if (oUnit != NULL)
//                {
//                    ArrayList oResult = new ArrayList();
//                    oResult.Add(libsbml.UnitKind_toString(oUnit.getKind()));
//                    oResult.Add(oUnit.getExponent());
//                    oResult.Add(oUnit.getMultiplier());
//                    oResult.Add(oUnit.getOffset());
//                    oResult.Add(oUnit.getScale());
//                    oResultDef.Add(oResult);
//                }
//            }
//            return oResultDef;
//        }
//
//        public static string getKineticLaw(int index)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            if (mModel.getNumReactions() <= (int)index)
//                throw new Exception("No Reaction for the provided index");
//            libsbmlcs.Reaction r = mModel.getReaction((int)index);
//            if (!r.isSetKineticLaw())
//                throw new Exception("No Kinetic Law present");
//            KineticLaw k = r.getKineticLaw();
//            if (!k.isSetFormula())
//                throw new Exception("No Formula present");
//            return k.getFormula();
//        }
//

//        public static ArrayList getListOfBoundarySpeciesIds()
//        {
//            ArrayList boundarySpeciesIdList = new ArrayList();
//
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (oSpecies.getBoundaryCondition())
//                {
//                    boundarySpeciesIdList.Add(GetId(oSpecies));
//                }
//            }
//
//            return boundarySpeciesIdList;
//        }
//
//        public static ArrayList getListOfErrors()
//        {
//            if (_oDoc == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            int nErrors = (int)_oDoc.getNumErrors();
//
//            ArrayList oErrorList = new ArrayList();
//            for (int i = 0; i < nErrors; i++)
//            {
//                oErrorList.Add(getNthError((int)i));
//            }
//            return oErrorList;
//        }
//
//        public static ArrayList getListOfFloatingSpecies()
//        {
//            ArrayList floatingSpeciesList = new ArrayList();
//
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (!oSpecies.getBoundaryCondition())
//                {
//                    ArrayList oSpeciesValues = new ArrayList();
//                    oSpeciesValues.Add(GetId(oSpecies));
//                    oSpeciesValues.Add(oSpecies.isSetInitialConcentration() ? oSpecies.getInitialConcentration() : oSpecies.getInitialAmount());
//                    oSpeciesValues.Add(oSpecies.isSetInitialConcentration());
//
//                    floatingSpeciesList.Add(oSpeciesValues);
//                }
//            }
//
//            return floatingSpeciesList;
//        }
//
//        public static ArrayList getListOfFloatingSpeciesIds()
//        {
//            ArrayList floatingSpeciesIdList = new ArrayList();
//
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (!oSpecies.getBoundaryCondition())
//                {
//                    floatingSpeciesIdList.Add(GetId(oSpecies));
//                }
//            }
//
//            return floatingSpeciesIdList;
//        }
//
//        public static ArrayList getListOfParameters()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            ArrayList paramStrValueList = new ArrayList();
//            int numOfGlobalParameters = (int)mModel.getNumParameters();
//            string paramStr; Parameter parameter; ArrayList tempStrValueList; double paramValue;
//            for (int i = 0; i < numOfGlobalParameters; i++)
//            {
//                parameter = mModel.getParameter(i);
//                paramStr = parameter.getId();
//                tempStrValueList = new ArrayList();
//                tempStrValueList.Add(paramStr);
//
//                if ((parameter.isSetValue()))
//                {
//                    paramValue = parameter.getValue();
//                }
//                else
//                {
//                    paramValue = 0.0;
//                }
//                tempStrValueList.Add(paramValue);
//
//                paramStrValueList.Add(tempStrValueList);
//            }
//
//            int numOfReactions = (int)mModel.getNumReactions();
//            libsbmlcs.Reaction r; KineticLaw kl;
//            for (int i = 0; i < numOfReactions; i++)
//            {
//                r = mModel.getReaction(i);
//                kl = r.getKineticLaw();
//                if (kl == NULL)
//                {
//                    continue;
//                }
//                else
//                {
//                    int numOfLocalParameters = (int)kl.getNumParameters();
//                    for (int j = 0; j < numOfLocalParameters; j++)
//                    {
//                        parameter = kl.getParameter(j);
//                        paramStr = parameter.getId();
//                        tempStrValueList = new ArrayList();
//
//                        tempStrValueList.Add(paramStr);
//                        if (parameter.isSetValue())
//                        {
//                            paramValue = parameter.getValue();
//                        }
//                        else
//                        {
//                            paramValue = 0.0;
//                        }
//                        tempStrValueList.Add(paramValue);
//                        paramStrValueList.Add(tempStrValueList);
//                    }
//                }
//            }
//            return paramStrValueList;
//        }
//
//        public static string getModelId()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return GetId(mModel);
//        }
//
//        public static string getModelName()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return GetName(mModel);
//        }
//
//        public static string getNotes(string sId)
//        {
//            if (mModel == NULL)
//            {
//                //throw new Exception("You need to load the model first");
//                return "";
//            }
//
//            string sResult = "";
//
//            if (mModel.getId() == sId || mModel.getName() == sId)
//            {
//                if (mModel.isSetNotes())
//                {
//                    sResult = mModel.getNotesString();
//                }
//                return sResult;
//
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                if (oSpecies.isSetNotes())
//                {
//                    sResult = oSpecies.getNotesString();
//                }
//                return sResult;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                if (oParameter.isSetNotes())
//                {
//                    sResult = oParameter.getNotesString();
//                }
//                return sResult;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                if (oCompartment.isSetNotes())
//                {
//                    sResult = oCompartment.getNotesString();
//                }
//                return sResult;
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                if (oReaction.isSetNotes())
//                {
//                    sResult = oReaction.getNotesString();
//                }
//                return sResult;
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                if (oRule.isSetNotes())
//                {
//                    sResult = oRule.getNotesString();
//                }
//                return sResult;
//            }
//
//            return "";
//            //throw new Exception("Invalid id. No element with the given id exists in the model.");
//        }
//
string NOMSupport::getNthBoundarySpeciesCompartmentName(const int& nIndex)
{
    if (mModel == NULL)
    {
        throw new RRException("You need to load the model first");
    }

//    int nCount = 0;
//    for (int i = 0; i < mModel->getNumSpecies(); i++)
//    {
//        libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//        if (oSpecies.getBoundaryCondition())
//        {
//            if (nCount == nIndex)
//            {
//                return oSpecies.getCompartment();
//            }
//            else
//            {
//                nCount++;
//            }
//        }
//    }
//    throw new Exception("The model does not have a boundary species corresponding to the index provided");
}

//        public static string getNthBoundarySpeciesId(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            int nCount = 0;
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (oSpecies.getBoundaryCondition())
//                {
//                    if (nCount == nIndex)
//                    {
//                        return GetId(oSpecies);
//                    }
//                    else
//                    {
//                        nCount++;
//                    }
//                }
//            }
//            throw new Exception("The model does not have a boundary species corresponding to the index provided");
//        }
//
//        public static string getNthBoundarySpeciesName(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            int nCount = 0;
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (oSpecies.getBoundaryCondition())
//                {
//                    if (nCount == nIndex)
//                    {
//                        return GetName(oSpecies);
//                    }
//                    else
//                    {
//                        nCount++;
//                    }
//                }
//            }
//            throw new Exception("The model does not have a boundary species corresponding to the index provided");
//        }
//
//        public static string getNthCompartmentId(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex < 0 || nIndex >= (int)mModel.getNumCompartments())
//            {
//                throw new Exception("Invalid input - Argument should be >= 0 and should be less than total number of compartments in the model");
//
//            }
//            Compartment oCompartment = mModel.getCompartment((int)nIndex);
//            return GetId(oCompartment);
//        }
//
//        public static string getNthCompartmentName(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex < 0 || nIndex >= (int)mModel.getNumCompartments())
//            {
//                throw new Exception("Invalid input - Argument should be >= 0 and should be less than total number of compartments in the model");
//
//            }
//            Compartment oCompartment = mModel.getCompartment((int)nIndex);
//            return GetName(oCompartment);
//        }
//
//        public static ArrayList getNthError(int nIndex)
//        {
//            if (_oDoc == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= _oDoc.getNumErrors())
//                throw new Exception("Index out of Bounds.");
//
//            SBMLError error = _oDoc.getError((int)nIndex);
//            ArrayList oResult = new ArrayList();
//
//            switch (error.getSeverity())
//            {
//                default:
//                case (int)libsbml.LIBSBML_SEV_INFO: oResult.Add("Advisory"); break;
//                case (int)libsbml.LIBSBML_SEV_WARNING: oResult.Add("Warning"); break;
//                case (int)libsbml.LIBSBML_SEV_FATAL: oResult.Add("Fatal"); break;
//                case (int)libsbml.LIBSBML_SEV_ERROR: oResult.Add("Error"); break;
//                case (int)libsbml.LIBSBML_SEV_SCHEMA_ERROR: oResult.Add("Error"); break;
//                case (int)libsbml.LIBSBML_SEV_GENERAL_WARNING: oResult.Add("Warning"); break;
//            }
//            oResult.Add((int)error.getLine());
//            oResult.Add((int)error.getColumn());
//            oResult.Add((int)error.getErrorId());
//            oResult.Add(error.getMessage());
//            return oResult;
//        }
//
//        public static bool getNthUseValuesFromTriggerTime(int arg)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            Event oEvent = mModel.getEvent((int)arg);
//
//            if (oEvent == NULL)
//            {
//                throw new Exception("The model does not have a Event corresponding to the index provided");
//            }
//            return oEvent.getUseValuesFromTriggerTime();
//        }
//
//        public static ArrayList getNthEvent(int arg)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            ArrayList triggerAssignmentsList = new ArrayList();
//            Event oEvent = mModel.getEvent((int)arg);
//
//            if (oEvent == NULL)
//            {
//                throw new Exception("The model does not have a Event corresponding to the index provided");
//            }
//
//            string trigger = libsbml.formulaToString(oEvent.getTrigger().getMath());
//            string delay;
//            triggerAssignmentsList.Add(trigger);
//
//            if (!oEvent.isSetDelay())
//            {
//                delay = "0";
//            }
//            else
//            {
//                Delay oDelay = oEvent.getDelay();
//                if (oDelay.isSetMath())
//                {
//                    delay = libsbml.formulaToString(oDelay.getMath());
//                }
//                else
//                {
//                    delay = "0";
//                }
//            }
//
//            triggerAssignmentsList.Add(delay);
//
//            int numEventAssignments = (int)oEvent.getNumEventAssignments();
//
//            for (int i = 0; i < numEventAssignments; i++)
//            {
//                ArrayList assignmentList = new ArrayList();
//
//                EventAssignment ea = oEvent.getEventAssignment(i);
//                string lValue = ea.getVariable();
//                string rValue = libsbml.formulaToString(ea.getMath());
//
//                assignmentList.Add(lValue);
//                assignmentList.Add(rValue);
//
//                triggerAssignmentsList.Add(assignmentList);
//
//            }
//
//            return triggerAssignmentsList;
//        }
//
string NOMSupport::getNthFloatingSpeciesCompartmentName(const int& nIndex)
{
    if (mModel == NULL)
    {
        throw RRException("You need to load the model first");
    }

    int nCount = 0;
    for (u_int i = 0; i < mModel->getNumSpecies(); i++)
    {
        Species *aSpecies = mModel->getSpecies(i);
        if (!aSpecies->getBoundaryCondition())
        {
            if (nCount == nIndex)
            {
                return aSpecies->getCompartment();
            }
            else
            {
                nCount++;
            }
        }
    }
    throw RRException("The model does not have a floating species corresponding to the index provided");
}

//        public static string getNthFloatingSpeciesId(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            int nCount = 0;
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (!oSpecies.getBoundaryCondition())
//                {
//                    if (nCount == nIndex)
//                    {
//                        return GetId(oSpecies);
//                    }
//                    else
//                    {
//                        nCount++;
//                    }
//                }
//            }
//            throw new Exception("The model does not have a floating species corresponding to the index provided");
//        }
//
//        public static string getNthFloatingSpeciesName(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            int nCount = 0;
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species oSpecies = mModel.getSpecies(i);
//                if (!oSpecies.getBoundaryCondition())
//                {
//                    if (nCount == nIndex)
//                    {
//                        return GetName(oSpecies);
//                    }
//                    else
//                    {
//                        nCount++;
//                    }
//                }
//            }
//            throw new Exception("The model does not have a floating species corresponding to the index provided");
//        }
//
//        public static ArrayList getNthFunctionDefinition(int arg)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (arg < 0 || arg >= (int)mModel.getNumFunctionDefinitions())
//            {
//                throw new Exception("Invalid input - Argument should be >= 0 and should be less than total number of Function Definitions in the model");
//            }
//
//            FunctionDefinition fnDefn = mModel.getFunctionDefinition((int)arg);
//
//            if (fnDefn == NULL)
//            {
//                throw new Exception("The model does not have a Function Definition corresponding to the index provided");
//            }
//
//            string fnId = fnDefn.getId();
//            string fnMath = libsbml.formulaToString(fnDefn.getBody());
//
//            ArrayList fnDefnList = new ArrayList();
//
//            fnDefnList.Add(fnId);
//
//            int numArgs = (int)fnDefn.getNumArguments();
//            ArrayList argList = new ArrayList();
//            for (int n = 0; n < numArgs; n++)
//            {
//                argList.Add(fnDefn.getArgument(n).getName());
//            }
//
//            fnDefnList.Add(argList);
//            fnDefnList.Add(fnMath);
//
//            return fnDefnList;
//        }
//
//        public static string getNthGlobalParameterId(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//
//            }
//
//            if (nIndex >= (int)mModel.getNumParameters())
//            {
//                throw new Exception("There is no parameter corresponding to the index you provided");
//
//            }
//
//            Parameter oParameter = mModel.getParameter((int)nIndex);
//            if (oParameter == NULL)
//            {
//                throw new Exception("There is no parameter corresponding to the index you provided");
//            }
//            return GetId(oParameter);
//        }
//
//        public static string getNthGlobalParameterName(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//
//            }
//
//            if (nIndex >= (int)mModel.getNumParameters())
//            {
//                throw new Exception("There is no parameter corresponding to the index you provided");
//
//            }
//
//            Parameter oParameter = mModel.getParameter((int)nIndex);
//            if (oParameter == NULL)
//            {
//                throw new Exception("There is no parameter corresponding to the index you provided");
//            }
//            return GetName(oParameter);
//        }
//
//        public static ArrayList getNthListOfModifiers(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            ArrayList modifierList = new ArrayList();
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            int numModifiers = (int)r.getNumModifiers();
//            for (int i = 0; i < numModifiers; i++)
//            {
//                modifierList.Add(r.getModifier(i).getSpecies());
//            }
//            return modifierList;
//        }
//
//        public static ArrayList getNthListOfProducts(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            ArrayList productList = new ArrayList();
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            int numProducts = (int)r.getNumProducts();
//            for (int i = 0; i < numProducts; i++)
//            {
//                libsbmlcs.SpeciesReference product = r.getProduct(i);
//                string stoichiometryMath = "";
//                if (product.isSetStoichiometryMath() && product.getStoichiometryMath().isSetMath())
//                    stoichiometryMath = libsbml.formulaToString(product.getStoichiometryMath().getMath());
//                ArrayList oTemp = new ArrayList(); oTemp.Add(product.getSpecies()); oTemp.Add(product.getStoichiometry());
//                oTemp.Add(stoichiometryMath);
//                productList.Add(oTemp);
//            }
//            return productList;
//        }
//
//        public static ArrayList getNthListOfReactants(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            ArrayList reactantList = new ArrayList();
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            int numReactants = (int)r.getNumReactants();
//            for (int i = 0; i < numReactants; i++)
//            {
//                libsbmlcs.SpeciesReference reactant = r.getReactant(i);
//                string stoichiometryMath = "";
//                if (reactant.isSetStoichiometryMath() && reactant.getStoichiometryMath().isSetMath())
//                    stoichiometryMath = libsbml.formulaToString(reactant.getStoichiometryMath().getMath());
//                ArrayList oTemp = new ArrayList(); oTemp.Add(reactant.getSpecies()); oTemp.Add(reactant.getStoichiometry());
//                oTemp.Add(stoichiometryMath);
//                reactantList.Add(oTemp);
//            }
//            return reactantList;
//        }
//
//        public static bool getNthParameterHasValue(int nReactionIndex, int nParameterIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nReactionIndex < 0 || nReactionIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction((int)nReactionIndex);
//            KineticLaw kl = oReaction.getKineticLaw();
//
//            if (nParameterIndex < 0 || nParameterIndex >= (int)kl.getNumParameters())
//            {
//                throw new Exception("Index exceeds the number of Parameters in the list");
//            }
//
//            return kl.getParameter((int)nParameterIndex).isSetValue();
//
//        }
//
//        public static string getNthParameterId(int nReactionIndex, int nParameterIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nReactionIndex < 0 || nReactionIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction((int)nReactionIndex);
//            KineticLaw kl = oReaction.getKineticLaw();
//
//            if (nParameterIndex < 0 || nParameterIndex >= (int)kl.getNumParameters())
//            {
//                throw new Exception("Index exceeds the number of Parameters in the list");
//            }
//
//            return kl.getParameter((int)nParameterIndex).getId();
//
//        }
//
//        public static string getNthParameterName(int nReactionIndex, int nParameterIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nReactionIndex < 0 || nReactionIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction((int)nReactionIndex);
//            KineticLaw kl = oReaction.getKineticLaw();
//
//            if (nParameterIndex < 0 || nParameterIndex >= (int)kl.getNumParameters())
//            {
//                throw new Exception("Index exceeds the number of Parameters in the list");
//            }
//
//            return kl.getParameter((int)nParameterIndex).getName();
//        }
//
//        public static double getNthParameterValue(int nReactionIndex, int nParameterIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nReactionIndex < 0 || nReactionIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction((int)nReactionIndex);
//            KineticLaw kl = oReaction.getKineticLaw();
//
//            if (nParameterIndex < 0 || nParameterIndex >= (int)kl.getNumParameters())
//            {
//                throw new Exception("Index exceeds the number of Parameters in the list");
//            }
//
//            return kl.getParameter((int)nParameterIndex).getValue();
//
//        }
//
//        public static string getNthProductName(int nIndex, int nProduct)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            libsbmlcs.SpeciesReference oRef = r.getProduct((int)nProduct);
//            if (oRef == NULL)
//                throw new Exception("No product for the provided index.");
//            return oRef.getSpecies();
//        }
//
//        public static int getNthProductStoichiometry(int nIndex, int nProduct)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            libsbmlcs.SpeciesReference oRef = r.getProduct((int)nProduct);
//            if (oRef == NULL)
//                throw new Exception("No product for the provided index.");
//            return (int)oRef.getStoichiometry();
//        }
//
//        public static double getNthProductStoichiometryDouble(int nIndex, int nProduct)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            libsbmlcs.SpeciesReference oRef = r.getProduct((int)nProduct);
//            if (oRef == NULL)
//                throw new Exception("No product for the provided index.");
//            return oRef.getStoichiometry();
//        }
//
//        public static string getNthReactantName(int nIndex, int nReactant)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            libsbmlcs.SpeciesReference oRef = r.getReactant((int)nReactant);
//            if (oRef == NULL)
//                throw new Exception("No reactant for the provided index.");
//            return oRef.getSpecies();
//        }
//
//        public static int getNthReactantStoichiometry(int nIndex, int nReactant)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            libsbmlcs.SpeciesReference oRef = r.getReactant((int)nReactant);
//            if (oRef == NULL)
//                throw new Exception("No reactant for the provided index.");
//            return (int)oRef.getStoichiometry();
//        }
//
//        public static double getNthReactantStoichiometryDouble(int nIndex, int nReactant)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            libsbmlcs.SpeciesReference oRef = r.getReactant((int)nReactant);
//            if (oRef == NULL)
//                throw new Exception("No reactant for the provided index.");
//            return oRef.getStoichiometry();
//        }
//
//        public static string getNthReactionId(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            return GetId(r);
//        }
//
//        public static string getNthReactionName(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            return GetName(r);
//        }
//
//        public static Pair<string, string> getNthInitialAssignmentPair(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            InitialAssignment oAssignment = mModel.getInitialAssignment((int)nIndex);
//            if (oAssignment == NULL)
//                throw new Exception("The model does not have an InitialAssignment corresponding to the index provided");
//
//            if (!oAssignment.isSetMath())
//                throw new Exception("The InitialAssignment contains no math.");
//
//            return new Pair<string, string>(oAssignment.getSymbol(), libsbml.formulaToString(oAssignment.getMath()));
//        }
//
//        public static string getNthInitialAssignment(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            InitialAssignment oAssignment = mModel.getInitialAssignment((int)nIndex);
//            if (oAssignment == NULL)
//                throw new Exception("The model does not have an InitialAssignment corresponding to the index provided");
//
//            if (!oAssignment.isSetMath())
//                throw new Exception("The InitialAssignment contains no math.");
//
//            return oAssignment.getSymbol() + " = " + libsbml.formulaToString(oAssignment.getMath());
//
//        }
//
//        public static string getNthConstraint(int nIndex, out string sMessage)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            Constraint oConstraint = mModel.getConstraint((int)nIndex);
//            if (oConstraint == NULL)
//                throw new Exception("The model does not have a constraint corresponding to the index provided");
//
//            if (!oConstraint.isSetMath())
//                throw new Exception("The constraint does not provide math.");
//
//            if (!oConstraint.isSetMessage())
//                sMessage = "Constraint: " + nIndex.ToString() + " was violated.";
//            else
//                sMessage = oConstraint.getMessage().toString();
//            return libsbml.formulaToString(oConstraint.getMath());
//
//        }
//
//        public static string getNthRule(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            Rule oRule = mModel.getRule((int)nIndex);
//            if (oRule == NULL)
//            {
//                throw new Exception("The model does not have a Rule corresponding to the index provided");
//            }
//
//            int type = oRule.getTypeCode();
//
//            switch (type)
//            {
//                case libsbml.SBML_PARAMETER_RULE:
//                case libsbml.SBML_SPECIES_CONCENTRATION_RULE:
//                case libsbml.SBML_COMPARTMENT_VOLUME_RULE:
//                case libsbml.SBML_ASSIGNMENT_RULE:
//                case libsbml.SBML_RATE_RULE:
//                    {
//                        string lValue = oRule.getVariable();
//                        string rValue = oRule.getFormula();
//
//                        return lValue + " = " + rValue;
//                    }
//                case libsbml.SBML_ALGEBRAIC_RULE:
//                    {
//                        string rValue = oRule.getFormula();
//                        return rValue + " = 0";
//                    }
//
//
//                default:
//                    break;
//            }
//
//            return "";
//        }
//
//        public static string getNthRuleType(int arg)
//        {
//            string result = "";
//            Rule rule;
//
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            rule = mModel.getRule((int)arg);
//
//            if (rule == NULL)
//            {
//                throw new Exception("The model does not have a Rule corresponding to the index provided");
//            }
//
//            int type = rule.getTypeCode();
//            if (type == libsbml.SBML_PARAMETER_RULE)
//            {
//                result = "Parameter_Rule";
//            }
//
//            if (type == libsbml.SBML_SPECIES_CONCENTRATION_RULE)
//            {
//                result = "Species_Concentration_Rule";
//            }
//
//            if (type == libsbml.SBML_COMPARTMENT_VOLUME_RULE)
//            {
//                result = "Compartment_Volume_Rule";
//            }
//
//            if (type == libsbml.SBML_ASSIGNMENT_RULE)
//            {
//                result = "Assignment_Rule";
//            }
//
//            if (type == libsbml.SBML_ALGEBRAIC_RULE)
//            {
//                result = "Algebraic_Rule";
//            }
//
//            if (type == libsbml.SBML_RATE_RULE)
//            {
//                result = "Rate_Rule";
//            }
//            return result;
//        }
//
//        public static int getNumBoundarySpecies()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumSpeciesWithBoundaryCondition();
//        }
//
//        public static int getNumCompartments()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumCompartments();
//        }
//
//        public static int getNumErrors()
//        {
//            if (_oDoc == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)_oDoc.getNumErrors();
//        }
//
//        public static int getNumEvents()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumEvents();
//        }
//
//        public static int getNumFloatingSpecies()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumSpecies() - (int)mModel.getNumSpeciesWithBoundaryCondition();
//
//        }
//
//        public static int getNumInitialAssignments()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumInitialAssignments();
//        }
//
//        public static int getNumConstraints()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumConstraints();
//        }
//
//        public static int getNumFunctionDefinitions()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumFunctionDefinitions();
//
//        }
//
//        public static int getNumGlobalParameters()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumParameters();
//
//        }
//
//        public static int getNumParameters(int var0)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            if (var0 > mModel.getNumReactions())
//                throw new Exception("Reaction does not exist");
//            libsbmlcs.Reaction r = mModel.getReaction((int)var0);
//            if (!r.isSetKineticLaw()) return 0;
//            return (int)r.getKineticLaw().getNumParameters();
//
//        }
//
//        public static int getNumProducts(int var0)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            if (var0 > mModel.getNumReactions())
//                throw new Exception("Reaction does not exist");
//            libsbmlcs.Reaction r = mModel.getReaction((int)var0);
//            return (int)r.getNumProducts();
//        }
//
//        public static int getNumReactants(int var0)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            if (var0 > mModel.getNumReactions())
//                throw new Exception("Reaction does not exist");
//            libsbmlcs.Reaction r = mModel.getReaction((int)var0);
//            return (int)r.getNumReactants();
//        }
//
//        public static int getNumReactions()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumReactions();
//        }
//
//        public static int getNumRules()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            return (int)mModel.getNumRules();
//        }
//
//        public static string getOutsideCompartment(string var0)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//
//            }
//
//            Compartment oCompartment = mModel.getCompartment(var0);
//            if (oCompartment == NULL)
//            {
//                throw new Exception("There is no compartment corresponding to the input argument.");
//            }
//            return oCompartment.getOutside();
//
//        }
//
//        public static string getParamPromotedSBML(string sArg)
//        {
//            SBMLDocument oSBMLDoc = NULL;
//            Model oModel = NULL;
//
//            oSBMLDoc = libsbml.readSBMLFromString(sArg);
//            try
//            {
//                if (oSBMLDoc.getLevel() == 1)
//                    oSBMLDoc.setLevelAndVersion(2, 1);
//                //oSBMLDoc.setLevelAndVersion(2, 1);
//                oModel = oSBMLDoc.getModel();
//
//                if (oModel == NULL)
//                {
//                    throw new Exception("SBML Validation failed");
//                }
//                else
//                {
//                    modifyKineticLaws(oSBMLDoc, oModel);
//
//                    changeTimeSymbol(oModel, "time");
//
//                    return libsbml.writeSBMLToString(oSBMLDoc);
//                }
//            }
//            finally
//            {
//                GC.Collect();
//                if (oSBMLDoc != NULL)
//                    oSBMLDoc.Dispose();
//            }
//        }
//
//        private static void modifyKineticLawsForLocalParameters(KineticLaw oLaw, string reactionId, Model oModel)
//        {
//            int numLocalParameters = (int)oLaw.getNumLocalParameters();
//            if (numLocalParameters > 0)
//            {
//                var oList = new StringCollection();
//                for (int j = numLocalParameters; j > 0; j--)
//                {
//                    var localParameter = (LocalParameter)oLaw.getLocalParameter(j - 1).clone();
//                    string parameterId = localParameter.getId();// GetId(localParameter);
//                    string sPrefix = reactionId + "_";
//                    if (!oLaw.isSetMath())
//                    {
//                        if (oLaw.isSetFormula())
//                        {
//                            ASTNode node = libsbml.readMathMLFromString(oLaw.getFormula());
//                            ChangeParameterName(node, parameterId, sPrefix);
//                            string sNode = libsbml.formulaToString(node);
//                            oLaw.setFormula(sNode);
//                        }
//                    }
//                    else
//                    {
//                        ChangeParameterName(oLaw.getMath(), parameterId, sPrefix);
//                    }
//
//                    Parameter p = oModel.createParameter();
//                    p.setId(sPrefix + parameterId);
//                    p.setNotes(localParameter.getNotesString());
//                    p.setAnnotation(localParameter.getAnnotationString());
//                    p.setConstant(true);
//                    if (localParameter.isSetSBOTerm()) p.setSBOTerm(localParameter.getSBOTerm());
//                    if (localParameter.isSetName()) p.setName(localParameter.getName());
//                    if (localParameter.isSetMetaId()) p.setMetaId(localParameter.getMetaId());
//                    if (localParameter.isSetValue()) p.setValue(localParameter.getValue());
//                    if (localParameter.isSetUnits()) p.setUnits(localParameter.getUnits());
//
//                    var oTemp = (LocalParameter)oLaw.getListOfLocalParameters().remove(j - 1);
//                    if (oTemp != NULL) oTemp.Dispose();
//
//                    oModel.addParameter(p);
//                    if (localParameter != NULL) localParameter.Dispose();
//                }
//            }
//        }
//
//        private static void modifyKineticLawsForReaction(KineticLaw oLaw, string reactionId, Model oModel)
//        {
//            int numLocalParameters = (int)oLaw.getNumParameters();
//            if (numLocalParameters > 0)
//            {
//                StringCollection oList = new StringCollection();
//                for (int j = numLocalParameters; j > 0; j--)
//                {
//                    Parameter parameter = (Parameter)oLaw.getParameter(j - 1).clone();
//                    string parameterId = GetId(parameter);
//                    string sPrefix = reactionId + "_";
//                    if (!oLaw.isSetMath())
//                    {
//                        if (oLaw.isSetFormula())
//                        {
//                            ASTNode node = libsbml.readMathMLFromString(oLaw.getFormula());
//                            ChangeParameterName(node, parameterId, sPrefix);
//                            string sNode = libsbml.formulaToString(node);
//                            oLaw.setFormula(sNode);
//                        }
//                    }
//                    else
//                    {
//                        ChangeParameterName(oLaw.getMath(), parameterId, sPrefix);
//                    }
//                    Parameter oTemp = (Parameter)oLaw.getListOfParameters().remove(j - 1);
//                    if (oTemp != NULL) oTemp.Dispose();
//                    parameter.setId(sPrefix + parameterId);
//                    //oModel.getListOfParameters().append(parameter);
//                    //oModel.getListOfParameters().appendAndOwn(parameter);
//                    oModel.addParameter(parameter);
//                    if (parameter != NULL) parameter.Dispose();
//                }
//            }
//        }
//
//        private static void modifyKineticLaws(SBMLDocument oSBMLDoc, Model oModel)
//        {
//            int numOfReactions = (int)oModel.getNumReactions();
//            for (int i = 0; i < numOfReactions; i++)
//            {
//                libsbmlcs.Reaction oReaction = oModel.getReaction(i);
//                string sId = GetId(oReaction);
//                KineticLaw oLaw = oReaction.getKineticLaw();
//                if (oLaw == NULL) { if (oReaction != NULL)oReaction.Dispose(); continue; }
//                modifyKineticLawsForLocalParameters(oLaw, sId, oModel);
//                modifyKineticLawsForReaction(oLaw, sId, oModel);
//                if (oLaw != NULL) oLaw.Dispose(); if (oReaction != NULL) oReaction.Dispose();
//            }
//
//        }
//
//        private static void ChangeParameterName(ASTNode node, string sParameterName, string sPrefix)
//        {
//            int c;
//
//            if (node.isName() && node.getName() == sParameterName)
//            {
//                node.setName(sPrefix + sParameterName);
//            }
//
//            for (c = 0; c < node.getNumChildren(); c++)
//            {
//                ChangeParameterName(node.getChild(c), sParameterName, sPrefix);
//            }
//        }
//
//        public static string getSBML()
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (_ParameterSets != NULL && mModel != NULL)
//                _ParameterSets.AddToModel(mModel);
//
//            return libsbml.writeSBMLToString(_oDoc);
//        }
//
//        public static int getSBOTerm(string sId)
//        {
//            if (mModel == NULL)
//            {
//                return 0;
//                //throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                return oSpecies.getSBOTerm();
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return oParameter.getSBOTerm();
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return oCompartment.getSBOTerm();
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                return oReaction.getSBOTerm();
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                return oRule.getSBOTerm();
//            }
//
//            if (mModel.getId() == sId)
//                return mModel.getSBOTerm();
//
//            return 0;
//            //throw new Exception("Invalid id. No element with the given id exists in the model.");
//
//        }
//
//        public static void TestASTTime()
//        {
//            var mathML = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n            <csymbol encoding=\"text\" definitionURL=\"http://www.sbml.org/sbml/symbols/time\"> time </csymbol>\n            </math>\n";
//            var node = libsbml.readMathMLFromString(mathML);
//
//            System.Diagnostics.Debug.WriteLine(
//                string.Format("Node Type: {0}, AST_NAME_TIME: {1}, AST_NAME: {2}, AST_NAME_AVOGADRO: {3}",
//                node.getType(),
//                libsbml.AST_NAME_TIME,
//                libsbml.AST_NAME,
//                libsbml.AST_NAME_AVOGADRO));
//
//
//        }
//
//
//        public static double getValue(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                if (oSpecies.isSetInitialAmount())
//                    return oSpecies.getInitialAmount();
//                else
//                    return oSpecies.getInitialConcentration();
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return oCompartment.getVolume();
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return oParameter.getValue();
//            }
//
//            for (int i = 0; i < mModel.getNumReactions(); i++)
//            {
//                var reaction = mModel.getReaction(i);
//                for (int j = 0; j < reaction.getNumReactants(); j++)
//                {
//                    var reference = reaction.getReactant(j);
//                    if (reference.isSetId() && reference.getId() == sId)
//                    {
//                        if (reference.isSetStoichiometry())
//                        {
//                            return reference.getStoichiometry();
//                        }
//                        else return 1;
//                    }
//                }
//                for (int j = 0; j < reaction.getNumProducts(); j++)
//                {
//                    var reference = reaction.getProduct(j);
//                    if (reference.isSetId() && reference.getId() == sId)
//                    {
//                        if (reference.isSetStoichiometry())
//                        {
//                            return reference.getStoichiometry();
//                        }
//                        else return 1;
//                    }
//                }
//            }
//
//
//            throw new Exception("Invalid string name. The id '" + sId + "' does not exist in the model");
//        }
//
//        public static bool hasInitialAmount(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//                return oSpecies.isSetInitialAmount();
//
//            throw new Exception("Invalid string name. The name is not a valid id/name of a floating / boundary species.");
//
//        }
//
//        public static bool hasInitialConcentration(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//                return oSpecies.isSetInitialConcentration();
//
//            throw new Exception("Invalid string name. The name is not a valid id/name of a floating / boundary species.");
//        }
//
//        public static bool hasSBOTerm(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                return oSpecies.isSetSBOTerm();
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return oParameter.isSetSBOTerm();
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return oCompartment.isSetSBOTerm();
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                return oReaction.isSetSBOTerm();
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                return oRule.isSetSBOTerm();
//            }
//
//            throw new Exception("Invalid id. No element with the given id exists in the model.");
//
//        }
//
//        public static bool hasValue(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                return (oSpecies.isSetInitialAmount() || oSpecies.isSetInitialAmount());
//
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return oCompartment.isSetVolume();
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return oParameter.isSetValue();
//            }
//
//            throw new Exception("Invalid string name. The id '" + sId + "' does not exist in the model");
//        }
//
//        public static bool isConstantImpl(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//                return oSpecies.getConstant();
//
//            throw new Exception("Invalid string name. The name is not a valid id/name of a floating / boundary species.");
//        }
//
//        public static bool isReactionReversible(int nIndex)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            ArrayList productList = new ArrayList();
//
//            if (nIndex >= (int)mModel.getNumReactions())
//            {
//                throw new Exception("There is no reaction corresponding to the index you provided");
//            }
//
//            libsbmlcs.Reaction r = mModel.getReaction((int)nIndex);
//            return r.getReversible();
//        }
//
//        private static void GetSymbols(ASTNode node, List<System.String> list)
//        {
//            if (node.isName())
//            {
//                var name = node.getName();
//                if (!list.Contains(name))
//                    list.Add(name);
//            }
//
//            for (int i = 0; i < node.getNumChildren(); i++)
//            {
//                GetSymbols(node.getChild(i), list);
//            }
//
//        }
//        /// <summary>
//        /// Returns the list of all names contained in the ASTNode
//        /// </summary>
//        /// <param name="math">ASTnode</param>
//        /// <returns>List of all symbols</returns>
//        public static List<string> GetSymbols(ASTNode math)
//        {
//            var result = new List<string>();
//            if (math == NULL) return result;
//
//            GetSymbols(math, result);
//
//            return result;
//        }
//
//        /// <summary>
//        /// Reorders assignment rules. In SBML assignment rules does not have to appear in the correct order.
//        /// That is you could have an assignment rule A = B, and a rule B = C. Now the result would differ,
//        /// if the first rule is evaluated before the second. Thus the rules will be reordered such that
//        /// this will be taken care of.
//        /// </summary>
//        /// <param name="assignmentRules">assignment rules in original ordering</param>
//        /// <returns>assignment rules in independent order</returns>
//        public static List<Rule> ReorderAssignmentRules(List<Rule> assignmentRules)
//        {
//            if (assignmentRules == NULL || assignmentRules.Count < 2)
//                return assignmentRules;
//
//            var result = new List<Rule>();
//            var allSymbols = new Dictionary<int, List<string>>();
//            var map = new Dictionary<string, List<string>>();
//            var idList = new List<string>();
//
//            // read id list, initialize all symbols
//            for (int index = 0; index < assignmentRules.Count; index++)
//            {
//                var rule = (AssignmentRule)assignmentRules[index];
//                var variable = rule.getVariable();
//                if (!rule.isSetMath())
//                    allSymbols[index] = new List<string>();
//                else
//                    allSymbols[index] = GetSymbols(rule.getMath());
//                idList.Add(variable);
//                map[variable] = new List<string>();
//            }
//
//            // initialize order array
//            var order = new int[assignmentRules.Count];
//            for (int i = 0; i < assignmentRules.Count; i++)
//            {
//                order[i] = i;
//            }
//
//            // build dependency graph
//            foreach (var id in idList)
//            {
//                for (int index = 0; index < assignmentRules.Count; index++)
//                    if (allSymbols[index].Contains(id))
//                        map[(assignmentRules[index]).getVariable()].Add(id);
//            }
//
//            // print dependency graph
//            //foreach (var id in idList)
//            //{
//            //    System.Diagnostics.Debug.Write(id + " depends on: ");
//            //    foreach (var symbol in map[id])
//            //    {
//            //        System.Diagnostics.Debug.Write(symbol + ", ");
//            //    }
//            //    System.Diagnostics.Debug.WriteLine("");
//            //}
//
//
//            // sort
//            bool changed = true;
//            while (changed)
//            {
//                changed = false;
//                for (int i = 0; i < order.Length; i++)
//                {
//
//                    var first = order[i];
//                    for (int j = i + 1; j < order.Length; j++)
//                    {
//                        var second = order[j];
//
//                        var secondVar = assignmentRules[second].getVariable();
//                        var firstVar = assignmentRules[first].getVariable();
//
//                        if (map[firstVar].Contains(secondVar))
//                        {
//                            // found dependency, swap and start over
//                            order[i] = second;
//                            order[j] = first;
//
//                            changed = true;
//                            break;
//                        }
//                    }
//
//                    // if swapped start over
//                    if (changed)
//                        break;
//                }
//            }
//
//            // create new order
//            for (int i = 0; i < order.Length; i++)
//                result.Add(assignmentRules[order[i]]);
//
//
//            return result;
//
//        }
//
//        /// <summary>
//        /// Reorders the Rules of the model in such a way, that AssignmentRules are calculated first, followed by Rate Rules and Algebraic Rules.
//        /// </summary>
//        /// <param name="doc">the document to use</param>
//        /// <param name="model">the model to use</param>
//        public static void ReorderRules(SBMLDocument doc, Model model)
//        {
//            var numRules = (int)model.getNumRules();
//
//            var assignmentRules = new List<Rule>();
//            var rateRules = new List<Rule>();
//            var algebraicRules = new List<Rule>();
//
//            for (int i = numRules - 1; i >= 0; i--)
//            {
//                var current = model.removeRule(i);
//                switch (current.getTypeCode())
//                {
//                    case libsbml.SBML_ALGEBRAIC_RULE:
//                        algebraicRules.Insert(0, current);
//                        break;
//                    case libsbml.SBML_RATE_RULE:
//                        rateRules.Insert(0, current);
//                        break;
//                    default:
//                    case libsbml.SBML_ASSIGNMENT_RULE:
//                        assignmentRules.Insert(0, current);
//                        break;
//                }
//            }
//            assignmentRules = ReorderAssignmentRules(assignmentRules);
//            assignmentRules.ForEach(item => model.addRule(item));
//            rateRules.ForEach(item => model.addRule(item));
//            algebraicRules.ForEach(item => model.addRule(item));
//
//        }
//
//        public static void loadSBML(string var0, string sTimeSymbol)
//        {
//            loadSBML(var0);
//            changeTimeSymbol(mModel, sTimeSymbol);
//            changeSymbol(mModel, "avogadro", libsbml.AST_NAME_AVOGADRO);
//            modifyKineticLaws(_oDoc, mModel);
//            ReorderRules(_oDoc, mModel);
//
//            BuildSymbolTable();
//
//        }
//
//        private static void changeTimeSymbol(Model model, string timeSymbol)
//        {
//            changeSymbol(model, timeSymbol, libsbml.AST_NAME_TIME);
//        }
//
//        public static void loadParameterPromotedSBML(string var0, string sTimeSymbol)
//        {
//            loadSBML(var0);
//            changeTimeSymbol(mModel, sTimeSymbol);
//            changeSymbol(mModel, "avogadro", libsbml.AST_NAME_AVOGADRO);
//            modifyKineticLaws(_oDoc, mModel);
//
//            BuildSymbolTable();
//
//        }
//
//        public static void loadFromFile(string fileName)
//        {
//            loadSBML(File.ReadAllText(fileName));
//        }
//
//        static Hashtable _symbolTable = new Hashtable();
//
//        static private void BuildSymbolTable()
//        {
//            _symbolTable = new Hashtable();
//
//            // Read CompartmentSymbols
//            for (int i = 0; i < mModel.getNumCompartments(); i++)
//            {
//                Compartment temp = mModel.getCompartment(i);
//
//                SBMLSymbol symbol = new SBMLSymbol();
//                symbol.Id = temp.getId();
//                if (temp.isSetSize()) symbol.Value = temp.getSize();
//                symbol.InitialAssignment = GetInitialAssignmentFor(symbol.Id);
//                symbol.Rule = GetRuleFor(symbol.Id);
//                symbol.Type = SBMLType.Compartment;
//
//                _symbolTable[symbol.Id] = symbol;
//            }
//
//            // Read Parameter Symbols
//            for (int i = 0; i < mModel.getNumParameters(); i++)
//            {
//                Parameter temp = mModel.getParameter(i);
//
//                SBMLSymbol symbol = new SBMLSymbol();
//                symbol.Id = temp.getId();
//                if (temp.isSetValue()) symbol.Value = temp.getValue();
//                symbol.InitialAssignment = GetInitialAssignmentFor(symbol.Id);
//                symbol.Rule = GetRuleFor(symbol.Id);
//                symbol.Type = SBMLType.Parameter;
//
//                _symbolTable[symbol.Id] = symbol;
//            }
//
//            // Read Species Symbols
//            for (int i = 0; i < mModel.getNumSpecies(); i++)
//            {
//                libsbmlcs.Species temp = mModel.getSpecies(i);
//
//                SBMLSymbol symbol = new SBMLSymbol();
//                symbol.Id = temp.getId();
//                if (temp.isSetInitialConcentration()) symbol.Concentration = temp.getInitialConcentration();
//                if (temp.isSetInitialAmount()) symbol.Amount = temp.getInitialAmount();
//                symbol.InitialAssignment = GetInitialAssignmentFor(symbol.Id);
//                symbol.Rule = GetRuleFor(symbol.Id);
//                symbol.Type = SBMLType.Species;
//
//                _symbolTable[symbol.Id] = symbol;
//            }
//
//            LookForDependencies();
//        }
//
//        private static void LookForDependencies()
//        {
//            // Go through each found Id, and test for dependencies
//            foreach (string sbmlId in _symbolTable.Keys)
//            {
//                UpdateDependencies(sbmlId);
//            }
//        }
//
//        private static void UpdateDependencies(string sbmlId)
//        {
//            SBMLSymbol current = (SBMLSymbol)_symbolTable[sbmlId];
//            if (current == NULL) return;
//
//            if (current.HasInitialAssignment)
//            {
//                List<string> dependentSymbols = GetSymbols(current.InitialAssignment);
//                foreach (string dependency in dependentSymbols)
//                    if (dependency != current.Id)
//                        current.Dependencies.Add((SBMLSymbol)_symbolTable[dependency]);
//            }
//
//            if (current.HasRule)
//            {
//                List<string> dependentSymbols = GetSymbols(current.Rule);
//                foreach (string dependency in dependentSymbols)
//                    if (dependency != current.Id)
//                        current.Dependencies.Add((SBMLSymbol)_symbolTable[dependency]);
//            }
//        }
//
//        private static List<string> GetSymbols(string formula)
//        {
//            List<string> sResult = new List<string>();
//            if (string.IsNullOrEmpty(formula)) return sResult;
//            ASTNode node = libsbml.parseFormula(formula);
//
//            addDependenciesToList(node, sResult);
//
//            return sResult;
//        }
//
//        private static void addDependenciesToList(ASTNode node, List<string> sResult)
//        {
//            for (int i = 0; i < node.getNumChildren(); i++)
//            {
//                addDependenciesToList(node.getChild(i), sResult);
//            }
//
//            if (node.isName() && _symbolTable.ContainsKey(node.getName()))
//                sResult.Add(node.getName());
//        }
//
//        private static string GetRuleFor(string sbmlId)
//        {
//            for (int i = 0; i < mModel.getNumRules(); i++)
//            {
//                Rule oRule = mModel.getRule(i);
//                switch (oRule.getTypeCode())
//                {
//                    case libsbml.SBML_PARAMETER_RULE:
//                    case libsbml.SBML_SPECIES_CONCENTRATION_RULE:
//                    case libsbml.SBML_COMPARTMENT_VOLUME_RULE:
//                    case libsbml.SBML_ASSIGNMENT_RULE:
//                        //case libsbml.SBML_RATE_RULE:
//                        {
//                            if (sbmlId == oRule.getVariable())
//                                return oRule.getFormula();
//                            break;
//                        }
//                    //case libsbml.SBML_ALGEBRAIC_RULE:
//                    //    {
//                    //        string rValue = oRule.getFormula();
//                    //        return rValue + " = 0";
//                    //    }
//
//
//                    default:
//                        break;
//                }
//
//            }
//
//            return NULL;
//        }
//
//        private static string GetInitialAssignmentFor(string sbmlId)
//        {
//            for (int i = 0; i < mModel.getNumInitialAssignments(); i++)
//            {
//                InitialAssignment oAssignment = mModel.getInitialAssignment(i);
//                if (oAssignment.getSymbol() == sbmlId && oAssignment.isSetMath())
//                    return libsbml.formulaToString(oAssignment.getMath());
//            }
//            return NULL;
//        }
//
//        private static List<string> _Namespaces;
//
//        public static List<string> Namespaces
//        {
//            get { return _Namespaces; }
//            set
//            {
//                _Namespaces = value;
//            }
//        }
//
//        public static void loadSBML(string var0)
//        {
//            byte[] oBuffer = ASCIIEncoding.ASCII.GetBytes(var0.ToCharArray());
//            System.IO.MemoryStream oStream = new System.IO.MemoryStream(oBuffer);
//            string sTemp = new System.IO.StreamReader(oStream).ReadToEnd();
//            if (_oDoc != NULL)
//            {
//                try
//                {
//                    if (mModel != NULL)
//                    {
//                        mModel.Dispose();
//                        mModel = NULL;
//                    }
//                    _oDoc.Dispose();
//                    _oDoc = NULL;
//                }
//                catch
//                {
//                    // never mind ....
//                }
//
//            }
//
//            // we also need to collect all namespaces from the file, or rather
//            // all registered prefixes:
//
//            //string regex=@"^.*?xmlns:(?<prefix>\w+?)^.*= "(?<namespace>.+?).*?$";
//            string regex = "xmlns:(?<prefix>\\w+?)\\s*=\\s*(?:\"(?<namespace>[^\"]*)\"|(?<namespace>\\S+))";
//            RegexOptions options = RegexOptions.IgnoreCase | RegexOptions.Multiline | RegexOptions.IgnorePatternWhitespace | RegexOptions.CultureInvariant;
//            string input = var0;
//
//            Namespaces = new List<string>();
//            List<string> prefixes = new List<string>();
//            MatchCollection matches = Regex.Matches(input, regex, options);
//            foreach (Match match in matches)
//            {
//                //Console.WriteLine(match.Value);
//                string prefix = match.Value.Substring(0, match.Value.IndexOf('='));
//                if (!prefixes.Contains(prefix) && !Namespaces.Contains(match.Value))
//                {
//                    Namespaces.Add(match.Value);
//                    prefixes.Add(prefix);
//                }
//
//                //Console.WriteLine("prefix:" + match.Groups["prefix"].Value);
//
//                //Console.WriteLine("namespace:" + match.Groups["namespace"].Value);
//
//            }
//
//            _ParameterSets = new ParameterSets(sTemp);
//
//            _oDoc = libsbml.readSBMLFromString(sTemp);
//            mModel = _oDoc.getModel();
//            if (mModel == NULL)
//            {
//                throw new Exception(validateSBML(sTemp));
//            }
//        }
//
//        private static ParameterSets _ParameterSets;
//
//        public static ParameterSets ParameterSets
//        {
//            get { return _ParameterSets; }
//            set { _ParameterSets = value; }
//        }
//
//        public static void setAnnotation(string sId, string sAnnotation)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (mModel.getId() == sId || mModel.getName() == sId)
//            {
//                mModel.setAnnotation(sAnnotation);
//                return;
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                oSpecies.setAnnotation(sAnnotation);
//                return;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                oParameter.setAnnotation(sAnnotation);
//                return;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                oCompartment.setAnnotation(sAnnotation);
//                return;
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                oReaction.setAnnotation(sAnnotation);
//                return;
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                oRule.setAnnotation(sAnnotation);
//                return;
//            }
//
//            throw new Exception("Invalid id. No element with the given id exists in the model.");
//        }
//
//        public static void setModelId(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//            mModel.setId(sId);
//        }
//
//        public static void setNotes(string sId, string sNotes)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            if (mModel.getId() == sId || mModel.getName() == sId)
//            {
//                mModel.setNotes(sNotes);
//                return;
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                oSpecies.setNotes(sNotes);
//                return;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                oParameter.setNotes(sNotes);
//                return;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                oCompartment.setNotes(sNotes);
//                return;
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                oReaction.setNotes(sNotes);
//                return;
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                oRule.setNotes(sNotes);
//                return;
//            }
//
//            throw new Exception("Invalid id. No element with the given id exists in the model.");
//        }
//
//        public static void setSBOTerm(string sId, int nSBOTerm)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                oSpecies.setSBOTerm(nSBOTerm);
//                return;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                oParameter.setSBOTerm(nSBOTerm);
//                return;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                oCompartment.setSBOTerm(nSBOTerm);
//                return;
//            }
//
//            libsbmlcs.Reaction oReaction = mModel.getReaction(sId);
//            if (oReaction != NULL)
//            {
//                oReaction.setSBOTerm(nSBOTerm);
//                return;
//            }
//
//            Rule oRule = mModel.getRule(sId);
//            if (oRule != NULL)
//            {
//                oRule.setSBOTerm(nSBOTerm);
//                return;
//            }
//
//            throw new Exception("Invalid id. No element with the given id exists in the model.");
//
//        }
//
//        public static void setValue(Model model, string id, double value, bool throwIfNotFound)
//        {
//            if (model == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = model.getSpecies(id);
//            if (oSpecies != NULL)
//            {
//                if (oSpecies.isSetInitialAmount())
//                    oSpecies.setInitialAmount(value);
//                else
//                    oSpecies.setInitialConcentration(value);
//                return;
//            }
//
//            Compartment oCompartment = model.getCompartment(id);
//            if (oCompartment != NULL)
//            {
//                oCompartment.setVolume(value); return;
//            }
//
//            Parameter oParameter = model.getParameter(id);
//            if (oParameter != NULL)
//            {
//                oParameter.setValue(value);
//                return;
//            }
//
//            for (int i = 0; i < mModel.getNumReactions(); i++)
//            {
//                var reaction = mModel.getReaction(i);
//                for (int j = 0; j < reaction.getNumReactants(); j++)
//                {
//                    var reference = reaction.getReactant(j);
//                    if (reference.isSetId() && reference.getId() == id)
//                    {
//                        reference.setStoichiometry(value);
//                        return;
//                    }
//                }
//                for (int j = 0; j < reaction.getNumProducts(); j++)
//                {
//                    var reference = reaction.getProduct(j);
//                    if (reference.isSetId() && reference.getId() == id)
//                    {
//                        reference.setStoichiometry(value);
//                        return;
//                    }
//                }
//            }
//
//            if (throwIfNotFound)
//                throw new Exception(string.Format("Invalid string name. The id '{0}' does not exist in the model", id));
//        }
//
//        public static void setValue(string sId, double dValue)
//        {
//            setValue(mModel, sId, dValue, true);
//        }
//
//        public static string validateSBML(string sModel)
//        {
//            SBMLDocument oDoc = libsbml.readSBMLFromString(sModel);
//            if (oDoc.getNumErrors() > 0)
//            {
//                StringBuilder oBuilder = new StringBuilder();
//                for (int i = 0; i < oDoc.getNumErrors(); i++)
//                {
//                    ArrayList oList = getNthError(i);
//                    oBuilder.Append(oList[0] + ": (" + oList[1] + ":" + oList[2] + "[" + oList[3] + "]) " + oList[4] + Environment.NewLine);
//                }
//                throw new Exception("Validation failed: " + Environment.NewLine + oBuilder.ToString());
//            }
//            return "Validation Successfull";
//        }
//
//        public static string validateWithConsistency(string sModel)
//        {
//            SBMLDocument oDoc = libsbml.readSBMLFromString(sModel);
//            if (oDoc.getNumErrors() + oDoc.checkConsistency() > 0)
//            {
//                StringBuilder oBuilder = new StringBuilder();
//                for (int i = 0; i < oDoc.getNumErrors(); i++)
//                {
//                    ArrayList oList = getNthError(i);
//                    oBuilder.Append(oList[0] + ": (" + oList[1] + ":" + oList[2] + "[" + oList[3] + "]) " + oList[4] + Environment.NewLine);
//                }
//                throw new Exception("Validation failed: " + Environment.NewLine + oBuilder.ToString());
//            }
//            return "Validation Successfull";
//        }
//
//        #region Pre Defined Functions
//
//        static string[][] _oPredefinedFunctions = new string[][]
//
//	{
//		new string[]{ "massi1", "Irreversible Mass Action Kinetics for 1 substrate", "S", "k", "k * S" },
//		new string[]{ "massi2", "Irreversible Mass Action Kinetics for 2 substrates", "S1", "S2", "k", "k * S1 * S2" },
//		new string[]{ "massi3", "Irreversible Mass Action Kinetics for 3 substrates", "S1", "S2", "S3", "k", "k * S1 * S2 * S3" },
//		new string[]{
//						"massr11", "Reversible Mass Action Kinetics for 1 substrate and 1 product", "S", "P", "k_1", "k_2",
//						"k_1 * S - k_2 * P"},
//		new string[]{
//						"massr12", "Reversible Mass Action Kinetics for 1 substrate and 2 products", "S", "P1", "P2", "k_1", "k_2",
//						"k_1 * S - k_2 * P1 * P2"},
//		new string[]{
//						"massr13", "Reversible Mass Action Kinetics for 1 substrate and 3 products", "S", "P1", "P2", "P3", "k_1", "k_2",
//						"k_1 * S - k_2 * P1 * P2 * P3"},
//		new string[]{
//						"massr21", "Reversible Mass Action Kinetics for 2 substrates and 1 product", "S1", "S2", "P", "k_1", "k_2",
//						"k_1 * S1 * S2 - k_2 * P"},
//		new string[]{
//						"massr22", "Reversible Mass Action Kinetics for 2 substrates and 2 products", "S1", "S2", "P1", "P2", "k_1", "k_2",
//						"k_1 * S1 * S2 - k_2 * P1 * P2"},
//		new string[]{
//						"massr23", "Reversible Mass Action Kinetics for 2 substrates and 3 products", "S1", "S2", "P1", "P2", "P3", "k_1", "k_2",
//						"k_1 * S1 * S2 - k_2 * P1 * P2 * P3"},
//		new string[]{
//						"massr31", "Reversible Mass Action Kinetics for 3 substrates and 1 product", "S1", "S2", "S3", "P", "k_1", "k_2",
//						"k_1 * S1 * S2 * S3 - k_2 * P"},
//		new string[]{
//						"massr32", "Reversible Mass Action Kinetics for 3 substrates and 2 products", "S1", "S2", "S3", "P1", "P2", "k_1", "k_2",
//						"k_1 * S1 * S2 * S3 - k_2 * P1 * P2"},
//		new string[]{
//						"massr33", "Reversible Mass Action Kinetics for 3 substrates and 3 products", "S1", "S2", "S3", "P1", "P2", "P3", "k_1", "k_2",
//						"k_1 * S1 * S2 * S3 - k_2 * P1 * P2 * P3"},
//		new string[]{ "uui", "Irreversible Simple Michaelis-Menten ", "S", "V_m", "K_m", "(V_m * S)/(K_m + S)" },
//		new string[]{
//						"uur", "Uni-Uni Reversible Simple Michaelis-Menten", "S", "P", "V_f", "V_r", "K_ms", "K_mp",
//						"(V_f * S / K_ms - V_r * P / K_mp)/(1 + S / K_ms +  P / K_mp)"},
//		new string[]{
//						"uuhr", "Uni-Uni Reversible Simple Michaelis-Menten with Haldane adjustment", "S", "P", "V_f", "K_m1", "K_m2", "K_eq",
//						"( V_f / K_m1 * (S - P / K_eq ))/(1 + S / K_m1 + P / K_m2)"},
//		new string[]{
//						"isouur", "Iso Uni-Uni", "S", "P", "V_f", "K_ms", "K_mp", "K_ii", "K_eq",
//						"(V_f * (S - P / K_eq ))/(S * (1 + P / K_ii ) + K_ms * (1 + P / K_mp))"},
//		new string[]{ "hilli", "Hill Kinetics", "S", "V", "S_0_5", "h", "(V * pow(S,h))/(pow(S_0_5,h) + pow(S,h))"},
//		new string[]{
//						"hillr", "Reversible Hill Kinetics", "S", "P", "V_f", "S_0_5", "P_0_5", "h", "K_eq",
//						"(V_f * (S / S_0_5) * (1 - P / (S * K_eq) ) * pow(S / S_0_5 + P / P_0_5, h-1))/(1 + pow(S / S_0_5 + P / P_0_5, h))"},
//		new string[]{
//						"hillmr", "Reversible Hill Kinetics with One Modifier", "S", "M", "P", "V_f", "K_eq", "k", "h", "alpha",
//						"(V_f * (S / S_0_5) * (1 - P / (S * K_eq) ) * pow(S / S_0_5 + P / P_0_5, h-1))/( pow(S / S_0_5 + P / P_0_5, h) + (1 + pow(M / M_0_5, h))/(1 + alpha * pow(M/M_0_5,h)))"},
//		new string[]{
//						"hillmmr", "Reversible Hill Kinetics with Two Modifiers", "S", "P", "M", "V_f", "K_eq", "k", "h", "a", "b", "alpha_1", "alpha_2", "alpha_12",
//						"(V_f * (S / S_0_5) * (1 - P / (S * K_eq) ) * pow(S / S_0_5 + P / P_0_5, h-1)) / (pow(S / S_0_5 + P / P_0_5, h) + ((1 + pow(Ma/Ma_0_5,h) + pow(Mb/Mb_0_5,h))/( 1 + alpha_1 * pow(Ma/Ma_0_5,h) + alpha_2 * pow(Mb/Mb_0_5,h) + alpha_1 * alpha_2 * alpha_12 * pow(Ma/Ma_0_5,h) * pow(Mb/Mb_0_5,h))))"},
//		new string[]{ "usii", "Substrate Inhibition Kinetics (Irreversible)", "S", "V", "K_m", "K_i", "V*(S/K_m)/(1 + S/K_m + sqr(S)/K_i)"},
//		new string[]{
//						"usir", "Substrate Inhibition Kinetics (Reversible)", "S", "P", "V_f", "V_r", "K_ms", "K_mp", "K_i",
//						"(V_f*S/K_ms + V_r*P/K_mp)/(1 + S/K_ms + P/K_mp + sqr(S)/K_i)"},
//		new string[]{ "usai", "Substrate Activation", "S", "V", "K_sa", "K_sc", "V * sqr(S/K_sa)/(1 + S/K_sc + sqr(S/K_sa) + S/K_sa)"},
//		new string[]{ "ucii", "Competitive Inhibition (Irreversible)", "S", "V", "K_m", "K_i", "(V * S/K_m)/(1 + S/K_m + I/K_i)"},
//		new string[]{
//						"ucir", "Competitive Inhibition (Reversible)", "S", "P", "V_f", "V_r", "K_ms", "K_mp", "K_i",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + S/K_ms + P/K_mp + I/K_i)"},
//		new string[]{ "unii", "Noncompetitive Inhibition (Irreversible)", "S", "I", "V", "K_m", "K_i", "(V*S/K_m)/(1 + I/K_i + (S/K_m)*(1 + I/K_i))"},
//		new string[]{
//						"unir", "Noncompetitive Inhibition (Reversible)", "S", "P", "I", "V_f", "K_ms", "K_mp", "K_i",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + I/K_i + (S/K_ms + P/K_mp )*(1 + I/K_i))"},
//		new string[]{ "uuci", "Uncompetitive Inhibition (Irreversible)", "S", "I", "V", "K_m", "K_i", "(V*S/K_m)/(1 + (S/K_m)*(1 + I/K_i))"},
//		new string[]{
//						"uucr", "Uncompetitive Inhibition (Reversible)", "S", "P", "I", "V_f", "V_r", "K_ms", "K_mp", "K_i",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + ( S/K_ms + P/K_mp )*( 1 + I/K_i))"},
//		new string[]{
//						"umi", "Mixed Inhibition Kinetics (Irreversible)", "S", "I", "V", "K_m", "K_is", "K_ic",
//						"(V*S/K_m)/(1 + I/K_is + (S/K_m)*(1 + I/K_ic))"},
//		new string[]{
//						"umr", "Mixed Inhibition Kinetics (Reversible)", "S", "P", "I", "V_f", "V_r", "K_ms", "K_mp", "K_is", "K_ic",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + I/K_is + ( S/K_ms + P/K_mp )*( 1 + I/K_ic ))"},
//		new string[]{ "uai", "Specific Activation Kinetics - irreversible", "S", "A_c", "V", "K_m", "K_a", "(V*S/K_m)/(1 + S/K_m + K_a/A_c)"},
//		new string[]{
//						"uar", "Specific Activation Kinetics (Reversible)", "S", "P", "A_c", "V_f", "V_r", "K_ms", "K_mp", "K_a",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + S/K_ms + P/K_mp + K_a/A_c)"},
//		new string[]{ "ucti", "Catalytic Activation (Irreversible)", "S", "A_c", "V", "K_m", "K_a", "(V*S/K_m)/(1 + K_a/A_c + (S/K_m)*(1 + K_a/A_c))"},
//		new string[]{
//						"uctr", "Catalytic Activation (Reversible)", "S", "P", "A_c", "V_f", "V_r", "K_ms", "K_mp", "K_a",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + K_a/A_c + (S/K_ms + P/K_mp)*(1 + K_a/A_c))"},
//		new string[]{
//						"umai", "Mixed Activation Kinetics (Irreversible)", "S", "A_c", "V", "K_m", "Kas", "Kac",
//						"(V*S/K_m)/(1 + Kas/A_c + (S/K_m)*(1 + Kac/A_c))"},
//		new string[]{
//						"umar", "Mixed Activation Kinetics (Reversible)", "S", "P", "A_c", "V_f", "V_r", "K_ms", "K_mp", "K_as", "K_ac",
//						"(V_f*S/K_ms - V_r*P/K_mp)/(1 + K_as/A_c + (S/K_ms + P/K_mp)*(1 + K_ac/A_c))"},
//		new string[]{
//						"uhmi", "General Hyperbolic Modifier Kinetics (Irreversible)", "S", "M", "V", "K_m", "K_d", "a", "b",
//						"(V*(S/K_m)*(1 + b * M / (a*K_d)))/(1 + M/K_d + (S/K_m)*(1 + M/(a*K_d)))"},
//		new string[]{
//						"uhmr", "General Hyperbolic Modifier Kinetics (Reversible)", "S", "P", "M", "V_f", "V_r", "K_ms", "K_mp", "K_d", "a", "b",
//						"((V_f*S/K_ms - V_r*P/K_mp)*(1 + b*M/(a*K_d)))/(1 + M/K_d + (S/K_ms + P/K_mp)*(1 + M/(a*K_d)))"},
//		new string[]{
//						"ualii", "Allosteric inhibition (Irreversible)", "S", "I", "V", "K_s", "K_ii", "n", "L",
//						"(V*pow(1 + S/K_s, n-1))/(L*pow(1 + I/K_ii,n) + pow(1 + S/K_s,n))"},
//		new string[]{
//						"ordubr", "Ordered Uni Bi Kinetics", "A", "P", "Q", "V_f", "V_r", "K_ma", "K_mq", "K_mp", "K_ip", "K_eq",
//						"(V_f*( A - P*Q/K_eq))/(K_ma + A*(1 + P/K_ip) + (V_f/(V_r*K_eq))*(K_mq*P + K_mp*Q + P*Q))"},
//		new string[]{
//						"ordbur", "Ordered Bi Uni Kinetics", "A", "B", "P", "V_f", "V_r", "K_ma", "Kmb", "K_mp", "K_ia", "K_eq",
//						"(V_f*(A*B - P/K_eq))/(A*B + K_ma*B + Kmb*A + (V_f/(V_r*K_eq))*(K_mp + P*(1 + A/K_ia)))"},
//		new string[]{
//						"ordbbr", "Ordered Bi Bi Kinetics", "A", "B", "P", "Q", "V_f", "K_ma", "K_mb", "K_mp", "K_ia", "K_ib", "K_ip", "K_eq",
//						"(V_f*(A*B - P*Q/K_eq))/(A*B*(1 + P/K_ip) + K_mb*(A + K_ia) + K_ma*B + ((V_f / (V_r*K_eq)) * (K_mq*P*( 1 + A/K_ia) + Q*(K_mp*( 1 + (K_ma*B)/(K_ia*K_mb) + P*(1 + B/K_ib))))))"},
//		new string[]{
//						"ppbr", "Ping Pong Bi Bi Kinetics", "A", "B", "P", "Q", "V_f", "V_r", "K_ma", "K_mb", "K_mp", "K_mq", "K_ia", "K_iq", "K_eq",
//						"(V_f*(A*B - P*Q/K_eq))/(A*B + K_mb*A + K_ma*B*(1 + Q/K_iq) + ((V_f/(V_r*K_eq))*(K_mq*P*(1 + A/K_ia) + Q*(K_mp + P))))"}};
//
//
//
//        public static SBMLDocument SbmlDocument
//        {
//            get
//            {
//                return _oDoc;
//            }
//        }
//
//
//        public static Model SbmlModel
//        {
//            get
//            {
//                return mModel;
//            }
//        }
//        #endregion
//
//        /// <summary>
//        /// Checks whether the element for the given SBML id is a compartment
//        /// </summary>
//        /// <param name="sId">the id to check</param>
//        /// <returns>true if element is a compartment, false otherwise</returns>
//        public static bool IsCompartment(string sId)
//        {
//            libsbmlcs.Compartment temp = mModel.getCompartment(sId);
//            if (temp != NULL) return true;
//            return false;
//        }
//
//        /// <summary>
//        /// Checks whether the element for the given SBML id is a species
//        /// </summary>
//        /// <param name="sId">the id to check</param>
//        /// <returns>true if element is a species, false otherwise</returns>
//        public static bool IsSpecies (string sId)
//        {
//            var temp = mModel.getSpecies(sId);
//            if (temp != NULL) return true;
//            return false;
//        }
//
//        /// <summary>
//        /// Checks whether the element for the given SBML id is a floating species
//        /// </summary>
//        /// <param name="sId">the id to check</param>
//        /// <returns>true if element is a floating species, false otherwise</returns>
//        public static bool IsFloating(string sId)
//        {
//            var temp = mModel.getSpecies(sId);
//            if (temp != NULL && temp.getBoundaryCondition() == false) return true;
//            return false;
//        }
//
//        /// <summary>
//        /// Returns the element for the given sId, or NULL if not present
//        /// </summary>
//        /// <param name="sId">the sbml id for the element to find</param>
//        /// <returns>the element with the given sbml id</returns>
//        public static SBase GetElement(string sId)
//        {
//            if (mModel == NULL)
//            {
//                throw new Exception("You need to load the model first");
//            }
//
//            libsbmlcs.Species oSpecies = mModel.getSpecies(sId);
//            if (oSpecies != NULL)
//            {
//                return oSpecies;
//            }
//
//            Compartment oCompartment = mModel.getCompartment(sId);
//            if (oCompartment != NULL)
//            {
//                return oCompartment;
//            }
//
//            Parameter oParameter = mModel.getParameter(sId);
//            if (oParameter != NULL)
//            {
//                return oParameter;
//            }
//
//            for (int i = 0; i < mModel.getNumReactions(); i++)
//            {
//                var reaction = mModel.getReaction(i);
//
//                if (reaction.isSetId() && reaction.getId() == sId)
//                    return reaction;
//
//                for (int j = 0; j < reaction.getNumReactants(); j++)
//                {
//                    var reference = reaction.getReactant(j);
//                    if (reference.isSetId() && reference.getId() == sId)
//                    {
//                        return reference;
//                    }
//                }
//                for (int j = 0; j < reaction.getNumProducts(); j++)
//                {
//                    var reference = reaction.getProduct(j);
//                    if (reference.isSetId() && reference.getId() == sId)
//                    {
//                        return reference;
//                    }
//                }
//            }
//
//
//
//
//            throw new Exception("Invalid string name. The id '" + sId + "' does not exist in the model");
//        }
//
//        /// <summary>
//        /// Checks whether the element for the given SBML id is a boundary species
//        /// </summary>
//        /// <param name="sId">the id to check</param>
//        /// <returns>true if element is a boundary species, false otherwise</returns>
//        public static bool IsBoundary(string sId)
//        {
//            var temp = mModel.getSpecies(sId);
//            if (temp != NULL && temp.getBoundaryCondition() == true) return true;
//            return false;
//        }
//
//        public static bool MultiplyCompartment(string sbmlId, out string compartmentId)
//        {
//
//            compartmentId = NULL;
//
//            libsbmlcs.Species temp = mModel.getSpecies(sbmlId);
//            if (temp != NULL &&
//                //temp.isSetInitialAmount() &&
//                temp.isSetCompartment() &&
//                !temp.getHasOnlySubstanceUnits())
//            {
//
//                compartmentId = temp.getCompartment();
//
//                libsbmlcs.Compartment comp = mModel.getCompartment(compartmentId);
//                if (comp == NULL || comp.getSpatialDimensions() == 0)
//                    return false;
//
//                return true;
//            }
//            return false;
//
//        }
//
//        /// <summary>
//        /// This should return an initialization for the given sbmlId that is sideeffect free
//        /// </summary>
//        /// <param name="sbmlId"></param>
//        /// <returns></returns>
//        public static Stack<string> GetMatchForSymbol(string sbmlId)
//        {
//            Stack<string> result = new Stack<string>();
//
//            FillStack(result, _symbolTable[sbmlId] as SBMLSymbol);
//
//            return result;
//        }
//
//        public static void FillStack(Stack<string> stack, SBMLSymbol symbol)
//        {
//
//            if (symbol == NULL) return;
//
//            if (symbol.HasRule)
//                stack.Push(symbol.Id + " = " + symbol.Rule);
//            if (symbol.HasInitialAssignment)
//                stack.Push(symbol.Id + " = " + symbol.InitialAssignment);
//            if (symbol.HasValue)
//                stack.Push(symbol.Id + " = " + symbol.Value);
//
//            foreach (SBMLSymbol dependency in symbol.Dependencies)
//            {
//                FillStack(stack, dependency);
//            }
//
//        }
//
//        public static string addSourceSinkNodes(string sbml)
//        {
//            SBMLDocument doc = libsbml.readSBMLFromString(sbml);
//
//            UpgradeToL2V4IfNecessary(doc);
//
//            Model model = doc.getModel();
//
//            if (NeedSourceNode(model))
//            {
//                Species source = model.getSpecies("source");
//                if (source == NULL)
//                {
//                    source = model.createSpecies();
//                    source.setId("source");
//                    source.setName(" ");
//                    //source.setName("Source");
//                    source.setSBOTerm(291);
//                    source.setCompartment(model.getCompartment(0).getId());
//                    source.setBoundaryCondition(true);
//                    source.setInitialAmount(0);
//                }
//
//                for (int i = 0; i < model.getNumReactions(); i++)
//                {
//                    libsbmlcs.Reaction r = model.getReaction(i);
//                    if (r.getNumReactants() == 0)
//                    {
//                        libsbmlcs.SpeciesReference reference = r.createReactant();
//                        reference.setSpecies(source.getId());
//                    }
//                }
//
//            }
//            if (NeedSinkNode(model))
//            {
//                Species sink = model.getSpecies("sink");
//                if (sink == NULL)
//                {
//                    sink = model.createSpecies();
//                    sink.setId("sink");
//                    //sink.setName("Sink");
//                    sink.setName(" ");
//                    sink.setSBOTerm(291);
//                    sink.setCompartment(model.getCompartment(0).getId());
//                    sink.setBoundaryCondition(true);
//                    sink.setInitialAmount(0);
//                }
//
//                for (int i = 0; i < model.getNumReactions(); i++)
//                {
//                    libsbmlcs.Reaction r = model.getReaction(i);
//                    if (r.getNumProducts() == 0)
//                    {
//                        libsbmlcs.SpeciesReference reference = r.createProduct();
//                        reference.setSpecies(sink.getId());
//                    }
//                }
//
//            }
//
//            return libsbml.writeSBMLToString(doc);
//        }
//
//        public static bool NeedSourceNode(Model model)
//        {
//            for (int i = 0; i < model.getNumReactions(); i++)
//            {
//                libsbmlcs.Reaction r = model.getReaction(i);
//                if (r.getNumReactants() == 0) return true;
//            }
//
//            return false;
//        }
//
//        public static bool NeedSinkNode(Model model)
//        {
//            for (int i = 0; i < model.getNumReactions(); i++)
//            {
//                libsbmlcs.Reaction r = model.getReaction(i);
//                if (r.getNumProducts() == 0) return true;
//            }
//
//            return false;
//        }
//
//        public static bool NeedEmptySetNode(Model model)
//        {
//            for (int i = 0; i < model.getNumReactions(); i++)
//            {
//                libsbmlcs.Reaction r = model.getReaction(i);
//                if (r.getNumReactants() == 0 || r.getNumProducts() == 0) return true;
//            }
//
//            return false;
//        }
//
//        public static string addEmptySetNodes(string sbml)
//        {
//            SBMLDocument doc = libsbml.readSBMLFromString(sbml);
//
//            UpgradeToL2V4IfNecessary(doc);
//
//            Model model = doc.getModel();
//
//            int nCount = 0;
//
//            while (model.getSpecies("empty_" + nCount) != NULL)
//            {
//                nCount++;
//            }
//
//            if (model != NULL)
//            {
//                for (int i = 0; i < model.getNumReactions(); i++)
//                {
//                    libsbmlcs.Reaction r = model.getReaction(i);
//                    if (r.getNumReactants() == 0)
//                    {
//
//                        Species species = model.createSpecies();
//                        nCount++;
//                        species.setId("empty_" + nCount);
//                        //species.setName("EmptySet");
//                        species.setName(" ");
//                        species.setSBOTerm(291);
//                        species.setCompartment(model.getCompartment(0).getId());
//                        species.setBoundaryCondition(true);
//                        species.setInitialAmount(0);
//
//                        libsbmlcs.SpeciesReference reference = r.createReactant();
//                        reference.setSpecies(species.getId());
//                    }
//                    if (r.getNumProducts() == 0)
//                    {
//                        Species species = model.createSpecies();
//                        nCount++;
//                        species.setId("empty_" + nCount);
//                        //species.setName("EmptySet");
//                        species.setName(" ");
//                        species.setSBOTerm(291);
//                        species.setCompartment(model.getCompartment(0).getId());
//                        species.setBoundaryCondition(true);
//                        species.setInitialAmount(0);
//
//                        libsbmlcs.SpeciesReference reference = r.createProduct();
//                        reference.setSpecies(species.getId());
//                    }
//                }
//            }
//
//
//            return libsbml.writeSBMLToString(doc);
//        }
//
//        public static string addEmptySetNode(string sbml)
//        {
//            SBMLDocument doc = libsbml.readSBMLFromString(sbml);
//
//            UpgradeToL2V4IfNecessary(doc);
//
//            Model model = doc.getModel();
//
//            if (NeedEmptySetNode(model))
//            {
//                Species source = model.getSpecies("emptySet");
//                if (source == NULL)
//                {
//                    source = model.createSpecies();
//                    source.setId("emptySet");
//                    //source.setName("EmptySet");
//                    source.setName(" ");
//                    source.setSBOTerm(291);
//                    source.setCompartment(model.getCompartment(0).getId());
//                    source.setBoundaryCondition(true);
//                    source.setInitialAmount(0);
//                }
//
//                for (int i = 0; i < model.getNumReactions(); i++)
//                {
//                    libsbmlcs.Reaction r = model.getReaction(i);
//                    if (r.getNumReactants() == 0)
//                    {
//                        libsbmlcs.SpeciesReference reference = r.createReactant();
//                        reference.setSpecies(source.getId());
//                    }
//                    if (r.getNumProducts() == 0)
//                    {
//                        libsbmlcs.SpeciesReference reference = r.createProduct();
//                        reference.setSpecies(source.getId());
//                    }
//                }
//
//            }
//
//
//            return libsbml.writeSBMLToString(doc);
//        }
//
//        public static string RemoveJD2Layout(string sSBML)
//        {
//            int jdStart = sSBML.IndexOf("<jd2:JDesignerLayout");
//            string endTag = "</jd2:JDesignerLayout>";
//            int jdEnd = sSBML.IndexOf(endTag);
//
//            if (jdEnd != -1)
//                return sSBML.Substring(0, jdStart) + sSBML.Substring(jdEnd + endTag.Length);
//            return sSBML;
//        }
//
//        public static string RemoveJD1Layout(string sSBML)
//        {
//            XmlDocument doc = new XmlDocument();
//            doc.LoadXml(sSBML);
//            var nodes = doc.DocumentElement.GetElementsByTagName("annotation");
//            foreach (var item in nodes)
//            {
//                XmlElement node = item as XmlElement;
//                for (int i = node.ChildNodes.Count - 1; i >= 0; i--)
//                {
//                    XmlNode child = node.ChildNodes[i];
//                    if (child.Prefix == "jd")
//                        node.RemoveChild(child);
//                }
//            }
//
//
//            string result;
//            using (MemoryStream stream = new MemoryStream())
//            {
//                XmlWriterSettings settingsVariable = new XmlWriterSettings();
//                settingsVariable.Indent = true;
//                XmlWriter write = XmlWriter.Create(stream, settingsVariable);
//                doc.WriteTo(write);
//                write.Close();
//                stream.Flush();
//                stream.SetLength(stream.Length);
//                stream.Close();
//
//                result = ASCIIEncoding.UTF8.GetString(stream.GetBuffer()).Trim();
//            }
//
//
//            return result.Replace("UTF-16", "utf-8");
//        }
//
//        public static string RemoveLayoutInformation(string sSBML)
//        {
//            sSBML = RemoveJD2Layout(sSBML);
//            sSBML = RemoveJD1Layout(sSBML);
//
//            return sSBML;
//        }
//
//    }
//
//}

}//namespace rr
