# Install script for directory: C:/rrw/ThirdParty/libsbml/src

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "C;\rrw\Installs\libsbml")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "Release")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/annotation" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/annotation/CVTerm.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/annotation/Date.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/annotation/ModelCreator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/annotation/ModelHistory.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/annotation/RDFAnnotation.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/common" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/common.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/extern.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/libsbml-config-common.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/libsbml-config.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/libsbml-namespace.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/libsbml-package.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/libsbml-version.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/operationReturnValues.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/common/sbmlfwd.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/compress" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/bzfstream.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/CompressCommon.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/crypt.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/InputDecompressor.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/ioapi.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/iowin32.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/OutputCompressor.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/unzip.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/zfstream.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/zip.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/compress/zipfstream.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/conversion" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/ConversionOption.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/ConversionProperties.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLConverter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLConverterRegister.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLConverterRegistry.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLFunctionDefinitionConverter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLInitialAssignmentConverter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLLevelVersionConverter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLRuleConverter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLStripPackageConverter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/conversion/SBMLUnitsConverter.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/extension" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/ISBMLExtensionNamespaces.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/RegisterExtensions.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBaseExtensionPoint.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBasePlugin.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBasePluginCreator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBasePluginCreatorBase.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBMLDocumentPlugin.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBMLExtension.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBMLExtensionException.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBMLExtensionNamespaces.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBMLExtensionRegister.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/extension/SBMLExtensionRegistry.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/math" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/math/ASTNode.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/math/FormulaFormatter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/math/FormulaParser.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/math/FormulaTokenizer.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/math/MathML.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/units" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/units/FormulaUnitsData.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/units/UnitFormulaFormatter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/units/UnitKindList.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/util" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/util/List.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/util/memory.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/util/Stack.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/util/StringBuffer.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/util/util.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/validator" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/ConsistencyValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/ConstraintMacros.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/IdentifierConsistencyValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/InternalConsistencyValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/L1CompatibilityValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/L2v1CompatibilityValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/L2v2CompatibilityValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/L2v3CompatibilityValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/L2v4CompatibilityValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/L3v1CompatibilityValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/MathMLConsistencyValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/ModelingPracticeValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/OverdeterminedValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/SBMLExternalValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/SBMLInternalValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/SBMLValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/SBOConsistencyValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/UnitConsistencyValidator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/Validator.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/validator/VConstraint.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml/xml" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/ExpatAttributes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/ExpatHandler.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/ExpatParser.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/LibXMLAttributes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/LibXMLHandler.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/LibXMLNamespaces.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/LibXMLParser.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/LibXMLTranscode.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XercesAttributes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XercesHandler.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XercesNamespaces.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XercesParser.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XercesTranscode.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLAttributes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLBuffer.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLConstructorException.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLError.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLErrorLog.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLExtern.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLFileBuffer.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLHandler.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLInputStream.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLMemoryBuffer.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLNamespaces.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLNode.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLOutputStream.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLParser.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLToken.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLTokenizer.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/xml/XMLTriple.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/sbml" TYPE FILE FILES
    "C:/rrw/ThirdParty/libsbml/src/sbml/AlgebraicRule.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/AssignmentRule.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Compartment.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/CompartmentType.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Constraint.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Delay.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Event.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/EventAssignment.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/ExpectedAttributes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/FunctionDefinition.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/InitialAssignment.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/KineticLaw.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/ListOf.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/LocalParameter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Model.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/ModifierSpeciesReference.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Parameter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Priority.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/RateRule.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Reaction.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Rule.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBase.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLConstructorException.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLDocument.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLError.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLErrorLog.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLErrorTable.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLNamespaces.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLReader.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLTransforms.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLTypeCodes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLTypes.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLVisitor.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBMLWriter.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SBO.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SimpleSpeciesReference.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Species.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SpeciesReference.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SpeciesType.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/StoichiometryMath.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/SyntaxChecker.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Trigger.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/Unit.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/UnitDefinition.h"
    "C:/rrw/ThirdParty/libsbml/src/sbml/UnitKind.h"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  IF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY OPTIONAL FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/Debug/libsbml.lib")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY OPTIONAL FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/Release/libsbml.lib")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Mm][Ii][Nn][Ss][Ii][Zz][Ee][Rr][Ee][Ll])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY OPTIONAL FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/MinSizeRel/libsbml.lib")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY OPTIONAL FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/RelWithDebInfo/libsbml.lib")
  ENDIF()
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  IF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE SHARED_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/Debug/libsbml.dll")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE SHARED_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/Release/libsbml.dll")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Mm][Ii][Nn][Ss][Ii][Zz][Ee][Rr][Ee][Ll])$")
    FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE SHARED_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/MinSizeRel/libsbml.dll")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE SHARED_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/RelWithDebInfo/libsbml.dll")
  ENDIF()
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  IF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml-static.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/Debug/libsbml-static.lib")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml-static.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/Release/libsbml-static.lib")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Mm][Ii][Nn][Ss][Ii][Zz][Ee][Rr][Ee][Ll])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml-static.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/MinSizeRel/libsbml-static.lib")
  ELSEIF("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    list(APPEND CPACK_ABSOLUTE_DESTINATION_FILES
     "C:/rrw/Installs/libsbml/lib/libsbml-static.lib")
FILE(INSTALL DESTINATION "C:/rrw/Installs/libsbml/lib" TYPE STATIC_LIBRARY FILES "C:/rrw/rrBuilds/vs-2010/libsbml/src/RelWithDebInfo/libsbml-static.lib")
  ENDIF()
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("C:/rrw/rrBuilds/vs-2010/libsbml/src/sbml/cmake_install.cmake")
  INCLUDE("C:/rrw/rrBuilds/vs-2010/libsbml/src/bindings/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

