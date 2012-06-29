#ifndef rr_svn_infoH
#define rr_svn_infoH
const int SVN_VERSION =  505;

const char* SVN_LAST_COMMIT_AUTHOR ="tottek";

const char* SVN_LASTLOG ="    ------------------------------------------------------------------------\n\
r505 | tottek | 2012-06-28 14:11:49 -0700 (Thu, 28 Jun 2012) | 20 lines\n\
\n\
New release\n\
\n\
Removed ArrayList's size function. use Count()..\n\
\n\
Added to API\n\
getFluxcoefficientNames\n\
getConcentrationControlCoefficientNames\n\
getEigenValueNames()\n\
getFluxControlCoefficientNames()\n\
getConcentrationControlCoefficientNames()\n\
getElasticityNames()\n\
getNumberOfCompartments()\n\
getCompartmentByIndex(const int& index, double& value)\n\
setCompartmentByIndex (const int& index, const double& value)\n\
getCompartmentNames()\n\
getRateOfChange(const int& index, double& value)\n\
printStringList(const RRStringListHandle list)\n\
\n\
\n\
 \n\
------------------------------------------------------------------------";

#endif