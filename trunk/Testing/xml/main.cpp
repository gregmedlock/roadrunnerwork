#include <iostream>
#include "rrXMLDocument.h"
#include "rrStringUtils.h"
#include "rrRoadRunner.h"


using namespace pugi;
using namespace std;
using namespace rr;
int main()
{
    RoadRunner rr;

    rr.loadSBMLFromFile("../Models/test_1.xml");
    string doc = rr.getCapabilities();


    cout<<doc;

//    stringstream xml;
//    doc.save(xml);
//
//    cout<<xml.str();
}


