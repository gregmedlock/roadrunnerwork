#include <iostream>
#include "rrXMLDocument.h"
#include "rrRoadRunner.h"


using namespace pugi;
using namespace std;
using namespace rr;
int main()
{
    RoadRunner rr;

    rrXMLDoc& doc = rr.getCapabilities();

//    doc.append_child();
     //doc.append_child(cap.root());

    stringstream xml;
    doc.save(xml);

    cout<<xml.str();
}


