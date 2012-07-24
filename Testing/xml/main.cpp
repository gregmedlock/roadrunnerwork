#include <iostream>
#include "pugi/pugixml.hpp"



using namespace pugi;
using namespace std;
int main()
{
    xml_document doc;

    // get a test document
    doc.load("<project><name>test</name><version>1.1</version><public>yes</public></project>");

    xml_node project = doc.child("project");

    //[code_text_access
    cout << "Project name: " << project.child("name").text().get() << std::endl;
    cout << "Project version: " << project.child("version").text().as_double() << std::endl;
    cout << "Project visibility: " << (project.child("public").text().as_bool(/* def= */ true) ? "public" : "private") << std::endl;
    cout << "Project description: " << project.child("description").text().get() << std::endl;
    //]

    cout << endl;

    //[code_text_modify
    // change project version
    project.child("version").text() = 1.2;

    // add description element and set the contents
    // note that we do not have to explicitly add the node_pcdata child
    project.append_child("description").text().set("a test project");
    //]

    doc.save(cout);
}

