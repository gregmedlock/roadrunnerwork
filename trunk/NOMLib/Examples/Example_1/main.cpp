#include <string>
#include <iostream>
#include "NOMLib.h"

using namespace std;
int main()
{
	cout<<"Hello NOMLib. Testing a function from the library\n";
	
	int returnVal = loadSBML("NONSENSE");

	cout<<"The return value was: "<<returnVal<<endl;
	cout<<"Last error was: "<<getError()<<endl;
	cout<<"Hit any key to exit...";

	char ch;
	cin>>ch;
    return 0;
}
