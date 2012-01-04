#include <conio.h>
#include <iostream>
#include "NOMLib.h"

using namespace std;
int main()
{
	cout<<"Hello NOMLib. \nTesting a function from the library\n";
	

	int returnVal = loadSBML("NONSENSE");

	cout<<"The return value was: "<<returnVal<<endl;
	cout<<"Last error was: "<<getError()<<endl;
	
	cout<<"Hit any key to exit...";
	cin.ignore(0,'\n');
    getch();
	
    return 0;
}
