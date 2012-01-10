//---------------------------------------------------------------------------

#pragma hdrstop
#include <string>
#include <iostream>
#include <strstream>
#include <conio.h>
#include <iomanip>
#include "rrMath.h"
//---------------------------------------------------------------------------
using namespace std;
using namespace rr;

int main()
{
	strstream outPut;

    outPut.precision(5);
    outPut.setf(ios::left);
	outPut<<"========== Constants ==========="<<endl;
	outPut<<setw(30)<<"EULER_CONSTANT_GAMMA"	<<setw(30)<<EULER_CONSTANT_GAMMA	<<endl;
	outPut<<setw(30)<<"GOLDEN_RATIO"			<<setw(30)<<GOLDEN_RATIO			<<endl;
	outPut<<setw(30)<<"LOG2E"					<<setw(30)<<LOG2E 					<<endl; /* log_2 (e) */
	outPut<<setw(30)<<"LOG10E"					<<setw(30)<<LOG10E 					<<endl; /* log_10 (e) */
	outPut<<setw(30)<<"SQRT2"					<<setw(30)<<SQRT2 					<<endl; /* sqrt(2) */
	outPut<<setw(30)<<"SQRT1_2"					<<setw(30)<<SQRT1_2					<<endl; /* sqrt(1/2) */
	outPut<<setw(30)<<"SQRT3"					<<setw(30)<<SQRT3					<<endl; /* sqrt(3) */
	outPut<<setw(30)<<"PI_BY_2"					<<setw(30)<<PI_BY_2					<<endl; /* pi/2 */
	outPut<<setw(30)<<"PI_BY_4"					<<setw(30)<<PI_BY_4					<<endl; /* pi/4 */
	outPut<<setw(30)<<"SQRTPI"					<<setw(30)<<SQRTPI 					<<endl; /* sqrt(pi) */
	outPut<<setw(30)<<"TWO_BY_SQRTPI"			<<setw(30)<<TWO_BY_SQRTPI			<<endl; /* 2/sqrt(pi) */
	outPut<<setw(30)<<"ONE_BU_PI"				<<setw(30)<<ONE_BU_PI				<<endl; /* 1/pi */
	outPut<<setw(30)<<"TWO_BY_PI"				<<setw(30)<<TWO_BY_PI				<<endl; /* 2/pi */
	outPut<<setw(30)<<"LN10"					<<setw(30)<<LN10					<<endl; /* ln(10) */
	outPut<<setw(30)<<"LN2"						<<setw(30)<<LN2						<<endl; /* ln(2) */
	outPut<<setw(30)<<"LNPI"					<<setw(30)<<LNPI					<<endl; /* ln(pi) */


	cout<<outPut.str()<<endl;


    cout<<"Hit any key to exit...";
	cin.ignore(0,'\n');
    getch();
	return 0;
}

