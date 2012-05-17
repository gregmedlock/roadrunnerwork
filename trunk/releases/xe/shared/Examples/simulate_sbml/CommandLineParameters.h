//---------------------------------------------------------------------------
#ifndef CommandLineParametersH
#define CommandLineParametersH
//---------------------------------------------------------------------------

class Paras
{
    public:
        								Paras();
        virtual 		               ~Paras(){}
		int								CaseNumber;
        double							ErrorThreshold;
        bool							OnlyCompile;
        bool							Pause;
        int                             VerboseMode;

};

Paras::Paras()
:
CaseNumber(1),
ErrorThreshold(1.e-6),
OnlyCompile(false),
Pause(false)
{

}

#endif
