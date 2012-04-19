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
        int                             VerboseMode;

};

Paras::Paras()
:
CaseNumber(1),
ErrorThreshold(1.e-6),
OnlyCompile(false)
{

}

#endif
