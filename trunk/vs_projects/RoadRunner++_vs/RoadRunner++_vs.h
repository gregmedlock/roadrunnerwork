// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the ROADRUNNER_VS_EXPORTS
// symbol defined on the command line. This symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// ROADRUNNER_VS_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
//#ifdef ROADRUNNER_VS_EXPORTS
//#define ROADRUNNER_VS_API __declspec(dllexport)
//#else
//#define ROADRUNNER_VS_API __declspec(dllimport)
//#endif
//
//// This class is exported from the RoadRunner++_vs.dll
//class ROADRUNNER_VS_API CRoadRunner_vs {
//public:
//	CRoadRunner_vs(void);
//	// TODO: add your methods here.
//};
//
//extern ROADRUNNER_VS_API int nRoadRunner_vs;
//
//ROADRUNNER_VS_API int fnRoadRunner_vs(void);
