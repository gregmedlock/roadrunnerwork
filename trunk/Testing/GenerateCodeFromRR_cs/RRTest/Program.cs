using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using LibRoadRunner;


namespace RRTest
{
	class Program
	{
		static void Main(string[] args)
		{
			RoadRunner oService = new RoadRunner();
	        string modelsRootPath = "C:\\RRW\\Testing\\models";
            
            StringStream modelSubPath;
            StringStream modelFName;

            string subFolder = ("test_cases_l2v4");

            //modelSubPath <<setfill('0')<<setw(5)<<caseNr;

            //modelFName<<setfill('0')<<setw(5)<<caseNr<<"-sbml-l2v4.xml";

            //string subFolder("");
            //model<<"feedback.xml";
            if(subFolder.Length > 0)
            {
                modelsRootPath = modelsRootPath + "\\" + subFolder + "\\" + modelSubPath.str();
            }

            string fullFilePath = (modelsRootPath +   "\\\\" + modelFName.str());

			string sbml= System.IO.File.ReadAllText(fullFilePath);
            
            
            


			oService.loadSBMLFromFile("feedback.xml");			
		}
	}
}
