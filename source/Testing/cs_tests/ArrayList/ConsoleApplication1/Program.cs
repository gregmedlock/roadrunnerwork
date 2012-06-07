using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;


namespace ConsoleApplication1
{
	class Program
	{
		static void Main(string[] args)
		{
			       
            ArrayList floatingSpeciesList = new ArrayList();

			
			string test;
			test = string.Format("{0}/ _c[{1}]", 2.33, 23);

            

            for (int i = 0; i < 5; i++)
            {
                
                    ArrayList oSpeciesValues = new ArrayList();
                    oSpeciesValues.Add(i);
                    oSpeciesValues.Add(i+1);
                    oSpeciesValues.Add(i+3);

                    floatingSpeciesList.Add(oSpeciesValues);                
            }

            
		}
	}
}
