cmake_minimum_required(VERSION 2.8)

PROJECT(rrModels)

set(models
feedback.xml
test_1.xml 				
ss_threestep.xml 	
ss_TurnOnConservationAnalysis.xml 	
)

install(    FILES ${models}
            DESTINATION Models 	
            COMPONENT sbml_models)
