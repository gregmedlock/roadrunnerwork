cmake_minimum_required(VERSION 2.8)
set(target rr_c)
set(rrSrc "../../../../src")

add_executable(		   ${target} 
main.cpp 
Args.cpp
GetOptions.cpp
#${rrSrc}/rrFileLog.cpp
#${rrSrc}/rrLog.cpp
#${rrSrc}/rrLogFile.cpp
#${rrSrc}/rrLoggerUtils.cpp
#${rrSrc}/rrLogOutput.cpp
#${rrSrc}/rrObject.cpp
#${rrSrc}/rrStringUtils.cpp
${rrSrc}/rrUtils.cpp
)

set_property(   TARGET ${target} 
                PROPERTY  COMPILE_DEFINITIONS 
                          STATIC_RR             #when linking with static lib, no declspec shuld be present..
)

target_link_libraries (${target} rr_c_api)

install(    
            TARGETS ${target} 		
            DESTINATION bin 
            COMPONENT apps)
