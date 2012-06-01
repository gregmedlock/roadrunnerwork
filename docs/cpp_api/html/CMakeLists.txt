cmake_minimum_required(VERSION 2.8)

#install C++ documentation
FILE (GLOB docs *.*)
install (FILES ${docs} DESTINATION Documentation/cpp_api/html COMPONENT rr_docs)

FILE (GLOB search_files search/*.*)
install (FILES ${search_files} DESTINATION Documentation/cpp_api/html/search COMPONENT rr_docs)


