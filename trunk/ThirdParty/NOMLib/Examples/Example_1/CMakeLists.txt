set(target NLExample1)

add_executable(${target} main.cpp)
target_link_libraries (${target} NOMLib)
target_link_libraries(${target} libsbml-static)
target_link_libraries(${target} libxml2)

install (TARGETS ${target} DESTINATION bin COMPONENT compiled_examples)

