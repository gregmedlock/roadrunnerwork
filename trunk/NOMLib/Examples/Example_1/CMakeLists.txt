set(target NLExample1)

add_executable(${target} main.cpp)

target_link_libraries (${target} NOMLib)


install (TARGETS ${target} DESTINATION bin COMPONENT compiled_examples)

