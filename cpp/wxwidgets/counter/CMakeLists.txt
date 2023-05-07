cmake_minimum_required(VERSION 3.25)
project(counter)

set(CMAKE_CXX_STANDARD 17)
set(wxWidgets_CONFIGURATION mswu)

find_package(wxWidgets COMPONENTS core base REQUIRED)
include(${wxWidgets_USE_FILE})

add_executable(${PROJECT_NAME} App.cpp MainFrame.cpp)
target_link_libraries(${PROJECT_NAME} ${wxWidgets_LIBRARIES})