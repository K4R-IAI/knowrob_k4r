cmake_minimum_required(VERSION 2.8.3)
project(knowrob_k4r)

find_package(catkin REQUIRED COMPONENTS
  knowrob
  knowrob_refills
)

find_package(catkin REQUIRED COMPONENTS
    rosprolog 
    roscpp 
    roslib
)

find_package(PkgConfig)

pkg_check_modules(RDKAFKA REQUIRED rdkafka)
include_directories(${RDKAFKA_INCLUDE_DIRS})
link_directories(${RDKAFKA_LIBRARY_DIRS})

pkg_check_modules(RDKAFKAPP REQUIRED rdkafka++)
include_directories(${RDKAFKAPP_INCLUDE_DIRS})
link_directories(${RDKAFKAPP_LIBRARY_DIRS})

pkg_check_modules(SWIPL REQUIRED swipl)
link_directories(${SWIPL_LIBRARY_DIRS})

pkg_check_modules(CURLPP REQUIRED curlpp)
include_directories(${CURLPP_INCLUDE_DIRS})
link_directories(${CURLPP_LIBRARY_DIRS})

pkg_check_modules(JSONCPP REQUIRED jsoncpp)
include_directories(${JSONCPP_INCLUDE_DIRS})
link_directories(${JSONCPP_LIBRARY_DIRS})

catkin_package(
  INCLUDE_DIRS include
  LIBRARIES ${PROJECT_NAME}
	CATKIN_DEPENDS rosprolog roscpp roslib
  DEPENDS RDKAFKAPP RDKAFKA JSONCPP
)

set(CMAKE_CXX_FLAGS "-std=c++0x -pthread ${CMAKE_CXX_FLAGS}")
include_directories(include SYSTEM ${SWIPL_INCLUDE_DIRS} ${CURLPP_INCLUDE_DIRS} ${JSONCPP_INCLUDE_DIRS} ${catkin_INCLUDE_DIRS})


##############
##### DB #####
##############

install(DIRECTORY owl/
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}/owl)

install(DIRECTORY prolog/
  DESTINATION ${CATKIN_PACKAGE_SHARE_DESTINATION}/prolog)

add_library(k4r_db_client SHARED src/k4r_db/k4r_db_kb.cpp)
target_link_libraries(k4r_db_client ${SWIPL_LIBRARIES} ${CURLPP_LIBRARIES} ${JSONCPP_LIBRARIES} ${catkin_LIBRARIES} stdc++fs)
add_dependencies(k4r_db_client
  ${${PROJECT_NAME}_EXPORTED_TARGETS}
  ${catkin_EXPORTED_TARGETS})
  
###############
##### Pa4 #####
###############

set(HEADER include/knowrob_k4r/kafka_event_producer.h)

add_library(kafka_plugin SHARED
            src/pa4/kafka_event_producer.cpp
            src/pa4/kafka_plugin.cpp
            ${HEADER}
            )

target_link_libraries(kafka_plugin 
                      ${SWIPL_LIBRARIES}
                      ${JSONCPP_LIBRARIES}
                      ${RDKAFKAPP_LIBRARIES}  
                      ${RDKAFKA_LIBRARIES}
                      ${catkin_LIBRARIES})

add_dependencies(kafka_plugin
  ${${PROJECT_NAME}_EXPORTED_TARGETS}
  ${catkin_EXPORTED_TARGETS})
