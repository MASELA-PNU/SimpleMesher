## -------------------------------------------------------------------------- ##
#
#   Project: FortranProjectBase
#
#   Description:
#       Project example to begin the computer package and to educate student.
#
#   Author:
#       Young-Myung CHOI
#         Dept. of Naval Arch. and Ocean Eng.,
#         Pusan National University, Republic of Korea
#
## -------------------------------------------------------------------------- ##

## Default Settings --------------------------------------------------------- ##

##... Set the minimum version of cmake
cmake_minimum_required(VERSION 2.9 FATAL_ERROR)

##... CMake Project Name
project(FortranProjectBase)

##... Set Default Fortran Compiler & Eable the Fortran Compilation Environment
enable_language(Fortran)
if (NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER gfortran)
endif()

## CMake Configuration ------------------------------------------------------ ##

##... Load Default CMake Configuration
include( config/config.cmake )

##... Set Fortran Compiler Flags
include( config/fortranFlag.cmake )

##... Auto Compilation Functions
include( config/Func_AutoCompile.cmake )

## Print CMake Project Information ------------------------------------------ ##
message("")
message("${BoldYellow}Project: ${BoldWhite}${CMAKE_PROJECT_NAME}")
message("")
message("  - ${Green}Projct Path       ${White}: ${PROJECT_COMPILE_PATH} ${Green}")
message("  - ${Green}GIT_BRANCH        ${White}: ${GIT_BRANCH} ${Green}")
message("  - ${Green}GIT_COMMIT_HASH   ${White}: ${GIT_COMMIT_HASH} ${Green}")
message("")
message("  - ${Green}Lib. Install Path ${White}: ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${Green}")
message("  - ${Green}Exe. Install Path ${White}: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} ${Green}")
message("")

if( ${FORTRAN_COMPILE_MODE} STREQUAL "Release" )
message("  - ${Green}Compilation Mode  ${White}: Release ${Green}")
elseif( ${FORTRAN_COMPILE_MODE} STREQUAL "Debug" )
message("  - ${Green}Compilation Mode  ${White}: Debug ${Green}")
endif()

message("${Green}")
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
message("  - ${Green}Fortran Compiler  ${White}: Intel Fortran ${Green}")
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
message("  - ${Green}Fortran Compiler  ${White}: GNU Fortran ${Green}")
endif()
message("  - ${Green}Global Flag       ${White}: ${CMAKE_Fortran_FLAGS} ${Green}")
message("${ColourReset}")

## Extra Lib Path ----------------------------------------------------------- ##

if ("${PATH_EaFort}" STREQUAL "")
    message(" ${BoldRed} No PATH_EaFort is defined. Please check CMakeOptions.cmake \n")
    message(" ${ColourReset} ")
    message( FATAL_ERROR )
endif()

if( WIN32 )

    set( LIB_PATH_EaFort ${PATH_EaFort}/lib/Debug/EaFort.lib )
    set( INC_PATH_EaFort ${PATH_EaFort}/include/Debug/ )

elseif( UNIX )
    set( LIB_PATH_EaFort ${PATH_EaFort}/lib/libEaFort.a )
    set( INC_PATH_EaFort ${PATH_EaFort}/include/ )
else()
    message(" ${BoldRed} In this environment, cmake with project is not validated. \n")
    message(" ${ColourReset} ")
    message( FATAL_ERROR )
endif()

set(LIB_PATH_EXTLIB ${LIB_PATH_EaFort})
set(INC_PATH_EXTLIB ${INC_PATH_EaFort})

## Compile Library ---------------------------------------------------------- ##

set(COMPILE_DYNAMIC_LIB False)
# set(COMPILE_DYNAMIC_LIB True)

#... Library 1
set( LIB_NAME_geometry geometry)
set(LIB_EXTLIB_geometry ${LIB_PATH_EXTLIB})
set(LIB_EXTINC_geometry ${INC_PATH_EXTLIB})
add_subdirectory( src/libGeometry )

## Compile Applications ----------------------------------------------------- ##

#... Application 1
set( APP_NAME_appSimpleMesher appSimpleMesher)
set( APP_DEPEND_appSimpleMesher geometry )
set(APP_EXTLIB_appSimpleMesher ${LIB_PATH_EXTLIB} )
set(APP_EXTINC_appSimpleMesher ${INC_PATH_EXTLIB} )
add_subdirectory(applications/appSimpleMesher)
