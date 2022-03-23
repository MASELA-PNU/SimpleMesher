!! -------------------------------------------------------------------------- !!
Module glbSimpleMesher
!! -------------------------------------------------------------------------- !!
    Use pkgEaFort
    Use pkgGeometry
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... JSON Input
    Character(len=:), Allocatable :: jsonInputFilePath
    Type(typJSONFile)      :: inputFile
    Type(typJSON), Pointer :: jsonInput

    !!... Mesh List Information
    Integer                      :: nMeshList
    Type(typString), Allocatable :: meshList(:)

    !!... Surf Mesh Class
    Class(typSurfMesh), Allocatable :: surfMesh

    !!... Gethered Data
    Integer :: nNode, nPanel
    Real(RP), Allocatable :: nodePos(:, :)
    Integer, Allocatable  :: panelConnect(:, :)

    !!... Final Mesh
    Type(typSurfMesh) :: finalMesh

!! -------------------------------------------------------------------------- !!
Contains
!! -------------------------------------------------------------------------- !!

#include "routines/initializeGURU.inc"

!! -------------------------------------------------------------------------- !!
End Module
!! -------------------------------------------------------------------------- !!
