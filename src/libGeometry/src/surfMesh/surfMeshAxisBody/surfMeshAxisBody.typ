!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typCircularCylinderShellSurfMesh
!!
!!  Description
!!      typHemiSphereSurfMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typCircularCylinderShellSurfMesh
!! ------------------------------------------------------------------ !!

    Type,extends(typSurfMesh), public :: typSurfMeshAxisBody

!! Type Data -------------------------------------------------------- !!

    Private

        !!... Number of Coordinates
        Integer :: nCoord

        !!... Radius Vector
        Real(RP), Allocatable :: radius(:)

        !!... Vertical Position
        Real(RP), Allocatable :: zPos(:)

        !1... Discretization Number
        Integer, Allocatable :: nDiscretization(:)

        !!... Number of Theta
        Integer :: nTheta

        !!... nQuaterTheta
        Integer :: nQuaterTheta

        !! - Origin
        Real(RP), dimension(3) :: centerMove

!! Type Procedures -------------------------------------------------- !!

    contains

        !!... Initialize the class
        Procedure :: Initialize => Initialize_typSurfMeshAxisBody

        !!... Create Mesh
        Procedure, Private :: CreateMesh => CreateMesh_typSurfMeshAxisBody

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type typCircularCylinderShellSurfMesh
!! ------------------------------------------------------------------ !!
