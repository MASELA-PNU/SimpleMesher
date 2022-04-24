!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typParaPipeMesh
!!
!!  Description
!!      typParaPipeMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typParaPipeMesh
!! ------------------------------------------------------------------ !!

    type,extends(typSurfMesh), public :: typSurfMeshBarge

!! Type Data -------------------------------------------------------- !!

    private

        !! - Length, Width, Depth
        Real(RP) :: Lx, Ly, Lz

        !! - Nx, Ny, Nz
        Integer  :: Nx, Ny, Nz

        !! - Origin
        Real(RP), dimension(3) :: centerMove

        !! - Ratio to Increment
        Real(RP) :: rX, rY, rZ

        !! - Face End Node Index
        Integer :: nNodeP1, nNodeP2, nNodeP3, nNodeP4, nNodeP5, nNodeP6

!! Type Procedures -------------------------------------------------- !!

    contains

        !!... Initialize the class
        Procedure :: Initialize => Initialize_typSurfMeshBarge

        !!... Create Mesh
        Procedure, Private :: CreateMesh => CreateMesh_typSurfMeshBarge

    end type

!! ------------------------------------------------------------------ !!
!!                      Type Point
!! ------------------------------------------------------------------ !!
