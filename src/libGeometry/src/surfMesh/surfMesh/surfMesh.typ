!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      surfMesh
!!
!!  Description
!!      Base Type for surface mesh
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type SurfMesh
!! ------------------------------------------------------------------ !!

    Type, public :: typSurfMesh

!! Type Data -------------------------------------------------------- !!

        !! - Is top is opened ?
        Logical :: isOpenTop

        !! - Number of Node
        Integer :: nNode, nLineNode

        !! - Number of Surf Panel
        Integer :: nPanel

        !! - Number of Line Mesh
        Integer :: nLine

        !! - Number of Segment of Line
        Integer :: nLineSub

        !! - Node Position
        type(typPoint), allocatable :: node(:)

        !! - Panel Connectivity
        type(typPanel), allocatable :: panel(:)

        !! - Line Node
        type(typPoint), allocatable :: lineNode(:)

        !! - Line Mesh if top is opened
        type(typPanel), allocatable :: linePanel(:)

!! Type Procedures -------------------------------------------------- !!

    contains

        procedure :: ManualSet => ManualSet_typSurfMesh

        procedure :: WriteHydroStarMesh => WriteHydroStarMesh_typSurfMesh

        procedure, pass, public :: WriteVTK => WriteVTK_typSurfMesh

        procedure, pass, public :: WriteVTKSurfMesh => WriteVTKSurfMesh_typSurfMesh

        procedure, pass, public :: WriteVTKSurfGaussPoint => WriteVTKSurfGaussPoint_typSurfMesh

        procedure, pass, public :: WriteVTKLineMesh => WriteVTKLineMesh_typSurfMesh

        procedure, pass, public :: WriteVTKLineMeshGaussPoint => WriteVTKLineMeshGaussPoint_typSurfMesh

        procedure, pass, public :: Copy => Copy_typSurfMesh

        generic :: assignment(=) => Copy

        procedure, pass, public :: Destroy => Destroy_typSurfMesh

        ! final :: finalSurfMesh

    end type

!! Type Procedures -------------------------------------------------- !!

    public :: InitializeSurfMesh

!! ------------------------------------------------------------------ !!
!!                      Type SurfMesh
!! ------------------------------------------------------------------ !!
