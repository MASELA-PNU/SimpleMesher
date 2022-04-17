!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      panel
!!
!!  Description
!!      Panel Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type Panel
!! ------------------------------------------------------------------ !!

    type, public :: typPanel

!! Type Data -------------------------------------------------------- !!

    private

        !! Number of Node of Panel
        integer  :: nNode_

        !! Node Label Array
        integer, allocatable :: nodeLabel_(:)

        !! Node Label Array
        type(typPoint), public, allocatable :: node_(:)

        !! vtk write Surface Type
        integer  :: vtkCellType_

        !! Area of Panel (Sum of sub-triangle area)
        real(RP), public :: area

        !! Center of Panel (Area averaged normal)
        type(typPoint), public :: center

        !! Normal Vector
        type(typPoint), public :: normal

        !! Number of Gauss Point
        integer, public        :: nGauss

        !! Gauss Point
        type(typPoint), public, allocatable :: gPoint(:)

        !! Gauss Point
        real(RP), public, allocatable       :: gWeight(:)

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Set Panel
        procedure, pass, public :: Set => Set_typPanel

        !! Compute Area and Normal with given nodes without changing without nodes
        procedure, pass, public :: Correct => Correct_typPanel

        !! Compute Area and Normal with given nodes without changing without nodes
        procedure, pass, public :: CorrectGaussPoint => CorrectGaussPoint_typPanel

        !! Return Number of Panel Node
        procedure, pass, public :: nNode => nNode_typPanel

        !! Return Node label
        procedure, pass, public :: NodeLabel => NodeLabel_typPanel

        !! Return VTK Surface Mesh Type
        procedure, pass, public :: vtkCellType => vtkCellType_typPanel

        !! Copy Subroutine
        procedure, pass, public :: Copy_typPanel

        !! Copy Operator
        generic :: assignment(=) => Copy_typPanel

        !! Destroy Panel Type
        procedure, pass, public :: Destroy => Destroy_typPanel

        !!... Revert the order
        Procedure :: Revert => Revert_typPanel

    end type

!! ------------------------------------------------------------------ !!
!!                      Type Panel
!! ------------------------------------------------------------------ !!
