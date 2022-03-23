!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      point
!!
!!  Description
!!      Point Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type Point
!! ------------------------------------------------------------------ !!

    type, public :: typPoint

!! Type Data -------------------------------------------------------- !!

    private

        !! Point Index
        integer  :: idx_

        !! Pointer Vector
        Real(RP), dimension(3) :: vec_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Set Point
        procedure, pass, public :: Set => Set_typPoint

        !! Move Point Position
        procedure, pass, public :: Move => Move_typPoint

        !! Return x - Position
        procedure, pass, public :: X => X_typPoint

        !! Return y - Position
        procedure, pass, public :: Y => Y_typPoint

        !! Return z - Position
        procedure, pass, public :: Z => Z_typPoint

        !! Return Position Vector
        procedure, pass, public :: Vec => Vec_typPoint

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type Point
!! ------------------------------------------------------------------ !!
