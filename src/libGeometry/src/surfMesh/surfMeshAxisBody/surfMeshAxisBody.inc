!! ------------------------------------------------------------------ !!
!! Procedures : Type circularCylinderShellMesh
!! ------------------------------------------------------------------ !!

!! -------------------------------------------------------------------------- !!
Subroutine Initialize_typSurfMeshAxisBody( this, json )
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class( typSurfMeshAxisBody ), intent(inout) :: this
    Type(typJSON), Pointer                      :: json
!! -------------------------------------------------------------------------- !!
    Real(RP), Allocatable :: centerMove(:)

    Call JSON_GetRealVector( json, "radius", this%radius )
    Call JSON_GetRealVector( json, "zPos", this%zPos )
    Call JSON_GetIntVector( json, "nDiscretization", this%nDiscretization )

    !!... Check input parameters
    Block
        Integer :: nRadius, nZPos, nDiscretization

        nRadius         = size(this%radius)
        nZPos           = size(this%zPos)
        nDiscretization = size(this%nDiscretization)


        If (nRadius.le.1) then
            Call GURU%Error( &
            &   msg  = "The dimension of given 'radius' should be larger than 1.", &
            &   head = "Initialize_typSurfMeshAxisBody" )
        End if

        If (nZPos.le.1) then
            Call GURU%Error( &
            &   msg  = "The dimension of given 'zPos' should be larger than 1.", &
            &   head = "Initialize_typSurfMeshAxisBody" )
        End if

        If ( nRadius.ne.nZPos ) then
            Call GURU%Error( &
            &   msg  = "The dimension of given 'radius' and 'zPos' should be the same.", &
            &   head = "Initialize_typSurfMeshAxisBody" )
        End if

        this%nCoord = nRadius

        If ( nDiscretization.NE.this%nCoord-1 ) then
            Call GURU%Error( &
            &   msg  = "The dimension of 'nDiscretization' should be the dimension of 'radius' - 1.", &
            &   head = "Initialize_typSurfMeshAxisBody" )
        End if


    End Block

    !!... Get Number of Theta to discretize
    Call JSON_GetInt( json, "nQuaterTheta", this%nQuaterTheta )

    If ( this%nQuaterTheta.LE.1 ) then
        Call GURU%Error( &
        &   msg  = "The dimension of given 'nQuaterTheta' should be larger than 1.", &
        &   head = "Initialize_typSurfMeshAxisBody" )
    end if

    this%nTheta = 4 * (this%nQuaterTheta - 1 )

    !!... Get Center Movement
    Call JSON_GetRealVectorOrDefault( json, "centerMove", centerMove, [0.0_RP, 0.0_RP, 0.0_RP] )

    If ( size(centerMove).EQ.3 ) then
        this%centerMove(:) = centerMove(:)
    else
        Call GURU%Error( &
        &   msg  = "The dimension of given 'centerMove' is not 3.", &
        &   head = "Initialize_typSurfMeshAxisBody" )
    end if

    If (Allocated(centerMove)) deallocate(centerMove)

    !!... Create Mesh
    Call this%CreateMesh()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine CreateMesh_typSurfMeshAxisBody(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Class( typSurfMeshAxisBody ), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer  :: nNode, nPanel, iZone, nZone, zoneID, iTheta, panelID
    Integer  :: nodeStart1, nodeStart2, iNode, nodeID, nInterval, iInterval
    Real(RP) :: tmpRadius, tmpZPos, dR, dZ, dTheta, x, y, z
    Real(RP), Allocatable :: theta(:), cosTheta(:), sinTheta(:)
    type(typPoint), Dimension(4) :: nodes
    Integer, Dimension(4) :: nodeLabel
    Integer :: nodeInd1, nodeInd2

    !!... Set Number of Panel and points
    nZone = this%nCoord - 1

    nNode  = 0
    nPanel = 0
    do iZone = 1, nZone
        nPanel = nPanel + this%nTheta * ( this%nDiscretization( iZone ) - 1 )
        nNode  = nNode  + this%nTheta * ( this%nDiscretization( iZone ) - 1 )
    End do
    nNode = nNode + this%nTheta

    this%nNode  = nNode
    this%nPanel = nPanel

    !!... Allocate
    Allocate( this%node(this%nNode)   )
    Allocate( this%panel(this%nPanel) )

    !!... Set theta Array
    Allocate( theta(this%nTheta), cosTheta(this%nTheta), sinTheta(this%nTheta) )
    dTheta = 2.0_RP * PI / dble(this%nTheta)
    do iTheta = 1, this%nTheta
        theta(iTheta)    = dTheta * (iTheta - 1.0_RP)
        cosTheta(iTheta) = dcos( theta(iTheta) )
        sinTheta(iTheta) = dsin( theta(iTheta) )
    End do

    nodeID = 0

    !!... Create the first layer nodes
    tmpRadius = this%radius(1)
    tmpZPos   = this%zPos(1)

    do iTheta = 1, this%nTheta
        nodeID = nodeID + 1

        x = tmpRadius * cosTheta(iTheta)
        y = tmpRadius * sinTheta(iTheta)
        z = tmpZPos

        Call this%node( nodeID )%Set( pidx = nodeID, x = x, y = y, z = z )

    End do

    !!... Generate Nodes
    do iZone = 1, nZone
        zoneID = iZone
        nInterval = this%nDiscretization(zoneID)

        dR = ( this%radius(zoneID + 1) - this%radius(zoneID) ) / ( nInterval - 1.0_RP)
        dZ = ( this%zPos(zoneID + 1)   - this%zPos(zoneID)   ) / ( nInterval - 1.0_RP)

        do iInterval = 2, nInterval
            tmpRadius = this%radius(zoneID) + dR * ( iInterval - 1.0_RP )
            tmpZPos   = this%zPos(zoneID)   + dZ * ( iInterval - 1.0_RP )
            do iTheta = 1, this%nTheta
                nodeID = nodeID + 1
                x = tmpRadius * cosTheta(iTheta)
                y = tmpRadius * sinTheta(iTheta)
                z = tmpZPos
                Call this%node( nodeID )%Set( pidx = nodeID, x = x, y = y, z = z )
            End do
        End do

    End do

    !!... Move node
    do iNode = 1, this%nNode
        x = this%node(iNode)%X()
        y = this%node(iNode)%Y()
        z = this%node(iNode)%Z()
        Call this%node(iNode)%Move( &
        &   x = x + this%centerMove(1), &
        &   y = y + this%centerMove(2), &
        &   z = z + this%centerMove(3)  )
    End do

    !!... Generate Panel
    nodeInd1 = 0
    nodeInd2 = nodeInd1 + this%nTheta

    panelID = 0
    do iZone = 1, nZone
        zoneID    = iZone
        nInterval = this%nDiscretization(zoneID)

        do iInterval = 1, nInterval - 1

            do iTheta = 1, this%nTheta

                panelID = panelID + 1

                if (iTheta.lt.this%nTheta) then
                    ! nodeLabel(1) = nodeInd1 + iTheta
                    ! nodeLabel(2) = nodeInd1 + iTheta + 1
                    ! nodeLabel(3) = nodeInd2 + iTheta + 1
                    ! nodeLabel(4) = nodeInd2 + iTheta

                    nodeLabel(1) = nodeInd1 + iTheta
                    nodeLabel(2) = nodeInd2 + iTheta
                    nodeLabel(3) = nodeInd2 + iTheta + 1
                    nodeLabel(4) = nodeInd1 + iTheta + 1
                else
                    ! nodeLabel(1) = nodeInd1 + iTheta
                    ! nodeLabel(2) = nodeInd1 + 1
                    ! nodeLabel(3) = nodeInd2 + 1
                    ! nodeLabel(4) = nodeInd2 + iTheta

                    nodeLabel(1) = nodeInd1 + iTheta
                    nodeLabel(2) = nodeInd2 + iTheta
                    nodeLabel(3) = nodeInd2 + 1
                    nodeLabel(4) = nodeInd1 + 1
                end if

                ! do iNode = 1, 4
                !     nodes(iNode) = this%node( nodeLabel(iNode) )
                ! End do

                Call this%panel(panelID)%Set( &
                &   nNode     = 4, &
                &   nodes     = this%node(:), &
                &   nodeLabel = nodeLabel)

            End do

            nodeInd1 = nodeInd2
            nodeInd2 = nodeInd1 + this%nTheta

        End do

    End do


    !!... Do Loop
    ! zoneID = 1
    !
    ! nSub =
    !

    !
    ! do iSub = 1, nSub
    !

    !
    !     do iNode = 1, this%nTheta
    !         nodeID = nodeID + 1
    !
    !     End do
    !
    ! End do


    If ( Allocated(theta) ) deallocate( theta )
    If ( Allocated(cosTheta) ) deallocate( cosTheta )
    If ( Allocated(sinTheta) ) deallocate( sinTheta )

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

! Subroutine initializeCirularCylinderShellMesh(this, radius, height, &
!                                                     nR, nZ,         &
!                                                     x0, y0, z0, rZ, &
!                                                     nLineSub,       &
!                                                     nGaussPoint,    &
!                                                     writeLog)
!     Implicit None
!     Class(typCircularCylinderShellSurfMesh), intent(inout)  :: this
!     Real(RP), intent(in)        :: radius, height
!     integer, intent(in)         :: nR, nZ
!     Real(RP), optional          :: x0, y0, z0
!     Real(RP), optional          :: rZ
!     Integer, optional           :: nLineSub
!     Integer, optional           :: nGaussPoint
!     Logical, optional           :: writeLog
!
!     integer                     :: ir, iz
!     real(RP)                    :: theta, dTheta
!     real(RP)                    :: z, dz
!     real(RP)                    :: x, y, xx, yy, zz, R
!     integer                     :: nNode, nPanel
!
!     Integer, Dimension(4)       :: nodeLabel
!     Integer, Dimension(2)       :: lineLabel
!     Real(RP),Dimension(3)       :: refVector
!
!     real(RP)                    :: z1, z2, theta1, theta2
!
!     integer :: nGauss
!     Real(RP), allocatable :: Nx1(:), Nx2(:), Ny1(:), Ny2(:)
!     Real(RP), allocatable :: W1(:), W2(:)
!     integer :: i, j, k
!
!     refVector(1) = 0.0_RP
!     refVector(2) = 0.0_RP
!     refVector(3) = 1.0_RP
!
!     !! Check Parameter
!     if (radius.gt.0.0_RP) then
!         this%radius_ = radius
!     else
!         write(*,*) " "
!         write(*,*) "    [ERROR] initializeCircularCylinderShell(...)"
!         write(*,*) "       radius should be larger than 0."
!         write(*,*) "       given radius : ", radius
!         write(*,*) " "
!         stop
!     end if
!
!     !! Check Parameter
!     if (height.gt.0.0_RP) then
!         this%height_ = height
!     else
!         write(*,*) " "
!         write(*,*) "    [ERROR] initializeCircularCylinderShell(...)"
!         write(*,*) "       height should be larger than 0."
!         write(*,*) "       given height : ", height
!         write(*,*) " "
!         stop
!     end if
!
!     !! Check Parameters
!     if (nLineSub.ge.1) then
!         this%nLineSub = nLineSub
!     else
!         write(*,*) " "
!         write(*,*) "    [Warning] : initializeCircularCylinderShell(...)"
!         write(*,*) "       Condition : nLineSub should larger than 1. "
!         write(*,*) "        - nLineSub = ", nLineSub
!         write(*,*) " "
!         write(*,*) "          nLineSub = 1 will be used."
!         write(*,*) " "
!         this%nLineSub = 1
!     end if
!
!     !! Check Parameters
!     if ((nR.le.3).OR.(nZ.le.2)) then
!         write(*,*) " "
!         write(*,*) "    [ERROR] : initializeCircularCylinderShell(...)"
!         write(*,*) "       Condition : nR >= 3 or nZ >= 2."
!         write(*,*) "        - nR, nZ = ", nR, nZ
!         write(*,*) " "
!         STOP
!     else
!         this%nR_ = nR
!         this%nZ_ = nZ
!     end if
!
!
!     this%x0_ = 0.0_RP;  this%y0_ = 0.0_RP;  this%z0_ = 0.0_RP
!     if (present(x0)) this%x0_ = x0
!     if (present(y0)) this%y0_ = y0
!     if (present(z0)) this%z0_ = z0
!
!     !! Set Parameters
!     this%rZ_ = 1.0_RP
!     if (present(rZ)) then
!         if (rZ.gt.0.00001_RP) then
!             this%rZ_ = rZ
!         else
!             write(*,*) " "
!             write(*,*) "    [ERROR] initializeCircularCylinderShell(...)"
!             write(*,*) "       rZ (meshRatio) should be larger than 0.00001."
!             write(*,*) "       given rZ : ", rZ
!             write(*,*) " "
!             stop
!         end if
!     end if
!
!     !! Generate Mesh
!
!     this%isOpenTop = .TRUE.
!     this%nNode     = nZ * nR
!     this%nPanel    = (nZ - 1) * nR
!
!     allocate(this%node(this%nNode))
!     allocate(this%panel(this%nPanel))
!
!     this%nLineNode = nR * nLineSub
!     this%nLine     = nR * nLineSub
!
!     allocate(this%lineNode(this%nLineNode))
!     allocate(this%linePanel(this%nLine))
!
!     if (this%rZ_.eq. 1) then
!         dz = height  / (nZ - 1.0_RP)
!     else
!         dz = height  * (this%rZ_ - 1.0_RP) / (this%rZ_**(nZ-1) - 1.0_RP)
!     end if
!
!     dtheta  = 2.0_RP * pi / nR
!
!     z = -0.5_RP * height
!     !! Make Node
!     nNode = 0
!     do iz = 1, nZ
!         do ir = 1, nR
!             theta = dtheta * real(ir - 1.0_RP, rp)
!             nNode = nNode + 1
!             x = radius * dcos(theta)
!             y = radius * dsin(theta)
!             Call this%node(nNode)%set(nNode, x, y, z)
!         enddo
!         z  = z + dz
!         dz = dz * this%rZ_
!     enddo
!
!     !! Make Panel
!     nPanel = 0
!     do iz = 1, nZ - 1
!         do ir = 1, nR
!             nPanel = nPanel + 1
!             nodeLabel(1) = (iz - 1) * nR + ir
!             nodeLabel(2) = iz       * nR + ir
!             nodeLabel(3) = iz       * nR + ir + 1
!             nodeLabel(4) = (iz - 1) * nR + ir + 1
!
!             if (ir.eq.nR) then
!                 ! nodeLabel(2) = (iz-1) * nR + 1
!                 nodeLabel(3) = iz * nR + 1
!                 nodeLabel(4) = (iz-1) * nR + 1
!             end if
!             Call this%panel(nPanel)%set(4, this%node, nodeLabel)
!         enddo
!     enddo
!
!     !! Make Line Node
!     dtheta  = 2.0_RP * pi / this%nLine
!     z = 0.5_RP * height
!
!     nNode = 0
!     do ir = 1, this%nLine
!         theta = dtheta * real(ir - 1.0_RP, rp)
!         x = radius * dcos(theta)
!         y = radius * dsin(theta)
!         nNode = nNode + 1
!         Call this%lineNode(nNode)%set(nNode, x, y, z)
!     enddo
!
!     !! Make Line
!     nPanel = 0
!     do ir = 1, this%nLine
!         nPanel   = nPanel + 1
!         lineLabel(1) = ir
!         lineLabel(2) = ir + 1
!         if (ir.eq.this%nLine) lineLabel(2) = 1
!         Call this%linePanel(nPanel)%set(2, this%lineNode, lineLabel, refVector)
!     enddo
!
!     !! Move Mesh with Origin
!     do nNode = 1, this%nNode
!         xx = this%node(nNode)%x() + this%x0_
!         yy = this%node(nNode)%y() + this%y0_
!         zz = this%node(nNode)%z() + this%z0_
!
!         Call this%node(nNode)%move(xx, yy, zz)
!     enddo
!
!     !! Move Line Mesh with Origin
!     do nNode = 1, this%nLineNode
!         xx = this%lineNode(nNode)%x() + this%x0_
!         yy = this%lineNode(nNode)%y() + this%y0_
!         zz = this%lineNode(nNode)%z() + this%z0_
!
!         Call this%lineNode(nNode)%move(xx, yy, zz)
!     enddo
!
!     !! - Reset Surface Mesh
!     do nPanel = 1, this%nPanel
!         Call this%panel(nPanel)%correct(this%node)
!         if (present(nGaussPoint)) then
!             Call this%panel(nPanel)%correctGaussPoint(nGaussPoint)
!         else
!             Call this%panel(nPanel)%correctGaussPoint(1)
!         end if
!     Enddo
!
!     !! - Reset Surface Mesh
!     do nPanel = 1, this%nLine
!         Call this%linePanel(nPanel)%correct(this%lineNode, (/0.0_RP, 0.0_RP, 1.0_RP/))
!         if (present(nGaussPoint)) then
!             Call this%linePanel(nPanel)%correctGaussPoint(nGaussPoint)
!         else
!             Call this%linePanel(nPanel)%correctGaussPoint(1)
!         end if
!     Enddo
!
!     ! if (this%rZ_.eq. 1) then
!     !     dz = height  / (nZ - 1.0_RP)
!     ! else
!     !     dz = height  * (this%rZ_ - 1.0_RP) / (this%rZ_**(nZ-1) - 1.0_RP)
!     ! end if
!     !
!     ! if (present(nGaussPoint)) then
!     !     if (nGaussPoint.ge.1) then
!     !         nGauss = nGaussPoint
!     !     else
!     !         nGauss = 1
!     !     end if
!     ! else
!     !     nGauss = 1
!     ! end if
!     !
!     ! allocate(Nx1(nGauss), Nx2(nGauss), &
!     !          Ny1(nGauss), Ny2(nGauss), &
!     !          W1(nGauss),  W2(nGauss)   )
!     !
!     ! Call getGauss2Coordinates(nGauss, nGauss, Nx1, Nx2, Ny1, Ny2, W1, W2)
!     !
!     ! !! Set Panel Information with analytic value
!     ! nPanel = 0
!     !
!     ! z1      = -0.5_RP * height
!     ! dtheta  = 2.0_RP * pi / nR
!     !
!     ! do iz = 1, nZ - 1
!     !     z2     = z1 + dz
!     !     do ir = 1, nR
!     !         nPanel = nPanel + 1
!     !
!     !         theta1 = dtheta * (ir - 1.0_RP)
!     !         theta2 = dtheta * real(ir, rp)
!     !
!     !         theta = (theta1 + theta2) / 2.0_RP
!     !
!     !         x = radius * dcos(theta) + this%x0_
!     !         y = radius * dsin(theta) + this%y0_
!     !         z = (z1 + z2) / 2.0_RP + this%z0_
!     !         !! Panel Normal, Center, Area
!     !
!     !         this%panel(nPanel)%area = this%radius_ * dtheta * dz
!     !         Call this%panel(nPanel)%center%move(x, y, z )
!     !         Call this%panel(nPanel)%normal%move( -dcos(theta), -dsin(theta), 0.0_RP)
!     !
!     !         !! Gauss Point
!     !         this%panel(nPanel)%nGauss = nGauss * nGauss
!     !         allocate( this%panel(nPanel)%gPoint(this%panel(nPanel)%nGauss) )
!     !         allocate( this%panel(nPanel)%gWeight(this%panel(nPanel)%nGauss) )
!     !
!     !         k = 0
!     !         do i = 1, nGauss
!     !             theta = Nx1(i) * theta1 + Nx2(i) * theta2
!     !             do j = 1, nGauss
!     !                 k = k + 1
!     !
!     !                 x = radius * dcos(theta) + this%x0_
!     !                 y = radius * dsin(theta) + this%y0_
!     !                 z = (Ny1(j) * z1 + Ny2(j) * z2) + this%z0_
!     !
!     !                 Call this%panel(nPanel)%gPoint(k)%set(k, x, y, z)
!     !                 this%panel(nPanel)%gWeight(k) = W1(i) * W2(j) * this%panel(nPanel)%area
!     !
!     !             enddo
!     !         enddo
!     !
!     !     enddo
!     !     z1 = z2
!     !     dz = dz * this%rZ_
!     ! enddo
!     !
!     ! !! Set Line Panel Gauss point
!     !
!     ! !! Set Panel Information with analytic value
!     ! z = 0.5_RP * height + this%z0_
!     ! dtheta  = 2.0_RP * pi / this%nLine
!     !
!     ! nPanel = 0
!     ! do ir = 1, this%nLine
!     !     nPanel = nPanel + 1
!     !
!     !     theta1 = dtheta * (ir - 1.0_RP)
!     !     theta2 = dtheta * real(ir, rp)
!     !
!     !     theta = (theta1 + theta2) / 2.0_RP
!     !
!     !     x = radius * dcos(theta)
!     !     y = radius * dsin(theta)
!     !
!     !     !! Line Panel Normal, Center, Area
!     !     this%linePanel(nPanel)%area = this%radius_ * dtheta
!     !     Call this%linePanel(nPanel)%center%move(x, y, z)
!     !     Call this%linePanel(nPanel)%normal%move( -dcos(theta), -dsin(theta), 0.0_RP)
!     !
!     !     !! Gauss Point
!     !     this%linePanel(nPanel)%nGauss = nGauss
!     !     allocate( this%linePanel(nPanel)%gPoint(this%linePanel(nPanel)%nGauss) )
!     !     allocate( this%linePanel(nPanel)%gWeight(this%linePanel(nPanel)%nGauss) )
!     !
!     !     k = 0
!     !     do i = 1, nGauss
!     !         k = k + 1
!     !
!     !         theta = Nx1(i) * theta1 + Nx2(i) * theta2
!     !
!     !         x = radius * dcos(theta) + this%x0_
!     !         y = radius * dsin(theta) + this%y0_
!     !
!     !         Call this%linePanel(nPanel)%gPoint(k)%set(k, x, y, z)
!     !         this%linePanel(nPanel)%gWeight(k) = W1(i) * this%linePanel(nPanel)%area
!     !
!     !     enddo
!     !
!     ! enddo
!
!
!     if (allocated(Nx1)) deallocate(Nx1)
!     if (allocated(Nx2)) deallocate(Nx2)
!
!     if (allocated(Ny1)) deallocate(Ny1)
!     if (allocated(Ny2)) deallocate(Ny2)
!
!     if (allocated(W1)) deallocate(W1)
!     if (allocated(W2)) deallocate(W2)
!
!
!     !! Write Input Parameters ----------------------------------------------
!     if (present(writeLog)) then
!     if (writeLog.EQV..TRUE.) then
!     write(*,1001) " "
!     write(*,1001) " mkSurfaceMesh : Generate Circular Cylinder Shell Surface Mesh"
!     write(*,1001) " "
!     write(*,1002) "    - Radius                       : ", this%radius_
!     write(*,1002) "    - Height                       : ", this%height_
!     write(*,1003) "    - Nr, Nz, (Number of Nodes)    : ", Nr, Nz
!     write(*,1002) "    - x0, y0, z0 (Center Position) : ", this%x0_, this%y0_, this%z0_
!     write(*,1002) "    - rZ (meshRatio, vertical)     : ", this%rZ_
!     write(*,1003) "    - Number of Sub Line Segment   : ", this%nLineSub
!     write(*,1001) " "
!     end if
!     end if
!
!     1001 FORMAT(a)
!     1002 FORMAT(a, 3(f15.6))
!     1003 FORMAT(a, 3(8x,i7))
!     !! ---------------------------------------------------------------------
!
! End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
