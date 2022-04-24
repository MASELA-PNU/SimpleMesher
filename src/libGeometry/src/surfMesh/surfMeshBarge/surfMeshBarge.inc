!! ------------------------------------------------------------------ !!
!! Procedures : Type Barge
!! ------------------------------------------------------------------ !!
Subroutine Initialize_typSurfMeshBarge( this, json )
!! -------------------------------------------------------------------------- !!
    Implicit None
    class(typSurfMeshBarge), intent(inout) :: this
    Type(typJSON), Pointer                 :: json
!! -------------------------------------------------------------------------- !!
    Real(RP), Allocatable :: centerMove(:)

    Call JSON_GetReal( json, "length",  this%Lx )
    Call JSON_GetReal( json, "breadth", this%Ly )
    Call JSON_GetReal( json, "depth",   this%Lz )

    If (this%Lx.le.0.0_RP) then
        Call GURU%Error( &
        &   msg  = "Given 'length' is equal or less than 0.", &
        &   head = "Initialize_typSurfMeshBarge" )
    end if
    If (this%Ly.le.0.0_RP) then
        Call GURU%Error( &
        &   msg  = "Given 'breadth' is equal or less than 0.", &
        &   head = "Initialize_typSurfMeshBarge" )
    end if
    If (this%Lz.le.0.0_RP) then
        Call GURU%Error( &
        &   msg  = "Given 'depth' is equal or less than 0.", &
        &   head = "Initialize_typSurfMeshBarge" )
    end if

    Call JSON_GetInt( json, "nX", this%nX )
    Call JSON_GetInt( json, "nY", this%nY )
    Call JSON_GetInt( json, "nZ", this%nZ )

    If (this%nX.le.2) then
        Call GURU%Error( &
        &   msg  = "Given 'nX' is equal or less than 2.", &
        &   head = "Initialize_typSurfMeshBarge" )
    end if
    If (this%nY.le.2) then
        Call GURU%Error( &
        &   msg  = "Given 'nY' is equal or less than 2.", &
        &   head = "Initialize_typSurfMeshBarge" )
    end if
    If (this%nZ.le.2) then
        Call GURU%Error( &
        &   msg  = "Given 'nZ' is equal or less than 2.", &
        &   head = "Initialize_typSurfMeshBarge" )
    end if

    Call JSON_GetRealOrDefault( json, "ratioX ", this%rX, 1.0_RP )
    Call JSON_GetRealOrDefault( json, "ratioY ", this%rY, 1.0_RP )
    Call JSON_GetRealOrDefault( json, "ratioZ ", this%rZ, 1.0_RP )

    Call JSON_GetRealVectorOrDefault( json, "centerMove", centerMove, [0.0_RP, 0.0_RP, 0.0_RP] )
    If ( size(centerMove).EQ.3 ) then
        this%centerMove(:) = centerMove(:)
    else
        Call GURU%Error( &
        &   msg  = "The dimension of given 'centerMove' is not 3.", &
        &   head = "Initialize_typSurfMeshAxisBody" )
    end if
    If (Allocated(centerMove)) deallocate(centerMove)

    Call JSON_GetLogicalOrDefault( json, "isOpenTop", this%isOpenTop,.TRUE. )

    !!... Create Mesh
    Call this%CreateMesh()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!

!! -------------------------------------------------------------------------- !!
Subroutine CreateMesh_typSurfMeshBarge(this)
!! -------------------------------------------------------------------------- !!
    Implicit None
    Real(RP),Dimension(3), parameter :: refVector = [0.0_RP, 0.0_RP, 1.0_RP]
    class(typSurfMeshBarge), intent(inout) :: this
!! -------------------------------------------------------------------------- !!
    Integer  :: nXp, nYp, nZp, nXh, nYh, nZh
    Logical  :: xEven, yEven, zEven
    Real(RP) :: rSumX, rSumY, rSumZ
    Real(RP) :: dx, dy, dz
    Real(RP) :: xMin, xMax, yMin, yMax, zMin, zMax
    Integer  :: nNodeP1, nNodeP2, nNodeP3, nNodeP4, nNodeP5, nNodeP6
    Integer, Dimension(4) :: nodeLabel
    Integer  :: iNode, iPanel, ix, iy, iz
    Real(RP) :: xx, yy, zz
    !!... Line
    Integer  :: iLine, nXLine, nYLine, nZLine
    Integer :: nLNodeP1, nLNodeP2, nLNodeP3
    Integer, Dimension(2) :: lineLabel
    Real(RP) :: xxx, yyy

    this%nLineSub = 1

    nXp = this%nX - 1; nYp = this%nY - 1; nZp = this%nZ - 1

    xEven = .FALSE.;    yEven = .FALSE.;    zEven = .FALSE.
    if (mod(nXp, 2).eq.0) xEven = .TRUE.
    if (mod(nYp, 2).eq.0) yEven = .TRUE.
    if (mod(nZp, 2).eq.0) zEven = .TRUE.

    nXh = (nXp - 1) / 2
    nYh = (nYp - 1) / 2
    nZh = (nZp - 1) / 2
    if (xEven) nXh = nXp / 2
    if (yEven) nYh = nYp / 2
    if (zEven) nZh = nZp / 2

    if ( dabs(this%rX-1.0_RP).ge.0.01 ) then
        if (xEven) then
            rSumX = 2.0_RP * (this%rX**nXh-1.0_RP) / (this%rX-1.0_RP)
        else
            rSumX = 2.0_RP * (this%rX**nXh-1.0_RP) / (this%rX-1.0_RP) + this%rX**nXh
        endif
        dx = this%Lx / rSumX
    else
        this%rX = 1.0_RP
        dx      = this%Lx / ( this%Nx - 1.0_RP )
    end if

    if ( dabs(this%rY-1.0_RP).ge.0.01 ) then
            if (yEven) then
                rSumY = 2.0_RP * (this%rY**nYh-1.0_RP) / (this%rY-1.0_RP)
            else
                rSumY = 2.0_RP * (this%rY**nYh-1.0_RP) / (this%rY-1.0_RP) + this%rY**nYh
            endif
            dy = this%Ly / rSumY
        else
            this%rY = 1.0_RP
            dy      = this%Ly / ( this%Ny - 1.0_RP )
        end if

    if ( dabs(this%rZ-1.0_RP).ge.0.01 ) then
        if (this%isOpenTop) then
            rSumZ = (this%rZ**nZp-1.0_RP) / (this%rZ-1.0_RP)
            dz    = this%Lz / rSumZ
        else
        if (zEven) then
            rSumZ = 2.0_RP * (this%rZ**nZh-1.0_RP) / (this%rZ-1.0_RP)
        else
            rSumZ = 2.0_RP * (this%rZ**nZh-1.0_RP) / (this%rZ-1.0_RP) + this%rZ**nZh
        endif
        dz = this%Lz / rSumZ
        endif
    else
        this%rZ = 1.0_RP
        dz      = this%Lz / ( this%Nz - 1.0_RP )
    end if

    xMin = -this%Lx / 2.0_RP + this%centerMove(1)
    xMax =  this%Lx / 2.0_RP + this%centerMove(1)

    yMin = -this%Ly / 2.0_RP + this%centerMove(2)
    yMax =  this%Ly / 2.0_RP + this%centerMove(2)

    zMin = -this%Lz + this%centerMove(3)
    zMax =  this%centerMove(3)

    !! - Allocate Nodes and Panels
    this%nNode  = 2 * this%Nz * (this%Nx + this%Ny - 2) &
    &           + (this%Nx - 2) * (this%Ny - 2) * 2
    this%nPanel = this%nNode - 2

    this%nLineNode = 0
    this%nLine     = 0
    if (this%isOpenTop.eqv..true.) then
        this%nNode     = this%nNode  - (this%Nx - 2) * (this%Ny - 2)
        this%nPanel    = this%nPanel - (this%Nx - 1) * (this%Ny - 1)
        this%nLine     = 2 * this%nLineSub * ( (this%Nx - 1) + (this%Ny - 1) )
        this%nLineNode = 2 * this%nLineSub * ( this%Nx + this%Ny - 2 ) + 2
        allocate(this%lineNode(this%nLineNode))
        allocate(this%linePanel(this%nLine))
    end if

    allocate(this%node(this%nNode))
    allocate(this%panel(this%nPanel))

    nNodeP1 = 0; nNodeP2 = 0; nNodeP3 = 0; nNodeP4 = 0; nNodeP5 = 0; nNodeP6 =0

    !! - Construct Surface Mesh
    iNode  = 0
    iPanel = 0

    !! y = -Ly/2 Plane

    !! - Construct Panel
    do iz = 1, this%Nz
        inode = inode + 1

        if (iz.eq.1) then
            Call this%node(inode)%set( inode, xMin, yMin, zMin )
        else
            if (this%isOpenTop) then
                 zz = dz*this%rZ**(iz - 2)
            else
                if (iz.le.nZh + 1) then
                    zz = dz * this%rZ**(iz - 2)
                else
                    zz = dz * this%rZ**(this%nZ - iz)
                endif
            endif

            Call this%node(inode)%set( inode, xMin, yMin, this%node(inode-1)%z() + zz )

        endif

        do ix = 1, this%Nx - 1
            inode = inode + 1

            if (ix.le.nXh) then
                xx = dx * this%rX**(ix - 1)
            else
                xx = dx * this%rX**(this%nX - ix - 1)
            endif

            Call this%node(inode)%set( inode, this%node(inode-1)%x() + xx, &
                                              this%node(inode-1)%y(), &
                                              this%node(inode-1)%z() )
        enddo
    enddo
    nNodeP1 = iNode

    !! - Construct Node
    do iz = 1, this%Nz - 1
        iPanel = iPanel + 1
        nodeLabel(1) = (iz - 1) * this%Nx + 1
        nodeLabel(2) = iz       * this%Nx + 1
        nodeLabel(3) = iz       * this%Nx + 2
        nodeLabel(4) = (iz - 1) * this%Nx + 2

        Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        do ix = 2, this%Nx - 1
            iPanel = iPanel + 1
            nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
            nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
            nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
            nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)
        enddo
    enddo

    !! x = Lx/2 Plane
    do iz = 1, this%Nz
        inode = inode + 1

        if (iz.eq.1) then
            Call this%node(inode)%set( inode, xMax, yMin + dy, zMin )
        else
            if (this%isOpenTop) then
                 zz = dz*this%rZ**(iz - 2)
            else
                if (iz.le.nZh + 1) then
                    zz = dz * this%rZ**(iz - 2)
                else
                    zz = dz * this%rZ**(this%nZ - iz)
                endif
            endif

            Call this%node(inode)%set( inode, xMax, yMin + dy, this%node(inode-1)%z() + zz )

        endif

        do iy = 2, this%Ny - 1
            inode = inode + 1

            if (iy.le.nYh) then
                yy = dy * this%rY**( iy - 1 )
            else
                yy = dy * this%rY**( this%nY - iy - 1)
            endif

            Call this%node(inode)%set( inode, this%node(inode-1)%x(), &
                                              this%node(inode-1)%y() + yy, &
                                              this%node(inode-1)%z() )
        enddo

    enddo
    nNodeP2 = iNode

    !! - Construct Node
    do iz = 1, this%Nz - 1
        iPanel = iPanel + 1
        nodeLabel(1) = iz     * this%Nx
        nodeLabel(2) = (iz+1) * this%Nx
        nodeLabel(3) = nNodeP1  + iz       * (this%Ny-1) + 1
        nodeLabel(4) = nNodeP1  + (iz - 1) * (this%Ny-1) + 1

        Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        iPanel = iPanel + 1
        nodeLabel(1) = nNodeP1 + (iz-1) * (this%Ny-1) + 1
        nodeLabel(2) = nNodeP1 + iz     * (this%Ny-1) + 1
        nodeLabel(3) = nNodeP1 + iz     * (this%Ny-1) + 2
        nodeLabel(4) = nNodeP1 + (iz-1) * (this%Ny-1) + 2

        Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        do iy = 3, this%Ny - 1
            iPanel = iPanel + 1
            nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
            nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
            nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
            nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)
        enddo
    enddo

    !! y = Ly/2 Plane

    !! - Construct Node
    do iz = 1, this%Nz
        inode = inode + 1

        if (iz.eq.1) then
            Call this%node(inode)%set( inode, xMax - dx, yMax, zMin )
        else
            if (this%isOpenTop) then
                 zz = dz*this%rZ**(iz - 2)
            else
                if (iz.le.nZh + 1) then
                    zz = dz * this%rZ**(iz - 2)
                else
                    zz = dz * this%rZ**(this%nZ - iz)
                endif
            endif

            Call this%node(inode)%set( inode, xMax - dx, yMax, this%node(inode-1)%z() + zz )

        endif

        do ix = 2, this%Nx - 1
            inode = inode + 1

            if (ix.le.nXh) then
                xx = dx * this%rX**( ix - 1 )
            else
                xx = dx * this%rX**( this%nX - ix - 1)
            endif

            Call this%node(inode)%set( inode, this%node(inode-1)%x() - xx, &
                                              this%node(inode-1)%y(), &
                                              this%node(inode-1)%z() )

        enddo
    enddo
    nNodeP3 = iNode

    !! - Construct Panel
    do iz = 1, this%Nz - 1
        iPanel = iPanel + 1
        nodeLabel(1) = nNodeP1 + iz     * (this%Ny-1)
        nodeLabel(2) = nNodeP1 + (iz+1) * (this%Ny-1)
        nodeLabel(3) = nNodeP2 + iz     * (this%Nx-1) + 1
        nodeLabel(4) = nNodeP2 + (iz-1) * (this%Nx-1) + 1
        Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        iPanel = iPanel + 1
        nodeLabel(1) = nNodeP2 + (iz-1) * (this%Nx-1) + 1
        nodeLabel(2) = nNodeP2 + iz     * (this%Nx-1) + 1
        nodeLabel(3) = nNodeP2 + iz     * (this%Nx-1) + 2
        nodeLabel(4) = nNodeP2 + (iz-1) * (this%Nx-1) + 2
        Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        do iy = 3, this%Nx - 1
            iPanel = iPanel + 1
            nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
            nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
            nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
            nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)
        enddo
    enddo

    !! x = -Lx/2 Plane

    !! - Construct Node
    do iz = 1, this%Nz
        inode = inode + 1

        if (iz.eq.1) then
            Call this%node(inode)%set( inode, xMin, yMax - dy, zMin )
        else
            if (this%isOpenTop) then
                 zz = dz*this%rZ**(iz - 2)
            else
                if (iz.le.nZh + 1) then
                    zz = dz * this%rZ**(iz - 2)
                else
                    zz = dz * this%rZ**(this%nZ - iz)
                endif
            endif

            Call this%node(inode)%set( inode, xMin, yMax - dy, this%node(inode-1)%z() + zz )

        endif

        do iy = 2, this%Ny - 2
            inode = inode + 1

            if (iy.le.nYh) then
                yy = dy * this%rY**( iy - 1 )
            else
                yy = dy * this%rY**( this%nY - iy - 1)
            endif

            Call this%node(inode)%set( inode, this%node(inode-1)%x(), &
                                              this%node(inode-1)%y() - yy, &
                                              this%node(inode-1)%z() )

        enddo
    enddo
    nNodeP4 = iNode

    !! - Construct Panel
    do iz = 1, this%Nz - 1
        if (this%Ny.eq.2) then
            iPanel   = iPanel + 1
            nodeLabel(1) = nNodeP2 + iz     * (this%Nx-1)
            nodeLabel(2) = nNodeP2 + (iz+1) * (this%Nx-1)
            nodeLabel(3) = iz      * (this%Nx-1)+ 1
            nodeLabel(4) = (iz-1)  * (this%Nx-1)+ 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        else if (this%Ny.eq.3) then
            iPanel   = iPanel + 1
            nodeLabel(1) = nNodeP2 + iz     * (this%Nx-1)
            nodeLabel(2) = nNodeP2 + (iz+1) * (this%Nx-1)
            nodeLabel(3) = nNodeP3 + iz     * (this%Ny-2) + 1
            nodeLabel(4) = nNodeP3 + (iz-1) * (this%Ny-2) + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            iPanel  = iPanel + 1
            nodeLabel(1) = nNodeP3 + (iz-1) * (this%Ny-2) + 1
            nodeLabel(2) = nNodeP3 + iz     * (this%Ny-2) + 1
            nodeLabel(3) = iz      * this%Nx + 1
            nodeLabel(4) = (iz-1)  * this%Nx + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)
        else
            iPanel   = iPanel + 1
            nodeLabel(1) = nNodeP2 + iz     * (this%Nx-1)
            nodeLabel(2) = nNodeP2 + (iz+1) * (this%Nx-1)
            nodeLabel(3) = nNodeP3 + iz     * (this%Ny-2) + 1
            nodeLabel(4) = nNodeP3 + (iz-1) * (this%Ny-2) + 1
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            iPanel   = iPanel + 1
            nodeLabel(1) = nNodeP3 + (iz-1) * (this%Ny-2) + 1
            nodeLabel(2) = nNodeP3 + iz     * (this%Ny-2) + 1
            nodeLabel(3) = nNodeP3 + iz     * (this%Ny-2) + 2
            nodeLabel(4) = nNodeP3 + (iz-1) * (this%Ny-2) + 2
            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do iy = 3, this%Ny - 2
                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel - 1 )%nodeLabel(3) + 1
                nodeLabel(4) = this%panel(iPanel - 1 )%nodeLabel(4) + 1
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            enddo

            iPanel   = iPanel + 1
            nodeLabel(1) = this%panel(iPanel - 1 )%nodeLabel(1) + 1
            nodeLabel(2) = this%panel(iPanel - 1 )%nodeLabel(2) + 1
            nodeLabel(3) = iz     * this%Nx + 1
            nodeLabel(4) = (iz-1) * this%Nx + 1

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)
        endif
    enddo

    yy = 0.0_RP
    !! Bottom
    do iy = 1, this%Ny -2

        if (iy.le.nYh) then
            yy = yy + dy * this%rY**( iy - 1 )
        else
            yy = yy + dy * this%rY**( this%nY - iy - 1)
        endif

        xx = 0.0_RP
        do ix = 1, this%Nx -2
            inode = inode + 1

            if (ix.le.nXh) then
                xx = xx + dx * this%rX**( ix - 1 )
            else
                xx = xx + dx * this%rX**( this%nX - ix - 1)
            endif

            Call this%node(inode)%set(iNode, xMin + xx, yMin + yy, zMin)

        enddo
    enddo
    nNodeP5 = inode

    do iy = 1, this%Ny - 1
        if (iy.eq.1) then

            iPanel   = iPanel + 1
            nodeLabel(1) = 1
            nodeLabel(2) = 2
            nodeLabel(3) = nNodeP4 + 1
            nodeLabel(4) = nNodeP3 + (this%Ny-1) - iy

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do ix = 2, this%Nx - 2
                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                nodeLabel(4) = nodeLabel(3) - 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            end do

            iPanel   = iPanel + 1
            nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
            nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
            nodeLabel(3) = nNodeP1 + iy
            nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        else if (iy.eq.this%Ny-1) then

            iPanel   = iPanel + 1
            nodeLabel(1) = this%panel(iPanel-(this%Nx-1))%nodeLabel(4)
            nodeLabel(2) = this%panel(iPanel-(this%Nx-1))%nodeLabel(3)
            nodeLabel(3) = nNodeP2 + (this%Nx-2)
            nodeLabel(4) = nNodeP2 + (this%Nx-1)

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do ix = 2, this%Nx - 2
                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) - 1
                nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)
                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            end do

            iPanel   = iPanel + 1
            nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
            nodeLabel(2) = nNodeP1 + this%Ny - 2
            nodeLabel(3) = nNodeP1 + this%Ny - 1
            nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        else

            iPanel   = iPanel + 1
            nodeLabel(1) = this%panel(iPanel-(this%Nx-1))%nodeLabel(4)
            nodeLabel(2) = this%panel(iPanel-(this%Nx-1))%nodeLabel(3)
            nodeLabel(3) = nNodeP4 + (iy-1)*(this%Nx-2) + 1
            nodeLabel(4) = nNodeP3 + (this%Ny-1) - iy

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            do ix = 2, this%Nx - 2
                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(2) + 1
                nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)
            end do

            iPanel   = iPanel + 1
            nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(2)
            nodeLabel(2) = nNodeP1 + iy - 1
            nodeLabel(3) = nNodeP1 + iy
            nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(3)

            Call this%panel(iPanel)%set(4, this%node, nodeLabel)

        endif
    end do

    !! Top (if top is closed)
    if (this%isOpenTop.EQV..FALSE.) then
        !! Generate Surface Mesh
        yy = 0.0_RP
        do iy = 1, this%Ny - 2

            if (iy.le.nYh) then
                yy = yy + dy * this%rY**( iy - 1 )
            else
                yy = yy + dy * this%rY**( this%nY - iy - 1)
            endif

            xx = 0.0_RP
            do ix = 1, this%Nx - 2
                inode = inode + 1

                if (ix.le.nXh) then
                    xx = xx + dx * this%rX**( ix - 1 )
                else
                    xx = xx + dx * this%rX**( this%nX - ix - 1)
                endif

                Call this%node(inode)%set(iNode, xMin + xx, yMin + yy, zMax)
            enddo
        enddo
        nNodeP6 = inode

        do iy = 1, this%Ny-1
            if (iy.eq.1) then

                iPanel   = iPanel + 1
                nodeLabel(1) = (this%Nz-1)*this%Nx + 1
                nodeLabel(2) = nNodeP4
                nodeLabel(3) = nNodeP5 + 1
                nodeLabel(4) = nodeLabel(1) + 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, this%Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                    nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(4) + 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(1) + 1
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                nodeLabel(3) = nNodeP2 - (this%Ny - 2)
                nodeLabel(4) = nNodeP1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else if (iy.eq.this%Ny-1) then

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-(this%Nx-1))%nodeLabel(2)
                nodeLabel(2) = nNodeP3
                nodeLabel(3) = nNodeP3 - 1
                nodeLabel(4) = this%panel(iPanel-(this%Nx-1))%nodeLabel(3)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, this%Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                    nodeLabel(3) = nNodeP3 - ix
                    nodeLabel(4) = nodeLabel(1) + 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                nodeLabel(3) = nNodeP2
                nodeLabel(4) = nNodeP2 - 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            else

                iPanel   = iPanel + 1
                nodeLabel(1) = nNodeP4 + 2 - iy
                nodeLabel(2) = nNodeP4 + 1 - iy
                nodeLabel(3) = nNodeP5 + (this%Nx-2)*(iy-1) + 1
                nodeLabel(4) = nNodeP5 + (this%Nx-2)*(iy-2) + 1

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

                do ix = 2, this%Nx - 2
                    iPanel   = iPanel + 1
                    nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                    nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                    nodeLabel(3) = this%panel(iPanel-1)%nodeLabel(3) + 1
                    nodeLabel(4) = this%panel(iPanel-1)%nodeLabel(4) + 1

                    Call this%panel(iPanel)%set(4, this%node, nodeLabel)
                end do

                iPanel   = iPanel + 1
                nodeLabel(1) = this%panel(iPanel-1)%nodeLabel(4)
                nodeLabel(2) = this%panel(iPanel-1)%nodeLabel(3)
                nodeLabel(3) = nNodeP2 - (this%Ny - 2) + (iy-1)
                nodeLabel(4) = nNodeP2 - (this%Ny - 2) + (iy-2)

                Call this%panel(iPanel)%set(4, this%node, nodeLabel)

            end if
        Enddo

    else

        !! Generate Line Mesh

        nXLine = this%nLineSub * (this%Nx - 1)
        nYLine = this%nLineSub * (this%Ny - 1)

        !! y = -Ly / 2
        iNode  = 0
        iPanel = 0

        do ix = 1, this%Nx - 1

            if (ix.eq.1) then
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, xMin , yMin, zMax)
            end if

            if (ix.le.nXh) then
                xx = dx * this%rX**( ix - 1 )
            else
                xx = dx * this%rX**( this%nX - ix - 1 )
            endif

            xxx = xx / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, this%lineNode(iNode-1)%x() + xxx , yMin, zMax)
            enddo

        enddo

        nLNodeP1 = iNode

        do ix = 1, nXLine
            iPanel   = iPanel + 1
            lineLabel(1) = ix
            lineLabel(2) = ix + 1
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)
        enddo

        !! x = Lx / 2
        do iy = 1, this%Ny - 1

            if (iy.le.nYh) then
                yy = dy * this%rY**( iy - 1 )
            else
                yy = dy * this%rY**( this%nY - iy - 1 )
            endif

            yyy = yy / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, xMax , this%lineNode(iNode-1)%y() + yyy, zMax)
            enddo

        enddo
         nLNodeP2 = iNode

        do iy = 1, nYLine
            iPanel   = iPanel + 1
            lineLabel(1) = nLNodeP1 + iy - 1
            lineLabel(2) = nLNodeP1 + iy
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)
        enddo

        !! y = Ly / 2
        do ix = 1, this%nX - 1

            if (ix.le.nXh) then
                xx = dx * this%rX**( ix - 1 )
            else
                xx = dx * this%rX**( this%nX - ix - 1 )
            endif

            xxx = xx / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, this%lineNode(iNode-1)%x() - xxx , yMax, zMax)
            enddo

        enddo
        nLNodeP3 = iNode

        do ix = 1, nXLine
            iPanel   = iPanel + 1
            lineLabel(1) = nLNodeP2 + ix - 1
            lineLabel(2) = nLNodeP2 + ix
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)
        enddo

        !! x = -Lx/2
        do iy = 1, this%nY - 1

            if (iy.le.nYh) then
                yy = dy * this%rY**( iy - 1 )
            else
                yy = dy * this%rY**( this%nY - iy - 1 )
            endif

            yyy = yy / this%nLineSub

            do iline = 1, this%nLineSub
                iNode = iNode + 1
                Call this%lineNode(iNode)%set(iNode, xMin, this%lineNode(iNode-1)%y() - yyy, zMax)
            enddo

        enddo

        do iy = 1, nYLine
            iPanel   = iPanel + 1
            lineLabel(1) = nLNodeP3 + iy - 1
            if (iy.eq. this%nLineSub * (this%Ny-1)) then
                lineLabel(2) = 1
            else
                lineLabel(2) = nLNodeP3 + iy
            endif
            Call this%linePanel(iPanel)%set(2, this%lineNode, lineLabel, refVector)

        enddo

        this%nLine = iPanel
        this%nLineNode = iNode

    end if

    !!... Revert the order
    do iPanel = 1, this%nPanel
        Call this%panel(iPanel)%Revert()
    end do

    do iPanel = 1, this%nLine
        Call this%linePanel(iPanel)%Revert()
    end do

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
