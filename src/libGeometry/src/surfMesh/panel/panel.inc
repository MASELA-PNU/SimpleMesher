!! ------------------------------------------------------------------ !!
!! Procedures : Type Panel
!! ------------------------------------------------------------------ !!

    ! Subroutine setPanel(this, nNode, nodes, vec)
    Subroutine Set_typPanel(this, nNode, nodes, nodeLabel, vec)
        Implicit None
        Class(typPanel), intent(inout)               :: this
        Integer, intent(in)                          :: nNode
        type(typPoint), intent(in)                   :: nodes(:)
        integer, dimension(nNode), intent(in)        :: nodeLabel
        real(rp),dimension(3), optional :: vec
        integer :: i
        real(rp),dimension(3) :: pos

        if (nNode.le.1) then
            Call GURU%Error( &
            &   msg  = "nNode should be larger than 1.", &
            &   head = "Set_typPanel" )
        endif

        this%nNode_ = nNode
        allocate( this%nodeLabel_(nNode) )
        allocate( this%node_(nNode) )

        this%vtkCellType_ = 7
        if (nNode.eq.2) this%vtkCellType_ = 3
        if (nNode.eq.3) this%vtkCellType_ = 5
        if (nNode.eq.4) this%vtkCellType_ = 9

        do i = 1, nNode
            this%nodeLabel_(i) = nodeLabel(i)
            pos = nodes(this%nodeLabel_(i))%vec()
            Call this%node_(i)%set(i, pos(1), pos(2), pos(3) )
        enddo

        Call this%Correct( nodes, vec )

    End Subroutine

    Subroutine CorrectGaussPoint_typPanel(this, nGaussPoint)
        Class(typPanel), intent(inout)  :: this
        integer                         :: nGaussPoint
        !! Local Variables
        real(rp),dimension(nGaussPoint) :: xi, eta
        Real(RP),dimension(nGaussPoint) :: Nx1, Nx2, Ny1, Ny2
        Real(RP),dimension(nGaussPoint) :: W1, W2
        integer :: i, j, k
        real(rp), dimension(3) :: x, x1, x2, x3, x4


        if (this%nNode_.eq.2) then

            Call GetGauss1Coordinates(nGaussPoint, Nx1, Nx2, W1)

            x1 = this%node_(1)%vec()
            x2 = this%node_(2)%vec()

            if (allocated(this%gPoint)) deallocate(this%gPoint)
            if (allocated(this%gWeight)) deallocate(this%gWeight)

            this%nGauss = nGaussPoint
            allocate(this%gPoint(this%nGauss), this%gWeight(this%nGauss))

            k = 0
            do i = 1, nGaussPoint
                k = k + 1
                x = Nx1(i) * x1 + Nx2(i) * x2
                Call this%gPoint(k)%set(k, x(1), x(2), x(3))
                this%gWeight(k) = W1(i) * this%area
            enddo

        else if (this%nNode_.eq.4) then

            Call  GetGauss2Coordinates(nGaussPoint, nGaussPoint, Nx1, Nx2, Ny1, Ny2, W1, W2)

            x1 = this%node_(1)%vec()
            x2 = this%node_(2)%vec()
            x3 = this%node_(3)%vec()
            x4 = this%node_(4)%vec()

            if (allocated(this%gPoint)) deallocate(this%gPoint)
            if (allocated(this%gWeight)) deallocate(this%gWeight)
            this%nGauss = nGaussPoint * nGaussPoint
            allocate(this%gPoint(this%nGauss), this%gWeight(this%nGauss))

            k = 0
            do j = 1, nGaussPoint
                do i = 1, nGaussPoint
                    k = k + 1

                    x = Nx1(i) * Ny1(j) * x1 &
                      + Nx2(i) * Ny1(j) * x2 &
                      + Nx2(i) * Ny2(j) * x3 &
                      + Nx1(i) * Ny2(j) * x4

                     Call this%gPoint(k)%set(k, x(1), x(2), x(3))

                    this%gWeight(k) = W1(i) * W2(j) * this%area

                enddo
            enddo

        else
            Call GURU%Error( &
            &   msg  = "Quadrilateral or Line Panel are only Implemented.", &
            &   head = "CorrectGaussPoint_typPanel" )
        end if

    End Subroutine

    Subroutine Correct_typPanel(this, nodes, vec)
        Implicit None
        Class(typPanel), intent(inout)  :: this
        type(typPoint), intent(in)      :: nodes(:)
        real(rp),dimension(3), optional :: vec
        !! ----------------------------------------------------------------
        integer  :: i
        integer  :: iNode, jNode, kNode
        Real(RP) :: xc, yc, zc
        Real(RP),dimension(3) :: vec1, vec2, vec3, nVec, sumNvec
        Real(RP) :: magV, area
        Real(RP) :: sumArea
        Real(RP),dimension(3) :: pos

        xc = 0.0_RP; yc = 0.0_RP; zc = 0.0_RP

        do i = 1, this%nNode_
            xc = xc + nodes( this%nodeLabel_(i) )%x()
            yc = yc + nodes( this%nodeLabel_(i) )%y()
            zc = zc + nodes( this%nodeLabel_(i) )%z()

            pos = nodes(this%nodeLabel_(i))%vec()
            Call this%node_(i)%set(i, pos(1), pos(2), pos(3) )
        enddo

        !! Panel Center
        xc = xc / real(this%nNode_, RP)
        yc = yc / real(this%nNode_, RP)
        zc = zc / real(this%nNode_, RP)

        Call this%center%set(-1, xc, yc, zc)

        if (this%nNode_.eq.2) then

            iNode = this%nodeLabel_(2)
            jNode = this%nodeLabel_(1)

            vec1(1) = nodes(jNode)%x() - nodes(iNode)%x()
            vec1(2) = nodes(jNode)%y() - nodes(iNode)%y()
            vec1(3) = nodes(jNode)%z() - nodes(iNode)%z()

            if (present(vec)) then
                vec2 = vec
            else
                Call GURU%Error( &
                &   msg  = "Reference vector should be given.", &
                &   head = "Correct_typPanel" )
            endif

            !! Cross Product
            vec3(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
            vec3(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
            vec3(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

            magV = dsqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2) + vec1(3)*vec1(3))

            !! Area
            this%area = magV

            magV = dsqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))
            vec3 = vec3 / magV
            Call this%normal%set(-1, vec3(1), vec3(2), vec3(3) )

        else if (this%nNode_.eq.3) then

            iNode = this%nodeLabel_(1)
            jNode = this%nodeLabel_(2)
            kNode = this%nodeLabel_(3)

            vec1(1) = nodes(jNode)%x() - nodes(iNode)%x()
            vec1(2) = nodes(jNode)%y() - nodes(iNode)%y()
            vec1(3) = nodes(jNode)%z() - nodes(iNode)%z()

            vec1(1) = nodes(kNode)%x() - nodes(jNode)%x()
            vec1(2) = nodes(kNode)%y() - nodes(jNode)%y()
            vec1(3) = nodes(kNode)%z() - nodes(jNode)%z()

            !! Cross Product
            vec3(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
            vec3(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
            vec3(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

            !! Area
            magV = dsqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))
            this%area = 0.5_RP * magV

            !! Normal
            vec3 = vec3 / magV
            Call this%normal%Set(-1, vec3(1) , vec3(2), vec3(3))

        else
        !! Panel normal vector and area
        sumNvec = 0.0_RP
        sumArea = 0.0_RP
        do i = 1, this%nNode_
            iNode = this%nodeLabel_(i)
            if (i.eq.this%nNode_) then
                jNode = this%nodeLabel_(1)
            else
                jNode = this%nodeLabel_(i + 1)
            end if

            vec1(1) = nodes(iNode)%x() - xc; vec1(2)  = nodes(iNode)%y() - yc; vec1(3)  = nodes(iNode)%z() - zc
            vec2(1) = nodes(jNode)%x() - xc; vec2(2)  = nodes(jNode)%y() - yc; vec2(3)  = nodes(jNode)%z() - zc

            !! Cross Product
            vec3(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
            vec3(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
            vec3(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

            !! Area
            magV = dsqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))

            area = 0.5_RP * magV
            nVec = vec3 / magV

            sumArea = sumArea + area
            sumNvec = sumNvec + area * nVec

        enddo
        !! Area weighed normal
        sumNvec = sumNvec / sumArea
        !! Area
        this%area   = sumArea

        Call this%normal%Set(-1, sumNvec(1) , sumNvec(2), sumNvec(3))

        end if

    End Subroutine

    Function nNode_typPanel(this) Result(nNode)
        Implicit None
        class(typPanel), intent(in) :: this
        Integer :: nNode
        nNode = this%nNode_
    End Function

    Function NodeLabel_typPanel(this, index) Result(NodeLabel)
        Implicit None
        class(typPanel), intent(in) :: this
        integer,intent(in) :: index
        Integer :: NodeLabel
        NodeLabel = -1
        if ((index.le.0).or.(index.gt.this%nNode_)) then
            Call GURU%Error( &
            &   msg      = "Wrong Panel Node Index.", &
            &   head     = "NodeLabel_typPanel", &
            &   value    = index, &
            &   refValue = this%nNode_  )
        else
            NodeLabel = this%nodeLabel_(index)
        end if
    End Function

    Function vtkCellType_typPanel(this) result( vtkCellType )
        Implicit None
        class(typPanel), intent(in) :: this
        Integer :: vtkCellType
        vtkCellType = this%vtkCellType_
    End Function

    Subroutine Copy_typPanel(to, from)
        Implicit None
        class(typPanel), intent(inout) :: to
        class(typPanel), intent(in)    :: from

        to%nNode_ = from%nNode_

        if (to%nNode_.ge.1) then
            allocate(to%nodeLabel_(to%nNode_))
            to%nodeLabel_ = from%nodeLabel_
        end if

        to%vtkCellType_ = from%vtkCellType_
        to%area         = from%area
        to%center       = from%center
        to%normal       = from%normal

    End Subroutine

    Subroutine Destroy_typPanel(this)
        Implicit None
        Class(typPanel), intent(inout) :: this
        if (allocated(this%nodeLabel_)) deallocate(this%nodeLabel_)

        this%nNode_       = 0
        this%vtkCellType_ = -1
        this%area         = 0.0_RP
        this%nGauss       = 0

        if (allocated(this%gPoint)) deallocate(this%gPoint)
        if (allocated(this%gWeight)) deallocate(this%gWeight)

    End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
