!! ------------------------------------------------------------------ !!
!! Procedures : Type surfMesh
!! ------------------------------------------------------------------ !!

Subroutine InitializeSurfMesh( surfMesh, json )
    Implicit None
    class(typSurfMesh), allocatable :: surfMesh
    Type(typJSON), Pointer          :: json
    !!... Local Variables
    Character(len=:), Allocatable :: meshType
    Type(typJSON), Pointer        :: jsonMesh
    !!... Each Mesh
    Type(typSurfMeshAxisBody) :: axisBody

    !!... Local Variables
    ! Type(typParaPipeMesh)           :: paraPipeMesh
    ! Type(typSphereSurfMesh)         :: sphereMesh
    ! Type(typHemiSphereSurfMesh)     :: hSphereMesh
    ! Type(typEllipsoidSurfMesh)      :: ellipsoidMesh
    ! Type(typSemiEllipsoidSurfMesh)  :: semiEllipsoidMesh
    ! Type(typCircularCylinderShellSurfMesh)  :: circularCylinderShell
    ! Character(len=CHAR_LEN) :: surfaceType
    ! Logical :: isWriteVTK

    if ( allocated(surfMesh) ) deallocate( surfMesh )

    !!... Get meshType and Child JSON
    Call JSON_GetChar( json, "meshType", meshType )
    Call JSON_GetChild( json, meshType, jsonMesh )

    If ( isStringEqual(meshType, "axisBody") ) then

        Call axisBody%Initialize( json = jsonMesh )

        !!... Allocate
        Allocate( surfMesh, source = axisBody )

    else

        Call GURU%Error( &
        &   msg  = "Given 'meshType' is not supported.", &
        &   head = "InitializeSurfMesh" )

    end if

End Subroutine

Subroutine ManualSet_typSurfMesh(this, nodePos, panelConnect)
    Implicit None
    class(typSurfMesh), intent(inout)     :: this
    Real(RP), dimension(:, :), intent(in) :: nodePos
    Integer, dimension(:, :), intent(in)  :: panelConnect
    !!... Local Variables
    Integer :: iNode, iPanel, nConnect, nodeDim1

    Call this%Destroy()

    nodeDim1 = size(nodePos, 1)
    nConnect = size(panelConnect, 1)

    If ( nodeDim1.ne.3 ) then
        Call GURU%Error( &
        &   msg  = "The first size of 'nodePos' should be 3.", &
        &   head = "ManualSet_typSurfMesh" )
    end if

    If ( nConnect.ne.4 ) then
        Call GURU%Error( &
        &   msg  = "The first size of 'panelConnect' should be 4.", &
        &   head = "ManualSet_typSurfMesh" )
    end if


    this%nNode  = size(nodePos, 2)
    this%nPanel = size(panelConnect, 2)

    If ( this%nNode.le.4 ) then
        Call GURU%Error( &
        &   msg  = "The number of nodes should be larger than 4.", &
        &   head = "ManualSet_typSurfMesh" )
    end if

    If ( this%nPanel.le.0 ) then
        Call GURU%Error( &
        &   msg  = "The number of panels should be larger than 1.", &
        &   head = "ManualSet_typSurfMesh" )
    end if

    Allocate( this%node(this%nNode) )
    Allocate( this%panel(this%nPanel) )

    do iNode = 1, this%nNode
        Call this%node(iNode)%Set( &
        &   pidx = iNode, &
        &   x = nodePos(1, iNode), &
        &   y = nodePos(2, iNode), &
        &   z = nodePos(3, iNode)  )
    End do

    do iPanel = 1, this%nPanel
        Call this%panel(iPanel)%Set( &
        &   nNode     = 4, &
        &   nodes     = this%node(:), &
        &   nodeLabel = panelConnect(:, iPanel) )
    End do

End Subroutine

Subroutine WriteHydroStarMesh_typSurfMesh(this, fileName)
    Implicit None
    class(typSurfMesh), intent(in) :: this
    Character(len=*),intent(in)    :: fileName
    !!... Local Variabls
    Character(len=:), Allocatable  :: filePath
    Integer :: fid, iNode, iPanel, nPanelNode, imod
    Integer, dimension(4) :: nodeLabels

    filePath = trim(adjustl(fileName))//".hst"

    !!
    Open(newunit = fid, file = filePath, status = "replace")

    !!... Write Node Info
    write(fid, 1001) "COORDINATES"
    do iNode = 1, this%nNode
        write(fid, 1002) iNode, this%node(iNode)%Vec()
    end do
    write(fid, 1001) "ENDCOORDINATES"
    write(fid, 1001) " "
    write(fid, 1001) "PANEL"
    do iPanel = 1, this%nPanel
        nPanelNode = this%panel(iPanel)%nNode()

        nodeLabels(:) = 0
        do imod = 1, nPanelNode
            nodeLabels(imod) = this%panel(iPanel)%NodeLabel(imod)
        end do

        write(fid, 1003) iPanel, nodeLabels(:)
    end do
    write(fid, 1001) "ENDPANEL"
    write(fid, 1001) " "
    write(fid, 1001) "ENDFILE"

    1001 format(a)
    1002 format(i5, 99(1pe15.6))
    1003 format(99(i8))

    close(fid)

End Subroutine

Subroutine WriteVTK_typSurfMesh(this, fileName)
    Implicit None
    class(typSurfMesh), intent(in) :: this
    Character(len=*),intent(in)    :: fileName
    Character(len=:), Allocatable  :: surfMeshPath, lineMeshPath
    Character(len=:), Allocatable  :: surfMeshGaussPath, lineMeshGaussPath

    surfMeshPath        = trim(adjustl(fileName))//".vtk"
    Call this%WriteVTKSurfMesh( surfMeshPath )

End Subroutine

Subroutine WriteVTKSurfMesh_typSurfMesh(this, filePath)
    Implicit None
    class(typSurfMesh), intent(in) :: this
    Character(len=*), intent(in)   :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nNode.ge.1).and.(this%nPanel.ge.1)) then

        Open(newunit = ioUnit,   &
             file    = trim(filePath), &
             status  = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nNode, " float"
        do i = 1, this%nNode
            write(ioUnit,1003) this%node(i)%x(), this%node(i)%y(), this%node(i)%z()
        enddo

        nConnect = 0
        do i =1, this%nPanel
            nConnect = nConnect + this%panel(i)%nNode() + 1
        enddo

        write(ioUnit,1004) "CELLS ", this%nPanel, nConnect
        do i = 1, this%nPanel
            write(ioUnit,1005) this%panel(i)%nNode(), &
                              (this%panel(i)%nodeLabel(j) - 1, j = 1,this%panel(i)%nNode())
        enddo

        write(ioUnit,1002) "CELL_TYPES ", this%nPanel
        do i = 1, this%nPanel
            write(ioUnit,1006) this%panel(i)%vtkCellType()
        enddo

        write(ioUnit,1002) "CELL_DATA ", this%nPanel
        write(ioUnit,1001) "SCALARS Panel_Area float"
        write(ioUnit,1001) "LOOKUP_TABLE default"
        do i = 1, this%nPanel
            write(ioUnit,1003) this%panel(i)%area
        enddo

        write(ioUnit,1001) "NORMALS Panel_Normal float"
        do i = 1, this%nPanel
            write(ioUnit,1003) &
                this%panel(i)%normal%x(), &
                this%panel(i)%normal%y(), &
                this%panel(i)%normal%z()
        enddo

        Close(ioUnit)

    else

        Call GURU%Warn( &
        &   msg  = "Number of Node and Panel should be larger than 1."//" \n"//&
            &"  - Number of Node  : "//Int2Char(this%nNode)//" \n"&
            &"  - Number of Panel : "//Int2Char(this%nPanel), &
        &   head = "WriteVTKSurfMesh_typSurfMesh" )

    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,99(i8))
    1006 format(i5)

End Subroutine

Subroutine WriteVTKSurfGaussPoint_typSurfMesh(this, filePath)
    Implicit None
    class(typSurfMesh), intent(in) :: this
    Character(len=*), intent(in)   :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nNode.ge.1).and.(this%nPanel.ge.1)) then

        Open(newunit = ioUnit,   &
             file    = trim(filePath), &
             status  = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nPanel * this%panel(1)%nGauss, " float"
        do i = 1, this%nPanel
            do j = 1, this%panel(1)%nGauss
                write(ioUnit,1003) &
                &   this%panel(i)%gPoint(j)%x(), &
                &   this%panel(i)%gPoint(j)%y(), &
                &   this%panel(i)%gPoint(j)%z()
            enddo
        enddo

        ! write(ioUnit,1002) "CELL_DATA ", this%nPanel
        ! write(ioUnit,1001) "SCALARS Panel_Area float"
        ! write(ioUnit,1001) "LOOKUP_TABLE default"
        ! do i = 1, this%nPanel
        ! write(ioUnit,1003) this%panel(i)%area
        ! enddo
        ! write(ioUnit,1001) "NORMALS Panel_Normal float"
        ! do i = 1, this%nPanel
        ! write(ioUnit,1003) this%panel(i)%normal%x(), this%panel(i)%normal%y(), this%panel(i)%normal%z()
        ! enddo

        Close(ioUnit)
    else

        Call GURU%Warn( &
        &   msg  = "Number of Node and Panel should be larger than 1."//" \n"//&
            &"  - Number of Node  : "//Int2Char(this%nNode)//" \n"&
            &"  - Number of Panel : "//Int2Char(this%nPanel), &
        &   head = "WriteVTKSurfGaussPoint_typSurfMesh" )

    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,99(i8))
    1006 format(i5)

End Subroutine

Subroutine WriteVTKLineMesh_typSurfMesh(this, filePath)
    Implicit None
    class(typSurfMesh), intent(in) :: this
    Character(len=*)               :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nLineNode.ge.1).and.(this%nLine.ge.1)) then

        Open(newunit = ioUnit,   &
             file    = trim(filePath), &
             status  = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nLineNode, " float"
        do i = 1, this%nLineNode
            write(ioUnit,1003) &
            &   this%lineNode(i)%x(), &
            &   this%lineNode(i)%y(), &
            &   this%lineNode(i)%z()
        enddo

        write(ioUnit,1004) "CELLS ", this%nLine, this%nLine * 3
        do i = 1, this%nLine
            write(ioUnit,1005) &
            &   this%linePanel(i)%nNode(),           &
            &   (this%linePanel(i)%nodeLabel(j)-1, j = 1, this%linePanel(i)%nNode())
        enddo

        write(ioUnit,1002) "CELL_TYPES ", this%nLine
        do i = 1, this%nLine
            write(ioUnit,1006) this%linePanel(i)%vtkCellType()
        enddo

        write(ioUnit,1002) "CELL_DATA ", this%nLine
        write(ioUnit,1001) "SCALARS Panel_Area float"
        write(ioUnit,1001) "LOOKUP_TABLE default"
        do i = 1, this%nLine
            write(ioUnit,1003) this%linePanel(i)%area
        enddo
        write(ioUnit,1001) "NORMALS Panel_Normal float"
        do i = 1, this%nLine
            write(ioUnit,1003) &
            &   this%linePanel(i)%normal%x(), &
            &   this%linePanel(i)%normal%y(), &
            &   this%linePanel(i)%normal%z()
        enddo

        Close(ioUnit)
    else

        Call GURU%Warn( &
        &   msg  = "Number of Node and Panel should be larger than 1."//" \n"//&
            &"  - Number of Node  : "//Int2Char(this%nNode)//" \n"&
            &"  - Number of Panel : "//Int2Char(this%nPanel), &
        &   head = "WriteVTKLineMesh_typSurfMesh" )

    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,4(i8))
    1006 format(i5)

End Subroutine

Subroutine WriteVTKLineMeshGaussPoint_typSurfMesh(this, filePath)
    Implicit None
    class(typSurfMesh), intent(in) :: this
    Character(len=*), intent(in)   :: filePath
    integer :: ioUnit
    integer :: i,j
    Integer :: nConnect

    if ((this%nLineNode.ge.1).and.(this%nLine.ge.1)) then

        Open(newunit = ioUnit,   &
             file    = trim(filePath), &
             status  = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nLine * this%linePanel(1)%nGauss, " float"

        do i = 1, this%nLine
        do j = 1, this%linePanel(1)%nGauss
            write(ioUnit,1003) this%linePanel(i)%gPoint(j)%vec()
        enddo
        enddo

        Close(ioUnit)
    else

        Call GURU%Warn( &
        &   msg  = "Number of Node and Panel should be larger than 1."//" \n"//&
            &"  - Number of Node  : "//Int2Char(this%nNode)//" \n"&
            &"  - Number of Panel : "//Int2Char(this%nPanel), &
        &   head = "WriteVTKLineMeshGaussPoint_typSurfMesh" )

    end if

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,4(i8))
    1006 format(i5)

End Subroutine

Subroutine Copy_typSurfMesh(to, from)
    class(typSurfMesh), intent(inout) :: to
    class(typSurfMesh), intent(in)    :: from

    to%isOpenTop = from%isOpenTop
    to%nNode     = from%nNode
    to%nLineNode = from%nLineNode
    to%nPanel    = from%nPanel
    to%nLine     = from%nLine
    to%nLineSub  = from%nLineSub

    if (to%nNode.gt.0) then
        if (allocated(to%node)) deallocate(to%node)
        allocate(to%node( to%nNode ))
        to%node = from%node
    end if

    if (to%nPanel.gt.0) then
        if (allocated(to%panel)) deallocate(to%panel)
        allocate(to%panel( to%nPanel ))
        to%panel = from%panel
    end if

    if (to%nLineNode.gt.0) then
        if (allocated(to%lineNode)) deallocate(to%lineNode)
        allocate(to%lineNode( to%nLineNode ))
        to%lineNode = from%lineNode
    endif

    if (to%nLine.gt.0) then
        if (allocated(to%linePanel)) deallocate(to%linePanel)
        allocate(to%linePanel( to%nLine ))
        to%linePanel = from%linePanel
    end if

End Subroutine

Subroutine Destroy_typSurfMesh(this)
    Implicit None
    Class(typSurfMesh), intent(inout) :: this
    integer :: i

    ! if (allocated(this%panel)) then
    !     do i = 1, this%nPanel
    !         Call this%panel(i)%destroy()
    !     enddo
    !     deallocate(this%panel)
    ! end if
    !
    ! if (allocated(this%linePanel)) then
    !     do i = 1, this%nPanel
    !         Call this%linePanel(i)%destroy()
    !     enddo
    !     deallocate(this%linePanel)
    ! end if

    this%isOpenTop = .FALSE.
    this%nNode     = 0;     this%nLineNode = 0;     this%nPanel     = 0;
    this%nLine = 0;         this%nLineSub = 0;

End Subroutine
