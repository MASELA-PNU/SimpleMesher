!! -------------------------------------------------------------------------- !!
Program appSimpleMesher
!! -------------------------------------------------------------------------- !!
    use glbSimpleMesher
!! -------------------------------------------------------------------------- !!
Implicit None
!! -------------------------------------------------------------------------- !!

    !!... Initialization
    blkInitialization: Block

        !!... Initialize GURU
        Call InitializeGURU()

        !!... Get Input JSON file path from argument
        Call GURU%GetArgChar( &
        &   key   = "input", &
        &   value = jsonInputFilePath )

        !!... Get Input JSON
        Call JSON_ReadFile( &
        &   filePath = jsonInputFilePath,   &
        &   jsonFile = inputFile,           &
        &   json     = jsonInput            )

        !!... Print if Debug mode is enabled
        If ( GURU%IsDebug() ) Call JSON_Print( jsonInput )

    End Block blkInitialization

    !!... Generate Single Meshes
    GenerateSingleMeshes: Block
        Integer :: iMesh
        Type(typJSON), Pointer :: jsonMesh
        Real(RP), Allocatable :: tmpNodePos(:, :)
        Integer, Allocatable  :: tmpPanelConnect(:, :)
        Integer :: nSubNode, nSubPanel, nPrevNode, nPrevPanel
        Integer :: iPanelNode, nPanelNode
        Integer :: iNode, iPanel, nodeID, panelID

        !!... Get Mesh List
        Call JSON_GetStringVector( jsonInput, "meshList", meshList )
        nMeshList = size( meshList )
        If ( nMeshList.LE.0 ) then
            Call GURU%Error( &
            &   msg  = "The size of 'meshList' is smaller than 1.", &
            &   head = "appSimpleMesher")
        End if

        !!... mesh loop
        nNode  = 0
        nPanel = 0

        do iMesh = 1, nMeshList

            Call GURU%Write( &
            &   msg = "  * Generating mesh :"//meshList(iMesh)%Chars() )
            Call GURU%Write( " " )

            !!... Get Sub JSON Child
            Call JSON_GetChild( jsonInput, meshList(iMesh)%Chars(), jsonMesh )

            !!... Generate Mesh
            Call InitializeSurfMesh( surfMesh = surfMesh, json = jsonMesh )

            !!... Get Node
            nSubNode  = surfMesh%nNode
            nSubPanel = surfMesh%nPanel

            If ( iMesh.EQ.1 ) then

                nNode  = nNode  + nSubNode
                nPanel = nPanel + nSubPanel

                Allocate( nodePos(3, nNode) );          nodePos(:, :) = 0.0_RP
                Allocate( panelConnect(4, nPanel) );    panelConnect(:, :) = 0

                do iNode = 1, nSubNode
                    nodeID = iNode
                    nodePos(:, nodeID) = surfMesh%node(iNode)%Vec()
                end do

                do iPanel = 1, nSubPanel
                    panelID = iPanel
                    nPanelNode = surfMesh%panel(iPanel)%nNode()
                    do iPanelNode = 1, nPanelNode
                        panelConnect(iPanelNode, panelID) &
                        &   = surfMesh%panel(iPanel)%NodeLabel(iPanelNode)
                    End do
                end do

            else

                !!... Move
                Call Move_Alloc( nodePos, tmpNodePos )
                Call Move_Alloc( panelConnect, tmpPanelConnect )

                nPrevNode  = nNode
                nPrevPanel = nPanel

                nNode  = nNode  + nSubNode
                nPanel = nPanel + nSubPanel

                Allocate( nodePos(3, nNode) );           nodePos(:, :) = 0.0_RP
                Allocate( panelConnect(4, nPanel) );     panelConnect(:, :) = 0

                nodePos(:, 1:nPrevNode)       = tmpNodePos(:, :)
                panelConnect(:, 1:nPrevPanel) = tmpPanelConnect(:, :)

                do iNode = 1, nSubNode
                    nodeID = nPrevNode + iNode
                    nodePos(:, nodeID) = surfMesh%node(iNode)%Vec()
                end do

                do iPanel = 1, nSubPanel
                    panelID = nPrevPanel + iPanel
                    nPanelNode = surfMesh%panel(iPanel)%nNode()
                    do iPanelNode = 1, nPanelNode
                        panelConnect(iPanelNode, panelID) &
                        &   = surfMesh%panel(iPanel)%NodeLabel(iPanelNode) &
                        &   + nPrevNode
                    End do
                end do

            end if

            Call surfMesh%Destroy()

        End do

    End Block GenerateSingleMeshes

    Call GURU%Write( "Total Mesh Info" )
    Call GURU%Write( " " )
    Call GURU%Write( "  - Number of Node  : "//Int2Char(nNode) )
    Call GURU%Write( "  - Number of Panel : "//Int2Char(nPanel) )
    Call GURU%Write( " " )

    blkGetherMeshes: Block
        Character(len=:), Allocatable :: outName

        !!... Manual Setting
        Call finalMesh%ManualSet( nodePos, panelConnect )

        !!... Get Out Name
        Call JSON_GetCharOrDefault( jsonInput, "outName", outName, "finalMesh")

        Call GURU%Write( &
        &   msg = "  * Writing VTK File: "//outName//".vtk"//" \n" )
        Call GURU%Write( " " )

        !!... Write VTK Format
        Call finalMesh%WriteVTK( outName )

        Call GURU%Write( &
        &   msg = "  * Writing HydroStar File: "//outName//".hst" )
        Call GURU%Write( " " )

        !!... Write HydroStar Mesh File
        Call finalMesh%WriteHydroStarMesh( outName )

    End Block blkGetherMeshes

        Call GURU%Write( "Done." )
        Call GURU%Write( " " )


!! -------------------------------------------------------------------------- !!
End Program
!! -------------------------------------------------------------------------- !!
