!! -------------------------------------------------------------------------- !!
Subroutine InitializeGURU()
!! -------------------------------------------------------------------------- !!
    Implicit None
!! -------------------------------------------------------------------------- !!

    Call GURU%Initialize( &
    &   projectName = "appSimpleMesher", &
    &   description = "Generating Simple Mesh", &
    &   authors     = "Young-Myung CHOI\n \n&
                    &  Dept. Naval Architecture and Ocean Engineering,\n&
                    &  Pusan National University, Republic of Korea.",  &
    &   date        = "23 March 2022",   &
    &   version     = "0.0.1"               )

    !!... Add Arguments
    Call GURU%AddKey( &
    &   key           = "input",                &
    &   singleKey     = "i",                    &
    &   description   = "Input JSON File Path", &
    &   nRequiredWord = 1                       )

    !!... Print & Print out
    Call GURU%UpdateAndPrint()

!! -------------------------------------------------------------------------- !!
End Subroutine
!! -------------------------------------------------------------------------- !!
