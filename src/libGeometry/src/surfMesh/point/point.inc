!! ------------------------------------------------------------------ !!
!! Procedures : Type Point
!! ------------------------------------------------------------------ !!

    Subroutine Set_typPoint(this, pidx, x, y, z)
        Implicit None
        class(typPoint), intent(inout) :: this
        Integer, intent(in)            :: pidx
        Real(RP),intent(in)            :: x, y, z
        this%idx_ = pidx
        this%vec_(1) = x;    this%vec_(2) = y;    this%vec_(3) = z
    End Subroutine

    Subroutine Move_typPoint(this, x, y, z)
        Implicit None
        class(typPoint), intent(inout) :: this
        Real(RP),intent(in)            :: x, y, z
        this%vec_(1) = x;    this%vec_(2) = y;    this%vec_(3) = z
    End Subroutine

    Function X_typPoint(this) result( x )
        Implicit None
        class(typPoint), intent(in) :: this
        Real(RP) :: x
        x = this%vec_(1)
    End Function

    Function Y_typPoint(this) result( y )
        Implicit None
        class(typPoint), intent(in) :: this
        Real(RP) :: y
        y = this%vec_(2)
    End Function

    Function Z_typPoint(this) result( z )
        Implicit None
        class(typPoint), intent(in) :: this
        Real(RP) :: z
        z = this%vec_(3)
    End Function

    Function Vec_typPoint(this) result(vec)
        Implicit None
        class(typPoint), intent(in) :: this
        Real(RP), Dimension(3) :: vec
        vec = this%vec_
    End Function

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
