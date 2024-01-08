program check
  use stdlib_kinds, only: sp
  use svg
  implicit none
  type(t_svg) s
  integer i
  real(kind=sp) a(10, 2)
  block
    do i = 1, 10
      a(i, 1) = real(i, kind=sp)
      a(i, 2) = real(i*i, kind=sp)
    end do
    ! call s%head()
    ! call s%defs()
    ! call s%rect()
    ! call s%grid()
    ! call s%circle(10.,10.,.5)
    call s%scatter(a)
    ! call s%add_text(50, 50, 10, "red", "Hello, World!")
    ! call s%tail()
    call s%export()
    call s%clean()
    ! print *, s%xml
  end block
end program check
