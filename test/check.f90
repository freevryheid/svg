program check
  use svg
  implicit none
  type(t_svg) s
  integer i
  integer a(10, 2)
  block
    do i = 1, 10
      a(i, 1) = i
      a(i, 2) = i*i
    end do
    call s%head()
    call s%defs()
    ! call s%rect(
    ! call s%circle(10.,10.,.5)
    ! call s%scatter(a)
    ! call s%add_text(50, 50, 10, "red", "Hello, World!")
    call s%tail()
    call s%export()
    call s%clean()
    ! print *, s%xml
  end block
end program check
