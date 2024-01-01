module svg

  use stdlib_strings, only: to_string
  use stdlib_optval, only: optval

  implicit none
  private

  integer, parameter, public :: SVG_WIDTH = 100
  integer, parameter, public :: SVG_HEIGHT = 100

  type, public :: t_svg
    integer :: width, height
    character(len=:), allocatable :: xml
    contains
      procedure head
      procedure tail
      procedure export
      procedure clean
      procedure add_text
      procedure scatter
  end type

  contains

    subroutine head(self, width, height)
      class(t_svg) self
      integer, optional :: width, height
      self%width = optval(width, SVG_WIDTH)
      self%height = optval(height, SVG_HEIGHT)
      self%xml = '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" viewBox="0 0 ' // &
      & to_string(self%width) // ' ' // to_string(self%height) // '">' // new_line('a')
    end subroutine head

    subroutine tail(self)
      class(t_svg) self
      self%xml = self%xml // '</svg>'
    end subroutine tail

    subroutine export(self, fin)
      class(t_svg) self
      character(len=*), optional :: fin
      character(len=:), allocatable :: fout
      integer io
      fout = "tst.svg"
      fout = optval(fin, fout)
      open(newunit=io, file=fout, status="replace", action="write")
      write(io, '(a)') self%xml
      close(io)
    end subroutine export

    subroutine clean(self)
      class(t_svg) self
      deallocate(self%xml)
    end subroutine clean

    subroutine add_text(self, x, y, fs, fill, txt)
      class(t_svg) :: self
      integer :: x, y, fs
      character(len=*) :: fill, txt
      self%xml = self%xml // '<text x="' // to_string(x) // '" y="' // to_string(y) // &
      & '" font-size="' // to_string(fs) // '" fill="' // fill // '">' // &
      & txt // '</text>' // new_line('(a)')
    end subroutine add_text

    subroutine scatter(self, a)
      class(t_svg) self
      integer a(:,:)
      integer rows, cols
      integer minx, maxx, miny, maxy
      integer mins(2), maxs(2)

      rows = size(a, 1)
      cols = size(a, 2)
      mins = minval(a, 1)
      maxs = maxval(a, 1)
      minx = mins(1)
      miny = mins(2)
      maxx = maxs(1)
      maxy = maxs(2)

      print *, minx, maxx, miny, maxy
    end subroutine scatter

end module svg
