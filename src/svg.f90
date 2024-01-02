module svg

  use stdlib_strings, only: to_string
  use stdlib_optval, only: optval
  use stdlib_kinds, only: sp

  implicit none
  private

  integer, parameter, public :: CHT_WIDTH = 100 ! does not include margins left and right
  integer, parameter, public :: CHT_HEIGHT = 75 ! does not include margins top and bot
  integer, parameter, public :: CHT_MARGIN_TOP = 25
  integer, parameter, public :: CHT_MARGIN_BOT= 25
  integer, parameter, public :: CHT_MARGIN_LEFT = 25
  integer, parameter, public :: CHT_MARGIN_RIGHT = 25

  type, public :: t_svg
    integer :: width, height, margin_top, margin_bot, margin_left, margin_right
    character(len=:), allocatable :: xml
    contains
      procedure head
      procedure defs
      procedure tail
      procedure export
      procedure clean
      procedure circle
      procedure rect
      procedure add_text
      procedure scatter
  end type

  contains

    subroutine head(self, width, height, margin_top, margin_bot, margin_left, margin_right)
      class(t_svg) self
      integer, optional :: width, height, margin_top, margin_bot, margin_left, margin_right
      self%margin_top = optval(margin_top, CHT_MARGIN_TOP)
      self%margin_bot = optval(margin_bot, CHT_MARGIN_BOT)
      self%margin_left = optval(margin_left, CHT_MARGIN_LEFT)
      self%margin_right = optval(margin_right, CHT_MARGIN_RIGHT)
      self%width = optval(width, self%margin_left + CHT_WIDTH + self%margin_right)
      self%height = optval(height, self%margin_top + CHT_HEIGHT + self%margin_bot)
      self%xml = '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" viewBox="0 0 ' // &
      & to_string(self%width) // ' ' // to_string(self%height) // '">' // new_line('a')
    end subroutine head

    ! FIXME - adjust size of these
    ! defines marker dot
    subroutine defs(self)
      class(t_svg) self
      self%xml = self%xml // &
      & '<defs>' // new_line('a') // &
      & '<marker id="dot"viewBox="0 0 2 2" refX="1"refY="1" markerWidth="1" markerHeight="1">' // new_line('a') // &
      & '<circle cx="1" cy="1" r="1" fill="black" />' // new_line('a') // &
      & '</marker>' // new_line('a') // &
      & '<pattern id="smallGrid" width="10" height="10" patternUnits="userSpaceOnUse">' // new_line('a') // &
      & '<path d="M 10 0 L 0 0 0 10" fill="none" stroke="lightgrey" stroke-width="0.5"/>' // new_line('a') // &
      & '</pattern>' // new_line('a') // &
      & '<pattern id="grid" width="100" height="100" patternUnits="userSpaceOnUse">' // new_line('a') // &
      & '<rect width="100" height="100" fill="url(#smallGrid)"/>' // new_line('a') // &
      & '<path d="M 100 0 L 0 0 0 100" fill="none" stroke="lightblue" stroke-width="1"/>' // new_line('a') // &
      & '</pattern>' // new_line('a') // &
      & '</defs>' // new_line('a')
    end subroutine defs

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

    subroutine circle(self, cx, cy, r, stroke_in, stroke_width_in, fill_in)
      class(t_svg) self
      real(kind=sp) cx, cy, r
      real(kind=sp), optional :: stroke_width_in
      real(kind=sp) stroke_width
      character(len=*), optional :: stroke_in, fill_in
      character(len=:), allocatable :: stroke, fill
      stroke_width = 1.0 ! make global?
      stroke_width = optval(stroke_width_in, stroke_width)
      stroke = "black"
      stroke = optval(stroke_in, stroke)
      fill = "none"
      fill = optval(fill_in, fill)
      self%xml = self%xml // '<circle cx="' // to_string(cx) // '" cy="' // to_string(cy) // '" r="' // to_string(r) // &
      & '" stroke="' // stroke // '" stroke-width="' // to_string(stroke_width) // '" fill="' // fill // '"/>' // new_line('a')
    end subroutine circle

    subroutine rect(self, x, y, width, height, stroke_in, stroke_width_in, fill_in)
      class(t_svg) self
      integer, optional :: x, y, width, height
      real(kind=sp), optional :: stroke_width_in
      real(kind=sp) stroke_width
      character(len=*), optional :: stroke_in, fill_in
      character(len=:), allocatable :: stroke, fill
      x = optval(x, self%margin_left)
      y = optval(y, self%height - self%

      stroke_width = 1.0 ! make global?
      stroke_width = optval(stroke_width_in, stroke_width)
      stroke = "black"
      stroke = optval(stroke_in, stroke)
      fill = "none"
      fill = optval(fill_in, fill)
      self%xml = self%xml // '<rect x="' // to_string(x) // '" y="' // to_string(y) // &
      & '" width="' // to_string(width) // '" height="' // to_string(height) // &
      & '" stroke="' // stroke // '" stroke-width="' // to_string(stroke_width) // '" fill="' // fill // '"/>' // new_line('a')
    end subroutine rect

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


  ! <!-- Data line with polymarkers -->
  ! <polyline
  !   points="15,80 29,50 43,60 57,30 71,40 85,15"
  !   fill="none"
  !   stroke="grey"
  !   marker-start="url(#dot)"
  !   marker-mid="url(#dot)"
  !   marker-end="url(#dot)" />



end module svg
