module svg

  use stdlib_strings, only: to_string
  use stdlib_optval, only: optval
  use stdlib_kinds, only: sp

  implicit none
  private

  real(kind=sp), parameter, public :: CHT_WIDTH = 100 ! does not include margins left and right
  real(kind=sp), parameter, public :: CHT_HEIGHT = 75 ! does not include margins top and bot
  real(kind=sp), parameter, public :: CHT_MARGIN_TOP = 25
  real(kind=sp), parameter, public :: CHT_MARGIN_BOT= 25
  real(kind=sp), parameter, public :: CHT_MARGIN_LEFT = 25
  real(kind=sp), parameter, public :: CHT_MARGIN_RIGHT = 25

  type, public :: t_svg
    integer :: width, height, margin_top, margin_bot, margin_left, margin_right
    character(len=:), allocatable :: xml
    contains
      procedure head
      procedure marker_defs
      procedure grid_defs
      procedure tail
      procedure export
      procedure clean
      procedure circle
      procedure rect
      procedure add_text
      procedure scatter
      procedure grid
  end type

  contains

    subroutine head(self, width, height, margin_top, margin_bot, margin_left, margin_right)
      class(t_svg) self
      real(kind=sp), optional :: width, height, margin_top, margin_bot, margin_left, margin_right
      self%margin_top = optval(margin_top, CHT_MARGIN_TOP)
      self%margin_bot = optval(margin_bot, CHT_MARGIN_BOT)
      self%margin_left = optval(margin_left, CHT_MARGIN_LEFT)
      self%margin_right = optval(margin_right, CHT_MARGIN_RIGHT)
      self%width = optval(width, CHT_WIDTH)
      self%height = optval(height, CHT_HEIGHT)
      self%xml = '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" viewBox="0 0 ' // &
      & to_string(self%margin_left + self%width + self%margin_right) // ' ' // &
      & to_string(self%margin_top + self%height + self%margin_bot) // '">' // new_line('a')
    end subroutine head

    subroutine marker_defs(self)
      class(t_svg) self
      self%xml = self%xml // &
      & '<defs>' // new_line('a') // &
      & '<marker id="dot" viewBox="0 0 2 2" refX="1" refY="1" markerWidth="1" markerHeight="1">' // new_line('a') // &
      & '<circle cx="1" cy="1" r="1" fill="black" />' // new_line('a') // &
      & '</marker>' // new_line('a') // &
      & '</defs>' // new_line('a')
    end subroutine marker_defs

    subroutine grid_defs(self)
      class(t_svg) self
      self%xml = self%xml // &
      & '<defs>' // new_line('a') // &
      & '<pattern id="smallGrid" width="10" height="10" patternUnits="userSpaceOnUse">' // new_line('a') // &
      & '<path d="M 10 0 L 0 0 0 10" fill="none" stroke="lightgrey" stroke-width="0.5"/>' // new_line('a') // &
      & '</pattern>' // new_line('a') // &
      & '<pattern id="grid" width="100" height="100" patternUnits="userSpaceOnUse">' // new_line('a') // &
      & '<rect width="100" height="100" fill="url(#smallGrid)"/>' // new_line('a') // &
      & '<path d="M 100 0 L 0 0 0 100" fill="none" stroke="lightgrey" stroke-width="1"/>' // new_line('a') // &
      & '</pattern>' // new_line('a') // &
      & '</defs>' // new_line('a')
    end subroutine grid_defs

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

    subroutine circle(self, cx, cy, r, stroke_opt, stroke_width_opt, fill_opt)
      class(t_svg) self
      real(kind=sp) cx, cy, r
      real(kind=sp), optional :: stroke_width_opt
      real(kind=sp) stroke_width
      character(len=*), optional :: stroke_opt, fill_opt
      character(len=:), allocatable :: stroke, fill
      stroke_width = 1.0 ! make global?
      stroke_width = optval(stroke_width_opt, stroke_width)
      stroke = "black"
      stroke = optval(stroke_opt, stroke)
      fill = "none"
      fill = optval(fill_opt, fill)
      self%xml = self%xml // '<circle cx="' // to_string(cx) // '" cy="' // to_string(cy) // '" r="' // to_string(r) // &
      & '" stroke="' // stroke // '" stroke-width="' // to_string(stroke_width) // '" fill="' // fill // '"/>' // new_line('a')
    end subroutine circle

    subroutine rect(self, x_opt, y_opt, width_opt, height_opt, stroke_opt, stroke_width_opt, fill_opt)
      class(t_svg) self
      integer, optional :: x_opt, y_opt, width_opt, height_opt
      integer x, y, width, height
      real(kind=sp), optional :: stroke_width_opt
      real(kind=sp) stroke_width
      character(len=*), optional :: stroke_opt, fill_opt
      character(len=:), allocatable :: stroke, fill
      x = optval(x_opt, self%margin_left)
      y = optval(y_opt, self%margin_top)
      width = optval(width_opt, self%width)
      height = optval(height_opt, self%height)
      stroke_width = 0.5 ! make global?
      stroke_width = optval(stroke_width_opt, stroke_width)
      stroke = "black"
      stroke = optval(stroke_opt, stroke)
      fill = "none"
      fill = optval(fill_opt, fill)
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


    subroutine polyline(self, a, fill_opt, stroke_opt, marker_opt)
      class(t_svg) self
      real(kind=sp) a(:,:)
      character(len=*), optional :: fill_opt, stroke_opt
      logical, optional :: marker_opt
      character(len=:), allocatable :: fill, stroke
      logical marker
      integer i
      character(len=16) :: nbr
      character(len=:), allocatable :: fin
      fill = optval(fill_opt, "none")
      stroke = optval(stroke_opt, "none")
      marker = optval(marker_opt, .true.)
      self%xml = self%xml // '<polyline>' // new_line('a') //'points="'
      fin = ""
      do i = 1,size(a,1)
        write(nbr, '(f16.4)') self%margin_left + a(i,1)
        fin = fin // trim(adjustl(nbr)) // ","
        write(nbr, '(f16.4)') self%margin_top + self%height - a(i,2)
        fin = fin // trim(adjustl(nbr)) // " "
      end do
      self%xml = self%xml // fin // '"' // new_line('a')
      self%xml = self%xml // 'fill="' // fill // '"' // new_line('a')
      self%xml = self%xml // 'stroke="' // stroke // '"' // new_line('a')
      if (marker) then
        self%xml = self%xml // 'marker-start="url(#dot)"' // new_line('a')
        self%xml = self%xml // 'marker-mid="url(#dot)"' // new_line('a')
        self%xml = self%xml // 'marker-end="url(#dot)"' // new_line('a')
      end if
      self%xml = self%xml // '</polyline>' // new_line('a')
    end subroutine polyline

    subroutine scatter(self, a, minx_opt, maxx_opt, miny_opt, maxy_opt, grid_opt, reg_opt)
      class(t_svg) self
      real(kind=sp) a(:,:)
      integer rows, cols
      real(kind=sp) minx, maxx, miny, maxy
      logical grid, reg
      real(kind=sp), optional :: minx_opt, maxx_opt, miny_opt, maxy_opt
      logical, optional :: grid_opt, reg_opt
      real(kind=sp) mins(2), maxs(2)

      rows = size(a, 1)
      cols = size(a, 2)
      mins = minval(a, 1)
      maxs = maxval(a, 1)
      minx = real(floor(mins(1)), kind=sp)
      miny = real(floor(mins(2)), kind=sp)
      maxx = real(ceiling(maxs(1)), kind=sp)
      maxy = real(ceiling(maxs(2)), kind=sp)

      minx = optval(minx_opt, minx)
      maxx = optval(maxx_opt, maxx)
      miny = optval(miny_opt, miny)
      maxy = optval(maxy_opt, maxy)
      grid = optval(grid_opt, .false.)
      reg = optval(reg_opt, .false.)

      call head(self, maxx-minx, maxy-miny)
      call marker_defs(self)
      call polyline(self, a)
      ! call tail(self)
    end subroutine scatter

    subroutine grid(self)
      class(t_svg) self
      self%xml = self%xml // '<rect width="100%" height="100%" fill="url(#grid)"/>' // new_line('a')
    end subroutine grid



  ! <!-- Data line with polymarkers -->
  ! <polyline
  !   points="15,80 29,50 43,60 57,30 71,40 85,15"
  !   fill="none"
  !   stroke="grey"
  !   marker-start="url(#dot)"
  !   marker-mid="url(#dot)"
  !   marker-end="url(#dot)" />



end module svg
