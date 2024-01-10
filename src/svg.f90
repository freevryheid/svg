module svg

  use stdlib_math, only :: gcd
  ! use stdlib_math
  use stdlib_strings, only: to_string
  use stdlib_optval, only: optval
  use stdlib_kinds, only: sp, int64

  implicit none
  private

  real(kind=sp), parameter, public :: CHT_WIDTH = 100.0      ! does not include LEFT and RIGHT MARGINS
  real(kind=sp), parameter, public :: CHT_HEIGHT = 75.0      ! does not include TOP and BOT MARGINS
  real(kind=sp), parameter, public :: CHT_MARGIN_SIDE = 25.0 ! LEFT and RIGHT MARGINS
  real(kind=sp), parameter, public :: CHT_MARGIN_FACE = 25.0 ! TOP and BOT MARGINS

  type, public :: t_svg
    real(kind=sp) width, height, margin_side, margin_face, xscale, yscale, xmin, ymin, xmax, ymax, xgcd, ygcd
    character(len=:), allocatable :: xml
    contains
      procedure init ! initialize
      procedure marker_defs
      procedure grid_defs
      procedure term ! terminate
      procedure export
      procedure clean
      procedure circle
      procedure rect
      procedure add_text
      procedure scatter
      procedure grid
      procedure group
      procedure ungroup
  end type

  contains

    subroutine init(self, width, height, margin_face, margin_side)
      class(t_svg) self
      real(kind=sp), optional :: width, height, margin_face, margin_side
      self%margin_face = optval(margin_face, CHT_MARGIN_FACE)
      self%margin_side = optval(margin_side, CHT_MARGIN_SIDE)
      self%width = optval(width, CHT_WIDTH)
      self%height = optval(height, CHT_HEIGHT)
      self%xml = '<svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" viewBox="0 0 ' // &
      & to_string(int(self%margin_side + self%width + self%margin_side)) // ' ' // &
      & to_string(int(self%margin_face + self%height + self%margin_face)) // '">' // new_line('a')
    end subroutine init

    subroutine marker_defs(self, size_opt)
      class(t_svg) self
      real(kind=sp), optional :: size_opt
      real(kind=sp) msize
      real(kind=sp) xgcd, ygcd
     msize = 1.0
      msize = optval(size_opt, msize)
      ! print *, xgcd, ygcd
      self%xml = self%xml // &
      & '<defs>' // new_line('a') // &
      & '<marker id="dot" viewBox="0 0 1 1" refX="0.5" refY="0.5" ' // &
      & 'markerWidth="' // to_string(msize) // '" markerHeight="' // to_string(msize) // '">' // new_line('a') // &
      & '<circle cx="0.5" cy="0.5" r="0.5" fill="black" />' // new_line('a') // &
      & '</marker>' // new_line('a') // &
      & '</defs>' // new_line('a')
    end subroutine marker_defs

    subroutine grid_defs(self)
      class(t_svg) self
      self%xml = self%xml // &
      & '<defs>' // new_line('a') // &
      ! & '<pattern id="smallGrid" width="10" height="10" patternUnits="userSpaceOnUse">' // new_line('a') // &
      ! & '<path d="M 10 0 L 0 0 0 10" fill="none" stroke="lightgrey" stroke-width="0.5"/>' // new_line('a') // &
      ! & '</pattern>' // new_line('a') // &
      & '<pattern id="grid" width="' // to_string(self%xscale*self%xgcd) // '" height="' // to_string(self%yscale*self%ygcd) // &
      & '" patternUnits="userSpaceOnUse">' // new_line('a') // &
      ! & '<!--rect width="100" height="100" fill="url(#smallGrid)"/-->' // new_line('a') // &
      ! & '<path d="M 100 0 L 0 0 0 100" fill="none" stroke="lightgrey" stroke-width="1"/>' // new_line('a') // &
      & '<path d="M ' // to_string(self%xscale*self%xgcd) // ' 0 L 0 0 0 ' // to_string(self%yscale*self%ygcd) // &
      & '" fill="none" stroke="lightgrey" stroke-width="0.1"/>' // new_line('a') // &
      & '</pattern>' // new_line('a') // &
      & '</defs>' // new_line('a')
    end subroutine grid_defs

    subroutine term(self)
      class(t_svg) self
      self%xml = self%xml // '</svg>'
    end subroutine term

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
      real(kind=sp), optional :: x_opt, y_opt, width_opt, height_opt
      real(kind=sp) x, y, width, height
      real(kind=sp), optional :: stroke_width_opt
      real(kind=sp) stroke_width
      character(len=*), optional :: stroke_opt, fill_opt
      character(len=:), allocatable :: stroke, fill
      x = optval(x_opt, self%margin_side)
      y = optval(y_opt, self%margin_face)
      width = optval(width_opt, self%width)
      height = optval(height_opt, self%height)
      stroke_width = 0.1 ! make global?
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
      self%xml = self%xml // '<polyline transform="translate(' // to_string(self%margin_side) // ', ' // &
      & to_string(self%margin_face) // ')"' // new_line('a') // &
      & 'points="'
      fin = ""
      do i = 1,size(a,1)
        write(nbr, '(f16.4)') self%xscale*(a(i, 1) - self%xmin)
        fin = fin // trim(adjustl(nbr)) // ","
        write(nbr, '(f16.4)') self%yscale*(a(i, 2) - self%ymin)
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
      self%xml = self%xml // '/>' // new_line('a')
    end subroutine polyline

    subroutine grid(self)
      class(t_svg) self
      self%xml = self%xml // '<rect x="' // to_string(self%margin_side) // '" y="' // to_string(self%margin_face) // &
      & '" width="' // to_string(self%width) // '" height="' // to_string(self%height) // '" fill="url(#grid)"/>' // new_line('a')
    end subroutine grid

    subroutine group(self)
      class(t_svg) self
      self%xml = self%xml // '<g transform="scale(1,-1)" transform-origin="center">' // new_line('a')
    end subroutine group

    subroutine ungroup(self)
      class(t_svg) self
      self%xml = self%xml // '</g>' // new_line('a')
    end subroutine ungroup

    subroutine scatter(self, a, minx_opt, maxx_opt, miny_opt, maxy_opt, grid_opt, reg_opt)
      class(t_svg) self
      real(kind=sp) a(:,:)
      integer rows, cols
      real(kind=sp) minx, maxx, miny, maxy
      logical grid1, reg
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
      grid1 = optval(grid_opt, .false.)
      reg = optval(reg_opt, .false.)

      call init(self)

      self%xmin = minx
      self%ymin = miny
      self%xmax = maxx
      self%ymax = maxy

      self%xscale =  self%width / (maxx - minx)
      self%yscale =  self%height / (maxy - miny)

      self%xgcd = real(gcd(int(minx, kind=int64), int(maxx, kind=int64)), kind=sp)
      self%ygcd = real(gcd(int(miny, kind=int64), int(maxy, kind=int64)), kind=sp)

      call group(self)
      call marker_defs(self)
      call grid_defs(self)
      call rect(self)
      call grid(self)
      call polyline(self, a)
      call ungroup(self)
      ! call term(self)
    end subroutine scatter

end module svg



! translate(<minX+maxX>,0) scale(-1, 1)   // for flip X
! translate(0,<minY+maxY>) scale(1, -1)   // for flip Y
