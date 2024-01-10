program example_gcd
  use stdlib_math, only: gcd
  use stdlib_strings, only: to_string
  use stdlib_optval, only: optval
  use stdlib_kinds, only: sp, int64


  implicit none
  integer :: a, b, c
  a = 48
  b = 18
  c = gcd(a, b) ! returns 6
end program example_gcd

