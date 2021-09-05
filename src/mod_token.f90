module mod_token
  use mod_tokentype

  implicit none

  type :: Token
    integer(kind(TokenType)) :: token
    character(32) :: lexeme
    !character(len=:),allocatable :: lexeme

    ! Object :: literal ! Can we do something similar to this in Fortran?
    ! Investigate: https://www.fortran90.org/src/best-practices.html#type-casting-in-callbacks

    integer :: line

  contains
    procedure write_token
    generic :: write(unformatted) => write_token
  end type Token

contains

  subroutine write_token(dtv, unit, iostat, iomsg)
    class(token), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write(unit, iostat=iostat, iomsg=iomsg) dtv%token
  end subroutine write_token

end module mod_token
