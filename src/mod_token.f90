module mod_token
  use mod_tokentype
  use M_strings, only: crop

  implicit none

  type :: Token
    integer(kind(TokenType)) :: token
    character(32) :: lexeme ! TODO: use iso_varying_string
    !character(len=:),allocatable :: lexeme

    ! Object :: literal ! Can we do something similar to this in Fortran?
    ! Investigate: https://www.fortran90.org/src/best-practices.html#type-casting-in-callbacks

    integer :: line

  contains
    procedure write_token
    !generic :: write(unformatted) => write_token
    generic :: write(formatted) => write_token
  end type Token

contains

  subroutine write_token(dtv, unit, iotype, v_list, iostat, iomsg)
    class(token), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !write(unit, iostat=iostat, iomsg=iomsg) dtv%token
    !write(unit, iostat=iostat, iomsg=iomsg, fmt='(a)') 'Token[' // tt_tostring(dtv%token) // ']'

    write(unit, iostat=iostat, iomsg=iomsg, fmt='(a)', advance="no") 'Token[' // tt_tostring(dtv%token) // ','
    !write(unit, iostat=iostat, iomsg=iomsg, fmt='(a)', advance="no") '"' // dtv%lexeme // '",'
    write(unit, iostat=iostat, iomsg=iomsg, fmt='(a)', advance="no") crop(dtv%lexeme) // ','
    write(unit, iostat=iostat, iomsg=iomsg, fmt='(a)', advance="no") 'line '
    write(unit, iostat=iostat, iomsg=iomsg, fmt='(i4)', advance="no") dtv%line
    write(unit, iostat=iostat, iomsg=iomsg, fmt='(a)') ']'
  end subroutine write_token

end module mod_token
