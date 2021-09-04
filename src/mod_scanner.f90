module mod_scanner
  use M_strings, only: split
  !use functional, only: map

  implicit none

  type :: Token
     character(32) :: token
     !character(len=:),allocatable :: token
   contains
     procedure write_token
     generic :: write(unformatted) => write_token
  end type Token

  type :: Scanner
     character(512) :: source
     type(Token), allocatable :: tokens(:)

   contains
     procedure, pass(self) :: scanTokens
  end type Scanner

contains

  subroutine write_token(dtv, unit, iostat, iomsg)
    class(token), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write(unit, iostat=iostat, iomsg=iomsg) dtv%token
  end subroutine write_token

  subroutine scanTokens(self)
    class(Scanner), intent(inout) :: self
    type(Token(:)),allocatable :: tokens(:)
    character(len=:),allocatable :: words(:)
    integer :: i

    call split(self%source, words)
    self%tokens = [(Token(words(i)), i=1, size(words))]
    !print *, 'DEBUG: tokens within scanTokens(): ', self%tokens

  end subroutine scanTokens


end module mod_scanner
