module mod_scanner
  implicit none

  type :: Scanner
     character(512) :: source
   contains
     procedure, pass(self) :: scanTokens
  end type Scanner

  type :: Token
     character(64) :: token
   contains
     ! AA TODO
  end type Token

contains

  subroutine scanTokens(self, tokens)
    class(Scanner), intent(in) :: self
    class(Token), intent(out) :: tokens(:)

    write(*,'(a)') (self % source)

  end subroutine scanTokens


end module mod_scanner
