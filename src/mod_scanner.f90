module mod_scanner
  use M_strings, only: split
  !use functional, only: map
  use mod_tokentype
  use mod_token, only: Token

  implicit none
  private

  public Scanner, scanTokens

  type :: Scanner
    character(512) :: source ! TODO use iso_varying_string
    type(Token), allocatable :: tokens(:)
    integer :: start=1, current=1, line=1

  contains
    procedure, pass(self) :: scanTokens
    procedure, pass(self) :: scanToken
    procedure, pass(self) :: isAtEnd
    procedure, pass(self) :: advanceChar
  end type Scanner


contains

  subroutine scanTokens(self)
    class(Scanner), intent(inout) :: self
    type(Token(:)),allocatable :: tokens(:)
    character(len=:),allocatable :: words(:)
    integer :: i

    call split(self%source, words)
    !self%tokens = [(Token(words(i)), i=1, size(words))]
    !print *, 'DEBUG: tokens within scanTokens(): ', self%tokens

    self%tokens = [(Token(TT_EOF, words(i), 1), i=1, size(words))]
    do while (.not. (self%isAtEnd()))
      self%start = self%current
      call self%scanToken()
    end do

  end subroutine scanTokens

  subroutine scanToken(self)
    class(Scanner), intent(inout) :: self
      character :: c

      ! AA TODO:  http://craftinginterpreters.com/scanning.html#recognizing-lexemes
      call self%advanceChar(c)

  end subroutine scanToken

  subroutine advanceChar(self, c)
    class(Scanner), intent(inout) :: self
      character, intent(out) :: c
      character(512) :: s
      integer :: i

      s = self%source
      i = self%current
      c = s(i:i)

      self%current = self%current + 1

  end subroutine advanceChar

  logical function isAtEnd(self)
    class(Scanner), intent(in) :: self
    character(:), allocatable :: s(:)
    s = self%source

    isAtEnd = (self%current) > size(s)
  end function isAtEnd

end module mod_scanner
