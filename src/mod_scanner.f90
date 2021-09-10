module mod_scanner
  use M_strings, only: split, len_white
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
    procedure, pass(self) :: addToken
    procedure, pass(self) :: match
  end type Scanner


contains

  subroutine scanTokens(self)
    class(Scanner), intent(inout) :: self
    type(Token(:)),allocatable :: tokens(:)
    character(len=:),allocatable :: words(:)
    integer :: i

    call split(self%source, words)
    !self%tokens = [(Token(words(i)), i=1, size(words))]

    self%tokens = [(Token(TT_EOF, words(i), 1), i=1, size(words))] ! TODO

    do while (.not. (self%isAtEnd()))
      self % start = self % current
      call self%scanToken()
    end do

  end subroutine scanTokens

  subroutine scanToken(self)
    class(Scanner), intent(inout) :: self
    character :: c
    logical :: bmatch = .false.

    call self%advanceChar(c)

    !print *, c
    select case (c)
    case ('(')
      call self%addToken(TT_LEFT_PAREN)
    case (')')
      call self%addToken(TT_RIGHT_PAREN)
    case ('{')
      call self%addToken(TT_LEFT_BRACE)
    case ('}')
      call self%addToken(TT_RIGHT_BRACE)
    case (',')
      call self%addToken(TT_COMMA)
    case ('.')
      call self%addToken(TT_DOT)
    case ('-')
      call self%addToken(TT_MINUS)
    case ('+')
      call self%addToken(TT_PLUS)
    case ('')
      call self%addToken(TT_SEMICOLON)
    case ('*')
      call self%addToken(TT_STAR)

    case ('!')
      call self%match('=', bmatch)
      if (bmatch) then
        call self%addToken(TT_BANG_EQUAL)
      else
        call self%addToken(TT_BANG)
      end if

    case ('=')
      call self%match('=', bmatch)
      if (bmatch) then
        call self%addToken(TT_EQUAL_EQUAL)
      else
        call self%addToken(TT_EQUAL)
      end if

    case ('<')
      call self%match('=', bmatch)
      if (bmatch) then
        call self%addToken(TT_LESS_EQUAL)
      else
        call self%addToken(TT_LESS)
      end if

    case ('>')
      call self%match('=', bmatch)
      if (bmatch) then
        call self%addToken(TT_GREATER_EQUAL)
      else
        call self%addToken(TT_GREATER)
      end if


    case default
      call error(self%line, 'Unexpected character.')
    end select

  end subroutine scanToken

  subroutine advanceChar(self, c)
    class(Scanner), intent(inout) :: self
    character, intent(out) :: c
    character(512) :: s
    integer :: i

    s = self%source
    i = self%current
    c = s(i:i)

    self % current = i + 1

  end subroutine advanceChar

  logical function isAtEnd(self)
    class(Scanner), intent(in) :: self

    !isAtEnd = ((self%current) > len(self%source))
    isAtEnd = ((self%current) > len_white(self%source))
  end function isAtEnd

  subroutine addToken(self, tokentype)
    class(Scanner), intent(inout) :: self
    !integer(kind(TokenType)) :: tokentype
    integer :: tokentype
    type(Token), allocatable :: tmptokens(:)
    type(Token) :: newtoken
    integer :: isize

    newtoken = Token(tokentype, self%source(self%start:self%current),self%line)

    if (allocated(self%tokens)) then
      isize = size(self%tokens)
      allocate(tmptokens(isize+1))
      tmptokens(1:isize) = self%tokens(1:isize)
      tmptokens(isize+1) = newtoken

      deallocate(self%tokens)
      call move_alloc(tmptokens, self%tokens)
    else
      allocate(self%tokens(1))
      self%tokens(1) = newtoken
    end if

  end subroutine addToken

  subroutine match(self, c, bmatch)
    class(Scanner), intent(inout) :: self
    character, intent(in) :: c
    logical, intent(out) :: bmatch
    bmatch = .false.

    if (self%isAtEnd()) then
      return
    end if
    if (c /= self%source(self%current:self%current)) then
      return
    end if

    self%current = self%current + 1
    bmatch = .true.
  end subroutine match

  subroutine error (line, message)
    !According to the book this is in the Lox class instead of Scanner
    integer :: line
    character(*) :: message

    call report(line, '', message)
  end subroutine error

  subroutine report (line, s_where, message)
    !According to the book this is in the Lox class instead of Scanner
    integer :: line
    character(*) :: s_where
    character(*) :: message

    print *, '[line ', line, '] Error', s_where, ': ', message
  end subroutine report

end module mod_scanner
