module mod_scanner
  use M_strings, only: split, len_white, isdigit, isalpha
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
    procedure, pass(self) :: addToken
    procedure, pass(self) :: advanceChar
    procedure, pass(self) :: isAtEnd
    procedure, pass(self) :: match
    procedure, pass(self) :: peek
    procedure, pass(self) :: peekNext
    procedure, pass(self) :: scanToken
    procedure, pass(self) :: scanTokens
    procedure, pass(self) :: tokenizeString
    procedure, pass(self) :: tokenizeNumber
    procedure, pass(self) :: tokenizeIdentifier
  end type Scanner


contains

  subroutine scanTokens(self)
    class(Scanner), intent(inout) :: self
    type(Token(:)),allocatable :: tokens(:)
    character(len=:),allocatable :: words(:)
    integer :: i

    call split(self%source, words)
    allocate(self%tokens(0))

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
    case (';')
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

    case ('/')
      call self%match('/', bmatch)
      if (bmatch) then
        ! Comment until end of line
        do while (self%peek() /= new_line('a') .and. .not. self%isAtEnd())
          call self%advanceChar(c)
        end do
      else
        call self%addToken(TT_SLASH)
      end if

    case (' ', achar(13), achar(9))
      ! Eat whitespace ' ', '\r', '\t'
      continue

    case (new_line('a'))
      self%line = self%line + 1

    case ('"')
      call self%tokenizeString()

    case default
      if (isdigit(c)) then
        call self%tokenizeNumber()
      else if (isalpha(c) .or. c == '_') then
        call self%tokenizeIdentifier()
      else
        call error(self%line, 'Unexpected character: ' // c)
      end if
    end select

  end subroutine scanToken

  subroutine advanceChar(self, c)
    class(Scanner), intent(inout) :: self
    character, intent(out) :: c

    c = self%source(self%current:self%current)
    self%current = self%current + 1
  end subroutine advanceChar

  character function peek(self)
    class(Scanner), intent(inout) :: self

    if (self%isAtEnd()) then
      peek = achar(0)
    end if
    peek = self%source(self%current:self%current)
  end function peek

  character function peekNext(self)
    class(Scanner), intent(inout) :: self

    if (self%current + 1 >= len_white(self%source)) then
      peekNext = achar(0)
    end if
    peekNext = self%source(self%current+1:self%current+1)
  end function peekNext

  logical function isAtEnd(self)
    class(Scanner), intent(in) :: self

    !isAtEnd = ((self%current) > len(self%source))
    isAtEnd = ((self%current) > len_white(self%source))
  end function isAtEnd

  logical function isAlphaNumeric(c)
    character :: c
    isAlphaNumeric = isalpha(c) .or. c == '_' .or. isdigit(c)
  end function isAlphaNumeric

  subroutine addToken(self, tokentype)
    class(Scanner), intent(inout) :: self
    !integer(kind(TokenType)) :: tokentype
    integer :: tokentype
    type(Token), allocatable :: tmptokens(:)
    type(Token) :: newtoken
    integer :: isize

    newtoken = Token(tokentype, self%source(self%start:self%current-1),self%line)

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

  subroutine tokenizeString(self)
    class(Scanner), intent(inout) :: self
    character :: c

    do while(self%peek() /= '"' .and. .not. self%isAtEnd())
      if (self%peek() == new_line('a')) then
        self%line = self%line + 1
      end if
      call self%advanceChar(c)
    end do

    if (self%isAtEnd()) then
      call error(self%line, 'Unterminated string.')
      return
    end if

    call self%advanceChar(c) ! Eat the closing " char
    call self%addToken(TT_STRING)
  end subroutine tokenizeString

  subroutine tokenizeNumber(self)
    class(Scanner), intent(inout) :: self
    character :: c

    do while (isdigit(self%peek()))
      call self%advanceChar(c)
    end do

    if (self%peek() == '.' .and. isdigit(self%peekNext())) then
      call self%advanceChar(c) ! Consume the '.'

      do while (isdigit(self%peek()))
        call self%advanceChar(c)
      end do
    end if

    call self%addToken(TT_NUMBER)
  end subroutine tokenizeNumber

  subroutine tokenizeIdentifier(self)
    class(Scanner), intent(inout) :: self
    character :: c
    integer(kind(TokenType)) :: tt

    do while (isAlphaNumeric(self%peek()))
      call self%advanceChar(c)
    end do

    tt = keywordToken(self%source(self%start:self%current-1))
    if (tt == TT_INVALID_KEYWORD) then
      tt = TT_IDENTIFIER
    end if

    call self%addToken(tt)
  end subroutine tokenizeIdentifier


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
