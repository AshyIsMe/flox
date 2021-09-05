module mod_tokentype
  implicit none
  !private

  !***  TokenType Enumeration:
!  public :: TokenType, LEFT_PAREN, ...

  enum, bind(c)
    enumerator :: TokenType = 0 ! we use this to name the enumeration

    ! TT prefix to avoid fortran keyword clashes
    ! Single-character tokens.
    enumerator :: TT_LEFT_PAREN, TT_RIGHT_PAREN, TT_LEFT_BRACE, TT_RIGHT_BRACE
    enumerator :: TT_COMMA, TT_DOT, TT_MINUS, TT_PLUS, TT_SEMICOLON, TT_SLASH, TT_STAR

    ! One or two character tokens.
    enumerator :: TT_BANG, TT_BANG_EQUAL
    enumerator :: TT_EQUAL, TT_EQUAL_EQUAL
    enumerator :: TT_GREATER, TT_GREATER_EQUAL
    enumerator :: TT_LESS, TT_LESS_EQUAL

    ! Literals.
    enumerator :: TT_IDENTIFIER, TT_STRING, TT_NUMBER

    ! Keywords.
    enumerator :: TT_AND, TT_CLASS, TT_ELSE, TT_FALSE, TT_FUN, TT_FOR, TT_IF, TT_NIL, TT_OR
    enumerator :: TT_PRINT, TT_RETURN, TT_SUPER, TT_THIS, TT_TRUE, TT_VAR, TT_WHILE

    enumerator :: TT_EOF

  end enum

end module mod_tokentype



!! AA TODO: delete the below
!!**********************************************************
!program Main
  !use Enumerations
  !implicit none
  !integer(kind(TokenType)) :: MyToken
  !!
  !MyToken = TT_BANG
  !!
  !if (MyToken == TT_BANG) then
    !write(*,*) 'MyToken is BANG'
  !end if
  !!
!end program Main
!!**********************************************************

