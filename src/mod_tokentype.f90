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

  contains
    function tt_tostring(tt) result(res)
      integer(kind(TokenType)) :: tt
      character(len=:), allocatable :: res
      character(modulo(huge(tt),10)+1+1) :: tts

      select case (tt)
      case(TT_LEFT_PAREN);      res   =   'TT_LEFT_PAREN'
      case(TT_RIGHT_PAREN);     res   =   'TT_RIGHT_PAREN'
      case(TT_LEFT_BRACE);      res   =   'TT_LEFT_BRACE'
      case(TT_RIGHT_BRACE);     res   =   'TT_RIGHT_BRACE'
      case(TT_COMMA);           res   =   'TT_COMMA'
      case(TT_DOT);             res   =   'TT_DOT'
      case(TT_MINUS);           res   =   'TT_MINUS'
      case(TT_PLUS);            res   =   'TT_PLUS'
      case(TT_SEMICOLON);       res   =   'TT_SEMICOLON'
      case(TT_SLASH);           res   =   'TT_SLASH'
      case(TT_STAR);            res   =   'TT_STAR'
      case(TT_BANG);            res   =   'TT_BANG'
      case(TT_BANG_EQUAL);      res   =   'TT_BANG_EQUAL'
      case(TT_EQUAL);           res   =   'TT_EQUAL'
      case(TT_EQUAL_EQUAL);     res   =   'TT_EQUAL_EQUAL'
      case(TT_GREATER);         res   =   'TT_GREATER'
      case(TT_GREATER_EQUAL);   res   =   'TT_GREATER_EQUAL'
      case(TT_LESS);            res   =   'TT_LESS'
      case(TT_LESS_EQUAL);      res   =   'TT_LESS_EQUAL'
      case(TT_IDENTIFIER);      res   =   'TT_IDENTIFIER'
      case(TT_STRING);          res   =   'TT_STRING'
      case(TT_NUMBER);          res   =   'TT_NUMBER'
      case(TT_AND);             res   =   'TT_AND'
      case(TT_CLASS);           res   =   'TT_CLASS'
      case(TT_ELSE);            res   =   'TT_ELSE'
      case(TT_FALSE);           res   =   'TT_FALSE'
      case(TT_FUN);             res   =   'TT_FUN'
      case(TT_FOR);             res   =   'TT_FOR'
      case(TT_IF);              res   =   'TT_IF'
      case(TT_NIL);             res   =   'TT_NIL'
      case(TT_OR);              res   =   'TT_OR'
      case(TT_PRINT);           res   =   'TT_PRINT'
      case(TT_RETURN);          res   =   'TT_RETURN'
      case(TT_SUPER);           res   =   'TT_SUPER'
      case(TT_THIS);            res   =   'TT_THIS'
      case(TT_TRUE);            res   =   'TT_TRUE'
      case(TT_VAR);             res   =   'TT_VAR'
      case(TT_WHILE);           res   =   'TT_WHILE'
      case(TT_EOF);             res   =   'TT_EOF'

      case default
        write(tts, '(i9)') tt
        error stop 'Error: Invalid TokenType value.' // tts
      end select
    end function


end module mod_tokentype
