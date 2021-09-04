module flox
  use mod_scanner, only: Scanner, Token, scanTokens
  use M_strings, only: split

  implicit none
  private

  ! AA TODO Figure out what to do about array size magic numbers
  ! integer,parameter,public :: line_len=512

  public :: runprompt, runfile
contains
  subroutine runprompt
    character(512) :: line
    ! character(len=line_len) :: line

    INFINITE: do
      write(*,'(a)', advance="no") "> "
      read(*,'(a)') line
      !write(*,'(a)') line
      call run(line)

    enddo INFINITE
  end subroutine runprompt

  subroutine runfile (path)
    character(*) :: path
    character(512) :: line

    open(1, file=path)
    do
       read(1, *) line
       call run(line)
    end do

  end subroutine runfile

  subroutine run (line)
    character(512), intent(in) :: line
    type(Scanner) :: a_scanner
    integer :: i

    a_scanner = Scanner(line)
    call scanTokens(a_scanner)

    print *, 'DEBUG: a_scanner:', a_scanner%tokens

  end subroutine run

  subroutine error (line, message)
    integer :: line
    character(*) :: message

    call report(line, '', message)
  end subroutine error

  subroutine report (line, s_where, message)
    integer :: line
    character(*) :: s_where
    character(*) :: message

    print *, '[line ', line, '] Error', s_where, ': ', message
  end subroutine report

end module flox
