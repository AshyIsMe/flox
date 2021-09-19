module flox
  use mod_scanner, only: Scanner, scanTokens
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

    !print *, 'DEBUG: size(a_scanner%tokens):', size(a_scanner%tokens)
    print *, 'DEBUG: a_scanner%tokens:', a_scanner%tokens

  end subroutine run


end module flox
