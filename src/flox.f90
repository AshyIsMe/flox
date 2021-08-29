module flox
  implicit none
  private

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

    write(*,'(a)') line

  end subroutine run

end module flox
