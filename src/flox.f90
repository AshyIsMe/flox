module flox
  implicit none
  private

  ! integer,parameter,public :: line_len=512

  public :: runprompt
contains
  subroutine runprompt
    character(512) :: line
    ! character(len=line_len) :: line

    INFINITE: do
      write(*,'(a)', advance="no") "> "
      read(*,'(a)') line
      write(*,'(a)') line
    enddo INFINITE
  end subroutine runprompt

end module flox
