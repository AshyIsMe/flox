module flox
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, flox!"
  end subroutine say_hello
end module flox
