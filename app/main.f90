program main
  use flox, only: runprompt
  use M_CLI,  only : commandline, check_commandline
  implicit none

  character(len=:),allocatable :: readme ! stores updated namelist
  character(len=256)           :: message
  integer                      :: ios
  character(len=256)           :: file

  namelist /args/ file
     readme=commandline('--file "filename"')
     read(readme,nml=args,iostat=ios,iomsg=message)
     call check_commandline(ios,message)
    !  write(*,*) file

  if (file /= "filename") then
     write(*,*) 'Running file: ',file
    !  call runfile(file)
  else
    call runprompt()
  end if

end program main