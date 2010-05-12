module mod_Message

type Message
  character (len = 2000) :: messText
  integer (kind = 4) :: messLength
end type

contains

  function newMessage (messageText)
    implicit none
  
!function name    
    type (message), pointer :: newMessage     
!arguments    
    character (len = *), optional :: messageText
!local variables>
    type (Message), pointer :: new => null()
    
    
!create the file    
    allocate (new)                   !weist speicherort zu
    if (present (messageText)) then
      new%messText = messageText
    else
      new%messText = 'dummyText'
    end if
    new%messLength = len ( trim (new%messText))
    
    newMessage => new
    return
  end function

end module