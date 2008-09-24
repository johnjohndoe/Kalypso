!     Last change:  JH   10 Oct 2007   11:53 am
MODULE generic_LOGGER

!**************************************LICENSE**************************************************
!
! This code is part of the library 'Kalypso-NA'.
! KALYPSO-NA is a deterministic, non-linear, detailed Rainfall-Runoff-Model (RRM).
! The model permits a complete simulation of the land bound
! part of the global water balance as a reaction on observed precipitation events.
! Copyright (C) 2004  HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering (in co-operation with Bjoernsen Cunsulting Engineers)
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
! For information please contact:
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering. Denickestr. 22, 21073 Hamburg, Germany.
! Dipl.-Ing. Jessica Hübsch:   phone: +49 40 42878 4181 mail: j.huebsch@tuhh.de
! See our web page: www.tuhh.de/wb
!
!
! HAMBURG UNIVERSITY OF TECHNOLOGY, Department of River and
! Coastal Engineering, hereby disclaims all copyright interest in
! the library 'Kalypso-NA'.
!
! Jessica Hübsch, 16 August 2004
! Research Associate
!
!***********************************************************************************************
IMPLICIT NONE
PUBLIC
LOGICAL :: language = .true. !Deutsch - evtl. später einmal wählbar in konfig
INTEGER, PUBLIC :: dummy_element_value = 0
REAL(KIND=8), PUBLIC :: dummy_para_value=0.
CONTAINS

! This soubroutine writes the errorlog file.
! logLevels:
!       7: SEVERE       serious failure (program stop)
!       6: WARNING      potential problem
!       5: INFO         informational messages
!       4: CONFIG       static configuration messages
!       3: FINE         tracing information
!       2: FINER        detailed tracing information
!       1: FINEST       highly detailed tracing message
! IN:
!       logLevel	: see logLevel description above
!       message_de      : Error message german
!       message_en      : Error message english (optional)
!       element         : element, where the error occures (strand-, node or sub catchmentnumber) (optional)
!       filename        : Name of the ASCII File, where the error occures (i.e. read/write statements errors )
!       element_value   : Value (name) of the element (optional)
!       para            : parameter, where the error occures (optional)
!       para_value      : value of the parameter (optional)
!       subroutine      : name of the caller subroutine (optional)


SUBROUTINE getLogLevel (loglevel, levelString)
implicit none
INTEGER, INTENT(IN) :: loglevel
CHARACTER (LEN=*), INTENT(OUT) :: levelString

if (loglevel == 1) then
levelString = 'FINEST'
else if (loglevel == 2) then
levelString = 'FINER'
else if (loglevel == 3) then
levelString = 'FINE'
else if (loglevel == 4) then
levelString = 'CONFIG'
else if (loglevel == 5) then
levelString = 'INFO'
else if (loglevel == 6) then
levelString = 'WARNING'
else if (loglevel == 7) then
levelString = 'SEVERE'
else
levelString = 'null'
end if
END SUBROUTINE

SUBROUTINE writeHeader()

USE Units
implicit none

WRITE(nerrlog,8000)
8000 format ("<?xml version=""1.0"" encoding=""UTF-8""?>",/&
            1X, "<LogMessage xmlns=""http://www.tuhh.de/NAFortranLog"" xmlns:gml=""http://www.opengis.net/gml"" ",/&
             &, "xmlns:xlink=""http://www.w3.org/1999/xlink"" xmlns:zmlinline=""inline.zml.kalypso.org"" ",/&
             &, "xmlns:obslink=""obslink.zml.kalypso.org"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">")
END SUBROUTINE writeHeader

SUBROUTINE writeFooter()

USE Units
implicit none

WRITE(nerrlog,8001)
8001 format (1X, "</LogMessage>")
END SUBROUTINE writeFooter




SUBROUTINE writeLogString( logLevel, message_de, message_en, filename, element, element_value, para, para_value, soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel
CHARACTER (LEN=8) :: levelString
CHARACTER (LEN=*), INTENT(IN) :: element_value
CHARACTER (LEN=*), INTENT(IN) :: para_value
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para, soub

call getLogLevel(loglevel,levelString)

IF (language) THEN
WRITE(nerrlog,9999)levelString,filename,soub,message_de,element,element_value,para,para_value
ELSE
WRITE(nerrlog,9999)levelString,filename,soub,message_en,element,element_value,para,para_value
END IF

9999 format (1X, '<log>',/&
      &1X, '<record>',/&
	  &3X, '<level>',A,'</level>',/&
	  &3X, '<class>',A,'</class>',/&
	  &3X, '<method>',A,'</method>',/&
	  &3X, '<message>',A,'</message>',/&
	  &3X, '<element>',A,'  ',A5,'</element>',/&
	  &3X, '<param>',A,'=',A,'</param>',/&
      &1X, '</record>',/&
      &1X, '</log>')


END SUBROUTINE

SUBROUTINE writeLogIntReal( logLevel, message_de, message_en, filename, element, element_value, para, para_value, soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel
CHARACTER (LEN=8) :: levelString
REAL, INTENT (IN) :: para_value
INTEGER, INTENT(IN) :: element_value
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para, soub

call getLogLevel(loglevel,levelString)
IF (language) THEN
WRITE(nerrlog,9998)levelString,filename,soub,message_de,element,element_value,para,para_value
ELSE
WRITE(nerrlog,9998)levelString,filename,soub,message_en,element,element_value,para,para_value
END IF

9998 format (1X, '<log>',/&
      &1X, '<record>',/&
	  &3X,'<level>',A,'</level>',/&
	  &3X,'<class>',A,'</class>',/&
	  &3X,'<method>',A,'</method>',/&
	  &3X,'<message>',A,'</message>',/&
	  &3X,'<element>',A,' ',I10,'</element>',/&
	  &3X,'<param>',A,'=',f15.4,'</param>',/&
      &1X, '</record>',/&
      &1X, '</log>')
      END SUBROUTINE

SUBROUTINE writeLogIntInt( logLevel, message_de, message_en, filename, element, element_value, para, para_value, soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel,para_value
CHARACTER (LEN=8) :: levelString
INTEGER, INTENT(IN) :: element_value
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para, soub

call getLogLevel(loglevel,levelString)
IF (language) THEN
WRITE(nerrlog,9997)levelString,filename,soub,message_de,element,element_value,para,para_value
ELSE
WRITE(nerrlog,9997)levelString,filename,soub,message_en,element,element_value,para,para_value
END IF

9997 format (1X, '<log>',/&
      &1X,'<record>',/&
	  &3X,'<level>',A,'</level>',/&
	  &3X,'<class>',A,'</class>',/&
	  &3X,'<method>',A,'</method>',/&
	  &3X,'<message>',A,'</message>',/&
	  &3X,'<element>',A,' ',I10,'</element>',/&
	  &3X,'<param>',A,'=',I10,'</param>',/&
	  &1X, '</record>',/&
      &1X, '</log>')

END SUBROUTINE


SUBROUTINE writeLogIntIntInt( logLevel, message_de, message_en, filename, element, element_value, para1, para1_value,&
& para2, para2_value,soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel,para1_value, para2_value
CHARACTER (LEN=8) :: levelString
INTEGER, INTENT(IN) :: element_value
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para1,para2, soub

call getLogLevel(loglevel,levelString)
IF (language) THEN
WRITE(nerrlog,9996)levelString,filename,soub,message_de,element,element_value,para1,para1_value,para2,para2_value
ELSE
WRITE(nerrlog,9996)levelString,filename,soub,message_en,element,element_value,para1,para1_value,para2,para2_value
END IF

9996 format (1X, '<log>',/&
      &1X,'<record>',/&
	  &3X,'<level>',A,'</level>',/&
	  &3X,'<class>',A,'</class>',/&
	  &3X,'<method>',A,'</method>',/&
	  &3X,'<message>',A,'</message>',/&
	  &3X,'<element>',A,' ',I10,'</element>',/&
	  &3X,'<param>',A,'=',I10,';',A,'=',I10,'</param>',/&
	  &1X, '</record>',/&
      &1X, '</log>')



END SUBROUTINE

SUBROUTINE writeLogIntIntReal( logLevel, message_de, message_en, filename, element, element_value, para1, para1_value,&
& para2, para2_value,soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel,para1_value
INTEGER, INTENT(IN) :: element_value
REAL, INTENT (IN) :: para2_value
CHARACTER (LEN=8) :: levelString
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para1,para2, soub


call getLogLevel(loglevel,levelString)
IF (language) THEN
WRITE(nerrlog,9995)levelString,filename,soub,message_de,element,element_value,para1,para1_value,para2,para2_value
ELSE
WRITE(nerrlog,9995)levelString,filename,soub,message_en,element,element_value,para1,para1_value,para2,para2_value
END IF

9995 format (1X, '<log>',/&
      &1X,'<record>',/&
	  &3X,'<level>',A,'</level>',/&
	  &3X,'<class>',A,'</class>',/&
	  &3X,'<method>',A,'</method>',/&
	  &3X,'<message>',A,'</message>',/&
	  &3X,'<element>',A,' ',I10,'</element>',/&
	  &3X,'<param>',A,'=',I10,';',A,'=',f15.4,'</param>',/&
	  &1X, '</record>',/&
      &1X, '</log>')


END SUBROUTINE


SUBROUTINE writeLogIntRealReal( logLevel, message_de, message_en, filename, element, element_value, para1, para1_value,&
& para2, para2_value,soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel
REAL, INTENT(IN)::  para1_value, para2_value
INTEGER, INTENT(IN) :: element_value
CHARACTER (LEN=8) :: levelString
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para1,para2, soub

call getLogLevel(loglevel,levelString)
IF (language) THEN
WRITE(nerrlog,9994)levelString,filename,soub,message_de,element,element_value,para1,para1_value,para2,para2_value
ELSE
WRITE(nerrlog,9994)levelString,filename,soub,message_en,element,element_value,para1,para1_value,para2,para2_value
END IF


9994 format (1X, '<log>',/&
      &1X,'<record>',/&
	  &3X,'<level>',A,'</level>',/&
	  &3X,'<class>',A,'</class>',/&
	  &3X,'<method>',A,'</method>',/&
	  &3X,'<message>',A,'</message>',/&
	  &3X,'<element>',A,' ',I10,'</element>',/&
	  &3X,'<param>',A,'=',f15.4,';',A,'=',f15.4,'</param>',/&
	  &1X, '</record>',/&
      &1X, '</log>')


END SUBROUTINE

SUBROUTINE writeLogIntRealRealRealReal( logLevel, message_de, message_en, filename, element, element_value, para1, para1_value,&
& para2,para2_value,para3,para3_value,para4,para4_value,soub )

USE Units
implicit none
INTEGER, INTENT(IN) :: loglevel
REAL, INTENT(IN)::  para1_value, para2_value,para3_value, para4_value
INTEGER, INTENT(IN) :: element_value
CHARACTER (LEN=8) :: levelString
CHARACTER (LEN=*), INTENT(IN) :: message_de, message_en, filename
CHARACTER (LEN=*), INTENT(IN) :: element, para1,para2,para3,para4, soub

call getLogLevel(loglevel,levelString)
IF (language) THEN
WRITE(nerrlog,9994)levelString,filename,soub,message_de,element,element_value,para1,para1_value,para2,para2_value,para3,&
                    &para3_value,para4,para4_value
ELSE
WRITE(nerrlog,9994)levelString,filename,soub,message_en,element,element_value,para1,para1_value,para2,para2_value,para3,&
                    &para3_value,para4,para4_value
END IF


9994 format (1X, '<log>',/&
      &1X,'<record>',/&
	  &3X,'<level>',A,'</level>',/&
	  &3X,'<class>',A,'</class>',/&
	  &3X,'<method>',A,'</method>',/&
	  &3X,'<message>',A,'</message>',/&
	  &3X,'<element>',A,' ',I10,'</element>',/&
	  &3X,'<param>',A,'=',f15.4,';',A,'=',f15.4,';',A,'=',f15.4,';',A,'=',f15.4,'</param>',/&
	  &1X, '</record>',/&
      &1X, '</log>')


END SUBROUTINE

END MODULE generic_LOGGER
