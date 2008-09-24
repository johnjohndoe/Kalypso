!     Last change:  JH   17 Jan 2007    7:54 am
subroutine inp_mrs(ntg,nammr,LEN_NAMMR,mrnutz,mrbod,mrelem, &
		  kfmr,bmr,drd,drs,drks,kno_ab,perkm,gwszu, &
                  flaech)
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
USE generic_LOGGER
USE Units
USE generic_LOG_FILE
USE generic_NAConstants
IMPLICIT NONE
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      EINGABE
!c      -------
!c      ntg             =       Nummer des Teilgebiets
!c      LEN_NAMMR       =       anzahl Characters des Dateinamens
!c      nammr           =       Name der Mulden-Rigolen Paramterdatei
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c      AUSGABE
!c      -------
!c      kfmr            =       Druchlaessigkeit des Draenrohres
!c      bmr             =       Breite des Mulden-Rigole-Systems
!c      drd             =       Durchmesser des Draenrohres
!c      drs             =       Gefaelle des Rohres
!c      drks            =       equivalente Sandrauigkeit des Draenrohres
!c      inRiver     	=       Flag ob die Mulden-Rigolen in das Gewaesser (1)
!c                              oder ins Kanalnetz (0) entwaessert
!c      flaech          =       Flaech der Mulden-Rigole in [km²]

INCLUDE 'include\param.cmn'
! Calling variables
INTEGER, INTENT(IN) :: ntg,LEN_NAMMR
CHARACTER (LEN=LEN_NAMMR), INTENT(IN) :: nammr
REAL, INTENT(OUT) :: kfmr,bmr,drd,drs,drks,gwszu,perkm,flaech
INTEGER, INTENT(OUT) :: kno_ab,mrelem(2,2*idimnutz)
CHARACTER(LEN=10), INTENT(INOUT) :: mrnutz
CHARACTER(LEN=10), INTENT(INOUT) :: mrbod(idimnutz2)
! Local variables
INTEGER :: ju0gfu,nmr,istat,num
CHARACTER (LEN=120) :: text,zeichen
INTEGER :: ndebug
LOGICAL :: existing
nmr = ju0gfu()



1     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: inp_mrs.f90                    ****',/,10x,     &
      &'****     Teilgebiet:',I6,'                       ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

2     FORMAT (/,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (inp_mrs.f90)             ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************')

ndebug = -1
IF(DEBUG) THEN
   CALL getLogFile( ndebug )
   WRITE(ndebug,1) ntg
ENDIF

! check if the *.mr file exists
INQUIRE ( FILE=nammr,EXIST=existing)
IF ( existing ) THEN

   OPEN (nmr,IOSTAT=istat,STATUS='old',FILE=nammr)
   if (istat < 0) then
      write (nerr,*) 'Fehler beim Oeffnen der Datei (Mulden-Rigolen Parameter)', nammr

      call writeLogString(7, 'Fehler beim Oeffnen der Datei (Mulden-Rigolen Parameter)!', &
           & 'Error opening the file (swale and trench parameter)!',nammr,'','','','','inp_mrs')

      CLOSE (nmr)
      call writeFooter()
      stop
   end if


   read_kopf: do
          read (nmr,*, IOSTAT=istat) zeichen
          if ( istat < 0 ) exit read_kopf
          if ( INDEX(zeichen,'#') == 0 ) exit read_kopf
   end do read_kopf

   BACKSPACE(nmr)

   read_file: do
    	   READ(nmr,'(a)',IOSTAT=istat) text
           IF ( istat /= 0 ) THEN
                 CLOSE(nmr)
                 EXIT read_file
           ENDIF
           ! 1.Zeile des Datenblocks oder Kommentar
      	      IF (text(1:1).eq.'/'.or.text(1:1).eq.'#') THEN
       	      CYCLE read_file
           ENDIF
	   ! liest die 1. Zeile des Datenblock
      	   READ(text(1:120),*,IOSTAT=istat)num
      	   IF ( num == ntg ) THEN
              READ(nmr, *, IOSTAT=istat) flaech,mrnutz,mrbod(1),perkm,gwszu
	      READ (nmr, *, iostat = istat) drd,kfmr,drs,drks,bmr,kno_ab
              ! Es gibt nur eine Nutzung und ein Bodenprofil für die MR
              mrelem(1,1) = 1
              mrelem(2,1) = 1
              !flaech = flaech / 1e+6 ! Umrechnen in km²
              EXIT read_file
           ELSE
              ! Ueberspringt die 2 Daten Zeilen
              READ(nmr,'(a)',IOSTAT=istat) text
              READ(nmr,'(a)',IOSTAT=istat) text
     	   END if
   end do read_file
   IF (DEBUG) THEN
      WRITE(ndebug,2)
   ENDIF
ELSE
  IF (DEBUG) THEN
     WRITE(ndebug,2)
  ENDIF
  RETURN
END IF
!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

end subroutine inp_mrs
