!     Last change:  JH   17 Jan 2007    8:15 am
subroutine calc_nutz_bod_id(nutz,boden,kennutz,kennbod,kenelem,anznutz,anzbod,anzelem,nerr)

!***************************************BESCHREIBUNG********************************************
!c      Diese subroutine ordnet die Nutzung und Bodenprofile den eingelesenen Hydrotpen in
!c	einem Teilgebiet zu.

!***************************************EIN-/AUSGABE********************************************
!c      EINGABE
!c      -------
!c
!c      anzelem                 Die anzahl der eingelesenen Hydtrotope
!c      nutz                    Die zum einzelnen Hydrotop eingelesene Nutzungs-ID
!c      boden                   Die 		-"-	               Bodenprofil-ID
!c      nerr                    Kanalnummer der Fehlerdatei (output.err)
!c
!c
!c      AUSGABE
!c      -------
!c
!c      kennutz                 Die entsprechend der Hydrotop-ID zugeordnete Nutzung
!c      kennbod                 Das                -"-                       Bodenprofil
!c      kenelem                 Die Verknüpfung der Hydrotope mit der
!c      anznutz                 Anzahl der vorhanden Nutzungen im Teilgebiet
!c      anzbod                          -"-          Bodenprofile im Teilgebiet


USE generic_LOG_FILE
USE generic_naconstants

IMPLICIT NONE
INCLUDE 'include\param.cmn'

!******************************************VARIABLEN********************************************
INTEGER, INTENT(IN) :: nerr,anzelem
CHARACTER(LEN=10), INTENT(IN) :: nutz(2*idimnutz), boden(2*idimnutz)

INTEGER, INTENT(INOUT):: anznutz,anzbod
INTEGER, INTENT(OUT) :: kenelem(2,2*idimnutz)
CHARACTER(LEN=10), INTENT(OUT) :: kennutz(idimnutz2)
CHARACTER(LEN=10), INTENT(OUT) :: kennbod(idimnutz2)

!Local vars
INTEGER :: i1,i3,ndebug

! DEBUG INFO

1     FORMAT (////,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     DEBUGINFO                               ****',/,10x,     &
      &'****     Routine: calc_nutz_bod_id.f90           ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',//)

2     FORMAT (////,10x,                                                  &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'****     ENDE OF BLOCK (calc_nutz_bod_id.f90)    ****',/,10x,     &
      &'****                                             ****',/,10x,     &
      &'*****************************************************',/,10x,     &
      &'*****************************************************',//)

ndebug = -1
 IF ( DEBUG ) THEN
     CALL getLogFile( ndebug )
     WRITE (ndebug,1)
 END IF

anzbod=0
anznutz=0
DO i1=1,anzelem
   DO i3=1,anzbod
      IF (kennbod(i3).eq.boden(i1)) THEN
	  kenelem(1,i1)=i3
          IF (DEBUG) THEN
	     write (ndebug,*) 'i1=',i1,' i3=',i3,' (bod-loop)kennelem(1,',i1,')=', kenelem(1,i1)
          END IF
	  goto 204
      END IF
   END DO
   anzbod=anzbod+1
   IF (anzbod > idimnutz2) THEN
      write(nerr,9037) idimnutz2
      call writeFooter()
      STOP 'calc_nutz_bod_id'
   ENDIF
   kennbod(anzbod)=boden(i1)
   kenelem(1,i1)=anzbod
   IF (DEBUG) THEN
      write (ndebug,*) '(elem-loop-1)kennbod(',anzbod,')=',boden(i1),' kenelem(1,',i1,')=',kenelem(1,i1)
   END IF
   204   continue
   DO i3=1,anznutz
      IF (kennutz(i3).eq.nutz(i1)) THEN
         kenelem(2,i1)=i3
         IF (DEBUG) THEN
            write (ndebug,*) 'i1=',i1,' i3=',i3,' (nutz-loop)kennelem(2,',i1,')=', kenelem(2,i1)
         END IF
         goto 206
      END IF
   END DO
   anznutz=anznutz+1
   if ( anznutz > idimnutz2) THEN
      write(nerr,9038) idimnutz2
      call writeFooter()
      STOP 'calc_nutz_bod_id'
   ENDIF
   kennutz(anznutz)=nutz(i1)
   kenelem(2,i1)=anznutz
   IF (DEBUG) THEN
      write (ndebug,*) '(elem-loop-2)kennutz(',anznutz,')=',nutz(i1),' kenelem(2,',i1,')=',kenelem(2,i1)
   END IF
   206      continue
END DO !ende anzelem

IF (DEBUG) THEN
   WRITE (ndebug,2)
END IF



!**********************************FEHLERMELDUNGEN UND FORMATE*****************************************

9037  format(/,'xxx max. ',i2,'bodenarten pro teilgebiet ',         &
               'sind zugelassen.'/                                    &
               'xxx erhoehe parameter idimnutz2 in param.cmn! ')

9038  format(/,'xxx max. ',i2,'nutzungsarten pro teilgebiet ',      &
               'sind zugelassen.'/                                    &
               'xxx erhoehe parameter idimnutz2 in param.cmn! ')


end
