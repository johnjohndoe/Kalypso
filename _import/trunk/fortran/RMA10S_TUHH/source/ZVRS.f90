!     Last change:  K    20 Jun 2007    3:58 pm
!IPK  LAST UPDATE SEP 6 2004 RENAME character variable FNAM to FNAMT
!ipk  last updated Feb 4 1998
!IPK  LAST UPDATED SEPT 8 1995
      SUBROUTINE ZVRS(NTIM)
      USE CHR1MOD
      USE BLK10
      USE BLK10MOD
      SAVE
!
!       This routine is used to define the various necessary configurations
!       based on the type of machine used.
!       Users must set IVRSID to the appropriate constant for their machine.
!
!       IVRSID can take the following values
!       1   F77 standard  Direct access record length unlimited, and
!                         defined in terms of bytes
!           Example systems            Definicon 032 board
!                                      M S FORTRAN for IBM micros
!
!       2   F77 standard  Direct access record length unlimited, and
!                         defined in terms of short words (2 bytes)
!           Example systems            Prime mini computers
!
!       3   F77 standard  Direct access record length limited to 32k bytes
!                         defined in terms of long words (4 bytes)
!           Example systems            DEC Vax
!
!       4   F77 standard  Direct access defined using multiple sequential
!                         access files that are opened as required. Note that
!                         this may generate and leave many files on disk
!           Example sytems             Intel based Lahey or Compaq compilers
!                                      DEC alpha to avoid short record limit
!
!       5   F77 standard  Direct access defined for systems using 64 bit
!                         or 8 byte words and where record lengths are
!                         defined in bytes
!           Example systems            Cray or CDC Cyber
!
!       6   F77 standard  Direct access defined using multiple sequential
!                         access files that are opened as required. Note that
!                         this version does not put period in file name.
!                         It may generate and leave many files on disk
!           Example sytems             CDC Cyber
!
!           USE PARAMETER VALUE FOR INITIAL BUFFER SIZE
!
      IF (NTIM == 0) then
!
      WRITE(LOUT,6000)
 6000 FORMAT(10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW..PROGRAM RMA-10S&
     &'/ 10X, ' HYDRODYNAMICS SAND/SED'                                 &
     &      /,10X,'VERSION 3.5D MAY 2006'//                             &
     &'          COPYRIGHT'/                                            &
     &'          IAN P KING'/                                           &
     &'          RESOURCE MODELLING ASSOCIATES'/                        &
     &'          9 DUMARESQ STREET'/                                    &
     &'          SYDNEY, AUSTRALIA'/                                    &
     &          ///)
      RETURN
!
!
      else
!
!..... Clean up scratch files
!
      WRITE(*,*) 'IN ZVRS MAXFIL =',MAXFIL
      IF(MAXFIL == 0) RETURN
      ivrsid = 4
      IF(IVRSID == 4 .OR. IVRSID == 6) THEN
        DO 800 NRC=1,MAXFIL
          WRITE(TSUB,3000) NRC
 3000     FORMAT(I3.3)
          IF(IVRSID == 4) THEN
!IPKSEP04
            FNAMT=FHED//'.'//TSUB(1:3)
            CLOSE (ND1)
            OPEN(ND1,FILE=FNAMT,STATUS='UNKNOWN',                       &
     &      FORM='UNFORMATTED')
            CLOSE(ND1,STATUS='DELETE')
          ELSE
            FNBM=FHED//TSUB(1:3)
            CLOSE (ND1)
            OPEN(ND1,FILE=FNBM,STATUS='UNKNOWN',                        &
     &      FORM='UNFORMATTED')
            CLOSE(ND1,STATUS='DELETE')
          ENDIF
!
  800   CONTINUE
      ENDIF
      RETURN
!
      endif 
      END
