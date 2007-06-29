C     Last change:  K    20 Jun 2007    3:58 pm
CIPK  LAST UPDATE SEP 6 2004 RENAME character variable FNAM to FNAMT
cipk  last updated Feb 4 1998
CIPK  LAST UPDATED SEPT 8 1995
      SUBROUTINE ZVRS(NTIM)
      USE BLK10
      USE BLK10MOD
      SAVE
CIPK AUG05      INCLUDE 'BLK10.COM'
CIPK AUG05      INCLUDE 'BLK4.COM'
CIPKSEP04
      COMMON /CHR1/ FNAMT,FNBM,TNAM,FHED,TSUB,TNUM
      CHARACTER*8 FNAMT
      CHARACTER*7 FNBM
      CHARACTER*4 TNAM,FHED
      CHARACTER*3 TSUB,TNUM
C
C       This routine is used to define the various necessary configurations
C       based on the type of machine used.
C       Users must set IVRSID to the appropriate constant for their machine.
C
C       IVRSID can take the following values
C       1   F77 standard  Direct access record length unlimited, and
C                         defined in terms of bytes
C           Example systems            Definicon 032 board
C                                      M S FORTRAN for IBM micros
C
C       2   F77 standard  Direct access record length unlimited, and
C                         defined in terms of short words (2 bytes)
C           Example systems            Prime mini computers
C
C       3   F77 standard  Direct access record length limited to 32k bytes
C                         defined in terms of long words (4 bytes)
C           Example systems            DEC Vax
C
C       4   F77 standard  Direct access defined using multiple sequential
C                         access files that are opened as required. Note that
C                         this may generate and leave many files on disk
C           Example sytems             Intel based Lahey or Compaq compilers
C                                      DEC alpha to avoid short record limit
C
C       5   F77 standard  Direct access defined for systems using 64 bit
C                         or 8 byte words and where record lengths are
C                         defined in bytes
C           Example systems            Cray or CDC Cyber
C
C       6   F77 standard  Direct access defined using multiple sequential
C                         access files that are opened as required. Note that
C                         this version does not put period in file name.
C                         It may generate and leave many files on disk
C           Example sytems             CDC Cyber
C
C           USE PARAMETER VALUE FOR INITIAL BUFFER SIZE
C
      IF(NTIM .GT. 0) GO TO 750
      LBMAX=NBS
C
      WRITE(LOUT,6000)
 6000 FORMAT(10X, 'FINITE ELEMENT METHOD FOR FLUID FLOW..PROGRAM RMA-10S
     +'/ 10X, ' HYDRODYNAMICS SAND/SED'
     +      /,10X,'VERSION 3.5D MAY 2006'//
     +'          COPYRIGHT'/
     +'          IAN P KING'/
     +'          RESOURCE MODELLING ASSOCIATES'/
     +'          9 DUMARESQ STREET'/
     +'          SYDNEY, AUSTRALIA'/
     +          ///)
!nis,jun07: Because file.sub is moved to a place before input is called and ioptzd is read, the variable has no value, therefore deactivating it here
!           What to do with it? It just shows the turbulence closure method, but what exactly?
!      WRITE(LOUT,*) 'IOPTZD HAS SET TO',IOPTZD
!-

C
C             SET IVRSID HERE!!
C
      IVRSID=4
C
C
      IF(IVRSID .EQ. 1 ) THEN
        WRITE(LOUT,6010)
        WRITE(*,6010)
 6010 FORMAT('  IVRSID=1   F77 standard  Direct access record length'/
     +       ' unlimited, and defined in terms of bytes'/
     +       ' Example systems            Definicon 032 board'/
     +      '                            M S FORTRAN for IBM micros'//)
      ELSEIF(IVRSID .EQ. 2) THEN
        WRITE(LOUT,6020)
        WRITE(*,6020)
 6020 FORMAT('  IVRSID=2   F77 standard  Direct access record length'/
     +     ' unlimited, and defined in terms of short words (2 bytes)'/
     +     ' Example systems            Prime mini computers'//)
      ELSEIF(IVRSID .EQ. 3) THEN
        WRITE(LOUT,6030)
        WRITE(*,6030)
 6030 FORMAT('  IVRSID=3   F77 standard  Direct access record length'/
     +     ' limited to 32k bytes, and  defined in terms of long '/
     +     ' words (4 bytes)'/
     +     ' Example systems            DEC Vax'//)
        IF(NBS .GT. 4000) THEN
          LBMAX=4000
          WRITE(LOUT,6031)
 6031     FORMAT(' Maximum buffer size reset to 4000 to conform to'/
     +           ' system limits as defined by IVRSID')
        ENDIF
      ELSEIF(IVRSID .EQ. 4) THEN
        WRITE(LOUT,6040)
        WRITE(*,6040)
 6040 FORMAT('  IVRSID=4   F77 standard  Direct access defined using'/
     + ' multiple sequential access files that are opened as required.'/
     + ' Note that this may generate and leave many files on disk'/
     + ' Example sytems             PCs under Lahey or Compaq FORTRAN'/
     + '                            UNIX systems'/
     +'                            DEC Vax to avoid short record limit'
     +//)
      ELSEIF(IVRSID .EQ. 5) THEN
        WRITE(LOUT,6050)
        WRITE(*,6050)
 6050 FORMAT('  IVRSID=5   F77 standard  Direct access defined for'/
     + ' systems using 64 bit or 8 byte words and where record'/
     + ' lengths are defined in bytes'/
     + ' Example systems            Cray or CDC Cyber'//)
      ELSEIF(IVRSID .EQ. 6) THEN
        WRITE(LOUT,6060)
        WRITE(*,6060)
 6060 FORMAT('  IVRSID=6   F77 standard  Direct access defined using'/
     + ' multiple sequential access files that are opened as required.'/
     + ' Note that this version generates file names that have no .'/
     + ' This option may generate and leave many files on disk'/
     + ' Example systems             CDC Cyber'/
     + '                            Definicon 020 board'/
     +'                            DEC Vax to avoid short record limit'
     +//)
      ENDIF
      RETURN
  750 CONTINUE
C
C..... Clean up scratch files
C
      WRITE(*,*) 'IN ZVRS MAXFIL =',MAXFIL
      IF(MAXFIL .EQ. 0) RETURN
      IF(IVRSID .EQ. 4  .OR.  IVRSID .EQ. 6) THEN
        DO 800 NRC=1,MAXFIL
          WRITE(TSUB,3000) NRC
 3000     FORMAT(I3.3)
          IF(IVRSID .EQ. 4) THEN
CIPKSEP04
            FNAMT=FHED//'.'//TSUB(1:3)
            CLOSE (ND1)
            OPEN(ND1,FILE=FNAMT,STATUS='UNKNOWN',
     +      FORM='UNFORMATTED')
            CLOSE(ND1,STATUS='DELETE')
          ELSE
            FNBM=FHED//TSUB(1:3)
            CLOSE (ND1)
            OPEN(ND1,FILE=FNBM,STATUS='UNKNOWN',
     +      FORM='UNFORMATTED')
            CLOSE(ND1,STATUS='DELETE')
          ENDIF
C
  800   CONTINUE
      ENDIF
      RETURN
      END
