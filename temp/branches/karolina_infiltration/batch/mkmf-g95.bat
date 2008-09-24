Echo off
REM This file can be called from anywhere to create a Makefile
REM based on the Fortran source files in that location.
REM a.exe is the name of final output. Change it to fit your needs.
REM If ignored, the mkmf script will use a.out by default.
REM g95_args is the name of a template file used to set user
REM environment variables. Change it as you wish.
REM
REM Perl batch\mkmf.pl -t batch\g95_args -p a.exe
If (%1)==() (
Perl batch\mkmf.pl -t batch\g95_args -p a.exe
) Else (
Perl batch\mkmf.pl -t batch\g95_args -p %1
)
REM The above allows the target name to be passed as an argument