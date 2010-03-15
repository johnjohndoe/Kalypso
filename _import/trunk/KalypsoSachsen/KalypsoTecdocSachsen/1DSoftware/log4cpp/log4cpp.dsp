# Microsoft Developer Studio Project File - Name="log4cpp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=log4cpp - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "log4cpp.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "log4cpp.mak" CFG="log4cpp - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "log4cpp - Win32 Release" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "log4cpp - Win32 Debug" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "log4cpp - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\log4cpp"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\log4cpp"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I ".." /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /FR /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "log4cpp - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\log4cpp"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\log4cpp"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ  /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I ".." /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /FR /FD /GZ  /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "log4cpp - Win32 Release"
# Name "log4cpp - Win32 Debug"
# Begin Group "Quellcodedateien"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\Appender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\AppenderSkeleton.cpp
# End Source File
# Begin Source File

SOURCE=.\src\BasicLayout.cpp
# End Source File
# Begin Source File

SOURCE=.\src\Category.cpp
# End Source File
# Begin Source File

SOURCE=.\src\CategoryStream.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FileAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\Filter.cpp
# End Source File
# Begin Source File

SOURCE=.\src\FixedContextCategory.cpp
# End Source File
# Begin Source File

SOURCE=.\src\HierarchyMaintainer.cpp
# End Source File
# Begin Source File

SOURCE=.\src\IdsaAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\LayoutAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\Log4cppCleanup.cpp
# End Source File
# Begin Source File

SOURCE=.\src\LoggingEvent.cpp
# End Source File
# Begin Source File

SOURCE=.\src\NDC.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OstreamAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\OstringStream.cpp
# End Source File
# Begin Source File

SOURCE=.\src\PatternLayout.cpp
# End Source File
# Begin Source File

SOURCE=.\src\Priority.cpp
# End Source File
# Begin Source File

SOURCE=.\src\RemoteSyslogAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\SimpleConfigurator.cpp
# End Source File
# Begin Source File

SOURCE=.\src\SimpleLayout.cpp
# End Source File
# Begin Source File

SOURCE=.\src\snprintf.c
# End Source File
# Begin Source File

SOURCE=.\src\StringQueueAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\SyslogAppender.cpp
# End Source File
# Begin Source File

SOURCE=.\src\TimeStamp.cpp
# End Source File
# End Group
# Begin Group "Header-Dateien"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Appender.hh
# End Source File
# Begin Source File

SOURCE=.\AppenderSkeleton.hh
# End Source File
# Begin Source File

SOURCE=.\BasicLayout.hh
# End Source File
# Begin Source File

SOURCE=.\Category.hh
# End Source File
# Begin Source File

SOURCE=.\CategoryStream.hh
# End Source File
# Begin Source File

SOURCE=".\config-openvms.h"
# End Source File
# Begin Source File

SOURCE=".\config-win32.h"
# End Source File
# Begin Source File

SOURCE=.\Export.hh
# End Source File
# Begin Source File

SOURCE=.\FileAppender.hh
# End Source File
# Begin Source File

SOURCE=.\Filter.hh
# End Source File
# Begin Source File

SOURCE=.\FixedContextCategory.hh
# End Source File
# Begin Source File

SOURCE=.\HierarchyMaintainer.hh
# End Source File
# Begin Source File

SOURCE=.\IdsaAppender.hh
# End Source File
# Begin Source File

SOURCE=.\Layout.hh
# End Source File
# Begin Source File

SOURCE=.\LayoutAppender.hh
# End Source File
# Begin Source File

SOURCE=.\Log4cppCleanup.hh
# End Source File
# Begin Source File

SOURCE=.\LoggingEvent.hh
# End Source File
# Begin Source File

SOURCE=.\NDC.hh
# End Source File
# Begin Source File

SOURCE=.\OstreamAppender.hh
# End Source File
# Begin Source File

SOURCE=.\OstringStream.hh
# End Source File
# Begin Source File

SOURCE=.\PatternLayout.hh
# End Source File
# Begin Source File

SOURCE=.\Portability.hh
# End Source File
# Begin Source File

SOURCE=.\Priority.hh
# End Source File
# Begin Source File

SOURCE=.\RemoteSyslogAppender.hh
# End Source File
# Begin Source File

SOURCE=.\SimpleConfigurator.hh
# End Source File
# Begin Source File

SOURCE=.\SimpleLayout.hh
# End Source File
# Begin Source File

SOURCE=.\StringQueueAppender.hh
# End Source File
# Begin Source File

SOURCE=.\SyslogAppender.hh
# End Source File
# Begin Source File

SOURCE=.\TimeStamp.hh
# End Source File
# End Group
# End Target
# End Project
