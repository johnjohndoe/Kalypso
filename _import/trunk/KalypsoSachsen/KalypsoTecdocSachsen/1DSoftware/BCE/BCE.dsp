# Microsoft Developer Studio Project File - Name="BCE" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=BCE - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "BCE.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "BCE.mak" CFG="BCE - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "BCE - Win32 Release" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "BCE - Win32 Debug" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "BCE - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\BCE"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\BCE"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "Include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /FR /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "BCE - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\BCE"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\BCE"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /FR /FD /GZ /c
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

# Name "BCE - Win32 Release"
# Name "BCE - Win32 Debug"
# Begin Group "Quellcodedateien"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Source\DbfFile.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Hmo.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Observable.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\PolyLine.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\ShapeFile.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\StringResource.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\TinCut.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\triangle.c
# End Source File
# Begin Source File

SOURCE=.\Source\vectorHelper.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\WSPFeatures.cpp
# End Source File
# End Group
# Begin Group "Header-Dateien"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Include\assocNN.h
# End Source File
# Begin Source File

SOURCE=.\Include\csv.h
# End Source File
# Begin Source File

SOURCE=.\Include\DbfFile.h
# End Source File
# Begin Source File

SOURCE=.\Include\HierarchyHolder.h
# End Source File
# Begin Source File

SOURCE=.\Include\Hmo.h
# End Source File
# Begin Source File

SOURCE=.\Include\LinearEquation.h
# End Source File
# Begin Source File

SOURCE=.\Include\Observable.h
# End Source File
# Begin Source File

SOURCE=.\Include\PolyLine.h
# End Source File
# Begin Source File

SOURCE=.\Include\Resource.h
# End Source File
# Begin Source File

SOURCE=.\Include\ShapeFile.h
# End Source File
# Begin Source File

SOURCE=.\Include\SimpleCache.h
# End Source File
# Begin Source File

SOURCE=.\Include\StringResource.h
# End Source File
# Begin Source File

SOURCE=.\Include\TinCut.h
# End Source File
# Begin Source File

SOURCE=.\Include\TinCutExc.h
# End Source File
# Begin Source File

SOURCE=.\Include\triangle.h
# End Source File
# Begin Source File

SOURCE=.\Include\vectorHelper.h
# End Source File
# Begin Source File

SOURCE=.\Include\WSPFeatures.h
# End Source File
# Begin Source File

SOURCE=.\Include\wspwin_regentries.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\History.txt
# End Source File
# Begin Source File

SOURCE=.\ToDo.txt
# End Source File
# End Target
# End Project
