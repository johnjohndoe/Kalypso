# Microsoft Developer Studio Project File - Name="WspPrj" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=WspPrj - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "WspPrj.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "WspPrj.mak" CFG="WspPrj - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "WspPrj - Win32 Release" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE "WspPrj - Win32 Debug" (basierend auf  "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "WspPrj - Win32 Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\WspPrj"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\WspPrj"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "Include" /I "..\\" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /YX"stdafx.h" /FD /c
# ADD BASE RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "WspPrj - Win32 Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\WspPrj"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\WspPrj"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Include" /I "..\\" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /YX"stdafx.h" /FD /GZ /c
# ADD BASE RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "WspPrj - Win32 Release"
# Name "WspPrj - Win32 Debug"
# Begin Group "Quellcodedateien"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\source\3dcoord.cpp
# End Source File
# Begin Source File

SOURCE=.\source\branch.cpp
# End Source File
# Begin Source File

SOURCE=.\source\brnchtab.cpp
# End Source File
# Begin Source File

SOURCE=.\source\calc.cpp
# End Source File
# Begin Source File

SOURCE=.\source\calcdata.cpp
# End Source File
# Begin Source File

SOURCE=.\source\connect.cpp
# End Source File
# Begin Source File

SOURCE=.\source\coord.cpp
# End Source File
# Begin Source File

SOURCE=.\source\csection.cpp
# End Source File
# Begin Source File

SOURCE=.\source\csregion.cpp
# End Source File
# Begin Source File

SOURCE=.\source\Datablck.cpp
# End Source File
# Begin Source File

SOURCE=.\source\datablocktypechooserdialog.cpp
# End Source File
# Begin Source File

SOURCE=.\source\datenbank.cpp
# End Source File
# Begin Source File

SOURCE=.\source\DIRDLG.CPP
# End Source File
# Begin Source File

SOURCE=.\source\giostr.cpp
# End Source File
# Begin Source File

SOURCE=.\source\KopfTxt.cpp
# End Source File
# Begin Source File

SOURCE=.\source\loss.cpp
# End Source File
# Begin Source File

SOURCE=.\source\lsection.cpp
# End Source File
# Begin Source File

SOURCE=.\source\outflow.cpp
# End Source File
# Begin Source File

SOURCE=.\source\PrjMngDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\profil.cpp
# End Source File
# Begin Source File

SOURCE=.\source\profilctrl.cpp
# End Source File
# Begin Source File

SOURCE=.\source\project.cpp
# End Source File
# Begin Source File

SOURCE=.\source\projectlist.cpp
# End Source File
# Begin Source File

SOURCE=.\source\section.cpp
# End Source File
# Begin Source File

SOURCE=.\source\state.cpp
# End Source File
# Begin Source File

SOURCE=.\source\statechooser.cpp
# End Source File
# Begin Source File

SOURCE=.\source\stateDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\source\TextBlock.cpp
# End Source File
# Begin Source File

SOURCE=.\source\Triple.cpp
# End Source File
# End Group
# Begin Group "Header-Dateien"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\include\3dcoord.h
# End Source File
# Begin Source File

SOURCE=.\include\branch.h
# End Source File
# Begin Source File

SOURCE=.\include\brnchtab.h
# End Source File
# Begin Source File

SOURCE=.\include\calc.h
# End Source File
# Begin Source File

SOURCE=.\include\calcdata.h
# End Source File
# Begin Source File

SOURCE=.\include\connect.h
# End Source File
# Begin Source File

SOURCE=.\include\coord.h
# End Source File
# Begin Source File

SOURCE=.\include\csection.h
# End Source File
# Begin Source File

SOURCE=.\include\csregion.h
# End Source File
# Begin Source File

SOURCE=.\include\Datablck.h
# End Source File
# Begin Source File

SOURCE=.\include\datablocktypechooserdialog.h
# End Source File
# Begin Source File

SOURCE=.\include\datenbank.h
# End Source File
# Begin Source File

SOURCE=.\include\dirdlg.h
# End Source File
# Begin Source File

SOURCE=.\include\dtypes.h
# End Source File
# Begin Source File

SOURCE=.\include\giostr.h
# End Source File
# Begin Source File

SOURCE=.\include\KopfTxt.h
# End Source File
# Begin Source File

SOURCE=.\include\loss.h
# End Source File
# Begin Source File

SOURCE=.\include\lsection.h
# End Source File
# Begin Source File

SOURCE=.\include\nmprojmng.h
# End Source File
# Begin Source File

SOURCE=.\include\outflow.h
# End Source File
# Begin Source File

SOURCE=.\include\PrjMngDlg.h
# End Source File
# Begin Source File

SOURCE=.\include\Profil.h
# End Source File
# Begin Source File

SOURCE=.\include\profilctrl.h
# End Source File
# Begin Source File

SOURCE=.\include\Project.h
# End Source File
# Begin Source File

SOURCE=.\include\projectlist.h
# End Source File
# Begin Source File

SOURCE=.\include\section.h
# End Source File
# Begin Source File

SOURCE=.\include\state.h
# End Source File
# Begin Source File

SOURCE=.\include\statechooser.h
# End Source File
# Begin Source File

SOURCE=.\include\stateDlg.h
# End Source File
# Begin Source File

SOURCE=.\include\stdafx.h
# End Source File
# Begin Source File

SOURCE=.\include\TextBlock.h
# End Source File
# Begin Source File

SOURCE=.\include\Triple.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\history.txt
# End Source File
# Begin Source File

SOURCE=.\readme.txt
# End Source File
# Begin Source File

SOURCE=.\ToDo.txt
# End Source File
# Begin Source File

SOURCE=.\wspprj.h
# End Source File
# End Target
# End Project
