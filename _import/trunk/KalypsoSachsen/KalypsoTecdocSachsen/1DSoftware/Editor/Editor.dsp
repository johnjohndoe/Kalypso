# Microsoft Developer Studio Project File - Name="Editor" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** NICHT BEARBEITEN **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Editor - Win32 Debug
!MESSAGE Dies ist kein gültiges Makefile. Zum Erstellen dieses Projekts mit NMAKE
!MESSAGE verwenden Sie den Befehl "Makefile exportieren" und führen Sie den Befehl
!MESSAGE 
!MESSAGE NMAKE /f "Editor.mak".
!MESSAGE 
!MESSAGE Sie können beim Ausführen von NMAKE eine Konfiguration angeben
!MESSAGE durch Definieren des Makros CFG in der Befehlszeile. Zum Beispiel:
!MESSAGE 
!MESSAGE NMAKE /f "Editor.mak" CFG="Editor - Win32 Debug"
!MESSAGE 
!MESSAGE Für die Konfiguration stehen zur Auswahl:
!MESSAGE 
!MESSAGE "Editor - Win32 Release" (basierend auf  "Win32 (x86) Application")
!MESSAGE "Editor - Win32 Debug" (basierend auf  "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Editor - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\Output\Tmp\Release\Editor"
# PROP Intermediate_Dir "..\..\Output\Tmp\Release\Editor"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /Ob2 /I "Include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /FR /YX"global.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /i "Include" /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 /nologo /subsystem:windows /incremental:yes /machine:I386
# Begin Special Build Tool
TargetPath=\Wsp\Output\Tmp\Release\Editor\Editor.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Release
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Editor - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Editor__"
# PROP BASE Intermediate_Dir "Editor__"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\Output\Tmp\Debug\Editor"
# PROP Intermediate_Dir "..\..\Output\Tmp\Debug\Editor"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /Gi- /GX /ZI /Od /I "Include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /FR /YX"global.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x407 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x407 /i "Include" /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# SUBTRACT LINK32 /incremental:no
# Begin Special Build Tool
TargetPath=\Wsp\Output\Tmp\Debug\Editor\Editor.exe
SOURCE="$(InputPath)"
PostBuild_Desc=Ausgabedatei wird kopiert...
PostBuild_Cmds=copy $(TargetPath) ..\..\Output\Debug
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "Editor - Win32 Release"
# Name "Editor - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\Source\Chicdial.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\childfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Cntritem.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\ConvDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Ddxm.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Doctype.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Editdoc.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Editor.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Editvw.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Formatpa.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Key.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Listdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\mainfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Multconv.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\OpenDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\openpdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Options.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Pageset.cpp
# End Source File
# Begin Source File

SOURCE=.\Source\Strings.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Include\Chicdial.h
# End Source File
# Begin Source File

SOURCE=.\Include\childfrm.h
# End Source File
# Begin Source File

SOURCE=.\Include\Cntritem.h
# End Source File
# Begin Source File

SOURCE=.\Include\ConvDlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\Ddxm.h
# End Source File
# Begin Source File

SOURCE=.\Include\Doctype.h
# End Source File
# Begin Source File

SOURCE=.\Include\editdoc.h
# End Source File
# Begin Source File

SOURCE=.\Include\Editor.h
# End Source File
# Begin Source File

SOURCE=.\Include\editvw.h
# End Source File
# Begin Source File

SOURCE=.\Include\Formatpa.h
# End Source File
# Begin Source File

SOURCE=.\Include\Global.h
# End Source File
# Begin Source File

SOURCE=.\Include\Helpids.h
# End Source File
# Begin Source File

SOURCE=.\Include\Key.h
# End Source File
# Begin Source File

SOURCE=.\Include\Listdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\mainfrm.h
# End Source File
# Begin Source File

SOURCE=.\Include\Mswd6_32.h
# End Source File
# Begin Source File

SOURCE=.\Include\Multconv.h
# End Source File
# Begin Source File

SOURCE=.\Include\OpenDlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\openpdlg.h
# End Source File
# Begin Source File

SOURCE=.\Include\Options.h
# End Source File
# Begin Source File

SOURCE=.\Include\Pageset.h
# End Source File
# Begin Source File

SOURCE=.\Include\resource.h
# End Source File
# Begin Source File

SOURCE=.\include\stdafx.h
# End Source File
# Begin Source File

SOURCE=.\Include\Strings.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\Include\res\csec.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\csec_sav.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Editor.ico
# End Source File
# Begin Source File

SOURCE=.\Include\Editor.rc
# End Source File
# Begin Source File

SOURCE=.\Include\res\Editor.rc2
# End Source File
# Begin Source File

SOURCE=.\Include\res\Editor48.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\font.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Formatba.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Formatbg.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\lsec.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\lsec_sav.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Main1.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Main1b.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Mainfrm.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Mainfrmb.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\project.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rtfdoc.ico
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerbl.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerblm.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerdo.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerdom.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerta.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulertam.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerup.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Rulerupm.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\sec_dis.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Srvr.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Srvrbig.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\state.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\text.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Textdoc.ico
# End Source File
# Begin Source File

SOURCE=.\Include\res\Toolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\water.bmp
# End Source File
# Begin Source File

SOURCE=.\Include\res\Write.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\history.txt
# End Source File
# End Target
# End Project
